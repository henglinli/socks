%%%-------------------------------------------------------------------
%%% @author lihenglin <lee@Alpha>
%%% @copyright (C) 2017, lihenglin
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2017 by lihenglin <lee@Alpha>
%%%-------------------------------------------------------------------
-module(socks_server).
-include("socks.hrl").
%%
-author('henglinli@gmail.com').
%%
-behaviour(gen_statem).
-behaviour(socks_listener).
%% API
-export([new_connection/1, start_link/2]).
%% gen_statem callbacks
-export([init/1, terminate/3, code_change/4, callback_mode/0]).
-export([handshake/3, auth/3, command/3, proxy/3]).
%%
-define(SERVER, ?MODULE).
-define(N, 64).
-define(OPTS, [{active, ?N}, {delay_send, true}]).
%%
-record(data, {server :: inet:socket(),
               client :: undefined | inet:socket(),
               auth :: undefined | module()}).
%%%===================================================================
%%% API
%%%===================================================================
callback_mode() ->
    state_functions.
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_connection(Socket :: inet:socket()) ->
                            ok | {error, Reason :: term()}.
%%
-ifdef(USE_SIDEJOB).
new_connection(Socket) ->
    case sidejob_supervisor:start_child(erl_socks_server_sj,
                                        gen_statem,
                                        start_link,
                                        [?SERVER, [], []]) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Socket, Pid);
        Others ->
            Others
    end.
-else.
new_connection(Server) ->
    case socks_server_sup:start_child(Server) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Server, Pid);
        Others ->
            Others
    end.
-endif.
%%
-spec start_link(undefined | module(), inet:socket()) ->
                        gen_statem:startlink_ret().
%%
start_link(Mod, Server) ->
    gen_statem:start(?SERVER, {Mod, Server}, []).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: tuple()) ->
                  ignore | {stop, Reason :: term()} |
                  {ok, State :: term(), Data :: term()} |
                  {ok, State :: term(), Data :: term(),
                   [gen_statem:action()] | gen_statem:action()}.
%%
init({Mod, Server}) ->
    %%false = process_flag(trap_exit, true),
    case inet:setopts(Server, ?OPTS) of
        ok ->
            {ok, handshake,
             #data{server = Server, client = undefined, auth = Mod}};
        {error, Reason} ->
            {stop, Reason}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the gen_statem runs with CallbackMode =:= state_functions
%% there should be one instance of this function for each possible
%% state name. Whenever a gen_statem receives an event,
%% the instance of this function with the same name
%% as the current state name StateName is called to
%% handle the event.
%% @end
%%--------------------------------------------------------------------
-spec handshake(gen_statem:event_type(), term(), #data{}) ->
                       gen_statem:state_function_result().
%%
handshake(info, {tcp, Socket, Packet}, #data{auth = Auth} = Data) ->
    case socks:parse_methods(Packet) of
        error ->
            Reply = socks:new_unaccept(),
            gen_tcp:send(Socket, Reply),
            stop;
        MethodSet ->
            case Auth of
                undefined ->
                    Reply = socks:new_noauth(),
                    NextState = {next_state, command, Data},
                    send(Socket, Reply, NextState);
                _Mod ->
                    case socks:have_auth(MethodSet) of
                        true ->
                            Reply = socks:new_auth(),
                            NextState = {next_state, auth, Data},
                            send(Socket, Reply, NextState);
                        _Others ->
                            Reply = socks:new_unaccept(),
                            gen_tcp:send(Socket, Reply),
                            stop
                    end
            end
    end;
%%
handshake(EventType, EventContent, _Data) ->
    check(EventType, EventContent).
%%
auth(info, {tcp, Socket, Packet}, #data{auth = Mod} = Data) ->
    case socks:parse_auth(Packet) of
        {_User, _Password} = Msg->
            case Mod:call(Msg) of
                ok ->
                    Reply = socks:new_success(),
                    NextState = {next_state, command, Data},
                    send(Socket, Reply, NextState);
                _Others ->
                    Reply = socks:new_failure(),
                    gen_tcp:send(Socket, Reply),
                    stop
            end;
        _Others ->
            Reply = socks:new_unaccept(),
            gen_tcp:send(Socket, Reply),
            stop
    end;
%%
auth(EventType, EventContent, _Data) ->
    check(EventType, EventContent).
%%%===================================================================
-spec command(gen_statem:event_type(), term(), #data{}) ->
                     gen_statem:state_function_result().
command(info, {tcp, Socket, Packet}, Data) ->
    case socks:parse_command(Packet) of
        {connect, ATYP, Address} ->
            Parser = parser(ATYP),
            case connect(Parser, Address) of
                {ok, Client} ->
                    Reply = socks:new_ipv4(?REP_SUCCESS),
                    NextState = {next_state, proxy,
                                 Data#data{client = Client}},
                    send(Socket, Reply, NextState);
                _Others ->
                    disallow(Socket)
            end;
        _Others ->
            disallow(Socket)
    end;
%%
command(EventType, EventContent, _Data) ->
    check(EventType, EventContent).
%%
-spec proxy(gen_statem:event_type(), term(), #data{}) ->
                     gen_statem:state_function_result().
proxy(info, {tcp, Server, Packet},
      #data{server = Server, client = Client}) ->
    send(Client, Packet, keep_state_and_data);
%%
proxy(info, {tcp, Client, Packet},
      #data{server = Server, client = Client}) ->
    send(Server, Packet, keep_state_and_data);
%%
proxy(EventType, EventContent, _Data) ->
    check(EventType, EventContent).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
%%
terminate(_Reason, proxy, #data{server = Server,
                                client = Client}) ->
    gen_tcp:close(Server),
    gen_tcp:close(Client),
    ignore;
%%
terminate(_Reason, _State, #data{server = Server}) ->
    gen_tcp:close(Server),
    ignore.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down,term()},
                  State :: term(), Data :: term(), Extra :: term()) ->
                         {gen_statem:callback_mode(),
                          NewState :: term(), NewData :: term()} |
                         (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {state_functions, State, Data}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%
check(info, {tcp_passive, Socket}) ->
    case inet:setopts(Socket, [{active, ?N}]) of
        ok ->
            keep_state_and_data;
        _Others ->
            stop
    end;
%%
check(info, _Info) ->
    stop;
%%
check({call, Caller}, _Msg) ->
    {keep_state_and_data, [{reply, Caller, ok}]};
%%
check(_EventType, _Msg) ->
    keep_state_and_data.
%%%===================================================================
disallow(Socket) ->
    Reply = socks:new_ipv4(?REP_DISALLOWED),
    gen_tcp:send(Socket, Reply),
    stop.
%%
send(Socket, Packet, NextState) ->
    case gen_tcp:send(Socket, Packet) of
        ok ->
            NextState;
        _Others ->
            stop
    end.
%%
connect(Parser, Address) ->
    case Parser(Address) of
        {Addr, Port} ->
            gen_tcp:connect(Addr, Port, ?OPTS);
        Others ->
            Others
    end.
%%%===================================================================
parser(ATYP) ->
    case ATYP of
        ?ATYP_IPV4 ->
            fun(Address) ->
                    socks:parse_ipv4(Address)
            end;
        ?ATYP_IPV6 ->
            fun(Address) ->
                    socks:parse_ipv6(Address)
            end;
        ?ATYP_ADDR ->
            fun(Address) ->
                    socks:parse_addr(Address)
            end;
        _Others ->
            fun(_) ->
                    error
            end
    end.
