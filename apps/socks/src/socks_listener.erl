%%%-------------------------------------------------------------------
%%% @author lihenglin <lee@Alpha>
%%% @copyright (C) 2017, lihenglin
%%% @doc
%%%
%%% @end
%%% Created : 19 Jan 2017 by lihenglin <lee@Alpha>
%%%-------------------------------------------------------------------
-module(socks_listener).
%%
-behaviour(gen_server).
%%
-author('henglinli@gmail.com').
%% API
-export([start_link/3, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%%
-define(SERVER, ?MODULE).
%%
-record(state, {callback :: module(), tcp_module :: module()}).
%%
-callback new_connection(Socket :: inet:socket()) ->
    ok | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Callback, Port, ListenOption, Options) ->
%% {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(module(), inet:port_number(),
                 [gen_tcp:listen_option()]) ->
                        gen_server:startlink_ret().
%%
start_link(Callback, Port, ListenOpts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          {Callback, Port, ListenOpts}, []).
%%
stop(Ref) ->
    gen_server:stop(Ref).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init([Callback, IpAddr, Port]) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Callback, Port, ListenOpts}) ->
    false = process_flag(trap_exit, true),
    case gen_tcp:listen(Port, ListenOpts) of
        {ok, Socket} ->
            case prim_inet:async_accept(Socket, -1) of
                {ok, _Ref} ->
                    {Mod, _Opts} = inet:tcp_module(ListenOpts),
                    {ok, #state{callback = Callback,
                                tcp_module = Mod}};
                {error, Reason} ->
                    {stop, Reason}
                end;
        {error, Reason} ->
            {stop, Reason}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc1
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({inet_async, Socket, _Ref, {ok, Server}},
            #state{callback = Callback, tcp_module = Mod} = State) ->
    case inet_db:register_socket(Server, Mod) of
        true ->
            case Callback:new_connection(Server) of
                ok ->
                    case prim_inet:async_accept(Socket, -1) of
                        {ok, _NewRef} ->
                            {noreply, State};
                        {error, Reason} ->
                            {stop, Reason, State}
                    end;
                {error, Reason} ->
                    {stop, Reason, State}
            end;
        _Else ->
            {stop, shutdown, State}
    end;
%%
handle_info({inet_async, _Soket, _Ref, Error}, State) ->
    {stop, Error, State};
%%
handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ignore.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
