%%%-------------------------------------------------------------------
%%% @author lihenglin <lee@Alpha>
%%% @copyright (C) 2017, lihenglin
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2017 by lihenglin <lee@Alpha>
%%%-------------------------------------------------------------------
-module(socks_listener_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
%%
-export([new_connection/1]).
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {motifskip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [start_stop, hello_server].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
start_stop() ->
    [].
%%
hello_server() ->
    [].
%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
%%
start_stop(_Config) ->
    {ok, Pid0} = socks_listener:start_link(
                   socks_listener_SUITE, 1081, []),
    socks_listener:stop(Pid0).
%%
hello_server(_Config) ->
    {ok, Pid0} = socks_listener:start_link(
                   socks_listener_SUITE, 1082,
                   [binary, {active, false}]),
    case gen_tcp:connect(localhost, 1082, []) of
        {ok, ClientSocket} ->
            case gen_tcp:recv(ClientSocket, 128) of
                {ok, _Packet} ->
                    gen_tcp:close(ClientSocket),
                    socks_listener:stop(Pid0);
                {error, _Reason} = Error ->
                    Error
            end
    end.
%%
echo_server(_Config) ->
    {ok, _Pid0} = socks_listener:start_link(
                    socks_listener_SUITE, 1080,
                    [binary, {active, false}]),
    case gen_tcp:connect(localhost, 1080, []) of
        {ok, ClientSocket} ->
            send_recv(ClientSocket, <<"hello">>);
        {error, _Reason} = Error ->
            Error
    end.
%%
new_connection(Socket) ->
    case gen_tcp:send(Socket, <<"hello">>) of
        ok ->
            gen_tcp:close(Socket);
        {error, _Reason} = Error ->
            Error
    end.
%%
send_recv(Socket, Packet) ->
    case gen_tcp:send(Socket, Packet) of
        ok ->
            case gen_tcp:recv(Socket, 128) of
                {ok, _Packet} ->
                    gen_tcp:close(Socket);
                {error, _Reason} = Error ->
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.
%%
recv_send(Socket) ->
    case gen_tcp:recv(Socket, 128) of
        {ok, Packet} ->
            case gen_tcp:send(Socket, Packet) of
                ok ->
                    gen_tcp:close(Socket);
                {error, _Reason} = Error ->
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.
%%
