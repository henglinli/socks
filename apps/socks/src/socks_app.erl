%%%-------------------------------------------------------------------
%% @doc socks public API
%% @end
%%%-------------------------------------------------------------------

-module(socks_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = application:get_env(socks, port, 1080),
    Options = [{reuseaddr, true}, {mode, binary}],
    ListenOpts = application:get_env(socks, listen, Options),
    socks_sup:start_link(socks_server, Port, ListenOpts).
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
