%%%-------------------------------------------------------------------
%% @doc socks public API
%% @end
%%%-------------------------------------------------------------------

-module(socks_app).

-behaviour(application).
-author('henglinli@gmail.com').
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = application:get_env(socks, port, 1080),
    Options = [{reuseaddr, true}, {mode, binary}],
    ListenOpts = application:get_env(socks, listen, Options),
    Opts = case application:get_env(socks, auth) of
               {ok, Auth} when is_list(Auth) ->
                   [{auth, Auth}];
               _Others ->
                   []
           end,
    socks_sup:start_link(socks_server, Port, ListenOpts, Opts).
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
