%%%-------------------------------------------------------------------
%%% @author system_user <chronos@localhost>
%%% @copyright (C) 2017, system_user
%%% @doc
%%%
%%% @end
%%% Created : 10 Feb 2017 by system_user <chronos@localhost>
%%%-------------------------------------------------------------------
-module(socks_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).
%% Supervisor callbacks
-export([init/1]).
%%
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(module(), inet:port_number(),
                 [gen_tcp:listen_option()]) ->
                        supervisor:startlink_ret().
%%
start_link(Callback, Port, ListenOpts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
                          {Callback, Port, ListenOpts}).
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init({Callback, Port, ListenOpts}) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    Listener = #{id => socks_listener,
                 start => {socks_listener, start_link,
                           [Callback, Port, ListenOpts]},
                 restart => permanent,
                 type => worker,
                 modules => [socks_listener]},
    ServerSup = #{id => socks_server_sup,
                  start => {socks_server_sup, start_link, []},
                  restart => permanent,
                  type => supervisor,
                  modules => [socks_server_sup]},
    {ok, {SupFlags, [Listener, ServerSup]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
