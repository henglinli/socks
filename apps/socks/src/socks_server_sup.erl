%%%-------------------------------------------------------------------
%%% @author system_user <chronos@localhost>
%%% @copyright (C) 2017, system_user
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2017 by system_user <chronos@localhost>
%%%-------------------------------------------------------------------
-module(socks_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

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
-spec start_link() -> supervisor:startlink_ret().
%%
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
%%
-spec start_child(inet:socket()) ->
                         supervisor:startchild_ret().
%%
start_child(Server) ->
    supervisor:start_child(socks_server_sup, [Server]).
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
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},
    Child = #{id => socks_server,
              start => {socks_server, start_link, []},
              shutdown => brutal_kill},
    {ok, {SupFlags, [Child]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
