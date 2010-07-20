%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%% simple one for one supervisor for handling the tcp server.
%%% @end
%%% Created : 13 May 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(ri_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1156).

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
start_link() ->
    Port =
	case application:get_env(restful_interface, port) of
	    {ok, Port_} ->
		Port_;
	    undefined -> 
		?DEFAULT_PORT
	end,
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%% ignore |
%% {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,
    
    WebSocket = {ri_gws_impl, {ri_gws_impl, start_link, [Port]},
		 Restart, Shutdown, Type, [ri_gws_impl]},
    
    {ok, {SupFlags, [WebSocket]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
