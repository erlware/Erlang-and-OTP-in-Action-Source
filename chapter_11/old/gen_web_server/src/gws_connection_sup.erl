%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%% simple one for one supervisor for handling http connections.
%%% @end
%%% Created : 13 May 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gws_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/4, start_child/1]).

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
%% @spec start_link(Callback, IP, Port, UserArgs) -> {ok, Pid} | ignore | {error, Error}
%% where
%%  IP = tuple() | default_ip
%% @end
%%--------------------------------------------------------------------
start_link(Callback, IP, Port, UserArgs) ->
    {ok, Pid} = supervisor:start_link(?MODULE, [Callback, IP, Port, UserArgs]),
    start_child(Pid),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @doc
%% Start a child process, an sc_connection.
%%
%% @spec (Server) -> void()
%% @end
%%--------------------------------------------------------------------
start_child(Server) ->
    supervisor:start_child(Server, []).

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
init([Callback, IP, Port, UserArgs]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,
    
    case IP of
	default_ip -> 
	    error_logger:info_msg("Start connection supervisor with ~p ~p ~p~n", [Port, Callback, UserArgs]),
	    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}, {packet, http_bin}, {reuseaddr, true}]);
	IP ->
	    error_logger:info_msg("Start connection supervisor with ~p ~p ~p ~p~n", [IP, Port, Callback, UserArgs]),
	    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}, {packet, http_bin}, {reuseaddr, true}, {ip, IP}])
    end,

    WebSocket = {gws_server, {gws_server, start_link, [Callback, LSock, UserArgs]},
		 Restart, Shutdown, Type, [gws_server]},
    
    {ok, {SupFlags, [WebSocket]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
