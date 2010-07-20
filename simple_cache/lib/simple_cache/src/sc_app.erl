%%%----------------------------------------------------------------
%%% @author Martin Logan & Eric Merritt <contact@erlware.org>
%%% @doc
%%%   Application behaviour implementation for simple_cache.
%%% @copyright 2008 Martin Logan & Eric Merritt
%%% @end
%%%----------------------------------------------------------------,
-module(sc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(WAIT_FOR_RESOURCES, 5000).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ensure_contact(),
    resource_discovery:add_local_resource(simple_cache, node()),
    resource_discovery:add_target_resource_type(simple_cache),
    resource_discovery:trade_resources(),
    timer:sleep(?WAIT_FOR_RESOURCES),
    sc_store:init(),
    case sc_sup:start_link() of
        {ok, Pid} ->
            sc_event_logger:add_handler(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ensure_contact() ->
    case get_env(simple_cache, contact_nodes, []) of
        {ok, []} ->
            error_logger:info_msg("no contact nodes~n");
        {ok, ContactNodes} ->
            ok = ensure_contact(ContactNodes),
            {ok, WaitTime} = get_env(simple_cache, wait_time, 6000),
            wait_for_nodes(ContactNodes, WaitTime)
    end.

ensure_contact([Node|T]) ->
    case net_adm:ping(Node) of
        pong ->
            lists:foreach(fun(N) -> net_adm:ping(N) end, T);
        pang ->
            ensure_contact(T)
    end;
ensure_contact([]) ->
    {error, no_contact_nodes_reachable}.

wait_for_nodes(ContactNodes, WaitTime) ->
    wait_for_nodes(ContactNodes, round(WaitTime / 3), 3).

wait_for_nodes(_, _, 0) ->
    ok;
wait_for_nodes(ContactNodes, WaitSlice, Iterations) ->
    case length(nodes()) > length(ContactNodes) of
        true ->
            ok;
        false ->
            timer:sleep(WaitSlice),
            wait_for_nodes(ContactNodes, WaitSlice, Iterations - 1)
    end.

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
        undefined -> {ok, Default};
        Found     -> Found
    end.
