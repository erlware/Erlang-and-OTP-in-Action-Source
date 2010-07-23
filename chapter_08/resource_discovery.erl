-module(resource_discovery).

-behaviour(gen_server).

-export([
         start_link/0,
         add_target_resource_type/1,
         add_local_resource/2,
         fetch_resources/1,
         trade_resources/0
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types,
                local_typed_resources,
                typed_resources}).

%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

%% Callbacks

init([]) ->
    {ok, #state{target_resource_types = [],
                local_typed_resources = dict:new(),
                typed_resources       = dict:new()}}.

handle_call({fetch_resources, Type}, _From, State) ->
    {reply, dict:find(Type, State#state.typed_resources), State}.

handle_cast({add_target_resource_type, Type}, State) ->
    TargetTypes = State#state.target_resource_types,
    NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
    {noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, {Type, Resource}}, State) ->
    LocalTypedResources = State#state.local_typed_resources,
    NewLocalTypedResources = add_resource(Type, Resource, LocalTypedResources),
    {noreply, State#state{local_typed_resources = NewLocalTypedResources}};
handle_cast(trade_resources, State) ->
    LocalTypedResources = State#state.local_typed_resources,
    AllNodes = [node() | nodes()],
    lists:foreach(
        fun(Node) ->
            gen_server:cast({?SERVER, Node},
                            {trade_resources, {node(), LocalTypedResources}})
        end,
        AllNodes),
    {noreply, State};
handle_cast({trade_resources, {ReplyTo, RemoteTypedResources}},
           #state{local_typed_resources = LocalTypedResources,
		  target_resource_types = TargetTypes,
		  typed_resources       = TypedResources} = State) ->
    FilteredRemoteTypedResources = filter_remote_resources_by_target_types(TargetTypes, RemoteTypedResources),
    NewTypedResources = add_typed_resources(FilteredRemoteTypedResources, TypedResources),
    case ReplyTo of
        noreply ->
            ok;
        _ ->
            gen_server:cast({?SERVER, ReplyTo},
                            {trade_resources, {noreply, LocalTypedResources}})
    end,
    {noreply, State#state{typed_resources = NewTypedResources}}.

handle_info(ok = _Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Utilities

add_typed_resources([{Type, Identifier}|T], Dict) ->
    add_typed_resources(T, add_resource(Type, Identifier, Dict));
add_typed_resources([], Dict) ->
    Dict.

add_resource(Type, Resource, Dict) ->
    case dict:find(Type, Dict) of
        {ok, ResourceList} ->
            NewList = [Resource | lists:delete(Resource, ResourceList)],
            dict:store(Type, NewList, Dict);
        error ->
            dict:store(Type, [Resource], Dict)
    end.

filter_remote_resources_by_target_types(Types, TypedResources) ->
    Fun =
        fun(Type, Acc) ->
            case dict:find(Type, TypedResources) of
                {ok, List} ->
                    [{Type, Resource} || Resource <- List] ++ Acc;
                error ->
                    Acc
            end
        end,
    lists:foldl(Fun, [], Types).
