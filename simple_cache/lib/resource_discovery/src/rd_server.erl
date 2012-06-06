%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@gmail.com>
%%% @copyright (C) 2009, Martin Logan
%%% @doc A very simple resource discovery system.
%%% List of terms and types:
%%%
%%% @type resource() = {resource_type(), resource_instance()}. The type of a resource followed by its identifier. Local
%%%       resources are communicated to other resource discovery instances and cached by those how have the
%%%       local resource type set as a target type. 
%%% @type resource_type() = atom(). The name of a resource, how it is identified. For example
%%%       a type of service that you have on the network may be identified by it's node name
%%%       in which case you might have a resource type of 'my_service' of which there may be
%%%       many node names representing resources such as {my_service, myservicenode@myhost}. 
%%% @type resource_instance() =  pid(). The resource being managed.
%%%
%%% @end
%%% Created : 21 Feb 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(rd_server).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 add_local_resource/2,
	 add_target_resource_type/1,
	 fetch_resources/1,
	 delete_resource/2,
	 trade_resources/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {local_resources, target_resource_types, resources}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-------------------------------------------------------------------
%% @doc Add a resource that is present on the local node that a
%%      remote service will want to consume.
%% @spec (Type::resource_type(), Resource::resource_instance()) -> ok
%% @end
%%-------------------------------------------------------------------
add_local_resource(Type, Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

%%-------------------------------------------------------------------
%% @doc Add a type of resource that you wish to cache any remote
%%      instances of.
%% @spec (Type::resource_type()) -> ok
%% @end
%%-------------------------------------------------------------------
add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

%%-------------------------------------------------------------------
%% @doc Fetch all the resources for a particular resource instance
%%      type.
%% @spec (Type::resource_type()) -> {ok, [Resource::resource_instance]} | error
%% @end
%%-------------------------------------------------------------------
fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

%%-------------------------------------------------------------------
%% @doc Delete a particular resource instance for a particular
%%      resource type.
%% @spec (Type::resource_type(), Resource::resource_instance()) -> ok
%% @end
%%-------------------------------------------------------------------
delete_resource(Type, Resource) ->
    gen_server:cast(?SERVER, {delete_resource, {Type, Resource}}).

%%-------------------------------------------------------------------
%% @doc trade resources with all remote nodes
%% @spec () -> ok
%% @end
%%-------------------------------------------------------------------
trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{resources = dict:new(), target_resource_types = [], local_resources = dict:new()}}.

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
handle_call({fetch_resources, Type}, _From, State) ->
    {reply, dict:find(Type, State#state.resources), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(trade_resources, #state{local_resources = LocalResources} = State) ->
    error_logger:info_msg("trade resources~p~n", [LocalResources]),
    lists:foreach(fun(Node) ->
			  gen_server:cast({?SERVER, Node}, {trade_resources, {node(), LocalResources}})
		  end,
		  [node()|nodes()]),
    {noreply, State};
handle_cast({trade_resources, {ReplyToken, RemoteResources}}, State) ->
    error_logger:info_msg("trade resources~p~n", [{ReplyToken, RemoteResources}]),
    #state{local_resources = LocalResources, target_resource_types = TargetTypes, resources = Resources} = State,
    ResourceList = return_resources_for_types(TargetTypes, RemoteResources),
    NewResources = add_resources(ResourceList, Resources),
    case ReplyToken of
	noreply ->
	    ok;
	ReplyToken ->
	    gen_server:cast({?SERVER, ReplyToken}, {trade_resources, {noreply, LocalResources}})
    end,
    {noreply, State#state{resources = NewResources}};

handle_cast({add_local_resource, {Type, Resource}}, #state{local_resources = LocalResources} = State) ->
    error_logger:info_msg("add local resource ~p~n", [{Type, Resource}]),
    {noreply, State#state{local_resources = add_resource(Type, Resource, LocalResources)}};
handle_cast({add_target_resource_type, Type}, #state{target_resource_types = TargetTypes} = State) ->
    {noreply, State#state{target_resource_types = [Type|lists:delete(Type, TargetTypes)]}};
handle_cast({delete_resource, {Type, Resource}}, #state{resources = Resources} = State) ->
    {noreply, State#state{resources = delete_resource(Type, Resource, Resources)}}.

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
    ok.

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
add_resources([{Type, Identifier}|T], Dict) ->
    add_resources(T, add_resource(Type, Identifier, Dict));
add_resources([], Dict) ->
    Dict.

add_resource(Type, Identifier, Dict) ->
    io:format("store ~p ~p in ~p~n", [Type, Identifier, Dict]),
    case dict:find(Type, Dict) of
	{ok, ResourceList} ->
	    dict:store(Type, [Identifier|lists:delete(Identifier, ResourceList)], Dict);
	error ->
	    dict:store(Type, [Identifier], Dict)
    end.
	    

delete_resource(Type, Identifier, Dict) ->
    case dict:find(Type, Dict) of
	{ok, [Identifier]} ->
	    dict:erase(Type, Dict);
	{ok, ResourceList} ->
	    dict:store(Type, lists:delete(Identifier, ResourceList), Dict);
	error ->
	    Dict
    end.
    
return_resources_for_types(Types, Resources) ->
    Fun = 
	fun(Type, Acc) ->
		case dict:find(Type, Resources) of
		    {ok, List}  -> [{Type, Resource} || Resource <- List] ++ Acc;
		    error       -> Acc
		end
	end,
    lists:foldl(Fun, [], Types).
