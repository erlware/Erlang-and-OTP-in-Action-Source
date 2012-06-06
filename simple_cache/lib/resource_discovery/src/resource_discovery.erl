%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  The api for the resource discovery system.
%%% @end
%%% Created : 23 Apr 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(resource_discovery).

-export([
	 add_local_resource/2,
	 add_target_resource_type/1,
	 fetch_resources/1,
	 delete_resource/2,
	 trade_resources/0
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Add a resource that is present on the local node that a
%%      remote service will want to consume.
%% @spec (Type::resource_type(), Resource::resource_instance()) -> ok
%% @end
%%-------------------------------------------------------------------
add_local_resource(Type, Resource) ->
    rd_server:add_local_resource(Type, Resource).

%%-------------------------------------------------------------------
%% @doc Add a type of resource that you wish to cache any remote
%%      instances of.
%% @spec (Type::resource_type()) -> ok
%% @end
%%-------------------------------------------------------------------
add_target_resource_type(Type) ->
    rd_server:add_target_resource_type(Type).

%%-------------------------------------------------------------------
%% @doc Fetch all the resources for a particular resource instance
%%      type.
%% @spec (Type::resource_type()) -> {ok, [Resource::resource_instance]} | error
%% @end
%%-------------------------------------------------------------------
fetch_resources(Type) ->
    rd_server:fetch_resources(Type).

%%-------------------------------------------------------------------
%% @doc Delete a particular resource instance for a particular
%%      resource type.
%% @spec (Type::resource_type(), Resource::resource_instance()) -> ok
%% @end
%%-------------------------------------------------------------------
delete_resource(Type, Resource) ->
    rd_server:delete_resource(Type, Resource).

%%-------------------------------------------------------------------
%% @doc trade resources with all remote nodes
%% @spec () -> ok
%% @end
%%-------------------------------------------------------------------
trade_resources() ->
    rd_server:trade_resources().
