%%%-------------------------------------------------------------------
%%% @doc
%%%  The callback module that handles all incoming requests at the
%%%  application level.
%%% @end
%%%-------------------------------------------------------------------
-module(ri_gws_impl).

-behavour(gen_web_server).

%% API
-export([start_link/1]).

%% Callbacks
-export([init/1, get/3, delete/3, put/4, post/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_web_server process 
%%
%% @spec start_link(SocketManager::pid()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    error_logger:info_msg("startlink on the gws impl~n"),
    gen_web_server:start_link(?MODULE, Port, []).

%%%===================================================================
%%% gen_web_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Initialize the web serving process
%% @spec (UserArgs) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(_UserArgs) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc Handles http GET requests
%% @spec (InitialRequestLine, Head, Body) -> Response
%% @end
%%--------------------------------------------------------------------
get({_, _, {abs_path, <<$/,Key/binary>>}, _}, _Head, _UserState) ->
    error_logger:info_msg("Key ~p~n", [Key]),
    case simple_cache:lookup(Key) of
	{ok, Value} ->
	    Headers = [{"Content-Type", "text/html"}],
	    gen_web_server:http_reply(200, Headers, Value);
	{error, not_found} ->
	    Headers = [{"Content-Type", "text/html"}],
	    gen_web_server:http_reply(404, Headers, "Content Not Found")
    end.

%%--------------------------------------------------------------------
%% @doc Handles http PUT requests
%% @spec (InitialRequestLine, Head, Body, UserArgs) -> Response
%% @end
%%--------------------------------------------------------------------
put({_, _, {abs_path, <<$/,Key/binary>>}, _}, _Head, Body, _UserState) ->
    simple_cache:insert(Key, Body),
    gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc Handles http DELETE requests
%% @spec (InitialRequestLine, Head, Body) -> Response
%% @end
%%--------------------------------------------------------------------
delete({_, _, {abs_path, <<$/,Key/binary>>}, _}, _Head, _UserState) ->
    simple_cache:delete(Key),
    gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc Handles http POST requests
%% @spec (InitialRequestLine, Head, Body, UserArgs) -> Response
%% @end
%%--------------------------------------------------------------------
post({_, _, _, _}, _Head, _Body, _UserState) ->
    gen_web_server:http_reply(501).

%%%===================================================================
%%% Internal functions
%%%===================================================================
