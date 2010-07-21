%%%-------------------------------------------------------------------
%%% @doc Server for the JSON parser integration.
%%% @end
%%%-------------------------------------------------------------------

-module(jp_server).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         parse_document/1,
         stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(APPNAME, json_parser).

-record(state, {port}).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Parses a document.
%% @spec parse_document(Data) -> ok
%% where
%%  Data = binary()
%% @end
%%--------------------------------------------------------------------
parse_document(Data) when is_binary(Data) ->
    gen_server:call(?SERVER, {parse_document, Data}).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Port = create_port(),
    {ok, #state{port=Port}}.

handle_call({parse_document, Msg}, _From, #state{port=Port}=State) ->
    Port ! {self(),{command, term_to_binary(Msg)}},
    receive
        {Port, {data, Data}} ->
            {reply, binary_to_term(Data), State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({Port, {exit_status, Status}}, #state{port=Port}=State) ->
    error_logger:format("port exited with status ~p; restarting~n",
                        [Status]),
    NewPort = create_port(),
    {noreply, State#state{port=NewPort}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_port() ->
    case code:priv_dir(?APPNAME) of
        {error, _} -> 
            error_logger:format("~w priv dir not found~n", [?APPNAME]),
            exit(error);
        PrivDir ->
            case erl_ddll:load(PrivDir, "jp_driver") of
                ok -> ok;
                Other -> exit(Other)
            end,
            open_port({spawn, "jp_driver"}, [binary])
    end.
