-module(ma_hello_server).
-behaviour(gen_server).

-include("eunit.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
 
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    error_logger:info_msg("~p init~n", [?MODULE]),
    {ok, #state{}, 0}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {stop, normal, State}.

terminate(_Reason, _State) ->
    Msg = get_conf_value(my_app, hello_msg, "Hello Erlware and OTP"),
    error_logger:info_msg(Msg).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_conf_value(App, Key, DefaultValue) ->
    case application:get_env(App, Key) of
	{ok, Value} -> Value;
	undefined   -> DefaultValue
    end.
	    
%%%===================================================================
%%% Test functions
%%%===================================================================

get_conf_value_test() ->
    ?assertMatch(default_value, get_conf_value(test, '$testval$', bad_default)).
