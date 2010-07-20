-module(link_ex2).

-behaviour(gen_server).

-export([start_link/0, ping/0, ping_error/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

ping_error() ->
    gen_server:cast(?SERVER, ping_error).

ping() ->
    gen_server:cast(?SERVER, ping).

init([]) ->
    process_flag(trap_exit, true),
    link_ex:start_link(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(ping, State) ->
    link_ex:ping(),
    {noreply, State};
handle_cast(ping_error, State) ->
    link_ex ! a_message_i_dont_understand,    
    {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    io:format("Restarting link_ex~n"),
    link_ex:start_link(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
