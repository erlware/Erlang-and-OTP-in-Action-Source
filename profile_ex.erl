%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@erlware.org>
%%% @copyright (C) 2009, Martin Logan
%%% @doc simple demonstration of profiling functionalty
%%% @end
%%%-------------------------------------------------------------------
-module(profile_ex).

%% API
-export([run/0, run_with_fprof/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc start the system and ouput fprof stats to a file
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
run_with_fprof() ->
    fprof:trace(start),
    run(),
    timer:sleep(5000),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, atom_to_list(?MODULE)}).
    
    
%%--------------------------------------------------------------------
%% @doc start two nearly identical running processes
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
run() ->
    spawn(fun() -> looper(1000) end),
    spawn(fun() -> funner(1000) end).

%%%===================================================================
%%% Internal functions
%%%===================================================================
looper(0) ->
    ok;
looper(N) ->
    integer_to_list(N),
    looper(N - 1).

funner(N) ->
    funner(fun(N_) -> integer_to_list(N_) end, N).

funner(_Fun, 0) ->
    ok;
funner(Fun, N) ->
    Fun(N),
    funner(Fun, N - 1).
