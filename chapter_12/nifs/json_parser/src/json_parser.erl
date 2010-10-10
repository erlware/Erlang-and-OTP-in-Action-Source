%%%-------------------------------------------------------------------
%%% @doc JSON parser user API.
%%% @end
%%%-------------------------------------------------------------------

-module(json_parser).

-export([init/0, parse_document/1]).

-on_load(init/0).

-define(APPNAME, json_parser).

init() ->
    case code:priv_dir(?APPNAME) of
        {error, _} -> 
            error_logger:format("~w priv dir not found~n", [?APPNAME]),
            exit(error);
        PrivDir ->
            erlang:load_nif(filename:join([PrivDir, "jp_nifs"]), 0)
    end.

%% @doc Parses a document given as a binary
parse_document(_Data) ->
    erlang:nif_error(nif_not_loaded).  % help Dialyzer and friends
