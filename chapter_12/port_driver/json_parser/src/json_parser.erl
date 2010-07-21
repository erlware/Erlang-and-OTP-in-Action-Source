%%%-------------------------------------------------------------------
%%% @doc JSON parser user API.
%%% @end
%%%-------------------------------------------------------------------

-module(json_parser).

-export([parse_document/1]).

%% @doc Parses a document given as a binary
parse_document(Data) ->
    jp_server:parse_document(Data).
