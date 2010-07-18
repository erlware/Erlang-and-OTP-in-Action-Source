%%%-------------------------------------------------------------------
%%% @doc JSON parser user API.
%%% @end
%%%-------------------------------------------------------------------

-module(json_parser).

-export([parse_document/1]).

parse_document(Data) when is_binary(Data) ->
    jp_server:parse_document(Data).
