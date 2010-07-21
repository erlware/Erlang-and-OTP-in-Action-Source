%%%-------------------------------------------------------------------
%%% @doc JSON parser user API.
%%% @end
%%%-------------------------------------------------------------------

-module(json_parser).

-export([parse_document/1]).

%% @doc Parses a document that can be an io-list
parse_document(Data) ->
    jp_server:parse_document(iolist_to_binary(Data)).
