%% ---------------------------------------------------------------------
%% File: test.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@klarna.com>
%% @copyright 2010 Richard Carlsson
%% @doc 

-module(test).

-export([parse/1, read/1, test/0]).

read(File) ->
    {ok, Doc} = file:read_file(File),
    parse(Doc).

parse(Doc) ->
    case erl_ddll:load("./priv", "jp_driver") of
        ok -> ok;
        Other -> exit(Other)
    end,
    Port = open_port({spawn, "jp_driver"}, [binary]),
    Port ! {self(), {command, term_to_binary(Doc)}},
    receive
        {Port, {data, Data}} ->
            binary_to_term(Data)
    end.

test() ->
    JSON = parse(<<"[null, true, false, "
                  "{\"int\": 4711, \"float\": 3.14}, "
                  "\"bye\"]">>),
    io:fwrite("~p.\n", [JSON]).
