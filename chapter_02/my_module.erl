%% This is a simple Erlang module

-module(my_module).

-export([pie/0, print/1, either_or_both/2, area/1, sign/1, yesno/1,
	 render/0, sum/1, do_sum/1, rev/1, tailrev/1]).


pie() ->
    3.14.


print(Term) ->
    io:format("The value of Term is: ~w.~n", [Term]).


either_or_both(true, B) when is_boolean(B) ->
    true;
either_or_both(A, true) when is_boolean(A) ->
    true;
either_or_both(false, false) ->
    false.


area(Shape) ->
    case Shape of
        {circle, Radius} ->
            Radius * Radius * math:pi();
        {square, Side} ->
            Side * Side;
        {rectangle, Height, Width} ->
            Height * Width
    end.


sign(N) when is_number(N) ->
    if
       N > 0 -> positive;
       N < 0 -> negative;
       true  -> zero
    end.


yesno(F) ->
    case F(true, false) of
        true  -> io:format("yes~n");
        false -> io:format("no~n")
    end.


to_html(Items, F) ->
    ["<dl>\n",
     [io_lib:format("<dt>~s:\n<dd>~s\n", [F(T),F(D)]) || {T, D} <- Items],
     "</dl>"
    ].

render(Items, Em) ->
    to_html(Items,
            fun (Text) ->
                "<" ++ Em ++ ">" ++ Text ++ "</" ++ Em ++ ">"
            end).

render() ->
    render([{"D&D", "Dungeons and Dragons"}], "b").


sum(0) -> 0;
sum(N) -> sum(N-1) + N.


do_sum(N) -> do_sum(N, 0).

do_sum(0, Total) -> Total;
do_sum(N, Total) -> do_sum(N-1, Total+N).


rev([X | TheRest]) -> rev(TheRest) ++ [X];
rev([]) -> [].


tailrev(List) -> tailrev(List, []).

tailrev([X | TheRest], Acc) -> tailrev(TheRest, [X | Acc]);
tailrev([], Acc) -> Acc.
