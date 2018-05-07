#!/usr/bin/env escript

-mode(compile).

-type int_list() :: [integer()].
-type int_operation() :: fun((integer()) -> integer()).

main(_Args) ->
    L = [1, 2, 5, 3, 9],

    print("square_list",
          square_list(L)),

    print("operation_on_list 1",
          operation_on_list(fun square/1, L)),

    print("operation_on_list 2",
          operation_on_list(fun(Item) -> round(math:pow(Item, 2)) end, L)),

    print("map 1",
          lists:map(fun square/1, L)),

    print("map 2",
          lists:map(
            fun(Item) ->
                    round(math:pow(Item, 2))
            end, L)),

    print("filter 1",
          lists:filter(
            fun(Item) ->
                    (Item rem 3) =:= 0
            end, L)),

    L2 = [{xml, "x1"}, {json, "j1"}, {xml, "x2"}, {json, "j2"}],
    print("filter 2",
          lists:filter(
            fun({xml, _}) ->
                    false;
               ({json, _}) ->
                    true
            end, L2)),

    print("lists:filtermap",
          lists:filtermap(
            fun(Item) ->
                    case (Item rem 3) =:= 0 of
                        true ->
                            {true, round(math:pow(Item, 2))};
                        false ->
                            false
                    end
            end, L)),

    print("len",
          lists:foldl(
            fun(_Item, Acc) ->
                    Acc + 1
            end, 0, L)),

    print("reverse",
          lists:foldl(
            fun(Item, Acc) ->
                    [Item|Acc]
            end, [], L)),

    print("list comprehension",
          [ round(math:pow(Item, 2)) || Item <- L ]),

    print("list comprehension with filter",
          [ round(math:pow(Item, 2)) || Item <- L, (Item rem 3) =:= 0 ]),

    print("list comprehension with pattern matching 1",
          [ {json, Content} || {json, Content} <- L2 ]),

    print("list comprehension with pattern matching 2",
          [ Json || Json = {json, _} <- L2 ]),

    print("list comprehension 5",
          [ {A, B, C} || A <- lists:seq(1, 20),
                         B <- lists:seq(A, 20),
                         C <- lists:seq(B, 20),
                         square(A) + square(B) =:= square(C)]),

    ok.

-spec print(iodata(), term()) -> ok.
print(Msg, Term) ->
    io:format("~s: ~w~n", [Msg, Term]).

-spec square_list(int_list()) -> int_list().
square_list([]) ->
    [];
square_list([H|T]) ->
    [H * H|square_list(T)].

-spec operation_on_list(int_operation(), int_list()) -> int_list().
operation_on_list(_F, []) ->
    [];
operation_on_list(F, [H|T]) ->
    [F(H)|operation_on_list(F, T)].

-spec square(integer()) -> integer().
square(N) ->
    round(math:pow(N, 2)).
