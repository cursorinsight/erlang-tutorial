-module(mylist).
-export([len/1, len2/1, reverse/1]).

%% Return the length of a list (same as `length/1`)
%% (naive solution)
-spec len([term()]) -> non_neg_integer().
len([]) ->
    0;
len([_H|T]) ->
    1 + len(T).

%% Return the length of a list (same as `length/1`)
%% (tail recursive solution)
-spec len2([term()]) -> non_neg_integer().
len2(L) ->
    len2(L, 0).

%% Calculate `len2(L) + Acc`
-spec len2([term()], non_neg_integer()) -> non_neg_integer().
len2([], Acc) ->
    Acc;
len2([_H|T], Acc) ->
    len2(T, Acc + 1).

%% Reverse a list (same as `lists:reverse/1`)
-spec reverse([term()]) -> [term()].
reverse(List) ->
    reverse(List, []).

%% Calculate `reverse(L) ++ Acc`
-spec reverse([term()], [term()]) -> [term()].
reverse([], Acc) ->
    Acc;
reverse([H|T], Acc) ->
    reverse(T, [H|Acc]).
