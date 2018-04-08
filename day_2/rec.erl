-module(rec).
-export([fac/1, f/0, fac2/1]).

% N! = 1 * 2 * 3 * ... * N
-spec fac(pos_integer()) -> pos_integer().
fac(1) ->
    {backtrace, Trace} = process_info(self(), backtrace), io:format("~s~n", [Trace]),
    1;
fac(N) ->
    fac(N - 1) * N.

% fac(5) -> fac(4) -> fac(3) ... -> fac(1)

f() ->
    g() + 1.

g() ->
    h() + 2.

h() ->
    {backtrace, Trace} = process_info(self(), backtrace), io:format("~s~n", [Trace]),
    0.

-spec fac2(pos_integer()) -> pos_integer().
fac2(N) ->
    do_fac2(N, 1).

% do_fac2(N, Acc) return N! * Acc
do_fac2(1, Acc) ->
    {backtrace, Trace} = process_info(self(), backtrace), io:format("~s~n", [Trace]),
    Acc;
do_fac2(N, Acc) ->
    % do_fac2(N, Acc) = N! * Acc = (N - 1)! * N * Acc = do_fac2(N - 1, N * Acc)
    do_fac2(N - 1, N * Acc).

% fac2(5)
% do_fac2(5, 1)
% do_fac2(4, 5 * 1)
% do_fac2(3, 4 * 5 * 1)
% do_fac2(2, 3 * 4 * 5 * 1)
% do_fac2(1, 2 * 3 * 4 * 5 * 1)
% 2 * 3 * 4 * 5 * 1
