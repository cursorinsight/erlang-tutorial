%%% @doc Add numbers and print greetings.
%%% @copyright 2018 My Name
-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).

%% @doc Add two numbers
-spec add(number(), number()) -> number().
add(A, B) ->
    A + B.

%% @doc Print greeting
-spec hello() -> any().
hello() ->
    io:format("Hello, world!~n").

%% @doc Print greeting and add two to the given number
%%
%% This function first prints the greeting, then proceeds to add two to the
%% number received as a parameter.
-spec greet_and_add_two(non_neg_integer()) -> pos_integer().
greet_and_add_two(X) ->
    hello(),
    add(X, 2).
