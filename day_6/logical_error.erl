-module(logical_error).

-export([fibonacci/1]).

fibonacci(N) when N =/= 1 andalso N =/= 2 -> fibonacci(N-1) + fibonacci(N-2);
fibonacci(1) -> 1;
fibonacci(2) -> 1.
