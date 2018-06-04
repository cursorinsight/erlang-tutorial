-module(throw).

-export([get_from_map/2,
         is_long_list_from_just_42/1]).

%% Get value from map by a given key, if the map does not contain this key
%% returns undefined.
-spec get_from_map(term(), map()) -> term().
get_from_map(Key, Map) when is_map(Map) ->
  try
    maps:get(Key, Map)
  catch
    error:{badkey, Key} -> undefined
  end.

%% Test all value of nested list is 42.
-spec is_42_in_depth(list()) -> boolean().
is_42_in_depth(List) ->
  try
    do_is_42_in_depth(List)
  catch
    throw:not_42 -> false
  end.

%% Test all value of nested list is 42, if there is an non-42 value then throws
%% not_42 atom.
-spec do_is_42_in_depth(list()) -> true.
do_is_42_in_depth([]) ->
  true;
do_is_42_in_depth([N | Rest]) ->
  true = is_42(N),
  do_is_42_in_depth(Rest).

%% Test value is 42 if that is not 42 then throws not_42 atom.
-spec is_42(number() | list()) -> true.
is_42(42) -> true;
is_42(List) when is_list(List) -> do_is_42_in_depth(List);
is_42(N) when is_integer(N) -> throw(not_42).
