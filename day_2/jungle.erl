-module(jungle).
-export([greet/1, greet2/1, greet3/1]).

-spec greet({atom(), atom() | string(), number()}) -> ok.
greet(Animal) ->
    case Animal of
        {butterfly, _Name, Age} when 2 =< Age, Age =< 5 ->
            io:format("Catch the butterfly!~n");
        {monkey, Name, _Age} ->
            io:format("Be careful on the branches, ~s!~n", [Name]);
        {tiger, _Name, Age} when Age =< 2; 20 =< Age->
            io:format("I'm not afraid of you!~n");
        {tiger, _Name, _Age} ->
            io:format("Run!~n");
        {_Species, Name, _Age} ->
            io:format("Hello ~s!~n", [Name]);
        Other ->
            throw({unexpected_value, Other})
    end.

-spec greet2({atom(), atom() | string(), number()}) -> ok.
greet2(Animal = {_Species, _Name, Age}) when Age > 1000 ->
    io:format("Warning: age is unrealistic of ~p~n", [Animal]);
greet2({Name, Name, _Age}) ->
    io:format("You have a boring name.~n");
greet2({butterfly, _Name, Age}) when 2 =< Age, Age =< 5 ->
    io:format("Catch the butterfly!~n");
greet2({monkey, Name, _Age}) ->
    io:format("Be careful on the branches, ~s!~n", [Name]);
greet2({tiger, _Name, Age}) when Age =< 2; 20 =< Age->
    io:format("I'm not afraid of you!~n");
greet2({tiger, _Name, _Age}) ->
    io:format("Run!~n");
greet2({_Species, Name, _Age}) ->
    io:format("Hello ~s!~n", [Name]);
greet2(Other) ->
    throw({unexpected_value, Other}).

-spec greet3(atom()) -> ok.
greet3(Species) ->
    if
        Species =:= monkey ->
            io:format("Be careful on the branches!~n");
        Species =:= tiger ->
            io:format("Run!~n");
        true ->
            io:format("Hello!~n")
    end.
