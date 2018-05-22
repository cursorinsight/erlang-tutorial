-module(tutorial).
-compile(export_all).

run() ->
    Pid = spawn(fun ping/0),
    Pid ! self(),
    receive
        pong -> "pong"
    end.

ping() ->
    receive
        From -> From ! pong
    end.

drone(State) ->
    receive
        {From, take_off} when State == on_the_ground->
            From ! "I am taking off",
            drone(in_air);
        {From ,land} when State == in_air ->
            From ! "I am landing!",
            drone(on_the_ground);
        _ ->
            io:format("I can't do that ~n"),
            drone(State)
    after 80000 ->
        io:format("Batteries Died ~n")
    end.