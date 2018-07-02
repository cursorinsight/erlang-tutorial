-module(id_server_1).
-export([start_link/0,
         get_id/0]).

%% @doc Starts an id server process.
-spec start_link() -> {ok, pid()}.
start_link() ->
    Pid = spawn_link(
            fun() ->
                    loop(#{next_id => 0})
            end),
    register(?MODULE, Pid),
    {ok, Pid}.

%% @doc Get a new id.
-spec get_id() -> {ok, integer()} |
                  {error, timeout}.
get_id() ->
    ?MODULE ! {get_id, self()},
    receive
        {id, Id} ->
            {ok, Id}
    after
        5000 ->
            {error, timeout}
    end.

-type state() :: #{next_id := integer()}.

%% @doc The id server process.
-spec loop(state()) -> no_return().
loop(#{next_id := NextId} = State) ->
    receive
        {get_id, Pid} ->
            Pid ! {id, NextId},
            loop(State#{next_id := NextId + 1})
    end.
