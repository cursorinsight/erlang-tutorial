Erlang tutorial day 5
=====================

<img src="process.png" width="400">

Table of Contents
-----------------

1. [Processes](#1-processes)
2. [Exercise](#2-exercise)

1 Processes
---------------------------------------------------

### 1 Process

**Process identifier (pid)**

For any code to run, an Erlang process must be running it. Every process has a unique identifier, usually referred to as a pid.

- `<0.35.0>`

**Spawning**

- This code starts a separate process, which prints the text “erlang!”

    ```erlang
    spawn(io, format, ["erlang!"]).

    spawn(fun() -> io:format("erlang") end).
    ```

### 2 Erlang and Scheduling.

Erlang usually has a thread per core you have in the machine.
Each of these threads runs what is known as a scheduler.

              CPU                                Erlang VM
    +---------------------+  +-------------------------------------------+
    |                     |  |                                           |
    | +-----------------+ |  |   +-----------------+-----------------+   |
    | |                 | |  |   |                 |                 |   |
    | |     Core 1      |-|--|---|  Scheduler # 1  |  Run Queue # 1  |   |
    | |                 | |  |   |                 |                 |   |
    | +-----------------+ |  |   +-----------------+-----------------+   |
    |                     |  |                                           |
    | +-----------------+ |  |   +-----------------+-----------------+   |
    | |                 | |  |   |                 |                 |   |
    | |     Core 2      |-|--|---|  Scheduler # 2  |  Run Queue # 2  |   |
    | |                 | |  |   |                 |                 |   |
    | +-----------------+ |  |   +-----------------+-----------------+   |
    |                     |  |                                           |
    | +-----------------+ |  |   +-----------------+-----------------+   |
    | |                 | |  |   |                 |                 |   |
    | |     Core N      |-|--|---|  Scheduler # N  |  Run Queue # N  |   |
    | |                 | |  |   |                 |                 |   |
    | +-----------------+ |  |   +-----------------+-----------------+   |
    |                     |  |                                           |
    +---------------------+  +-------------------------------------------+

    +--------------------------------------------------------+
    |                                                        |
    |  +-----------------+              +-----------------+  |
    |  |                 |              |                 |  |
    |  |    Scheduler    +-------------->     Task # 1    |  |
    |  |                 |              |                 |  |
    |  +-----------------+              |     Task # 2    |  |
    |                                   |                 |  |
    |                                   |     Task # 3    |  |
    |                                   |                 |  |
    |                                   |     Task # 4    |  |
    |                                   |                 |  |
    |                                   |     Task # N    |  |
    |                                   |                 |  |
    |                                   +-----------------+  |
    |                                   |                 |  |
    |                                   |    Run Queue    |  |
    |                                   |                 |  |
    |                                   +-----------------+  |
    |                                                        |
    +--------------------------------------------------------+

### 3 Operating on processes.

- stopping a process:
    1. normal: when the process finished executing.
    2. exception:
        Exceptions are run-time errors (1+a.. /0.. etc) or generated errors (erlang:error/1,2).
    3. exit(reason): send an exit signal explicitly from one process to another.


### 4 Registering a process.

- If you start your own processes, you can register them with the register function:
    ```erlang
    1> Pid = spawn(timer, sleep, [60000]).
    <0.34.0>
    2> register(name, Pid).
    true
    3> whereis(name).
    <0.34.0>
    4> whereis(name).
    undefined
    ```

### 5 Delivery of messages.

The basic operator for sending a message is !, pronounced “bang,” and it’s used in the form “Destination ! Message”.



                                  MAILBOX
          Process           +-----------------+
    +-----------------+     |                 |
    |                 |     |      MSG 1      |
    |    receive      |-----|                 |
    |                 |     |      MSG 2      |
    +-----------------+     |                 |
                            |      MSG 3      |
                            |                 |
                            |       ...       |
                            |                 |
                            |      MSG N      |
                            |                 |
                            +--------+--------+
                                     |
                    MSG 1 Matches with a recieve clause
                                     |
                                    \|/ MAILBOX
                            +--------+--------+
          Process           |                 |
    +-----------------+     |      MSG 2      |
    |                 |     |                 |
    |    receive      |-----|      MSG 3      |
    |                 |     |                 |
    +-----------------+     |       ...       |
                            |                 |
                            |      MSG N      |
                            |                 |
                            +-----------------+


- Ping Pong Example:
    ```erlang
    run() ->
        Pid = spawn(fun ping/0),
        Pid ! self(),
        receive
            pong -> ok
        end.

    ping() ->
        receive
            From -> From ! pong
        end.
    ```

- Drone Example:
    ```erlang
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
        ```

([tutorial.erl](tutorial.erl) contains the full source code of the examples)

2 Exercise
----------

[Étude 8-1: Using Processes to Simulate a Card Game](https://github.com/cursorinsight/etudes-for-erlang/blob/master/ch08-processes.asciidoc).
