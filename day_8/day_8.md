Erlang tutorial day 8
=====================

<img src="https://pics.me.me/how-your-supervisor-watches-you-at-work-8419363.png" width="300">

Table of Contents
-----------------

1. [Maps](#1-maps)
2. [Error propagation](#2-error-propagation)
3. [gen\_servers](#3-gen_servers)
4. [Supervisors](#4-supervisors)
5. [Exercises](#5-exercises)

1 Maps
------

Creating a new map:

```erl
> M1 = #{a => 1, b => 2}.
#{a => 1,b => 2}
```

Key update:

```erl
> M2 = M1#{a => 3}.
#{a => 3,b => 2}

> M2 = M1#{a := 3}.
#{a => 3,b => 2}

> M3 = M1#{c => 3}.
#{a => 1,b => 2,c => 3}

> M3 = M1#{c := 3}.
* exception error: {badkey,c}
```

Pattern matching:

```erl
> #{a := A, c := C} = M3.
#{a => 1,b => 2,c => 3}

> A.
1

> C.
3
```

[`maps` module](http://erlang.org/doc/man/maps.html) example:

```erl
> maps:get(a, M3).
1

> maps:get(d, M3, mydefault).
mydefault
```

2 Error propagation
-------------------

Linking example:

```erl
> self().
<0.63.0>

> spawn(fun() -> timer:sleep(2000), 1+a end).
<0.65.0>

=ERROR REPORT==== 3-Jul-2018::07:54:16 ===
Error in process <0.80.0> with exit value:
{badarith,[{erlang,'+',[a,1],[]}]}

> self().
<0.63.0>

> spawn_link(fun() -> timer:sleep(2000), 1+a end).
<0.67.0>

=ERROR REPORT==== 1-Jul-2018::22:37:55 ===
Error in process <0.67.0> with exit value:
{badarith,[{erlang,'+',[1,a],[]}]}
** exception error: an error occurred when evaluating an arithmetic
   expression
     in operator  +/2
        called as 1 + a

> self().
<0.69.0>
```

Trap exit example:

```erl
> self().
<0.69.0>

> process_flag(trap_exit, true).
false

> spawn_link(fun() -> timer:sleep(2000), 1+a end).
<0.71.0>

=ERROR REPORT==== 1-Jul-2018::22:37:55 ===
Error in process <0.67.0> with exit value:
{badarith,[{erlang,'+',[1,a],[]}]}

> self().
<0.69.0>
```

Trap exit example with three processes:

```
+-----------+    link     +-----------+    link    +------------+
| Process 1 |  ---------  | Process 2 |  --------  | Process 3  |
|           |             |           |            | traps exit |
+-----------+             +-----------+            +------------+


xxxxxxxxxxxxx    link     +-----------+    link    +------------+
xxxxxxxxxxxxx  ---------  | Process 2 |  --------  | Process 3  |
xxxxxxxxxxxxx             |           |            | traps exit |
xxxxxxxxxxxxx             +-----------+            +------------+


xxxxxxxxxxxxx    exit     +-----------+    link    +------------+
xxxxxxxxxxxxx  -------->  | Process 2 |  --------  | Process 3  |
xxxxxxxxxxxxx   signal    |           |            | traps exit |
xxxxxxxxxxxxx             +-----------+            +------------+


xxxxxxxxxxxxx    exit     xxxxxxxxxxxxx    link    +------------+
xxxxxxxxxxxxx  -------->  xxxxxxxxxxxxx  --------  | Process 3  |
xxxxxxxxxxxxx   signal    xxxxxxxxxxxxx            | traps exit |
xxxxxxxxxxxxx             xxxxxxxxxxxxx            +------------+


xxxxxxxxxxxxx    exit     xxxxxxxxxxxxx    exit    +------------+
xxxxxxxxxxxxx  -------->  xxxxxxxxxxxxx  ------->  | Process 3  |
xxxxxxxxxxxxx   signal    xxxxxxxxxxxxx   signal   | traps exit |
xxxxxxxxxxxxx             xxxxxxxxxxxxx            +------------+

xxxxxxxxxxxxx    exit     xxxxxxxxxxxxx    exit    +------------+
xxxxxxxxxxxxx  -------->  xxxxxxxxxxxxx  ------->  | Process 3  |
xxxxxxxxxxxxx   signal    xxxxxxxxxxxxx   message  | traps exit |
xxxxxxxxxxxxx             xxxxxxxxxxxxx            +------------+
```

3 gen\_servers
--------------

Id server as a plain process: [`id_server_1.erl`](id_server_1.erl).

```erl
> c(id_server_1).
{ok,id_server_1}

> id_server_1:start_link().
{ok,<0.104.0>}

> id_server_1:get_id().    
{ok,0}

> id_server_1:get_id().
{ok,1}
```

Idea: let's **extract the generic part** of a server process (spawn,
registration, loop, receive, timeouts, stopping, state management, system
messages, error handling, etc.) into a **separate module**! → This is the
`gen_server` module.

Now we only have to write the part specific to us: handling requests, updating
the server state and providing the API.

* `gen_server` template: [`gen_server_template.erl`](gen_server_template.erl)
* Id server as a `gen_server` process: [`id_server_2.erl`](id_server_2.erl)

Calling the id server from the shell:

```erl
> id_server_2:start_link().
{ok, <0.91.0>}

> is_server_2:get_id().
0

> is_server_2:get_id().
1
```

`gen_server` structure:

```
         Caller process                        Server process
 ---------------------------------   ------------------------------------
/                                 \ /                                    \

+-------------+                                          +----------------+
| id_server_2 |    call     +------------+  handle_call  | id_server_2    |
| interface   |  -------->  | gen_server |  ---------->  | implementation |
| (API)       |             +------------+               | (callbacks)    |
+-------------+                                          +----------------+
```

4 Supervisors
-------------

Process hierarchy example:

```
                    +============+
                    | supervisor |
                    +============+
                          |
      +-------------------+----------------------------+
      |                   |                            |
      V                   V                            V
+------------+      +============+                +------------+
| gen_server |      | supervisor |                | gen_server |
+------------+      +============+                +------------+
                           |
          +----------------+----------------+
          |                |                |
          V                V                V
    +------------+   +------------+
    | gen_server |   | gen_server |        ...
    +------------+   +------------+
```

A few rules:

* The processes are started [pre-order][pre-order]: parent, left child (+its
  subtree), next child, etc.
* The processes are terminate in the reverse order.
* Processes can send synchronous requests only to processes that proceed them in
  the start order (otherwise deadlocks can easily occur).
* Processes can send asynchronous requests and messages to any other process.

Id server example:

* `supervisor` template: [`supervisor_template.erl`](supervisor_template.erl)
* Id server supervisor: [`id_server_2_sup.erl`](id_server_2_sup.erl)

```erl
> catch_exception(true).
false

> application:start(sasl).
...

> id_server_2_sup:start_link().
{ok, <0.91.0>}

> id_server_2:get_id().
0

> id_server_2:get_id().
1

> exit(whereis(id_server_2), test).

=SUPERVISOR REPORT==== 3-Jul-2018::08:59:06 ===
     Supervisor: {local,id_server_2_sup}
     Context:    child_terminated
     Reason:     test
     Offender:   [{pid,<0.88.0>},
                  {id,id_server_2},
                  {mfargs,{id_server_2,start_link,[]}},
                  {restart_type,permanent},
                  {shutdown,5000},
                  {child_type,worker}]


=PROGRESS REPORT==== 3-Jul-2018::08:59:06 ===
          supervisor: {local,id_server_2_sup}
             started: [{pid,<0.101.0>},
                       {id,id_server_2},
                       {mfargs,{id_server_2,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]
true

> id_server_2:get_id().
0

> [begin exit(whereis(id_server_2), test), timer:sleep(100) end || _ <- lists:seq(1, 6)].
...
** exception exit: shutdown

> id_server_2:get_id().                                                                  
* exception exit: {noproc,{gen_server,call,[id_server_2,get_id,5000]}}
    in function  gen_server:call/3 (gen_server.erl, line 212)
```

## 5 Exercises

1.  Write a `beer_inventory` `gen_server` that keeps track of how many beers we
    have.

    You can use the following state:

    ```erl
    -term state() :: #{beer_count := integer()}.
    ```

    Implement the following API:

    ```erl
    -spec start_link() -> {ok, pid()} | {error, term()}.
    
    -spec increase_beer_count(integer()) -> ok.
    
    -spec get_beer_count() -> integer().
    ```

    You can copy [`gen_server_template.erl`](gen_server_template.erl) and modify it.

    Try the API from the Erlang shell:

    ```erl
    > beer_inventory:start_link().
    ok

    > beer_inventory:get_beer_count().
    0

    > beer_inventory:increase_beer_count(1).
    ok

    > beer_inventory:increase_beer_count(2).
    ok

    > beer_inventory:get_beer_count().
    3

    ```

2.  Create a `beer_inventory_sup.erl` with the following API:

    ```erl
    -spec start_link() -> {ok, pid()} | {error, term()}.
    ```

    Try the API from the Erlang shell:

    ```erl
    > beer_inventory_sup:start_link().
    {ok, <0.91.0>}

    > beer_inventory:get_beer_count().
    0
    ```

    Kill the server and check if it is automatically restarted.

[pre-order]: https://en.wikipedia.org/wiki/Tree_traversal#Pre-order_(NLR)
