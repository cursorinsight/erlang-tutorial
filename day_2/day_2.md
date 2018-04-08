Erlang tutorial day 2
=====================

Table of Contents
-----------------

1. [Logic, atoms, tuples, pattern matching, recursion](#1-logic-atoms-tuples-pattern-matching-recursion)
2. [Exercises](#2-exercises)

1 Logic, atoms, tuples, pattern matching, recursion
---------------------------------------------------

### 1.1 Logic, atoms and tuples ([Starting out (for real)](http://learnyousomeerlang.com/starting-out-for-real))

<img src="https://www.webdevelopersnotes.com/wp-content/uploads/joke-false-itss-funny-because-its-true.png" width="500">

**Boolean Algebra & Comparison operators**

- `true`, `false`
- normal: `not`, `and`, `or`, `xor`
- short circuit: `andalso`, `orelse`
- `<`, `>`, `=<`, `>=`
- `number < atom < reference < fun < port < pid < tuple < list < bit string`

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/2/2e/Simple_Periodic_Table_Chart-en.svg/1024px-Simple_Periodic_Table_Chart-en.svg.png" width="500">

**Atoms**: `atom`, `'Special atom'`. Usage examples:

- Atoms vs strings:
    - atoms consume less memory
    - atoms are faster to compare
    - the number of atoms is limited
    - there is no GC for atoms
- What are atoms useful for? A few examples:
    - Return value tag: `{ok, Result} | {error, Error}`
    - Type tag: `{person, Name, Address}`
    - Enums: `white | yellow | green | oolong | black | dark`

**Tuples**:

- Pattern matching:

    ```erlang
    > Point = {10, 4}.
    {10,4}
    > {X, Y} = Point.
    {4,5}
    > X.
    4
    > {X, _} = Point.
    {4,5}
    > {_, _} = {4, 5}.
    {4,5}
    > {_, _} = {4, 5, 6}.
    ** exception error: no match of right hand side value {4,5,6}
    > {Y, Y} = {4, 5}.
    ** exception error: no match of right hand side value {4,5}
    ```

### 1.2 Pattern matching ([Syntax in functions](http://learnyousomeerlang.com/syntax-in-functions))

<img src="https://english2ni.files.wordpress.com/2012/02/1st-conditional.jpg">

[Source code: jungle.erl.](jungle.erl)

**Case expression**

```erlang
greet(Animal) ->
    case Animal of
        monkey ->
            io:format("Be careful on the branches!~n");
        tiger ->
            io:format("Run!~n");
        _ ->
            io:format("Hello!~n")
    end.
```

**Case + Pattern matching**

```erlang
greet(Animal) ->
    case Animal of
        {monkey, Name} ->
            io:format("Be careful on the branches, ~s!~n", [Name]);
        {tiger, _Name} ->
            io:format("Run!~n");
        {_Species, Name} ->
            io:format("Hello, ~s!~n", [Name]);
        Other ->
            throw({error, {unexpected_value, Other}})
    end.
```

**Pattern matching: `=` vs `case`**

```erlang
Pattern = Value

case Value of
    Pattern1 ->
        Expression1;
    Pattern2 ->
        Expression2;
    ...
end
```

**Guards**

```erlang
greet(Animal) ->
    case Animal of
        % We are interested only in middle-aged butterflies
        {butterfly, Name, Age} when 2 =< Age, Age =< 5 ->
            io:format("Come here, ~s!~n", [Name]);
        {monkey, Name, _Age} ->
            io:format("Be careful on the branches, ~s!~n", [Name]);
        % We aren't afraid of young or old tigers
        {tiger, _Name, Age} when Age =< 2; 20 =< Age ->
            io:format("I'm not afraid of you!~n", []);
        {tiger, _Name, _Age} ->
            io:format("Run!~n");
        {_Species, Name, _Age} ->
            io:format("Hello, ~s!~n", [Name]);
        Other ->
            throw({error, {unexpected_value, Other}})
    end.
```

**Pattern matching in function clauses**

- The formal parameters of functions are actually also patterns:

    ```erlang
    greet({_, Name, Age}) when Age =< 1 ->
        io:format("Hellobello, ~s!~n", [Name]);
    greet({butterfly, Name, Age}) when 2 =< Age, Age =< 5 ->
        io:format("Come here, ~s!~n", [Name]);
    greet({monkey, Name, _Age}) ->
        io:format("Be careful on the branches, ~s!~n", [Name]);
    greet({tiger, _Name, Age}) when Age =< 2; 20 =< Age ->
        io:format("I'm not afraid of you!~n", []);
    greet({tiger, _Name, _Age}) ->
        io:format("Run!~n", []);
    greet({_Species, Name, _Age}) ->
        io:format("Hello, ~s!~n", [Name]);
    greet(Other) ->
        throw({error, {unexpected_value, Other}}).
    ```

- These patterns may bind the same variables:

    ```erlang
    greet({Name, Name, _Age}) ->
        io:format("Your have a boring name, ~s!~n", [Name]);
    ```

- They may even contain pattern matchings:

    ```erlang
    greet(Animal = {_Species, _Name, Age}) when Age > 1000 ->
        io:format("Warning. Unrealistic age for animal ~p~n", [Animal]);
    ```

**If expressions**

- `if` expressions don't have pattern matching, only guards:

    ```erlang
    greet(Animal) ->
        if  Animal =:= monkey ->
                io:format("Be careful on the branches!~n");
            Animal =:= tiger ->
                io:format("Run!~n");
            true ->
                io:format("Hello!~n")
        end.
    ```

### 1.3 [Recursion](http://learnyousomeerlang.com/recursion)

<img src="https://qph.fs.quoracdn.net/main-qimg-8a9819c6a73d1a65ec1c6b7a221ececf.webp" width="500">

[Source code: rec.erl.](rec.erl)

**Recursion:**

```erlang
fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).
```

**What is the call stack?**

```erlang
f() ->
    g() + 1.

g() ->
    h() + 2.

h() ->
    0.
```

Call stack:

```
|        |       |        |       |        |       |        |       |        |
|        |       |        |       |        |       |        |       |        |
|        |  -->  |        |  -->  | h()    |  -->  |        |  -->  |        |
|        |       | g()    |       | g()    |       | g()    |       |        |
| f()    |       | f()    |       | f()    |       | f()    |       | f()    |
+--------+       +--------+       +--------+       +--------+       +--------+
```

We can check the stack trace with the following call:

```erlang
{backtrace, Trace} = process_info(self(), backtrace), io:format("~s~n", [Trace]).
```

Call stack of the previous `fac` function:

```
|        |       |        |       |        |                 | fac(1) |
|        |       |        |       |        |                 | fac(2) |
|        |  -->  |        |  -->  | fac(3) |  -->  ...  -->  | fac(3) |
|        |       | fac(4) |       | fac(4) |                 | fac(4) |
| fac(5) |       | fac(5) |       | fac(5) |                 | fac(5) |
+--------+       +--------+       +--------+                 +--------+
```

**Tail recursion:**

```erlang
fac(N) ->
    fac(N, 1).

% Return fac(N) * Acc.
%
% Example call chain:
%
%     fac(5, 1) ->
%     fac(4, 5) ->
%     fac(3, 4*5) ->
%     fac(2, 3*4*5) ->
%     fac(1, 2*3*4*5) ->
%     2*3*4*5
fac(1, Acc) ->
    Acc;
fac(N, Acc) ->
    % do_fac2(N, Acc) = N! * Acc = (N - 1)! * N * Acc = do_fac2(N - 1, N * Acc)
    fac(N - 1, N * Acc).
```

Call stack:

```
|             |       |             |       |             |
|             |       |             |       |             |
|             |  -->  |             |  -->  |             |  -->  ...
|             |       |             |       |             |
| fac(5, Acc) |       | fac(4, Acc) |       | fac(3, Acc) |
+-------------+       +-------------+       +-------------+
```

<img src="https://s3-us-west-2.amazonaws.com/tile-s3-static-bucket/prod/media/user_uploads/user_930/slide_contentimages/1502972805_1_RecursionJoke2.jpg" width="500">

2 Exercises
-----------

<img src="http://lh3.ggpht.com/-KAaL0NBbArk/TpXBOLrh3uI/AAAAAAAAFg0/zmD0MV1nAXc/s1600/python%25255B2%25255D.jpg" width="500">

[Étude 3: Atoms, Tuples, and Pattern Matching](https://github.com/cursorinsight/etudes-for-erlang/blob/master/ch03-tuples-and-records.asciidoc).

[Étude 4: Logic and Recursion](https://github.com/cursorinsight/etudes-for-erlang/blob/master/ch04-logic-and-recursion.asciidoc).
