Erlang tutorial day 3
=====================

<img src="https://cdn.pixabay.com/photo/2015/07/18/09/57/list-850188_960_720.png" width="400">

Table of Contents
-----------------

1. [Lists](#1-lists)
2. [Strings](#2-strings)
3. [Exercises](#3-exercises)

1 Lists
-------

### The list data structure ([Starting out (for real)](http://learnyousomeerlang.com/starting-out-for-real))

*   Lists are heterogeneous and immutable:

    ```erl
    A = [1, 2, 3]
    ```

    Representation:

    ```
      A
    +---+---+     +---+---+      +---+---+
    | 1 | ----->  | 2 | ----->   | 3 | ----->    []
    +---+---+     +---+---+      +---+---+
    ```

*   Manipulating the beginning of a list is cheap:

    ```erl
    > B = [0|A]
    [0, 1, 2, 3]
    ```

    Representation:

    ```
      B             A
    +---+---+     +---+---+     +---+---+      +---+---+
    | 0 | ----->  | 1 | ----->  | 2 | ----->   | 3 | ----->    []
    +---+---+     +---+---+     +---+---+      +---+---+
    ```

*   Manipulating the end of a list is expensive:

    ```erl
    > C = A ++ [0]
    [1, 2, 3, 0]
    ```

    Representation:

    ```
      A
    +---+---+     +---+---+      +---+---+
    | 1 | ----->  | 2 | ----->   | 3 | ----->    []
    +---+---+     +---+---+      +---+---+

      C
    +---+---+     +---+---+      +---+---+      +---+---+
    | 1 | ----->  | 2 | ----->   | 3 | ----->   | 0 | ----->    []
    +---+---+     +---+---+      +---+---+      +---+---+
    ```

*   Heads and tails:

    ```erl
    > B = [0, 1, 2, 3].

    > hd(B).
    0

    > tl(B).
    [1,2,3]

    > [Head|Tail] = B.
    [0,1,2,3]

    > Head
    0

    > Tail
    [1,2,3]
    ```

*   Operations on lists:

    ```erl
    > length([1, 2, 3]).
    3

    > [1, 2] ++ [3, 4].
    [1,2,3,4]

    > [1, 2, 3, 4] -- [2, 4].
    [1,3]

    > lists:member(2, [1,2,3]).
    true

    > lists:reverse([1, 2, 3]).
    [3,2,1]

    > lists:flatten([[1, 2], [3], [[4]]]).
    [1,2,3,4]

    > lists:append([[1, 2], [3], [[4]]]).
    [1,2,3,[4]]
    ```

*   Pattern matching on lists:

    ```erl
    case List of
        % Empty list
        [] ->
            ...
        % Non-empty list
        [H|T] ->
            ...
    end

    case List of
        % List with one element
        [Item] ->
            ...
        % List that starts with [1, 2, 3]
        [1, 2, 3] ++ _ ->
            ...

        % Long list
        _ when length(List) > 100 ->
            ...

    end
    ```

*   [Type specifications](http://erlang.org/doc/reference_manual/typespec.html):

    ```erl
    [integer()]  % A list of integers
    [term()]     % A list of anything
    [any()]      % A list of anything
    list()       % A list of anything
    []           % Empty list

    [integer()] | [float()]  % A list of integers or a list of floats
    [integer() | float()]    % A list of numbers
    ```

    Note:

    * `{atom(), integer()}` = A tuple with two elements. There is no equivalent
      syntax for lists.
    * `[atom()]` = A list of atoms. There is no equivalent syntax for tuples.

### 1.2 Recursion with lists ([Recursion](http://learnyousomeerlang.com/recursion))

*   Lists and recursion. ([mylist.erl](mylist.erl) contains the full source code.)

    Naive recursion:

    ```erl
    %% Return the length of a list (same as `length/1`)
    -spec len([term()]) -> non_neg_integer().
    len([]) ->
        0;
    len([_H|T]) ->
        1 + len(T).
    ```

    Tail recursive recursion:

    ```erl
    %% Return the length of a list (same as `length/1`)
    %% (tail recursive solution)
    -spec len2([term()]) -> non_neg_integer().
    len2(List) ->
        len2(List, 0).

    %% Calculate `len2(List) + Acc`
    -spec len2([term()], non_neg_integer()) -> non_neg_integer().
    len2([], Acc) ->
        Acc;
    len2([_H|T], Acc) ->
        len2(T, Acc + 1).
    ```

    Tail recursive recursion when the accumulator is a list:

    ```erl
    %% Reverse a list (same as `lists:reverse/1`)
    -spec reverse([term()]) -> [term()].
    reverse(List) ->
        reverse(List, []).

    %% Calculate `reverse(List) ++ Acc`
    -spec reverse([term()], [term()]) -> [term()].
    reverse([], Acc) ->
        Acc;
    reverse([H|T], Acc) ->
        reverse(T, [H|Acc]).
    ```

    This function works by building up the `Acc` variable as it is
    deconstructing the original `List` parameter:

    ```
    ======================================================================
     List
    +---+---+     +---+---+      +---+---+
    | 1 | ----->  | 2 | ----->   | 3 | ----->    []
    +---+---+     +---+---+      +---+---+

                                                 Acc
                                                 []
    ======================================================================
                   List
                  +---+---+      +---+---+
                  | 2 | ----->   | 3 | ----->    []
                  +---+---+      +---+---+

                                  Acc
                                 +---+---+   
                                 | 1 | ----->    []
                                 +---+---+   
    ======================================================================
                                  List
                                 +---+---+
                                 | 3 | ----->    []
                                 +---+---+
                    Acc
                   +---+---+     +---+---+   
                   | 2 | ----->  | 1 | ----->    []
                   +---+---+     +---+---+   
    ======================================================================
                                                 List
                                                 []
     Acc
    +---+---+      +---+---+     +---+---+   
    | 3 | ----->   | 2 | ----->  | 1 | ----->    []
    +---+---+      +---+---+     +---+---+   
    ======================================================================
    ```

### 1.3 Strings ([Starting out (for real)](http://learnyousomeerlang.com/starting-out-for-real))


*   Characters are numbers:

    ```erl
    > $a.
    97
    ```

*   Strings are lists:

    ```erl
    > [97, 98, 99].
    "abc"
    > [$a, $b, $c].
    "abc"
    > "".
    []
    ```

*   Using `io:format` to print lists:

    ```erl
    % ~p: Try to be smart
    > io:format("~p~n", ["abc"]).
    "abc"
    > io:format("~p~n", ["ab\0"]).
    [97,98,0]

    % ~w: Always print as list of numbers
    > io:format("~w~n", ["abc"]).
    [97,98,99]
    > io:format("~w~n", ["ab\0"]).
    [97,98,0]

    % ~s: Always print as string
    > io:format("~s~n", ["abc"]).
    abc
    > io:format("~s~n", ["ab\0"]).
    ab^@
    ```

*   Pattern matching on strings:

    ```erl
    case Str of
        % Empty string (i.e. empty list)
        "" ->
            ...
        % String that starts with "version-1.0-"
        "version-1.0-" ++ _ ->
            ...
    end
    ```

    My favorite real-world example:

    ```erl
    case lists:reverse(Filename) of
        "piz.dorptset." ++ _ ->
            do_something();
        _ ->
            do_something_else()
    end
    ```

*   Binaries are usually better for strings than lists:

    ```erl
    > <<"abc">>.
    <<"abc">>
    ```

    * They are quicker to handle (except when modified)
    * They use much less memory

*   Regular expressions:

    Search:

    ```erl
    > re:run("abc", "b").
    {match,[{1,1}]}

    > re:run("abc", "b", []).
    {match,[{1,1}]}

    > re:run("abc", "b", [{capture, none}]).
    match
    ```

    Replace:

    ```erl
    > re:replace("abcd", "c", "[&]", [{return, list}]).
    "ab[c]d"

    > re:replace("abcd", "c", "[&]", [{return, binary}]).
    <<"ab[c]d">>

    > S = re:replace("abcd", "c", "[&]", []).
    [<<"ab">>,[<<"[">>,<<"c">>,<<"]">>]|<<"d">>]

    > io:format("~s~n", [S]).
    ab[c]d
    ```

*   `iodata()`:

    - Faster to construct than either binaries or strings.
    - Conversion to string: `unicode:characters_to_list(IoData)`
    - Conversion to binary: `unicode:characters_to_binary(IoData)`
      
*   Conversions:

    ```erl
    > list_to_integer("42").
    42
    > list_to_float("3.14").
    3.14

    > string:to_integer("42").
    {42,[]}
    > string:to_float("3.14").
    {3.14,[]}
    ```

    They behave differently if the string is not a valid number.

*   Reading input:

    ```erl
    > io:get_line("What is your name? ").
    What is your name? Joe
    "Joe\n"
    ```

*   [Type specifications](http://erlang.org/doc/reference_manual/typespec.html):

    ```erl
    char()    % 0..1114111
    string()  = [char()]

    binary()  % A sequence of bytes

    byte()    % 0..255
    iolist()  = [byte() | binary() | iolist()]   % Simplified
    iodata()  = iolist() | binary()
    ```

    Note:

    * `white | black` = Either the `white` or the `black` atom. There is no
      equivalent syntax for strings (or binaries).

3 Exercises
-----------

<img src="https://c1.staticflickr.com/7/6101/6301987888_d906c752ab_b.jpg" width="400">

* [Étude 5-2: Using the `re` Module](https://github.com/cursorinsight/etudes-for-erlang/blob/master/ch05-strings.asciidoc#Étude-5-2-using-the-re-module).
* [Étude 6: Lists](https://github.com/cursorinsight/etudes-for-erlang/blob/master/ch06-lists.asciidoc).
    * Bonus: Interlude section
* Bonus: [Étude 5-1: Validating Input](https://github.com/cursorinsight/etudes-for-erlang/blob/master/ch05-strings.asciidoc#Étude-5-1-validating-input).
