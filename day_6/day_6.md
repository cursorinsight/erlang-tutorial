Erlang tutorial day 6
=====================

<img src="https://qph.fs.quoracdn.net/main-qimg-a5609d6368de25cd8248acfa5bc82c13" width="400">

Table of Contents
-----------------

1. [Compile-time errors](#1-compile-time-errors)
2. [Logical errors](#2-logical-errors)
3. [Run-time errors](#3-run-time-errors)
4. [Exceptions](#4-exceptions)
5. [Error logging](#5-error-logging)
6. [Exercises](#6-exercises)
7. [Readings](#7-readings)

1 Compile-time errors
---------------------

[List of compilation errors][errors-learn-you-some-erlang]

Example code ([no_module.erl](no_module.erl), [syntax_error.erl](syntax_error.erl), [warnings.erl](warnings.erl))

```erl
1> c(no_module).
no_module.erl:1: no module definition
error

2> c(syntax_error).
syntax_error.erl:3: syntax error before: '.'
error

3> c(warnings).
warnings.erl:3: Warning: function unused_fun/1 is unused
warnings.erl:3: Warning: variable 'UnusedVar' is unused
{ok, warnings}
```

2 Logical errors
----------------

Example code ([logical_error.erl](logical_error.erl))

```erl
1> c(logical_error).
{ok,logical_error}
2> logical_error:fibonacci(6).
8
3> logical_error:fibonacci(-1).
...
^C
```

Preventions:

* eunit, ct (common test), ...

3 Run-time errors
-----------------

```erl
1> {ok, Result} = {ok, some, return_value}.
** exception error: no match of right hand side value {ok,some,return_value}
```

```erl
1> lists:magical_sort([1,2,3,2,1]).
** exception error: undefined function lists:magical_sort/1
```

4 Exceptions
------------

4.1 Classes (error, exit, throw)
----------

```erl
1> error(badarith).
** exception error: an error occurred when evaluating an arithmetic expression

2> error(my_error).
** exception error: my_error

3> exit(exception).
** exception exit: exception

4> throw(exception).
** exception throw: exception
```

[Source code of throw module](throw.erl)

```erl
1> Map = #{key1 => value1}.
#{key1 => value1}
2> maps:get(key1, Map).
value1
3> maps:get(key2, Map).
** exception error: {badkey,key2}
     in function  maps:get/2
        called as maps:get(key2,#{key1 => value1})
4> throw:get_from_map(key1, Map).
value1
5> throw:get_from_map(key2, Map).
undefined
```

4.2 Type specification
----------------------

```erl
no_return()
no_return() | integer() % this will be integer()
```

4.3 Handling
------------

<img src="https://pics.me.me/do-or-do-not-there-is-no-try-there-is-19341826.png" width="400"/>

```erl
1> try 1/0
   catch
     error:badarith -> infinity
   end.
infinity
2> try 1/1 of
     1 -> {ok, 1}
   catch
     Class:Reason -> {Class, Reason}
   end.
{ok, 1}
```

```erl
try function_may_raise_exception() of
  Val1 -> ...;
  Val2 -> ...;
  ...
catch
  Class1:Reason1 -> ...;
  Class2:Reason2 -> ...;
  ...
after % this always gets executed and this block will not return anything
  ...
end.
```

5 Error logging
---------------

<img src="http://i0.kym-cdn.com/entries/icons/original/000/000/157/itsatrap.jpg" width="400">

5.1 Info message
----------------

```erl
1> error_logger:info_msg("What is love?").
ok

=INFO REPORT==== 1-Jan-1900::00:00:01 ===
What is love?
```

5.2 Warning message
-------------------

```erl
2> error_logger:warning_msg("Baby don't hurt me! Don't hurt me!").
ok

=WARNING REPORT==== 1-Jan-1900::00:00:02 ===
Baby don't hurt me! Don't hurt me!
```

5.3 Error message
-----------------

```erl
3> error_logger:error_msg("No more!").
ok

=ERROR REPORT==== 1-Jan-1900::00:00:03 ===
No more!
```

6 Exercises
-----------

<img src="https://img-9gag-fun.9cache.com/photo/266277_700b.jpg" width="400">

* [Étude 9-1: try and catch](https://github.com/cursorinsight/etudes-for-erlang/blob/master/ch09-errors.asciidoc#%C3%89tude-9-1-try-and-catch)
* [Étude 9-2: Logging Errors](https://github.com/cursorinsight/etudes-for-erlang/blob/master/ch09-errors.asciidoc#%C3%89tude-9-2-logging-errors)

7 Readings
----------

* [Errors and Error Handling -- Official](http://erlang.org/doc/reference_manual/errors.html)
* [Errors and Exceptions -- learn you some Erlang][errors-learn-you-some-erlang]
* [Error handling in Erlang - a primer](http://blog.thedigitalcatonline.com/blog/2013/05/30/error-handling-in-erlang-a-primer/)
* [Let it crash (the right way…)](https://mazenharake.wordpress.com/2009/09/14/let-it-crash-the-right-way/)

[errors-learn-you-some-erlang]: http://learnyousomeerlang.com/errors-and-exceptions
