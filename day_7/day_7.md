Erlang tutorial day 7
=====================

# ETS (Erlang Term Storage)

*ETS* is a simple, built-in database management system in Erlang, supporting destructive updates to tables, which are collections (of some kind) of tuples. The merits of ETS include being
- reliable,
- lightweight and
- native.

A table is a collection of tuples, or *objects*, with an appointed *key position*, controlled access based on an *owner* process of the table, and other settings/options.

`new(Name, [Option…])` → `Table` — create an empty table
- Name must be an atom, and each `Option` can be:
    - `Type` — the kind of collection, which can be:
        - `set` — a map (key ↦ maybe tuple) (the default)
        - `ordered_set` — a special map in which the tuples are always ordered by the elements in the key position
        - `bag` — a multimap (key ↦ set of tuples), equivalently a set
        - `duplicate_bag` — a multiset of tuples
        `ordered_set` tables generally use `==` for comparing key values, others use `=:=`
    - `{keypos, Index}` — the key position (default: 1)
        - Each tuple in the table has (is to have) at least as many elements as the key position.
        - Note that a record is a tuple tagged by the record-type. Use a key position greater than 1.
    - `Access`
        - `public` — any process can read from and write to the table
        - `private` — only the owner of the table has access to the table
        - `protected` — non-owners can only read from the table (the default)
    - `named_table` — use `Name` instead of an automatically generated number to refer to the table; without this option, the choice of `Name` has no real effect; `Name` is registered, there can be at most one named table with a given name
    - `Heir`
        - `{heir, Pid, Message}` — when the process dies, the table will be automatically transferred to Pid, which will furthermore receive the folloiwng message: `{'ETS-TRANSFER', Table, FromPid, Message}`
            - `Pid` must be local.
        - `{heir, none}` — the table will disappear along with its owner (the default setting)
- Returns an identifier (`Name` or an automatically generated number) with which the table can be referred to.

`rename(Table, NewName)` — change the name of the `Table` to `NewName`
- This has no real effect on unnamed tables.

```erl
1> T = ets:new(irrelevant, [ordered_set, {keypos, 2}]).
16400
```

`delete(Table)` — delete the existing Table
- Note: there's no garbage collection of tables.

`insert(Table, Object | [Object…])` — insert `Object`(s) into the `Table`
- In a `set` or `ordered_set` table, if Object matches a table element on the key position, then the `Object` overwrites that element. (For a `bag` or `duplicate_bag` table, there is no notion of overwriting.)
- If there are multiple `Object`s overriding a given object, which one overwrites the others is undefined.
- Insertion (even of `[Object…]` as a whole) is atomic and isolated.

`insert_new(Table, [Object…])` → `false | true` — insert `Object`(s) into the `Table`
- Only insert the `Object`s if they can all be added to the able without overwriting others.
- This operation is all-or-nothing, and atomic and isolated.
- Returns whether insertion happened.

```erl
2> ets:insert(T, [{good, 30.9}, {bad, 0.0}, {average, 25.4}]).
true
3> ets:insert(T, {very_bad, 0.0, "try again"}).
true
```

`member(Table, Key)` → `false | true` — is there a table element with `Key` in the key position?

`tab2list(Table)` → `[Object…]` — get the list of all elements in the `Table`

`lookup(Table, Key)` → `[Object…]` — get the list of `Table` elements with `Key` in the key position
- For `set` and `ordered_set` tables, the list has at most 1 element.
- For `set`, `bag` and `duplicate_bag` tables, lookup is a constant-time operation, regardless of the table size.

`lookup_element(Table, Key, Position)` → `Value | [Value…]` — get the `Position`th component(s) of `Table` element(s) with `Key` in the key position
- If there is no matching element, or if `Position` is out-of-bounds for some matching element, then a `badarg` exception is raised.
- For `set` and `ordered_set` tables, the component of the appropriate table element is returned.
- For `bag` and `duplicate_bag` tables, the (non-empty) list of component of the appropriate table element is returned.

```erl
4> ets:tab2list(T).
[{very_bad,0.0,"try again"},{average,25.4},{good,30.9}]
5> ets:member(T, 0).
true
```

`delete_all_objects(Table)` — clear the `Table`
- This call is atomic and isolated.

`delete(Table, Key)` — delete all table elements with `Key` in the key position

`delete_object(Table, Object)` — delete `Object` from `Table`
- In a `duplicate_bag` table, all instances of the object are deleted.
- `=:=` is used even for an `ordered_set` table.

`take(Table, Key)` → `[Object…]` − get the list of `Table` elements with `Key` in the key position, and remove those elements from the `Table`

`all()` → `[Table]` — get the list of all tables

`i()` — print an overview of the existing tables

`i(Table)` — interactively browse objects in the `Table`

`info(Table)` → [Option…] — get the list of settings/options for the `Table`

`info(Table, Item)` → Value — get where `Item` can be either a key as in `info/1`, or:
- `fixed` — `false | true`

```erl
6> ets:info(T).
[{compressed,false},
 {memory,121},
 {owner,<0.33.0>},
 {heir,none},
 {name,irrelevant},
 {size,3},
 {node,nonode@nohost},
 {named_table,false},
 {type,ordered_set},
 {keypos,2},
 {protection,protected}]
7> ets:new(tb, [named_table, duplicate_bag]).
tb
8> ets:insert(tb, [{1, orange}, {1, pear}, {2, banana}, {3, grape}, {3, pear}, {3, pear}, {4, grape}, {4, grape}]).
true
```

`first(Table)` → `Key | '$end_of_table'` — get a key from the elements in the `Table`
- For an `ordered_set` table, this returns the smallest (first) element in order.

`next(Table, Key)` → `Key | '$end_of_table'` — get the "next" key from the elements in the `Table`
- For an `ordered_set` table, this returns a key that is closest, but greater than Key in the table (or `'$end_of_table'`).
- Otherwise, this is based on a lookup which may fail with `badarg`. To avoid this, use `safe_fixtable/2`, or avoid concurrent deletions.

`last(Table)` → `Key | '$end_of_table'` — get a key from the elements in the `Table`
- For an `ordered_set` table, this is like `first/1`, but with "greatest (last)" instead of "smallest (first)".
- Otherwise, `last/1` is just `first/1`.

`prev(Table, Key)` → `Key | '$end_of_table'` — get the "previous" key from the elements in the `Table`
- For an `ordered_set` table, this is like `next/1`, but with "lesser" instead of "greater".
- Otherwise, `last/1` is just `first/1`.
As a consequence of the allocation of `'$end_of_table'` for `first/1`, `next/2`, `last/1` and `prev/2`, avoid the use of `'$end_of_table'` in key positions in tables.

```erl
9> ets:first(tb).
4
10> ets:lookup(tb, 4).
[{4,grape},{4,grape}]
11> ets:next(tb, 4).
3
12> ets:lookup(tb, 4).
[{4,grape},{4,grape}]
13> ets:delete_object(tb, {3, grape}).
true
14> ets:delete_object(tb, {3, pear}).
true
15> ets:lookup(tb, 3).
[]
16> ets:next(tb, 3).
** exception error: bad argument
     in function  ets:next/2
        called as ets:next(tb,3)
17> ets:all().
[8207,4110,13,file_io_servers,inet_hosts_file_byaddr,
 inet_hosts_file_byname,inet_hosts_byaddr,inet_hosts_byname,
 inet_cache,inet_db,global_pid_ids,global_pid_names,
 global_names_ext,global_names,global_locks,ac_tab]
18> ets:info(T).
undefined
```

`safe_fixtable(Table, false | true)` — fixate a table, or release a fixation, respectively
- Fixation is for safe traversal of a given table by `first/1`, `next/2`, `last/1` and `prev/2`.
- Fixations stack up for a table.
- While fixated (at least once) a table is fixated, a sequence of `first/1` and `next/2` on the table will be guaranteed to succeed, regardless of concurrent deletions.
- But during this time, the table's operational performance is lowered.

```erl
19> ets:new(tb, [named_table, duplicate_bag]).
tb
20> ets:insert(tb, [{1, orange, 50}, {1, pear, 80}, {2, banana, 70}, {3, grape, 1}, {3, pear, 90}, {3, pear, 100}, {4, grape, 2}, {4, grape, 3}]).
true
21> ets:safe_fixtable(tb, true).
true
22> ets:delete(tb, 2).
true
23> ets:next(tb, 2).
1
24> ets:safe_fixtable(tb, false).
true
```

`fun2ms(Function)` → `MatchSpecification`
- Must `-include_lib("stdlib/include/ms_transform.hrl")` in the relevant modules. This will rewrite each literal `ets:fun2ms(…)` call to the appropriate match specification (if possible, otherwise fail) at compile time. Otherwise, the call will remain as-is. This can cause a run-time error, because the real `ets:fun2ms/1` is to be used on shell-defined functions.
- The class of permitted `Function`s is very restricted; it must be `/1`, it may only call built-in functions, `if`/`case`/`receive`/etc constructs are not allowed (they don't have representations in a match specification), and no side-effects are allowed.
- However, (assigned) variables from the environment may be used.
- If there is an exception in a body of a match specification, then the result is `'EXIT'`.

`select(Table, MatchSpecification)` → `[Object…]` — get the result of a standard database select operation described by `MatchSpecification`
- In certain classes of match specifications, algorithmically efficient lookups may happen.

`select_reverse(Table, MatchSpecification)` → `[Object…]` — get the result of a standard database select operation described by `MatchSpecification`

`select(Table, MatchSpecification, Limit)` → `{[Answer…], Continuation} | '$end_of_table'`

`select(Continuation)` → `{[Answer…], Continuation} | '$end_of_table'`

```erl
27> ets:select(tb, ets:fun2ms(  fun({Number, Kind, Cost}) when Cost =< 30 * Number -> {Kind, Cost + Number} end  )).
[{grape,4},{pear,93},{grape,6},{grape,7}]
28> MS = ets:fun2ms(  fun({Number, Kind, Cost}) when Cost =< 30 * Number -> {Kind, Cost + Number} end  ).
[{{'$1','$2','$3'},
  [{'=<','$3',{'*',30,'$1'}}],
  [{{'$2',{'+','$3','$1'}}}]}]
```

```erl
-module(sc).
-compile([export_all]).

test_sc(Table, MatchSpec, Limit) ->
	continue_sc(ets:select(Table, MatchSpec, Limit)).

continue_sc({Answers, Continuation}) ->
	io:format("~p~n", [Answers]),
	continue_sc(ets:select(Continuation));
continue_sc('$end_of_table') ->
	done.
```

```erl
27> c(sc).
{ok,sc}
28> sc:test_sc(tb, MS, 2).
[{grape,6},{grape,7}]
[{grape,4},{pear,93}]
done
```

`select_delete(Table, MatchSpecification)` → `DeletionCount` — delete elements from `Table` for which the `MatchSpecification` call returns `true`

`select_replace(Table, MatchSpecification)` → `ReplacementCount` — transforms elements in the `Table` according to the `MatchSpecification`
- The transformation must retain the key of any given object, otherwise there will be a `badarg` exception.
- For each object, the match-and-replace operations form an atomic and isolated operation.
- Currently, `bag` tables are not supported.

`foldl(Fun, Acc0, Table)` — fold the elements of `Table`
- `Fun` must be `(X, Acc)` → `XAcc`.
- For `ordered_set` tables, traversal is in order.
- Fun may insert objects into the table!

`foldr(Fun, Neut, Table)` — fold the elements of `Table`
- Like `foldl/3`, but with "in reverse order" instead of "in order".

`setopts(Table, [Option…])` — reconfigure the `Table`
- The caller must be the owner of the process.
- Practically, Option can only be `{heir, none}` or `{heir, Pid, Message}`, in which `Pid` must be local.

`give_away(Table, Pid, Message)` — transfer owner
- The caller must be the owner of the process.
- `Pid` must be alive and local, but must not be the caller.
- This does not affect the heir option of the table. Idea: set the heir to be `self()`, and give the table away to a temporal process.



# Query List Comprehensions

Problem: Find the names of people who own Trabants made before the year 2000.
Solution: `qlc:e(qlc:q([Name || {PersonID, Name} <- ets:table(person), {_CarID, Owner, trabant, Year} <- ets:table(car), Year < 2000, PersonID =:= Owner], unique)).`

This looks like a naturally expressed query, and it's computed efficiently using a standard lookup-based table joining technique. How?

Effectively, the concept of *query list comprehension*s extends basic list comprehensions with new generators: *query handle*s.
- `ets:table(Table)` is a query handle generating the elements of the `Table` (by iterating over it).
- `qlc:q(QueryLC)`, where `QueryLC` is an "extended" list comprehension, constructs a query handle with the obvious semantics.
    - `qlc:q/1,2` works much like `ets:fun2ms/1`. Need to include `-include_lib("stdlib/include/qlc.hrl")` in modules.
- `qlc:q(QueryLC, unique)` also causes the removal of duplicate elements from the result.
- `qlc:q(QueryLC, cache)` comes with a modification that each generator is drained at most once, and temporarily cached for subsequent passes.
- `qlc:sort(QueryHandle)` wraps `QueryHandle` in a sorting phase.
`qlc:e(QueryHandle)`, aka `qlc:eval(QueryHandle)` evaluates the query handle, and returns the list of elements generated.
- The `cache_all` and `unique_all` options to `qlc:e/2` spreads the cache and unique (respectively) option deeply to all query handles (sub query handles, etc).

In line with this, in a QLC, a generator expression may not depend on results from previous generators.

Evaluation of a QLC proceeds in 2 steps:
1. The structure of the QLC is analyzed, some query optimizations are done.
- `qlc:sort(qlc:q(QueryLC, unique))` is calculated as: get all answers, sort (if needed), remove adjacent duplicates according to `==`.
- `qlc:q([… || {A, …} <- QH1, {B, …} <- QH2, A =:= B])` is calculated using a join operation technique; even filters may be added, one after each generator.
2. Generator expressions are evaluated (in a unspecified order).

The above optimizations can be requested at compile time by specifying option `{join, lookup}` or `{join, merge}` to `qlc:q/2`.

`qlc:info(QH)` — get a plan description

```erl
1> ets:new(person, [named_table]).
person
2> ets:new(car, [named_table]).
car
3> io:format("~s~n", [qlc:info(qlc:q([Name || {PersonID, Name} <- ets:table(person), {_CarID, Owner, trabant, Year} <- ets:table(car), Year < 2000, PersonID =:= Owner], unique))]).
begin
    V1 =
        qlc:q([
               SQV ||
                   SQV <-
                       ets:table(car,
                                 [{traverse,
                                   {select,
                                    [{{'_','$1',trabant,'$2'},
                                      [{'<','$2',2000}],
                                      ['$_']}]}}])
              ],
              [{unique,true}]),
    V2 =
        qlc:q([
               P0 ||
                   P0 = {_CarID,Owner,trabant,Year} <- V1
              ]),
    V3 =
        qlc:q([
               [G1|G2] ||
                   G2 <- V2,
                   G1 <- ets:table(person),
                   element(2, G1) =:= element(1, G2)
              ],
              [{join,lookup},{unique,true}]),
    qlc:q([
           Name ||
               [{PersonID,Name}|{_CarID,Owner,trabant,Year}] <- V3
          ],
          [{unique,true}])
end
ok
```

Draining of a query handle can be done in chunks by use of a query cursor:

`cursor(QH)` → `Cursor` — spawn a cursor
- The caller process becomes the owner of the cursor. Only the owner may drain and delete the cursor.

`next_answers(Cursor, Amount)`, where `Amount` is a positive integer or `infinity` (default: 10)
- Can be called even after returning less than `Amount` answers, will just return `[]`.

`delete_cursor(Cursor)` — delete the `Cursor`

`append(QH1, QH2)` — compose query handles: concatenate the two query handles

`append([QH…])` — compose query handles: concatenate the query handles in order

`fold(Fun, Acc0, QH)` — evaluate `QH` and fold its output
- `Fun` must be `(X, Acc)` → `XAcc`.



# DETS (Disk-ETS)

*DETS* is the disk/file-backed variant of ETS.
The API and semantics for DETS tables are almost the same as for ETS tables.

Tables must be opened and properly closed (beware of ^C).
Multiple processes can own the same table.
There's no `ordered_set` table type.
There is no `safe_fixtable/2`-like support for concurrent modification of the table.
There's a 2GiB size limit for each table.

Unless deferred, writing to DETS tables is much slower than writing to ETS tables.

`open_file(Name, [Option…])` → `{ok, Table}` — open a table, create an empty one if needed
- `Option` can be, for example:
    - `{file, Path}` — use the file at `Path` for disk storage
    - `{ram_file, Path}` — keep the (modified) table in memory
    Note: use `sync/1` or close the table in time.
- `Table` is `Name`.

`open_file(Path)` → `{ok, Table}` — open an existing table stored at `Path`
- `Table` is a reference.

`close(Table)` — close the (opened) `Table`

`sync(Table)` — ensure that all updates made to `Table` are written to the disk
- In particular, flush a `ram_file` to the disk.

See also: `to_ets/2`, `from_ets/2`, `ets:to_dets/2` and `ets:from_dets/2`.

# Exercise

Consider an imaginary (role playing) computer game that involves the objective of putting together the epic combinations of *gun*s and *ammunition*.

A gun consists of a *base* (e.g. pistol or rocket launcher) and a set of *upgrade*s (e.g. no upgrades, or the upgrades of gold coating plus aiming assistant). Guns use *ammunition*, which is of some *kind* (e.g. bullet, rocket or petroleum), and contains an active *load* (e.g. plain mass, explosive or acid). However, not every gun base is compatible with every kind of ammunition (e.g. a pistol is not compatible with a rocket), and not every load is sensible in every kind of ammunition (e.g. plain mass is not sensible in petroleum).

The potential of a combination is rated by some attributes (e.g. accuracy, firing rate or ammunition cost); some are *positive* (higher value is better, e.g. accuracy), others are *negative* (higher value is worse, e.g. cost). The attribute values depend first on the gun base, and are further influenced by the upgrades and ammunition, additively.

Details:
- gun bases: `pistol`, `rifle`, `machine_pistol`, `assault_rifle`, `rocket_launcher`, `sprayer_handgun`, `sprayer_set`
- gun upgrades: `scope`, `external_clip`, `aiming_assistant`, `gold_coating`
- ammunition kinds: `bullet`, `buckshot`, `grenade`, `rocket`, `missile`, `petroleum`, `acid`
- ammunition loads: `plain_mass`, `plain_explosive`, `plain_flammable`, `plain_acid`, `explosive`, `flammable`, `electromagnetic_charge`, `poison`, `acid`, `virus`, `radioactive`
- ammunition kind compatibilities with guns:
    - `bullet`, `buckshot` with `pistol`, `rifle`, `machine_pistol`, `assault_rifle`
    - `grenade`, `rocket`, `missile` with `rocket_launcher`
    - `petroleum`, `acid` with `sprayer_handgun`, `sprayer_set`
- load sensibilities in ammunition kinds:
	- `plain_mass`, `high_explosive`, `flammable`, `poison`, `acid`, `virus`, `radioactive` in `bullet`, `buckshot`
	- `plain_explosive`, `flammable`, `electromagnetic_charge`, `poison`, `acid`, `radioactive` in `grenade`, `rocket`, `missile`
	- `plain_flammable`, `poison`, `radioactive` in `petroleum`
	- `plain_acid`, `poison`, `virus`, `radioactive` in `acid`
- attributes: `accuracy`, `effective_range`, `clip_capacity`, `firing_rate`, `reliability`, `cost`, `ammo_availability`

Problem:
- Let there be a process that randomly generates gun bases with attributes into a table of *manufacturable* gun bases; the values should depend crucially on the gun base (e.g. a pistol is usually less accurate than a rifle). Likewise for gun upgrades, ammunition kinds and ammunition loads.
- Let there be a process that periodically searches for and removes obviously inferior elements (guns bases, etc.). An element is obviously inferior if it is worse (or at least not better) on all attributes than some other element (of the same class).
- Let there be a table (set) of metrics, to be used to compare combinations. `40 * accuracy + 2 * effective_range - 3 x weight`. This table may be expanded (by the user) over time.
- Let there be a process that, for each metric, collects a few of the best, manufacturable, compatible and sensible combinations a table.
