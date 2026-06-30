# The Scandalous Truth: Graphs Mutate

Here `digraph` does something faintly scandalous for an Erlang data structure: it *mutates*. Where tuples and maps cling to immutability like a cherished principle, a digraph is secretly a cluster of ETS tables wearing a trench coat — a *reference* to shared, mutable state rather than a value you can pass around with a clear conscience. Operations change the graph in place, which is enormously convenient and exactly the sort of thing that will surprise you at 3 a.m. if you forget it. Pass a digraph to two processes expecting two independent copies and you will get one graph and two confused processes.

Everything else in this Part has trained you to expect a particular bargain: a function takes a value and returns a *new* value, the original sitting serenely unchanged, immutable and eternal. `setelement` doesn't set; `maps:put` doesn't mutate; the old version is always still there if you kept a name bound to it. This bargain is the bedrock of Erlang's concurrency story, and `digraph` quietly breaks every clause of it. Understanding *how* it breaks them is the difference between using graphs confidently and being ambushed by them.

## A Digraph Is a Reference, Not a Value

When you write `(set g (digraph:new))`, the variable `g` does not hold a graph. It holds a *handle* — a small tuple of references to ETS tables where the actual vertices and edges live. Every `digraph` function you call reaches through that handle and modifies the shared tables behind it. There is no copy. There is no new version. There is one graph, and `g` is a finger pointing at it.

The consequences ripple outward immediately. Consider:

```lfe
lfe> (set g (digraph:new))
lfe> (digraph:add_vertex g 'marvin)
marvin
lfe> (set h g)
lfe> (digraph:add_vertex h 'zaphod)
zaphod
lfe> (digraph:vertices g)
(marvin zaphod)
```

Binding `h` to `g` did not copy the graph; it made a second finger pointing at the *same* graph. Adding a vertex "through `h`" adds it to the one and only graph, and `g` sees it too, because `g` and `h` were always the same graph wearing two names. Every other data structure in this book would have given you two independent values here. The digraph gives you one, and a lesson.

## Only the Owner May Write

Because the tables are ETS tables, the ordinary ETS ownership rules apply, and they are stricter than you might guess. **Only the process that created the digraph is allowed to update it.** Another process may *read* a `protected` graph (the default) — call `vertices`, `get_path`, `topsort` to its heart's content — but any attempt to *modify* it from a process other than its creator will fail. A `private` graph (see [Conjuring a Graph](create.md)) goes further and forbids even reading from the outside.

This has a sharp practical edge: a digraph is **not** a thing you casually send in a message to another process and expect to keep working with. You can send the handle — it is just a term — but the receiving process cannot modify the graph, and if the *creating* process dies, the graph dies with it (more on that below), leaving the recipient holding a handle to a graph that has ceased to be. If you need a graph shared across processes, you build it in one long-lived owner process and have that process mediate all changes. The graph does not parallelise the way an immutable value does, and pretending otherwise is a fine recipe for a genuinely baffling bug.

## No Garbage Collection — You Must `delete`

Here is the part that catches people. Every immutable value you have ever created in Erlang gets cleaned up automatically: when the last reference goes away, [the cosmic janitor](../tuples/creating.md) that is the garbage collector sweeps in and reclaims the memory. ETS tables are different. **Digraphs are not garbage collected.** The tables backing a graph persist until one of exactly two things happens:

1. You call `digraph:delete/1` on the graph, explicitly freeing its tables; or
2. The process that created the graph terminates, at which point its ETS tables are released along with everything else it owned.

```lfe
lfe> (set g (digraph:new))
lfe> ;; ... build, query, use the graph ...
lfe> (digraph:delete g)
true
```

In a short-lived worker — a process spawned to do one job, build a graph along the way, and then exit — point (2) saves you: the graph is cleaned up when the worker dies, and you can be a little careless. But in a long-lived process — a `gen_server` that handles request after request, building a throwaway graph for each — point (2) never arrives, and a forgotten `delete/1` means every request leaks a fistful of ETS tables. Do this a few hundred thousand times and you will meet the bug at 3 a.m., cold coffee in hand, watching memory climb for reasons that the immutable rest of your codebase has left you wholly unprepared to suspect.

The rule, then, is simple and unforgiving: **if you created it, you delete it** — unless you are certain the owning process is about to die anyway. Treat `digraph:new` and `digraph:delete` the way a careful C programmer treats `malloc` and `free`, because under the trench coat that is very nearly what they are.

## Making Peace With It

None of this is a defect. The mutability is *why* digraphs are fast and convenient for the graph algorithms in [the power tools](utils.md): building a ten-thousand-vertex graph by threading an immutable structure through ten thousand function calls would be a misery of allocation, whereas mutating ETS tables in place is brisk and pleasant. The standard library made a deliberate, sensible trade — performance and convenience in exchange for surrendering immutability — and it was the right call for the job. It simply asks one thing of you in return: that you remember the trade exists. Hold the handle, respect the ownership, delete when you're done, and the digraph will serve you well. Forget, and the trench coat comes off at the worst possible moment.
