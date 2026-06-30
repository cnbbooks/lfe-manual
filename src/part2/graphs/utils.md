# The Power Tools: digraph_utils

The `digraph` module builds and modifies graphs; the `digraph_utils` module *reasons* about them. Where `digraph` lets you add an edge, `digraph_utils` lets you ask whether the whole tangled mess can be put in order, which parts of it are mutually reachable, and whether it secretly contains a cycle you would very much rather it didn't. Everything here is built on depth-first traversal, and everything here takes a graph you have already built with `digraph` and hands back an answer — usually a plain list of vertices, which is a refreshing change from the opaque handles of the previous sections.

## Topological Sorting: `topsort/1`

This is the function that justifies the entire module for most working programmers. A **topological sort** of a graph is a linear ordering of its vertices in which, for every vertex, none of its out-neighbours appears earlier in the list. Put less formally: if `a` points to `b`, then `a` comes before `b`. This is exactly the order in which you must perform tasks when some tasks depend on others — build order, load order, the order in which to wake up the galaxy's subsystems so that nothing boots before the thing it relies on.

```lfe
lfe> (set g (digraph:new '(acyclic)))
lfe> (lc ((<- v '(power core drive hull ship)))
       (digraph:add_vertex g v))
(power core drive hull ship)
lfe> (digraph:add_edge g 'power 'core)
lfe> (digraph:add_edge g 'core 'drive)
lfe> (digraph:add_edge g 'drive 'ship)
lfe> (digraph:add_edge g 'hull 'ship)
lfe> (digraph_utils:topsort g)
(hull power core drive ship)
```

Every prerequisite precedes the thing that needs it: `power` before `core`, `core` before `drive`, everything before `ship`. Note that the ordering is *not unique* — `hull` and `power` have no dependency between them, so a different valid run could just as legitimately swap them. `topsort/1` promises *a* valid order, not *the* valid order; never write code that depends on which one you got.

And the crucial failure case: if the graph contains a cycle, **no** topological ordering can exist, and `topsort/1` returns `false`:

```lfe
lfe> (set bad (digraph:new))
lfe> (lc ((<- v '(chicken egg)))
       (digraph:add_vertex bad v))
(chicken egg)
lfe> (digraph:add_edge bad 'chicken 'egg)
lfe> (digraph:add_edge bad 'egg 'chicken)
lfe> (digraph_utils:topsort bad)
false
```

A `false` from `topsort/1` is the standard library's polite way of telling you that you have a circular dependency. (Notice we had to build `bad` as a *cyclic* graph for the cycle to get in at all — an `acyclic` graph would have refused the second edge outright, per [Edges](edges.md). The two mechanisms are complementary: build acyclic to *prevent* cycles, or build cyclic and `topsort` to *detect* them.)

## Detecting Cycles: `is_acyclic/1`

If all you want is a yes-or-no answer, `digraph_utils:is_acyclic/1` returns `true` if and only if the graph has no cycles:

```lfe
lfe> (digraph_utils:is_acyclic g)
true
lfe> (digraph_utils:is_acyclic bad)
false
```

## Reachability: `reachable/2` and `reaching/2`

The reachability functions answer "what can I get to?" and its mirror image "what can get to me?". `digraph_utils:reachable/2` takes a *list* of starting vertices and a graph, and returns every vertex reachable from any of them by a path of length zero or more — which, the "length zero" being the operative phrase, means the starting vertices are themselves always included:

```lfe
lfe> (digraph_utils:reachable '(core) g)
(ship drive core)
```

From `core` you can reach `drive`, then `ship`, and `core` counts as reaching itself. Its mirror, `digraph_utils:reaching/2`, returns every vertex that can reach the given ones — following the arrows *backwards*:

```lfe
lfe> (digraph_utils:reaching '(ship) g)
(power core drive hull ship)
```

Everything that must exist before `ship` can: the entire ancestry. These two functions are the backbone of impact analysis — "if I change this module, what is affected?" is `reaching`; "what does this module pull in?" is `reachable`.

Each has a `_neighbours` variant — `reachable_neighbours/2` and `reaching_neighbours/2` — that demands a path of length **one or more**, thereby *excluding* any starting vertex that cannot get back to itself. The practical effect is that a starting vertex only appears in the result if it sits on a cycle, which makes the `_neighbours` forms a slightly oblique way of asking "is this vertex part of a loop?"

## Connected Components: `components/1` and `strong_components/1`

A **connected component** is a maximal clump of vertices all reachable from one another *if you ignore edge direction*. `digraph_utils:components/1` carves the graph into these clumps, each represented by its list of vertices, every vertex appearing in exactly one:

```lfe
lfe> (digraph_utils:components g)
((hull ship drive core power))
```

Our ship is all one piece, so it forms a single component. A **strongly connected component**, by contrast, respects direction: it is a maximal set of vertices in which there is a directed path from each to every other. `digraph_utils:strong_components/1` finds these, and `digraph_utils:cyclic_strong_components/1` returns only the ones that actually contain a cycle (discarding the lonely single-vertex components that are technically "strongly connected" only in the trivial sense of a vertex reaching itself):

```lfe
lfe> (digraph_utils:strong_components bad)
((egg chicken))
lfe> (digraph_utils:cyclic_strong_components bad)
((egg chicken))
```

The chicken and the egg, correctly identified as inseparable.

## Traversal Orders: `preorder/1` and `postorder/1`

For when you want the vertices in depth-first-traversal order rather than topological order, `digraph_utils:preorder/1` returns them as collected on the way *down* (a vertex before its descendants), and `digraph_utils:postorder/1` as collected on the way back *up* (a vertex after its descendants). Both visit every vertex exactly once.

## The Specialist Tools

The remainder of the module is a drawer of specialist instruments you will reach for rarely but be glad exist when you do:

- **`is_tree/1`** — `true` if the graph is a tree (acyclic, with a unique undirected path between every pair of vertices).
- **`is_arborescence/1`** and **`arborescence_root/1`** — the former tests whether the graph is an *arborescence* (an acyclic graph with a root from which there is a unique directed path to every other vertex); the latter returns `#(yes Root)` or `no`. An arborescence is, essentially, a tree that has made up its mind about which way is down.
- **`loop_vertices/1`** — the list of all vertices that sit on some loop.
- **`subgraph/2,3`** — builds a brand-new graph containing only the vertices you name and the edges among them. Note well: this *creates a new digraph*, with its own ETS tables, which means it is your responsibility to `delete` when you are done. The power tools are not exempt from the chapter's central obligation.
- **`condensation/1`** — collapses each strongly connected component down to a single vertex, yielding a new (and necessarily acyclic) graph that shows the large-scale shape of a cyclic one. Also a fresh digraph; also yours to delete.

These last two are the first hint of the recurring caveat we keep deferring — that these graphs are mutable, ETS-backed, reference-typed things that allocate real tables and demand real cleanup. We have deferred it long enough. It is time to face the scandal directly.
