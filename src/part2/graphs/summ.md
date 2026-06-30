# Summary

Graphs are how you model *things and the relationships between them* when a list or a map can no longer keep the relationships straight. In LFE, you get them from two standard-library modules — `digraph` for building and querying, `digraph_utils` for the real algorithms — and the whole chapter reduces to a handful of moves worth carrying away:

- **Conjure and dispose.** `digraph:new/0,1` creates a graph (default `cyclic` and `protected`; pass `acyclic` to forbid cycles at insertion time, `private` to lock other processes out). `digraph:delete/1` frees it. The second is not optional.
- **Vertices are the things.** `add_vertex`, `vertex`, `vertices`, `del_vertex`, `no_vertices`, plus the degree and neighbour functions. A vertex is any term; a label is a note pinned to it, not its identity.
- **Edges are the directed lines between.** `add_edge`, `edge` (returning the canonical `#(E V1 V2 Label)`), `edges`, `del_edge`, `no_edges`. Direction matters; multiple edges and labels are allowed; an `acyclic` graph refuses an edge that would close a loop, handing you the offending path.
- **Paths and cycles.** `get_path` (depth-first, *a* path) versus `get_short_path` (breadth-first, the *short* path); `get_cycle` and `get_short_cycle` for loops; `del_path` for severance.
- **The power tools.** `topsort` for build order (`false` on a cycle), `is_acyclic` for a quick verdict, `reachable`/`reaching` for impact analysis, `components`/`strong_components` for structure, and a drawer of specialists for the rarer days.

And above all, the one fact that makes graphs different from everything else in this Part: **a digraph mutates.** It is a reference to shared, ETS-backed state, not an immutable value — owned by one process, invisible to the garbage collector, and yours to `delete`. Hold the handle, respect the ownership, clean up when you're done, and `digraph` is a superb tool. Forget, and it will wait for you at 3 a.m. with the patience of a thing that knows it has all the time in the world.

The other data structures in this book ask nothing of you but that you understand them. The graph asks that, and one thing more: that you remember to draw the line, walk it, and then — like any good guest — turn off the lights on your way out. Don't Panic. Just don't forget to `delete`.
