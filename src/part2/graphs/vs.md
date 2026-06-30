# When to Reach for a Graph

Every chapter in this Part has ended by comparing its subject to the alternatives, and graphs are no exception — though the comparison here is unusual, because the chief alternative to a `digraph` is *not another library*. It is a map. A great many problems that *look* like they need a graph are served perfectly well by a humble `#M(...)` of vertex-to-neighbours, with none of the ETS tables, none of the mutability, and none of the obligation to remember `delete/1`. The honest question is therefore not "is this a graph problem?" — almost everything with relationships is, technically — but "is this a graph problem big and algorithmic enough to be worth the trench coat?"

## The Hand-Rolled Alternative

Before reaching for `digraph`, consider what a map already gives you. An adjacency map is just a map from each vertex to its list of out-neighbours:

```lfe
lfe> (set graph #M(earth (betelgeuse magrathea)
                   betelgeuse (magrathea)
                   magrathea (ursa-minor)))
#M(betelgeuse (magrathea) earth (betelgeuse magrathea) magrathea (ursa-minor))
```

This is an immutable value. You can pass it between processes freely, store old versions, pattern-match on it, and let the garbage collector tidy it away when you are done — every comfort the rest of this book has taught you to expect, and every one of which `digraph` quietly revokes. For *representing* a graph, for *traversing* it with your own recursion, for storing it in a `gen_server`'s state, the map is very often the better-behaved choice. It is the data-structure equivalent of taking the stairs: unglamorous, but you always know exactly where you stand.

## Use a `digraph` when:

- **You need the algorithms, not just the structure.** Topological sort, strongly connected components, shortest paths, cycle detection — these are real, fiddly algorithms, and `digraph_utils` has already written, debugged, and optimised them for you. Re-implementing topological sort over an adjacency map to avoid one ETS table is a poor trade; this is precisely the work the module exists to spare you.
- **The graph is large and you build it incrementally.** Threading an immutable map through ten thousand insertions allocates ten thousand intermediate maps; mutating ETS tables in place does not. For big graphs assembled piece by piece, the mutable representation is genuinely faster, and that speed is the whole reason it surrendered immutability.
- **You need labelled edges, multiple edges between vertices, or rich edge identity.** Modelling "two distinct flights from Heathrow to Betelgeuse, each with its own details" is awkward in a plain adjacency map and native to a `digraph`.
- **The graph lives in one owning process for its lifetime.** A long-lived service that builds a graph once and answers many queries against it plays to every one of `digraph`'s strengths and none of its weaknesses.

## Avoid a `digraph` when:

- **The graph is small and you only need to traverse it.** If a few lines of recursion over a map will do, they will do it with less ceremony and no cleanup obligation.
- **You want to share the graph across processes, or send it in messages.** Only the creating process may modify a digraph, and it evaporates when that process dies. An immutable map travels between processes without complaint and without surprises.
- **You want to keep old versions, or treat the graph as a value in a larger immutable structure.** A digraph has exactly one version — the current one — because it mutates. If history matters, the immutable representation is not a preference but a requirement.
- **You cannot guarantee cleanup.** A `digraph` built in a long-lived process and never `delete`d is a memory leak with a slow fuse. If the lifecycle is murky and the owning process outlives the graph's usefulness, a map sidesteps the entire hazard — there is nothing to forget to free, and no one will judge you for choosing the structure that cannot leak.

## The Short Version

Reach for a `digraph` when the *algorithms* in `digraph_utils` are doing the heavy lifting — when you genuinely need topological order, component analysis, or path-finding over a graph large enough that hand-rolling would hurt. Reach for a map when you mostly need to *represent* relationships and walk them yourself. The graph module is a specialist's tool: superb at the job it was built for, slightly dangerous left lying around, and — like a good towel — far better to have and not need than to need and not have.
