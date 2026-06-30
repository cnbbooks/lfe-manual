# Edges: The Lines Between

If vertices are the nouns of a graph, edges are the verbs. An edge is a **directed** connection from one vertex to another — it *emanates from* `V1` and is *incident on* `V2`, which is to say it is an arrow with a tail at `V1` and a head at `V2`. Direction matters: an edge from `'zaphod` to `'ford` is emphatically not the same as an edge from `'ford` to `'zaphod`, a distinction that anyone who has confused "depends on" with "is depended on by" will have learned the hard way.

## Adding Edges: `add_edge/3,4,5`

The workhorse is `digraph:add_edge/3`. You give it the graph and two vertices — tail first, then head — and it returns an opaque **edge identifier**:

```lfe
lfe> (set g (digraph:new))
lfe> (digraph:add_vertex g 'zaphod)
lfe> (digraph:add_vertex g 'ford)
lfe> (digraph:add_edge g 'zaphod 'ford)
(|$e| . 0)
```

That `(|$e| . 0)` is the edge's identity — its name — handed back so you can refer to the edge later. Like the auto-generated vertex ids of the previous section, it is opaque and not for human consumption; bind it to a name if you need it and otherwise let it be.

As with vertices, an edge may carry a **label** — arbitrary ancillary information — via `add_edge/4`:

```lfe
lfe> (digraph:add_edge g 'zaphod 'ford "owes drinks")
(|$e| . 1)
```

It is worth being precise here, because the documentation is precise: an edge's **identifier** and an edge's **label** are different things. The identifier *names* the edge; the label *describes* it. You can have two distinct edges between the same pair of vertices, each with its own identifier — multiple edges between vertices are explicitly allowed, which is what makes a digraph a *non-proper* directed graph in the formal sense, and what makes it able to model "two flights from Heathrow to Betelgeuse" without complaint.

If you want to name the edge yourself, or modify an existing edge, reach for the five-argument `add_edge/5`, which takes the graph, the edge identifier, the two vertices, and the label:

```lfe
lfe> (digraph:add_edge g 'flight-42 'zaphod 'ford "first class")
flight-42
```

Calling `add_edge/5` with an identifier that already exists **modifies** that edge rather than creating a second one — the same mutate-in-place behaviour we met with vertex labels — *provided you keep the same endpoints*. Reuse an existing edge identifier to connect a *different* pair of vertices and `digraph` refuses, returning `#(error #(bad_edge (V1 V2)))` rather than silently rewiring the edge under you. An edge id may have its label changed, in other words, but not its destination.

## When an Edge Is Refused

`add_edge` does not always succeed, and its failures are informative rather than merely annoying. Instead of an edge identifier it may return an error tuple:

- **`#(error #(bad_vertex V))`** — you named a vertex `V` (either the tail or the head) that is not in the graph. The `digraph` module will not invent vertices for you; add them first.
- **`#(error #(bad_edge Path))`** — adding this edge would create a **cycle in an acyclic graph**. Recall from [Conjuring a Graph](create.md) that a graph made with the `acyclic` option refuses to close a loop, and here is where it makes good on that promise. The `Path` it hands back is the cycle your edge would have completed, which is enormously helpful for working out which dependency you got backwards.

```lfe
lfe> (set dag (digraph:new '(acyclic)))
lfe> (digraph:add_vertex dag 'a)
lfe> (digraph:add_vertex dag 'b)
lfe> (digraph:add_edge dag 'a 'b)
(|$e| . 0)
lfe> (digraph:add_edge dag 'b 'a)
#(error #(bad_edge (a b)))
```

The acyclic graph has just saved you from a circular dependency *at the moment you tried to introduce it*, rather than three hours later when a topological sort mysteriously returns `false`. This is the kind of error you should be grateful for.

## Inspecting Edges: `edge/2`, `edges/1`, `edges/2`

To look up a single edge by its identifier, use `digraph:edge/2`, which returns a four-tuple `#(E V1 V2 Label)` — the edge id, its tail, its head, and its label — or `false` if no such edge exists:

```lfe
lfe> (digraph:edge g 'flight-42)
#(flight-42 zaphod ford "first class")
```

That four-tuple is the canonical, complete description of an edge: this is edge `flight-42`, it runs from `zaphod` to `ford`, and it is labelled `"first class"`. Commit its shape to memory; you will pattern-match on it constantly.

For all the edges in the graph, use `digraph:edges/1`; for just the edges touching a particular vertex (emanating from *or* incident on it), use `digraph:edges/2`:

```lfe
lfe> (digraph:edges g)
((|$e| . 0) (|$e| . 1) flight-42)
lfe> (digraph:edges g 'zaphod)
((|$e| . 0) (|$e| . 1) flight-42)
```

Both return edge *identifiers*, in the usual unspecified order, leaving you to call `edge/2` on each if you want the full picture. When you care about direction, the finer-grained `out_edges/2` (edges emanating from a vertex) and `in_edges/2` (edges incident on it) let you ask precisely which way the arrows point.

To count edges without materialising the list, there is `digraph:no_edges/1`:

```lfe
lfe> (digraph:no_edges g)
3
```

## Deleting Edges: `del_edge/2` and `del_edges/2`

Removing an edge is the work of `digraph:del_edge/2`, which takes the edge's identifier:

```lfe
lfe> (digraph:del_edge g 'flight-42)
true
```

Deleting an edge removes only the edge — the vertices it connected carry on, unbothered, possibly relieved. (Contrast this with deleting a *vertex*, which takes all its edges down with it.) To remove several edges at once, hand `digraph:del_edges/2` a list of identifiers:

```lfe
lfe> (digraph:del_edges g (digraph:edges g 'zaphod))
true
```

With vertices placed and edges drawn between them, you have a genuine graph — a structure with shape, direction, and the capacity to be asked interesting questions. The most interesting of those questions are about *paths*, and that is where we turn next.
