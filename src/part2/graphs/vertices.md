# Vertices: The Things Themselves

A graph without vertices is a philosophical curiosity; a graph with vertices is a tool. Vertices are the *things* your graph is about — the modules, the airports, the tasks, the characters in a tediously complicated space opera. In a digraph, a vertex can be **any Erlang term**: an atom, a tuple, a binary, an integer, whatever names your thing most naturally. The graph asks only that each vertex be unique.

## Adding Vertices: `add_vertex/1,2,3`

The everyday form is `digraph:add_vertex/2`, which adds a vertex you name yourself and returns it:

```lfe
lfe> (set g (digraph:new))
lfe> (digraph:add_vertex g 'zaphod)
zaphod
lfe> (digraph:add_vertex g 'ford)
ford
lfe> (digraph:add_vertex g 'arthur)
arthur
```

Each vertex here is an atom, which is the most common and most readable choice. The return value is simply the vertex you handed in, which feels redundant until you meet the labelled and anonymous forms below.

You may also attach a **label** — an arbitrary term carrying ancillary information about the vertex — with `add_vertex/3`:

```lfe
lfe> (digraph:add_vertex g 'marvin "Paranoid Android, second-hand")
marvin
```

The label is *not* the vertex's identity. The vertex is `marvin`; the string is merely a note pinned to it. Two crucial consequences follow. First, you still refer to the vertex by `'marvin`, never by its label. Second — and this surprises people — `add_vertex/3` on an *existing* vertex does not create a duplicate; it **modifies** the label of the vertex already there. Adding `'marvin` twice gives you one Marvin, the second call quietly overwriting the first's label. This is your first taste of mutation, served early so it doesn't ambush you later.

The third form, `add_vertex/1`, takes no name at all and invents one for you, returning an opaque auto-generated identifier — internally a cons of a reserved atom and an integer, which the REPL prints in all its unlovely glory:

```lfe
lfe> (digraph:add_vertex g)
(|$v| . 0)
```

These anonymous vertices have their uses, but in practice you almost always want to name your own — a graph of `'zaphod` and `'ford` is infinitely easier to debug six months later than a graph of machine-generated gensyms. We mention `add_vertex/1` chiefly so you recognise it when it turns up in someone else's code, presumably written under deadline.

## Looking Vertices Up: `vertex/2` and `vertices/1`

To retrieve a single vertex and its label, use `digraph:vertex/2`, which returns a `#(Vertex Label)` tuple — or the atom `false` if no such vertex exists:

```lfe
lfe> (digraph:vertex g 'marvin)
#(marvin "Paranoid Android, second-hand")
lfe> (digraph:vertex g 'zaphod)
#(zaphod ())
lfe> (digraph:vertex g 'trillian)
false
```

Note that `'zaphod`, added without a label, reports the empty list `()` as its label — the default. Note also that a missing vertex yields `false` rather than an exception, which makes `vertex/2` a perfectly civil way to test for membership.

To get *all* the vertices, use `digraph:vertices/1`:

```lfe
lfe> (digraph:vertices g)
(arthur zaphod marvin ford)
```

A word of caution that applies throughout this module: the order is **unspecified**. The list above comes back in an order that is neither insertion order nor anything else you could usefully predict, and the documentation promises nothing of the sort, so you must never write code that depends on the ordering of `vertices/1`. If you need an order, impose one yourself — or reach for [topological sorting](utils.md), which exists precisely to hand you a meaningful order when the graph's structure implies one.

To count vertices without dragging the whole list out, use `digraph:no_vertices/1`:

```lfe
lfe> (digraph:no_vertices g)
4
```

## Deleting Vertices: `del_vertex/2` and `del_vertices/2`

Removing a vertex is the work of `digraph:del_vertex/2`:

```lfe
lfe> (digraph:del_vertex g 'arthur)
true
```

This does more than it might appear. Deleting a vertex also deletes **every edge** emanating from or incident on it — which is exactly what you want, since an edge to a vertex that no longer exists would be a dangling reference and a small lie. Poor Arthur departs, and every flight to and from Arthur departs with him.

To remove several vertices at once, hand `digraph:del_vertices/2` a list:

```lfe
lfe> (digraph:del_vertices g '(ford marvin))
true
```

## Counting the Neighbourhood: Degrees and Neighbours

Once edges enter the picture (the [next section](edges.md)), each vertex acquires a *neighbourhood*, and the `digraph` module offers a small family of functions for surveying it. Suppose a fresh graph in which `zaphod` points at both `ford` and `trillian`, and `ford` in turn points at `trillian`. The **out-degree** of a vertex is the number of edges emanating from it; the **in-degree** is the number incident on it:

```lfe
lfe> (digraph:out_degree g 'zaphod)
2
lfe> (digraph:in_degree g 'zaphod)
0
```

Zaphod points at two things and is pointed at by none — the out-degree is two, the in-degree zero. To get the neighbours themselves rather than merely count them, there are `out_neighbours/2` (the vertices your vertex points *at*) and `in_neighbours/2` (the vertices that point *at* your vertex):

```lfe
lfe> (digraph:out_neighbours g 'zaphod)
(trillian ford)
lfe> (digraph:in_neighbours g 'trillian)
(ford zaphod)
```

These, too, come back in unspecified order, as is by now the house custom. With the things themselves accounted for, we may turn to the lines between them.
