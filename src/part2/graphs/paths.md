# Paths, Cycles, and Getting From Here to There

A path is the graph-theoretic answer to the question every traveller eventually asks: *can I get there from here, and if so, how?* Formally, a path from `v[1]` to `v[k]` is a non-empty sequence of vertices in which each consecutive pair is joined by an edge pointing the right way. The path's *length* is the number of edges it uses — one fewer than the number of vertices, rather like the way a row of five fence posts has only four gaps between them, a discrepancy that has tripped up generations of programmers and at least one of the authors. A path is *simple* if no vertex repeats (except that the first and last may coincide), and a path that begins and ends at the same vertex with non-zero length is a **cycle**. A cycle of length one — a vertex with an edge to itself — is a **loop**.

The `digraph` module will find these things for you, so you need never write a depth-first search by hand again unless you genuinely want to, which some evenings one does.

## Finding a Path: `get_path/3`

`digraph:get_path/3` tries to find a simple path from `V1` to `V2`. It returns the path as a list `(V1 ... V2)` of vertices, or `false` if no such path of length one or more exists. Consider a small map of the galaxy:

```lfe
lfe> (set g (digraph:new))
lfe> (lc ((<- v '(earth magrathea betelgeuse ursa-minor)))
       (digraph:add_vertex g v))
(earth magrathea betelgeuse ursa-minor)
lfe> (digraph:add_edge g 'earth 'betelgeuse)
lfe> (digraph:add_edge g 'betelgeuse 'magrathea)
lfe> (digraph:add_edge g 'magrathea 'ursa-minor)
lfe> (digraph:get_path g 'earth 'ursa-minor)
(earth betelgeuse magrathea ursa-minor)
```

There is your itinerary: Earth to Betelgeuse to Magrathea to Ursa Minor, every leg an edge in the right direction. Ask for a journey that the edges do not permit, and you get an honest `false`:

```lfe
lfe> (digraph:get_path g 'ursa-minor 'earth)
false
```

The arrows point one way; there is no flight home. A small caution worth internalising: `get_path/3` traverses the graph **depth-first** and returns the *first* path it finds, which is not promised to be the shortest. If you simply need to know whether *a* route exists, `get_path/3` is your function. If you need the *best* route, read on.

## Finding the Shortest Path: `get_short_path/3`

When length matters, `digraph:get_short_path/3` traverses the graph **breadth-first** and returns an as-short-as-possible simple path:

```lfe
lfe> (digraph:add_edge g 'earth 'magrathea)
lfe> (digraph:get_short_path g 'earth 'ursa-minor)
(earth magrathea ursa-minor)
```

With a direct Earth-to-Magrathea edge now in place, the shortest route skips Betelgeuse entirely. Same start, same destination, fewer stops — the difference between `get_path` and `get_short_path` is precisely the difference between "a way" and "the short way", and choosing the wrong one is how you end up routing a packet across three extra hops for no reason anyone can later reconstruct.

## Finding Cycles: `get_cycle/2` and `get_short_cycle/2`

To discover whether a vertex sits on a cycle, use `digraph:get_cycle/2`. If a simple cycle of length two or more passes through `V`, you get it back as a list `(V ... V)`; if `V` has a loop (an edge to itself), you get `(V)`; and if no cycle through `V` exists, you get `false`:

```lfe
lfe> (set h (digraph:new))
lfe> (lc ((<- v '(a b c)))
       (digraph:add_vertex h v))
(a b c)
lfe> (digraph:add_edge h 'a 'b)
lfe> (digraph:add_edge h 'b 'c)
lfe> (digraph:add_edge h 'c 'a)
lfe> (digraph:get_cycle h 'a)
(a b c a)
```

The cycle `a → b → c → a` is reported beginning and ending at the vertex you asked about. Its companion `digraph:get_short_cycle/2` does the same job but tries to find as short a cycle as possible — and note one small wrinkle the documentation is careful to spell out: a loop through `V` comes back from `get_short_cycle/2` as the two-element list `(V V)`, not the single-element `(V)` that `get_cycle/2` would give. A pedantic distinction, perhaps, but the kind that matters when you are pattern-matching the result.

## Severing Connections: `del_path/3`

Finally, the demolition crew. `digraph:del_path/3` deletes edges from the graph until **no path remains** from `V1` to `V2`:

```lfe
lfe> (digraph:del_path g 'earth 'ursa-minor)
true
lfe> (digraph:get_path g 'earth 'ursa-minor)
false
```

This is more thorough than deleting a single edge. The procedure finds a path, removes every edge along it, and repeats until the two vertices are well and truly disconnected — useful when you need to guarantee that one part of a system can no longer reach another, and merrily destructive in exactly the way that demands you remember a digraph mutates in place. Which, as ever, brings us back to the central scandal of this chapter — but first, let us meet the power tools.
