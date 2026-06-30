# Conjuring a Graph

Every graph begins life empty, which is to say as the *empty digraph* — a perfectly respectable graph with no vertices and no edges, much like a guest list before anyone has been invited. You summon one with `digraph:new/0`:

```lfe
lfe> (set g (digraph:new))
#(digraph #Ref<0.1.2.3> #Ref<0.1.2.4> #Ref<0.1.2.5> true)
```

That alarming-looking return value is the graph itself — or rather, your *handle* to it. Those `#Ref<...>` entries are the identifiers of the ETS tables in which your graph quietly lives. You are not meant to read this, poke at it, or take it apart; it is an opaque term, and the polite thing to do is to bind it to a name (here, `g`) and pass that name to every other `digraph` function. Treat the internals as you would the wiring behind a wall socket: present, essential, and absolutely none of your business.

## Choosing the Graph's Character: `new/1`

If you want a graph with opinions, hand `digraph:new/1` a list of option atoms. There are two independent choices to make, each a pair of mutually exclusive moods:

**Cyclicity — may the graph contain cycles?**

- `cyclic` — cycles are permitted. This is the **default**.
- `acyclic` — the graph is to be kept free of cycles. Any edge you try to add that would close a loop is refused at the door (more on that refusal in [Edges](edges.md)).

**Protection — who is allowed to touch it?**

- `protected` — the creating process may modify the graph; other processes may *read* it. This is the **default**.
- `private` — only the creating process may read or modify the graph; everyone else is locked out entirely.

So a graph meant to model build dependencies — where a cycle is not a quirk but a catastrophe — is best conjured acyclic:

```lfe
lfe> (set deps (digraph:new '(acyclic)))
#(digraph #Ref<0.1.2.6> #Ref<0.1.2.7> #Ref<0.1.2.8> false)
```

You may combine one option from each pair; order does not matter:

```lfe
lfe> (set g (digraph:new '(acyclic private)))
```

Hand `new/1` an option it does not recognise, or something that is not a proper list, and it will raise a `badarg` exception rather than guess at what you meant — a small kindness, since a graph silently created with the wrong cyclicity is exactly the sort of thing that ruins a Tuesday.

## Taking the Graph's Pulse: `info/1`

To ask a graph about itself, use `digraph:info/1`, which returns a list of `#(Tag Value)` pairs:

```lfe
lfe> (digraph:info deps)
(#(cyclicity acyclic) #(memory 950) #(protection protected))
```

You get back its `cyclicity` (`cyclic` or `acyclic`), its `protection` (`protected` or `private`), and its `memory` — the number of words allocated to its underlying ETS tables. That last figure is a genuine, load-bearing detail: a digraph occupies real memory in real tables, and unlike the immutable values elsewhere in this book, it will *not* quietly vanish when you stop looking at it.

## The Most Important Function in This Chapter: `delete/1`

Which brings us, with indecent haste for a section titled "creating things", to destruction. When you are finished with a graph, you must dispose of it yourself:

```lfe
lfe> (digraph:delete g)
true
```

`digraph:delete/1` frees the ETS tables backing the graph. This is not optional housekeeping you may get around to eventually; it is the counterpart to `new`, and forgetting it is the canonical way to leak memory with this module. There is no garbage collector coming to tidy up after you — the graph is not an immutable term that becomes unreachable and gets swept away by [the cosmic janitor](../tuples/creating.md). The one mercy is that the tables are also released automatically if the process that created the graph terminates, so a short-lived worker process that builds a graph, uses it, and dies will not haunt you. A long-lived process that builds graphs in a loop and forgets to delete them, on the other hand, absolutely will.

We will return to *why* all of this is true — the ETS tables, the ownership, the conspicuous absence of garbage collection — in [The Scandalous Truth: Graphs Mutate](mutability.md). For now: conjure with `new`, inspect with `info`, and always, *always* remember to `delete`.
