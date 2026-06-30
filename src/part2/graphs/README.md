# Graphs: Drawing Lines Between Things

## In Which We Discover That Some Relationships Refuse to Be Flattened Into a List

> "The History of every major Galactic Civilization tends to pass through three
> distinct and recognizable phases, those of Survival, Inquiry and
> Sophistication, otherwise known as the How, Why, and Where phases. For
> instance, the first phase is characterized by the question 'How can we eat?',
> the second by the question 'Why do we eat?' and the third by the question
> 'Where shall we have lunch?'" — Douglas Adams

Sooner or later, dear reader, you will meet a problem that a list simply refuses to model honestly. You have a pile of *things*, and between those things run *relationships* — this module depends on that one, this airport has flights to those three, this task must finish before that task may begin. You could try to cram all of this into nested lists and tuples, and for a little while it would even work, in the way that balancing your accounts on the back of a napkin works right up until the moment it spectacularly doesn't.

What you actually want is a **graph**: a set of *vertices* (the things) and a set of directed *edges* (the relationships between them). In the formal telling, a directed graph — a "digraph", which is the word we shall use henceforth because typing "directed graph" several hundred times is its own small tragedy — is a pair `(V, E)` where `V` is a finite set of vertices and `E` is a set of edges, each edge being an ordered pair of vertices drawn from `V`. An edge `(v, w)` *emanates from* `v` and is *incident on* `w`, which is mathematician for "there's an arrow pointing from v to w."

In LFE, you do not build this machinery yourself. Erlang's standard library hands you two modules, fully assembled and quietly battle-tested:

- **`digraph`** — constructs and modifies graphs: adding and deleting vertices and edges, attaching labels, finding paths and cycles, asking how many of each thing you have.
- **`digraph_utils`** — the power tools: topological sorting, reachability, connected components, cycle detection, and the other algorithms you half-remember from a course you have spent the intervening years doing your level best to forget.

Before we go a single step further, one fact deserves to be stated plainly, in good light, while everyone is paying attention: **a digraph is mutable.** It is the one data structure in this entire corner of the standard library that does not play by the immutable rules every other chapter has so lovingly drilled into you. A digraph is implemented with ETS tables, which means the thing you hold is a *reference* to shared, mutable state rather than a value you can pass around with a clear conscience. We will devote an entire, slightly scandalised section to this ([The Scandalous Truth: Graphs Mutate](mutability.md)); for now, simply file it away somewhere you will not lose it.

The rest of the chapter proceeds in the natural order: how to [conjure a graph](create.md), how to populate it with [vertices](vertices.md) and [edges](edges.md), how to interrogate it for [paths and cycles](paths.md), and how to reach for the [power tools](utils.md) when you need a real algorithm. We finish with a [worked example](example.md) — working out build order from a tangle of dependencies, which is the single most common reason a working programmer ever touches this module — and some honest counsel on [when a graph is worth the trouble](vs.md) and when a humble map would have done.

Right, then. *Shall* we begin?
