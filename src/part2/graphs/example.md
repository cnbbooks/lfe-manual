# A Worked Example: Build Order from Dependencies

Theory is all very well, but let us now do the single most common thing a working programmer ever uses a graph for: working out what order to build things in when some of them depend on others. Compilers do this with modules, package managers do it with libraries, build tools do it with targets, and at least one spacecraft does it with subsystems. The underlying question is always the same — *given a tangle of "X needs Y first" relationships, in what order may I proceed?* — and the answer is always the same: a topological sort.

## The Problem

Suppose we are assembling the *Heart of Gold* from a parts list, where each component declares the components it depends on:

- `ship` needs `drive` and `hull`
- `drive` needs `core`
- `core` needs `power`
- `hull` needs nothing
- `power` needs nothing

We want a build order in which nothing is assembled before the things it depends on. By eye, on a list this small, you can squint and work it out. With four hundred components and a deadline, you want the machine to do it — and you want the machine to *notice* if someone has accidentally declared a circular dependency, because a build order for a circular dependency does not exist and no amount of squinting will conjure one.

## The Module

```lfe
(defmodule build-order
  (export (resolve 1)))

;; A spec is #(component (dependency ...)).
;; resolve/1 returns #(ok Order), with every dependency appearing
;; before the component that needs it, or #(error cyclic) if the
;; specs contain a circular dependency that no build order can satisfy.
(defun resolve (specs)
  (let ((g (digraph:new)))
    (try
      (progn
        ;; Model each "component needs dependency" as an edge pointing
        ;; FROM the dependency TO the component — "dependency first".
        ;; add-edge needs both endpoints to exist, so we add them too;
        ;; add_vertex is idempotent for our label-free purposes.
        (lc ((<- spec specs))
          (let ((component (element 1 spec))
                (deps (element 2 spec)))
            (digraph:add_vertex g component)
            (lc ((<- dep deps))
              (progn
                (digraph:add_vertex g dep)
                (digraph:add_edge g dep component)))))
        ;; topsort hands back a valid order, or 'false for a cycle.
        (case (digraph_utils:topsort g)
          ('false #(error cyclic))
          (order (tuple 'ok order))))
      ;; Whatever happens — success, cycle, or exception — free the
      ;; ETS tables. A digraph is not garbage collected; see the
      ;; Scandalous Truth. The `after` clause is how we keep that promise.
      (after
        (digraph:delete g)))))
```

Two details earn their keep here. The first is the **edge direction**: an edge runs from a dependency *to* the component that needs it, so that `topsort/1` — which guarantees no out-neighbour appears earlier than the vertex it points from — places every dependency before its dependent. Get this backwards and you will get a perfectly valid topological sort of a perfectly backwards graph, which is the kind of bug that survives code review because everything *looks* right.

The second is the `try ... (after ...)` wrapper. Because a digraph mutates and lives in ETS tables that no garbage collector will reclaim ([The Scandalous Truth](mutability.md)), the `after` clause guarantees we call `digraph:delete/1` on every exit path — normal return, cycle, or unexpected exception. This is the idiomatic shape for any function that builds a temporary graph: create, use, and *unconditionally* delete.

## Running It

```lfe
lfe> (c "build-order.lfe")
#(module build-order)
lfe> (build-order:resolve
       '(#(ship  (drive hull))
         #(drive (core))
         #(core  (power))
         #(hull  ())
         #(power ())))
#(ok (hull power core drive ship))
```

There is our build order: `hull` and `power` first (they depend on nothing), then `core` (which needed `power`), then `drive` (which needed `core`), and finally `ship` (which needed both `drive` and `hull`). Every part arrives after its prerequisites. As ever, the order is *a* valid one rather than *the* valid one — `power` could just as correctly have come first, or `hull` later — so resist the urge to write a test asserting this exact list.

And now the part that makes it worth automating. Suppose a weary engineer declares, late in the day, that the chicken depends on the egg and the egg depends on the chicken:

```lfe
lfe> (build-order:resolve
       '(#(chicken (egg))
         #(egg     (chicken))))
#(error cyclic)
```

No build order exists, and `resolve/1` says so plainly rather than looping forever or returning nonsense. `topsort/1` returned `false`, we turned that into a tidy `#(error cyclic)`, and the `after` clause cleaned up the graph regardless. Forty lines of LFE, and you have a dependency resolver that a great many real build systems would recognise as a close cousin — which is rather the point of having `digraph` in the standard library at all.
