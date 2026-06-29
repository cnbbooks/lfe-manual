# Slice 07 — Graphs (authoring)

> Plan-of-record. Parent: `../arc-plan.md` (Phase 2, §2.1, §2.2/§A2.6).
> Voice: `docs/writers-guide/cosmic-techno-wit-style-guide.md`.
> **First authoring slice — there is no draft to split; we write the chapter.**

## Goal

Author and publish the **Graphs** chapter (`src/part2/graphs/`) as a multi-leaf
chapter in Cosmic Techno-Wit voice, covering Erlang's `digraph` and
`digraph_utils` modules as used from LFE. README + 9 leaves; add the ToC entries.

## Scope

**In:** `src/part2/graphs/` README + 9 leaves; the SUMMARY Graphs block.
**Out:** general graph theory beyond what `digraph` needs; alternative graph
libraries; anything outside `part2/graphs/`.

## Source / reference material

- `workbench/graphs/digraph — stdlib v7.1.pdf` — the `digraph` API (authoritative).
- `workbench/graphs/digraph_utils — stdlib v7.1.pdf` — the `digraph_utils` API.
- `workbench/graphs/Learn You Some Erlang.pdf` — narrative/worked-example flavour.
- Cross-refs: the data-structure chapters already written (maps, dicts) for
  "when not to use a graph"; `byte-bin`/others for voice calibration.

All source examples are Erlang — **convert every one to idiomatic LFE**
(`(digraph:new)`, `(digraph:add_vertex g 'a)`, `#(...)` tuples, `lfe>` REPL
prompts, etc.).

## Leaf breakdown (proposed slugs → topic; CTW titles finalised at authoring)

| Slug | Topic | Working title |
|------|-------|---------------|
| README | What graphs are; the standout fact that `digraph` is **mutable**/ETS-backed/reference-typed | Graphs: Drawing Lines Between Things |
| create | `digraph:new/0,1`; cyclic/acyclic, protected/private/public options | Conjuring a Graph |
| vertices | add_vertex/vertex/vertices/del_vertex/no_vertices; vertex labels | Vertices: The Things Themselves |
| edges | add_edge/edge/edges/del_edge/no_edges; the `{V1,V2}` model; edge labels | Edges: The Lines Between |
| paths | get_path, get_cycle, get_short_path, get_short_cycle | Paths, Cycles, and Getting From Here to There |
| utils | `digraph_utils`: topsort, is_acyclic, reachable/reaching, components, strong_components, cyclic_strong_components, arborescence | The Power Tools: digraph_utils |
| mutability | The big caveat: shared mutable ETS state, ownership, `digraph:delete/1`, not immutable, not auto-GC'd | The Scandalous Truth: Graphs Mutate |
| example | Worked example — dependency/build order via topsort | A Worked Example: Build Order from Dependencies |
| vs | When to reach for `digraph` vs hand-rolled maps; when graphs matter | When to Reach for a Graph |
| summ | Summary | Summary |

## Method (per §A2.6)

1. **Outline → ToC.** Write-probe `src/part2/graphs/` for stubs; add 9 nested
   `SUMMARY.md` entries under the Graphs line; create/fill the leaf files.
2. **Author in voice.** Each leaf: technically correct and complete first
   (Prime Directive), then CTW wit on top. Lean into the genuinely funny angle
   that `digraph` *mutates* (the style guide's "ETS tables wearing a trench
   coat" example lives here). Themed example data welcome (HHG cast).
3. **Convert all code to idiomatic LFE.**

## Verification approach (dual — authoring has no draft to diff)

- **Technical accuracy:** every `digraph`/`digraph_utils` claim and every code
  example checked against the stdlib PDFs; LFE is valid and idiomatic (use the
  `erlang-guidelines` / LFE knowledge skills; independent CDC pass).
- **Voice conformance:** against the style guide ship-it checklist (§12).
- **Structure:** no placeholders; ToC entries == leaf titles and resolve;
  operator mdBook build.

## Exit criteria

10 files authored, 9 SUMMARY entries, ledger walked (incl. both verification
gates), close set written with bubble-up to the arc.
