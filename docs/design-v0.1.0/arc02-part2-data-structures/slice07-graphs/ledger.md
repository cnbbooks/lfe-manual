# Ledger — Slice 07 (Graphs, authoring)

> Rows = 10 leaves + ToC + 4 reconciliation = 15. Authoring slice: each leaf is
> *written* (not transcribed), so per-leaf evidence is "authored; technically
> checked; in-voice". Conventions: arc01 §6 + arc02 §A2 (incl. §A2.6 dual
> verification).

## Group 1 — files (`src/part2/graphs/`)

| # | Target | Topic | Status | Evidence |
|---|--------|-------|--------|----------|
| 01 | README.md | What graphs are; `digraph` is mutable/ETS-backed | done | Authored. H1 "Graphs: Drawing Lines Between Things" + Adams epigraph + "In Which…" landing; orients graphs = `digraph`/`digraph_utils`; previews the mutability scandal; links all 9 leaves. |
| 02 | create.md | `digraph:new/0,1`; cyclic/acyclic, access options | done | Authored. `new/0`, `new/1` options (cyclic/acyclic default cyclic; protected/private default protected), `info/1`, `delete/1`. `info` memory figure (950) taken from live `erl` run on an acyclic empty graph. |
| 03 | vertices.md | add/del/vertex/vertices/no_vertices; labels | done | Authored. add_vertex/1,2,3, vertex/2 (`#(V Label)`/`false`), vertices/1, no_vertices/1, del_vertex/2 (+edge cascade), del_vertices/2, degrees + neighbours. Outputs (`#(zaphod ())`, degree/neighbour lists) from live `erl` runs; auto-id shown as `(|$v| . 0)`. |
| 04 | edges.md | add_edge/3,4,5; `#(E V1 V2 Label)`; labels | done | Authored. add_edge/3,4,5, edge/2 four-tuple, edges/1,2, no_edges, del_edge/2, del_edges/2; multiple edges; the `#(error #(bad_vertex V))` / `#(error #(bad_edge (a b)))` error paths — bad_edge `(a b)` confirmed by live `erl` run. |
| 05 | paths.md | get_path/get_cycle/get_short_path/get_short_cycle | done | Authored. get_path/3 (DFS, *a* path) vs get_short_path/3 (BFS, short path); get_cycle/2 `[V…V]`/`[V]`; get_short_cycle/2 loop-as-`(V V)` note; del_path/3. All transcripts (`(earth betelgeuse magrathea ursa-minor)`, `false`, `(earth magrathea ursa-minor)`, `(a b c a)`) confirmed by live `erl` runs. |
| 06 | utils.md | `digraph_utils`: topsort/reachable/components/… | done | Authored. topsort/1 (+`false` on cycle), is_acyclic/1, reachable/reaching (+`_neighbours`), components/strong_components/cyclic_strong_components, pre/postorder, is_tree/is_arborescence/arborescence_root, loop_vertices, subgraph, condensation (+ the "subgraph/condensation create new graphs you must delete" caveat). topsort/reachable/reaching/components outputs corrected to match live `erl` runs. |
| 07 | mutability.md | shared ETS state; ownership; `delete/1`; not GC'd | done | Authored. Reference-not-value (aliasing demo), only-creator-may-write (protected/private), no GC → must `delete/1` (or owner dies), the trench-coat + 3 a.m. example from style-guide §13. Claims match the `digraph` module preamble verbatim in intent. |
| 08 | example.md | worked example — build order via topsort | done | Authored. `build-order` module (defmodule/export/defun/let/try-after/lc/case/element); edge = "dependency → dependent"; `try…after` guarantees `delete`. Erlang-equivalent of the module run live: `#(ok (hull power core drive ship))` and `#(error cyclic)` confirmed. |
| 09 | vs.md | `digraph` vs hand-rolled maps; when graphs matter | done | Authored. Adjacency-map alternative (`#M(...)`); paired Use-when / Avoid-when lists; the algorithmic-need / size / labelled-edges / single-owner decision axes; honest cleanup-hazard guidance. |
| 10 | summ.md | Summary | done | Authored. Recaps create/dispose, vertices, edges, paths, power tools, and the mutability scandal; callback close ("draw the line, walk it… turn off the lights"; "Don't Panic. Just don't forget to `delete`."). |

## Group 2 — ToC

| # | Item | Check | Status | Evidence |
|---|------|-------|--------|----------|
| 11 | SUMMARY Graphs block | 9 nested entries, correct indent, text == leaf title, paths resolve | done | `src/SUMMARY.md`: chapter line `[Graphs]` retained (matches sibling chapters' short ToC names); 9 entries added at 4-space indent; each link text == the leaf `#` title verbatim; mdBook build resolves all links (no broken-link WARN). |

## Group 3 — reconciliation & verification (dual gate)

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 12 | **Technical accuracy** — every `digraph`/`digraph_utils` claim matches the stdlib docs; no invented functions/behaviour. | reconciled | done | Every function/arity cross-checked against the stdlib v7.1 extracts; all deterministic outputs (return-tuple shapes, error tuples, false/true, build-order result) verified by live `erl` runs against the same underlying stdlib modules. Independent CDC pass confirmed (`cdc-verification.md`). |
| 13 | **Idiomatic LFE** — every code example is valid LFE (no raw Erlang); REPL transcripts use `lfe>`; checked with LFE/erlang-guidelines knowledge. | reproduced | done | Scan: 0 raw-Erlang call/`[{…}]` syntax; 42/42 fences are ```` ```lfe ````; all prompts `lfe>`. `build-order` module reviewed for LFE syntax validity (defmodule/export/defun/let/try-after/lc/case/element/`#(...)`). Independent CDC pass confirmed. |
| 14 | **Voice conformance** — passes the style-guide ship-it checklist. | attested | done | Prime Directive (explanation-before-joke) held; ≥1 precise "rather like" simile (paths.md fence-post); running motif (the mutating digraph / trench coat) planted in README, paid off in mutability.md + summ.md; no punching at reader; motifs rationed (3 a.m. ×4 spread; trench coat ×3 concentrated in its home section; no forced 42); CTW headings. Independent CDC pass confirmed. |
| 15 | No lone-heading placeholder; mdBook build / link check. | reproduced | done | `make build` → 0 `WARN`, exit 0 (only the pre-existing mermaid preprocessor version note on stderr). No lone-heading files; HTML book written. |

**Open-count (15) == close-count (15). 15 done, 0 deferred, 0 silent drops.**
