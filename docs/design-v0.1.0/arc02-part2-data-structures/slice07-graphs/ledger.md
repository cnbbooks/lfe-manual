# Ledger — Slice 07 (Graphs, authoring)

> Rows = 10 leaves + ToC + 4 reconciliation = 15. Authoring slice: each leaf is
> *written* (not transcribed), so per-leaf evidence is "authored; technically
> checked; in-voice". Conventions: arc01 §6 + arc02 §A2 (incl. §A2.6 dual
> verification).

## Group 1 — files (`src/part2/graphs/`)

| # | Target | Topic | Status | Evidence |
|---|--------|-------|--------|----------|
| 01 | README.md | What graphs are; `digraph` is mutable/ETS-backed | | |
| 02 | create.md | `digraph:new/0,1`; cyclic/acyclic, access options | | |
| 03 | vertices.md | add/del/vertex/vertices/no_vertices; labels | | |
| 04 | edges.md | add_edge/edge/edges/del_edge; `{V1,V2}`; labels | | |
| 05 | paths.md | get_path/get_cycle/get_short_path/get_short_cycle | | |
| 06 | utils.md | `digraph_utils`: topsort/reachable/components/… | | |
| 07 | mutability.md | shared ETS state; ownership; `delete/1`; not GC'd | | |
| 08 | example.md | worked example — build order via topsort | | |
| 09 | vs.md | `digraph` vs hand-rolled maps; when graphs matter | | |
| 10 | summ.md | Summary | | |

## Group 2 — ToC

| # | Item | Check | Status | Evidence |
|---|------|-------|--------|----------|
| 11 | SUMMARY Graphs block | 9 nested entries, correct indent, text == leaf title, paths resolve | | |

## Group 3 — reconciliation & verification (dual gate)

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 12 | **Technical accuracy** — every `digraph`/`digraph_utils` claim matches the stdlib docs; no invented functions/behaviour. | reconciled | | |
| 13 | **Idiomatic LFE** — every code example is valid LFE (no raw Erlang); REPL transcripts use `lfe>`; checked with LFE/erlang-guidelines knowledge. | reproduced | | |
| 14 | **Voice conformance** — passes the style-guide ship-it checklist (explanation-before-joke; precise similes; no punching at reader; code carries voice; motifs rationed; CTW headings). | attested | | |
| 15 | No lone-heading placeholder; mdBook build / link check (operator). | reproduced | | |
