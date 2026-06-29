# CC Prompt — Slice 07 (Graphs, authoring)

**Author** the Graphs chapter (`src/part2/graphs/`) — this is original writing,
not split-and-publish. Target: README + 9 leaves (slugs/topics in
`slice-doc.md`). Apply arc01 §6 + arc02 §A2 (incl. §A2.6).

## Voice — non-negotiable

Write in **Cosmic Techno-Wit** per `docs/writers-guide/cosmic-techno-wit-style-guide.md`.
Internalise it before writing. Above all the **Prime Directive**: state the
technical truth completely and correctly *first*; the wit rides on top and must
survive deletion. Use the checklist (§12) on every leaf.

The natural comic spine of this chapter is that **`digraph` mutates** — it is an
ETS-backed, reference-typed structure in a language otherwise devoted to
immutability. The style guide's worked example ("ETS tables wearing a trench
coat", with the 3 a.m. callback) is written *for this chapter* — use it in
`mutability.md`.

## Sources (convert ALL Erlang to idiomatic LFE)

- `workbench/graphs/digraph — stdlib v7.1.pdf` (the `digraph` API — authoritative)
- `workbench/graphs/digraph_utils — stdlib v7.1.pdf` (the `digraph_utils` API)
- `workbench/graphs/Learn You Some Erlang.pdf` (narrative flavour, examples)

Read the PDFs (Read tool, `pages` param) for the exact API surface. Every
example must be valid LFE: `(digraph:new)`, `(digraph:add_vertex g 'a)`,
`(digraph:add_edge g 'a 'b)`, `#(...)` for tuples, `lfe>` prompts in transcripts,
`'atoms`, etc. Do not leave any Erlang `Module:func(Args)` / `[{...}]` syntax.

## Mechanics

- Write-probe `src/part2/graphs/` for pre-existing stubs (a Write erroring "not
  read yet" ⇒ stub exists, Read then write; clean create ⇒ new). Don't trust
  `ls`/glob/git for existence.
- README = chapter H1 (CTW title) + a short landing intro (optionally an "In
  Which…" / Adams epigraph) + the orientation that graphs here means `digraph`.
- Each `##`-worthy topic → its own leaf (slugs in `slice-doc.md`); leaf `#`
  title == the SUMMARY link text.
- Edit `SUMMARY.md`: replace `  * [Graphs](part2/graphs/README.md)` with that
  line + 9 nested entries at one deeper indent.

## Done

Every ledger row reaches a final status, including the **dual verification**
(technical accuracy vs the stdlib docs + idiomatic LFE; voice vs the style
guide). Write `closing-report.md` (+ bubble-up) and hand off for
`cdc-verification.md` (independent technical + voice review).
