# CC Prompt — Slice 03 (Bitstrings & Operators)

Split-and-place, same shape as slices 01–02. Apply `../arc-plan.md` §6
conventions verbatim. Sources: `workbench/bytes/10-bitstrs-README.md`,
`11-ops-README.md`. Targets: `src/part2/byte-bin/bitstrs/`, `ops/`. Work the
ledger row by row.

`bitstrs/` is a clean `##`→leaf split (its `###`s — Identification in the REPL,
Why Non-Alignment Happens, Padding to Byte Alignment — stay inline as `##`).

`ops/` has one §6.8 promotion:
- `ops/pract.md` ← the draft's `### Practical Use: Extraction and Division`
  (nested under `## Bit Shift Right`), retitled `#`.
- `ops/bsr.md` ← the BSR `##` section **without** that subsection.
- The other operators (`and`/`or`/`xor`/`not`/`bsl`) keep their own
  `### Practical Use: …` inline (demoted to `##`) — the ToC gives them no
  separate leaf.
- `ops/README.md` ← chapter intro **including the operator summary table**.

Drop the trailing `*Next: …*` footers (§6.9). Don't edit `SUMMARY.md`. Don't
touch `ser/`/`realwrld/`. Preserve prose/code verbatim. Finish with
`closing-report.md` (per-row walk + bubble-up) and hand off for
`cdc-verification.md`.
