# Slice 03 — Bitstrings & Operators

> Plan-of-record. Parent: `../arc-plan.md`. Conventions: `arc-plan.md` §6.

## Goal

Publish two `workbench/bytes/` drafts into their sections: `bitstrs/`
(non-byte-aligned data) ← `10-bitstrs-README.md`, and `ops/` (bitwise operators)
← `11-ops-README.md`. 26 leaf files.

## Scope

**In:** `byte-bin/bitstrs/` (13 leaves) and `byte-bin/ops/` (13 leaves).
**Out:** `ser/`, `ser/realwrld/`, `realwrld/` (slice04); `SUMMARY.md` edits.

## Method

Per §6. One §6.8 promotion in `ops/`: the ToC lists `ops/pract.md`
("Practical Use: Extraction and Division") as its own leaf, but in the draft
that is a `### Practical Use: Extraction and Division` nested under
`## Bit Shift Right (BSR)`. So:

- `ops/pract.md` ← that `###` (promoted, `#` title).
- `ops/bsr.md` ← the BSR `##` section **without** its Practical-Use subsection.
- The *other* operators (`and`, `or`, `xor`, `not`, `bsl`) keep their own
  `### Practical Use: …` subsection inline (demoted to `##`), because the ToC
  gives them no separate leaf. (Asymmetric, but it is what `SUMMARY.md`
  dictates.)

`ops/README.md` gets the chapter intro **including the operator summary table**
(lines before the first `##`). Drop the drafts' trailing "*Next: …*" footers
(§6.9). `bitstrs/` is otherwise a clean `##`→leaf split.

## Verification approach

Independent CDC re-read vs. both drafts: no placeholders, fidelity, the
`bsr`/`pract` split correct with no duplication, no dropped subsection, tables
and ASCII intact.

## Exit criteria

All 26 leaves real; `bsr`/`pract` split correct; ledger walked;
`closing-report.md` + `cdc-verification.md` written with bubble-up.
