# CC Prompt — Slice 02 (Matching & Comprehensions)

Split-and-place task, same shape as slice01. Apply the publishing conventions in
`../arc-plan.md` §6 verbatim. Sources: `workbench/bytes/07-ends-README.md`,
`08-patts-README.md`, `09-comps-README.md`. Targets: `src/part2/byte-bin/ends/`,
`patts/`, `comps/`. Work the ledger row by row.

Conventions recap: leaf `#` title = `SUMMARY.md` link text; section README =
SUMMARY title + pre-first-`##` draft prose (epigraph included) + intro flourish
as `##`; draft `###` → `##`; REPL `>` → `lfe>`; code fences → `lfe`; preserve
prose/code verbatim otherwise.

Two draft-specific items (also in the ledger):

1. **`comps/` `###` promotions.** `comps/xform.md` ← the draft's `### Transforming
   Values`; `comps/xxx.md` ← the draft's `### Bit-Level Manipulation`. So
   `comps/reverse.md` and `comps/binbin.md` contain their parent `##` section
   *without* that nested `###` (it moved to its own leaf, retitled `#`).
2. **Drop the trailing `*Next: …*` footers** in all three drafts (mdBook
   generates navigation).

Do not edit `SUMMARY.md`. Do not touch sections outside ends/patts/comps. Don't
validate or rewrite LFE code — preserve verbatim. Finish by writing
`closing-report.md` (per-row walk + bubble-up) and handing off for
`cdc-verification.md`.
