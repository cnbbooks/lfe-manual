# Slice 02 — Matching & Comprehensions

> Plan-of-record for this slice. Parent: `../arc-plan.md`.
> Applies the publishing conventions pinned in `arc-plan.md` §6.

## Goal

Publish three `workbench/bytes/` drafts into their `src/part2/byte-bin/`
sections: `ends/` (endianness), `patts/` (pattern matching), `comps/` (binary
comprehensions). 36 leaf files total.

## Scope

**In:**

- `byte-bin/ends/` ← `07-ends-README.md` (10 leaves).
- `byte-bin/patts/` ← `08-patts-README.md` (12 leaves).
- `byte-bin/comps/` ← `09-comps-README.md` (14 leaves).

**Out:** all other byte-bin sections (slices 03–04); `SUMMARY.md` edits.

## Method

Per `arc-plan.md` §6. Two draft-specific notes carried into this slice's
ledger:

1. **`comps/` has two `###`→leaf promotions.** The ToC gives "Transforming
   Values" (`xform.md`) and "Bit-Level Manipulation" (`xxx.md`) their own leaf
   files, though the draft nests them as `###` under "List to Binary" and
   "Binary to Binary" respectively. So `reverse.md` and `binbin.md` each get
   their parent `##` content *minus* the promoted `###` subsection, which moves
   to its own leaf with a `#` title.
2. **Drop the hand-rolled "*Next: …*" navigation footers** at the end of all
   three drafts — mdBook generates prev/next navigation. (Disclosed deviation.)

Each draft also opens with an epigraph + intro before its first `##`; that goes
on the section `README.md` per §6 rule 2.

## Verification approach

Independent CDC re-read (subagent) vs. the three drafts: no placeholders, source
fidelity, `comps/` promotions placed correctly, no dropped `##`/promoted-`###`
subsection. Ledger walked to closure.

## Exit criteria

All 36 leaves carry real content; the two `comps/` promotions land in the right
leaves; ledger fully walked; `closing-report.md` + `cdc-verification.md` written
with bubble-up to the arc.
