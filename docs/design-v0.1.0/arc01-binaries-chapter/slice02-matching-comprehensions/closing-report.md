# Closing Report — Slice 02 (Matching & Comprehensions)

Written by the implementing context (CC) at slice close, 2026-06-28.

## Per-row walk

All 39 ledger rows reached a final status: **38 done, 1 deferred, 0 silent
drops.** Open-count (39) == close-count (39).

- **Rows 1–10 (`ends/`):** done. 10 leaves split from `07-ends-README.md`.
  README carries the James Nicoll epigraph + intro; `summ.md` keeps the ASCII
  decision-tree.
- **Rows 11–22 (`patts/`):** done. 12 leaves split from `08-patts-README.md`.
- **Rows 23–36 (`comps/`):** done. 14 leaves split from `09-comps-README.md`,
  including the two `###`→leaf promotions: `xform.md` (Transforming Values) and
  `xxx.md` (Bit-Level Manipulation). `reverse.md` and `binbin.md` correctly
  shed those subsections — CDC confirmed no duplication.
- **Rows 37–38 (reconciliation):** done. CDC confirmed the full subsection→leaf
  mapping (none dropped/duplicated), zero remaining placeholders, code intact.
- **Row 39 (mdBook build):** deferred — sandbox cannot mount the repo. Link
  graph unchanged (all targets pre-existed as SUMMARY-listed placeholders).
  Carry to arc close.

### Disclosed deviations from verbatim

Applied the `arc-plan.md` §6 conventions, plus two draft-specific normalizations
recorded in the slice-doc:

1. **`lisp`→`lfe` code-fence labels** across all three drafts.
2. **Dropped the hand-rolled `*Next: …*` navigation footers** at the end of
   each draft (mdBook generates prev/next navigation).
3. **`comps/` `###`→leaf promotions** for Transforming Values and Bit-Level
   Manipulation, per the ToC structure (finer than the draft's `##` nesting).

LFE code preserved verbatim (the comprehension forms `binary-comp`/`list-comp`/
`binary-gen`/`list-gen` and `<<…>>`/`<=`/`<-` generator syntax carried through
unchanged; not validated/rewritten).

## Bubble-up to the arc

**1. Did this slice deliver its assigned piece?** Yes. `arc-plan.md` assigned
slice02 the `ends/`, `patts/`, `comps/` sections; all three are published and
CDC-verified. The byte-bin chapter is now ~88/130 real (was ~52 after slice01).

**2. What did implementing this slice reveal that the arc-plan did not
anticipate?**

- **The `comps/` `###`→leaf promotion** is a structural wrinkle the §6
  conventions didn't name: the ToC is occasionally *finer* than the draft's
  `##` nesting, promoting a `###` to its own leaf. Slices 03–04 must diff the
  ToC leaf list against the draft headings per section, not assume a clean
  `##`→leaf mapping. → Recommend adding this as convention §6.8.
- **Drafts carry hand-rolled "Next:" footers** (drafts 07–09 did; 03–06 did
  not). These must be stripped. → §6.9.
- The mdBook-build deferral recurs (same tooling constraint as slice01),
  reinforcing that the build check belongs at arc close (ledger A5), run once
  over the whole chapter.

**3. Silent-drop diff.** Specified: publish `ends/`, `patts/`, `comps/`.
Delivered: all three in full. Only deferral is the in-session mdBook build
(disclosed). No undisclosed omissions.

## Arc-plan update decision

Two reusable findings (the `###`→leaf promotion rule and the "Next:" footer
strip) should be pinned so slices 03–04 apply them. Recorded in `arc-plan.md`
v1.2 (§6.8, §6.9 + Version History). No change to slice breakdown or sequencing.
