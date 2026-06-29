# Closing Report — Slice 05 (Dicts omnibus)

Written by CC at slice close, 2026-06-28.

## Per-row walk

All 10 ledger rows reached a final status: **9 done, 1 deferred, 0 silent
drops.** Open-count (10) == close-count (10).

- **Rows 1–6 (files):** done. README holds the Triumvirate overview; the five
  `#` sub-chapters (orddict, dict, gb_trees, choosing, conclusion) each became
  one leaf with its internal `##`/`###` hierarchy preserved verbatim. CDC
  confirmed all 18 `orddict:*` function headings, the gb_trees smart/naive API,
  and the closing lines.
- **Row 7 (ToC):** done. 5 entries; CDC confirmed exact title match and the
  gb_trees-underscore-title / gb-trees.md-hyphen-path distinction.
- **Rows 8–9 (reconciliation):** done. CDC 6/6 section mapping, no leakage, no
  stray `lisp`/`erlang` fences.
- **Row 10 (mdBook build):** deferred to operator at arc02 close (A5).

### Disclosed deviations

`lisp`→`lfe` fences only. Internal `##`/`###` kept as-is (no demotion). Prose/
code verbatim.

## Bubble-up to the arc

**1. Assigned piece delivered?** Yes — Dicts published as a 6-file omnibus
chapter (README + 5 sub-chapter leaves) with ToC, CDC-verified.

**2. What did this slice reveal?**

- **A new leaf grain for omnibus drafts.** Dicts' draft has six `#` (H1)
  sub-chapters, unlike the single-H1 + `##` shape of the other Part II chapters.
  Splitting per-`##` would have produced ~40 sibling leaves and destroyed the
  orddict/dict/gb_trees grouping. The right grain was **one leaf per `#`
  sub-chapter**, keeping each section's internal `##`/`###` intact. → Worth
  recording as convention §A2.5 for any future omnibus draft.
- **First genuinely un-scaffolded chapter.** `git ls-files` showed dicts/ as
  README-only, and all 5 leaf Writes created new files (no "not read yet"). So
  the §A2.2 "create the leaf" branch applied for real here — and the git
  ls-files signal (README-only) proved accurate, unlike the bash `ls`/Read-probe
  false-negatives seen for already-scaffolded chapters. Net: `git ls-files` is a
  reliable *committed-structure* signal; the Write outcome remains the final
  arbiter per file.

**3. Silent-drop diff.** Specified: Dicts omnibus + ToC. Delivered: all 6 files +
5 ToC entries. Only deferral is the operator build. No undisclosed omissions.

## Arc-plan update decision

Add §A2.5 (omnibus grain: one leaf per `#` sub-chapter for multi-H1 drafts).
Recorded in `arc-plan.md` v1.5. No breakdown/sequencing change. Next and last:
slice06 (Records).
