# Ledger — Slice 04 (Arrays)

> Rows = 14 files + ToC + 3 reconciliation = 18. Close-count = 18 (no silent drops).
> Conventions: arc01 §6 + arc02 §A2.
>
> **CLOSE (2026-06-28):** 17 `done`, 1 `deferred` (mdBook build, A5/operator).
> Normalization: `lisp`→`lfe` fences (CDC: 0 stray `lisp`). Bold inline
> sub-labels preserved (CDC: 19/19). The 13 leaf stubs pre-existed at the planned
> slugs (Write-probe; the earlier Read-probe falsely reported them absent — see
> bubble-up). Independent CDC: PASS on all 14 + ToC + bold + fence audit — see
> `cdc-verification.md`.

## Group 1 — files (`src/part2/arrays/`)

| # | Target | Status | Evidence |
|---|--------|--------|----------|
| 01 | README.md | done | CDC PASS; H1 + 2 intro paras + Philosophical Interlude |
| 02 | creating.md | done | CDC PASS |
| 03 | zero-based.md | done | CDC PASS |
| 04 | basic.md | done | CDC PASS; 5 bold sub-labels |
| 05 | fixed-ext.md | done | CDC PASS |
| 06 | convert.md | done | CDC PASS; 2 bold sub-labels |
| 07 | hof.md | done | CDC PASS; 4 bold sub-labels |
| 08 | defaults.md | done | CDC PASS |
| 09 | perf.md | done | CDC PASS |
| 10 | when.md | done | CDC PASS; 2 bold sub-labels |
| 11 | examples.md | done | CDC PASS; 3 bold sub-labels |
| 12 | pitfalls.md | done | CDC PASS; 3 bold sub-labels |
| 13 | integration.md | done | CDC PASS |
| 14 | pragmatism.md | done | CDC PASS; closing line retained |

## Group 2 — ToC

| # | Item | Status | Evidence |
|---|------|--------|----------|
| 15 | SUMMARY Arrays block | done | CDC: 13 entries, 4-space indent, link text == leaf `#`, paths resolve |

## Group 3 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 16 | Every `##` section → one leaf; none dropped/duplicated. | reconciled | done | CDC 13/13 bijective mapping |
| 17 | No lone-heading placeholder; code intact; `lisp`→`lfe`; bold sub-labels preserved. | reproduced | done | CDC read all 14; grep 0 stray `lisp`; 19/19 bold labels |
| 18 | mdBook build / link check. | reproduced→**deferred** | **deferred** | operator build at arc02 close (A5) |
