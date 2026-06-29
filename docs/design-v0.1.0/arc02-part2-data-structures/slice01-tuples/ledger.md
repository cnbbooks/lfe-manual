# Ledger — Slice 01 (Tuples)

> Rows = target leaves + ToC + reconciliation. Open-count = 17. Close-count = 17
> (no silent drops). Conventions: arc01 §6 + arc02 §A2.1–A2.4.
>
> **CLOSE (2026-06-28):** 16 `done`, 1 `deferred` (mdBook build, A5/operator).
> Normalization: `lisp`→`lfe` fences; `###`→`##` in `examples.md`. No `lfe>`
> prompts in this draft. Independent CDC: PASS on all 13 files + ToC — see
> `cdc-verification.md`.
>
> **Discovery:** the 12 leaf files already existed on disk as lone-heading stubs
> with the exact slugs/titles planned (prior-session scaffolding) — so this
> slice *filled* them and *added* their ToC entries. Bubbled up to `arc-plan.md`
> (§A2.2 refined for slices 02–06).

## Group 1 — files (`src/part2/tuples/`)

| # | Target | Source `##` section | Status | Evidence |
|---|--------|---------------------|--------|----------|
| 01 | `README.md` | H1 + intro | done | CDC PASS; landing page only |
| 02 | `anatomy.md` | The Anatomy of a Tuple | done | CDC PASS |
| 03 | `tagged.md` | Tagged Tuples | done | CDC PASS |
| 04 | `creating.md` | Creating Tuples | done | CDC PASS |
| 05 | `extract.md` | Extracting Values | done | CDC PASS |
| 06 | `anon.md` | The Anonymous Variable | done | CDC PASS |
| 07 | `bifs.md` | Tuple BIFs | done | CDC PASS |
| 08 | `compare.md` | Comparing Tuples | done | CDC PASS |
| 09 | `nested.md` | Nested Tuples | done | CDC PASS |
| 10 | `vs-lists.md` | Tuples vs. Lists | done | CDC PASS |
| 11 | `failures.md` | Pattern Matching Failures | done | CDC PASS |
| 12 | `examples.md` | Practical Examples (`###`→`##`) | done | CDC PASS |
| 13 | `summ.md` | Summary | done | CDC PASS; transition paragraph retained |

## Group 2 — ToC

| # | Item | Check | Status | Evidence |
|---|------|-------|--------|----------|
| 14 | `SUMMARY.md` Tuples block | 12 nested entries, correct indent, link text == leaf title, paths resolve | done | CDC TOC PASS: exactly 12, 4-space indent, text/path match |

## Group 3 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 15 | Every `##` section maps to exactly one leaf; none dropped/duplicated. | reconciled | done | CDC bijective mapping table (13/13) |
| 16 | No lone-heading placeholder among the 13 files; code intact; `lisp`→`lfe`. | reproduced | done | CDC read all 13 |
| 17 | mdBook build / link check. | reproduced→**deferred** | **deferred** | operator build at arc02 close (A5) |
