# Ledger — Slice 05 (Dicts omnibus)

> Rows = 6 files + ToC + 3 reconciliation = 10. Close-count = 10 (no silent drops).
> Conventions: arc01 §6 + arc02 §A2. Leaf grain = one per `#` sub-chapter.
>
> **CLOSE (2026-06-28):** 9 `done`, 1 `deferred` (mdBook build, A5/operator).
> Normalization: `lisp`→`lfe` fences only (CDC: 0 stray `lisp`/`erlang`).
> Internal `##`/`###` kept as-is (no demotion — omnibus grain). All 5 leaves were
> **created new** (Dicts was not pre-scaffolded; git showed README-only).
> Independent CDC: PASS on all 6 + ToC + fence audit + heading spot-checks — see
> `cdc-verification.md`.

## Group 1 — files (`src/part2/dicts/`)

| # | Target | `#` sub-chapter | Status | Evidence |
|---|--------|-----------------|--------|----------|
| 01 | README.md | The Ancient Key-Value Triumvirate (overview + buyer's guide + perf) | done | CDC PASS; 4 `##` + 3 `###` intact |
| 02 | orddict.md | orddict: The Ordered Dictionary | done | CDC PASS; all 18 `### orddict:*` fn headings present |
| 03 | dict.md | dict: The Pragmatic Workhorse (Historical Division) | done | CDC PASS; all `##`/`###` intact |
| 04 | gb-trees.md | gb_trees: The Balanced Perfectionist | done | CDC PASS; smart/naive API + ordered ops intact |
| 05 | choosing.md | Choosing Your Key-Value Store: A Decision Framework | done | CDC PASS |
| 06 | concl.md | In Conclusion: The Legacy Lives On | done | CDC PASS; closing lines retained |

## Group 2 — ToC

| # | Item | Status | Evidence |
|---|------|--------|----------|
| 07 | SUMMARY Dicts block | done | CDC: 5 entries, 4-space indent, link text == leaf `#` (gb_trees-underscore title / gb-trees.md-hyphen path), paths resolve |

## Group 3 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 08 | Every `#` sub-chapter → one leaf; none dropped/duplicated/misplaced; internal `##`/`###` preserved. | reconciled | done | CDC 6/6 mapping; `---` separators correctly not leaked |
| 09 | No lone-heading placeholder; code intact; `lisp`→`lfe`. | reproduced | done | CDC read all 6; grep 0 stray fences |
| 10 | mdBook build / link check. | reproduced→**deferred** | **deferred** | operator build at arc02 close (A5) |
