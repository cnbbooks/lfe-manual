# Ledger — Slice 03 (Maps)

> Rows = 16 files + ToC + 3 reconciliation = 20. Close-count = 20 (no silent drops).
> Conventions: arc01 §6 + arc02 §A2.
>
> **CLOSE (2026-06-28):** 19 `done`, 1 `deferred` (mdBook build, A5/operator).
> Normalization: `lisp`→`lfe` fences; `###`→`##`; the `erlang` fence in
> vs-proplists preserved. The 15 leaf stubs pre-existed at the planned slugs
> (write-probe; the bash `ls` that showed README-only was a stale sandbox view).
> Independent CDC: PASS on all 16 + ToC + fence audit — see `cdc-verification.md`.

## Group 1 — files (`src/part2/maps/`)

| # | Target | Status | Evidence |
|---|--------|--------|----------|
| 01 | README.md | done | CDC PASS; landing page only |
| 02 | nature.md | done | CDC PASS |
| 03 | creating.md | done | CDC PASS |
| 04 | operators.md | done | CDC PASS; => / := subsections |
| 05 | patts.md | done | CDC PASS |
| 06 | clauses.md | done | CDC PASS |
| 07 | module.md | done | CDC PASS; all 13 maps:* functions present |
| 08 | vs-records.md | done | CDC PASS |
| 09 | vs-proplists.md | done | CDC PASS; `erlang` fence preserved |
| 10 | vs-dicts.md | done | CDC PASS |
| 11 | ordering.md | done | CDC PASS |
| 12 | lfe.md | done | CDC PASS; map/mref/mset/mupd/map-get present |
| 13 | json.md | done | CDC PASS |
| 14 | immut.md | done | CDC PASS |
| 15 | guidelines.md | done | CDC PASS |
| 16 | concl.md | done | CDC PASS; "#M(answer 42)" closing retained |

## Group 2 — ToC

| # | Item | Status | Evidence |
|---|------|--------|----------|
| 17 | SUMMARY Maps block | done | CDC: 15 entries, 4-space indent, link text == leaf `#` (incl. nature parenthetical), paths resolve |

## Group 3 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 18 | Every `##` section → one leaf; none dropped/duplicated. | reconciled | done | CDC 15/15 bijective mapping |
| 19 | No lone-heading placeholder; code intact; `lisp`→`lfe`; `erlang` fence preserved. | reproduced | done | CDC grep: 0 stray `lisp`, 1 correct `erlang` |
| 20 | mdBook build / link check. | reproduced→**deferred** | **deferred** | operator build at arc02 close (A5) |
