# Ledger — Slice 06 (Records)

> Rows = 16 files + ToC + 3 reconciliation = 20. Close-count = 20 (no silent drops).
> Conventions: arc01 §6 + arc02 §A2.
>
> **CLOSE (2026-06-28):** 19 `done`, 1 `deferred` (mdBook build, A5/operator).
> Normalization: `lisp`→`lfe` fences; bare `>`→`lfe>` (CDC: 0 bare prompts);
> `###`→`##` in accessing/patterns. The 15 leaf stubs pre-existed at the planned
> slugs (Write-probe; `git ls-files` README-only was a FALSE signal here).
> Independent CDC: PASS on all 16 + ToC + prompt/fence audits — see
> `cdc-verification.md`.

## Group 1 — files (`src/part2/records/`)

| # | Target | Status | Evidence |
|---|--------|--------|----------|
| 01 | README.md | done | CDC PASS; H1 + subtitle + Fundamental Problem |
| 02 | declaring.md | done | CDC PASS |
| 03 | creating.md | done | CDC PASS |
| 04 | accessing.md | done | CDC PASS; 3 method `###`→`##` |
| 05 | updating.md | done | CDC PASS |
| 06 | practice.md | done | CDC PASS; `lfe>` prompts |
| 07 | shell.md | done | CDC PASS; `lfe>` prompts |
| 08 | truth.md | done | CDC PASS; `lfe>` prompt; CRITICAL WARNING intact |
| 09 | metadata.md | done | CDC PASS; `lfe>` prompts |
| 10 | guards.md | done | CDC PASS |
| 11 | nested.md | done | CDC PASS |
| 12 | headers.md | done | CDC PASS; header-file fence kept `lfe` |
| 13 | vs-maps.md | done | CDC PASS |
| 14 | patterns.md | done | CDC PASS; Builder/Updater/Transformer `###`→`##` |
| 15 | wisdom.md | done | CDC PASS |
| 16 | see-also.md | done | CDC PASS; `---` + colophon retained |

## Group 2 — ToC

| # | Item | Status | Evidence |
|---|------|--------|----------|
| 17 | SUMMARY Records block | done | CDC: 15 entries, 4-space indent, link text == leaf `#`, paths resolve |

## Group 3 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 18 | Every `##` section → one leaf; none dropped/duplicated. | reconciled | done | CDC 15/15 bijective mapping |
| 19 | No lone-heading placeholder; code intact; `lisp`→`lfe`; `>`→`lfe>`. | reproduced | done | CDC read all 16; 0 bare prompts; 0 stray `lisp` fences |
| 20 | mdBook build / link check. | reproduced→**deferred** | **deferred** | operator build at arc02 close (A5) |
