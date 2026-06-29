# Ledger — Slice 02 (Property Lists)

> Rows = 22 files + ToC + 3 reconciliation = 26. Close-count = 26 (no silent drops).
> Conventions: arc01 §6 + arc02 §A2.
>
> **CLOSE (2026-06-28):** 25 `done`, 1 `deferred` (mdBook build, A5/operator).
> Normalization: `lisp`→`lfe` fences; `###` function docs → `##` inside category
> leaves; `lfe>` prompts kept. The 21 leaf stubs pre-existed with matching slugs
> (prior scaffolding, per §A2.2). Independent CDC: PASS on all 22 + ToC — see
> `cdc-verification.md`.

## Group 1 — files (`src/part2/proplists/`)

| # | Target | Status | Evidence |
|---|--------|--------|----------|
| 01 | README.md | done | CDC PASS; landing page only |
| 02 | nature.md | done | CDC PASS |
| 03 | creating.md | done | CDC PASS |
| 04 | accessing.md | done | CDC PASS; get_value/get_bool/get_all_values/lookup/lookup_all present |
| 05 | membership.md | done | CDC PASS |
| 06 | modify.md | done | CDC PASS |
| 07 | transform.md | done | CDC PASS; append_values/compact/unfold present |
| 08 | subst.md | done | CDC PASS; aliases/negations/expand/normalize present |
| 09 | split.md | done | CDC PASS |
| 10 | convert.md | done | CDC PASS; from_map/to_map present |
| 11 | patts.md | done | CDC PASS |
| 12 | vs-maps.md | done | CDC PASS |
| 13 | defaults.md | done | CDC PASS |
| 14 | idioms.md | done | CDC PASS |
| 15 | perf.md | done | CDC PASS |
| 16 | wild.md | done | CDC PASS |
| 17 | when-not.md | done | CDC PASS |
| 18 | json.md | done | CDC PASS |
| 19 | lfe-syntax.md | done | CDC PASS |
| 20 | guidelines.md | done | CDC PASS |
| 21 | testing.md | done | CDC PASS |
| 22 | concl.md | done | CDC PASS; "Long live the proplist." retained |

## Group 2 — ToC

| # | Item | Status | Evidence |
|---|------|--------|----------|
| 23 | SUMMARY Property Lists block | done | CDC: 21 entries, 4-space indent, link text == leaf `#` (incl. 3 short forms), paths resolve |

## Group 3 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 24 | Every `##` section → one leaf; none dropped/duplicated. | reconciled | done | CDC 21/21 bijective mapping |
| 25 | No lone-heading placeholder; code intact; `lisp`→`lfe`; no orphan stubs. | reproduced | done | CDC read all 22; all 21 stubs filled at matching slugs (no orphans) |
| 26 | mdBook build / link check. | reproduced→**deferred** | **deferred** | operator build at arc02 close (A5) |
