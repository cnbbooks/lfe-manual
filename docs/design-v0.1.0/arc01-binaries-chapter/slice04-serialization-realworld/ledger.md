# Ledger — Slice 04 (Serialization & Real-World)

> Rows = target leaves + reconciliation. Open-count = 21. Close-count = 21
> (no silent drops). Conventions: `arc-plan.md` §6.
>
> **CLOSE (2026-06-28):** 20 `done`, 1 `deferred` (mdBook build, tooling).
> Normalizations: `lisp`→`lfe` fences; `###`→`##`; leaf titles = SUMMARY link
> text. Path quirk respected (`realwrld/README` vs `ser/realwrld/` children).
> ASCII diagrams + Erlang `<<…>>` snippets preserved verbatim. Independent CDC
> re-read: PASS on all 18 leaves — see `cdc-verification.md`.

## Group 1 — `ser/` (source: `12-serialization.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 01 | `ser/README.md` | H1 + epigraph + intro | done | CDC PASS; epigraph retained |
| 02 | `ser/basics.md` | `## The Basics…` | done | CDC PASS |
| 03 | `ser/why.md` | `## Why Serialize?…` | done | CDC PASS |
| 04 | `ser/example.md` | `## A More Sophisticated Example: The Deep Space Signal` | done | CDC PASS |
| 05 | `ser/pres.md` | `## The Secret Sauce: What Gets Preserved` | done | CDC PASS |
| 06 | `ser/sizecompe.md` | `## Practical Considerations: Size and Compression` | done | CDC PASS |
| 07 | `ser/stor.md` | `## Storage Patterns: Files and Mnesia` | done | CDC PASS |
| 08 | `ser/evo.md` | `## Versioning and Evolution: The Heat Death Problem` | done | CDC PASS |
| 09 | `ser/dist.md` | `## The Distributed Angle: Cross-Node Communication` | done | CDC PASS |
| 10 | `ser/sec.md` | `## Security Considerations: Trust No Binary` | done | CDC PASS |
| 11 | `ser/summ.md` | `## Summary` | done | CDC PASS; transition paragraph retained |

## Group 2 — `realwrld/` + `ser/realwrld/` (source: `13-real-world-applications.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 12 | `realwrld/README.md` | H1 + epigraph + intro | done | CDC PASS; Yogi Berra epigraph retained |
| 13 | `ser/realwrld/ipv4.md` | `## Example 1: Dissecting IPv4 Datagrams` | done | CDC PASS; ASCII header diagram intact |
| 14 | `ser/realwrld/custom.md` | `## Example 2: Building a Custom Binary Protocol` | done | CDC PASS; TSP frame diagram intact |
| 15 | `ser/realwrld/logform.md` | `## Example 3: Log File Format with Binary Efficiency` | done | CDC PASS; `<<"BLOG">>` verbatim |
| 16 | `ser/realwrld/perf.md` | `## Performance Considerations: The Need for Speed` | done | CDC PASS; `<<…>>` snippets verbatim |
| 17 | `ser/realwrld/debug.md` | `## Debugging Binary Code: When Patterns Don't Match` | done | CDC PASS |
| 18 | `ser/realwrld/summ.md` | `## Summary` | done | CDC PASS |

## Group 3 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 19 | Every `##` subsection of drafts 12/13 maps to one leaf; realwrld README→`realwrld/` vs examples→`ser/realwrld/` path split respected. | reconciled | done | CDC mapping: 10/10 (12) + 6/6 (13), none dropped/duplicated |
| 20 | No lone-heading placeholder in `ser/`/`realwrld/`/`ser/realwrld/`; code/tables/ASCII diagrams intact; `lisp`→`lfe`. | reproduced | done | CDC read all 18; diagrams + `<<…>>` intact |
| 21 | mdBook build, no broken ToC links. | reproduced→**deferred** | **deferred** | sandbox can't mount repo; carry to arc close A5 |
