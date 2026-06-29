# Ledger — Slice 03 (Bitstrings & Operators)

> Rows = target leaves + reconciliation. Open-count = 29. Close-count = 29
> (no silent drops). Conventions: `arc-plan.md` §6.
>
> **CLOSE (2026-06-28):** 28 `done`, 1 `deferred` (mdBook build, tooling).
> Normalizations: `lisp`→`lfe` fences; `###`→`##`; leaf titles = SUMMARY link
> text; "Next:" footers dropped. `ops/pract.md` promoted per §6.8; `ops/bsr.md`
> excludes it (no duplication). Independent CDC re-read: PASS on all 26 leaves —
> see `cdc-verification.md`.

## Group 1 — `bitstrs/` (source: `10-bitstrs-README.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 01 | `bitstrs/README.md` | H1 + epigraph + intro | done | CDC PASS; dolphins epigraph retained |
| 02 | `bitstrs/what.md` | `## What Bitstrings Are` | done | CDC PASS |
| 03 | `bitstrs/create.md` | `## Creating Bitstrings` | done | CDC PASS |
| 04 | `bitstrs/size.md` | `## Size Queries` | done | CDC PASS |
| 05 | `bitstrs/patts.md` | `## Pattern Matching Bitstrings` | done | CDC PASS |
| 06 | `bitstrs/append.md` | `## Appending Bitstrings` | done | CDC PASS |
| 07 | `bitstrs/morse.md` | `## Practical Example: Morse Code` | done | CDC PASS |
| 08 | `bitstrs/ecode.md` | `## Variable-Length Encoding Example` | done | CDC PASS |
| 09 | `bitstrs/align.md` | `## The Alignment Problem` | done | CDC PASS |
| 10 | `bitstrs/extract.md` | `## Extracting Bits from Bytes` | done | CDC PASS |
| 11 | `bitstrs/why.md` | `## Why Bitstrings Matter` | done | CDC PASS |
| 12 | `bitstrs/perf.md` | `## Performance Characteristics` | done | CDC PASS |
| 13 | `bitstrs/summ.md` | `## Summary` | done | CDC PASS |

## Group 2 — `ops/` (source: `11-ops-README.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 14 | `ops/README.md` | H1 + epigraph + intro + operator table | done | CDC PASS; table retained |
| 15 | `ops/and.md` | `## Bitwise AND` (incl Masking) | done | CDC PASS |
| 16 | `ops/or.md` | `## Bitwise OR` (incl Setting Bits) | done | CDC PASS |
| 17 | `ops/xor.md` | `## Bitwise XOR` (incl Toggling/Encryption) | done | CDC PASS |
| 18 | `ops/not.md` | `## Bitwise NOT` (incl Creating Masks) | done | CDC PASS |
| 19 | `ops/bsl.md` | `## Bit Shift Left` (incl Scaling/Positioning) | done | CDC PASS |
| 20 | `ops/bsr.md` | `## Bit Shift Right` (minus promoted Practical Use) | done | CDC PASS; no pract content (no duplication) |
| 21 | `ops/pract.md` | `### Practical Use: Extraction and Division` (promoted) | done | CDC PASS; holds unpack-rgb→#(255 128 64) |
| 22 | `ops/combo.md` | `## Combining Operators: Real-World Example` | done | CDC PASS |
| 23 | `ops/perf.md` | `## Performance Characteristics` | done | CDC PASS |
| 24 | `ops/when.md` | `## When to Use Bitwise Operators` | done | CDC PASS |
| 25 | `ops/negs.md` | `## A Note on Negative Numbers` | done | CDC PASS |
| 26 | `ops/summ.md` | `## Summary` (table) | done | CDC PASS; table intact |

## Group 3 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 27 | Every `##` subsection (+ `ops/pract` promotion) maps to one leaf; `bsr.md` excludes promoted content. | reconciled | done | CDC confirmed asymmetry intentional, no duplication |
| 28 | No lone-heading placeholder in `bitstrs/`/`ops/`; code/tables intact; `lisp`→`lfe`; "Next:" footers dropped. | reproduced | done | CDC read all 26 |
| 29 | mdBook build, no broken ToC links. | reproduced→**deferred** | **deferred** | sandbox can't mount repo; link graph unchanged. Carry to arc close. |
