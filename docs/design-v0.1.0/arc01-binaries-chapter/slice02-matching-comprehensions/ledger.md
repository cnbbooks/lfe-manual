# Ledger — Slice 02 (Matching & Comprehensions)

> Rows = target leaves + reconciliation. Discipline per `LEDGER-DISCIPLINE.md`.
> Open-count = 39 rows. Close-count = 39 rows (no silent drops).
> Conventions: `arc-plan.md` §6.
>
> **CLOSE (2026-06-28):** 38 `done`, 1 `deferred` (mdBook build, tooling).
> Normalizations applied: `lisp`→`lfe` fences; `###`→`##` in leaves; leaf titles =
> SUMMARY link text; hand-rolled "*Next: …*" footers dropped. Drafts already used
> `lfe>` prompts. Independent CDC re-read: PASS on all 36 leaves — see
> `cdc-verification.md`.

## Group 1 — `ends/` (source: `07-ends-README.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 01 | `ends/README.md` | H1 + epigraph + intro | done | CDC PASS; epigraph retained |
| 02 | `ends/parable.md` | `## The Parable of Big-Endian and Little-Endian` | done | CDC PASS |
| 03 | `ends/lfe.md` | `## The Endianness Specifiers in LFE` | done | CDC PASS |
| 04 | `ends/when.md` | `## When Endianness Matters` | done | CDC PASS |
| 05 | `ends/native.md` | `## The Native Option: Runtime Flexibility` | done | CDC PASS |
| 06 | `ends/patts.md` | `## Pattern Matching with Endianness` | done | CDC PASS |
| 07 | `ends/xplat.md` | `## Cross-Platform Binary Protocols` | done | CDC PASS |
| 08 | `ends/hist.md` | `## A Historical Note` | done | CDC PASS |
| 09 | `ends/examples.md` | `## Practical Examples` | done | CDC PASS |
| 10 | `ends/summ.md` | `## Summary: The Endianness Decision Tree` | done | CDC PASS; ASCII decision tree intact |

## Group 2 — `patts/` (source: `08-patts-README.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 11 | `patts/README.md` | H1 + epigraph + intro | done | CDC PASS; epigraph retained |
| 12 | `patts/symm.md` | `## The Fundamental Symmetry` | done | CDC PASS |
| 13 | `patts/basic.md` | `## Basic Pattern Matching Examples` | done | CDC PASS |
| 14 | `patts/rest.md` | `## The Rest of the Binary` | done | CDC PASS |
| 15 | `patts/whatevs.md` | `## Don't Care Variables` | done | CDC PASS |
| 16 | `patts/funcls.md` | `## Pattern Matching in Function Clauses` | done | CDC PASS |
| 17 | `patts/bits.md` | `## Bit-Level Pattern Matching` | done | CDC PASS |
| 18 | `patts/size.md` | `## The Dual Nature of Size Variables` | done | CDC PASS |
| 19 | `patts/tlv.md` | `## Complex Real-World Example: Decoding a TLV Structure` | done | CDC PASS |
| 20 | `patts/guards.md` | `## Pattern Matching with Guards` | done | CDC PASS |
| 21 | `patts/pits.md` | `## Common Pitfalls and How to Avoid Them` | done | CDC PASS |
| 22 | `patts/summ.md` | `## Summary` | done | CDC PASS |

## Group 3 — `comps/` (source: `09-comps-README.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 23 | `comps/README.md` | H1 + epigraph + intro | done | CDC PASS; epigraph retained |
| 24 | `comps/syntx.md` | `## The Basic Syntax` | done | CDC PASS |
| 25 | `comps/simple.md` | `## Binary to List: The Simplest Case` | done | CDC PASS |
| 26 | `comps/reverse.md` | `## List to Binary` (minus `### Transforming Values`) | done | CDC PASS; no xform content (no duplication) |
| 27 | `comps/xform.md` | `### Transforming Values` (promoted) | done | CDC PASS; holds `(* n 2)`→#B(2 4 6 8 10) |
| 28 | `comps/binbin.md` | `## Binary to Binary` (minus `### Bit-Level Manipulation`) | done | CDC PASS; no xxx content (no duplication) |
| 29 | `comps/xxx.md` | `### Bit-Level Manipulation` (promoted) | done | CDC PASS; holds nibble→(10..15) |
| 30 | `comps/rgb.md` | `## Practical Example: RGB Color Manipulation` | done | CDC PASS |
| 31 | `comps/cartprod.md` | `## Multiple Generators: The Cartesian Product` | done | CDC PASS |
| 32 | `comps/manip.md` | `## Bit String Manipulation` | done | CDC PASS |
| 33 | `comps/encode.md` | `## Complex Example: Run-Length Encoding` | done | CDC PASS |
| 34 | `comps/window.md` | `## Advanced Pattern: Sliding Window` | done | CDC PASS |
| 35 | `comps/perf.md` | `## Performance Considerations` | done | CDC PASS |
| 36 | `comps/summ.md` | `## Summary` | done | CDC PASS |

## Group 4 — reconciliation

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 37 | Every `##` subsection (+ the two `comps/` `###` promotions) maps to exactly one leaf; none dropped/duplicated. | reconciled | done | CDC full mapping; reverse/binbin confirmed free of promoted content |
| 38 | No lone-heading placeholder in `ends/`/`patts/`/`comps/`; code intact; `lisp`→`lfe`; "Next:" footers dropped. | reproduced | done | CDC read all 36; confirmed |
| 39 | mdBook build, no broken ToC links. | reproduced→**deferred** | **deferred** | sandbox can't mount repo; link graph unchanged (targets pre-existed in SUMMARY). Carry to arc close. |
