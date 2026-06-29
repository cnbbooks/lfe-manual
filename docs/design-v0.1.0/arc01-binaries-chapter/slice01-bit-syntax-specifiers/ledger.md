# Ledger — Slice 01 (Bit Syntax & Specifiers)

> Acceptance criteria as verifiable rows (the steps). One row per target leaf
> plus reconciliation/cleanup rows. Discipline per
> `collaboration-framework/templates/LEDGER-DISCIPLINE.md`.
> Strengths: `asserted < attested < reproduced < reconciled`.
> Open-count = 36 rows. Close-count = 36 rows (no silent drops).
>
> **CLOSE (2026-06-28):** 34 `done`, 2 `deferred`. Two systematic, disclosed
> normalizations applied across leaves: REPL prompt `>` → `lfe>` (all sections),
> and code fence `lisp` → `lfe` (sizes section only). `what/README.md` intro is
> newly-written connective tissue (the draft had no pre-section prose) — row 31.
> Independent re-read verification (CDC): PASS — see `cdc-verification.md`.

## Group 1 — finish `bifs/` (source: `workbench/bytes/03-binary-bifs.md`)

| # | Target `src/part2/byte-bin/` | Source subsection | Status | Evidence |
|---|------------------------------|-------------------|--------|----------|
| 01 | `bifs/to-term.md` | `## binary-to-term: The Reanimator` | done | written; CDC PASS |
| 02 | `bifs/byte.md` | `## byte-size: The Measurer` | done | written; CDC PASS |
| 03 | `bifs/bit.md` | `## bit-size: The Precise Measurer` | done | written; CDC PASS |
| 04 | `bifs/summ.md` | `## Summary` | done | written; CDC PASS |

## Group 2 — `bits/` (source: `04-bit-syntax-fundamentals.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 05 | `bits/README.md` | intro + `## In Which We Descend Into Particular Detail` | done | written; CDC PASS |
| 06 | `bits/genform.md` | `## The General Form` | done | written; CDC PASS |
| 07 | `bits/segs.md` | `## Segments: The Atomic Unit` | done | written; CDC PASS |
| 08 | `bits/size.md` | `## Size Specifications` | done | written; CDC PASS |
| 09 | `bits/defsize.md` | `## Default Sizes` | done | written; CDC PASS |
| 10 | `bits/types.md` | `## Type Annotations` | done | written; CDC PASS |
| 11 | `bits/combo.md` | `## Combining Size and Type` | done | written; CDC PASS |
| 12 | `bits/vars.md` | `## Variables in Segments` | done | written; CDC PASS |
| 13 | `bits/phil.md` | `## Philosophical Implications` | done | written; CDC PASS |

## Group 3 — `types/` (source: `05-type-specifiers.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 14 | `types/README.md` | `## On the Nature of Types in Binary Segments` | done | written; CDC PASS |
| 15 | `types/what.md` | `## The Type Itself…` (integer/float/binary/bitstring/bytes-bits/utf) | done | written; CDC confirmed all 6 sub-parts |
| 16 | `types/sign.md` | `## Sign…` (unsigned/signed) | done | written; CDC PASS |
| 17 | `types/ends.md` | `## Endianness…` (big/little/native) | done | written; CDC PASS |
| 18 | `types/units.md` | `## The Unit Specifier…` | done | written; CDC PASS |
| 19 | `types/combo.md` | `## Combining Specifiers…` | done | written; CDC PASS |
| 20 | `types/edge.md` | `## Special Cases and Edge Conditions` | done | written; CDC PASS |
| 21 | `types/summ.md` | `## Summary` | done | written; CDC PASS |

## Group 4 — `sizes/` (source: `06-sizes-README.md`)

| # | Target | Source subsection | Status | Evidence |
|---|--------|-------------------|--------|----------|
| 22 | `sizes/README.md` | epigraph + `## The Fundamental Problem of Bit Accounting` | done | written; CDC confirmed epigraph retained |
| 23 | `sizes/def.md` | `## Default Sizes…` | done | written; CDC PASS |
| 24 | `sizes/sepcs.md` | `## Explicit Size Specifications…` | done | written; CDC PASS |
| 25 | `sizes/xxx.md` | `## The Unit Specification…` | done | written; CDC PASS |
| 26 | `sizes/constr8.md` | `## The Constraint of Divisibility by Eight` | done | written; CDC PASS |
| 27 | `sizes/example.md` | `## Practical Example: Fixed-Width Records` | done | written; CDC PASS |
| 28 | `sizes/patts.md` | `## The Pattern Matching Constraint` | done | written; CDC PASS |
| 29 | `sizes/utf.md` | `## A Note on UTF Strings` | done | written; CDC PASS |
| 30 | `sizes/summ.md` | `## Summary` (table) | done | written; CDC confirmed table intact |

## Group 5 — cleanups

| # | Item | Check | Status | Evidence |
|---|------|-------|--------|----------|
| 31 | Fill `what/README.md` | >1 non-blank line, not a lone heading | done | filled with newly-written in-voice intro (draft had no pre-section prose); CDC confirmed coherent |
| 32 | Remove stray `syntax/fundform.md` | file absent; not referenced by `SUMMARY.md` | **deferred** | operator declined delete permission 2026-06-28. Not in `SUMMARY.md` → mdBook does not render it; harmless. Recommend manual `rm src/part2/byte-bin/syntax/fundform.md`. |

## Group 6 — reconciliation (composition of this slice)

| # | Criterion | Strength | Status | Evidence |
|---|-----------|----------|--------|----------|
| 33 | Every `##` subsection of drafts 03(tail)/04/05/06 maps to exactly one published leaf. | reconciled | done | CDC heading-by-heading map: 30/30, none dropped, none duplicated |
| 34 | No lone-heading placeholder remains in `bifs/`, `bits/`, `types/`, `sizes/`. | reproduced | done | CDC read every target file; all carry real content |
| 35 | LFE code blocks intact (no truncation/mangling). | reproduced | done | CDC spot-diffed code fences; intact. Disclosed: `lisp`→`lfe` fence label normalized in sizes/ |
| 36 | mdBook builds; sections render with no broken ToC links. | reproduced→**deferred** | **deferred** | cannot run `mdbook build` here (repo not mountable in sandbox). Link graph unchanged: every target pre-existed as a SUMMARY-listed placeholder, so no new links introduced. Recommend `mdbook build` on next local checkout. |
