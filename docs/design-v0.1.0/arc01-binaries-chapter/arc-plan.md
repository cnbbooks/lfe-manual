# Arc 01 — Binaries Chapter

> Plan-of-record (the slice breakdown) for completing the Bits/Bytes/Binaries
> chapter. Parent: `../project-plan.md`. Layout per
> `collaboration-framework/docs/PROJECT-MANAGEMENT.md` v2.1.

## 1. Capability statement

The **Bits, Bytes & Binaries** chapter (`src/part2/byte-bin/`) is fully
published from the `workbench/bytes/` source drafts: every leaf file named in
`src/SUMMARY.md` carries real prose and code, the chapter reads coherently
from its intro through to its conclusion in ToC order, and no placeholder
(lone-heading) files remain. This resumes and finishes the publishing pass
interrupted in November 2025, which stopped partway through the `bifs/`
section.

The work is **splitting, not authoring**: each `workbench/bytes/NN-*.md` draft
is a monolithic file whose `##`/`###` sections map onto the per-leaf files of
one `byte-bin/` subdirectory. Publishing a section means moving each draft
subsection into its target leaf file, promoting its heading to `#`, and adding
only the light connective tissue the split requires. Content is preserved, not
reinvented.

## 2. Slice breakdown (batched, 4 slices)

Operator chose batched slices (~3–4 sections each) over one-per-section. Each
slice fits one context with iteration headroom: splitting 3–4 drafts of
150–400 lines into leaf files.

| Slice | Sections published | Workbench source | Load-bearing for |
|-------|--------------------|------------------|------------------|
| **slice01 — bit-syntax-specifiers** ✅ CDC-closed 2026-06-28 | finish `bifs/` (to-term, byte, bit, summ); `bits/`; `types/`; `sizes/`; + byte-bin cleanups | tail of `03-binary-bifs`, `04-bit-syntax-fundamentals`, `05-type-specifiers`, `06-sizes-README` | establishes the split workflow the later slices reuse |
| **slice02 — matching-comprehensions** ✅ CDC-closed 2026-06-28 | `ends/`; `patts/`; `comps/` | `07-ends-README`, `08-patts-README`, `09-comps-README` | — |
| **slice03 — bitstrings-operators** ✅ CDC-closed 2026-06-28 | `bitstrs/`; `ops/` | `10-bitstrs-README`, `11-ops-README` | — |
| **slice04 — serialization-realworld** | `ser/`; `ser/realwrld/`; `realwrld/` | `12-serialization`, `13-real-world-applications` | closes the chapter; arc composition check runs here |

Already published (not in scope, listed for the composition check): chapter
`README` (← `00`), `what/` (← `01`), `syntax/` (← `02`), `concl/` (← `14`).

Sequencing: slice01 first because it both resumes the interrupted `bifs/` work
*and* shakes out the split workflow (heading promotion, leaf mapping, ToC
reconciliation) that 02–04 repeat. After that, 02–04 are independent and may
run in any order; listed order follows the ToC for reader-coherence checking.

## 3. Dependencies

- **Consumes:** the `workbench/bytes/` drafts (source content) and
  `src/SUMMARY.md` (target structure). Both already exist and are stable.
- **Leaves for later:** nothing for arc02 (independent). At arc close, bubbles
  up to the project whether the split-from-workbench workflow is reusable for
  arc02's data-structure chapters (likely yes — same shape).

## 4. Arc ledger (verifies the capability composes)

Composition rows; close as a per-row walk in this arc's `closing-report.md`,
reproduced end-to-end at arc scale.

| # | Criterion | How verified (target strength) |
|---|-----------|-------------------------------|
| A1 | Every ToC leaf under `byte-bin/` has real content. | *reproduced* — scan for lone-heading files across the whole chapter; expect zero. |
| A2 | Each published leaf traces to its `workbench/bytes/` source subsection; no draft subsection dropped. | *reconciled* — heading-by-heading diff, draft vs published, per section. |
| A3 | The chapter reads coherently in ToC order (intro → bifs → … → conclusion); section/leaf order matches `SUMMARY.md`. | *attested* — end-to-end read at arc close. |
| A4 | Cleanups resolved: `what/README` filled; stray `syntax/fundform.md` removed or reconciled into the ToC. | *reproduced* — file checks. |
| A5 | mdBook builds the chapter with no broken ToC links. | *reproduced* — build/link check. |

## 6. Publishing conventions (proven in slice01 — apply in slices 02–04)

Pin these so the remaining slices are pure mechanical application, not
re-derivation:

1. **Leaf `#` title = the `SUMMARY.md` link text** (page title == ToC entry),
   not the draft's longer colon-subtitle heading.
2. **Section `README.md`** = the SUMMARY title, then any draft prose that
   precedes the first `##` (epigraphs included), then the draft's intro flourish
   as a `##` sub-section.
3. **Draft `###` → `##`** inside leaf files (shift the whole hierarchy up one
   level, since the section heading became `#`).
4. **REPL prompts `>` → `lfe>`** (matches the Nov-published files).
5. **Code fences → ` ```lfe `** (some drafts use ` ```lisp `/` ```lisp `).
6. **Preserve prose and code verbatim** otherwise; do not validate or rewrite
   LFE code. Add a one-line bridge only where a leaf would otherwise start
   mid-thought, or where a section README has no draft intro (the `what/` case).
7. **Stray-file cleanups** need operator delete-permission; if declined, defer
   (a file absent from `SUMMARY.md` is not rendered by mdBook).
8. **The ToC is sometimes finer than the draft's `##` nesting** — a draft `###`
   may be promoted to its own leaf (proven in `comps/`: `xform.md`,
   `xxx.md`; and `ops/pract.md`). Per section, diff the SUMMARY leaf list against
   the draft headings; when a leaf has no `##` match, it's a promoted `###`. Its
   parent `##` leaf then *excludes* that subsection (no duplication). Promotions
   can be **asymmetric**: where several parallel `###`s exist (e.g. each
   operator's "Practical Use"), the ToC may promote only one and leave the
   siblings inline — follow the ToC, don't move them all.
9. **Drop hand-rolled `*Next: …*` navigation footers** from draft tails —
   mdBook generates prev/next navigation. (Some drafts have them, some don't.)

## 5. Version History

### v1.3 — 2026-06-28
slice03 CDC-closed (`bitstrs/`, `ops/` — 26 leaves). Clarified §6.8: `###`→leaf
promotions can be asymmetric (proven in `ops/`, where only the BSR "Practical
Use" was promoted to `pract.md` while the other operators kept theirs inline).
No structural change to the slice breakdown. Only slice04 (`ser/`, `realwrld/`)
remains before arc close.

### v1.2 — 2026-06-28
slice02 CDC-closed (`ends/`, `patts/`, `comps/` — 36 leaves). Added conventions
§6.8 (ToC finer than draft: `###`→leaf promotion, proven in `comps/`) and §6.9
(strip "Next:" footers), surfaced by slice02's bubble-up for slices 03–04. The
mdBook-build deferral recurs (tooling) and is confirmed as belonging to arc
close (ledger A5). No change to slice breakdown or sequencing.

### v1.1 — 2026-06-28
slice01 CDC-closed. Added §6 **Publishing conventions** (the reusable split
workflow proven in slice01: SUMMARY-title leaves, README assembly, `###`→`##`,
`lfe>` prompts, `lfe` fences). Surfaced by slice01's bubble-up so slices 02–04
apply it directly. Two slice01 deferrals carried to arc close: the stray
`syntax/fundform.md` deletion (operator-declined) and a whole-chapter
`mdbook build` (sandbox can't mount the repo) — the latter is the natural home
for arc-ledger row A5. No change to the slice breakdown or sequencing.

### v1.0 — 2026-06-28
Initial slice breakdown: 4 batched slices (operator's chosen granularity).
slice01 sequenced first to resume `bifs/` and establish the split workflow;
02–04 follow the ToC. Arc ledger A1–A5 opened.
