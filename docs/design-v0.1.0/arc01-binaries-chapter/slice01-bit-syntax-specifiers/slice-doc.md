# Slice 01 — Bit Syntax & Specifiers

> Plan-of-record for this slice. Parent: `../arc-plan.md`.

## Goal

Resume the interrupted publishing pass and split four `workbench/bytes/`
drafts into their target `src/part2/byte-bin/` leaf files: finish the `bifs/`
section, then publish `bits/`, `types/`, and `sizes/` in full. Resolve the two
byte-bin cleanups. This slice also establishes the split workflow (heading
promotion, leaf mapping, source→leaf reconciliation) that slices 02–04 reuse.

## Scope

**In:**

- Finish `byte-bin/bifs/`: `to-term.md`, `byte.md`, `bit.md`, `summ.md`
  (source: `workbench/bytes/03-binary-bifs.md`, the four subsections not yet
  published).
- Publish `byte-bin/bits/` in full (source: `04-bit-syntax-fundamentals.md`).
- Publish `byte-bin/types/` in full (source: `05-type-specifiers.md`).
- Publish `byte-bin/sizes/` in full (source: `06-sizes-README.md`).
- Cleanup: fill `byte-bin/what/README.md` (section intro, currently a stub).
- Cleanup: remove the stray `byte-bin/syntax/fundform.md` (not in the ToC).

**Out:**

- All other byte-bin sections (`ends/`, `patts/`, `comps/`, `bitstrs/`,
  `ops/`, `ser/`, `realwrld/`) — slices 02–04.
- `SUMMARY.md` edits — the target leaf files already exist as placeholders and
  are already in the ToC; this slice fills them, it does not restructure.
- Any content invention beyond light connective tissue.

## Method

For each target leaf: take the matching `##`/`###` subsection from the source
draft, move it into the leaf file, promote its top heading to `#`, and
preserve the prose and code verbatim. Section `README.md` files receive the
draft's chapter-level intro paragraph(s). Preserve the drafts' established
voice and all LFE code examples exactly — do not rewrite. Where a leaf needs a
one-line bridge sentence the source lacks, add it sparingly.

## Verification approach

Per `ledger.md`: every target leaf checked for real content (>1 non-blank
line, no lone heading) and source-traceability (content matches the named
draft subsection). Reconcile the source draft's heading list against the
published leaves to confirm no subsection was dropped or duplicated. Confirm
the two cleanups. Spot-check that LFE code blocks were not corrupted in the
move.

## Exit criteria

- All `bifs/`, `bits/`, `types/`, `sizes/` ToC leaves carry real content.
- `what/README.md` filled; `syntax/fundform.md` gone.
- No source subsection from drafts 03(tail)/04/05/06 left unpublished.
- Ledger fully walked; `closing-report.md` + `cdc-verification.md` written,
  including the bubble-up to `arc-plan.md`.
