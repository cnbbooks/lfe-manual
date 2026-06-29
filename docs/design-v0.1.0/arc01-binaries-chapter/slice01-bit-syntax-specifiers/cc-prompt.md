# CC Prompt — Slice 01 (Bit Syntax & Specifiers)

You are publishing four draft sections of the LFE manual's Bits/Bytes/Binaries
chapter from the `workbench/` staging area into the book's `src/` tree. This is
a **split-and-place** task: the content already exists and is good; your job is
to move each draft subsection into its target leaf file, not to write new prose.

## Repo facts

- Book source: `src/part2/byte-bin/`. ToC: `src/SUMMARY.md` (already lists every
  target leaf — do NOT edit SUMMARY).
- Drafts: `workbench/bytes/03-binary-bifs.md`, `04-bit-syntax-fundamentals.md`,
  `05-type-specifiers.md`, `06-sizes-README.md`.
- The target leaf files already exist as placeholders (a single `#` heading).
  You are filling them, not creating the tree.

## The assignment

Work the ledger (`ledger.md`) row by row. For each target leaf:

1. Find the matching `##`/`###` subsection in the named source draft (the
   ledger names it).
2. Move that content into the leaf file. Promote the subsection's heading to a
   single `#` at the top (the leaf's title — keep it consistent with the
   existing placeholder heading and the SUMMARY link text).
3. Preserve prose and **all LFE code blocks verbatim**. Do not rewrite, "improve,"
   or re-voice — the drafts have an established style; keep it.
4. `README.md` files get the section's intro paragraph(s) from the draft.
5. Add a one-line bridge sentence only where the split genuinely leaves a leaf
   starting mid-thought; otherwise add nothing.

Then the cleanups (rows 31–32): fill `what/README.md` with its section intro
(pull from `workbench/bytes/01-what-binaries-are.md`'s opening), and delete the
stray `syntax/fundform.md` (it is not in the ToC).

## Heading→leaf maps (authoritative — also in `ledger.md`)

- `bifs/` (finish): `03` → to-term ← binary-to-term; byte ← byte-size;
  bit ← bit-size; summ ← Summary.
- `bits/` ← `04`: README ← intro; genform ← General Form; segs ← Segments;
  size ← Size Specifications; defsize ← Default Sizes; types ← Type Annotations;
  combo ← Combining Size and Type; vars ← Variables in Segments;
  phil ← Philosophical Implications.
- `types/` ← `05`: README ← "On the Nature of Types…"; what ← The Type Itself;
  sign ← Sign; ends ← Endianness; units ← The Unit Specifier;
  combo ← Combining Specifiers; edge ← Special Cases and Edge Conditions;
  summ ← Summary.
- `sizes/` ← `06`: README ← "The Fundamental Problem of Bit Accounting";
  def ← Default Sizes; sepcs ← Explicit Size Specifications; xxx ← The Unit
  Specification; constr8 ← The Constraint of Divisibility by Eight;
  example ← Practical Example: Fixed-Width Records; patts ← The Pattern Matching
  Constraint; utf ← A Note on UTF Strings; summ ← Summary.

## Constraints

- Touch only the in-scope files. Do not modify `ends/`, `patts/`, `comps/`,
  `bitstrs/`, `ops/`, `ser/`, `realwrld/`, or any file outside `byte-bin/`.
- Do not edit `SUMMARY.md`.
- Do not invent content beyond minimal connective bridges.
- If a draft subsection has no clean target, STOP and flag it — do not force it.

## Definition of done

Every ledger row reaches a final status with evidence. Reconciliation rows
33–36: confirm no draft subsection was dropped or duplicated, no lone-heading
placeholder remains in the four sections, LFE code fences are intact, and the
chapter builds. Then write `closing-report.md` (per-row walk + bubble-up to the
arc) and hand off for `cdc-verification.md`.

## Useful context

- For judging LFE code correctness if anything looks off mid-move, the
  `erlang-guidelines` / LFE knowledge skills are available — but the default is
  *preserve verbatim*, not validate-and-rewrite.
- The shell sandbox cannot currently mount this repo; use file tools
  (Read/Write/Edit) on the host paths, and a narrow per-directory scan rather
  than repo-wide globs (the repo is large and times out).
