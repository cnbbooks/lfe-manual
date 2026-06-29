# CC Prompt — Slice 01 (Tuples)

Publish the Tuples chapter as a multi-leaf chapter. Unlike arc01, the leaf files
do NOT exist yet — you create them — and you DO edit `SUMMARY.md` (the one
sanctioned edit). Source: `workbench/tuples/new-section-tuples.md`. Target:
`src/part2/tuples/`. Work the ledger row by row.

Apply arc01 §6 + arc02 §A2.1–A2.4 (`../arc-plan.md`):
- `README.md` (currently a `# Tuples` placeholder — Read then overwrite): the
  draft's `#` H1 (keep the flourish title) + the draft's intro section
  ("## In Which We Discover…"). Short landing page, not the whole chapter.
- Each `##` section → its own new leaf file (slugs in `slice-doc.md` /
  `ledger.md`), with the heading as both the leaf `#` title and the SUMMARY
  link text.
- `examples.md`: the `### Example 1/2/3` subheadings demote to `##`.
- `summ.md`: keep the closing "Now then, shall we move on to lists?" paragraph.
- Normalize ```lisp → ```lfe. This draft has no `lfe>` REPL prompts (examples
  are bare code snippets) — leave them as-is. Preserve prose/code verbatim.

`SUMMARY.md` edit: replace the single line `  * [Tuples](part2/tuples/README.md)`
with that same chapter line followed by 12 nested entries at one deeper indent
(`    * [The Anatomy of a Tuple](part2/tuples/anatomy.md)` etc.), matching the
byte-bin indentation style.

Don't touch other chapters or other parts of SUMMARY. Finish with
`closing-report.md` (per-row walk + bubble-up) and hand off for
`cdc-verification.md`.
