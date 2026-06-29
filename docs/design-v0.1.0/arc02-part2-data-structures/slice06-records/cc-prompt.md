# CC Prompt — Slice 06 (Records)

Final content slice of arc02. Publish the Records chapter multi-leaf. Source:
`workbench/records/new-section-records.md`. Target: `src/part2/records/`. Apply
arc01 §6 + arc02 §A2. Work the ledger; slug→section map in `slice-doc.md`.

`git ls-files` shows records/ README-only — likely create the leaves; the
Write-probe confirms per file ("not read yet" ⇒ adopt; clean create ⇒ new).

- README (Read the `# Records` placeholder then overwrite): draft H1 + the italic
  subtitle line + the `## The Fundamental Problem…` motivation section.
- Each `##` → its slug leaf, full heading as both `#` title and SUMMARY text.
- `###` method subsections (in accessing.md) and pattern subsections (in
  patterns.md) → `##`.
- `lisp`→`lfe` fences; the one already-`lfe` fence (header-file example) stays.
- This draft uses bare `>` REPL prompts → normalize to `lfe>`.
- `see-also.md` keeps the trailing `---` + italic "*This guide compiled at
  compile-time…*" colophon.
- `SUMMARY.md`: replace `  * [Records](part2/records/README.md)` with that line +
  15 nested entries at one deeper indent.

Don't touch other chapters. Preserve prose/code verbatim. Finish with
`closing-report.md` + hand off for `cdc-verification.md`. Then arc02 close.
