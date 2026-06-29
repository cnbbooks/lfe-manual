# CC Prompt — Slice 02 (Property Lists)

Publish the Property Lists chapter multi-leaf, same shape as slice01 (Tuples).
Source: `workbench/proplists/new-section-proplists.md`. Target:
`src/part2/proplists/`. Apply arc01 §6 + arc02 §A2 (`../arc-plan.md`). Work the
ledger row by row; slug→section map is in `slice-doc.md`.

- README (Read then overwrite the `# Property Lists` placeholder): draft H1
  ("Proplists: The Elegant Simplicity of Named Chaos") + the draft's intro
  section ("## In Which We Discover…"). Landing page only.
- Each `##` section → its slug leaf, heading as both `#` title and SUMMARY link
  text. The `###` function docs (get_value, etc.) stay inline, demoted to `##`.
- `lisp`→`lfe` fences. Keep the `lfe>` REPL prompts (this draft has them).
  `concl.md` keeps the "Long live the proplist." closing.
- `SUMMARY.md`: replace the single `  * [Property Lists](part2/proplists/README.md)`
  line with that line + 21 nested entries at one deeper indent.

Tooling note: the dir can't be pre-listed (glob unreliable; shell can't mount).
Attempt writes; if a Write fails "not read yet" the stub pre-exists — Read then
write. Flag any leftover stub at an unexpected slug.

Don't touch other chapters / other SUMMARY blocks. Preserve prose/code verbatim.
Finish with `closing-report.md` + hand off for `cdc-verification.md`.
