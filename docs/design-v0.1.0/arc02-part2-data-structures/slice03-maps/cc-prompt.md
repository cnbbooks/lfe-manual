# CC Prompt — Slice 03 (Maps)

Publish the Maps chapter multi-leaf, same shape as slices 01–02. Source:
`workbench/maps/new-section-maps.md`. Target: `src/part2/maps/`. Apply arc01 §6
+ arc02 §A2. Work the ledger; slug→section map in `slice-doc.md`.

Dir is README-only (not pre-scaffolded) — you CREATE the 15 leaf files (Write
creates new files). README: Read the `# Maps` placeholder then overwrite with
the draft H1 + intro section.

- Each `##` → its slug leaf, full heading as both `#` title and SUMMARY text.
- `###` operator/function/convenience subsections → `##`.
- `lisp`→`lfe` fences, EXCEPT keep the `erlang` fence in vs-proplists (genuine
  Erlang code). Draft examples are bare code (no `lfe>` prompts) — leave as-is.
- `concl.md` keeps the closing "It's `#M(answer 42)`." line.
- `SUMMARY.md`: replace `  * [Maps](part2/maps/README.md)` with that line + 15
  nested entries at one deeper indent.

Don't touch other chapters / SUMMARY blocks. Preserve prose/code verbatim.
Finish with `closing-report.md` + hand off for `cdc-verification.md`.
