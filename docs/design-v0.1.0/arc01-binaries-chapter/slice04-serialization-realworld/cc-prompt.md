# CC Prompt — Slice 04 (Serialization & Real-World)

Final content slice of arc01. Split-and-place, same shape as slices 01–03.
Apply `../arc-plan.md` §6 verbatim. Sources: `workbench/bytes/12-serialization.md`,
`13-real-world-applications.md`. Work the ledger row by row.

`ser/` (← draft 12) is a clean `##`→leaf split into
`src/part2/byte-bin/ser/`.

`13-real-world-applications.md` splits across **two paths** (a pre-existing ToC
quirk — follow `SUMMARY.md` exactly, don't relocate):
- intro → `src/part2/byte-bin/realwrld/README.md`
- `## Example 1/2/3`, `## Performance Considerations`, `## Debugging`, `## Summary`
  → `src/part2/byte-bin/ser/realwrld/{ipv4,custom,logform,perf,debug,summ}.md`

Notes:
- Both drafts already use `lfe>` prompts. Normalize `lisp`→`lfe` fences.
- Preserve the ASCII header/frame diagrams (plain ``` blocks) and the
  Erlang-flavored `<<…>>` snippets in draft 13 **verbatim** — illustrative, do
  not validate/rewrite.
- Neither draft has a "*Next:*" footer; keep their closing paragraphs.
- Don't edit `SUMMARY.md`. Preserve prose/code verbatim otherwise.

Finish with `closing-report.md` (per-row walk + bubble-up) and hand off for
`cdc-verification.md`. After this slice closes, arc01 close follows
(composition check + project bubble-up).
