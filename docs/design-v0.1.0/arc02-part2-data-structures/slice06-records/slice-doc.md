# Slice 06 — Records (final slice of arc02)

> Plan-of-record. Parent: `../arc-plan.md`. Conventions: arc01 §6 + arc02 §A2.

## Goal

Publish the Records chapter multi-leaf from `workbench/records/new-section-records.md`
(~372 ln): README landing + 15 leaves, add 15 SUMMARY entries. Last content
slice; after it, arc02 closes.

## Dir state

`git ls-files src/part2/records/` = README-only → likely un-scaffolded (create
leaves). Per-file the Write-probe is authoritative: "not read yet" ⇒ stub exists
(adopt); clean create ⇒ new.

## Leaf breakdown (draft `##` → slug; full heading = leaf `#` = SUMMARY text)

| Slug | `##` section |
|------|--------------|
| README | H1 + subtitle + `## The Fundamental Problem (And Why Records Are a Beautiful Hack)` (motivation/intro) |
| declaring | Declaring Records: The Art of Structured Procrastination |
| creating | Creating Records: Instantiation Without the Existential Crisis |
| accessing | Accessing Fields: The Dot Syntax Situation (`###` methods → `##`) |
| updating | Updating Records: The Philosophy of Selective Modification |
| practice | Records in Practice: A Complete Example |
| shell | The Shell and Records: A Slightly Awkward Relationship |
| truth | The Terrible Truth: Records Are Just Tuples |
| metadata | Record Metadata: Asking Questions About Records |
| guards | Guards and Records: Testing for Type |
| nested | Nested Records: Here Be Dragons |
| headers | Header Files: Sharing Records Between Modules |
| vs-maps | Maps vs Records: The Eternal Question |
| patterns | Practical Patterns: Working With Records Idiomatically (`###` patterns → `##`) |
| wisdom | Final Wisdom |
| see-also | See Also (keeps the closing italic colophon) |

## Method

Per §A2. README = H1 + italic subtitle + the Fundamental Problem section.
`lisp`→`lfe` fences (one fence is already `lfe` — the header-file example — keep).
This draft uses bare `>` REPL prompts in transcripts → normalize to `lfe>`
(§6.4). `###` method/pattern subsections → `##`. `see-also.md` keeps the trailing
`---` + italic "*This guide compiled at compile-time…*" colophon.

## Verification approach

Independent CDC: README + 15 leaves real and faithful; `>`→`lfe>` applied;
SUMMARY 15 entries match titles + resolve; every `##` mapped once; colophon kept.

## Exit criteria

16 files, 15 SUMMARY entries, ledger walked, close set with bubble-up. Then
arc02 close (composition check + project bubble-up).
