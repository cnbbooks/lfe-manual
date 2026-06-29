# Slice 01 — Tuples

> Plan-of-record. Parent: `../arc-plan.md`. Conventions: arc01 §6 + arc02
> addendum §A2.1–A2.4.

## Goal

Publish the Tuples chapter as a multi-leaf chapter from
`workbench/tuples/new-section-tuples.md`: a short `README.md` landing page plus
12 per-section leaf files, and add the 12 sub-entries to `src/SUMMARY.md` under
the existing Tuples line.

## Scope

**In:** `src/part2/tuples/` — README (rewrite landing page) + 12 new leaf files;
and the `SUMMARY.md` sub-entries for Tuples.
**Out:** all other chapters; the already-published Lists chapter; any
`SUMMARY.md` change outside the Tuples block.

## Leaf breakdown (draft `##` → slug)

| Slug | Source `##` section |
|------|---------------------|
| `README.md` | H1 + `## In Which We Discover That Order Matters, But Only Sometimes` (intro) |
| `anatomy.md` | `## The Anatomy of a Tuple` |
| `tagged.md` | `## Tagged Tuples: A Convention Posing as Wisdom` |
| `creating.md` | `## Creating Tuples: An Exercise in Spontaneous Existence` |
| `extract.md` | `## Extracting Values: Pattern Matching to the Rescue` |
| `anon.md` | `## The Anonymous Variable: Underscore of Mystery` |
| `bifs.md` | `## Tuple BIFs: The Standard Library Weighs In` |
| `compare.md` | `## Comparing Tuples: In Which Order Matters` |
| `nested.md` | `## Nested Tuples: Turtles All the Way Down` |
| `vs-lists.md` | `## Tuples vs. Lists: A Brief Philosophical Digression` |
| `failures.md` | `## Pattern Matching Failures: When Things Go Splendidly Wrong` |
| `examples.md` | `## Practical Examples: Tuples in the Wild` (incl `### Example 1/2/3` → `##`) |
| `summ.md` | `## Summary: Tuples in Brief` (keeps the closing "shall we move on to lists?" transition) |

## Method

Per §A2.1–A2.4: README = draft H1 + intro section; each `##` → its slug leaf
with the heading as both `#` title and SUMMARY link text; `###`→`##`;
`lisp`→`lfe` fences (no `lfe>` prompts in this draft — examples are bare code
snippets). Edit `SUMMARY.md`: replace the single Tuples line with the chapter
line + 12 nested leaf lines at one deeper indent.

## Verification approach

Independent CDC: README + 12 leaves are real and faithful; SUMMARY has 12 new
correctly-indented entries whose text matches the leaf titles and whose links
resolve to the created files; no `##` section dropped/duplicated.

## Exit criteria

13 files written, 12 SUMMARY entries added, ledger walked, close set written
with bubble-up to the arc.
