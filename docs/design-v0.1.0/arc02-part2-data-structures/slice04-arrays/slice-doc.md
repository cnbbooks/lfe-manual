# Slice 04 — Arrays

> Plan-of-record. Parent: `../arc-plan.md`. Conventions: arc01 §6 + arc02 §A2.

## Goal

Publish the Arrays chapter multi-leaf from `workbench/arrays/new-section-arrays.md`
(~578 ln): README landing page + 13 leaves, and add 13 sub-entries to
`src/SUMMARY.md`.

## Dir state (Read-probe, file tools = ground truth)

`src/part2/arrays/` has **only README.md** — NOT pre-scaffolded. Read-probed 13
natural slugs + 5 alternates: all "file does not exist". So this slice **creates**
the 13 leaf files (first arc02 chapter where the §A2.2 "no stub → derive slug"
branch actually applies). (Note: the bash `ls` is unreliable, but here the
Read-probe — which is ground truth — agrees the dir is README-only.)

## Leaf breakdown (draft `##` → slug; full heading = leaf `#` = SUMMARY text)

| Slug | `##` section |
|------|--------------|
| README | H1 + intro paragraphs + `## A Brief Philosophical Interlude` |
| creating | Creating Arrays |
| zero-based | The Zero-Based Heresy |
| basic | Basic Operations |
| fixed-ext | Fixed vs. Extendible Arrays |
| convert | Conversion Operations |
| hof | Higher-Order Functions |
| defaults | Default Values and Philosophy |
| perf | Performance Characteristics |
| when | When to Use Arrays |
| examples | Working Examples |
| pitfalls | Common Pitfalls |
| integration | Integration with Other LFE Patterns |
| pragmatism | A Final Note on Pragmatism |

## Method

Per §A2. README = draft H1 "Arrays" + the two pre-`##` intro paragraphs + the
"## A Brief Philosophical Interlude" section. `lisp`→`lfe` fences. The draft uses
**bold** inline sub-labels (e.g. `**Setting values:**`) inside several sections
— these are NOT headings, keep them as bold text (no `###`→`##` needed). Draft
examples use `lfe>` prompts — keep. `pragmatism.md` keeps the closing
"starting from zero seems as reasonable a choice as any" line.

## Verification approach

Independent CDC: README + 13 leaves real and faithful; SUMMARY 13 entries match
titles + resolve; every `##` mapped once; bold sub-labels preserved.

## Exit criteria

14 files, 13 SUMMARY entries, ledger walked, close set with bubble-up.
