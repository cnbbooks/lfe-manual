# Slice 05 — Dicts (orddict / dict / gb_trees omnibus)

> Plan-of-record. Parent: `../arc-plan.md`. Conventions: arc01 §6 + arc02 §A2.

## Goal

Publish the Dicts chapter from `workbench/dicts/new-section-dicts.md` (~1,168 ln).
This draft is an **omnibus with six `#` (H1) sub-chapters**, not the usual
single-H1 + `##` structure. So the leaf grain is **one leaf per `#`
sub-chapter** (NOT per-`##`, which would flatten orddict/dict/gb_trees into ~40
confusing sibling leaves and lose the module grouping). README + 5 leaves.

## Dir state

`git ls-files src/part2/dicts/` shows **only README.md committed** — Dicts was
NOT pre-scaffolded (unlike the prior four chapters). So this slice **creates**
the 5 leaf files. (Write-probe will confirm per file: a "not read yet" error
means a stub exists → adopt; a clean create means new.)

## Leaf breakdown (draft `#` section → slug)

| Slug | `#` sub-chapter | Internal headings |
|------|-----------------|-------------------|
| README | `# The Ancient Key-Value Triumvirate: A Historical Retrospective` | keeps its `##`/`###` (overview + buyer's guide + perf) — the chapter landing/overview |
| orddict | `# orddict: The Ordered Dictionary` | keeps `##`/`###` (store/find/fetch/… as `###`) |
| dict | `# dict: The Pragmatic Workhorse (Historical Division)` | keeps `##`/`###` |
| gb-trees | `# gb_trees: The Balanced Perfectionist` | keeps `##`/`###` (smart/naive, ordered ops, iterators) |
| choosing | `# Choosing Your Key-Value Store: A Decision Framework` | keeps `##`/`###` |
| concl | `# In Conclusion: The Legacy Lives On` | — |

## Method

Per §A2, with a twist for the omnibus: **each `#` section becomes a leaf whose
`#` title is that section's heading**, and all internal `##`/`###` are kept
**as-is** (no demotion — the hierarchy is already correct under the leaf `#`).
README holds the full Triumvirate overview section (it is the chapter's intro +
buyer's guide). `lisp`→`lfe` fences. Draft examples are bare code (no `lfe>`
prompts) — keep as-is. `concl` keeps the closing "...whether you need ordered
access." line.

ToC: leaf titles = the `#` section headings; SUMMARY entries match.

## Verification approach

Independent CDC: README + 5 leaves real and faithful; each leaf == its `#`
section verbatim (internal headings intact); SUMMARY 5 entries match titles +
resolve; no `#` section dropped/duplicated; the `### orddict:store/3`-style
function headings inside leaves preserved.

## Exit criteria

6 files, 5 SUMMARY entries, ledger walked, close set with bubble-up.
