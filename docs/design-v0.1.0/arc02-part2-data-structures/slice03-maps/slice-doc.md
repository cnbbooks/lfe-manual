# Slice 03 — Maps

> Plan-of-record. Parent: `../arc-plan.md`. Conventions: arc01 §6 + arc02 §A2.

## Goal

Publish the Maps chapter multi-leaf from `workbench/maps/new-section-maps.md`
(~383 ln): README landing page + 15 leaves, and add 15 sub-entries to
`src/SUMMARY.md`.

## Dir state (bash `ls`, now working)

`src/part2/maps/` contains **only README.md** — NOT pre-scaffolded. So this
slice *creates* the 15 leaf files (Write creates new files; no Read needed) and
*authors* the slugs. (Confirmed via shell that arrays/dicts/records are also
README-only — slices 04–06 will likewise create leaves.)

## Leaf breakdown (draft `##` → slug; full heading = leaf `#` = SUMMARY text)

| Slug | `##` section |
|------|--------------|
| README | H1 + `## In Which We Discover That Keys Unlock More Than Doors` |
| nature | The Fundamental Nature of Maps (Or: What Maps Actually Are When Nobody's Looking) |
| creating | Creating Maps: The Art of Literal Expression |
| operators | The Two Operators: A Tale of Intention (=> / :=) |
| patts | Pattern Matching: The Art of Selective Attention |
| clauses | Function Clauses: Maps as Dispatch Mechanism |
| module | The Maps Module: A Compendium of Useful Functions (new/size/get/find/is_key/keys/values/to_list/from_list/remove/merge/map/filter) |
| vs-records | Maps vs. Records: A Comparison of Philosophies |
| vs-proplists | Maps vs. Proplists: The Evolution of Options |
| vs-dicts | Maps vs. Dicts and Other Erlang Data Structures |
| ordering | Ordering and Comparison |
| lfe | LFE-Specific Conveniences (map / mref / mset / mupd / map-get) |
| json | The JSON Connection |
| immut | A Note on Immutability and Illusions |
| guidelines | Practical Guidelines |
| concl | In Conclusion |

## Method

Per §A2. `lisp`→`lfe` fences. **Keep the one `erlang` fence** in `vs-proplists`
(it is genuinely Erlang comparison code — do not convert). `###` function/operator
subsections → `##`. Draft examples are bare code (no `lfe>` prompts). `concl.md`
keeps the "#M(answer 42)" closing.

## Verification approach

Independent CDC: README + 15 leaves real and faithful; SUMMARY 15 entries match
leaf titles + resolve; every `##` mapped once; the `erlang` fence preserved.

## Exit criteria

16 files, 15 SUMMARY entries, ledger walked, close set with bubble-up.
