# Slice 02 — Property Lists

> Plan-of-record. Parent: `../arc-plan.md`. Conventions: arc01 §6 + arc02 §A2.

## Goal

Publish the Property Lists chapter multi-leaf from
`workbench/proplists/new-section-proplists.md` (~641 ln): a short `README.md`
landing page + 21 per-`##`-section leaves, and add 21 sub-entries to
`src/SUMMARY.md` under the existing "Property Lists" line.

## Scope

**In:** `src/part2/proplists/` README + 21 leaves; the SUMMARY Property Lists
block. **Out:** all other chapters; `workbench/proplists/lists.md` (that is the
already-published Lists chapter source, not in scope).

## Leaf breakdown (draft `##` → slug; `###` function docs stay inline as `##`)

| Slug | `##` section |
|------|--------------|
| README | H1 + `## In Which We Discover That Sometimes the Old Ways Are Best` |
| nature | The Fundamental Nature of Proplists |
| creating | Creating Proplists |
| accessing | Accessing Values: The Proplists Module (get_value/get_bool/get_all_values/lookup/lookup_all) |
| membership | Membership and Keys (is_defined/get_keys) |
| modify | Modification Operations (delete) |
| transform | Transformation Operations (append_values/compact/unfold) |
| subst | Substitution and Expansion (substitute_aliases/substitute_negations/expand/normalize) |
| split | Partitioning and Splitting (split) |
| convert | Conversion Operations (from_map/to_map) |
| patts | Pattern Matching with Proplists |
| vs-maps | Proplists vs. Maps: A Philosophical Comparison |
| defaults | The Default Values Pattern |
| idioms | Common Patterns and Idioms |
| perf | Performance Considerations |
| wild | Proplists in the Wild |
| when-not | When Not to Use Proplists |
| json | The JSON Connection (Or Lack Thereof) |
| lfe-syntax | Proplists in LFE: Syntactic Considerations |
| guidelines | Guidelines for Proplist Usage |
| testing | The Proplist Testing Pattern |
| concl | In Conclusion: The Utility of Simplicity |

## Method

Per §A2. **Note:** the dir could not be enumerated (glob unreliable here; shell
can't mount the repo). I'll attempt writes at the slugs above; the Write result
reveals which pre-exist (fill those) vs which are new (created). README = draft
H1 + intro section. Each `##` → its slug leaf with heading as both `#` title and
SUMMARY link text; `###`→`##`; `lisp`→`lfe` fences; this draft uses `lfe>`
prompts in REPL blocks — keep them. `concl.md` keeps the closing "The proplist
is dead. Long live the proplist." flourish.

## Verification approach

Independent CDC: README + 21 leaves real and faithful; SUMMARY has 21
correctly-indented entries matching leaf titles + resolving paths; every `##`
mapped once. Also flag any orphan stub files at unexpected slugs (tooling
couldn't pre-list the dir).

## Exit criteria

22 files, 21 SUMMARY entries, ledger walked, close set written with bubble-up.
