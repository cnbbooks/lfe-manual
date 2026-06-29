# CC Prompt — Slice 05 (Dicts omnibus)

Publish the Dicts chapter. Source: `workbench/dicts/new-section-dicts.md` (~1,168
ln). Target: `src/part2/dicts/`. Apply arc01 §6 + arc02 §A2, with the omnibus
twist below.

**Omnibus structure:** the draft has SIX `#` (H1) sub-chapters. The leaf grain is
**one leaf per `#` section** (slug map in `slice-doc.md`). Each leaf's `#` title
= that section's heading; keep ALL internal `##`/`###` headings **as-is** (no
demotion — they're already correct under the leaf `#`). Do NOT split each `##`
into its own leaf.

- README (Read the `# Dicts` placeholder then overwrite): the entire
  `# The Ancient Key-Value Triumvirate…` sub-chapter (overview + buyer's guide +
  performance) — the chapter landing/overview.
- orddict.md / dict.md / gb-trees.md / choosing.md / concl.md: each = its full
  `#` sub-chapter verbatim.
- `lisp`→`lfe` fences. Bare code examples (no `lfe>` prompts) — keep as-is.
- `concl.md` keeps the closing "...whether you need ordered access." line.
- `SUMMARY.md`: replace `  * [Dicts](part2/dicts/README.md)` with that line + 5
  nested entries at one deeper indent.

Files don't pre-exist (git shows README-only) — Write creates them. If a Write
errors "not read yet", the stub exists → Read then write. Don't touch other
chapters. Preserve prose/code verbatim. Finish with `closing-report.md` + hand
off for `cdc-verification.md`.
