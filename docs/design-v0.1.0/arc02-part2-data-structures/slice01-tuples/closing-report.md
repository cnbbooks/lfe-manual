# Closing Report — Slice 01 (Tuples)

Written by the implementing context (CC) at slice close, 2026-06-28.

## Per-row walk

All 17 ledger rows reached a final status: **16 done, 1 deferred, 0 silent
drops.** Open-count (17) == close-count (17).

- **Rows 1–13 (files):** done. README is a short landing page (draft H1 + intro
  section); the 12 `##` sections each became their slug leaf. `examples.md`
  promoted its `### Example 1/2/3` to `##`; `summ.md` kept the closing
  "shall we move on to lists?" transition.
- **Row 14 (ToC):** done. 12 nested entries added under the Tuples line in
  `SUMMARY.md` at one deeper indent; CDC confirmed link text == leaf titles and
  all paths resolve.
- **Rows 15–16 (reconciliation):** done. CDC bijective mapping (13/13), no
  placeholders, code intact.
- **Row 17 (mdBook build):** deferred to the operator build at arc02 close (A5).

### Disclosed deviations from verbatim

`lisp`→`lfe` fences; `###`→`##` in `examples.md`. No `lfe>` prompts in this
draft (examples are bare code snippets, kept as-is). Prose/code preserved
verbatim.

## Bubble-up to the arc

**1. Did this slice deliver its assigned piece?** Yes — the Tuples chapter is
published multi-leaf (README + 12 leaves) with its ToC entries, CDC-verified.

**2. What did implementing this slice reveal that the arc-plan did not
anticipate?**

- **The leaf stubs already exist.** All 12 Tuples leaf files were already on
  disk as lone-heading placeholders, with the exact slugs and `#` titles I'd
  planned — i.e. a prior session had already scaffolded the multi-leaf
  structure (files + headings), leaving only the content and the ToC entries
  undone. Two consequences for slices 02–06:
  - **Check the chapter dir first.** Before designing slugs, `ls`/glob the
    `src/part2/<chapter>/` dir — the canonical slug set and leaf titles are
    likely already there. Adopt them rather than inventing (avoids slug drift).
    Refines §A2.2.
  - **Files must be Read before Write** (they exist), and the §A2.1 ToC-authoring
    step is still needed (the stubs were never linked in `SUMMARY.md`).
- This makes slices 02–06 lower-risk than estimated: the leaf breakdown is
  partly pre-decided by the existing stubs; verify it matches the draft's `##`
  structure and fill.

**3. Silent-drop diff.** Specified: Tuples chapter, multi-leaf, with ToC.
Delivered: all 13 files + 12 ToC entries. Only deferral is the operator mdBook
build. No undisclosed omissions.

## Arc-plan update decision

Additive refinement to §A2.2 (check the chapter dir for pre-scaffolded stubs
before designing slugs). No change to the slice breakdown or sequencing.
Recorded in `arc-plan.md` v1.1.
