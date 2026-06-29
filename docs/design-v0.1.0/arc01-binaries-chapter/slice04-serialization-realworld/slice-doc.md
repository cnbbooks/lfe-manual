# Slice 04 — Serialization & Real-World (final slice of arc01)

> Plan-of-record. Parent: `../arc-plan.md`. Conventions: `arc-plan.md` §6.

## Goal

Publish the last two `workbench/bytes/` drafts: `12-serialization.md` → `ser/`,
and `13-real-world-applications.md` → `realwrld/README.md` + `ser/realwrld/`.
18 leaf files. This is the final content slice; after it, arc01 closes.

## Scope

**In:**

- `byte-bin/ser/` (11 leaves) ← `12-serialization.md`.
- `byte-bin/realwrld/README.md` (1) ← the intro of `13-real-world-applications.md`.
- `byte-bin/ser/realwrld/` (6 leaves) ← the example sections of draft 13.

**Out:** `SUMMARY.md` edits. (`concl/` is already published — not in scope.)

## Method

Per §6. Note the **pre-existing path quirk** in the ToC: the "Real-World
Applications" section's landing page is `byte-bin/realwrld/README.md`, but its
child example leaves live under `byte-bin/ser/realwrld/`. Follow the existing
file paths in `SUMMARY.md` exactly — do not "fix" or relocate them.

Draft 13's intro → `realwrld/README.md`; its `## Example 1/2/3`, `## Performance
Considerations`, `## Debugging`, `## Summary` → the six `ser/realwrld/` leaves.
Both drafts already use `lfe>` prompts; normalize `lisp`→`lfe` fences. Preserve
the ASCII header/frame diagrams (plain code blocks) and the Erlang-flavored
`<<…>>` snippets in draft 13 **verbatim** — illustrative, not validated.
Neither draft has a "Next:" footer (12 ends on a transition paragraph into
real-world; 13 ends on "bring your towel") — keep those closing paragraphs.

## Verification approach

Independent CDC re-read vs. both drafts: no placeholders, fidelity, correct
path placement (realwrld/README vs ser/realwrld/ children), no dropped section,
ASCII diagrams and tables intact.

## Exit criteria

All 18 leaves real; path quirk respected; ledger walked; close set written with
bubble-up. Then **arc01 close** (separate step): composition check + project
bubble-up.
