# Closing Report — Slice 04 (Arrays)

Written by CC at slice close, 2026-06-28.

## Per-row walk

All 18 ledger rows reached a final status: **17 done, 1 deferred, 0 silent
drops.** Open-count (18) == close-count (18).

- **Rows 1–14 (files):** done. README = H1 + two intro paragraphs + the
  Philosophical Interlude section; the 13 `##` sections each became their slug
  leaf. The draft's 19 inline **bold** sub-labels (Setting values, Mapping, etc.)
  preserved as bold text (not headings). `pragmatism.md` keeps its closing line.
- **Row 15 (ToC):** done. 13 entries; CDC confirmed exact title match + resolving
  paths.
- **Rows 16–17 (reconciliation):** done. CDC bijective mapping (13/13); grep
  confirmed 0 stray `lisp` fences; 19/19 bold labels intact.
- **Row 18 (mdBook build):** deferred to operator at arc02 close (A5).

### Disclosed deviations

`lisp`→`lfe` fences only. Bold sub-labels and `lfe>` prompts kept verbatim.
Prose/code verbatim.

## Bubble-up to the arc

**1. Assigned piece delivered?** Yes — Arrays published multi-leaf (README + 13)
with ToC, CDC-verified.

**2. What did this slice reveal — and a process correction.**

- **Both non-Write existence signals are unreliable here; only the Write-probe
  is trustworthy.** This slice exposed it cleanly:
  - bash `ls` reported `arrays/` as README-only (as it did for maps).
  - I then **Read-probed** 13 natural slugs + 5 alternates; *all 18* returned
    "file does not exist." On that basis I planned Arrays as "create the leaves
    from scratch" and the slice-doc/open-set say so.
  - But the **Write-probe** then failed "not read yet" for all 13 planned slugs
    — i.e. the stubs **do** exist — and a follow-up Read returned their headings.
  So the Read-probe gave 13 false negatives, and bash `ls` gave a false negative.
  Arrays **was** pre-scaffolded with my exact slugs, like Tuples/Proplists/Maps.
- **Corrected rule (refines §A2.2 again):** to detect pre-scaffolded stubs, use
  the **Write-probe only** — attempt the Write; "not read yet" means the file
  exists (Read then Write); a clean create means it's genuinely new. Do **not**
  trust bash `ls`, glob, *or* a Read-probe for existence on never-accessed paths
  in this repo. (The slice-doc's "not pre-scaffolded" note is superseded by this
  finding; left in place per spec-keeping, corrected here.)
- Net effect on output: none — all 13 slugs matched the stubs, zero orphans, CDC
  PASS. The wrong assumption cost a redundant Read pass, not a content error.

**3. Silent-drop diff.** Specified: Arrays multi-leaf + ToC. Delivered: all 14
files + 13 ToC entries. Only deferral is the operator build. No undisclosed
omissions.

## Arc-plan update decision

Refine §A2.2: Write-probe is the *only* reliable stub-detector here. Recorded in
`arc-plan.md` v1.4. No breakdown/sequencing change. Next: slice05 (Dicts — the
~1,170-line orddict/dict/gb_trees omnibus, the largest).
