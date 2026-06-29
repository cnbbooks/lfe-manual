# Closing Report — Slice 01 (Bit Syntax & Specifiers)

Written by the implementing context (CC) at slice close, 2026-06-28.

## Per-row walk

All 36 ledger rows reached a final status: **34 done, 2 deferred, 0 silent
drops**. Open-count (36) == close-count (36).

- **Rows 1–4 (`bifs/` finish):** done. `to-term`, `byte`, `bit`, `summ` split
  from the tail of `03-binary-bifs.md`. The `bifs/` section is now complete
  (its first five files were published in the Nov-2025 pass).
- **Rows 5–13 (`bits/`):** done. All nine leaves split from
  `04-bit-syntax-fundamentals.md`. README carries the chapter intro plus the
  "In Which We Descend Into Particular Detail" opening.
- **Rows 14–21 (`types/`):** done. All eight leaves split from
  `05-type-specifiers.md`. `what.md` holds the full six-part type catalogue
  (integer/float/binary/bitstring/bytes-and-bits/utf).
- **Rows 22–30 (`sizes/`):** done. All nine leaves split from
  `06-sizes-README.md`. README retained the Ford Prefect epigraph; `summ.md`
  retained its markdown table.
- **Row 31 (fill `what/README.md`):** done. The draft (`01-what-binaries-are.md`)
  has no prose before its first `##`, so there was no section-intro to lift.
  I wrote a short in-voice bridge paragraph (light connective tissue, sanctioned
  by the slice-doc). Flagged as newly-authored, not draft-sourced.
- **Row 32 (remove `syntax/fundform.md`):** **deferred.** Operator declined the
  delete-permission request. The file is not referenced by `SUMMARY.md`, so
  mdBook will not render it — it is harmless clutter, not a broken link.
  Recommended follow-up: `rm src/part2/byte-bin/syntax/fundform.md`.
- **Rows 33–35 (reconciliation):** done. Independent CDC pass confirmed the
  30/30 subsection→leaf mapping (none dropped/duplicated), zero remaining
  lone-heading placeholders in the four sections, and intact code fences.
- **Row 36 (mdBook build):** **deferred.** The sandbox cannot mount this repo,
  so I could not run `mdbook build`. Mitigating reasoning: every file written
  pre-existed as a `SUMMARY.md`-listed placeholder, so the link graph is
  unchanged — no new broken links can have been introduced. Recommended
  follow-up: `mdbook build` on a local checkout.

### Disclosed deviations from verbatim

1. **REPL prompt normalization** `>` → `lfe>` across all leaves, matching the
   convention already present in the Nov-published files (`bifs/to-list.md`
   etc.). Not content change; presentation consistency.
2. **Code-fence label** `lisp` → `lfe` in the `sizes/` leaves (draft 06 used
   ```` ```lisp ````; the rest of the chapter and book use ```` ```lfe ````).
3. **Leaf titles use the SUMMARY link text**, not the draft's longer
   colon-subtitle headings (e.g. `# The Type Itself`, not `# The Type Itself:
   What Are These Bits, Really?`). This matches the existing `bifs/README.md`
   precedent (`# Binary BIFs` vs draft `# Binary BIFs: The Built-In Function
   Catalogue`) and keeps page title == ToC entry. The colon-subtitles were
   editorial flourishes on the section heading, preserved in spirit by the
   in-body prose; no prose or code lost.

LFE code was **not** validated or rewritten — preserved verbatim per the
assignment. (Draft 06 uses some underscore BIF spellings, e.g. `byte_size`, and
`binary-comp`/`binary-gen` comprehension forms; carried through unchanged.)

## Bubble-up to the arc

**1. Did this slice deliver its assigned piece of the arc capability?**
Yes. Per `arc-plan.md`'s slice breakdown, slice01 was to finish `bifs/` and
publish `bits/`, `types/`, `sizes/` plus the byte-bin cleanups. All four
sections are published and verified; one cleanup done, one deferred (declined
delete). The byte-bin chapter has moved from 21/130 to **51/130** real files.

**2. What did implementing this slice reveal that the arc-plan did not anticipate?**

- **A reusable publishing convention set**, now proven and worth pinning so
  slices 02–04 don't re-derive it: (a) leaf `#` title = SUMMARY link text;
  (b) section README = SUMMARY title + any pre-first-`##` draft prose (epigraph
  included) + the draft's intro flourish as a `##`; (c) draft `###` → `##` in
  leaves; (d) REPL prompts → `lfe>`; (e) code fences → `lfe`. → Recommend
  recording this in `arc-plan.md` so 02–04 are pure mechanical application.
- **Draft sections sometimes lack a section-intro** (the `what/` case): the
  landing README needs a short authored bridge. Slices 02–04 should expect to
  write minimal connective tissue for section READMEs, not just lift prose.
- **Tooling constraint:** the sandbox cannot mount the repo, so `mdbook build`
  verification (ledger row of strength *reproduced*) cannot be run in-session.
  Arc-level close should run one build across the whole chapter rather than
  relying on per-slice builds. → Surfaced to `arc-plan.md` ledger row A5.
- **File deletion needs operator permission** and may be declined; treat stray
  files as deferred cleanups, not blockers.

**3. Silent-drop diff (scope-as-specified vs scope-as-delivered).**
Specified: finish `bifs/`; publish `bits/`, `types/`, `sizes/`; fill
`what/README`; remove `fundform.md`. Delivered: all of the above **except** the
`fundform.md` deletion (deferred, disclosed) and the in-session mdBook build
(deferred, disclosed). No undisclosed omissions.

## Arc-plan update decision

Implementing this slice surfaced a reusable convention set (above) that slices
02–04 should apply verbatim. That is a change worth recording in `arc-plan.md`
(a Version-History entry + a short "publishing conventions" note), so the next
slice plans against it. Recorded in `arc-plan.md` v1.1 — see its Version
History. No change to the slice breakdown or sequencing is required.
