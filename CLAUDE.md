# LFE Machine Manual ("Chineual") — working notes

## Project management

This is a **living book (a Saga)** of several Projects. Planning layers:

- **`docs/book-roadmap.md`** — the **whole-book Saga roadmap** (vision +
  living-book principles + by-Part Project roadmap + open backlog). Sits above
  the per-Project trees. Read this first for the big picture. One Project per
  Part; sequenced along the reader's path (P1 Part II → P2 Part III → …).
- **Per-Project design trees** under `docs/<part-slug>/design-vX.Y.Z/…` per the
  canonical layout in `collaboration-framework/docs/PROJECT-MANAGEMENT.md`
  (v2.1). Convention confirmed with the operator 2026-06-28. P1 (Part II) is
  grandfathered at `docs/design-v0.1.0/` (pre-dates the convention; no move
  forced). Future Projects: e.g. `docs/part3-data-as-code/design-v0.1.0/`.
- **House voice:** `docs/writers-guide/cosmic-techno-wit-style-guide.md` — the
  canonical Cosmic Techno-Wit style guide (derived from the published corpus);
  all new content is authored in this voice.

### Project P1 — Part II (`docs/design-v0.1.0/`)

- `docs/design-v0.1.0/project-plan.md` — arc roadmap + project ledger. Scope:
  publish the staged `workbench/` Part II drafts, **plus** author the four
  draft-less Part II chapters (arc02 Phase 2). Part II tail (Characters &
  Strings, Manipulating List Structure) → Saga backlog.
- `arc01-binaries-chapter/` — **CLOSED (2026-06-28).** The whole Bits/Bytes/
  Binaries chapter (`src/part2/byte-bin/`) is published from `workbench/bytes/
  00–14`. All 4 slices CDC-closed; whole-chapter sweep = 153 files, 0
  placeholders (`closing-report.md`). Reusable publishing conventions pinned in
  `arc-plan.md` §6. One open item: a local `mdbook build` (the sandbox can't run
  it) closes project-ledger P4. Stray `syntax/fundform.md` is now absent.
- `arc02-part2-data-structures/` — **ACTIVE (re-opened & extended 2026-06-28).**
  - **Phase 1 ✅ delivered:** all six *drafted* chapters published as multi-leaf
    with SUMMARY sub-entries — Tuples (README+12), Property Lists (README+21),
    Maps (README+15), Arrays (README+13), Dicts (README+5 omnibus), Records
    (README+15). Sweep: **87 files, 0 placeholders.** Conventions in `arc-plan.md`
    §6 + §A2.1–A2.5. **Stub detection: Write-probe ONLY** — bash `ls`, glob,
    Read-probe, AND `git ls-files` all gave false negatives; the Write guard
    ("not read yet" ⇒ exists) is ground truth.
  - **Phase 2 (new, pending):** author & publish four *draft-less* chapters —
    **Graphs, Queues, Pattern Matching, Generic Sequence Functions** (slices
    07–10, roadmap-only). This is authoring, not split-publish. **Blocked on the
    recent whole-book plan** (Nov-2025 Claude Desktop archive) — NOT in the
    project (searched `workbench/`, `old/`, loose files, `src/`, `docs/`); the
    operator is retrieving it. Source material: `workbench/graphs|queues/*.pdf`
    and `workbench/lfe_*.md` man pages.
- Still out of scope: Characters & Strings, Manipulating List Structure,
  AI-resources pocket reference, Typed LFE chapter.

## How this book is structured

- mdBook. ToC is `src/SUMMARY.md` — it lists every leaf file (many are currently
  lone-heading placeholders awaiting content).
- `workbench/` holds finished source drafts (monolithic per chapter) plus
  reference PDFs. Publishing = split a draft's `##` subsections into the ToC's
  per-leaf files, promoting headings to `#`, preserving content verbatim.

## Build, theming & mdBook version

**Pinned toolchain.** `.github/workflows/publish.yml` pins both
`MDBOOK_VERSION` and `MDBOOK_MERMAID_VERSION` (currently mdBook `0.5.3` +
mdbook-mermaid `0.17.0`). Keep your local mdBook matching the pin so local
builds mirror the deploy. The mermaid major must match the mdBook major:
mdbook-mermaid `0.16.x` ↔ mdBook `0.4.52`; `0.17.0` ↔ mdBook `0.5`. A
mismatch fails the build with "Unable to parse the input / Broken pipe"
from the mermaid preprocessor.

**Theme is additive-only.** `theme/` holds *only* `highlight.css` and the
favicons. All LFE customization — the `lfe-pdp` color theme, fonts, alert
boxes, footnotes — lives in `css/custom.css`, wired via `additional-css` in
`book.toml` and loaded last so it wins. **Do not re-fork mdBook's internal
theme files** (`chrome.css`, `general.css`, `variables.css`, `book.js`,
`print.css`). Forking them is what broke the published site in June 2026:
the forks hard-coded mdBook's element IDs/markup and silently drifted out of
sync when mdBook upgraded (giant toolbar icons, unstyled ToC). Letting mdBook
ship its own version-correct internals is the whole point.

**Upgrade ritual (do this every time you bump the pin — it's deliberate, not
automatic).** Bump `MDBOOK_VERSION` and the matching `MDBOOK_MERMAID_VERSION`
together, then `make clean && make run` locally and eyeball: toolbar icons are
normal-sized, the ToC sidebar is styled, code highlighting + copy button work,
`.alert-*` boxes are coloured, and `print.html` hides the chrome. If a colour
looks off, a newer mdBook likely renamed or added a CSS var — add it to the
`.lfe-pdp` block in `css/custom.css`. (`make clean` only removes the generated
`book/`; it must never delete `theme/` source files.)

## Environment note

The sandboxed shell cannot currently mount this repo. Use the file tools
(Read/Write/Edit) on host paths; prefer narrow per-directory globs over
repo-wide ones (the repo is large enough that broad ripgrep scans time out).
