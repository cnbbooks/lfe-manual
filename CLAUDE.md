# LFE Machine Manual ("Chineual") — working notes

## Project management

Planning artifacts live under `docs/design-v0.1.0/…`, per the canonical layout
in `collaboration-framework/docs/PROJECT-MANAGEMENT.md` (v2.1). Confirmed with
the operator 2026-06-28 — this project uses the default layout, no overrides.

- `docs/design-v0.1.0/project-plan.md` — arc roadmap + project ledger (DoD:
  publish the staged `workbench/` drafts into `src/`; new-authoring deferred).
- `arc01-binaries-chapter/` — active arc: finish the Bits/Bytes/Binaries chapter
  by splitting `workbench/bytes/00–14` drafts into `src/part2/byte-bin/` leaves.
  Batched into 4 slices. **slice01 CDC-closed (2026-06-28)** — `bifs/` finished
  and `bits/`/`types/`/`sizes/` published; byte-bin now 51/130 real. Next:
  slice02 (matching-comprehensions: `ends/`, `patts/`, `comps/`). Reusable
  publishing conventions are pinned in `arc-plan.md` §6.
- `arc02-part2-data-structures/` — roadmap-only (plan late): publish the six
  drafted Part II chapters (Tuples, Proplists, Maps, Arrays, Dicts, Records).

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
