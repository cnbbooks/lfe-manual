# Project Plan — Publish the staged workbench drafts into the manual

> Plan-of-record (the arc roadmap) for the effort to land the finished
> drafts sitting in `workbench/` as real chapter content in `src/`.
> Layout per `collaboration-framework/docs/PROJECT-MANAGEMENT.md` v2.1.
> Design-doc version: v0.1.0.

## 1. Definition of done and boundaries

**What this project delivers.** Every book section that *already has a
finished draft in `workbench/`* is published into `src/` as real content,
matching the table of contents in `src/SUMMARY.md`, with no placeholder
(lone-heading) files left in those chapters.

Concretely, "done" means:

- The **Bits, Bytes & Binaries** chapter (`src/part2/byte-bin/`) is complete:
  all ToC-listed leaf files carry real prose/code, traceable to the
  `workbench/bytes/00–14` source drafts.
- The six **Part II data-structure** chapters that have finished drafts —
  Tuples, Property Lists, Maps, Arrays, Dicts, Records — are published from
  their `workbench/*/new-section-*.md` sources.
- The byte-bin cleanups (below) are resolved.
- `mdBook` builds the affected chapters with no broken ToC links.

**What this project explicitly does NOT deliver (out of scope, roadmap-only):**

- Authoring the draft-less Part II chapters: Characters & Strings, Graphs,
  Queues, Pattern Matching, Generic Sequence Functions, Manipulating List
  Structure. (These have reference PDFs or nothing — original writing.)
- The AI-resources Pocket Reference and stdlib inventories (Part VII stubs;
  `workbench/lfe-pocket-reference-project-spec.md` is its seed).
- The Typed LFE chapter (`workbench/typed-lfe-chapter-bootstrap.md` seed;
  not yet in the ToC).
- The remaining placeholder Parts (III, IV, V, and the unfinished portions of
  VI, VII, VIII).

This file is the plan, not the design. The "design" here is thin by nature:
the work is *splitting* existing drafts into the ToC's file structure, not
inventing content. The controlling spec is `src/SUMMARY.md` (the target
structure) plus the `workbench/` drafts (the source content).

## 2. The arc roadmap

| Arc | Capability | Depends on | Status |
|-----|-----------|-----------|--------|
| **arc01 — binaries-chapter** | The Bits/Bytes/Binaries chapter is fully published from `workbench/bytes/`, no placeholders, reads coherently end to end. | — (resumes interrupted Nov 2025 work) | **✅ CLOSED 2026-06-28** (see `arc01-binaries-chapter/closing-report.md`) |
| **arc02 — part2-data-structures** | The six drafted Part II data-structure chapters (Tuples, Proplists, Maps, Arrays, Dicts, Records) are published from their workbench drafts. | none (independent of arc01; sequenced after it) | **active next** — roadmap-only, plan in detail when started (adopt arc01's §6 conventions) |

Arc 2 is deliberately **not** planned in detail yet (*plan late, plan deep*):
its `arc-plan.md` is written when arc01 closes, so it can absorb anything
arc01 reveals about the split-from-workbench workflow.

## 3. Current status

- **arc01 is CLOSED (2026-06-28).** All four slices CDC-closed; the entire
  Bits/Bytes/Binaries chapter is published. Independent whole-chapter sweep:
  **153 files, 0 placeholders.** Composition check passed (`arc01-binaries-
  chapter/closing-report.md`).
- **arc02 is now the active-next arc** (roadmap-only; plan in detail when
  started). It will publish the six drafted Part II data-structure chapters
  (Tuples, Proplists, Maps, Arrays, Dicts, Records) using arc01's proven §6
  publishing conventions.
- **One open item, project-scope:** a single `mdbook build` / `make run` on a
  local checkout (sandbox can't mount the repo). This is the standing close step
  for documentation arcs here; it closes project-ledger row P4. The stray
  `syntax/fundform.md` is now absent (resolved).

### Known cleanups (carried as ledger rows where they apply)

- `src/part2/byte-bin/what/README.md` is a stub although its leaf files are
  written — the section intro was skipped. (→ arc01 slice01)
- `src/part2/byte-bin/syntax/fundform.md` is a stray placeholder not in the
  ToC. (→ arc01 slice01: remove or reconcile)
- Part IV filenames are misspelled `REAEDME.md` (`ports/`, `servers/`,
  `clients/`). Out of project scope, but logged here so it is not lost.
- `src/part9/README.md` internally self-labels "Part VIII". Out of scope; logged.

## 4. Project ledger (verifies the DoD)

Composition criteria, stated up front; each closes (per-row walk) in this
project's `closing-report.md` at project close. Strengths per
`LEDGER-DISCIPLINE.md` (`asserted < attested < reproduced < reconciled`).

| # | Criterion | How verified (target strength) | Status |
|---|-----------|-------------------------------|--------|
| P1 | No placeholder (lone-heading) files remain among the ToC-listed `byte-bin` leaves. | *reproduced* — scan `src/part2/byte-bin/` for files whose only non-blank line is a heading; expect zero. | **CLOSED (arc01)** — sweep: 153 files, 0 placeholders |
| P2 | Every published byte-bin leaf's content traces to a `workbench/bytes/` source section; no source section silently dropped. | *reconciled* — diff workbench draft headings against published leaves. | **CLOSED (arc01)** — per-slice CDC reconciliations |
| P3 | The six drafted Part II chapters each have real content (no placeholder README). | *reproduced* — read each `src/part2/<chapter>/README.md`. | open — arc02 |
| P4 | All in-scope `SUMMARY.md` ToC entries resolve to non-placeholder files; mdBook builds without broken links. | *reproduced* — `mdbook build` (or link check) on the affected chapters. | **pending operator build** (byte-bin link graph unchanged; arc02 still to come) |
| P5 | Byte-bin cleanups (P-row scope: `what/README`, stray `fundform.md`) resolved. | *attested* — confirmed in arc01 close. | **CLOSED (arc01)** — `what/README` filled; `fundform.md` absent |

## 5. Version History

### v1.1 — 2026-06-28
**arc01 closed.** Bubble-up from `arc01-binaries-chapter/closing-report.md`: the
Binaries chapter delivered as the roadmap defined it (153 files, 0
placeholders), so no roadmap change forced. Closed project-ledger rows P1, P2,
P5; P4 left pending a single operator `mdbook build` (sandbox tooling limit, not
a content gap). arc02 promoted to active-next; it will reuse arc01's §6
publishing conventions. Which arc surfaced this: arc01.

### v1.0 — 2026-06-28
Initial roadmap. Two arcs: arc01 (binaries chapter, planned in detail) and
arc02 (drafted Part II data-structure chapters, roadmap-only). Scope boundary
confirmed with operator: publish staged drafts only; new-authoring deferred.
Project resumes interrupted Nov-2025 work; status snapshot captured in §3.
