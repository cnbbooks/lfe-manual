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
- **(Added 2026-06-28)** Four **draft-less Part II chapters authored from
  scratch** and published: **Graphs, Queues, Pattern Matching, Generic Sequence
  Functions** (arc02 Phase 2). Source/reference material: the `workbench/graphs`
  & `workbench/queues` PDFs and the `workbench/lfe_*.md` man pages; scope/depth
  to be driven by the recent whole-book plan.
- The byte-bin cleanups (below) are resolved.
- `mdBook` builds the affected chapters with no broken ToC links.

**What this project explicitly does NOT deliver (out of scope, roadmap-only):**

- The other two draft-less Part II chapters: Characters & Strings, Manipulating
  List Structure. *(was: also Graphs, Queues, Pattern Matching, Generic Sequence
  Functions — those moved IN-scope 2026-06-28 as arc02 Phase 2.)*
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
| **arc02 — part2-data-structures** | **Phase 1:** publish the six *drafted* Part II chapters (Tuples, Proplists, Maps, Arrays, Dicts, Records). **Phase 2 (added 2026-06-28):** author & publish the four *draft-less* chapters (Graphs, Queues, Pattern Matching, Generic Sequence Functions). | none (independent of arc01) | **ACTIVE — re-opened & extended.** Phase 1 ✅ delivered (87 files, 0 placeholders). Phase 2 roadmap-only (slices 07–10), pending the whole-book plan. |

Arc 2 is deliberately **not** planned in detail yet (*plan late, plan deep*):
its `arc-plan.md` is written when arc01 closes, so it can absorb anything
arc01 reveals about the split-from-workbench workflow.

## 3. Current status

- **arc01 (Binaries) CLOSED** — Bits/Bytes/Binaries chapter, 153 files, 0
  placeholders (`arc01-binaries-chapter/closing-report.md`).
- **arc02 (Part II data structures) ACTIVE — re-opened & extended (2026-06-28).**
  - *Phase 1 ✅ delivered:* six drafted chapters published as multi-leaf (87
    files, 0 placeholders, 81 new SUMMARY entries). Operator-built OK; everything
    rendered "great".
  - *Phase 2 (new, pending):* author & publish the four draft-less chapters —
    **Graphs, Queues, Pattern Matching, Generic Sequence Functions** (slices
    07–10, roadmap-only). **Blocked on input:** the recent whole-book plan from
    the Nov-2025 Claude Desktop archive, which should drive each chapter's scope
    and structure. It is **not** in the project (searched exhaustively).
- The earlier "project DoD substantively met" note (v1.2) is **superseded**: the
  DoD now includes Phase 2 authoring. The publishing of all *drafted* material is
  complete; authoring the 4 new chapters is the remaining work.

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
| P3 | The six drafted Part II chapters each have real content (no placeholder README). | *reproduced* — read each `src/part2/<chapter>/README.md`. | **CLOSED (arc02)** — whole-arc sweep: 87 files, 0 placeholders across all six chapters |
| P4 | All in-scope `SUMMARY.md` ToC entries resolve to non-placeholder files; mdBook builds without broken links. | *reproduced* — `mdbook build` (or link check) on the affected chapters. | **byte-bin + arc02 slices 01–05 operator-built OK.** One final `make run` needed to validate the Records (slice06) ToC edit, then fully closed. |
| P5 | Byte-bin cleanups (P-row scope: `what/README`, stray `fundform.md`) resolved. | *attested* — confirmed in arc01 close. | **CLOSED (arc01)** — `what/README` filled; `fundform.md` absent |
| P6 | The four authored Part II chapters (Graphs, Queues, Pattern Matching, Generic Sequence Functions) are written and published as multi-leaf chapters with ToC entries, no placeholders. | *reproduced* — whole-arc sweep at Phase-2 close. | **open — arc02 Phase 2** (pending whole-book plan) |

## 5. Version History

### v1.3 — 2026-06-28
**arc02 re-opened & extended; project scope expanded** (operator request). Added
arc02 Phase 2: author & publish four draft-less Part II chapters — Graphs,
Queues, Pattern Matching, Generic Sequence Functions (slices 07–10). Moved those
four from "out of scope" to in-scope in §1; added project-ledger row **P6**;
reverted the v1.2 "DoD substantively met" claim (DoD now includes Phase-2
authoring). Phase 1 (the six drafted chapters) remains delivered. **Phase 2 is
blocked pending the recent whole-book plan** from the Nov-2025 Claude Desktop
archive — confirmed absent from the project after an exhaustive search; the
operator is retrieving it. Which arc surfaced this: operator directive on arc02.

### v1.2 — 2026-06-28
**arc02 closed; project DoD substantively met.** Bubble-up from
`arc02-part2-data-structures/closing-report.md`: the six drafted Part II chapters
published as multi-leaf chapters (87 files, 0 placeholders, 81 new SUMMARY
entries), so no roadmap change forced. Closed project-ledger row P3; P4 needs
only a final operator build of the Records ToC edit. With arc01 + arc02 closed,
every chapter with a finished workbench draft is now published — the publishing
project is complete pending that build. Out-of-scope items (draft-less Part II
chapters, AI-resources pocket reference, Typed LFE) remain available as a future
authoring project, using the hardened §6 / §A2 methodology. Which arc surfaced
this: arc02.

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
