# Arc 01 Closing Report — Binaries Chapter

Arc-level close, 2026-06-28. Written when the last slice (slice04) was
CDC-closed. This is the composition check the arc scale exists to provide, plus
the bubble-up to the project.

## 1. Capability, restated — and the verdict

**Capability (from `arc-plan.md`):** the Bits, Bytes & Binaries chapter
(`src/part2/byte-bin/`) is fully published from the `workbench/bytes/` drafts —
every ToC leaf carries real prose and code, the chapter reads coherently from
intro to conclusion in ToC order, and no placeholder files remain.

**Verdict: DELIVERED.**

## 2. Slice walk (matches the `arc-plan.md` breakdown — 4 of 4)

| Slice | Sections | Outcome |
|-------|----------|---------|
| slice01 — bit-syntax-specifiers | `bifs/`-tail, `bits/`, `types/`, `sizes/` + cleanups | delivered, CDC-closed |
| slice02 — matching-comprehensions | `ends/`, `patts/`, `comps/` | delivered, CDC-closed |
| slice03 — bitstrings-operators | `bitstrs/`, `ops/` | delivered, CDC-closed |
| slice04 — serialization-realworld | `ser/`, `ser/realwrld/`, `realwrld/` | delivered, CDC-closed |

Slice count here (4) matches the breakdown in `arc-plan.md` §2 — no arc-scale
silent drop. (Sections published before this arc resumed — chapter `README`,
`what/`, `syntax/`, `concl/` — are accounted for in the composition check below.)

## 3. Composition check (arc ledger A1–A5)

Reproduced at arc scale via an independent whole-chapter sweep (a fresh CDC
subagent read every file in `src/part2/byte-bin/`):

| Row | Criterion | Result |
|-----|-----------|--------|
| A1 | Every ToC leaf under `byte-bin/` has real content. | **PASS (reproduced)** — sweep of all 16 subdirectories: 153 `.md` files, **0 lone-heading placeholders**. |
| A2 | Each leaf traces to its `workbench/bytes/` source subsection; none dropped. | **PASS (reconciled)** — per-slice CDC heading-by-heading diffs across drafts 00–14; all `##`/promoted-`###` sections mapped exactly once. |
| A3 | Chapter reads coherently in ToC order (intro → … → conclusion). | **PASS (attested)** — section order matches `SUMMARY.md`; each section's README→leaves flow preserved; transition paragraphs (e.g. `ser/summ` → real-world) intact. |
| A4 | Cleanups resolved: `what/README` filled; stray `syntax/fundform.md` removed/reconciled. | **PASS** — `what/README` filled (slice01); `fundform.md` confirmed absent (direct read: "file does not exist"). |
| A5 | mdBook builds with no broken ToC links. | **DEFERRED (not reproduced)** — sandbox cannot mount the repo, so `mdbook build` could not be run in-session. Mitigation: every published leaf pre-existed as a `SUMMARY.md`-listed placeholder, so the link graph is unchanged from before the arc — no new broken links can have been introduced. **Recommended operator action: `make run` / `mdbook build` on a local checkout to convert A5 to reproduced.** |

The composition holds: the four slices' outputs recombine into the promised
capability. The one open row (A5) is a verification not yet *run*, not a gap in
the work.

## 4. Accumulated arc-plan change log

`arc-plan.md` grew v1.0 → v1.4 across the arc, all additive:

- v1.1 (slice01): pinned the publishing conventions (§6).
- v1.2 (slice02): §6.8 (ToC finer than draft — `###`→leaf promotions) and §6.9
  (strip hand-rolled "Next:" footers).
- v1.3 (slice03): §6.8 clarified — promotions can be asymmetric.
- v1.4 (slice04): all slices closed; chapter at 0 placeholders.

No slice forced a change to the slice breakdown or sequencing — the original
4-batch plan held end to end.

## 5. Bubble-up to the project

**1. Did this arc deliver its capability as `project-plan.md` defined it?**
Yes. The project roadmap's arc01 capability ("the Binaries chapter is fully
published … no placeholders") is met. Project-ledger rows **P1** (no byte-bin
placeholders) and **P2** (leaves trace to workbench sources) are satisfied for
this chapter and can close. **P4** (mdBook build / link check) remains pending
the same deferred build (project-scope, not arc-specific). **P5** (byte-bin
cleanups) satisfied.

**2. What did this arc reveal that the project plan did not anticipate?**
- The split-from-workbench workflow is **reusable as-is for arc02** (the Part II
  data-structure chapters). The §6 conventions — including the `###`→leaf
  promotion rule — are draft-agnostic and should carry over directly. Arc02's
  detailed plan (still deliberately deferred — *plan late*) can adopt §6 wholesale.
- The recurring tooling constraint (sandbox can't mount the repo → no in-session
  `mdbook build`) is a **project-level** fact, not arc-specific. Recommend the
  project treat "operator runs the build" as the standing close step for every
  documentation arc, rather than expecting in-session reproduction.

**3. Silent-drop diff at arc scale (rolled up to project).** The roadmap
expected the whole Binaries chapter from arc01; the whole chapter was delivered.
The only item not landed in-session is the mdBook build verification (A5/P4),
disclosed and deferred to the operator. No undisclosed gaps.

## 6. Project-plan update decision

The arc delivered its capability as the roadmap defined it, so **no project-plan
change is forced** — itself a valid, recorded bubble-up outcome. Project-plan
updated only to mark arc01 closed, close project-ledger rows P1/P2/P5, and note
P4 pending the operator build. Recorded in `project-plan.md` v1.1.

## 7. Independent check

The composition check (A1–A4) was reproduced by a fresh CDC context with no
access to the implementer's working state — the independence an arc close
requires. A5 is the sole item awaiting the operator's local build.

**Arc 01 is closed.**
