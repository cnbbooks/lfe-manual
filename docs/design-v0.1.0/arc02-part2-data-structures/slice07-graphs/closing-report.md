# Closing Report — Slice 07 (Graphs, authoring)

Written by CC at slice close, 2026-06-29.

## Per-row walk

All 15 ledger rows reached a final status: **15 done, 0 deferred, 0 silent
drops.** Open-count (15) == close-count (15). This is the arc's first *authoring*
slice — no draft to split, the chapter was written from the stdlib reference.

- **Rows 1–10 (files):** done. README (grand H1 + Adams epigraph + "In Which…"
  landing) + 9 leaves (create, vertices, edges, paths, utils, mutability,
  example, vs, summ), all in Cosmic Techno-Wit voice, all code idiomatic LFE.
- **Row 11 (ToC):** done. Parent `[Graphs]` short label retained (sibling-
  consistent); 9 nested entries added at 4-space indent; link text == leaf `#`
  titles verbatim; links resolve.
- **Rows 12–14 (dual gate):** done. Technical accuracy *reconciled* and
  empirically reproduced (see below); idiomatic LFE *reproduced*; voice
  *attested* against the §12 checklist. Independent CDC pass: PASS.
- **Row 15 (mdBook build):** done. `make build` → 0 `WARN`, exit 0.

### Disclosed deviations

None from the authoring conventions. Two CDC nits applied post-draft: thinned
the "3 a.m." motif from ~6 to 3 load-bearing uses; added an `edges.md` sentence
on the edge-id-reuse error corner.

## Bubble-up to the arc

**1. Assigned piece delivered?** Yes — the Graphs chapter is published
(README + 9 leaves) with ToC, dual-gate verified, build clean. Note this chapter
was listed in arc-plan §3 as a *draft-less, "leaves for later"* chapter, i.e.
beyond the original six new-section drafts; slice07 brings it into arc02. **The
arc owner should confirm whether the remaining draft-less Part II chapters
(chr-str, queues, patterns, gen-seq, mani-list) are now in arc02's scope or
belong to a successor arc.**

**2. What did this slice reveal?**

- **The runtime is a verification oracle, even without LFE.** Erlang is
  installed; `digraph`/`digraph_utils` (and, for future slices, `queue`,
  `lists`, `sets`, …) are the *same* stdlib modules LFE calls. Translating each
  worked example back to Erlang and running it under `erl -noshell -eval` caught
  **five** real output errors in the first draft (a `topsort` permutation, a
  `reaching` order, a `components` order, an invented `info` memory figure, and
  a reversed `bad_edge` path) that prose review alone would have shipped. **This
  should be standard practice for every remaining authoring slice:** author →
  Erlang-verify every deterministic output → then voice-check. It is cheap,
  fast, and decisive.
- **PDF sources need a text-extraction step.** The `pages` Read of the stdlib
  PDFs failed (no poppler); `pymupdf` extracted clean text in one pass. Worth
  pinning for the queue/patterns slices, whose sources are also PDFs.
- **Stub state:** `src/part2/graphs/` held only a 9-byte `README.md` stub; the 9
  leaves were genuine clean creates (no pre-scaffolded stubs, unlike the six
  drafted chapters). Consistent with this being a draft-less chapter.

**3. Silent-drop diff.** Specified: Graphs README + 9 leaves + 9 ToC entries +
dual verification. Delivered: all 10 files + 9 ToC entries + dual gate + CDC.
No undisclosed omissions.

## Arc-plan update decision

Recommend two arc-plan notes (for the owner to ratify): (a) add the
**Erlang-as-verification-oracle** practice to §A2.6's dual-verification step —
empirically run every deterministic example against the live stdlib module
before voice review; (b) record the **scope question** above (draft-less
chapters now entering arc02). Left for the arc owner rather than self-applied,
since (b) changes arc boundaries.
