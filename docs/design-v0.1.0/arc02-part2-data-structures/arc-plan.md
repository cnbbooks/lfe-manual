# Arc 02 — Part II Data Structures

> Plan-of-record (slice breakdown). Parent: `../project-plan.md`. Layout per
> `collaboration-framework/docs/PROJECT-MANAGEMENT.md` v2.1. Reuses arc01's §6
> publishing conventions, with the arc02 addendum below.

## 1. Capability statement

> **Status: RE-OPENED & EXTENDED 2026-06-28** (operator request). The arc was
> closed after Phase 1; it is re-opened with a Phase 2 (authoring) added. The
> Phase 1 close record is preserved in `closing-report.md` (see its re-open
> note). A fresh arc-close will be written when Phase 2 lands.

**Phase 1 — publish the drafted chapters (DELIVERED).** The six *drafted* Part
II data-structure chapters — **Tuples, Property Lists, Maps, Arrays, Dicts,
Records** — are published from their `workbench/<chapter>/new-section-*.md`
drafts as **multi-leaf chapters**, with their sub-sections added to
`src/SUMMARY.md`. No placeholders remain. (Composition check: 87 files, 0
placeholders — `closing-report.md`.)

**Phase 2 — author & publish the draft-less chapters (NEW, pending).** The four
remaining Part II chapters that have **no workbench draft** — **Graphs, Queues,
Pattern Matching, Generic Sequence Functions** — are written from source/reference
material and published as multi-leaf chapters with their ToC entries. This is
*authoring*, not split-and-publish: there is no draft to split, so each slice's
open set must carry a content outline. **Detailed planning is deliberately
deferred until the operator supplies the recent whole-book plan** (sought from
the Nov-2025 Claude Desktop archive) — that plan should drive each chapter's
scope, depth, and structure.

Operator decisions: **multi-leaf** structure; **one slice per chapter**.

## 2. Slice breakdown

### Phase 1 — publishing (6 slices, all closed)

| Slice | Chapter | Source draft | Leaves |
|-------|---------|--------------|--------|
| **slice01 — tuples** ✅ CDC-closed 2026-06-28 | Tuples | `workbench/tuples/new-section-tuples.md` (~244 ln) | README + 12 |
| **slice02 — proplists** ✅ CDC-closed 2026-06-28 | Property Lists | `workbench/proplists/new-section-proplists.md` (~641 ln) | README + 21 |
| **slice03 — maps** ✅ CDC-closed 2026-06-28 | Maps | `workbench/maps/new-section-maps.md` (~383 ln) | README + 15 |
| **slice04 — arrays** ✅ CDC-closed 2026-06-28 | Arrays | `workbench/arrays/new-section-arrays.md` (~578 ln) | README + 13 |
| **slice05 — dicts** ✅ CDC-closed 2026-06-28 | Dicts | `workbench/dicts/new-section-dicts.md` (~1168 ln) | README + 5 (omnibus) |
| **slice06 — records** ✅ CDC-closed 2026-06-28 | Records | `workbench/records/new-section-records.md` (~372 ln) | README + 15 |

### Phase 2 — authoring (4 slices, drawn up 2026-06-28)

These are **authoring** slices: there is no workbench draft to split, so each
slice *writes* a new chapter from source/reference material, in the **Cosmic
Techno-Wit** voice (`docs/writers-guide/cosmic-techno-wit-style-guide.md`). Each
is published multi-leaf with its ToC entries, exactly like Phase 1, but the
content is composed rather than transcribed.

| Slice | Chapter | ToC target | Primary source | Leaves | Status |
|-------|---------|-----------|----------------|--------|--------|
| **slice07 — graphs** | Graphs | `part2/graphs/README.md` | `workbench/graphs/*.pdf` (LYSE; `digraph`, `digraph_utils`) | README + 9 | ✅ CDC-closed 2026-06-28 |
| **slice08 — queues** | Queues | `part2/queues/README.md` | `workbench/queues/*.pdf` (LYSE; `queue`) | README + ~9 | ⏭️ next — open set written |
| **slice09 — pattern-matching** | Pattern Matching | `part2/patterns/README.md` | `workbench/lfe_guide.7.md` §Patterns/§Guards + cross-refs | README + ~11 | 🗓️ outlined |
| **slice10 — gen-seq** | Generic Sequence Functions | `part2/gen-seq/README.md` | Erlang `lists` module + `lfe_guide.7.md` §Predefined functions | README + ~12 | 🗓️ outlined |

Slices are independent; listed in ToC order. Plan-late still applies: the **next**
slice gets a full open set (slice-doc + ledger + cc-prompt); the outlines below
firm up when each becomes next. Out of Part II's draft-less set, **Characters &
Strings** and **Manipulating List Structure** remain in the Saga backlog (not in
the operator's 4-chapter request).

#### 2.1 Phase-2 chapter outlines (the leaf breakdowns)

**slice07 — Graphs** (`digraph` / `digraph_utils`; the data structure that
*mutates*). README (what graphs are; the ETS-backed, mutable, reference-typed
nature — the standout caveat) · `create` (`digraph:new/0,1`; cyclic/acyclic,
protected/private/public) · `vertices` (add/del/vertex/vertices/no_vertices;
vertex labels) · `edges` (add_edge/edge/edges/del_edge; the `{V1,V2}` model;
edge labels) · `paths` (get_path, get_cycle, get_short_path, get_short_cycle) ·
`utils` (`digraph_utils`: topsort, is_acyclic, reachable/reaching, components,
strong_components, cyclic_strong_components, arborescence) · `mutability` (the
big caveat: shared mutable ETS state, ownership, `digraph:delete/1`, not
immutable, not auto-GC'd) · `example` (worked example — e.g. dependency/build
order via topsort) · `vs` (when to reach for `digraph` vs hand-rolled maps) ·
`summ`.

**slice08 — Queues** (`queue`; the double-ended FIFO that is secretly two lists).
README (what a queue is; the two-lists-back-to-back insight) · `create`
(new/is_queue/is_empty/len) · `inout` (in/2, out/1, in_r/2, out_r/1 — the FIFO
and the reverse end) · `amortized` (how two lists yield amortised O(1) — the
"stroke of genius") · `ops` (reverse, join, split, filter, member) · `convert`
(to_list, from_list, ordering) · `apis` (the Original / Extended / Okasaki APIs —
cons/head/tail/snoc/daeh/liat) · `example` (worked example — e.g. a job queue or
BFS frontier) · `vs` (queue vs plain list vs other; when FIFO matters) · `summ`.

**slice09 — Pattern Matching** (the unifying treatment; you've met it in every
chapter, here's the whole picture). README (pattern matching as the heart of LFE)
· `what` (a pattern is a data expression: symbols are variables, `quote` matches
literals; match vs assignment) · `where` (let, function clauses, `case`,
`receive`, `lambda`/`match-lambda`, comprehensions, `try`) · `literals-vars`
(literals via quote; binding; the `_` don't-care that never binds) · `nonlinear`
(repeated variables + automatic equality) · `aliases` (`(= pattern1 pattern2)` —
bind the whole and the parts) · `by-type` (tuples, cons/lists, binaries/bitstrings,
maps, records, structs — cross-ref the dedicated chapters) · `guards` (`(when …)`,
guard BIFs/type-tests, the empty guard, what's allowed) · `functions` (multi-clause
`defun` / `match-lambda`; clause order; fall-through) · `failure` (badmatch /
function_clause; let-it-crash as a feature) · `idioms` (ok/error tuples,
destructuring returns, head|tail recursion) · `summ`.

**slice10 — Generic Sequence Functions** (the higher-order toolkit over
sequences — distinct from the Lists & Strings chapter, which is list/string
*basics*; this is the algorithmic library). README ("not writing the same loop
twice") · `map` (map; flatmap; mapfoldl) · `filter` (filter, partition,
takewhile, dropwhile, splitwith) · `fold` (foldl, foldr — the universal
recursion) · `search` (all, any, member, find, foreach) · `build` (seq,
duplicate) · `combine` (zip/zip3/zipwith/unzip; append/concat) · `reorder`
(sort, usort, reverse, sublist, nth/nthtail) · `aggregate` (sum, max, min) ·
`comprehensions` (list comprehensions as the declarative alternative; cross-ref
`byte-bin/comps` for binary comps) · `composition` (pipelines, `->`/`->>`, HOF
style) · `summ`.

#### 2.2 Authoring-slice workflow (§A2.6)

For each Phase-2 slice:

1. **Outline → ToC.** Confirm the leaf breakdown (above) against the source; add
   the `SUMMARY.md` sub-entries (§A2.1) and write the leaf files (write-probe to
   detect any pre-existing stubs, §A2.2).
2. **Author in voice.** Write each leaf from the source material in the Cosmic
   Techno-Wit voice, per the style guide's Prime Directive (technically correct
   and complete first; wit on top). All code is **idiomatic LFE** — convert every
   Erlang example from the source (`digraph`, `queue`, `lists`, LYSE) to LFE
   syntax.
3. **Dual verification at close** (this replaces Phase 1's "preserve verbatim"
   reconciliation — there is no draft to diff against):
   - **Technical accuracy** — every claim and every code example checked against
     the stdlib docs / man pages; LFE code is valid and idiomatic. Use the
     `erlang-guidelines` / LFE knowledge skills and an independent CDC pass.
   - **Voice conformance** — checked against the style guide's ship-it checklist
     (§12): explanation-before-joke, precise similes, no punching at the reader,
     code carries the voice, motifs rationed.
   - **Structure** — no placeholders; ToC entries match leaf titles and resolve;
     mdBook build (operator).

Note: `workbench/proplists/lists.md` is **not** in scope — it is the source for
the already-published Lists chapter (`src/part2/lists/`), a reconcile-check
only, not new work.

## 3. Dependencies

- **Consumes:** the six `new-section-*.md` drafts (Phase 1) and the Phase-2
  source material (`workbench/graphs|queues/*.pdf`, `lfe_guide.7.md`, Erlang
  `lists`); and `src/SUMMARY.md` (target ToC, which this arc *edits*).
- **Scope clarification (2026-06-28, superseding the original "out of scope"
  note).** Four of the draft-less Part II chapters — **Graphs, Queues, Pattern
  Matching, Generic Sequence Functions** — are **in this arc** as Phase 2
  (slices 07–10), per the operator's re-open directive (v1.7). The remaining
  two — **Characters & Strings, Manipulating List Structure** — are **not** in
  this arc; they live in the Saga backlog (`../../book-roadmap.md` §6) for a
  future Part II push. *(was: "the six draft-less Part II chapters … remain out
  of project scope" — true only before the v1.7 re-open.)*

## 4. §6 addendum for arc02 (authoring ToC + files)

arc01 *filled pre-existing placeholder leaves*; arc02 *creates* the leaves and
their ToC entries. Added conventions:

- **§A2.1 — Author the ToC sub-entries.** Each chapter is currently a single
  `SUMMARY.md` line (`  * [Tuples](part2/<ch>/README.md)`). Add the `##`-section
  leaves beneath it at one deeper indent (`    * [Heading](part2/<ch>/slug.md)`),
  matching the byte-bin indentation style. This is the one sanctioned
  `SUMMARY.md` edit (arc01 forbade SUMMARY edits because leaves pre-existed).
- **§A2.2 — Slug design / pre-scaffolded stubs.** **Check the chapter dir first**
  (`ls src/part2/<ch>/`): a prior session already scaffolded the leaf files as
  lone-heading stubs with canonical slugs and titles (confirmed for Tuples —
  all 12 existed). Adopt the existing slugs/titles rather than inventing (avoids
  drift); the files must be `Read` before `Write`. Only if a section has no
  pre-existing stub, derive a short kebab-case slug from its heading. README
  stays `README.md`. (The stubs are not yet in `SUMMARY.md` — §A2.1 still
  applies.) **Detection method:** use the Read/Write file tools (write-probe: a
  Write that errors "not read yet" means the stub exists at that slug — adopt and
  fill). Do **not** rely on the bash `ls`: the shell sandbox mount of this repo
  is stale and has reported chapter dirs as README-only when the leaf stubs
  actually exist (observed for `maps/` in slice03). **Use the Write-probe ONLY.**
  In slice04 a **Read-probe** of 13 arrays slugs + 5 alternates *all* returned
  "file does not exist" — yet the Write-probe then proved every stub exists. So
  bash `ls`, glob, AND Read-probe all produce false negatives for never-accessed
  paths in this repo; the Write guard ("not read yet" ⇒ file exists) is the only
  reliable signal. Tuples, Proplists, Maps, and Arrays were ALL pre-scaffolded
  with slugs matching natural heading-derivation (zero orphans across 4 chapters).
- **§A2.3 — Title consistency.** Use the draft's `##` heading text as **both**
  the leaf `#` title and the SUMMARY link text, so page title == ToC entry.
- **§A2.4 — README landing page.** The chapter README = the draft's `#` H1
  (title-page flourish ok) + the draft's intro section (the "In Which…"
  opening), a short landing page — not the whole chapter.
- Otherwise apply arc01 §6 as-is: `lisp`→`lfe` fences; draft `###`→`##` inside
  leaves; preserve prose/code verbatim; keep closing transition paragraphs;
  no hand-rolled "Next:" footers.

## 5. Arc ledger (verifies the capability composes)

| # | Criterion | How verified (target strength) |
|---|-----------|-------------------------------|
| A1 | Each of the six chapters has a real README + leaf files; no lone-heading placeholders. | *reproduced* — scan each `src/part2/<ch>/`. |
| A2 | Every `##` section of each draft maps to exactly one published leaf; none dropped/duplicated. | *reconciled* — per-slice heading diff. |
| A3 | `SUMMARY.md` lists every new leaf at correct indentation; ToC entry text == leaf title; links resolve. | *reproduced* — read SUMMARY diff + spot links. |
| A4 | Chapters read coherently in ToC order. | *attested* — per-slice read. |
| A5 | mdBook builds with no broken ToC links (operator build at arc close). | *reproduced* — operator `make run`. |

## 6. Version History

### v1.8 — 2026-06-28
**Phase 2 drawn up.** Fleshed §2 with the four authoring slices' chapter
outlines (§2.1) and the authoring-slice workflow + dual verification — voice
(Cosmic Techno-Wit style guide) **and** technical accuracy (stdlib docs / man
pages; idiomatic LFE) — as §A2.6 (§2.2), since authoring has no draft to diff
against. Sources grounded: `digraph`/`digraph_utils` (graphs), `queue` (queues),
`lfe_guide.7.md` §Patterns/§Guards (pattern matching), Erlang `lists` +
`lfe_guide.7.md` §Predefined functions (gen-seq); confirmed `lfe_gen` is the code
generator, *not* sequence functions, and that gen-seq must not overlap the
existing Lists & Strings chapter. slice07 (Graphs) open set written; 08–10
outlined (plan-late). Decision to proceed on today's roadmap + style guide (the
Nov-2025 plan was not recoverable).

### v1.7 — 2026-06-28
**Arc RE-OPENED and EXTENDED** at operator request, after the Phase-1 close.
Added Phase 2: author & publish the four draft-less Part II chapters (Graphs,
Queues, Pattern Matching, Generic Sequence Functions) as slices 07–10
(roadmap-only). Capability statement (§1) split into Phase 1 (delivered) and
Phase 2 (pending). **Phase-2 detailed planning deferred until the operator
supplies the recent whole-book plan** (Nov-2025 Desktop archive) — not found
anywhere in the project (searched `workbench/`, `workbench/old/`, loose files,
`src/`, `docs/`). These are *authoring* slices (no draft to split). Phase 1's
`closing-report.md` is preserved with a re-open note; a new arc-close covers
Phase 2 when it lands. Which child surfaced this: operator directive.

### v1.6 — 2026-06-28
slice06 (records) CDC-closed — README + 15 leaves + 15 ToC entries, CDC PASS
(0 bare `>` prompts, 0 stray `lisp` fences). **All six slices closed; arc ready
for formal close.** Corrected the stub-detection guidance: `git ls-files` joins
bash `ls`/glob/Read-probe on the false-negative list — it reported records/ as
README-only, but the Write-probe proved all 15 stubs exist. **The Write-probe is
the sole reliable stub-detector.** Five chapters were pre-scaffolded (tuples,
proplists, maps, arrays, records) at natural-derivation slugs; only dicts (the
omnibus) was genuinely un-scaffolded. Zero orphans arc-wide.

### v1.5 — 2026-06-28
slice05 (dicts) CDC-closed — README + 5 sub-chapter leaves + 5 ToC entries, CDC
PASS (all 18 `orddict:*` headings, gb_trees smart/naive API, 0 stray fences).
Added §A2.5: omnibus drafts (multiple `#` sub-chapters) use one-leaf-per-`#`
grain with internal `##`/`###` preserved. Dicts was the first genuinely
un-scaffolded chapter (git ls-files README-only; all 5 leaves created new). No
breakdown/sequencing change. Next and last: slice06 (records).

### v1.4 — 2026-06-28
slice04 (arrays) CDC-closed — README + 13 leaves + 13 ToC entries, CDC PASS
(incl. 19 preserved bold sub-labels, 0 stray `lisp` fences). Refined §A2.2: the
**Write-probe is the only reliable stub-detector** — in slice04 a Read-probe of
all 13 slugs (+5 alternates) falsely reported "does not exist," but the stubs
existed. Arrays was pre-scaffolded like the other three; all slugs matched, zero
orphans. No breakdown/sequencing change. Next: slice05 (dicts — the largest).

### v1.3 — 2026-06-28
slice03 (maps) CDC-closed — README + 15 leaves + 15 ToC entries, CDC PASS
(incl. the preserved `erlang` comparison fence). Refined §A2.2: the bash `ls`
is a stale view of this repo and falsely reported maps/arrays/dicts/records as
README-only; use the Read/Write write-probe to detect pre-scaffolded stubs. Maps
was in fact pre-scaffolded with matching slugs. No breakdown/sequencing change.
Next: slice04 (arrays).

### v1.2 — 2026-06-28
slice02 (proplists) CDC-closed — README + 21 leaves + 21 ToC entries, CDC PASS.
§A2.2 confirmed across a second chapter: the leaf stubs pre-existed with the
exact planned slugs (zero orphans), so the write-probe reliably substitutes for
the broken glob. No breakdown/sequencing change. Next: slice03 (maps).

### v1.1 — 2026-06-28
slice01 (tuples) CDC-closed — README + 12 leaves + 12 ToC entries, CDC PASS.
Refined §A2.2: the leaf stubs were already scaffolded on disk (prior session)
with the exact planned slugs/titles, so slices 02–06 should check the chapter
dir first and adopt existing stubs rather than invent slugs. No change to the
slice breakdown or sequencing. Surfaced by slice01's bubble-up.

### v1.0 — 2026-06-28
Initial breakdown: 6 slices, one per chapter (operator's choice), multi-leaf
structure (operator's choice). Added the §6 arc02 addendum (§A2.1–A2.4) for
authoring ToC entries, slugs, and landing pages — the new work vs arc01.
slice01 (tuples) planned in detail; 02–06 roadmap-only (plan late).
