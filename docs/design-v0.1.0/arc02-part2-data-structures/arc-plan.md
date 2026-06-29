# Arc 02 — Part II Data Structures

> Plan-of-record (slice breakdown). Parent: `../project-plan.md`. Layout per
> `collaboration-framework/docs/PROJECT-MANAGEMENT.md` v2.1. Reuses arc01's §6
> publishing conventions, with the arc02 addendum below.

## 1. Capability statement

The six drafted Part II data-structure chapters — **Tuples, Property Lists,
Maps, Arrays, Dicts, Records** — are published from their
`workbench/<chapter>/new-section-*.md` drafts as **multi-leaf chapters** (a
short README landing page plus per-`##`-section leaf files), consistent with the
finished Part II chapters (data-types, lists, cons-cells, vars). Each chapter's
sub-sections are added to `src/SUMMARY.md`. No placeholders remain in these six
chapters.

Operator decisions (2026-06-28): **multi-leaf** structure (expand the ToC);
**one slice per chapter** (6 slices).

## 2. Slice breakdown (6 slices, one per chapter)

| Slice | Chapter | Source draft | Approx leaves |
|-------|---------|--------------|---------------|
| **slice01 — tuples** ✅ CDC-closed 2026-06-28 | Tuples | `workbench/tuples/new-section-tuples.md` (~244 ln) | README + 12 |
| **slice02 — proplists** ✅ CDC-closed 2026-06-28 | Property Lists | `workbench/proplists/new-section-proplists.md` (~641 ln) | README + 21 |
| **slice03 — maps** ✅ CDC-closed 2026-06-28 | Maps | `workbench/maps/new-section-maps.md` (~383 ln) | README + 15 |
| **slice04 — arrays** ✅ CDC-closed 2026-06-28 | Arrays | `workbench/arrays/new-section-arrays.md` (~578 ln) | README + 13 |
| **slice05 — dicts** ✅ CDC-closed 2026-06-28 | Dicts | `workbench/dicts/new-section-dicts.md` (~1168 ln) | README + 5 (omnibus: one leaf per `#` sub-chapter) |
| **slice06 — records** ✅ CDC-closed 2026-06-28 | Records | `workbench/records/new-section-records.md` (~372 ln) | README + 15 |

Leaf counts are estimates; each slice's exact breakdown is derived from its
draft's heading structure when the slice is planned (*plan late*). Slices are
independent (chapters don't depend on each other); listed in ToC order.

Note: `workbench/proplists/lists.md` is **not** in scope — it is the source for
the already-published Lists chapter (`src/part2/lists/`), a reconcile-check
only, not new work.

## 3. Dependencies

- **Consumes:** the six `new-section-*.md` drafts (content) and `src/SUMMARY.md`
  (target ToC, which this arc *edits* to add sub-entries).
- **Leaves for later:** the six draft-less Part II chapters (chr-str, graphs,
  queues, patterns, gen-seq, mani-list) remain out of project scope.

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
