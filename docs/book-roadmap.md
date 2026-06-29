# The Chineual — Whole-Book Roadmap (the Saga)

> The living, book-level plan for the **LFE Machine Manual ("Chineual")**. This
> sits one level above the per-Project plans in `docs/<project>/design-vX.Y.Z/…`
> and instantiates the **Saga** slot named (but left open) in
> `collaboration-framework/docs/PROJECT-MANAGEMENT.md`: *a multi-version vision
> spanning several Projects.*
>
> A Saga is not a Project with a finish line. The Chineual is a **living book** —
> like *The Guide* itself, comprehensive enough that committing it to a single
> static edition would risk localized gravitational collapse. So this document is
> deliberately built to be *rigorous about what's next and loose about what's
> far*, and to keep working as the book grows in directions we can't yet see.

---

## 1. The vision

The Chineual is the definitive, joyful, technically exact manual for Lisp
Flavoured Erlang — written end to end in the **Cosmic Techno-Wit** voice
(`docs/writers-guide/cosmic-techno-wit-style-guide.md`), built with mdBook, and
structured as the nine Parts listed in `src/SUMMARY.md`. It serves the learner
reading front-to-back, the working programmer reaching for a reference, and the
contributor diving into internals — without ever sacrificing accuracy for wit or
wit for accuracy.

It is never "done." New language features, libraries, idioms, and whole new
Parts will arrive. The job of this roadmap is to make steady, checkable progress
toward a *more complete* book while staying graceful about an expanding universe.

---

## 2. The scales of work (how this fits the framework)

```
Saga  ──  THE CHINEUAL (this document: vision + project roadmap + backlog)
  └─ Project  ──  one per Part (or coherent sub-Part); own docs/<project>/design-vX.Y.Z/
       └─ Arc  ──  a coherent group of chapters/slices within a Project
            └─ Slice  ──  one chapter (or section cluster) — a mergeable unit
                 └─ Step ── a leaf file / ledger row
```

- **This file** owns the vision, the **Project roadmap** (§5), and the **backlog**
  (§6). It does *not* hold arc/slice detail — that lives in each Project's design
  tree, written *when that Project is started* (plan late, plan deep).
- **`src/SUMMARY.md` is the structural source of truth** for what the book
  *contains*. This roadmap references it; it does not duplicate it. When the book
  grows, SUMMARY changes and the roadmap re-points — the two are decoupled on
  purpose.
- Each Project is decomposed and closed with the full
  `collaboration-framework` discipline (arc-plan, slices, ledgers, closing
  reports, bubble-up). The proven publishing/authoring conventions live in
  `docs/design-v0.1.0/arc01-binaries-chapter/arc-plan.md` §6 and the arc02 §A2
  addendum, and apply to every future Project.

---

## 3. Living-book principles (the flexibility, made explicit)

1. **Plan late, plan deep — at Saga scale.** Only the **active** and **next**
   Projects get a detailed design tree. Far-future Parts are one-liners in §5
   until they're near. A roadmap is cheap to keep current; ten detailed
   project-plans written in advance are ten documents that rot.
2. **The backlog never closes.** §6 is open-ended by design. "Finishing" the
   book is not the goal; *advancing* it is. New material appends to the backlog
   or slots into a Part; it never forces a re-architecture.
3. **Stable categories, fluid contents.** The nine Parts are the stable
   skeleton. Chapters come and go within them. New top-level Parts are rare and
   are added by amending §5 + `SUMMARY.md` together, with a version-history note.
4. **The plan is expected to be wrong.** Every Project will reveal things the
   roadmap couldn't know. That's why the bubble-up + plan-change discipline
   (§7) exists: drift is *tracked, never silent*. A roadmap whose Version
   History stops growing while the work continues is unmaintained, not stable.
5. **Decoupled versioning.** Each Project carries its own `design-vX.Y.Z`
   (a design-doc version, not a book release). Projects advance independently;
   the Saga just tracks their order and status.
6. **Reader-path spine, opportunistic interleaving.** The default sequence
   follows the book's reading order (§5). But partially-complete Parts are cheap
   to finish, so the operator may pull one forward at any time — the roadmap
   bends without breaking.

---

## 4. Snapshot — where the book stands (surveyed 2026-06-28)

Per-Part status, from this session's content survey (sampling-based for Parts
III–IX; to be firmed up when each Part becomes an active Project):

| Part | Title | State |
|------|-------|-------|
| **I** | Getting Started | ✅ **Complete** — intro, REPL, hello-worlds, guessing-game all written. |
| **II** | Data Types & Structures | 🟡 **Nearly complete** — primitive types, cons cells, lists, vars, **binaries** + **tuples/proplists/maps/arrays/dicts/records** all published. *In progress:* Graphs, Queues, Pattern Matching, Generic Sequence Functions (authoring). *Backlog:* Characters & Strings, Manipulating List Structure. |
| **III** | Data as Code | 🔴 **All stubs** — expressions, functions, closures, evaluation, flow of control, processes, messages, objects/flavors, I/O, files, modules, packages, scripting, projects. |
| **IV** | Advanced Topics | 🔴 **All stubs** — error/debug, eunit, common test, PropEr, compiler, macros, distribution, ports, servers, clients. *(also: 3 `REAEDME.md` filename typos to fix.)* |
| **V** | OTP | 🔴 **All stubs** — behaviours, applications, releases, data/Mnesia, project. |
| **VI** | Tooling | 🟡 **Mostly written** — the rebar3 internals reference (~110 files) is done; the `rebar3_lfe` quick-start + plugin pages are partial. |
| **VII** | Guides | 🟡 **Mostly written** — style guide + code-of-conduct done; AI-resources partial (syntax-pocket-reference, lfe-stdlb, erlang-stdlib are stubs). |
| **VIII** | LFE for Contributors | 🟡 **~⅓ written** — arch, sub-sys, mod-ref, lang-ref, erl-int intro done; ~9 subsections (compat-layers, tooling, data-struct-cat, comp-rel-grphs, test-qual, idioms, perf-consids, fut-dir, rsrs) are stubs. |
| **IX** | Conclusion | ✅ **Complete.** |

---

## 5. The Project roadmap

One Project per Part (operator's decomposition choice), sequenced along the
**reader's path** (operator's priority). Status legend: ✅ done · 🟢 active ·
⏭️ next · 🗓️ scheduled (plan late) · 💤 backlog.

| # | Project | Capability (definition of done) | Design tree | Status |
|---|---------|--------------------------------|-------------|--------|
| **P0** | Part I — Getting Started | The on-ramp chapters are written and coherent. | — (pre-dates this roadmap) | ✅ done |
| **P1** | **Part II — Data Types & Structures** | Every Part II chapter is published as a multi-leaf chapter in CTW voice, no placeholders. | `docs/design-v0.1.0/` | 🟢 **active** — arc01 (Binaries) ✅; arc02 Phase 1 (6 drafted chapters) ✅; arc02 Phase 2 (Graphs, Queues, Pattern Matching, Generic Sequence Functions) authoring. *Tail → backlog:* Characters & Strings, Manipulating List Structure. |
| **P2** | **Part III — Data as Code** | The core-language chapters (expressions, functions, modules, processes, messages, I/O, files, scripting, projects, …) authored from scratch in CTW voice. | `docs/part3-data-as-code/design-v0.1.0/` *(created when started)* | ⏭️ **next** |
| **P3** | Part IV — Advanced Topics | Error handling/debugging, testing (eunit/CT/PropEr), compiler, macros, distribution, ports, servers, clients. *(+ fix the `REAEDME.md` typos.)* | `docs/part4-advanced/…` | 🗓️ scheduled |
| **P4** | Part V — OTP | Behaviours, applications, releases, data/Mnesia, project structure. | `docs/part5-otp/…` | 🗓️ scheduled |
| **P5** | Part VI — Tooling (finish) | Complete the `rebar3_lfe` quick-start + plugin pages (internals reference already done). | `docs/part6-tooling/…` | 🗓️ scheduled · *cheap; pull forward at will* |
| **P6** | Part VII — Guides (finish) | Complete AI-resources (pocket reference + stdlib inventories) and any remaining style-guide leaves. | `docs/part7-guides/…` | 🗓️ scheduled · *overlaps the AI-resources backlog item* |
| **P7** | Part VIII — Contributors (finish) | Author the ~9 stub subsections (compat-layers, tooling, data-struct-cat, comp-rel-grphs, test-qual, idioms, perf-consids, fut-dir, rsrs). | `docs/part8-contributors/…` | 🗓️ scheduled |

**Sequencing.** The spine is reader order: **P1 → P2 → P3 → P4 → P5 → P6 → P7**.
Because P5/P6/P7 are partly written, any of them may be *pulled forward*
opportunistically (Principle §3.6) when a short, high-value push is wanted. Such
a change is recorded in §8, not treated as a re-architecture.

**Per-Project dir convention.** New Projects live under
`docs/<part-slug>/design-vX.Y.Z/…` (e.g. `docs/part3-data-as-code/design-v0.1.0/`).
P1 is grandfathered at `docs/design-v0.1.0/` (it pre-dates this convention; it may
optionally be relocated under `docs/part2-data-structures/` later, but no move is
forced). Each Project's design version starts at `v0.1.0` and bumps as *its*
design moves.

---

## 6. The backlog (open-ended — never closes)

Material that is in-scope for the book but not yet scheduled into a Project.
Pull from here when a Project finishes or when a short detour is worthwhile.

**Part II tail**
- Characters & Strings chapter.
- Manipulating List Structure chapter.

**Reference & companion material**
- **AI-resources pocket reference** — LFE syntax pocket reference + LFE/Erlang
  stdlib inventories (Part VII stubs). Seed: `workbench/lfe-pocket-reference-project-spec.md`.
- **Typed LFE chapter** — not yet in the ToC. Seed: `workbench/typed-lfe-chapter-bootstrap.md`;
  related repo: the `typed` project.

**Cleanups / debt**
- Fix Part IV `ports/`, `servers/`, `clients/` `REAEDME.md` → `README.md`.
- Part IX `README.md` self-labels "Part VIII" internally — correct the label.

**Source material already on hand** (lowers authoring cost when these get scheduled)
- `workbench/graphs/*.pdf`, `workbench/queues/*.pdf` (Learn You Some Erlang +
  Erlang stdlib `digraph`/`digraph_utils`/`queue` docs).
- `workbench/lfe_*.md` man pages (`lfe_guide`, `lfe_gen`, `lfe_lib`, `lfe_io`,
  `lfe_macro`, `lfe_types`, `lfe_comp`, `lfe_bits`, `lfe_cl`, `lfe_clj`,
  `lfe_docs`, `lfescript`, `lfe.1`) — reference for many Part III/IV chapters.
- `workbench/old/` — superseded earlier drafts (historical reference only).

**Ever-after**
- New language features, libraries, idioms; reader-requested chapters; whole new
  Parts. *Append here as they arise.* The mark of a healthy Saga is that this
  list keeps growing.

---

## 7. The "whole-book plan" input (pending)

A more recent **whole-book plan** exists in the operator's **Nov-2025 Claude
Desktop** archive (not in this repo — searched exhaustively). When retrieved, it
should be **reconciled against this roadmap and `src/SUMMARY.md`**: confirm the
Part/chapter structure, fold in any chapters this roadmap is missing, and adjust
ordering. Until then, this roadmap is the working plan and `SUMMARY.md` is the
structural truth. *(Drop the plan into the repo or paste it, and it gets folded
in with a §8 entry.)*

---

## 8. How this plan changes (Saga-level plan-change discipline)

Same discipline the framework applies to project/arc plans, applied here:

- **A Project closes →** mark it ✅ in §5, and roll up any book-level discovery
  (a new Part, a re-sequencing, a new backlog item) into §5/§6 with a dated §9
  entry naming *which Project* surfaced it.
- **New material arrives →** append to §6 (backlog) or insert into the right
  Part in §5; never silently restructure. Mark superseded text as superseded
  (strike or "was: …") rather than deleting it.
- **A Project is pulled forward / re-sequenced →** update §5's status column and
  log it in §9 with the reason.
- **The book gains a new Part →** amend §4/§5 and `src/SUMMARY.md` together; §9
  entry. This is the rare, deliberate "stable category changed" event.

Every change gets a dated §9 line: *what* changed, *which Project (or operator
directive)* surfaced it, and *why*.

---

## 9. Version history

### v1.0 — 2026-06-28
Initial Saga roadmap. Instantiates the framework's Saga slot for the Chineual as
a **living book**. Operator decisions recorded: (1) a new book-level roadmap
above the per-Project trees [this file]; (2) decomposition **by Part**, one
Project per Part; (3) sequencing along the **reader's path** (P1 Part II → P2
Part III → …), with opportunistic interleaving allowed for the partially-written
Parts (VI/VII/VIII). Captured the current per-Part snapshot (§4), the Project
roadmap (§5), the open backlog (§6), and the pending Nov-2025 whole-book-plan
reconciliation (§7). P1 (Part II) is active under the existing `docs/design-v0.1.0/`
tree; future Projects use `docs/<part-slug>/design-vX.Y.Z/`.

---

_This document is a living spec. It is meant to grow. If it ever stops growing
while the book keeps growing, that is the bug._
