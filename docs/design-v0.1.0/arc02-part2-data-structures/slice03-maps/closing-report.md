# Closing Report — Slice 03 (Maps)

Written by CC at slice close, 2026-06-28.

## Per-row walk

All 20 ledger rows reached a final status: **19 done, 1 deferred, 0 silent
drops.** Open-count (20) == close-count (20).

- **Rows 1–16 (files):** done. README landing page + 15 `##`-section leaves.
  `module.md` holds all 13 `maps:*` functions; `lfe.md` holds the five LFE forms;
  `concl.md` keeps the "#M(answer 42)" closing.
- **Row 17 (ToC):** done. 15 entries; CDC confirmed exact title match (incl. the
  long `nature` parenthetical) and resolving paths.
- **Rows 18–19 (reconciliation):** done. CDC bijective mapping (15/15); grep
  confirmed 0 stray `lisp` fences and the single `erlang` fence preserved in
  vs-proplists.
- **Row 20 (mdBook build):** deferred to operator at arc02 close (A5).

### Disclosed deviations

`lisp`→`lfe` fences; `###`→`##`. The `erlang` fence in vs-proplists kept (genuine
Erlang comparison code). Prose/code verbatim.

## Bubble-up to the arc

**1. Assigned piece delivered?** Yes — Maps published multi-leaf (README + 15)
with ToC, CDC-verified.

**2. What did this slice reveal?**

- **Correction to a slice-planning assumption: the bash `ls` is unreliable
  here.** When the shell came online I ran `ls src/part2/{maps,arrays,dicts,records}/`
  and it reported **README-only** for all four — so I planned Maps as "create
  the leaves from scratch." But the Write-probe then showed all 15 Maps leaf
  stubs already exist (every Write hit an existing file at the planned slug), and
  the Read tool confirmed them. **The file tools (Read/Write) are ground truth;
  the bash sandbox mount is stale.** So Maps WAS pre-scaffolded like Tuples and
  Proplists, and my slugs matched again (zero orphans).
- **Implication for slices 04–06 (arrays/dicts/records):** do NOT trust the bash
  `ls` that suggested they're README-only. Use the Write-probe (attempt the
  planned slug; "already exists" → adopt-and-fill). They are most likely
  pre-scaffolded too. Refines §A2.2.

**3. Silent-drop diff.** Specified: Maps multi-leaf + ToC. Delivered: all 16
files + 15 ToC entries. Only deferral is the operator build. No undisclosed
omissions.

## Arc-plan update decision

Refine §A2.2: the shell `ls` is a stale view of this repo; use the Read/Write
file tools (write-probe) to detect pre-scaffolded stubs, not bash. Recorded in
`arc-plan.md` v1.3. No breakdown/sequencing change. Next: slice04 (Arrays).
