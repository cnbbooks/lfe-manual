# CDC Verification — Slice 07 (Graphs, authoring)

Independent dual-gate verification at slice close, 2026-06-29. A fresh subagent
read all 10 published files and the Graphs block of `SUMMARY.md`, cross-checked
every `digraph`/`digraph_utils` claim against the stdlib v7.1 reference, and —
because Erlang is installed locally and `digraph`/`digraph_utils` are the same
underlying stdlib modules — **empirically ran the equivalent calls through
`erl`** (translating each LFE example back to Erlang) to confirm outputs.

## Verdict: PASS (technical accuracy + LFE validity + voice), nits addressed

### Gate 1 — Technical accuracy (reconciled, empirically)

Every blocker-class claim held up against live Erlang:

- **`create.md`** — `info/1` on an acyclic empty graph returns `(#(cyclicity
  acyclic) #(memory 950) #(protection protected))` — confirmed *exactly*,
  including the `memory 950` literal and field order. `new/0`/`new/1` handle
  shapes (`#(digraph … true)` cyclic, `… false)` acyclic) confirmed.
- **`vertices.md`** — add_vertex/2,3 returns, `vertex/2` shapes (`#(V Label)`,
  `#(zaphod ())`, `false`), degrees (2 / 0), neighbour lists, auto-id
  `(|$v| . 0)` — all confirmed.
- **`edges.md`** — add_edge/3,4,5 returns, `edge/2` four-tuple, the
  `#(error #(bad_vertex V))` and `#(error #(bad_edge (a b)))` error paths —
  confirmed. `bad_edge` path is `(a b)`, matching the doc.
- **`paths.md`** — `get_path` / `get_short_path` / `get_cycle` / del_path
  transcripts and the `get_short_cycle` loop-as-`(V V)` distinction — confirmed.
- **`utils.md`** — `topsort` `(hull power core drive ship)`, `reachable`
  `(ship drive core)`, `reaching` `(power core drive hull ship)`,
  cyclic `topsort` → `false`, strong/cyclic_strong_components `((egg chicken))`,
  the `_neighbours` length-≥1 semantics, is_tree/is_arborescence/
  arborescence_root — all confirmed.
- **`example.md`** — the `build-order` module translated faithfully to Erlang
  and run: `#(ok (hull power core drive ship))` and `#(error cyclic)` —
  confirmed *exactly* as printed in the chapter.
- **`mutability.md`** — aliasing demo (`h = g` ⇒ one graph, two names),
  ETS-backed / owner-only-writes / not-GC'd / dies-with-creator / `delete/1`
  — all match the `digraph` stdlib intro.

### Gate 2 — LFE validity & idiom (reproduced)

- Zero raw-Erlang syntax; all 42 fences ```` ```lfe ````; all prompts `lfe>`.
- The `build-order` module is well-formed LFE (defmodule/export/defun/let/
  try-after/progn/lc/case/element/`#(...)`); CDC confirmed the s-expressions
  are syntactically valid (LFE not installed, so verified by structure +
  Erlang-equivalent run).
- The opaque auto-id rendering `(|$v| . 0)` / `(|$e| . 0)` is the correct LFE
  print form of the Erlang improper list `['$v'|0]` (bar-quoted atom · integer).

### Gate 3 — Voice & structure (attested)

- Prime Directive upheld throughout; `mutability.md` opener is the style guide's
  own §13 Example B (sanctioned). No punching at the reader. Code carries voice
  (HHG cast). No lone-heading placeholders.
- All 9 SUMMARY link texts == leaf `#` titles verbatim; all 10 paths resolve;
  parent `[Graphs]` short-label entry consistent with sibling chapters.

## Nits raised by CDC → resolved

1. **"3 a.m." over-rationed (≈5–6×).** Thinned to **3** load-bearing uses
   (mutability opener, the leak payoff, the summ closing callback); the three
   incidental uses re-voiced (README, vertices, and the mutability close — the
   last now pays off the *trench-coat* motif instead).
2. **`edges.md` omitted a corner:** reusing an edge id across *different*
   endpoints errors (`#(error #(bad_edge …))`) rather than modifying. Added a
   sentence making this explicit (CDC empirically confirmed the error path).

## Residual risk

Low. Every deterministic output was reproduced against the live stdlib modules.
The only unreproduced item is a direct LFE *compile* of the worked module (LFE
is not installed in this environment); its syntax was reviewed against the
published corpus and its semantics confirmed via the Erlang equivalent.

**Slice 07 (Graphs) is CDC-closed.**
