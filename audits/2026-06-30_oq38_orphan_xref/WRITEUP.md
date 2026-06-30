# OQ-38 — Reproducible export-vs-caller orphan census

**Execution date:** 2026-06-30
**Resolves:** OQ-38 (dead-code / orphan triage). Routes the remainder to **OQ-196**.
**Pipeline manifest at execution:** `n_constraints=116`, per_constraint sha256 `d9c85bec…`
(byte-stable across all three pipeline runs this session — the strips are behavior-preserving).

## Problem

ISSUES.md carried a "**217-candidate orphan upper bound**" (`528 exports → 422
zero-external-caller → 217 candidate`) hand-transcribed from an **uncommitted ad-hoc grep sweep**
(2026-05-31 `wiring_gap_census.md`). No reproducible tool existed, and the 217 conflated
genuinely-dead predicates with over-exported-but-internally-called ones. Per the operator's scope
ruling (**Option 1**): build the reproducible tool, strip only the **four calibration orphans**,
and route the held-out remainder to a follow-up OQ — because a first-run analyzer's *full* list is
itself an aggregate, and mass-stripping it would trust exactly the green check this OQ exists to
distrust.

## Tool (canonical; this dir holds output copies, not a fork)

- **`prolog/orphan_xref.pl`** — `library(prolog_xref)` clause-head-vs-body separator. Mirrors
  `check_stack.pl`: load-path-independent, library-driven, **diagnostic NOT a pipeline gate**. Per
  defined `Name/Arity`: file, exported?, static-caller set (module-stripped), class
  (`LIVE` / `ENTRYPOINT_CLI` / `STATIC_ORPHAN`). Caller matching is global `Name/Arity` —
  deliberately **conservative** (biases LIVE; the only dangerous error for an orphan tool is a
  false orphan).
- **`python/audits/oq38_orphan_sweep.py`** — runs the core, builds the dynamic-reachability surface
  (Python/shell goal-strings + Prolog name-construction prefixes), masks static orphans, emits the
  funnel.

Re-run: `cd prolog && swipl -l orphan_xref.pl -g "run_orphan_xref, halt" -t "halt(1)"` then
`python3 python/audits/oq38_orphan_sweep.py`.

## Tool-native funnel (replaces the ad-hoc grep; 121 sources, pre-strip)

```
tool exports ............ 614   (prior grep claim 528  delta +86  <- FINDING: grep undercounted exports)
zero-static-caller ...... 255   (= 201 STATIC_ORPHAN + 54 ENTRYPOINT_CLI)
N (xref STATIC_ORPHAN) .. 201   (prior grep candidate 217  delta -16)
- dynamic-masked ........ 29
= M (real orphan list) .. 172
```

**[EDGE]** M is still an **upper bound**: "statically uncalled" ≠ "dead" for anything reachable via
a Python goal-string or Prolog name-construction (`=..`, `atom_concat`, `atomic_list_concat`,
`format(atom(...))`, `term_to_atom`) — the axis `prolog_xref` is blind to. The funnel names every
delta from the prior claim; `tool-exports ≠ 528` is a finding, not an error suppressed.

## Stage-1 hard gate (the tool earned trust before any positive flag)

| Negative control | Tool verdict | Caller |
|---|---|---|
| `cs_reference_frame/2` | **LIVE** | `json_report.pl:write_per_constraint_entry/4` |
| `non_monotonic_trajectory/2` | **LIVE** | `metric_drift_report.pl:generate_drift_report/1` |

`cs_reference_frame/2` is the adversarial case the stale grep called dead (OQ-35 corrected it by
code-read). The tool reports it `LIVE` — it discriminates on the exact case the grep blew.
`non_monotonic_trajectory/2`'s caller resolves to **one** file, `metric_drift_report.pl` — confirming
the OQ census's `drift_report.pl:164` cite was **stale** (that file is absent). Cite corrected in the
OQ-38 entry. All five name-construction positive controls fire (see `stage1_controls.txt`).

## The four calibration orphans stripped

| Predicate | File | Tool class | Commit |
|---|---|---|---|
| `linear_slope/2` | drl_composition.pl | STATIC_ORPHAN | A `736783e4` |
| `slope_accum/3` | drl_composition.pl | LIVE-via-`linear_slope` only (cascade tail) | A `736783e4` |
| `safe_get_all_metrics/2` | utils.pl | STATIC_ORPHAN | B `6a3acf1d` |
| `safe_get_profile_components/2` | utils.pl | STATIC_ORPHAN | B `6a3acf1d` |

All four absent from the dynamic surface (`py_surface=False constructible=False`). The
"safe_get wrappers are what a harvester invokes by name" worry was a labeled **risk-flag, not a
fact** — downgraded by witness: zero literal references, no `safe_get_` construction across any
class.

**Behavior-preserving witnesses (same session, both commits):** `load_warning_gate.py` exit 0
(3 allowlisted, 0 unexpected); validation suite byte-identical (timing-normalized); `run_pipeline.py`
exit 0 with `per_constraint` sha256 unchanged (`d9c85bec…`) and mtime advanced.

## Cascade finding

Commit B newly orphaned exactly **one** predicate: `safe_get_category/3` (its sole caller
`safe_get_all_metrics/2` removed). Arithmetic reconciles: `201 −1 (linear_slope) −2 (safe_get pair)
+1 (safe_get_category) = 199` post-strip STATIC_ORPHAN. Per the scope ruling it is **not** stripped
here — it routes to OQ-196 with the pre-existing `safe_get_extractiveness/2` / `safe_get_suppression/2`
orphans (the now-dead tail of the batch wrapper). Stripping dead code reveals transitively-dead code;
the single-pass tool surfaces it, a fixpoint pass (OQ-196) drains it.

## Artifacts in this directory

- `orphan_xref_census.tsv` — full per-predicate census (post-strip; the four absent).
- `orphan_funnel.json` — the tool-native funnel + masked/survivor lists.
- `stage1_controls.txt` — negative-control verdicts + the four + the cascade seed.
