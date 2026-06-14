# Omega-gap feeder reconstruction — rewire `detect_gap_pattern` onto authored seats

**Date:** 2026-06-14 · **Branch:** `omega-gap-rewire` · **OQ:** OQ-129
**Files changed:** `prolog/report_generator.pl` (the one real rewire + labeling),
`prolog/json_report.pl` (coverage-bit guard).

## Finding (rename-stranded feeder, RESOLVED for the rewire)

`omega_from_gap/5` stopped firing after the 2026-06-05 corpus rebuild not because it broke
but because its **feeder** read a **retired predicate**. `detect_gap_pattern/2` queried
`constraint_indexing:constraint_classification/3` — the pre-rebuild "type pre-computed per
power-seat and stored as a fact" surface. The rebuild moved that idea to
`narrative_ontology:constraint_stakeholder/7` (each authored seat carries its
`(Power,Time,Exit,Scope)` context; the type is **computed on demand**). Live corpus:
0 `constraint_classification` facts (bar one engine demo) → 0 gaps → 0 omegas. One dead wire.

## The change

- **`detect_gap_pattern/2`** now enumerates the authored seats and computes each seat's type
  via the **canonical seat path** `stakeholder_seats:dr_type_for_stakeholder/3` (per-`(C,Name)`
  d = role-d + exit modulation — the coordinate that *escapes the same-power atom collapse*).
  A gap = **≥2 distinct non-`unknown` computed types**. Fail-closed: <2 typeable seats or <2
  distinct types ⇒ abstain (mint nothing).
- **Deviation from plan, flagged:** the plan proposed inline `drl_core:dr_type(C, Ctx, Type)`.
  That re-collapses two same-power seats into one coordinate — exactly what `stakeholder_seats`
  exists to avoid. The canonical seat path is used instead. **Witnessed verdict-equivalent**
  on the live corpus (`dpath_equivalence.txt`: both paths → gap=20, zero DIFFER rows); the
  paths differ only in a few all-`unknown` cases that never cross the ≥2-distinct threshold.
- **`omega_from_gap/5`** rewritten as **labeling** (not detection). Two grounded patterns:
  `extraction_blindness` (an extractive-typed seat at **lower power** than a functional-typed
  seat — the Theorem-1 cover-story structure → `omega_extraction_blindness_<C>`, critical) and
  `general_type_mismatch` (→ `omega_perspectival_<C>`). The label is computed into **fresh
  vars then unified** (mirrors `drl_core:dr_type/3` FinalType) so a caller pre-binding the
  pattern cannot bypass the priority cascade.
- **`json_report.pl` gaps-array guard** (the plan missed this consumer): was guarded on the
  dead `constraint_classification/3` → would have kept emitting `gaps:null` for every live
  constraint. Replaced with `report_generator:gap_coverage/1` (true iff ≥1 seat computes a
  non-`unknown` type) so `null` (didn't-look) and `[]` (looked, no gap) stay distinct.

## Witnesses (this dir)

- **`per_item_dump.txt`** — standing per-constraint witness (one row each: verdict, pattern,
  omega, seat power-type pairs). Tally: **20 GAP / 17 no_gap / 20 abstain** (19 no-seat +
  1 all-unknown). All 20 gaps label `extraction_blindness`; `general:0` on the live corpus.
- **`label_controls.txt`** — 5/5 PASS. Positive control for **both** labels (so `general:0`
  is genuine absence, not dead code), negative control on the power ordering (extractive-at-
  *higher*-power must NOT label extraction_blindness → it routes to general), and single-type
  → fail.
- **`dpath_equivalence.txt`** — canonical-seat vs inline-`dr_type`: both gap=20, no DIFFER.
- **`twin_breadth.txt`** — breadth check on the twins (per operator request): `testsets_flash`
  960 → 481/267/212; `testsets_haiku` 960 → 369/125/466. Fires gaps, abstains on empty,
  discriminates (large no-gap populations). All-unknown-seated population tiny (2 / 0).
- **Mint witness** (plan item 5), `sex_gender_category__identity_reading`:
  `gap(extraction_blindness,snare,naturalized)` →
  `omega_extraction_blindness_sex_gender_category__identity_reading`, conceptual, **critical**.
- **Pipeline (item 4):** `python3 python/run_pipeline.py` all steps ok; `pipeline_output.json`
  carries 20 unique `omega_extraction_blindness_*` omegas; gaps array null=20 · []=17 ·
  populated=20.
- **No regression:** `check_stack` clean; dynamic validation suite 0 errors (1 pre-existing
  unrelated `classification_mismatch` warn on zionist).

## Carried OPENs → OQ-129

- **OPEN-A** — Ω labeling partition. extraction_blindness/general grounded (docs/logic.md §B.7;
  v7 Theorem-1 raw orbit `[naturalized,snare,rope,snare]`). Finer labels deferred:
  `cut_safety` (mountain/rope) and `learned_helplessness` (snare/mountain) currently fold into
  `general_type_mismatch` / `extraction_blindness` — no grounded partition for them yet.
- **OPEN-B** — `coverage_map.md`: deliberate-vs-hole call per abstainer needs prose.
  `catastrophe_memory_kernel` is a strong HOLE candidate (six_questions=8, stakeholders=0).
- **OPEN-C** — `livelihood_security_reading`: 8 seats all compute `unknown`. Now `gaps:null`.
  Missing-metric hole vs genuinely untyped — unresolved.
- **OPEN-D** — dedup. On live corpus moot (20 gap-omega constraints, 0 authored-omega
  constraints; distinct ID prefixes; `collect_omegas` already dedups by ID). Re-check when
  authored omegas co-occur with gaps.

## Reproduce

Probe sources are saved alongside this README (`probe_*.pl`). Run from `prolog/`:
`swipl -q -g main probe_verify_feeder.pl` etc.
