# Q6 crosscheck completion — audit & witnesses

**Date:** 2026-06-16  **Branch:** `q6-crosscheck`  **Tracking:** OQ-83 follow-through; deferred tier → OQ-133

Plan: `~/.claude/plans/melodic-tumbling-fern.md`. Completes the Q6 synchronic crosscheck —
replaces the single-cell `zombie_piton_crosscheck/2` (dead × piton) with the full
status × computed-signature matrix `stakeholder_seats:q6_crosscheck/3`. Commentary-grade
(NEVER overrides `dr_type`).

## Load-bearing witness settled (the plan's outstanding one-query)

`dr_type/2` resolves at `constraint_indexing:default_context/1` =
`context(agent_power(analytical), …)` (constraint_indexing.pl:156–161), and json_report's
analytical perspective block is computed via the same `dr_type(C, Ctx, Type)` at analytical
(json_report.pl:906). **The proxy gap closes: `dr_type/2`-at-default IS the analytical
perspective.** Every control below is therefore selected by its real `dr_type/2` value, not a
pipeline_output proxy.

## Live corpus (`testsets/`, N=71) — status × dr_type/2 matrix

|          | snare | tangled_rope | rope | piton | unknown |
|----------|-------|--------------|------|-------|---------|
| live(23) | 8     | 6            | 2    | 2     | 5       |
| dead(8)  | 6     | 0            | 1    | 1     | 0       |
| contested(21)| 15| 1            | 2    | 0     | 3       |

**Zero stories at mountain/scaffold/naturalized under `dr_type/2`** → `q6_unclassified` is
WITNESSED 0 on the live corpus (not a proxy 0). Cell histogram (sums to 71):
contested_open=18, dead_claim_vs_piton_present=1, dead_claim_vs_rope_present=1,
dead_claim_vs_snare_present=6, live_claim_vs_piton_present=2, live_claim_vs_rope_present=2,
live_claim_vs_snare_present=8, live_claim_vs_tangled_present=6, q6_signature_unknown=8,
q6_unmeasured=19.

## Positive controls — every cell + each non-verdict bucket (all OK)

| story | cell |
|-------|------|
| adjunctification_of_university_teaching_c0 (dead) | dead_claim_vs_snare_present |
| press_reformation_causation__mutual_shaping (dead×piton) | dead_claim_vs_piton_present |
| catastrophe_memory_kernel__boundary_maintenance_reading (live×snare) | live_claim_vs_snare_present |
| refugee_convention_text__expansive_humanitarian_reading (live×piton) | live_claim_vs_piton_present |
| actinide_replenishment_mechanism_flat_control (live×tangled) | live_claim_vs_tangled_present |
| behavioral_adoption_friction (block, dr_type=unknown) | q6_signature_unknown |
| actinide_replenishment_mechanism_contradictions (no block) | q6_unmeasured |

## Mode-robustness bug caught by its own positive control

First implementation used a multi-clause first-match `q6_cell` with an unguarded catch-all
`q6_cell(_C, q6_unclassified)`. The reachability probe `q6_crosscheck(C, q6_unclassified, _)`
(expected 0) returned **all 71** — the catch-all spuriously succeeds when `Cell` is pre-bound,
and `q6_crosscheck/3` is exported (a consumer filtering by cell would get false-presence).
Fixed: single ordered if-then-else computing into a fresh var, unified with the caller's `Cell`
only at the end. Re-witnessed: bound-Cell census = 0; determinism = exactly 1 solution/story.

## Grade guard (commentary-grade — classification untouched)

`q6_crosscheck` has exactly one caller (report_generator.pl:r5_zombie_crosscheck_line/1) and is
NOT in json_report's per_constraint path (json_report uses report_generator only for
gap/omega/type-severity predicates). Nothing fed to `dr_type` changed → per_constraint
classification is byte-identical by construction. (Structural witness, not a recomputed diff.)

## Daylight axis — ships INERT (OPEN graduation step)

`founding_problem_corroboration_class/2` is an authored atom (NOT parsed from prose), declared
multifile+dynamic in narrative_ontology, emitted by generate_constraint_pl.py (witnessed on
scale_ceiling_c0: both `…/2` decl and the fact line emit when the field is authored), lint-gated
({independent, interested, ambiguous}). On merge no story authors it → every with-block story
reads `daylight(unstated)` (witnessed: daylight value set = [unstated]). Channel proven live by
asserting `interested` → `daylight(interested)` while the Cell stayed unchanged
(qualifier never changes the Cell). **OPEN next step:** a bounded R5 human/interview backfill over
the 52 corroboration prose blocks to author the class atom; until it lands the daylight axis is
inert and must be reported as such.

## Linter fail-loud (positive controls)

valid atoms → no error; `founding_problem_status(t, alive)` → INVALID_FOUNDING_PROBLEM_STATUS;
`founding_problem_corroboration_class(t, biased)` → INVALID_CORROBORATION_CLASS.

## Validation suite

`run_dynamic_suite`: 0 errors, 1 pre-existing warning (classification_mismatch on
zionist_legitimacy_basis__national_liberation_reading — corpus content, unrelated), 1 info.

## Twin-corpus check (operator request)

Both twins overlaid via `asserta(config:param(corpus_path, …))` (default-first-clause trap avoided).

| corpus | N | status domain | q6_unclassified | !=1 solution |
|--------|---|---------------|-----------------|--------------|
| testsets_haiku | 960 | {contested,dead,live} | 1 | 0 |
| testsets_flash | 960 | {contested,dead,live} | 5 | 0 |

**New finding:** `q6_unclassified` IS corpus-reachable on the twins (the live corpus merely lacks
the types). All members are `live × mountain` (haiku: price_formation_kernel__naturalist_reading;
flash: +article_27_veto_power__sovereignty_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading,
hebrew_living_language__liturgical_continuity_reading, quantum_formalism__copenhagen_reading) —
genuine residual types correctly routed as present-fell-through, never as absence, never
mis-named. Confirms the catch-all is real work, not dead code, and the mode-robustness fix matters.

## Files changed

- `prolog/stakeholder_seats.pl` — `zombie_piton_crosscheck/2` → `q6_crosscheck/3` (+ q6_cell, q6_named_cell, q6_daylight); export updated; tier-limit + uniform-arity + mode-robustness documented at the clause.
- `prolog/report_generator.pl` — `r5_zombie_crosscheck_line/1` prints Cell + Daylight + tier-limit label; silent on q6_unmeasured.
- `prolog/narrative_ontology.pl` — `founding_problem_corroboration_class/2` declared (multifile + dynamic); the mandatrophy-successor comment updated to q6_crosscheck/3.
- `python/generate_constraint_pl.py` — emit the corroboration_class atom + decl (conditional on authored presence).
- `python/linter.py` — fail-loud domain checks for founding_problem_status + corroboration_class.

## Deferred (out of scope) → OQ-133

The diachronic tier (wire `founding_problem_status` as the `t0` anchor of the temporal series).
Its target is the **confrontation-response signature** (Corollary 3 honor-vs-reabsorb), NOT the
trajectory — trajectory underdetermines orientation. Gated behind OQ-109/OQ-110 + repair_dynamics §6.
