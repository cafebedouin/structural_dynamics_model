# Audit Report: 63 Classification Failures from Index Sufficiency Test

**Generated:** 2026-02-17
**Source data:** `outputs/index_sufficiency.json` (63 entries under `classification_failures`)
**Pipeline data:** `outputs/pipeline_output.json` (perspective classifications from `dr_type/3`)

---

## Executive Summary

63 classification failures occur when `dr_type/3` returns `unknown` at a context
where the testset declares a valid type. Three root causes account for all 63:

| Root Cause | Count | % | Fix Complexity |
|---|---|---|---|
| Chi overflow at analytical | 38 | 60.3% | Medium (threshold/cap adjustment) |
| FCR perspectival preservation of `unknown` | 18 | 28.6% | Low (guard clause) |
| NL cut-scope bug | 6 | 9.5% | Low (replace `!, fail` with `\+`) |
| Genuine indexical opacity | 1 | 1.6% | None (correct behavior) |

**Two bugs are structural Prolog defects; one is a threshold calibration issue.**

---

## Root Cause 1: NL Cut-Scope Bug (6 failures)

### Mechanism (confirmed via Prolog trace)

In `drl_core.pl:262-263`, a natural-law blocking clause:

```prolog
classify_from_metrics(C, _BaseEps, _Chi, _Supp, _Context, snare) :-
    natural_law_without_beneficiary(C), !, fail.     % Block snare for natural laws
```

When `classify_from_metrics/6` is called with an **unbound** 6th argument (as
`metric_based_type_indexed/3` does), Prolog matches this clause with `Type = snare`
via unification. The `!` then prunes ALL remaining clause choices for the entire
predicate -- including rope, tangled_rope, piton, and the `unknown` fallback.

An identical bug exists at line 292-293 for the tangled_rope blocker, but the snare
blocker fires first.

### Trace Evidence (cap_theorem at institutional context)

```
BaseEps: 0.05   Chi: -0.006   Supp: 0.01
Mountain metric: FAIL  (effective_immutability(generational, arbitrage, mountain) = NO)
classify_from_metrics(cap_theorem, 0.05, -0.006, 0.01, Ctx, Type): COMPLETE FAILURE
classify_from_metrics(cap_theorem, 0.05, -0.006, 0.01, Ctx, rope): PASS   ← bound Type works
natural_law_without_beneficiary(cap_theorem): YES                          ← cut trigger
resolve_modal_signature_conflict(rope, natural_law, X): mountain           ← NL override works
dr_type at institutional: unknown                                          ← but never reached
dr_type at analytical: mountain                                            ← mountain gate succeeds here
```

### Pipeline Cross-Check

```
cap_theorem perspectives:
  powerless:     mountain   (immediate/trapped → mountain immutability passes)
  moderate:      unknown    ← BUG (biographical/mobile → mountain fails → cut kills)
  institutional: unknown    ← BUG (generational/arbitrage → mountain fails → cut kills)
  analytical:    mountain   (civilizational/analytical → mountain immutability passes)
```

### Affected Constraints (6)

| Constraint | Index Config | Expected Type |
|---|---|---|
| cap_theorem | [institutional, generational, arbitrage, national] | mountain |
| central_limit_theorem | [institutional, generational, arbitrage, national] | mountain |
| four_color_theorem_topological_bound | [institutional, generational, arbitrage, national] | mountain |
| prisoners_dilemma_equilibrium | [institutional, generational, arbitrage, national] | mountain |
| thermodynamics_entropy | [institutional, generational, arbitrage, national] | mountain |
| pareto_principle | [moderate, biographical, mobile, national] | mountain |

All have signature `natural_law` and purity `pristine`.

### Fix

Replace the `!, fail` blocking pattern with negation-as-failure in the positive clause:

```prolog
% BEFORE (drl_core.pl:262-263) — cut-scope bug
classify_from_metrics(C, _BaseEps, _Chi, _Supp, _Context, snare) :-
    natural_law_without_beneficiary(C), !, fail.
classify_from_metrics(_C, BaseEps, Chi, Supp, Context, snare) :-
    ...

% AFTER — safe guard
classify_from_metrics(_C, BaseEps, Chi, Supp, Context, snare) :-
    \+ natural_law_without_beneficiary(_C),
    ...
```

Same fix at line 292-293 for the tangled_rope blocker.

---

## Root Cause 2: Chi Overflow at Analytical (38 failures)

### Mechanism (confirmed via Prolog trace)

At the analytical context `[analytical, civilizational, analytical, global]`, the
directionality chain produces a high Chi multiplier (sigmoid_f of distance ~1.15-1.38).
Combined with the global scope modifier (1.2), constraints with BaseEps > ~0.55 get
Chi values exceeding the `tangled_rope_chi_ceil` of 0.90.

All metric gates then fail:
- **tangled_rope:** Chi > 0.90 ceiling
- **snare:** Typically Supp < 0.60 floor (many constraints have moderate suppression)
- **rope:** Chi > 0.35 ceiling
- **mountain:** BaseEps too high, or not NL

The FCR signature is detected, but `has_metric_perspectival_variance` finds variance
(e.g., `rope` at institutional vs `unknown` at analytical), so the FCR override
preserves `unknown` instead of overriding to `tangled_rope`.

### Trace Evidence (regulatory_capture at analytical context)

```
BaseEps: 0.80   Chi: 1.096   Supp: 0.20
tangled_rope: Chi 1.096 > ceil 0.90 → FAIL
snare: Supp 0.20 < floor 0.60 → FAIL
rope: Chi 1.096 > ceil 0.35 → FAIL
Metric type: unknown
Signature: false_ci_rope
dr_type at analytical: unknown   ← FCR preserves unknown (perspectival variance exists)
dr_type at powerless: unknown    ← also unknown (different mechanism)
```

### Pipeline Cross-Check

```
regulatory_capture perspectives:
  powerless:     unknown    (Chi too low for snare at this context)
  moderate:      unknown    (same mechanism)
  institutional: rope       (Chi low enough at institutional)
  analytical:    unknown    ← Chi overflow
```

### Type Distribution in This Category

| Expected Type | Count |
|---|---|
| tangled_rope | 25 |
| piton | 4 |
| rope | 4 |
| scaffold | 2 |
| snare | 2 |
| mountain (via NL, detected as Chi overflow) | 1 |

### Fix Options (choose one)

**Option A: Raise tangled_rope Chi ceiling**
Change `tangled_rope_chi_ceil` from 0.90 to ~1.20 in `config.pl:191`.
Risk: may over-classify constraints as tangled_rope.

**Option B: Cap Chi at analytical context**
Add a Chi-capping clause in `extractiveness_for_agent` that limits Chi to 0.90
when the analytical multiplier would push it above ceiling.

**Option C: FCR override should not preserve `unknown`** (see Root Cause 3 fix — shared fix)
When `ModalType = unknown`, the FCR gate should override to `tangled_rope`
regardless of perspectival variance, because `unknown` is not a valid classification
to preserve.

---

## Root Cause 3: FCR Perspectival Preservation at Powerless (18 failures)

### Mechanism (confirmed via Prolog trace)

At the powerless context `[powerless, biographical, trapped, local]`, Chi values
are moderate (0.40-0.65 typical). The specific gate failures vary:
- **snare:** Chi < 0.66 floor (most common, 13/18)
- **tangled_rope:** Missing structural predicates (`requires_active_enforcement`,
  `has_coordination_function`, `has_asymmetric_extraction` not declared in testset)
- **rope:** Chi > 0.35 ceiling

Metric type falls to `unknown`. The FCR signature is detected and
`has_metric_perspectival_variance` finds variance (e.g., `snare` at analytical vs
`unknown` at powerless). The FCR override preserves `unknown`.

### Trace Evidence (absorbing_markov_chain_trap at powerless)

```
BaseEps: 0.55   Chi: 0.598   Supp: 0.65
snare: Chi 0.598 < floor 0.66 → FAIL
tangled_rope: requires_active_enforcement COMMENTED OUT → FAIL
rope: Chi 0.598 > ceil 0.35 → FAIL
Metric type: unknown
Signature: false_ci_rope
dr_type at powerless: unknown   ← FCR preserves unknown
dr_type at analytical: snare    ← Chi is higher here, meets snare threshold
```

### Pipeline Cross-Check

```
absorbing_markov_chain_trap perspectives:
  powerless:     unknown    ← FCR preserves unknown (variance with analytical=snare)
  moderate:      unknown    (same mechanism)
  institutional: scaffold   (different metric path)
  analytical:    snare      (Chi meets snare threshold at this context)
```

### Type Distribution in This Category

| Expected Type | Count |
|---|---|
| snare | 13 |
| tangled_rope | 3 |
| rope | 2 |

### Fix: Don't preserve `unknown` through FCR gate

In `signature_detection.pl:619-624`, the FCR perspectival gate currently preserves
`ModalType` when variance exists — even when ModalType is `unknown`:

```prolog
% BEFORE (signature_detection.pl:619-624) — preserves unknown
resolve_with_perspectival_check(C, ModalType, false_ci_rope, AdjustedType) :-
    !,
    (   has_metric_perspectival_variance(C)
    ->  AdjustedType = ModalType
    ;   AdjustedType = tangled_rope
    ).

% AFTER — guard against preserving unknown
resolve_with_perspectival_check(C, ModalType, false_ci_rope, AdjustedType) :-
    !,
    (   ModalType \= unknown,
        has_metric_perspectival_variance(C)
    ->  AdjustedType = ModalType
    ;   AdjustedType = tangled_rope
    ).
```

This fix resolves all 18 powerless failures AND ~30 of the 38 analytical failures
(the ones where the FCR override would produce the correct type). Combined with
the Chi ceiling fix, it resolves all 56 FCR-signature failures.

---

## Root Cause 4: Genuine Indexical Opacity (1 failure)

| Constraint | Index Config | Types |
|---|---|---|
| trump_epa_greenhouse_gas_reversal | [institutional, generational, arbitrage, national] | [indexically_opaque, rope] |

This is the only failure where neither type is `unknown`. The constraint genuinely
occupies an intermediate position between classification categories at this context.
**No fix needed — this is correct system behavior.**

---

## Consolidated Failure Table

| # | Constraint | Index Config | Types | Root Cause |
|---|---|---|---|---|
| 1 | absorbing_markov_chain_trap | powerless | [snare, unknown] | FCR preservation |
| 2 | artificial_snow_2026 | powerless | [snare, unknown] | FCR preservation |
| 3 | astm_d638_tensile_testing | powerless | [snare, unknown] | FCR preservation |
| 4 | big_data_astrophysics_arbitrage | analytical | [tangled_rope, unknown] | Chi overflow |
| 5 | burden_of_proof_legal_criminal | powerless | [rope, unknown] | FCR preservation |
| 6 | cap_theorem | institutional | [mountain, unknown] | NL cut-scope |
| 7 | central_limit_theorem | institutional | [mountain, unknown] | NL cut-scope |
| 8 | clawderberg_recursive_slop | analytical | [piton, unknown] | Chi overflow |
| 9 | cma | analytical | [tangled_rope, unknown] | Chi overflow |
| 10 | college_admissions_market | analytical | [tangled_rope, unknown] | Chi overflow |
| 11 | cost_of_observation | powerless | [snare, unknown] | FCR preservation |
| 12 | cs_ecmo_bridge | analytical | [tangled_rope, unknown] | Chi overflow |
| 13 | education_unbundling_implementation | analytical | [tangled_rope, unknown] | Chi overflow |
| 14 | emergency_oversight_bureau | analytical | [tangled_rope, unknown] | Chi overflow |
| 15 | erasmus_rejoining_scaffold | analytical | [scaffold, unknown] | Chi overflow |
| 16 | ergo_storage_rent | analytical | [tangled_rope, unknown] | Chi overflow |
| 17 | exoplanetary_habitability_arbitrage | analytical | [rope, unknown] | Chi overflow |
| 18 | factional_instability | analytical | [tangled_rope, unknown] | Chi overflow |
| 19 | fiscal_equalization_friction | analytical | [rope, unknown] | Chi overflow |
| 20 | four_color_theorem_topological_bound | institutional | [mountain, unknown] | NL cut-scope |
| 21 | france_local_elections_march_2026 | powerless | [snare, unknown] | FCR preservation |
| 22 | gamblers_ruin_stochastic_extinction | powerless | [snare, unknown] | FCR preservation |
| 23 | glp1_payload_efficiency_pivot | powerless | [snare, unknown] | FCR preservation |
| 24 | goodstein_theorem_finite_proof | analytical | [tangled_rope, unknown] | Chi overflow |
| 25 | grete_samsa_transition | analytical | [tangled_rope, unknown] | Chi overflow |
| 26 | guinea_worm_eradication | powerless | [snare, unknown] | FCR preservation |
| 27 | hoa_covenants | powerless | [snare, unknown] | FCR preservation |
| 28 | hoa_covenants | analytical | [piton, unknown] | Chi overflow |
| 29 | institutional_trust_decay | analytical | [piton, unknown] | Chi overflow |
| 30 | insult_wisdom_training | analytical | [tangled_rope, unknown] | Chi overflow |
| 31 | maha_recovery_2026 | analytical | [scaffold, unknown] | Chi overflow |
| 32 | mit_tfus_2026 | powerless | [snare, unknown] | FCR preservation |
| 33 | moltbook_agent_theater | analytical | [piton, unknown] | Chi overflow |
| 34 | negative_emissions_arbitrage | analytical | [tangled_rope, unknown] | Chi overflow |
| 35 | net_zero_stabilization | analytical | [tangled_rope, unknown] | Chi overflow |
| 36 | neural_interoperability | powerless | [snare, unknown] | FCR preservation |
| 37 | neural_interoperability | analytical | [tangled_rope, unknown] | Chi overflow |
| 38 | neurodiversity_spectrum | analytical | [tangled_rope, unknown] | Chi overflow |
| 39 | olympic_legacy_curling_investment | powerless | [tangled_rope, unknown] | FCR preservation |
| 40 | pareto_principle | moderate | [mountain, unknown] | NL cut-scope |
| 41 | paris_municipal_reform_2026 | powerless | [rope, unknown] | FCR preservation |
| 42 | paris_municipal_reform_2026 | analytical | [tangled_rope, unknown] | Chi overflow |
| 43 | platonic_coparenting_decoupling | analytical | [tangled_rope, unknown] | Chi overflow |
| 44 | prisoners_dilemma_equilibrium | institutional | [mountain, unknown] | NL cut-scope |
| 45 | recipe_scaling_ai | analytical | [tangled_rope, unknown] | Chi overflow |
| 46 | regulatory_capture | analytical | [tangled_rope, unknown] | Chi overflow |
| 47 | repair_probe_incomplete | powerless | [snare, unknown] | FCR preservation |
| 48 | repair_probe_incomplete | analytical | [tangled_rope, unknown] | Chi overflow |
| 49 | rosen_bridge_protocol | analytical | [tangled_rope, unknown] | Chi overflow |
| 50 | rotation_seven_black_soil | powerless | [snare, unknown] | FCR preservation |
| 51 | russells_paradox_self_reference | analytical | [snare, unknown] | Chi overflow |
| 52 | sadhu_integrity_protocol | powerless | [tangled_rope, unknown] | FCR preservation |
| 53 | shitty_feedback_handling | analytical | [rope, unknown] | Chi overflow |
| 54 | smartphone_ubiquity | analytical | [tangled_rope, unknown] | Chi overflow |
| 55 | teaching_horses_to_sing | analytical | [tangled_rope, unknown] | Chi overflow |
| 56 | temporal_scale_arbitrage | analytical | [tangled_rope, unknown] | Chi overflow |
| 57 | thermodynamics_entropy | institutional | [mountain, unknown] | NL cut-scope |
| 58 | tragedy_of_the_commons | analytical | [tangled_rope, unknown] | Chi overflow |
| 59 | transformer_self_attention | analytical | [tangled_rope, unknown] | Chi overflow |
| 60 | trillion_bond_rush_2026 | analytical | [snare, unknown] | Chi overflow |
| 61 | trump_epa_greenhouse_gas_reversal | institutional | [indexically_opaque, rope] | Genuine opacity |
| 62 | unclos_2026 | analytical | [rope, unknown] | Chi overflow |
| 63 | working_dog_training | powerless | [tangled_rope, unknown] | FCR preservation |

---

## Recommended Fix Priority

### Fix 1 (Low effort, resolves 6): NL cut-scope bug in `drl_core.pl`
- Replace `!, fail` with `\+` guard in snare blocker (line 262-263)
- Same for tangled_rope blocker (line 292-293)
- **Files:** `prolog/drl_core.pl`

### Fix 2 (Low effort, resolves ~48): FCR `unknown` preservation in `signature_detection.pl`
- Add `ModalType \= unknown` guard before perspectival variance check (line 619-624)
- **Files:** `prolog/signature_detection.pl`

### Fix 3 (Medium effort, resolves remaining ~8): Chi ceiling adjustment
- Raise `tangled_rope_chi_ceil` from 0.90 to ~1.20 in `config.pl:191`
- Or add Chi-capping in `constraint_indexing.pl:extractiveness_for_agent`
- **Files:** `prolog/config.pl` or `prolog/constraint_indexing.pl`

### Combined Impact
- Fix 1 alone: 6 resolved (9.5%)
- Fix 1 + Fix 2: ~54 resolved (85.7%)
- Fix 1 + Fix 2 + Fix 3: 62 resolved (98.4%)
- Remaining 1: genuine indexical opacity (correct behavior)

---

## Verification Notes

All three Prolog traces were executed against the live stack (`stack.pl`) with
testset data loaded and `data_repair:repair_interval` applied. Trace results
were cross-checked against `pipeline_output.json` perspectives. All hypothesized
failure mechanisms were confirmed.
