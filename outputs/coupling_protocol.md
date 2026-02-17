
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
# Inferred Coupling Activation Protocol

*Tests whether the dormant `infer_structural_coupling/3` mechanism*
*can be activated by providing `measurement/5` ground facts, and*
*measures the resulting network impact.*

---

## Protocol Documentation

### measurement/5 Signature

```prolog
measurement(?Source, ?Constraint, extractiveness, ?Time, ?Value)
```

- **Source**: Atom identifying the measurement source
- **Constraint**: The constraint being measured
- **Metric**: Must be the atom `extractiveness` (hardcoded in `dr_gradient_at/3`)
- **Time**: Numeric timepoint
- **Value**: Float in [0, 1]

### Algorithm

1. `dr_gradient_at(C, T, Grad)` extracts gradient = X(T2) - X(T) for consecutive timepoints
2. `infer_structural_coupling(C1, C2, Strength)` computes sign-agreement ratio
3. If Strength >= `network_coupling_threshold` (0.50), an edge is created
4. `constraint_neighbors/3` includes inferred edges in neighbor discovery

### Requirements

- 3+ timepoints per constraint (produces 2+ gradients, satisfying L > 1)
- Both constraints in a pair must have the same number of gradients
- Sign-agreement: both positive, both negative, or both zero

## Phase 2: Gradient Verification

Verifying that `dr_gradient_at/3` produces gradients from the `measurement/5` facts:

| Constraint | Gradients | Count | Status |
|------------|-----------|-------|--------|
| quantum_decryption_risk_2026 | [] | 0 | FAIL |
| smartphone_ubiquity | [] | 0 | FAIL |
| regulatory_capture | [] | 0 | FAIL |
| institutional_trust_decay | [] | 0 | FAIL |
| tragedy_of_the_commons | [] | 0 | FAIL |
| pareto_principle | [] | 0 | FAIL |
| hawthorne_effect | [] | 0 | FAIL |
| rotation_seven_black_soil | [] | 0 | FAIL |

**Result**: 0/8 constraints produce 2+ gradients.

## Phase 3: Edge Creation Verification

Testing each designed constraint pair:

| Pair | C1 | C2 | Expected | Actual | Edge? | Verdict |
|------|----|----|----------|--------|-------|---------|
| pair_1_tech_ecosystem | quantum_decryption_risk_2026 | smartphone_ubiquity | 1.00 | 0.00 | no | FAIL |
| pair_2_institutional_erosion | regulatory_capture | institutional_trust_decay | 1.00 | 0.00 | no | FAIL |
| pair_3_commons_degradation | tragedy_of_the_commons | pareto_principle | 1.00 | 0.00 | no | FAIL |
| pair_4_negative_control | hawthorne_effect | rotation_seven_black_soil | 0.00 | 0.00 | no | PASS |

**Result**: 1/4 pairs behave as expected.

## All Inferred Coupling Edges

Total inferred edges above threshold: **22**

| C1 | C2 | Strength |
|----|----|-----------| 
| artificial_snow_2026 | emotional_cycles_2026 | 0.500 |
| artificial_snow_2026 | floating_wall_2026 | 1.000 |
| artificial_snow_2026 | iran_war_room_2026 | 1.000 |
| artificial_snow_2026 | silklink_2026 | 1.000 |
| artificial_snow_2026 | trillion_bond_rush_2026 | 1.000 |
| artificial_snow_2026 | world_factbook_sunset_2026 | 1.000 |
| boltzmann_universality_2026 | emotional_cycles_2026 | 0.500 |
| emotional_cycles_2026 | floating_wall_2026 | 0.500 |
| emotional_cycles_2026 | iran_war_room_2026 | 0.500 |
| emotional_cycles_2026 | silklink_2026 | 0.500 |
| emotional_cycles_2026 | trillion_bond_rush_2026 | 0.500 |
| emotional_cycles_2026 | world_factbook_sunset_2026 | 0.500 |
| floating_wall_2026 | iran_war_room_2026 | 1.000 |
| floating_wall_2026 | silklink_2026 | 1.000 |
| floating_wall_2026 | trillion_bond_rush_2026 | 1.000 |
| floating_wall_2026 | world_factbook_sunset_2026 | 1.000 |
| iran_war_room_2026 | silklink_2026 | 1.000 |
| iran_war_room_2026 | trillion_bond_rush_2026 | 1.000 |
| iran_war_room_2026 | world_factbook_sunset_2026 | 1.000 |
| silklink_2026 | trillion_bond_rush_2026 | 1.000 |
| silklink_2026 | world_factbook_sunset_2026 | 1.000 |
| trillion_bond_rush_2026 | world_factbook_sunset_2026 | 1.000 |

## Phase 4: Network Impact

| Metric | Baseline | With Inferred | Delta |
|--------|----------|---------------|-------|
| Total edges | 689 | 711 | +22 |
| Connected components | 796 | 789 | -7 |
| Largest component | 39 | 39 | +39-39 |
| Largest as % of corpus | 3.8% | 3.8% | +0.0% |

**No giant component**: Largest component is 39 nodes (3.8% of corpus). Network remains fragmented even with inferred coupling.

## Phase 5: Cross-Domain Bridge Analysis

### Bridge Edges

Inferred edges that connect previously separate components:

| C1 | Type1 | C2 | Type2 | Strength | Bridge? |
|----|-------|----|-------|----------|---------|
| artificial_snow_2026 | unknown | emotional_cycles_2026 | tangled_rope | 0.500 | yes |
| artificial_snow_2026 | unknown | floating_wall_2026 | snare | 1.000 | yes |
| artificial_snow_2026 | unknown | iran_war_room_2026 | snare | 1.000 | yes |
| artificial_snow_2026 | unknown | silklink_2026 | rope | 1.000 | yes |
| artificial_snow_2026 | unknown | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| artificial_snow_2026 | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |
| boltzmann_universality_2026 | rope | emotional_cycles_2026 | tangled_rope | 0.500 | yes |
| emotional_cycles_2026 | tangled_rope | floating_wall_2026 | snare | 0.500 | yes |
| emotional_cycles_2026 | tangled_rope | iran_war_room_2026 | snare | 0.500 | yes |
| emotional_cycles_2026 | tangled_rope | silklink_2026 | rope | 0.500 | yes |
| emotional_cycles_2026 | tangled_rope | trillion_bond_rush_2026 | unknown | 0.500 | yes |
| emotional_cycles_2026 | tangled_rope | world_factbook_sunset_2026 | snare | 0.500 | yes |
| floating_wall_2026 | snare | iran_war_room_2026 | snare | 1.000 | yes |
| floating_wall_2026 | snare | silklink_2026 | rope | 1.000 | yes |
| floating_wall_2026 | snare | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| floating_wall_2026 | snare | world_factbook_sunset_2026 | snare | 1.000 | yes |
| iran_war_room_2026 | snare | silklink_2026 | rope | 1.000 | yes |
| iran_war_room_2026 | snare | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| iran_war_room_2026 | snare | world_factbook_sunset_2026 | snare | 1.000 | yes |
| silklink_2026 | rope | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| silklink_2026 | rope | world_factbook_sunset_2026 | snare | 1.000 | yes |
| trillion_bond_rush_2026 | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |

**22/22 inferred edges are bridges** (connecting previously separate components).

### Component Merges

- Baseline components: 796
- After inferred coupling: 789
- Components merged: 7

### Merged Component Members

Bridge `artificial_snow_2026` -- `emotional_cycles_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `artificial_snow_2026` -- `floating_wall_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `artificial_snow_2026` -- `iran_war_room_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `artificial_snow_2026` -- `silklink_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `artificial_snow_2026` -- `trillion_bond_rush_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `artificial_snow_2026` -- `world_factbook_sunset_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `boltzmann_universality_2026` -- `emotional_cycles_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `emotional_cycles_2026` -- `floating_wall_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `emotional_cycles_2026` -- `iran_war_room_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `emotional_cycles_2026` -- `silklink_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `emotional_cycles_2026` -- `trillion_bond_rush_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `emotional_cycles_2026` -- `world_factbook_sunset_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `floating_wall_2026` -- `iran_war_room_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `floating_wall_2026` -- `silklink_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `floating_wall_2026` -- `trillion_bond_rush_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `floating_wall_2026` -- `world_factbook_sunset_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `silklink_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `trillion_bond_rush_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `world_factbook_sunset_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `silklink_2026` -- `trillion_bond_rush_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `silklink_2026` -- `world_factbook_sunset_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `trillion_bond_rush_2026` -- `world_factbook_sunset_2026` is now in a component of size 8:
  - artificial_snow_2026
  - boltzmann_universality_2026
  - emotional_cycles_2026
  - floating_wall_2026
  - iran_war_room_2026
  - silklink_2026
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

### Type Diversity in Bridged Components

If inferred coupling edges carry contamination, what types are now reachable from each bridged constraint?

Component containing `artificial_snow_2026` and `emotional_cycles_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `artificial_snow_2026` and `floating_wall_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `artificial_snow_2026` and `iran_war_room_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `artificial_snow_2026` and `silklink_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `artificial_snow_2026` and `trillion_bond_rush_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `artificial_snow_2026` and `world_factbook_sunset_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `boltzmann_universality_2026` and `emotional_cycles_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `emotional_cycles_2026` and `floating_wall_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `emotional_cycles_2026` and `iran_war_room_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `emotional_cycles_2026` and `silklink_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `emotional_cycles_2026` and `trillion_bond_rush_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `emotional_cycles_2026` and `world_factbook_sunset_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `floating_wall_2026` and `iran_war_room_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `floating_wall_2026` and `silklink_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `floating_wall_2026` and `trillion_bond_rush_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `floating_wall_2026` and `world_factbook_sunset_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `iran_war_room_2026` and `silklink_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `iran_war_room_2026` and `trillion_bond_rush_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `iran_war_room_2026` and `world_factbook_sunset_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `silklink_2026` and `trillion_bond_rush_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `silklink_2026` and `world_factbook_sunset_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

Component containing `trillion_bond_rush_2026` and `world_factbook_sunset_2026`:
  - rope: 2
  - snare: 3
  - tangled_rope: 1
  - unknown: 2

---

*End of Inferred Coupling Activation Protocol*
