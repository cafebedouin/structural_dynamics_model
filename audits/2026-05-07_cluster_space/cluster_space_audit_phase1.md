# Cluster-Space Audit — Phase 1 Results

Sample: 265 constraints | 34980 pairs | seed 42

## Calibration Results

- **Mountain–Mountain metric clustering** → **PASS**
- **Snare–Snare metric clustering** → **PASS**
- **advice anchor: high metric / low idea** → **PASS**
- **false_mountain metric/semantic split** → **NOT ASSESSED**

Genuine-mountain within-type metric distribution: mean=0.846, std=0.141, range=[0.697, 1.000]
Snare within-type metric mean: 0.925

- **false_mountain_naturalization**: claimed_type=snare; this constraint is a meta-constraint describing the false-mountain phenomenon as an object of analysis, not a candidate instance of it. The corpus contains both object-level constraints (things in the world) and meta-level constraints (the apparatus's own concepts). The spec assumed the latter would manifest as metric mountains; they don't and shouldn't.
- **false_mountain_persistence**: claimed_type=tangled_rope; this constraint is a meta-constraint describing the false-mountain phenomenon as an object of analysis, not a candidate instance of it. The corpus contains both object-level constraints (things in the world) and meta-level constraints (the apparatus's own concepts). The spec assumed the latter would manifest as metric mountains; they don't and shouldn't.

Anchor 1 (`advice_as_dangerous_gift` ↔ `central_bank_independence`): met=0.999, sem=0.061, ben=0.000

**Overall calibration: PASS — Phase 1 results follow**

## A. Spearman Correlation Matrix (5×5)

|  | Observer-space | Beneficiary (Jaccard) | Coupling (BFS decay) | Semantic (embedding) | Metric-space |
| --- | --- | --- | --- | --- | --- |
| Observer-space | 1.000 | 0.015 | 0.009 | 0.196 | 0.776 |
| Beneficiary (Jaccard) | 0.015 | 1.000 | -0.000 | 0.025 | 0.019 |
| Coupling (BFS decay) | 0.009 | -0.000 | 1.000 | 0.012 | 0.001 |
| Semantic (embedding) | 0.196 | 0.025 | 0.012 | 1.000 | 0.220 |
| Metric-space | 0.776 | 0.019 | 0.001 | 0.220 | 1.000 |

### Idea-space sub-measure cross-correlations

- Beneficiary (Jaccard) × Coupling (BFS decay): ρ = -0.000 ← **low: idea space internally fractured**
- Beneficiary (Jaccard) × Semantic (embedding): ρ = 0.025 ← **low: idea space internally fractured**
- Coupling (BFS decay) × Semantic (embedding): ρ = 0.012 ← **low: idea space internally fractured**

Idea-space aggregation benchmarks (all three sub-measures):
_(best-of-three, mean, and max aggregations are reported in the JSON output)_

## B. Population Matrix (Diagnostic Cells)

| Cell | Count | Fraction |
| --- | --- | --- |
| Orphan invisibility (met Q4 × idea Q1) | 1732 | 5.0% |
| Lensing zone (idea Q4 × obs Q1) | 2870 | 8.2% |
| Cross-cutting frame (obs Q4 × idea Q1) | 0 | 0.0% |
| Total pairs | 34980 | 100% |

Quartile thresholds (idea = best-of-three): obs=[0.000, 0.250, 1.000] | met=[0.113, 0.367, 0.925] | idea=[0.011, 0.066, 0.132]

## C. Top Outlier Pairs — Orphan Invisibility (met Q4 × idea Q1)

| id1 (type) | id2 (type) | obs | ben | coup | sem | met |
| --- | --- | --- | --- | --- | --- | --- |
| pancreatic_cancer_lethality_v1 (mountain) | selective_attention_constraint (mountain) | 1.000 | 0.000 | 0.000 | -0.054 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | southwestern_megadrought_1130_1180 (mountain) | 1.000 | 0.000 | 0.000 | -0.008 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | picard_lindelof_existence (mountain) | 1.000 | 0.000 | 0.000 | -0.008 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | angular_momentum_conservation (mountain) | 1.000 | 0.000 | 0.000 | -0.003 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | substrate_as_unrecognized_archive (mountain) | 1.000 | 0.000 | 0.000 | -0.030 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | riemann_zeta_function_values (mountain) | 1.000 | 0.000 | 0.000 | -0.073 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | suslin_hypothesis (mountain) | 1.000 | 0.000 | 0.000 | -0.025 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | quotient_group_properties (mountain) | 1.000 | 0.000 | 0.000 | -0.052 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | planar_graph_embeddability (mountain) | 1.000 | 0.000 | 0.000 | -0.052 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | square_cube_law (mountain) | 1.000 | 0.000 | 0.000 | -0.042 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | bolzano_weierstrass_theorem (mountain) | 1.000 | 0.000 | 0.000 | -0.029 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | noethers_theorem_symmetry (mountain) | 1.000 | 0.000 | 0.000 | -0.082 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | divergence_theorem (mountain) | 1.000 | 0.000 | 0.000 | -0.007 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | nonstandard_models_of_arithmetic (mountain) | 1.000 | 0.000 | 0.000 | -0.102 | 1.000 |
| pancreatic_cancer_lethality_v1 (mountain) | light_speed_latency (mountain) | 1.000 | 0.000 | 0.000 | -0.081 | 1.000 |
| selective_attention_constraint (mountain) | southwestern_megadrought_1130_1180 (mountain) | 1.000 | 0.000 | 0.000 | -0.092 | 1.000 |
| selective_attention_constraint (mountain) | picard_lindelof_existence (mountain) | 1.000 | 0.000 | 0.000 | -0.009 | 1.000 |
| selective_attention_constraint (mountain) | cantor_set_topology (mountain) | 1.000 | 0.000 | 0.000 | -0.071 | 1.000 |
| selective_attention_constraint (mountain) | riemann_zeta_function_values (mountain) | 1.000 | 0.000 | 0.000 | -0.073 | 1.000 |
| selective_attention_constraint (mountain) | halting_problem (mountain) | 1.000 | 0.000 | 0.000 | -0.005 | 1.000 |

## C. Top Outlier Pairs — Lensing Zone (idea Q4 × obs Q1)

| id1 (type) | id2 (type) | obs | ben | coup | sem | met |
| --- | --- | --- | --- | --- | --- | --- |
| connectome_sufficiency (tangled_rope) | scaling_feasibility (rope) | 0.000 | 0.000 | 0.000 | 0.742 | 0.160 |
| eu_russian_asset_freeze_2025 (tangled_rope) | russian_asset_freezing (tangled_rope) | 0.000 | 0.000 | 0.000 | 0.716 | 0.684 |
| prisoners_dilemma_equilibrium (mountain) | nash_equilibrium_coordination (rope) | 0.000 | 0.000 | 0.000 | 0.681 | 0.068 |
| program_verification_hardness (mountain) | iterative_algorithm_correctness (rope) | 0.000 | 0.000 | 0.000 | 0.557 | 0.141 |
| finite_group_classification (mountain) | collective_action_problem (rope) | 0.000 | 0.000 | 0.000 | 0.461 | 0.100 |
| vehicle_routing_problem_distance_symmetry (mountain) | quadratic_assignment_symmetry_handling (rope) | 0.000 | 0.000 | 0.000 | 0.460 | 0.090 |
| parable_as_transmission_layer (mountain) | epistemic_commons_contamination (tangled_rope) | 0.000 | 0.000 | 0.000 | 0.454 | 0.123 |
| consensus_governance_capture (tangled_rope) | collective_action_problem_general (rope) | 0.000 | 0.000 | 0.000 | 0.444 | 0.640 |
| other_peoples_troubles_2026 (tangled_rope) | complicity_through_competence (snare) | 0.000 | 0.000 | 0.000 | 0.431 | 0.454 |
| repeat_player_structural_advantage (mountain) | nash_equilibrium_coordination (rope) | 0.000 | 0.000 | 0.000 | 0.423 | 0.179 |
| geopolitical_compute_dominance (tangled_rope) | capability_overhang (snare) | 0.000 | 0.000 | 0.000 | 0.422 | 0.512 |
| prisoners_dilemma_equilibrium (mountain) | coordination_game_standard (rope) | 0.000 | 0.000 | 0.000 | 0.416 | 0.508 |
| program_verification_hardness (mountain) | capability_acceleration_outpacing_safety (snare) | 0.000 | 0.000 | 0.000 | 0.413 | 0.200 |
| genetic_information_storage (mountain) | genetic_algorithms_evolution (rope) | 0.000 | 0.000 | 0.000 | 0.411 | 0.036 |
| rices_theorem_undecidability (mountain) | ontological_friction_resolution (tangled_rope) | 0.000 | 0.000 | 0.000 | 0.401 | 0.141 |
| lyapunov_exponent_computation (mountain) | cryptocurrency_velocity_dynamics (tangled_rope) | 0.000 | 0.000 | 0.000 | 0.400 | 0.192 |
| eu_russian_asset_freeze_2025 (tangled_rope) | new_start_expiration (snare) | 0.000 | 0.000 | 0.000 | 0.398 | 0.734 |
| consensus_governance_capture (tangled_rope) | collective_action_problem (rope) | 0.000 | 0.000 | 0.000 | 0.396 | 0.637 |
| quantum_entanglement_swapping (mountain) | quadratic_assignment_symmetry_handling (rope) | 0.000 | 0.000 | 0.000 | 0.393 | 0.090 |
| prisoners_dilemma_equilibrium (mountain) | collective_action_problem (rope) | 0.000 | 0.000 | 0.000 | 0.392 | 0.495 |

## C. Top Outlier Pairs — Cross-Cutting Frame (obs Q4 × idea Q1)

_No pairs in this cell._

## D. Stratification Check (Within-Type Similarities)

| Type | N pairs | Observer-space mean | Beneficiary (Jaccard) mean | Coupling (BFS decay) mean | Semantic (embedding) mean | Metric-space mean |
| --- | --- | --- | --- | --- | --- | --- |
| mountain | 3160 | 0.988 | 0.000 | 0.000 | 0.101 | 0.985 |
| piton | 91 | 0.527 | 0.004 | 0.000 | 0.127 | 0.665 |
| rope | 435 | 0.840 | 0.000 | 0.001 | 0.113 | 0.610 |
| scaffold | 55 | 0.955 | 0.000 | 0.000 | 0.139 | 0.782 |
| snare | 1225 | 0.551 | 0.000 | 0.000 | 0.088 | 0.783 |
| tangled_rope | 3160 | 0.675 | 0.000 | 0.000 | 0.112 | 0.829 |

**Naturalized-mountain signal present**: Mountain within-type metric similarity (0.985) > semantic similarity (0.101) — Δ = 0.884

## Phase 3 — Structural Observations

- Constraints excluded for missing human_readable: 0 (not in sample)
- Constraints with missing perspectives: 0
- Sample strata: mountain=80, tangled_rope=80, snare=50, rope=30, scaffold=11, piton=14
- Stratum shortfalls: none
- Constraints not in neighbors.json: 182/265 (68.7%). These receive zero coupling similarity to all others.
- Constraints with empty beneficiary lists: 86/265 (32.5%). Pairs where both are empty receive Jaccard=0 (not NaN).
- Pairs with zero coupling similarity: 34978/34980 (100.0%). Coupling is extremely sparse; Q1 boundary is likely 0.
- Pairs with zero beneficiary Jaccard: 34959/34980 (99.9%).
- Advice anchor 1 method: **network** — sibling: `central_bank_independence`. Check 3 was assessed.
- Calibration check 4 (false_mountain anchors) is NOT ASSESSED. `false_mountain_naturalization` (claimed_type=snare) and `false_mountain_persistence` (claimed_type=tangled_rope) are **meta-constraints** — they describe the false-mountain phenomenon as an object of analysis ('false mountains naturalize over time' is itself an extractive coordination pattern), not candidate instances of it. The spec assumed these named constraints would manifest as metric mountains; they don't and shouldn't. This surfaces a category distinction in the corpus: it contains both object-level constraints (things in the world) and meta-level constraints (the apparatus's own concepts as objects of analysis). Distinguishing these two uses is an open structural question for the corpus. Finding actual false-mountain candidate instances — mountains in metric space that have low semantic affinity with the mountain cluster — is deferred to output D, which tests the naturalized-mountain hypothesis directly across the full 80-mountain stratum.
- Semantic embedding field: `human_readable`. Alternative `topic_domain` would cluster by domain label rather than content — expected to reduce within-domain variance and inflate between-domain gaps, likely increasing sem×obs and sem×met correlations.
- Coupling decay: λ=1.0. Under λ=2 (slower decay), 2–3-hop pairs rise from ~0.05–0.14 to ~0.22–0.37, increasing non-zero coupling fraction and likely raising coup×met and coup×obs correlations.
- Signature flags are one-hot in the metric vector. Missing or unknown signature maps to all-zero flags — conflates 'no signature' with 'unknown signature.'
- Quartile boundaries are data-driven. High zero-inflation in coupling and beneficiary measures compresses Q1 boundaries toward zero, making orphan-invisibility and cross-cutting-frame cell sizes sensitive to zero-fraction choice.