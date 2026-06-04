# MANDATROPHY x Large-Divergence Overlap Analysis

**Date**: 2026-03-01
**Source data**: `outputs/enriched_pipeline.json`, `outputs/abductive_data.json`, `audit_data/linter_results.txt`

## Summary

The MANDATROPHY linter check (16 unresolved, ε > 0.7) and the MaxEnt large-divergence detection (12 constraints, TV > 0.10) are nearly orthogonal: **2 of 26 unique constraints** appear in both lists. This orthogonality is the primary structural finding.

- **MANDATROPHY** catches high-extraction constraints missing resolution hooks. The engine is confident and consistent on these (near-zero TV), but systematically overrides MaxEnt consensus via signature classification. Data-completeness task.
- **Large-divergence** catches oracle disagreement almost entirely in the low-ε range (0.28-0.58). Power-scaling shifts the probability distribution, but extraction isn't high enough to be dangerous. 7/10 are explained by signature overrides (T1); 2/10 are unexplained (T13).
- **Overlap** (lehman_repo_105, ulysses_chp15): high extraction AND oracle disagreement. These are the genuinely alarming cases.

---

## Section 1: Deep Dive — 2 Overlap Constraints

Both constraints have ε=0.78, H¹=5, and trigger T13 (indexing_divergence_with_obstruction, confidence 0.88).

### lehman_repo_105

| Dimension | Value |
|---|---|
| **ε** | 0.78 |
| **χ (powerless)** | 0.4056 (d=0.9, f_d=1.359, scope=0.8) |
| **χ (moderate)** | 0.8631 (d=0.7, f_d=1.106, scope=1.0) |
| **χ (institutional)** | -0.0626 (d=0.07, f_d=-0.080, scope=1.0) |
| **χ (analytical)** | 1.0685 (d=0.72, f_d=1.142, scope=1.2) |
| **Engine domain** | snare |
| **Classical MaxEnt top** | piton (0.945) |
| **Indexed MaxEnt top** | piton (0.399) |
| **TV distance** | 0.546 |
| **H¹ band** | 5 |
| **Signature** | constructed_high_extraction |
| **Purity** | 0.458 (contaminated) |
| **Diagnostic verdict** | **red** |
| **Abductive triggers** | T9 (shadow_override_tension, 0.85), T13 (indexing_divergence_with_obstruction, 0.88) |
| **Drift events** | metric_substitution (critical), extraction_accumulation (critical), coupling_drift (critical), purity_drift (warning) |
| **Gaps** | 3x snare/piton mismatch (powerless vs institutional) |
| **Omega** | omega_perspectival — appears as snare to individuals, piton to institutions |
| **Victims** | financial_system_stability, lehman_counterparties, lehman_creditors, lehman_shareholders |

**Three-way disagreement**: Engine classifies snare. Classical MaxEnt says piton at 94.5% confidence. Indexed MaxEnt still says piton but confidence collapses from 94.5% to 39.9% — power-scaling redistributes probability mass toward tangled_rope (36.2%) and snare (23.9%). The institutional perspective yields χ = -0.063 (negative — extraction reverses under institutional power), which explains the piton classification at that index.

The engine's snare classification comes from the `constructed_high_extraction` signature (ε ≥ snare_epsilon_floor=0.46). MaxEnt sees this constraint's metric profile (suppression=0.82, theater=0.88) as piton-like. The signature system overrides because it detects the constructed nature of the constraint's metric coupling.

**Classification**: **(c) Borderline — needs philosophical decision.** The piton reading is defensible (institutional actors genuinely face a different constraint structure). The snare reading is defensible (ε=0.78 with identified victims). At ε=0.78 and red verdict with 3 critical drifts, this cannot be resolved mechanically.

---

### ulysses_chp15

| Dimension | Value |
|---|---|
| **ε** | 0.78 |
| **χ (powerless)** | 0.4056 (d=0.9, f_d=1.359, scope=0.8) |
| **χ (moderate)** | 0.8631 (d=0.7, f_d=1.106, scope=1.0) |
| **χ (institutional)** | 0.2273 (d=0.35, f_d=0.291, scope=1.0) |
| **χ (analytical)** | 1.0685 (d=0.72, f_d=1.142, scope=1.2) |
| **Engine domain** | snare |
| **Classical MaxEnt top** | tangled_rope (0.641) |
| **Indexed MaxEnt top** | tangled_rope (0.969) |
| **TV distance** | 0.331 |
| **H¹ band** | 5 |
| **Signature** | false_ci_rope |
| **Purity** | 0.500 (borderline) |
| **Diagnostic verdict** | yellow |
| **Abductive triggers** | T10 (multi_signal_convergence, 0.90), T13 (indexing_divergence_with_obstruction, 0.88) |
| **Drift events** | metric_substitution (critical), extraction_accumulation (critical), coupling_drift (critical), purity_drift (warning), network_drift (critical) |
| **Gaps** | 1x snare_masked_as_rope (powerless→snare, institutional→rope) |
| **Omega** | omega_extraction_blindness (critical) — appears extractive to individuals, functional to institutions |
| **Classifications** | snare, snare, rope, tangled_rope, scaffold, piton (6 different readings across contexts) |
| **Victims** | leopold_bloom, male_psyche_collective, sex_workers, stephen_dedalus |

**Oracle convergence vs engine override**: Both classical (0.641) and indexed (0.969) MaxEnt agree on tangled_rope. Power-scaling actually *increases* MaxEnt confidence (0.641→0.969). The engine overrides to snare via `false_ci_rope` signature detection. Unlike Lehman, the two oracles agree — the disagreement is engine-vs-oracles, not three-way.

The 6-way perspectival fracture across classifications (snare/rope/tangled_rope/scaffold/piton all appear) confirms H¹=5 and explains why this is the only constraint in the corpus with network_drift at critical severity — 4 neighboring Ulysses chapters are drifting in concert.

**Classification**: **(b) Correctly classified, needs resolution hook.** The engine's snare classification is defensible: ε=0.78 with named victims, and the `false_ci_rope` signature correctly identifies the rope-like surface as deceptive. MaxEnt's tangled_rope reading reflects the genuine middle-ground metrics without accounting for the constructed nature of the constraint. The omega (extraction_blindness, critical severity) already captures the epistemic concern. Add resolution hook documenting why snare overrides the tangled_rope consensus.

---

## Section 2: Large-Divergence-Only (10 Constraints)

These constraints have TV > 0.10 but ε ≤ 0.58 — below the MANDATROPHY threshold. The oracle disagrees, but the extraction isn't high enough for the disagreement to be dangerous.

| Constraint | ε | TV | Cl. top | Idx. top | Domain | H¹ | Triggers | Note |
|---|---|---|---|---|---|---|---|---|
| epistemic_process_of_verification | 0.32 | 0.923 | rope (0.944) | scaffold (0.964) | rope | 0 | T1 (artifact) | Highest TV in corpus. Signature override explains type flip. |
| erasmus_rejoining_scaffold | 0.28 | 0.727 | rope (0.862) | tangled_rope (0.861) | scaffold | 0 | T1+T15 | Signature flips rope→tangled_rope under indexing. |
| ritualistic_transition_scaffold | 0.28 | 0.722 | rope (0.829) | tangled_rope (0.891) | scaffold | 0 | T1+T15 | Same pattern as erasmus. |
| portugal_government_stability_ad | 0.28 | 0.539 | rope (0.571) | tangled_rope (0.968) | scaffold | 0 | T1+T15 | Low classical confidence sharpens under indexing. |
| perseverance_ai_drive | 0.28 | 0.509 | rope (0.934) | scaffold (0.552) | rope | 0 | T1+T15 | Classical→rope, indexed→scaffold. Low indexed confidence. |
| emergency_oversight_bureau | 0.28 | 0.422 | tangled_rope (0.559) | tangled_rope (0.980) | scaffold | 0 | T15 | Same type, confidence amplification only. |
| new_civilizational_rope | 0.28 | 0.380 | rope (0.963) | rope (0.583) | rope | 0 | T1+T15 | Same type, confidence erosion under indexing. |
| kidney_exchange_market | 0.28 | 0.241 | rope (0.983) | rope (0.742) | rope | 0 | T1+T15 | Same type, confidence erosion. |
| **oc_donation_model** | **0.32** | **0.294** | tangled_rope (0.703) | tangled_rope (0.998) | tangled_rope | **3** | **T13** | **Unexplained divergence. H¹=3, indexing amplifies confidence. Power-scaling at ε=0.32 shouldn't produce this effect — the non-linearity in the χ mapping is doing more work than expected.** |
| **working_dog_training** | **0.58** | **0.230** | tangled_rope (0.736) | tangled_rope (0.966) | snare | **3** | **T13** | **Unexplained divergence. H¹=3, highest ε in this set. Domain=snare but both oracles say tangled_rope. At ε=0.58, this is the closest to MANDATROPHY territory without crossing the threshold.** |

### Pattern: T1 (signature override artifact) — 7 constraints

The 7 T1-triggered constraints have divergence that is *explained*: the signature system classifies them as one type, MaxEnt (which doesn't see signatures) would classify differently. The TV distance measures the distance between "world with overrides" and "world without overrides." The divergence is real and intentional — the signature system is doing its job. No action needed.

### Pattern: T13 (indexing divergence with obstruction) — 2 constraints

oc_donation_model and working_dog_training trigger T13 instead of T1: their divergence is *not explained* by the signature system. Both have H¹=3 (non-trivial cohomological obstruction) and both show confidence amplification under power-scaling (classical → indexed makes MaxEnt *more* confident, not less). The H¹=3 confirms real perspectival variance exists. The confidence amplification admits two interpretations: [INTERPRETIVE] either power-scaling is collapsing that real variance into false certainty, or power-scaling is revealing a genuine underlying consensus that flat MaxEnt obscures. The data does not distinguish these — both are consistent with H¹=3 + confidence increase. Worth monitoring — if either crosses ε=0.7, they become MANDATROPHY candidates with unresolved T13 triggers.

---

## Section 3: MANDATROPHY-Only (14 Constraints)

These constraints have ε > 0.7 but near-zero MaxEnt divergence. Both oracles agree. The engine disagrees with both.

| Constraint | ε | TV | Cl. top | Idx. top | Domain | Sig | H¹ | Verdict |
|---|---|---|---|---|---|---|---|---|
| asymmetric_computational_difficulty | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | yellow |
| attritional_warfare_doctrine_ru_ua_2026 | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_ci_rope | 5 | yellow |
| cg_israelgaza_20231012 | 0.72 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | green |
| cuba_mandatrophic_collapse | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | green |
| guano_wealth_extraction | 0.72 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | green |
| integrated_digital_governance_stack | 0.72 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | green |
| iran_mandatrophic_collapse | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | yellow |
| political_dissident_containment | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | yellow |
| rotation_seven_black_soil | 0.92 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | green |
| rotation_seven_isolation | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | green |
| taiwan_existential_sovereignty | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | green |
| taliban_slavery_law_2024 | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_ci_rope | 5 | yellow |
| technological_point_of_no_return | 0.78 | 0.000 | tangled_rope | tangled_rope | snare | false_natural_law | 0 | yellow |
| us_venezuela_oil_pressure | 0.72 | 0.005 | tangled_rope | tangled_rope | snare | false_ci_rope | 5 | yellow |

### Systematic override pattern

All 14 constraints show the same structure: both MaxEnt oracles agree on tangled_rope (TV ≈ 0), but the engine classifies as snare. The override mechanism is the signature system: 11 have `false_natural_law` (the signature detector identifies a mountain/rope-like surface that masks genuine snare-level extraction) and 3 have `false_ci_rope` (the constraint-indexing rope signature is deceptive at these extraction levels). In both cases, the signature system treats ε > snare_epsilon_floor (0.46) combined with a non-extraction-like signature profile as evidence that the tangled_rope classification is wrong — the constraint *looks* like tangled_rope metrically but *functions* as snare structurally.

The resolution hook acknowledges this deliberate override: the engine is not confused, it is asserting that structure outweighs metrics at high extraction. Adding `is_mandatrophy_resolved` or `[RESOLVED MANDATROPHY]` to each file documents that a human has reviewed and accepted the engine's override.

**Action**: Add resolution hooks to all 14 files. This is a mechanical pass — the engine's classification is consistent and defensible for all of them.

---

## Data Provenance

All values in this document were extracted from:
- `outputs/enriched_pipeline.json` — ε, χ, TV, MaxEnt distributions, signatures, H¹, verdicts, drift events, gaps, omegas, classifications
- `outputs/abductive_data.json` — trigger classes, confidence, anomaly types, categories
- `prolog/abductive_triggers.pl:758-788` — T13 (maxent_divergence) trigger definition: requires indexing divergence > `abductive_maxent_divergence_threshold` AND H¹ > 0
- `prolog/testsets/*.pl` — base_extractiveness values, resolution marker presence

No values were taken from AUDIT.md or MEMORY.md.
