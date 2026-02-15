# Deferential Realism: Logic Thresholds Registry

**Version 4.0**  
**Purpose:** Single source of truth for all system parameters  
**Source:** config.pl (lines 71-410)  
**Last Updated:** February 2026

---

## Overview

This document provides the **canonical threshold values** for all parameters in the Deferential Realism logic system. These values are:

- **Authoritative**: Implementation must use these exact values from config.pl
- **Calibrated**: Derived from 691-constraint corpus analysis
- **Provisional**: Subject to refinement through validation and cross-cultural replication
- **Documented**: Each parameter includes stage, logic, and implementation significance

**Critical Principle:** Changes flow spec â†’ registry â†’ implementation, never backward.

---

## Table of Contents

1. Power Modifiers (Ï€) â€” How extraction scales by power position
2. Scope Modifiers (Ïƒ) â€” How scope affects verification difficulty
3. Classification Gates â€” Type threshold values (Mountain, Rope, Snare, etc.)
4. Structural Signatures â€” Natural Law, Coordination Scaffold, Constructed
5. Boltzmann Compliance â€” Independence testing for natural law claims
6. Purity Scoring â€” Coordination health measurement
7. Network Dynamics â€” Contamination propagation parameters
8. Lifecycle Drift â€” Threshold values for detecting degradation
9. Action Layer â€” Energy costs and decision gates
10. Defaults & Meta â€” Fallback values

---

## 1. Power Modifiers (Ï€)

**Formula:** Ï‡ = Îµ Ã— Ï€(P) Ã— Ïƒ(S)  
**Purpose:** Determines how much base extraction is "felt" by agent at power position P

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `power_modifier_powerless` | **1.5** | 1-6 | Extraction amplified â€” powerless agents bear full cost |
| `power_modifier_moderate` | **1.0** | 1-6 | Baseline â€” default extraction experience |
| `power_modifier_powerful` | **0.6** | 1-6 | Extraction reduced â€” powerful agents can deflect costs |
| `power_modifier_organized` | **0.4** | 1-6 | Shared burden â€” collective action distributes costs |
| `power_modifier_institutional` | **-0.2** | 1-6 | **Net beneficiary** â€” institutions extract more than they pay |
| `power_modifier_analytical` | **1.15** | 7 | Degeneracy-breaking value â€” detects extraction moderate agents normalize |

**Key Notes:**
- **Ï€(analytical) = 1.15**: Chosen specifically to break moderate-analytical degeneracy. At 1.0, analytical and moderate produced identical Ï‡ values. At 1.15, 93 corpus constraints show "only analyst catches snare" pattern.
- **Ï€(institutional) = -0.2**: Negative value represents net extraction from system. Handle sign carefully in implementations.
- **Calibration source**: 691-constraint corpus (2024-2026)
- **Known limitation**: Values are Western-biased, require non-WEIRD validation

---

## 2. Scope Modifiers (Ïƒ)

**Formula:** Ï‡ = Îµ Ã— Ï€(P) Ã— Ïƒ(S)  
**Purpose:** Models verification difficulty and hidden extraction at larger scales

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `scope_modifier_local` | **0.8** | 1-6 | Easy verification â†’ extraction dampened |
| `scope_modifier_regional` | **0.9** | 1-6 | Easier verification |
| `scope_modifier_national` | **1.0** | 1-6 | Baseline |
| `scope_modifier_continental` | **1.1** | 1-6 | Harder verification â†’ extraction amplified |
| `scope_modifier_global` | **1.2** | 1-6 | Hardest verification â†’ maximum extraction hiding |
| `scope_modifier_universal` | **1.0** | 1-6 | Neutral â€” natural laws don't scale with scope |

**Key Notes:**
- **Ïƒ(universal) = 1.0**: Natural laws are scope-invariant, hence neutral modifier
- **Ïƒ(global) = 1.2**: Maximum amplification reflects Dunbar-number constraints on verification at planetary scale
- **Rationale**: Larger scope = more participants = harder to verify claims = easier to hide extraction

---

## 3. Classification Gates

### 3a. Mountain (â– C[I])

**Formal:** `â– C[I] â†” Îµ(C) â‰¤ 0.15 âˆ§ Supp(C) â‰¤ 0.05 âˆ§ Immutable(C, I.T, I.E)`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `mountain_extractiveness_max` | **0.15** | 1-6 | Max base extraction for natural law (coordination floor) |
| `mountain_suppression_ceiling` | **0.05** | 1-6 | Noise floor â€” no enforcement needed |
| `mountain_extractiveness_min` | **0.0** | 1-6 | Theoretical minimum (unused in practice) |

**Implementation:** `classify_from_metrics/6` line 2946 (drl_core.pl)  
**Structural Gate:** [Requires Boltzmann Compliance for NL Signature. See logic_extensions.md §1.3]

**Mountain Validity Constraint (empirically confirmed, February 2026):**

Three threshold rules are individually sufficient to invalidate Mountain classification. The corpus audit confirmed these as bright-line rules — every file violating any of them was correctly flagged as a false Mountain (96 of 594 files with direct definitional violations).

```
Mountain_valid(C) ↔ ε(C) ≤ 0.15 ∧ TR(C) ≤ 0.10 ∧ ¬requires_active_enforcement(C)
```

| Rule | Threshold | Rationale |
|------|-----------|-----------|
| Extraction ceiling | ε ≤ 0.15 | Natural law does not extract from subjects |
| Theater ratio ceiling | TR ≤ 0.10 | Natural law needs no legitimation theater |
| No active enforcement | ¬requires_active_enforcement | Natural law self-enforces through reality, not institutions |

**Recommendation:** Formalize as meta-engine rule rather than classification engine rule, to preserve the generating LLM's honest read as data while catching definitional violations mechanically. See `limitations.md` for full audit findings.

---

### 3b. Rope (âŠžC[I])

**Formal:** `âŠžC[I] â†” Ï‡(C, I.P, I.S) â‰¤ 0.35 âˆ§ Îµ(C) â‰¤ 0.15 âˆ§ Changeable(C, I.T, I.E)`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `rope_chi_ceiling` | **0.35** | 1-6 | Max power-scaled extraction for pure coordination |
| `rope_epsilon_ceiling` | **0.15** | 1-6 | Max base extraction (same as Mountain â€” coordination floor) |
| `rope_suppression_ceiling` | **0.16** | 1-6 | Base suppression ceiling for pure coordination |
| `rope_extractiveness_min` | **0.0** | 1-6 | Theoretical minimum |

**Implementation:** `classify_from_metrics/6` line 2970 (drl_core.pl)  
**Structural Gate:** [Can be certified as CI_Rope if Boltzmann-compliant. See logic_extensions.md Â§1.4]

**Key Note:** Dual threshold (Ï‡ AND Îµ) prevents high-power agents from misclassifying high-Îµ constraints as Ropes.

---

### 3c. Snare (âŠ C[I])

**Formal:** `âŠ C[I] â†” Ï‡(C, I.P, I.S) â‰¥ 0.66 âˆ§ Îµ(C) â‰¥ 0.46 âˆ§ Supp(C) â‰¥ 0.60 âˆ§ Changeable(C, I.T, I.E)`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `snare_chi_floor` | **0.66** | 1-6 | Min power-scaled extraction for pure extraction |
| `snare_epsilon_floor` | **0.46** | 1-6 | Min base extraction (prevents powerless-only misclassification) |
| `snare_suppression_floor` | **0.60** | 1-6 | Requires active enforcement |
| `snare_extraction_ceil` | **1.00** | 1-6 | Maximum possible extraction |
| `snare_load_bearing_threshold` | **0.70** | 1-6 | Above â†’ load-bearing snare (Theorem 3: cutting causes collapse) |

**Implementation:** `classify_from_metrics/6` line 2953 (drl_core.pl)  
**Structural Gate:** [Nonsensical coupling is evidence of snare. See logic_extensions.md Â§1.6]

**Key Note:** Triple threshold (Ï‡ AND Îµ AND Supp) is strictest gate â€” prevents false positives.

---

### 3d. Tangled Rope (âŠžâŠ C[I])

**Formal:** `âŠžâŠ C[I] â†” 0.40 â‰¤ Ï‡(C, I.P, I.S) â‰¤ 0.90 âˆ§ Îµ(C) â‰¥ 0.50 âˆ§ Supp(C) â‰¥ 0.40 âˆ§ Enforce(C) âˆ§ Coord(C) âˆ§ Asymmetric(C)`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `tangled_rope_chi_floor` | **0.40** | 1-6 | Min power-scaled extraction for hybrid |
| `tangled_rope_chi_ceil` | **0.90** | 1-6 | Max power-scaled extraction (overlaps with snare) |
| `tangled_rope_epsilon_floor` | **0.50** | 1-6 | Min base extraction for hybrid classification |
| `tangled_rope_suppression_floor` | **0.40** | 1-6 | Requires active enforcement |
| `tangled_rope_suppression_ceil` | **1.00** | 1-6 | Maximum suppression |

**Implementation:** `classify_from_metrics/6` line 2977 (drl_core.pl)

**Key Notes:**
- **Empirical prevalence:** ~36% of analyzed constraints (most common type)
- **Structural requirements:** Requires Coord(C) âˆ§ Enforce(C) âˆ§ Asymmetric(C)
- **Overlap with snare:** A constraint can be tangled at moderate power, snare at powerless power
- **CRITICAL:** Îµ threshold (0.50) appears higher than some examples (e.g., carbon credits at 0.40). This needs reconciliation â€” see Known Issues.

---

### 3e. Scaffold (âŠ¡C[I])

**Formal:** `âŠ¡C[I] â†” Ï‡(C, I.P, I.S) â‰¤ 0.30 âˆ§ Coord(C) âˆ§ Sunset(C) âˆ§ Theater(C) â‰¤ 0.70`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `scaffold_extraction_ceil` | **0.30** | 1-6 | Max extraction for temporary support |

**Implementation:** `classify_from_metrics/6` line 2962 (drl_core.pl)

**Key Notes:**
- **No immutability gate**: Scaffolds are inherently temporary
- **Theater ceiling (0.70)**: Ensures constraint does real work, not mere performance
- **Sunset requirement**: Built-in expiration distinguishes from Rope

---

### 3f. Piton (âŠŸC[I])

**Formal:** `âŠŸC[I] â†” Ï‡(C, I.P, I.S) â‰¤ 0.10 âˆ§ Îµ(C) > 0.10 âˆ§ Theater(C) â‰¥ 0.70`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `piton_extraction_ceiling` | **0.10** | 1-6 | Minimal effective extraction for anyone |
| `piton_epsilon_floor` | **0.10** | 1-6 | Still costs energy to maintain (distinguishes from Îµ=0) |
| `piton_theater_floor` | **0.70** | 1-6 | High theater ratio distinguishes from low-extraction ropes |

**Implementation:** `classify_from_metrics/6` line 2990 (drl_core.pl)

**Key Note:** Degradation state â€” extraction dried up but structure persists. Theater ratio (performance/substance) is diagnostic.

---

## 4. Structural Signatures

**Purpose:** Detect constraint ORIGIN (natural vs coordination vs constructed) beyond metrics

### 4a. Natural Law (NL)

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `natural_law_collapse_min` | **0.85** | 3 | Extreme universal inaccessibility |
| `natural_law_suppression_max` | **0.15** | 3 | No enforcement needed |
| `natural_law_resistance_max` | **0.15** | 3 | Cannot be resisted |

**Implementation:** `structural_signatures.pl` natural_law_signature/1

**Enhancement (Stage 7):** NL claims must pass `boltzmann_invariant_mountain/2` test:
- Factorization across Power Ã— Scope
- Scope invariance
- No excess extraction
- Boltzmann compliance

---

### 4b. Coordination Scaffold (CS)

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `coordination_collapse_min` | **0.85** | 3 | Universal adoption achieved |
| `coordination_suppression_max` | **0.15** | 3 | Voluntary compliance |
| `coordination_resistance_max` | **0.15** | 3 | Minimal opposition |

**Implementation:** `structural_signatures.pl` coordination_scaffold_signature/1

**Key Difference from NL:** Alternatives existed (path-dependent), but this path won.

---

### 4c. Constructed Constraint (CC)

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `constructed_suppression_min` | **0.20** | 3 | Requires enforcement |
| `constructed_resistance_min` | **0.20** | 3 | Faces opposition |
| `constructed_beneficiary_min` | **2** | 3 | Asymmetric gains threshold (count of beneficiaries) |

**Implementation:** `structural_signatures.pl` constructed_constraint_signature/1

**Sub-categories:** CC_low, CC_mid, CC_high based on extraction levels

---

### 4d. Isomorphism Detection

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `isomorphism_threshold` | **0.85** | 6 | Min similarity score for fingerprint matching |

**Implementation:** `isomorphism_engine.pl`

**Usage:** Cross-domain pattern matching via logical fingerprints

---

## 5. Boltzmann Compliance (Stage 7)

**Purpose:** Natural laws must factorize across index dimensions â€” independence test for NL claims

### 5a. Coupling Detection

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `boltzmann_coupling_threshold` | **0.15** | 7 | Max allowable coupling score for Boltzmann compliance |
| `boltzmann_coupling_strong_threshold` | **0.40** | 7 | Above â†’ "strong coupling" classification |
| `boltzmann_factorization_tolerance` | **0.10** | 7 | Relative error margin for Ï‡(P,S) â‰ˆ f(P)Ã—g(S) test |
| `boltzmann_min_classifications` | **3** | 7 | Min indexed classifications for reliable test (epistemic access) |

**Implementation:** `structural_signatures.pl` boltzmann_compliant/2, cross_index_coupling/2

**Key Insight:** If Ï‡ doesn't factorize â†’ constraint couples independent variables â†’ constructed, not natural.

---

### 5b. Complexity Offsets

**Purpose:** Raise coupling threshold for inherently complex coordination types

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `complexity_offset_information_standard` | **0.00** | 7 | Simple naming conventions (no offset) |
| `complexity_offset_resource_allocation` | **0.05** | 7 | Markets, allocation mechanisms |
| `complexity_offset_enforcement_mechanism` | **0.08** | 7 | Legal systems, governance |
| `complexity_offset_global_infrastructure` | **0.15** | 7 | Power grids, internet protocols |
| `complexity_offset_default` | **0.00** | 7 | Fallback |

**Formula:** effective_threshold = boltzmann_coupling_threshold + complexity_offset

**Rationale:** Global power grid MUST couple more dimensions than "drive on right" convention.

---

### 5c. Boltzmann Floor (Price of Anarchy)

**Purpose:** Minimum extraction inherent to each coordination type

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `boltzmann_floor_information_standard` | **0.02** | 7 | Minimal coordination overhead (e.g., UTF-8) |
| `boltzmann_floor_resource_allocation` | **0.15** | 7 | Market mechanisms have inherent costs |
| `boltzmann_floor_enforcement_mechanism` | **0.10** | 7 | Legal systems require enforcement overhead |
| `boltzmann_floor_global_infrastructure` | **0.20** | 7 | Planetary-scale coordination is expensive |
| `boltzmann_floor_default` | **0.05** | 7 | Fallback |

**Implementation:** `structural_signatures.pl` excess_extraction/2

**Formula:** Excess = Îµ(C) - BoltzmannFloor(coordination_type)

**Key Insight:** Extraction above floor = extractive overhead (PoA excess), not necessary coordination cost.

**Note:** Provisional values â€” require corpus calibration.

---

### 5d. Reformability

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `reformability_high_threshold` | **0.70** | 7 | Above â†’ "highly reformable" |
| `reformability_low_threshold` | **0.30** | 7 | Below â†’ "low reformability" |

**Implementation:** `drl_modal_logic.pl` reformability_score/3

**Formula:** 30% separability + 40% coupling topology + 30% excess extraction

---

### 5e. Drift Detection

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `boltzmann_floor_drift_threshold` | **0.05** | 7 | Min floor increase â†' drift event (Type 9) |

> **Note (February 2026):** `coupling_drift_threshold` (0.10) was removed â€" it was an orphan parameter never wired into any code. `detect_coupling_drift` uses `boltzmann_coupling_threshold` (0.25) as the actual coupling gate.

**Implementation:** `drl_lifecycle.pl` detect_coupling_drift/3, boltzmann_floor_drift/2

**Purpose:** Distinguish necessary complexity increase from extractive complexity increase.

---

## 6. Purity Scoring (Stage 7)

**Purpose:** Structural integrity measurement â€” continuous health metric

### 6a. Purity Score Components

**Formula:**
```
purity_score = 0.30 Ã— (1.0 - coupling_score)
             + 0.25 Ã— scope_invariance
             + 0.25 Ã— coupling_cleanliness
             + 0.20 Ã— excess_extraction_decay
```

**Weights:**
- **30%** factorization (Boltzmann compliance)
- **25%** scope invariance (classification stable across scopes)
- **25%** coupling cleanliness (no nonsensical coupling)
- **20%** excess extraction decay (proximity to Boltzmann floor)

**Zones:**
- **pristine** (â‰¥ 0.9)
- **sound** (â‰¥ 0.7)
- **borderline** (â‰¥ 0.5)
- **contaminated** (â‰¥ 0.3)
- **degraded** (< 0.3)

**Sentinel:** -1.0 = insufficient epistemic data

---

### 6b. Action Gates

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `purity_action_sound_floor` | **0.70** | 7 | Below â†’ monitor purity |
| `purity_action_escalation_floor` | **0.50** | 7 | Below â†’ escalate action urgency |
| `purity_action_degraded_floor` | **0.30** | 7 | Below â†’ action type override (cut vs reform) |
| `purity_surgical_reform_gate` | **0.30** | 7 | **Min purity for surgical reform** â€” below this, reform fails |
| `purity_scaffold_health_gate` | **0.50** | 7 | Min scaffold purity for safe transition |

**Implementation:** `drl_modal_logic.pl` purity_qualified_action/4, action_composition_gate/3

**Key Insight:** Degraded constraints (< 0.30) block reform â€” transition to Cut/Exit.

---

### 6c. Energy Scaling

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `purity_energy_max_multiplier` | **3.0** | 7 | **Cap on energy cost scaling** â€” max overhead for fixing degraded system |

**Implementation:** `drl_modal_logic.pl` purity_adjusted_energy/4

**Formula:** Energy_cost Ã— multiplier(purity)

**Example:** Reform at purity 0.31 â†’ multiplier = 2.16Ã— (near cap)

**Rationale:** Fixing rotten systems is harder than building new ones.

---

## 7. Network Dynamics (Stage 8)

**Purpose:** Contamination propagation through constraint networks

### 7a. Network Discovery

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `network_coupling_threshold` | **0.50** | 8 | Min inferred coupling for network edge |
| `network_shared_agent_min` | **1** | 8 | Min shared agents (beneficiary/victim) for edge |

**Implementation:** `drl_modal_logic.pl` constraint_neighbors/3

---

### 7b. Contamination Propagation

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `purity_contamination_cap` | **0.30** | 8 | **Max purity reduction per edge** (prevents catastrophic drops) |
| `purity_attenuation_factor` | **0.50** | 8 | **Edge strength scaling** (contamination loses 50% per hop) |
| `purity_contamination_source_floor` | **0.50** | 8 | Below â†’ active contamination source |

**Implementation:** `drl_modal_logic.pl` effective_purity/4, purity_contamination_pressure/4

**Key Rule:** Downward-only contamination (Snare â†’ Rope, not vice versa)

---

### 7c. Type Contamination Strength

**Purpose:** Different types have different contamination potency

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `contamination_strength_snare` | **1.0** | 8 | Maximum contamination (pure extraction) |
| `contamination_strength_piton` | **0.8** | 8 | High contamination (degraded state) |
| `contamination_strength_tangled_rope` | **0.5** | 8 | Moderate contamination (hybrid) |
| `contamination_strength_scaffold` | **0.2** | 8 | Low contamination (temporary support) |
| `contamination_strength_rope` | **0.1** | 8 | Minimal contamination (pure coordination) |
| `contamination_strength_mountain` | **0.0** | 8 | **Mountains don't contaminate** (natural laws) |

**Implementation:** `drl_modal_logic.pl` type_contamination_strength/2

---

### 7d. Network Metrics

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `network_contamination_risk_threshold` | **2** | 8 | Low-purity neighbors â†’ "at_risk" classification |
| `network_cluster_degraded_floor` | **0.40** | 8 | Below â†’ cluster classified as "degraded" |

**Implementation:** `drl_modal_logic.pl` network_purity_metrics/2

---

## 8. Network Drift Dynamics (Stage 9)

**Purpose:** How purity drift propagates over time through networks

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `network_drift_velocity_threshold` | **0.01** | 9 | Min effective purity drift per year â†’ network drift event |
| `network_hub_degree_threshold` | **3** | 9 | Neighbors required for "hub" â†’ severity escalation |
| `network_cascade_count_threshold` | **3** | 9 | Drifting constraints â†’ "cascading" network classification |
| `network_drift_hub_escalation` | **1** | 9 | 1=enable hub-based severity escalation (boolean flag) |

**Implementation:** `drl_lifecycle.pl` network_drift_velocity/4, cascade_prediction/3, network_stability_assessment/2

**Key Insight:** Constraints can degrade due to neighbor drift even if own metrics stable (induced degradation).

---

## 9. Lifecycle Drift Thresholds

**Purpose:** Additional parameters for drift event detection

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `system_gradient_threshold` | **0.01** | 2 | Min change â†’ non-stable system |
| `system_gradient_strong_threshold` | **1.00** | 2 | Above â†’ "strong" intent classification |

**Implementation:** `drl_lifecycle.pl` + `intent_engine.pl`

---

## 10. Defaults & Meta-Parameters

### 10a. Default Values

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `default_extractiveness` | **0.10** | 1-6 | Fallback when Îµ unmeasured |
| `default_suppression` | **0.10** | 1-6 | Fallback when Supp unmeasured |
| `default_theater` | **0.0** | 1-6 | Fallback when theater unmeasured |

---

### 10b. Data Quality

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `data_high_threshold` | **0.95** | 2 | Above â†’ "high confidence" |
| `data_medium_threshold` | **0.75** | 2 | Above â†’ "medium confidence" |

**Implementation:** `data_validation.pl`

---

### 10c. Coalition Modeling

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `critical_mass_threshold` | **3** | 6 | Shared snare victims â†’ organized power modifier |

**Implementation:** `constraint_indexing.pl`

**Rationale:** 3+ victims can coordinate â†’ shift from powerless to organized.

---

## Implementation Notes

### Priority Ordering (classify_from_metrics/6)
```
Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > unknown
```

### Two-Regime Architecture
1. **Metrics-first** (`drl_core.pl`): Uses thresholds from this registry
2. **Signature-override** (`structural_signatures.pl`): Can override metric classification

### Shadow Mode (Stages 7-9)
Boltzmann/Purity/Network logic runs alongside core, doesn't modify `classify_from_metrics/6`.

---

## Known Issues & Calibration Needs

### Issue 1: Tangled Rope Threshold Mismatch
**Problem:** `tangled_rope_epsilon_floor` = 0.50, but carbon credits example uses Îµ = 0.40  
**Status:** Requires reconciliation or example update  
**Impact:** High â€” affects canonical examples

### Issue 2: Boltzmann Floor Calibration
**Problem:** Current values (0.02-0.20) are provisional estimates  
**Need:** Corpus-based calibration per coordination type  
**Impact:** Medium â€” affects excess extraction calculations

### Issue 3: Purity Weight Sensitivity
**Problem:** Weights (30/25/25/20) derived theoretically, not empirically fitted  
**Need:** Sensitivity analysis to test robustness  
**Impact:** Medium â€” affects purity score stability

### Issue 4: Power Modifier Cultural Bias
**Problem:** Calibrated on 691-constraint Western corpus  
**Need:** Non-WEIRD validation and potential recalibration  
**Impact:** High â€” affects indexical relativity claims

### Issue 5: Scope Modifier Empirical Uncertainty
**Problem:** Ïƒ values based on theoretical reasoning about verification difficulty  
**Need:** Empirical validation of actual extraction hiding at different scopes  
**Impact:** Medium â€” affects Ï‡ calculations at extreme scopes

---

## Version History

**v1.0** (2023): Initial 6-type system, power scaling  
**v2.0** (2023): Added lifecycle states, temporal operators  
**v3.0** (2024): Added structural signatures, two-regime architecture  
**v3.3** (2024): Refined from 467-constraint corpus  
**v4.0** (2026): Added Boltzmann (Stage 7), Purity (Stage 8), Network Drift (Stage 9), 691-constraint corpus

---

## Cross-Reference

**For formal definitions** â†’ logic.md Â§II  
**For implementation architecture** â†’ logic_extensions.md Â§6  
**For calibration methodology** â†’ validation/validation_report.md  
**For Prolog source** â†’ config.pl (lines 71-410)

---

**"Thresholds are not arbitrary. They are empirical discoveries about where the structure of constraint-space shifts from one regime to another."**
