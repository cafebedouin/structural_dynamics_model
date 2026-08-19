# Deferential Realism: Logic Thresholds Registry

**Version 4.0**  
**Purpose:** Single source of truth for all system parameters  
**Source:** config.pl (lines 71-503)  
**Last Updated:** February 2026

---

## Overview

This document provides the **canonical threshold values** for all parameters in the Deferential Realism logic system. These values are:

- **Authoritative**: Implementation must use these exact values from config.pl
- **Calibrated**: Derived from 691-constraint corpus analysis
- **Provisional**: Subject to refinement through validation and cross-cultural replication
- **Documented**: Each parameter includes stage, logic, and implementation significance

**Critical Principle:** Changes flow spec → registry → implementation, never backward.

---

## Table of Contents

1. Power Modifiers (π) — How extraction scales by power position
2. Scope Modifiers (σ) — How scope affects verification difficulty
3. Classification Gates — Type threshold values (Mountain, Rope, Snare, etc.)
4. Structural Signatures — Natural Law, Coordination Scaffold, Constructed
5. Boltzmann Compliance — Independence testing for natural law claims
6. Purity Scoring — Coordination health measurement
7. Network Dynamics — Contamination propagation parameters
8. Lifecycle Drift — Threshold values for detecting degradation
9. Action Layer — Energy costs and decision gates
10. Defaults & Meta — Fallback values
11. Maximum Entropy Shadow Classifier (v6.2) — Diagnostic probability distributions
12. Abductive Reasoning Engine (v6.3) — Cross-subsystem anomaly detection
13. Trajectory Mining (v6.4) — Structural family detection and isomorphisms

---

## 1. Power Modifiers (π)

**Formula:** χ = ε × π(P) × σ(S)  
**Purpose:** Determines how much base extraction is "felt" by agent at power position P

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `power_modifier_powerless` | **1.5** | 1-6 | Extraction amplified — powerless agents bear full cost |
| `power_modifier_moderate` | **1.0** | 1-6 | Baseline — default extraction experience |
| `power_modifier_powerful` | **0.6** | 1-6 | Extraction reduced — powerful agents can deflect costs |
| `power_modifier_organized` | **0.4** | 1-6 | Shared burden — collective action distributes costs |
| `power_modifier_institutional` | **-0.2** | 1-6 | **Net beneficiary** — institutions extract more than they pay |
| `power_modifier_analytical` | **1.15** | 7 | Degeneracy-breaking value — detects extraction moderate agents normalize |

**Key Notes:**
- **π(analytical) = 1.15**: Chosen specifically to break moderate-analytical degeneracy. At 1.0, analytical and moderate produced identical χ values. At 1.15, 93 corpus constraints show "only analyst catches snare" pattern.
- **π(institutional) = -0.2**: Negative value represents net extraction from system. Handle sign carefully in implementations.
- **Calibration source**: 691-constraint corpus (2024-2026)
- **Known limitation**: Values are Western-biased, require non-WEIRD validation

---

## 2. Scope Modifiers (σ)

**Formula:** χ = ε × π(P) × σ(S)  
**Purpose:** Models verification difficulty and hidden extraction at larger scales

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `scope_modifier_local` | **0.8** | 1-6 | Easy verification → extraction dampened |
| `scope_modifier_regional` | **0.9** | 1-6 | Easier verification |
| `scope_modifier_national` | **1.0** | 1-6 | Baseline |
| `scope_modifier_continental` | **1.1** | 1-6 | Harder verification → extraction amplified |
| `scope_modifier_global` | **1.2** | 1-6 | Hardest verification → maximum extraction hiding |
| `scope_modifier_universal` | **1.0** | 1-6 | Neutral — natural laws don't scale with scope |

**Key Notes:**
- **σ(universal) = 1.0**: Natural laws are scope-invariant, hence neutral modifier
- **σ(global) = 1.2**: Maximum amplification reflects Dunbar-number constraints on verification at planetary scale
- **Rationale**: Larger scope = more participants = harder to verify claims = easier to hide extraction

---

## 2b. Sigmoid Directionality (v5.0)

**Formula:** `f(d) = L + (U - L) / (1 + e^(-k*(d - d0)))`
**Purpose:** Replaces discrete power_modifier dispatch with continuous sigmoid mapping from directionality `d ∈ [0.0, 1.0]` to modifier value

### Sigmoid Shape Parameters

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `sigmoid_lower` | **-0.20** | 7 | L: lower asymptote (institutional end) |
| `sigmoid_upper` | **1.50** | 7 | U: upper asymptote (powerless end) |
| `sigmoid_midpoint` | **0.50** | 7 | d0: inflection point |
| `sigmoid_steepness` | **6.00** | 7 | k: steepness of transition |

### Canonical Directionality Positions

These calibrate the sigmoid so `f(d) ≈ π(P)` for each power level:

| Parameter | Value | Stage | Corresponding π | Logic/Significance |
|-----------|-------|-------|-----------------|-------------------|
| `canonical_d_powerless` | **1.00** | 7 | 1.50 | f(1.0)≈1.42 vs 1.50 — tail residual, beyond gate boundaries |
| `canonical_d_analytical` | **0.7250** | 7 | 1.15 | Analyst position in directionality space |
| `canonical_d_moderate` | **0.6459** | 7 | 1.00 | Exact match at mid-range |
| `canonical_d_powerful` | **0.4804** | 7 | 0.60 | Exact match at mid-range |
| `canonical_d_organized` | **0.3990** | 7 | 0.40 | Exact match at mid-range |
| `canonical_d_institutional` | **0.00** | 7 | -0.20 | f(0.0)≈-0.12 vs -0.20 — tail residual, beyond gate boundaries |

**Implementation:** `config.pl` Section 4C

**Key Notes:**
- Mid-range atoms (moderate, powerful, organized) match exactly
- Extremes (institutional, powerless) have small residuals because sigmoid asymptotes are unreachable
- Residuals are at tails where χ is well beyond gate boundaries, so no classification shifts result
- The relationship between canonical d positions and legacy power modifiers enables smooth transition from discrete to continuous directionality

---

## 3. Classification Gates

### 3a. Mountain (■C[I])

**Formal:** `■C[I] ↔ ε(C) ≤ 0.25 ∧ Supp(C) ≤ 0.05 ∧ NaturalEmergence(C) ∧ Immutable(C, I.T, I.E)`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `mountain_extractiveness_max` | **0.25** | 1-6 | Max base extraction for natural law (coordination floor) |
| `mountain_suppression_ceiling` | **0.05** | 1-6 | Noise floor — no enforcement needed |
| `mountain_extractiveness_min` | **0.0** | 1-6 | Theoretical minimum (unused in practice) |

**Implementation:** `classify_from_metrics/6` line 2946 (drl_core.pl)  
**Structural Gate:** [Requires Boltzmann Compliance for NL Signature. See logic_extensions.md §1.3]

**Mountain Validity Constraint (empirically confirmed, February 2026):**

Three threshold rules are individually sufficient to invalidate Mountain classification. The corpus audit confirmed these as bright-line rules — every file violating any of them was correctly flagged as a false Mountain (96 of 594 files with direct definitional violations).

```
Mountain_valid(C) ↔ ε(C) ≤ 0.25 ∧ TR(C) ≤ 0.10 ∧ ¬requires_active_enforcement(C)
```

| Rule | Threshold | Rationale |
|------|-----------|-----------|
| Extraction ceiling | ε ≤ 0.25 | Natural law does not extract from subjects |
| Theater ratio ceiling | TR ≤ 0.10 | Natural law needs no legitimation theater |
| No active enforcement | ¬requires_active_enforcement | Natural law self-enforces through reality, not institutions |

**Recommendation:** Formalize as meta-engine rule rather than classification engine rule, to preserve the generating LLM's honest read as data while catching definitional violations mechanically. See `limitations.md` for full audit findings.

---

### 3b. Rope (⊞C[I])

**Formal:** `⊞C[I] ↔ χ(C, I.P, I.S) ≤ 0.35 ∧ ε(C) ≤ 0.45 ∧ Changeable(C, I.T, I.E)`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `rope_chi_ceiling` | **0.35** | 1-6 | Max power-scaled extraction for pure coordination |
| `rope_epsilon_ceiling` | **0.45** | 1-6 | Max base extraction for coordination (wider gate than Mountain) |
| `rope_suppression_ceiling` | **0.16** | 1-6 | Base suppression ceiling for pure coordination. **Note:** Not currently checked in `classify_from_metrics/6` rope gate. Reserved for future use or used by other predicates. |
| `rope_extractiveness_min` | **0.0** | 1-6 | Theoretical minimum |

**Implementation:** `classify_from_metrics/6` line 2970 (drl_core.pl)  
**Structural Gate:** [Can be certified as CI_Rope if Boltzmann-compliant. See logic_extensions.md §1.4]

**Key Note:** Dual threshold (χ AND ε) prevents high-power agents from misclassifying high-ε constraints as Ropes.

---

### 3c. Snare (⊠C[I])

**Formal:** `⊠C[I] ↔ χ(C, I.P, I.S) ≥ 0.66 ∧ ε(C) ≥ 0.46 ∧ Supp(C) ≥ 0.60 ∧ Changeable(C, I.T, I.E)`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `snare_chi_floor` | **0.66** | 1-6 | Min power-scaled extraction for pure extraction |
| `snare_epsilon_floor` | **0.46** | 1-6 | Min base extraction (prevents powerless-only misclassification) |
| `snare_suppression_floor` | **0.60** | 1-6 | Requires active enforcement |
| `snare_extraction_ceil` | **1.00** | 1-6 | Maximum possible extraction |
| `snare_load_bearing_threshold` | **0.70** | 1-6 | Above → load-bearing snare (Theorem 3: cutting causes collapse) |

**Implementation:** `classify_from_metrics/6` line 2953 (drl_core.pl)  
**Structural Gate:** [Nonsensical coupling is evidence of snare. See logic_extensions.md §1.6]

**Key Note:** Triple threshold (χ AND ε AND Supp) is strictest gate — prevents false positives.

---

### 3d. Tangled Rope (⊞⊠C[I])

**Formal:** `⊞⊠C[I] ↔ 0.40 ≤ χ(C, I.P, I.S) ≤ 0.90 ∧ ε(C) ≥ 0.30 ∧ Supp(C) ≥ 0.40 ∧ Enforce(C) ∧ Coord(C) ∧ Asymmetric(C)`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `tangled_rope_chi_floor` | **0.40** | 1-6 | Min power-scaled extraction for hybrid |
| `tangled_rope_chi_ceil` | **0.90** | 1-6 | Max power-scaled extraction (overlaps with snare) |
| `tangled_rope_epsilon_floor` | **0.30** | 1-6 | Min base extraction for hybrid classification |
| `tangled_rope_suppression_floor` | **0.40** | 1-6 | Requires active enforcement |
| `tangled_rope_suppression_ceil` | **1.00** | 1-6 | Maximum suppression |

**Implementation:** `classify_from_metrics/6` line 2977 (drl_core.pl)

**Key Notes:**
- **Empirical prevalence:** ~36% of analyzed constraints (most common type)
- **Structural requirements:** Requires Coord(C) ∧ Enforce(C) ∧ Asymmetric(C)
- **Overlap with snare:** A constraint can be tangled at moderate power, snare at powerless power
- **Calibration note:** ε threshold lowered to 0.30 during calibration to accommodate real constraints with moderate extractiveness that exhibit both coordination and extraction.

---

### 3e. Scaffold (⊡C[I])

**Formal:** `⊡C[I] ↔ χ(C, I.P, I.S) ≤ 0.45 ∧ Coord(C) ∧ Sunset(C) ∧ Theater(C) ≤ 0.70`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `scaffold_extraction_ceil` | **0.45** | 1-6 | Max extraction for temporary support |

**Implementation:** `classify_from_metrics/6` line 2962 (drl_core.pl)

**Key Notes:**
- **No immutability gate**: Scaffolds are inherently temporary
- **Theater ceiling (0.70)**: Ensures constraint does real work, not mere performance
- **Sunset requirement**: Built-in expiration distinguishes from Rope

---

### 3f. Piton (⊟C[I])

**Formal:** `⊟C[I] ↔ χ(C, I.P, I.S) ≤ 0.45 ∧ ε(C) > 0.10 ∧ Theater(C) ≥ 0.70`

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `piton_extraction_ceiling` | **0.45** | 1-6 | Minimal effective extraction for anyone |
| `piton_epsilon_floor` | **0.10** | 1-6 | Still costs energy to maintain (distinguishes from ε=0) |
| `piton_theater_floor` | **0.70** | 1-6 | High theater ratio distinguishes from low-extraction ropes |

**Implementation:** `classify_from_metrics/6` line 2990 (drl_core.pl)

**Key Note:** Degradation state — extraction dried up but structure persists. Theater ratio (performance/substance) is diagnostic.

---

## 4. Structural Signatures

**Purpose:** Detect constraint ORIGIN (natural vs coordination vs constructed) beyond metrics

### 4a. Natural Law (NL)

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `natural_law_collapse_min` | **0.85** | 3 | Extreme universal inaccessibility |
| `natural_law_suppression_max` | **0.15** | 3 | No enforcement needed |
| `natural_law_resistance_max` | **0.15** | 3 | Cannot be resisted |

**Implementation:** `signature_detection.pl` natural_law_signature/1

**Enhancement (Stage 7):** NL claims must pass `boltzmann_invariant_mountain/2` test:
- Factorization across Power × Scope
- Scope invariance
- No excess extraction
- Boltzmann compliance

> **[CORRECTION — OQ-302, 2026-08-19; snapshot ANNOTATED, not rewritten]** This enhancement
> has **never shipped** and is dark twice over: `boltzmann_invariant_mountain/2` returned
> `inconclusive(insufficient_data)` for every constraint until 2026-08-19 (a bound-`false`
> call at `boltzmann_compliance.pl:577`), and even repaired its `invariant(_)` verdict is
> unreachable because Test 4 gates on the dead-by-range `natural_law_signature/1`
> (`T4 = fail` on 5,311/5,311 across six legs). Full status: `docs/logic_thresholds.md`
> (the live spec). Preserved unedited here — `docs/v8/` is v8 source material (OQ-135).

---

### 4b. Coordination Scaffold (CS)

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `coordination_collapse_min` | **0.85** | 3 | Universal adoption achieved |
| `coordination_suppression_max` | **0.15** | 3 | Voluntary compliance |
| `coordination_resistance_max` | **0.15** | 3 | Minimal opposition |

**Implementation:** `signature_detection.pl` coordination_scaffold_signature/1

**Key Difference from NL:** Alternatives existed (path-dependent), but this path won.

---

### 4c. Constructed Constraint (CC)

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `constructed_suppression_min` | **0.20** | 3 | Requires enforcement |
| `constructed_resistance_min` | **0.20** | 3 | Faces opposition |
| `constructed_beneficiary_min` | **2** | 3 | Asymmetric gains threshold (count of beneficiaries) |

**Implementation:** `signature_detection.pl` constructed_constraint_signature/1

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

**Purpose:** Natural laws must factorize across index dimensions — independence test for NL claims

### 5a. Coupling Detection

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `boltzmann_coupling_threshold` | **0.25** | 7 | Max allowable coupling score for Boltzmann compliance |
| `boltzmann_coupling_strong_threshold` | **0.50** | 7 | Above → "strong coupling" classification |
| `boltzmann_factorization_tolerance` | **0.10** | 7 | Relative error margin for χ(P,S) ≈ f(P)×g(S) test |
| `boltzmann_min_classifications` | **3** | 7 | Min indexed classifications for reliable test (epistemic access) |

**Implementation:** `boltzmann_compliance.pl` boltzmann_compliant/2, cross_index_coupling/2

**Key Insight:** If χ doesn't factorize → constraint couples independent variables → constructed, not natural.

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

**Implementation:** `boltzmann_compliance.pl` excess_extraction/2

**Formula:** Excess = ε(C) - BoltzmannFloor(coordination_type)

**Key Insight:** Extraction above floor = extractive overhead (PoA excess), not necessary coordination cost.

**Note:** Provisional values — require corpus calibration.

---

### 5d. Reformability

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `reformability_high_threshold` | **0.70** | 7 | Above → "highly reformable" |
| `reformability_low_threshold` | **0.30** | 7 | Below → "low reformability" |

**Implementation:** `drl_modal_logic.pl` reformability_score/3

**Formula:** 30% separability + 40% coupling topology + 30% excess extraction

---

### 5e. Drift Detection

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `boltzmann_floor_drift_threshold` | **0.05** | 7 | Min floor increase → drift event (Type 9) |

> **Note (February 2026):** `coupling_drift_threshold` (0.10) was removed — it was an orphan parameter never wired into any code. `detect_coupling_drift` uses `boltzmann_coupling_threshold` (0.25) as the actual coupling gate.

**Implementation:** `drift_events.pl` (via `drl_lifecycle.pl` facade) `detect_coupling_drift/1`, `detect_boltzmann_floor_drift/1`

**Purpose:** Distinguish necessary complexity increase from extractive complexity increase.

---

## 6. Purity Scoring (Stage 7)

**Purpose:** Structural integrity measurement — continuous health metric

### 6a. Purity Score Components

**Formula:**
```
purity_score = 0.30 × (1.0 - coupling_score)
             + 0.25 × scope_invariance
             + 0.25 × coupling_cleanliness
             + 0.20 × excess_extraction_decay
```

**Weights:**
- **30%** factorization (Boltzmann compliance)
- **25%** scope invariance (classification stable across scopes)
- **25%** coupling cleanliness (no nonsensical coupling)
- **20%** excess extraction decay (proximity to Boltzmann floor)

**Zones:**
- **pristine** (≥ 0.9)
- **sound** (≥ 0.7)
- **borderline** (≥ 0.5)
- **contaminated** (≥ 0.3)
- **degraded** (< 0.3)

**Sentinel:** -1.0 = insufficient epistemic data

---

### 6b. Action Gates

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `purity_action_sound_floor` | **0.70** | 7 | Below → monitor purity |
| `purity_action_escalation_floor` | **0.50** | 7 | Below → escalate action urgency |
| `purity_action_degraded_floor` | **0.30** | 7 | Below → action type override (cut vs reform) |
| `purity_surgical_reform_gate` | **0.30** | 7 | **Min purity for surgical reform** — below this, reform fails |
| `purity_scaffold_health_gate` | **0.50** | 7 | Min scaffold purity for safe transition |

**Implementation:** `drl_boltzmann_analysis.pl` (via `drl_modal_logic.pl` facade) `purity_qualified_action/4`, `action_composition_gate/3`

**Key Insight:** Degraded constraints (< 0.30) block reform — transition to Cut/Exit.

---

### 6c. Energy Scaling

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `purity_energy_max_multiplier` | **3.0** | 7 | **Cap on energy cost scaling** — max overhead for fixing degraded system |

**Implementation:** `drl_modal_logic.pl` purity_adjusted_energy/4

**Formula:** Energy_cost × multiplier(purity)

**Example:** Reform at purity 0.31 → multiplier = 2.16× (near cap)

**Rationale:** Fixing rotten systems is harder than building new ones.

---

## 7. Network Dynamics (Stage 8)

**Purpose:** Contamination propagation through constraint networks

### 7a. Network Discovery

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `network_coupling_threshold` | **0.50** | 8 | Min inferred coupling for network edge |
| `network_shared_agent_min` | **1** | 8 | Min shared agents (beneficiary/victim) for edge |

**Implementation:** `drl_purity_network.pl` constraint_neighbors/3

---

### 7b. Contamination Propagation

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `purity_contamination_cap` | **0.30** | 8 | **Max purity reduction per edge** (prevents catastrophic drops) |
| `purity_attenuation_factor` | **0.50** | 8 | **Edge strength scaling** (contamination loses 50% per hop) |
| `purity_contamination_source_floor` | **0.50** | 8 | Below → active contamination source |

**Implementation:** `drl_purity_network.pl` effective_purity/4, purity_contamination_pressure/4

**Key Rule:** Downward-only contamination (Snare → Rope, not vice versa)

---

### 7c. Type Contamination Strength

**Purpose:** Different types have different contamination potency

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `contamination_strength_snare` | **1.0** | 8 | Maximum contamination (pure extraction) |
| `contamination_strength_piton` | **0.8** | 8 | High contamination (degraded state) |
| `contamination_strength_tangled_rope` | **0.5** | 8 | Moderate contamination (hybrid) |
| `contamination_strength_naturalized` | **0.3** | 8 | Moderate-low contamination (ambiguous structure) |
| `contamination_strength_scaffold` | **0.2** | 8 | Low contamination (temporary support) |
| `contamination_strength_rope` | **0.1** | 8 | Minimal contamination (pure coordination) |
| `contamination_strength_mountain` | **0.0** | 8 | **Mountains don't contaminate** (natural laws) |

**Implementation:** `drl_purity_network.pl` type_contamination_strength/2

**Note:** These values are hardcoded as facts in `drl_purity_network.pl`, not wired through `config.pl`. The `contamination_strength_*` parameters in `config.pl` exist but are unused (dead params). Values match between the two locations.

---

### 7d. Type Immunity (Target Susceptibility)

**Purpose:** Different target types have different susceptibility to contamination from neighbors. Immunity scales the total contamination before subtraction from intrinsic purity.

| Type | Immunity | Stage | Logic/Significance |
|------|----------|-------|-------------------|
| `mountain` | **0.0** | 8 | **Immune** — natural laws unaffected by neighbor contamination |
| `piton` | **0.3** | 8 | Low susceptibility — degraded state already resistant |
| `snare` | **0.5** | 8 | Moderate susceptibility |
| `naturalized` | **0.7** | 8 | High susceptibility — ambiguous structures easily influenced |
| `tangled_rope` | **0.8** | 8 | High susceptibility — hybrid state easily influenced |
| `scaffold` | **0.9** | 8 | Very high susceptibility — temporary structures fragile |
| `rope` | **1.0** | 8 | **Fully susceptible** — pure coordination most affected |

**Implementation:** `drl_purity_network.pl` type_immunity/2

**Key Insight:** Mountains (immunity 0.0) are completely immune to contamination; Ropes (immunity 1.0) receive full contamination pressure. This reflects the asymmetry: natural laws are structurally robust, while pure coordination is structurally fragile.

**Formula integration:** `EffPurity = max(0.0, Intrinsic - TotalContam × Immunity)`

---

### 7e. Network Metrics

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `network_contamination_risk_threshold` | **2** | 8 | Low-purity neighbors → "at_risk" classification |
| `network_cluster_degraded_floor` | **0.40** | 8 | Below → cluster classified as "degraded" |

**Implementation:** `drl_purity_network.pl` network_purity_metrics/2

---

## 8. Network Drift Dynamics (Stage 9)

**Purpose:** How purity drift propagates over time through networks

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `network_drift_velocity_threshold` | **0.01** | 9 | Min effective purity drift per year → network drift event |
| `network_hub_degree_threshold` | **3** | 9 | Neighbors required for "hub" → severity escalation |
| `network_cascade_count_threshold` | **3** | 9 | Drifting constraints → "cascading" network classification |
| `network_drift_hub_escalation` | **1** | 9 | 1=enable hub-based severity escalation (boolean flag) |

**Implementation:** `drl_lifecycle.pl` network_drift_velocity/4, cascade_prediction/3, network_stability_assessment/2

**Key Insight:** Constraints can degrade due to neighbor drift even if own metrics stable (induced degradation).

---

## 9. Lifecycle Drift Thresholds

**Purpose:** Additional parameters for drift event detection

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `system_gradient_threshold` | **0.01** | 2 | Min change → non-stable system |
| `system_gradient_strong_threshold` | **1.00** | 2 | Above → "strong" intent classification |

**Implementation:** `drl_lifecycle.pl` + `intent_engine.pl`

---

## 10. Defaults & Meta-Parameters

### 10a. Default Values

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `default_extractiveness` | **0.10** | 1-6 | Fallback when ε unmeasured |
| `default_suppression` | **0.10** | 1-6 | Fallback when Supp unmeasured |
| `default_theater` | **0.0** | 1-6 | Fallback when theater unmeasured |

---

### 10b. Data Quality

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `data_high_threshold` | **0.95** | 2 | Above → "high confidence" |
| `data_medium_threshold` | **0.75** | 2 | Above → "medium confidence" |

**Implementation:** `data_validation.pl`

---

### 10c. Coalition Modeling

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `critical_mass_threshold` | **3** | 6 | Shared snare victims → organized power modifier |

**Implementation:** `constraint_indexing.pl`

**Rationale:** 3+ victims can coordinate → shift from powerless to organized.

---

## 10d. Fixed-Point Network Iteration (v5.3)

**Purpose:** Controls FPN convergence behavior for purity propagation

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `fpn_epsilon` | **0.001** | 8 | Convergence threshold (< min zone gap 0.20) |
| `fpn_max_iterations` | **20** | 8 | Hard cap (2× theoretical worst case) |
| `fpn_enabled` | **1** | 8 | Graduated Phase 7-T2: FPN iteration enabled |

**Implementation:** `drl_fpn.pl`

**Key Notes:**
- Convergence threshold must be smaller than the minimum zone gap (0.20) to prevent oscillation across zone boundaries
- Max iterations cap prevents infinite loops in degenerate topologies
- Enable flag graduated Phase 7-T2: computation runs unconditionally

---

## 11. Maximum Entropy Shadow Classifier (v6.2)

**Purpose:** Diagnostic shadow classifier producing probability distributions, entropy scores, and disagreement flags alongside the deterministic cascade

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `maxent_enabled` | **1** | 7 | Graduated Phase 7-T1: computation runs unconditionally |
| `maxent_uncertainty_threshold` | **0.40** | 7 | H_norm above this = flagged as uncertain |
| `maxent_disagreement_prob_threshold` | **0.50** | 7 | P(det_type) below this = soft disagreement with deterministic classifier |
| `maxent_boolean_penalty` | **-4.0** | 7 | Log-likelihood for violated boolean gate |
| `maxent_boolean_bonus` | **1.0** | 7 | Log-likelihood for satisfied bonus feature |
| `maxent_prior_mode` | **corpus** | 7 | Prior distribution source: `corpus` or `uniform` |
| `maxent_signature_override_strength` | **0.95** | 7 | P assigned to unconditional override target |

**Implementation:** `maxent_classifier.pl`

**Key Notes:**
- Runs as shadow classifier — produces diagnostics only, does not modify `classify_from_metrics/6`
- Entropy score H_norm measures classifier uncertainty; high values flag ambiguous constraints
- Prior mode `corpus` uses empirical type frequencies; `uniform` assigns equal probability to all types

---

## 12. Abductive Reasoning Engine (v6.3)

**Purpose:** Cross-subsystem anomaly detection synthesizing signals from structural signatures, MaxEnt, FPN, Dirac orbits, drift detection, and logical fingerprints

### 12a. Core Parameters

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `abductive_enabled` | **1** | 7 | Graduated Phase 7-T1: computation runs unconditionally |
| `abductive_confidence_floor` | **0.30** | 7 | Hypotheses below this confidence not stored |
| `abductive_fpn_divergence_threshold` | **0.02** | 7 | FPN EP divergence threshold for triggers |
| `abductive_maxent_mountain_deception` | **0.50** | 7 | P(mountain) threshold for deep_deception trigger |
| `abductive_dormant_entropy_ceiling` | **0.15** | 7 | Max H_norm for dormant_extraction trigger |

### 12b. Trigger-Specific Thresholds

| Parameter | Value | Trigger | Logic/Significance |
|-----------|-------|---------|-------------------|
| `abductive_shadow_divergence_threshold` | **0.85** | T9 | Min P(MaxEntTop) for shadow divergence |
| `abductive_stress_convergence_min` | **4** | T10 | Min signals for common core (rare gate provides selectivity) |
| `abductive_snare_lean_psi_threshold` | **0.90** | T11 | Min ψ for snare-leaning tangled |
| `abductive_snare_lean_psnare_floor` | **0.85** | T11 | Min P(snare) for snare-leaning tangled |
| `abductive_stress_purity_threshold` | **0.60** | T10 | Purity below this = stressed indicator |
| `abductive_stress_coupling_threshold` | **0.75** | T10 | Coupling above this = stressed indicator |
| `abductive_stress_entropy_threshold` | **0.15** | T10 | Entropy above this = stressed indicator |
| `abductive_stress_drift_mode` | **any** | T10 | Drift indicator mode: `any`, `critical`, or `count_2plus` |
| `abductive_maxent_divergence_threshold` | **0.05** | T13 | Min indexing divergence to fire |
| `abductive_hub_conflict_h1_threshold` | **4** | T14 | Exact H¹ band for hub conflict |
| `abductive_oracle_entropy_ceiling` | **0.40** | T16 | Max H_norm for "confident oracle" |

### 12c. Post-Synthesis Divergence (T12)

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `post_synthesis_enabled` | **1** | 7 | T12 master switch |
| `post_synthesis_green_trigger_threshold` | **2** | 7 | Case 2: min genuine triggers for green divergence |

**Implementation:** `abductive_engine.pl`

---

## 13. Trajectory Mining (v6.4)

**Purpose:** Extends 24 orbit families (type-only) into richer structural families incorporating continuous metrics, entropy, coupling, drift, and fingerprint voids. Detects structural isomorphisms — constraints from different domains that behave identically under observer shift.

| Parameter | Value | Stage | Logic/Significance |
|-----------|-------|-------|-------------------|
| `trajectory_enabled` | **0** | 9 | 0=disabled, 1=enabled. Deferred — requires runtime benchmarking |
| `trajectory_distance_shift_weight` | **0.35** | 9 | Weight for shift (type sequence) distance |
| `trajectory_distance_metric_weight` | **0.25** | 9 | Weight for metric (χ, entropy) distance |
| `trajectory_distance_stability_weight` | **0.25** | 9 | Weight for stability (coupling, purity) distance |
| `trajectory_distance_pathology_weight` | **0.15** | 9 | Weight for pathology (drift, voids) distance |
| `trajectory_family_cut_level` | **0.30** | 9 | Dendrogram cut height for family assignment |
| `trajectory_isomorphism_threshold` | **0.15** | 9 | Max distance for trajectory isomorphism |
| `trajectory_coupling_band_width` | **0.15** | 9 | Coupling match tolerance for isomorphism |

**Implementation:** `trajectory_mining.pl`

**Key Notes:**
- Currently disabled (`trajectory_enabled = 0`) — checked at pipeline level (run_pipeline.py), entire trajectory mining step is skipped
- Distance metric is a weighted sum of four components (shift, metric, stability, pathology)
- Isomorphism detection finds constraints from different domains with near-identical structural trajectories

---

## Implementation Notes

### Priority Ordering (classify_from_metrics/6)
```
Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > Naturalized > unknown
```

### Two-Regime Architecture
1. **Metrics-first** (`drl_core.pl`): Uses thresholds from this registry
2. **Signature-override** (`signature_detection.pl` via wrapper `structural_signatures.pl`): Can override metric classification

### Shadow Mode (Stages 7-9)
Boltzmann/Purity/Network logic runs alongside core, doesn't modify `classify_from_metrics/6`.

---

## Known Issues & Calibration Needs

### Issue 1: Tangled Rope Threshold Mismatch — RESOLVED
**Problem:** `tangled_rope_epsilon_floor` was documented as 0.50, but carbon credits example uses ε = 0.40.
**Resolution:** The threshold was lowered to 0.30 during calibration to accommodate real constraints with moderate extractiveness that exhibit both coordination and extraction. Documentation updated February 2026 to match config.pl.
**Status:** Resolved
**Impact:** No longer an issue — threshold, documentation, and examples are now consistent

### Issue 2: Boltzmann Floor Calibration
**Problem:** Current values (0.02-0.20) are provisional estimates  
**Need:** Corpus-based calibration per coordination type  
**Impact:** Medium — affects excess extraction calculations

### Issue 3: Purity Weight Sensitivity
**Problem:** Weights (30/25/25/20) derived theoretically, not empirically fitted  
**Need:** Sensitivity analysis to test robustness  
**Impact:** Medium — affects purity score stability

### Issue 4: Power Modifier Cultural Bias
**Problem:** Calibrated on 691-constraint Western corpus  
**Need:** Non-WEIRD validation and potential recalibration  
**Impact:** High — affects indexical relativity claims

### Issue 5: Scope Modifier Empirical Uncertainty
**Problem:** σ values based on theoretical reasoning about verification difficulty  
**Need:** Empirical validation of actual extraction hiding at different scopes  
**Impact:** Medium — affects χ calculations at extreme scopes

---

## Version History

**v1.0** (2023): Initial 6-type system, power scaling  
**v2.0** (2023): Added lifecycle states, temporal operators  
**v3.0** (2024): Added structural signatures, two-regime architecture  
**v3.3** (2024): Refined from 467-constraint corpus  
**v4.0** (2026): Added Boltzmann (Stage 7), Purity (Stage 8), Network Drift (Stage 9), 691-constraint corpus

---

## Cross-Reference

**For formal definitions** → logic.md §II  
**For implementation architecture** → logic_extensions.md §6  
**For calibration methodology** → validation/validation_report.md  
**For Prolog source** → config.pl (lines 71-503)

---

**"Thresholds are not arbitrary. They are empirical discoveries about where the structure of constraint-space shifts from one regime to another."**
