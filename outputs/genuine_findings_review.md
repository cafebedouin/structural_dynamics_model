# Deep Review: 6 Genuine Abductive Findings

*Full diagnostic profiles and structural narratives for the non-artifact hypotheses produced by the abductive reasoning engine (v6.3).*

## Overview

The abductive engine produces 56 hypotheses: 50 signature_override_artifacts (mechanistic explanations for MaxEnt hard disagreements caused by known override rules) and **6 genuine findings** — constraints flagged by multiple independent diagnostic subsystems from different theoretical angles. These 6 are the analytically most interesting constraints in the 1,023-constraint corpus.

The 6 divide into two hypothesis classes:
- **4 metric_structural_divergence** — high MaxEnt entropy but preserved single-type Dirac orbit
- **2 confirmed_liminal** — triple-confirmed liminality (high entropy + multi-type orbit + active drift)

---

## Finding 1: `decentralized_infrastructure_rope`

**Hypothesis class:** metric_structural_divergence
**Confidence:** 0.73
**Investigation action:** inspect_metrics

### Full Diagnostic Profile

| Signal | Source | Value |
|--------|--------|-------|
| **DR type** (default context) | deterministic classifier | rope |
| **Base extractiveness (epsilon)** | narrative_ontology | 0.08 |
| **Raw suppression** | narrative_ontology | 0.25 |
| **Theater ratio** | narrative_ontology | 0.02 |
| **Chi value** (default context) | extractiveness_for_agent | 0.1096 |
| **Metric match** | classify_from_metrics | rope (also matches rope clause) |
| **Distance from rope chi ceiling (0.35)** | threshold analysis | 0.2404 |
| **Distance from tangled_rope chi floor (0.40)** | threshold analysis | 0.2904 |
| **Structural signature** | constraint_signature | constructed_low_extraction |
| **Purity score** | purity_score | 0.8755 |
| **Boltzmann compliance** | boltzmann_compliant | non_compliant(0.375, 0.25) |
| **Cross-index coupling** | cross_index_coupling | 0.375 |
| **Structural purity** | structural_purity | inconclusive |
| **Factorization subscore** | factorization_subscore | 0.625 |
| **Scope invariance subscore** | scope_invariance_subscore | 1.0 |
| **Coupling cleanliness subscore** | coupling_cleanliness_subscore | 1.0 |
| **Excess extraction subscore** | excess_extraction_subscore | 0.94 |
| **MaxEnt distribution** | maxent_distribution | rope: 0.640, scaffold: 0.313, tangled_rope: 0.048 |
| **MaxEnt entropy (H_norm)** | maxent_entropy | 0.4435 |
| **MaxEnt confidence** | maxent_confidence | 0.5565 |
| **MaxEnt disagreement** | maxent_disagreement | entropy_flag(0.4435) — no type disagreement |
| **MaxEnt top type** | maxent_top_type | rope (agrees with deterministic) |
| **Gauge orbit** | gauge_orbit | [rope, rope, rope, rope] — preserved |
| **Orbit preserved** | preserved_under_context_shift | preserved(rope) |
| **Drift events** | scan_constraint_drift | 1: purity_drift (warning) |
| **Drift details** | drift evidence | coupling_above_threshold(0.375), excess_above_floor(0.03) |
| **Mismatches** | dr_mismatch | none |
| **Fingerprint voids** | fingerprint_voids | none |
| **Fingerprint shift** | fingerprint_shift | shift(rope, rope, rope, rope) |
| **Fingerprint zone** | fingerprint_zone | zone(negligible, low) |
| **Fingerprint coupling** | fingerprint_coupling | weakly_coupled, score=0.375 |

### Structural Narrative

`decentralized_infrastructure_rope` presents a compelling case of **metric-structural divergence**: MaxEnt assigns it only 64% probability of being rope (with 31% probability of scaffold), yielding a normalized entropy of 0.44 — well above the 0.40 uncertainty threshold. Yet the Dirac orbit is perfectly preserved: rope at every standard context. The MaxEnt shadow classifier sees genuine metric ambiguity; the structural identity is unambiguous.

The ambiguity source is clear: the constraint's metrics place it in the overlap zone between rope and scaffold. Its low extraction (epsilon=0.08, chi=0.11) and low theater (0.02) are consistent with both coordination types. The deterministic classifier resolves this via priority ordering (scaffold before rope in classify_from_metrics, but the scaffold clause requires has_coordination_function and scaffold_temporality_check — the latter likely differentiates). The MaxEnt model, lacking access to the priority cascade's boolean gates, sees the continuous metrics and legitimately hedges.

What makes this constraint structurally interesting is the **tension between its Boltzmann non-compliance (coupling=0.375, above the 0.25 threshold) and its high purity score (0.8755)**. The coupling violation comes from factorization failures across the Power x Scope grid, yet the scope invariance, coupling cleanliness, and excess extraction subscores are all near-perfect. This suggests the coupling is *structural* — inherent to how decentralized infrastructure operates across power positions — rather than *extractive*. A human analyst would investigate whether this coupling pattern is the defining structural signature of "infrastructure coordination" as a category.

### What to Investigate Next

1. **Is the Boltzmann non-compliance an artifact of infrastructure complexity?** The complexity_adjusted_threshold mechanism raises the coupling threshold for high-complexity coordination types. If this constraint is classified as global_infrastructure, the effective threshold would be 0.40, making it compliant.
2. **Why does MaxEnt assign 31% probability to scaffold?** This is not noise — it's a genuine structural resemblance. The constraint may warrant dual classification or explicit documentation of why it's rope rather than scaffold.

---

## Finding 2: `fair_use_doctrine`

**Hypothesis class:** metric_structural_divergence
**Confidence:** 0.73
**Investigation action:** inspect_metrics

### Full Diagnostic Profile

| Signal | Source | Value |
|--------|--------|-------|
| **DR type** | deterministic | rope |
| **Base extractiveness** | narrative_ontology | 0.10 |
| **Raw suppression** | narrative_ontology | 0.40 |
| **Theater ratio** | narrative_ontology | 0.13 |
| **Chi value** | extractiveness_for_agent | 0.1370 |
| **Metric match** | classify_from_metrics | rope |
| **Distance from rope chi ceiling (0.35)** | threshold | 0.2130 |
| **Structural signature** | constraint_signature | constructed_low_extraction |
| **Purity score** | purity_score | -1.0 (inconclusive) |
| **Boltzmann** | boltzmann_compliant | inconclusive(insufficient_classifications) |
| **Coupling** | cross_index_coupling | 0.375 |
| **MaxEnt distribution** | maxent_distribution | rope: 0.590, scaffold: 0.319, tangled_rope: 0.091 |
| **MaxEnt entropy (H_norm)** | maxent_entropy | 0.4989 |
| **MaxEnt confidence** | maxent_confidence | 0.5011 |
| **MaxEnt disagreement** | maxent_disagreement | entropy_flag(0.4989) |
| **MaxEnt top type** | maxent_top_type | rope (agrees) |
| **Gauge orbit** | gauge_orbit | preserved(rope) |
| **Drift events** | scan_constraint_drift | none |
| **Mismatches** | dr_mismatch | none |
| **Fingerprint voids** | fingerprint_voids | none |
| **Fingerprint shift** | fingerprint_shift | shift(rope, rope, rope, rope) |
| **Fingerprint zone** | fingerprint_zone | zone(negligible, high) |

### Structural Narrative

Fair use doctrine is the **highest-entropy genuine finding** at H_norm=0.4989, essentially at the theoretical coin-flip between rope and scaffold. The MaxEnt model assigns 59% to rope and 32% to scaffold, with a non-trivial 9% tail for tangled_rope. Despite this near-maximal metric ambiguity, the Dirac orbit is perfectly preserved: rope from every perspective.

The structural signature is `constructed_low_extraction`, confirming that this is a human-created rule system with low extraction. The suppression score (0.40) is notably higher than the other metric_structural_divergence findings — it's in the tangled_rope suppression zone. This explains the 9% tangled_rope probability in MaxEnt. Fair use requires active enforcement (courts must adjudicate it), which pushes suppression up, yet its extraction remains low (epsilon=0.10), keeping it firmly in rope territory by the chi threshold.

The Boltzmann compliance test returns inconclusive due to insufficient indexed classifications, making the purity score unavailable (-1.0). This is itself diagnostic: the constraint has not been observed from enough perspectives to assess its coupling structure. The coupling score (0.375) is identical to decentralized_infrastructure_rope, suggesting a common structural pattern for coordination mechanisms with enforcement requirements.

What's analytically interesting is the **suppression-extraction gap**: suppression=0.40 (high zone) but extraction=0.10 (negligible zone). Fair use is a constraint that requires significant enforcement machinery but extracts very little. This gap is exactly what creates the MaxEnt ambiguity — the suppression pushes toward tangled_rope/scaffold while the extraction anchors at rope. The system is correctly classifying it, but the gap itself is the structural story.

### What to Investigate Next

1. **Add indexed classifications** to resolve the Boltzmann inconclusiveness. Fair use is a constraint where perspectival variation seems likely (a powerless individual vs. an institutional actor experiences fair use very differently).
2. **The suppression-extraction gap** (0.40 vs 0.10) may be a structural signature worth cataloging — "high-enforcement, low-extraction coordination" as a recognized pattern.

---

## Finding 3: `noethers_theorem_symmetry`

**Hypothesis class:** metric_structural_divergence
**Confidence:** 0.73
**Investigation action:** inspect_metrics

### Full Diagnostic Profile

| Signal | Source | Value |
|--------|--------|-------|
| **DR type** | deterministic | scaffold |
| **Base extractiveness** | narrative_ontology | 0.10 |
| **Raw suppression** | narrative_ontology | 0.10 |
| **Theater ratio** | narrative_ontology | 0.0 |
| **Chi value** | extractiveness_for_agent | 0.1370 |
| **Metric match** | classify_from_metrics | scaffold (also matches rope) |
| **Distance from rope chi ceiling** | threshold | 0.2130 |
| **Structural signature** | constraint_signature | constructed_low_extraction |
| **Purity score** | purity_score | -1.0 (inconclusive) |
| **Boltzmann** | boltzmann_compliant | inconclusive |
| **Coupling** | cross_index_coupling | 0.0 (perfectly factorized) |
| **MaxEnt distribution** | maxent_distribution | scaffold: 0.499, rope: 0.496, mountain: 0.005 |
| **MaxEnt entropy (H_norm)** | maxent_entropy | 0.4019 |
| **MaxEnt disagreement** | maxent_disagreement | soft(scaffold, scaffold, 0.499) — agrees but low confidence |
| **MaxEnt top type** | maxent_top_type | scaffold (agrees) |
| **Gauge orbit** | gauge_orbit | preserved(scaffold) |
| **Drift events** | scan_constraint_drift | none |
| **Mismatches** | dr_mismatch | none |
| **Fingerprint voids** | fingerprint_voids | none |
| **Fingerprint shift** | fingerprint_shift | shift(scaffold, scaffold, scaffold, scaffold) |
| **Fingerprint zone** | fingerprint_zone | zone(negligible, low) |

### Structural Narrative

Noether's theorem — that every continuous symmetry of a physical system corresponds to a conserved quantity — is classified as scaffold. This is structurally unusual for a mathematical theorem, which might be expected to classify as mountain (natural law). The deterministic classifier reaches scaffold because the constraint has a coordination function and passes the scaffold temporality check, and the scaffold clause fires before the rope clause in the priority cascade. MaxEnt agrees on scaffold but with razor-thin confidence: 49.9% scaffold vs 49.6% rope vs 0.5% mountain.

The zero coupling score is striking — perfectly factorized across all Power x Scope combinations. This is exactly what one would expect for a mathematical truth: it operates identically regardless of who observes it or at what scale. The purity subscores confirm this: factorization=1.0, scope_invariance=1.0, coupling_cleanliness=1.0, excess_extraction=0.90. The only imperfection is a slight excess extraction subscore, which likely reflects the 0.10 base extractiveness — a small but non-zero value that seems anomalous for a pure mathematical theorem.

The structural signature `constructed_low_extraction` is itself a potential misclassification. Noether's theorem doesn't have enforcement requirements or beneficiary asymmetries — it's a mathematical invariant. The signature system detects it as "constructed" because the profile-based classification fires after the Boltzmann-derived signatures all fail to match (no FNL, no FCR, no CI_Rope, no NL). The NL signature requires `emerges_naturally(C)` — if this property isn't declared for Noether's theorem in the testset, the NL signature won't fire. This may be a **data gap** rather than a genuine structural finding.

### What to Investigate Next

1. **Check the testset data**: Does `noethers_theorem_symmetry` declare `emerges_naturally`? If not, adding it would route through the natural_law signature and potentially reclassify to mountain.
2. **The near-exact rope/scaffold split** (49.9% vs 49.6%) is the closest to a true tie in the corpus. This constraint may need explicit disambiguation in the testset metadata.

---

## Finding 4: `reciprocity_laws_math`

**Hypothesis class:** metric_structural_divergence
**Confidence:** 0.73
**Investigation action:** inspect_metrics

### Full Diagnostic Profile

| Signal | Source | Value |
|--------|--------|-------|
| **DR type** | deterministic | scaffold |
| **Base extractiveness** | narrative_ontology | 0.10 |
| **Raw suppression** | narrative_ontology | 0.10 |
| **Theater ratio** | narrative_ontology | 0.0 |
| **Chi value** | extractiveness_for_agent | 0.1370 |
| **All metrics** | (identical to noethers_theorem_symmetry) | — |
| **MaxEnt distribution** | maxent_distribution | scaffold: 0.499, rope: 0.496, mountain: 0.005 |
| **MaxEnt entropy (H_norm)** | maxent_entropy | 0.4019 |
| **Gauge orbit** | gauge_orbit | preserved(scaffold) |
| **Coupling** | cross_index_coupling | 0.0 |
| **Everything else** | (identical to finding 3) | — |

### Structural Narrative

Reciprocity laws in mathematics (Artin reciprocity, quadratic reciprocity, etc.) present an **identical diagnostic profile** to Noether's theorem. Every metric, every subsystem signal, every diagnostic output is identical. This is because both constraints share the same metric values (epsilon=0.10, suppression=0.10, theater=0.0) and the same structural properties.

This exact duplication is itself diagnostically significant: it reveals that the abductive engine identifies constraints based on their *structural position in metric space*, not their domain content. Two mathematical theorems with identical metrics produce identical hypotheses. This validates the system's domain-agnosticism but also highlights a limitation: the system cannot distinguish between these two constraints at all. Any trajectory mining system (Phase B) would correctly place them in the same structural family.

The analysis from Finding 3 applies in full. The key question is the same: are these constraints missing the `emerges_naturally` declaration that would route them through the natural_law signature?

### What to Investigate Next

1. **Same as Finding 3** — check testset declarations.
2. **Are there other mathematical theorems in the corpus with identical metrics?** The 14-member `shift(scaffold, scaffold, scaffold, scaffold)` family from the fingerprint report includes `church_turing_thesis`, `euler_characteristic_topology`, `fundamental_theorem_of_algebra`, `liar_paradox`, and `universal_mathematics_communication`. If all share epsilon=0.10/suppression=0.10/theater=0.0, the MaxEnt ambiguity is systematic, not specific.

---

## Finding 5: `moltbook_agent_theater`

**Hypothesis class:** confirmed_liminal
**Confidence:** 0.85
**Investigation action:** monitor_drift

### Full Diagnostic Profile

| Signal | Source | Value |
|--------|--------|-------|
| **DR type** | deterministic | unknown |
| **Base extractiveness** | narrative_ontology | 0.70 |
| **Raw suppression** | narrative_ontology | 0.45 |
| **Theater ratio** | narrative_ontology | 0.90 |
| **Chi value** | extractiveness_for_agent | 0.9590 |
| **Metric match** | classify_from_metrics | no clause matches (falls through to unknown) |
| **Distance from snare chi floor (0.66)** | threshold | 0.2990 |
| **Structural signature** | constraint_signature | false_ci_rope |
| **Purity score** | purity_score | 0.3125 |
| **Boltzmann** | boltzmann_compliant | non_compliant(1.0, 0.4) |
| **Coupling** | cross_index_coupling | 1.0 (maximally coupled) |
| **Structural purity** | structural_purity | inconclusive |
| **Factorization subscore** | factorization_subscore | 0.0 |
| **Scope invariance subscore** | scope_invariance_subscore | 0.75 |
| **Coupling cleanliness subscore** | coupling_cleanliness_subscore | 0.50 |
| **Excess extraction subscore** | excess_extraction_subscore | ~0.0 |
| **MaxEnt distribution** | maxent_distribution | tangled_rope: 0.642, piton: 0.194, snare: 0.164 |
| **MaxEnt entropy (H_norm)** | maxent_entropy | 0.5021 |
| **MaxEnt confidence** | maxent_confidence | 0.4979 |
| **MaxEnt disagreement** | maxent_disagreement | residual_override(tangled_rope, unknown) |
| **MaxEnt top type** | maxent_top_type | tangled_rope |
| **Gauge orbit** | gauge_orbit | [rope, tangled_rope, tangled_rope, unknown] — 3 types |
| **Orbit preserved** | preserved_under_context_shift | violated (5 transitions) |
| **Drift events** | scan_constraint_drift | 4 events: metric_substitution (critical), extraction_accumulation (critical), coupling_drift (critical), purity_drift (warning) |
| **Mismatches** | dr_mismatch | 5 perspectival_incoherence mismatches |
| **Fingerprint voids** | fingerprint_voids | drifting_without_limit, no_exit_for_victims, unaccountable_extraction |
| **Fingerprint shift** | fingerprint_shift | shift(tangled_rope, tangled_rope, rope, unknown) |
| **Fingerprint zone** | fingerprint_zone | zone(extreme, high) |
| **Fingerprint coupling** | fingerprint_coupling | strongly_coupled, score=1.0, 6 coupled pairs |

### Structural Narrative

`moltbook_agent_theater` is the **most structurally complex constraint** in the genuine findings set — and arguably in the entire corpus. It triggers the confirmed_liminal hypothesis because three independent subsystems converge on the same diagnosis: this constraint is genuinely in transition.

The MaxEnt entropy of 0.50 is near-maximal uncertainty. The model splits probability across tangled_rope (64%), piton (19%), and snare (16%), reflecting genuine metric ambiguity. The deterministic classifier classifies it as `unknown` — it falls through *every clause* in `classify_from_metrics/6`. This is remarkable: with epsilon=0.70, chi=0.96, and suppression=0.45, it has extreme extraction but doesn't meet the snare floor (suppression needs >= 0.60 for snare), doesn't qualify for tangled_rope (which requires `requires_active_enforcement`, `has_coordination_function`, and `has_asymmetric_extraction` simultaneously), and far exceeds rope/scaffold ceilings. The structural signature `false_ci_rope` then fires, but since it has metric perspectival variance (different types across power levels), the FCR override defers to the metric classification — which is `unknown`. So the constraint is genuinely unclassifiable by the deterministic cascade.

The Dirac orbit is maximally variant: 3 distinct types across 4 contexts. Institutional sees rope (chi drops below thresholds due to negative power modifier), moderate and powerless see tangled_rope (amplified extraction), and analytical sees unknown (the classifier can't resolve it even with enhanced clarity). This produces 5 perspectival incoherence mismatches — the highest count among the genuine findings.

The drift profile is alarming: 4 active drift events, all pointing in the same direction. Extraction has accumulated from 0.10 to 0.70 over a 10-year span. Theater has risen from 0.20 to 0.90 — the highest theater ratio in the genuine findings set. The coupling has drifted to the theoretical maximum of 1.0. And purity is declining through 4 simultaneous decline signals. The predicted terminal state, if temporal data supported it, would be either snare or piton.

The three extractive fingerprint voids (drifting_without_limit, no_exit_for_victims, unaccountable_extraction) paint a picture of a constraint that has become pathological: high extraction without accountability, victims without exit, and temporal drift without any limiting mechanism. This is a textbook case of a constraint in active degradation.

### What to Investigate Next

1. **Priority: Determine the terminal trajectory.** With extraction at 0.70 and rising, theater at 0.90, this constraint is past the point of reform. The question is whether it ends as snare (if coordination fully dies) or piton (if extraction dries up but theater persists).
2. **Why does the classify_from_metrics cascade fail?** The suppression=0.45 is below snare floor (0.60) but above tangled_rope floor (0.40). The tangled_rope boolean gates (requires_active_enforcement, has_coordination_function, has_asymmetric_extraction) must not all be true. Checking which gate fails would clarify the structural gap.
3. **The 1.0 coupling score** is the theoretical maximum. Every Power x Scope combination produces different classifications. This is either a genuinely pathological constraint or a testset that needs review.

---

## Finding 6: `ulysses_calypso_1904`

**Hypothesis class:** confirmed_liminal
**Confidence:** 0.85
**Investigation action:** monitor_drift

### Full Diagnostic Profile

| Signal | Source | Value |
|--------|--------|-------|
| **DR type** | deterministic | tangled_rope |
| **Base extractiveness** | narrative_ontology | 0.47 |
| **Raw suppression** | narrative_ontology | 0.60 |
| **Theater ratio** | narrative_ontology | 0.75 |
| **Chi value** | extractiveness_for_agent | 0.6439 |
| **Metric match** | classify_from_metrics | tangled_rope |
| **Distance from snare chi floor (0.66)** | threshold | 0.0161 |
| **Distance from rope chi ceiling (0.35)** | threshold | 0.2939 |
| **Structural signature** | constraint_signature | false_ci_rope |
| **Purity score** | purity_score | 0.4903 |
| **Boltzmann** | boltzmann_compliant | non_compliant(1.0, 0.25) |
| **Coupling** | cross_index_coupling | 1.0 (maximally coupled) |
| **Factorization subscore** | factorization_subscore | 0.0 |
| **Scope invariance subscore** | scope_invariance_subscore | 1.0 |
| **Coupling cleanliness subscore** | coupling_cleanliness_subscore | 0.833 |
| **Excess extraction subscore** | excess_extraction_subscore | 0.16 |
| **MaxEnt distribution** | maxent_distribution | tangled_rope: 0.594, snare: 0.394, piton: 0.011 |
| **MaxEnt entropy (H_norm)** | maxent_entropy | 0.4058 |
| **MaxEnt confidence** | maxent_confidence | 0.5942 |
| **MaxEnt disagreement** | maxent_disagreement | entropy_flag(0.4058) |
| **MaxEnt top type** | maxent_top_type | tangled_rope (agrees) |
| **Gauge orbit** | gauge_orbit | [tangled_rope, tangled_rope, rope, tangled_rope] — 2 types |
| **Orbit preserved** | preserved_under_context_shift | violated (3 transitions) |
| **Drift events** | scan_constraint_drift | 5 events: metric_substitution (critical), extraction_accumulation (critical), coupling_drift (critical), purity_drift (warning), network_drift (critical) |
| **Network drift** | detect_network_drift | EP=0.400, intrinsic=0.490; contamination from 6 Ulysses episode neighbors |
| **Mismatches** | dr_mismatch | 3 perspectival_incoherence mismatches |
| **Fingerprint voids** | fingerprint_voids | drifting_without_limit, no_exit_for_victims, unaccountable_extraction |
| **Fingerprint shift** | fingerprint_shift | shift(tangled_rope, tangled_rope, rope, tangled_rope) |
| **Fingerprint zone** | fingerprint_zone | zone(extreme, extreme) |
| **Fingerprint coupling** | fingerprint_coupling | strongly_coupled, score=1.0, 2 coupled pairs |

### Structural Narrative

`ulysses_calypso_1904` is the **most fully characterized liminal constraint** in the corpus. It sits 0.016 chi-units from the snare boundary — essentially one rounding error from reclassification. The MaxEnt model reflects this: 59% tangled_rope, 39% snare. The deterministic classifier places it as tangled_rope, but it's on the knife-edge.

The Dirac orbit tells a crucial story: institutional sees rope (the negative power modifier drops chi below thresholds, and the institutional observer benefits from the constraint), while all other perspectives see tangled_rope. This single-context rope classification is what distinguishes it from a snare — from the institutional perspective, the constraint's coordination function is visible and the extraction is muted. From every other perspective, it's a tangled mess.

The drift profile is the richest in the corpus: **5 active drift events**, including network drift. The constraint is embedded in a network of Ulysses episode constraints (Aeolus, Circe, Cyclops, Lestrygonians, Lotus, Sirens) that are all contaminating each other. The effective purity (0.400) is already below the intrinsic purity (0.490), and the network drift velocity is non-zero. This is an active contamination cascade within a constraint cluster.

The temporal data shows extraction rising from 0.35 to 0.47 and theater from 0.65 to 0.75 over a 10-year span. The constraint is drifting *toward* snare, and the network contamination is accelerating the process. The 0.016 distance from the snare threshold, combined with the positive drift velocity, makes reclassification a matter of "when" not "if."

The convergence of diagnostic signals is total: MaxEnt says the boundary is near, Dirac orbits confirm perspectival instability, drift detection shows active degradation across 5 channels, network analysis reveals contamination cascading from 6 neighbors, and logical fingerprints flag three extractive voids. No single subsystem captures the full picture — but together they paint a portrait of a constraint in terminal transition.

### What to Investigate Next

1. **Cascade timeline**: Given the drift velocity and distance to snare threshold, estimate time to reclassification.
2. **Network topology**: The 6 contaminating Ulysses neighbors form a structural cluster. Is this the most interconnected constraint cluster in the corpus? Would removing the highest-contamination neighbor (Aeolus, edge_contam=0.024) slow the cascade?
3. **Institutional rope classification**: The single institutional-perspective rope classification is the only thing keeping this from being flagged as snare. Is this "institutional escape hatch" genuine (the institution genuinely benefits from the coordination) or an artifact of the negative power modifier?

---

## Cross-Comparison of the 6 Findings

### Clustering by Hypothesis Class

The 6 findings cleanly separate into two structural families:

**Family A: Metric-Structural Divergence (4 findings)**
- decentralized_infrastructure_rope, fair_use_doctrine, noethers_theorem_symmetry, reciprocity_laws_math
- All have **preserved Dirac orbits** (single type across all contexts)
- All have **entropy-flag disagreement** (not type disagreement — MaxEnt agrees on type, just with low confidence)
- All have **low extraction** (epsilon <= 0.10), **low theater** (<= 0.13), **low chi** (<= 0.14)
- All sit in the **rope-scaffold ambiguity zone**
- All have `constructed_low_extraction` signatures

**Family B: Confirmed Liminal (2 findings)**
- moltbook_agent_theater, ulysses_calypso_1904
- Both have **violated Dirac orbits** (multi-type across contexts)
- Both have **active drift events** (4-5 each, including critical-severity)
- Both have **high extraction** (epsilon >= 0.47), **high theater** (>= 0.75), **high chi** (>= 0.64)
- Both have **extractive fingerprint voids** (identical set: drifting_without_limit, no_exit_for_victims, unaccountable_extraction)
- Both have `false_ci_rope` signatures
- Both have **maximal coupling** (1.0)

### Shared Patterns Within Families

**Family A shares:**
- The rope/scaffold ambiguity pattern, where MaxEnt entropy is driven by the proximity of continuous metrics to the scaffold/rope classification boundary while boolean gates definitively resolve the deterministic classifier
- Zero or near-zero theater ratios (indicating genuine coordination, not performance)
- Lack of drift events (stable constraints — the ambiguity is definitional, not transitional)
- Zero fingerprint voids (structurally clean)

**Family B shares:**
- The tangled_rope/snare boundary proximity, with institutional-perspective rope as the only stabilizing classification
- Identical void profiles suggesting common pathological patterns
- Maximal coupling (1.0) — these constraints cannot be analyzed dimension-by-dimension
- Active temporal degradation along all measured axes

### Cross-Family Contrasts

| Dimension | Family A (divergence) | Family B (liminal) |
|-----------|----------------------|-------------------|
| Core pattern | Metric ambiguity, structural certainty | Metric ambiguity, structural instability |
| Orbit | Preserved (single type) | Violated (multi-type) |
| Drift | None or minimal | Intense, multi-channel |
| Coupling | 0.0 to 0.375 | 1.0 (maximal) |
| Extraction zone | Negligible | Extreme |
| Voids | None | Three extractive voids |
| Actionability | Low (definitional ambiguity) | High (active degradation) |

### Domain Distribution

- **Family A spans**: infrastructure (decentralized_infrastructure_rope), law (fair_use_doctrine), mathematics (noethers_theorem_symmetry, reciprocity_laws_math)
- **Family B spans**: agent/AI systems (moltbook_agent_theater), literary analysis (ulysses_calypso_1904)

The two families have **no domain overlap**. Their structural patterns are domain-agnostic — a mathematical theorem and a legal doctrine share the same ambiguity pattern; an AI agent system and a literary constraint share the same degradation pattern. This validates the core premise of trajectory pattern-mining: structural families cut across domains.

### Signal for Phase B

For trajectory mining design, the key observation is: **orbit family alone cannot distinguish the 4 Family A constraints** (all are in the [rope] orbit family, the largest single-type family with 34 members in the fingerprint report). But enriching with MaxEnt confidence distinguishes them from the other 30 members of their orbit family — they are the 4 with highest entropy within the preserved-rope orbit. Similarly, the 2 Family B constraints are in a multi-type orbit family but are distinguished by their extreme drift profiles and maximal coupling. A trajectory system that combines orbit type + MaxEnt entropy + coupling score + drift event count would successfully separate these 6 from the ~1,017 other constraints.
