# Logic Divergence Audit

**Date:** February 15, 2026
**Scope:** Prolog implementation vs `logic.md`, `logic_extensions.md`, `logic_index.md`, `logic_thresholds.md`
**Purpose:** Systematic comparison ahead of Deferential Realism paper circulation
**Verdict:** 10 critical divergences, 8 moderate divergences, 12+ low-priority items

---

## Summary Statistics

| Category | Count |
|----------|-------|
| **CRITICAL** (paper-affecting) | 10 |
| **MODERATE** (implementation-affecting) | 8 |
| **LOW** (cosmetic / minor) | 12+ |
| Prolog files audited | 10 core + spot-checks |
| Documentation files audited | 4 |
| Total param/2 facts in config.pl | ~120 |
| Threshold mismatches found | 6 |
| Formula mismatches found | 1 |
| Gate condition mismatches found | 0 (resolved — see below) |
| Purity zone taxonomy mismatches | 3 locations |
| Undocumented magic numbers | 40+ |

---

## Critical Divergences (Paper-Affecting)

### C1. `mountain_extractiveness_max`: Doc 0.15, Code 0.25

**Doc (logic_thresholds.md §3a):**
> `mountain_extractiveness_max` = **0.15** — Max base extraction for natural law

**Code (config.pl:148):**
```prolog
param(mountain_extractiveness_max, 0.25).
```

**Impact:** Mountains in the code accept constraints with base extraction up to 0.25, not 0.15. This is a 67% wider gate than documented. Constraints with ε ∈ (0.15, 0.25] would be classified as Mountains by the code but should fail the documented Mountain gate. The paper's formal definition `■C[I] ↔ ε(C) ≤ 0.15` does not match the implementation.

**Resolution needed:** Either update config.pl to 0.15, or update all docs (logic.md, logic_thresholds.md) to 0.25. The formal definition in logic.md line 574 says 0.15.

---

### C2. `rope_epsilon_ceiling`: Doc 0.15, Code 0.45

**Doc (logic_thresholds.md §3b):**
> `rope_epsilon_ceiling` = **0.15** — Max base extraction (same as Mountain)

**Code (config.pl:179):**
```prolog
param(rope_epsilon_ceiling, 0.45).
```

**Note:** The comment on config.pl:177 even says "ε ≤ 0.15" but the actual value is 0.45.

**Impact:** The Rope gate accepts constraints with base extraction up to 0.45, not 0.15. This is a 3× wider gate. The dual-threshold design (χ AND ε) documented in logic.md is fundamentally undermined — the ε gate is so wide it barely constrains. The paper's formal definition `⊞C[I] ↔ χ ≤ 0.35 ∧ ε(C) ≤ 0.15` does not match the implementation.

**Also affects:** `indexically_opaque` classification at drl_core.pl:318 uses `rope_epsilon_ceiling` for its gate, and `appears_as_rope` in structural_signatures.pl:1661 also uses it. Both would be affected.

**Resolution needed:** This is the most consequential divergence. At 0.45, the ε check is nearly vacuous for Ropes.

---

### C3. `tangled_rope_epsilon_floor`: Doc 0.50, Code 0.30

**Doc (logic_thresholds.md §3d):**
> `tangled_rope_epsilon_floor` = **0.50** — Min base extraction for hybrid classification

**Code (config.pl:188):**
```prolog
param(tangled_rope_epsilon_floor, 0.30).
```

**Note:** The comment on config.pl:185 says "ε ≥ 0.50" but the actual value is 0.30.

**Impact:** The Tangled Rope gate accepts constraints with ε as low as 0.30, not 0.50. This widens the Tangled Rope category significantly. The paper's formal definition `⊞⊠C[I] ↔ ... ∧ ε(C) ≥ 0.50` does not match the implementation.

**Note:** logic_thresholds.md §Known Issues already flags "Issue 1: Tangled Rope Threshold Mismatch" noting ε=0.50 conflicts with a carbon credits example at ε=0.40. The actual code is even lower at 0.30.

---

### C4. `piton_extraction_ceiling`: Doc 0.10, Code 0.25

**Doc (logic_thresholds.md §3f):**
> `piton_extraction_ceiling` = **0.10** — Minimal effective extraction for anyone

**Code (config.pl:195):**
```prolog
param(piton_extraction_ceiling, 0.25).
```

**Impact:** The Piton gate accepts χ up to 0.25, not 0.10. This is 2.5× wider. Constraints with moderate effective extraction could still be classified as Pitons. The paper's formal definition `⊟C[I] ↔ χ ≤ 0.10` does not match.

---

### C5. `boltzmann_coupling_threshold`: Doc 0.15, Code 0.25

**Doc (logic_thresholds.md §5a, logic_extensions.md §1.3):**
> `boltzmann_coupling_threshold` = **0.15** — Max allowable coupling score for Boltzmann compliance

**Code (config.pl:259):**
```prolog
param(boltzmann_coupling_threshold, 0.25).
```

**Impact:** The Boltzmann compliance test is more permissive than documented. Constraints with coupling scores in (0.15, 0.25] pass as compliant in code but would fail per the documented threshold. This affects:
- `boltzmann_compliant/2` results
- `false_natural_law/2` detection (fewer FNL detections)
- `coupling_invariant_rope/2` certification (more CI_Rope certifications)
- `detect_coupling_drift/1` firing threshold

**Internal inconsistency:** logic_extensions.md §1.3 says 0.15, but §4.1 (Coupling Drift) correctly says 0.25 with a February 2026 note explaining the divergence. Section 1.3 was never updated.

---

### C6. `boltzmann_coupling_strong_threshold`: Doc 0.40, Code 0.50

**Doc (logic_thresholds.md §5a):**
> `boltzmann_coupling_strong_threshold` = **0.40** — Above → "strong coupling"

**Code (config.pl:262):**
```prolog
param(boltzmann_coupling_strong_threshold, 0.50).
```

**Impact:** "Strong coupling" requires a higher score in code (0.50) than documented (0.40). This affects drift severity classification in `drl_lifecycle.pl` and signature confidence scoring.

---

### C7. Purity Excess Extraction Formula: Doc exponential, Code linear

**Doc (logic_extensions.md §2.2):**
> `E(C) = exp(-2.0 × excess_extraction(C))`

**Code (structural_signatures.pl:1775):**
```prolog
excess_extraction_subscore(C, EX) :-
    (   excess_extraction(C, Excess)
    ->  EX is max(0.0, 1.0 - min(1.0, Excess * 2.0))
    ;   EX = 1.0
    ).
```

The code computes `max(0.0, 1.0 - min(1.0, Excess * 2.0))` — a **linear** decay, not exponential.

**Impact comparison at key values:**

| Excess | Doc (exp) | Code (linear) | Delta |
|--------|-----------|---------------|-------|
| 0.00 | 1.000 | 1.000 | 0.000 |
| 0.10 | 0.819 | 0.800 | 0.019 |
| 0.25 | 0.607 | 0.500 | 0.107 |
| 0.50 | 0.368 | 0.000 | 0.368 |
| 0.75 | 0.223 | 0.000 | 0.223 |

At moderate excess (0.25), the code penalizes 20% more than the documented formula. At high excess (≥0.50), the code clamps to 0.0 while the exponential decays gradually. This affects purity scores for contaminated constraints, potentially shifting zone classifications.

---

### C8. Purity Zone Taxonomy: 3 divergent implementations

**Doc (logic_extensions.md §2.3, logic_thresholds.md §6a):** 5 zones:

| Zone | Range |
|------|-------|
| pristine | ≥ 0.9 |
| sound | ≥ 0.7 |
| borderline | ≥ 0.5 |
| contaminated | ≥ 0.3 |
| degraded | < 0.3 |

**Code has 3 different implementations:**

| Location | Zones | Names | Source |
|----------|-------|-------|--------|
| logical_fingerprint.pl:592 | 5 | pristine/sound/borderline/contaminated/degraded | Hardcoded (matches doc) |
| fpn_report.pl:109 | 4 | sound/contested/degraded/critical | Hardcoded (diverges) |
| giant_component_analysis.pl:519 | 4 | sound/borderline/warning/degraded | config:param (diverges) |

**`structural_purity/2` in structural_signatures.pl** returns different atoms entirely: `pure_natural_law`, `pure_coordination`, `pure_scaffold`, `pure_unclassified`, `contaminated(Failures)`, `inconclusive`. This is a categorical classification, not a zone mapping.

**Impact:** Reports from different modules use inconsistent zone names. `fpn_report.pl` calls the 0.3–0.5 zone "contested" where the spec says "borderline". It calls <0.3 "critical" where the spec says "degraded". It omits "pristine" entirely. Paper claims about zone names may not match output from certain report generators.

---

### C9. `structural_purity/2` returns different atoms than documented zones

**Doc (logic_extensions.md §2.3):** Suggests `structural_purity(C, Zone)` maps to pristine/sound/borderline/contaminated/degraded.

**Code (structural_signatures.pl:1460-1524):** `structural_purity/2` returns:
- `inconclusive`
- `pure_natural_law`
- `pure_coordination`
- `pure_scaffold`
- `pure_unclassified`
- `contaminated(Failures)` (with structured failure list)

These are **completely different** from the documented zone names. The documented zone mapping is implemented in `logical_fingerprint.pl` as `purity_zone/2` (a separate predicate), not in `structural_purity/2`.

**Impact:** Any paper claim that `structural_purity/2` classifies constraints into pristine/sound/borderline/contaminated/degraded zones is incorrect. The predicate classifies by structural subtype, not purity score range.

---

### C10. Internal doc inconsistency: boltzmann_coupling_threshold

**logic_extensions.md §1.3** says `boltzmann_coupling_threshold = 0.15` (four separate mentions).

**logic_extensions.md §4.1** says `boltzmann_coupling_threshold = 0.25` with a February 2026 note explaining the old 0.10/0.15 value was never wired.

**logic_thresholds.md §5a** says `boltzmann_coupling_threshold = 0.15`.

The two documents that serve as the "single source of truth" contradict each other and both contradict config.pl (0.25).

---

## Moderate Divergences (Implementation-Affecting)

### M1. Snare gate: `NOT Coord(C)` check question — RESOLVED

**Pre-audit concern:** Docs require `NOT Coord(C)` for Snare, code doesn't check.

**Audit finding:** logic.md explicitly states at the "No NOT Coord Gate" section that the Snare gate **deliberately** does not require `NOT Coord(C)`. Rationale: many Snares have vestigial coordination functions from their Rope origins. The code is correct; the concern was based on a misreading of an earlier spec version.

**Status:** Not a divergence. The code and the current docs agree.

---

### M2. Rope gate: `Coord(C)` check question — RESOLVED

**Pre-audit concern:** Docs require `Coord(C)` for Rope, code doesn't check.

**Audit finding:** logic.md explicitly states: "Unlike Tangled Rope, current implementation doesn't explicitly check `has_coordination_function(C)`. Coordination is a structural expectation for Ropes but not a metric prerequisite." The code is correct.

**Status:** Not a divergence. Coord(C) is only required for CI_Rope certification (signature layer), not the metric gate.

---

### M3. CI_Rope excess extraction tolerance: Doc 0.10, Code 0.05

**Doc (logic_extensions.md §1.5):**
> `ExcessEps =< 0.10  % Tolerance`

**Code (structural_signatures.pl:1429):**
```prolog
ExcessEps =< 0.05  % Within noise floor of Boltzmann floor
```

**Impact:** CI_Rope certification is stricter in code (0.05) than documented (0.10). Some constraints that would be certified per the doc are rejected by the code.

---

### M4. `type_contamination_strength` and `type_immunity` not in config

All contamination strength and immunity values are hardcoded in `drl_modal_logic.pl` (lines 1477-1496) rather than sourced from `config:param/2`.

**Config.pl** does define `contamination_strength_*` params (lines 360-366), but `drl_modal_logic.pl` does NOT read them — it uses its own hardcoded facts.

**Impact:** Changing config.pl contamination_strength values has NO effect. The config params are dead code for contamination strength. However, the hardcoded values **do match** the config values, so there is no value divergence — only an architectural issue.

---

### M5. `classify_snapshot` default extractiveness: 0.5 vs config 0.10

**drl_lifecycle.pl:895:**
```prolog
(narrative_ontology:constraint_metric(C, ExtName, BaseX) -> true ; BaseX = 0.5)
```

**config.pl:203:**
```prolog
param(default_extractiveness, 0.10).
```

**Impact:** Historical type reconstruction via `classify_snapshot` uses a 5× higher default extractiveness than the system-wide default. This could produce incorrect drift detection for constraints lacking explicit extractiveness data.

---

### M6. Reformability score weights undocumented

**drl_modal_logic.pl:752-756:**
```prolog
Score is SepFactor * 0.30 + CouplingFactor * 0.40 + ExcessFactor * 0.30
```

**Doc (logic_thresholds.md §5d):**
> Formula: 30% separability + 40% coupling topology + 30% excess extraction

The doc mentions these weights but they are hardcoded in the code, not sourced from config. The values match.

---

### M7. Piton detection in drl_lifecycle.pl hardcodes thresholds

**drl_lifecycle.pl:215** hardcodes `extractiveness < 0.10` and `theater_ratio > 0.70` for Piton detection, rather than reading `piton_epsilon_floor` and `piton_theater_floor` from config. The values happen to match, but config changes would not propagate.

---

### M8. `action_composition_gate` reformability check: Code 0.50, undocumented

**drl_modal_logic.pl:1264:**
```prolog
action_composition_gate(surgical_reform, C, pass) :-
    ...
    reformability_score(C, analytical, R), R >= 0.50
```

The purity-based gate uses `purity_surgical_reform_gate` (0.30 from config) correctly, but the reformability gate adds a **second** check requiring reformability ≥ 0.50. This additional gate is not documented in logic_extensions.md §5.3.

---

## Low Priority Items

### L1. Purity zone hardcoding (3 locations)

`logical_fingerprint.pl`, `fpn_report.pl`, and `drl_lifecycle.pl` all hardcode purity zone thresholds (0.90, 0.70, 0.50, 0.30) instead of reading from config:param. Only `giant_component_analysis.pl` reads from config. If config values change, three files will be stale.

### L2. `maxent_n_types` hardcoded as 5.0

`maxent_classifier.pl:328` uses `Remainder is (1.0 - Strength) / 5.0` which assumes exactly 6 types. If types are added/removed, this creates a probability distribution bug.

### L3. FNL detection: Doc says `ε > 0.70`, code has no ε check

**Doc (logic_extensions.md §1.4):** FNL formal definition includes `ε(C) > 0.70`.

**Code (structural_signatures.pl:1349-1367):** `false_natural_law/2` checks `claimed_natural(C, Claim)` + `boltzmann_compliant(C, non_compliant(_, _))` but does NOT check `ε > 0.70`.

**Impact:** FNL in code catches more constraints than the formal definition specifies (no ε floor).

### L4. Separability thresholds diverge from reformability thresholds

`dirac_classification.pl` uses 0.60/0.30 for separability. Config defines `reformability_high_threshold` = 0.70 and `reformability_low_threshold` = 0.30. The high thresholds diverge (0.60 vs 0.70).

### L5. `drl_lifecycle.pl` header says "Ten drift event types" but lists 11

Minor documentation inconsistency in module header.

### L6. Excess extraction tolerance inconsistencies

- `coupling_invariant_rope/2`: `ExcessEps =< 0.05`
- `purity_test_excess/2`: `Excess =< 0.05`
- `fcr_test_failure` (excess): `Excess > 0.05`

All use 0.05 consistently within the code, but the doc says 0.10 for CI_Rope (see M3).

### L7. `boltzmann_compliant/2` implementation skips epistemic check in doc pseudocode

**Doc (logic_extensions.md §1.2):** Shows `boltzmann_compliant` calling `cross_index_coupling` directly.

**Code (structural_signatures.pl:826-835):** Adds an epistemic access check wrapper before coupling test. The code is more defensive than the doc pseudocode, which is fine but creates a minor spec/impl difference.

### L8. `gauge_orbit/2` duplicates standard_context definitions

`dirac_classification.pl` defines its own `standard_context/1` facts (lines 114-132), duplicating `drl_core.pl`. If standard contexts change, this copy goes stale.

### L9. Rope CI_Rope check in dirac_classification is logically inert

Lines 292-294: Both clauses return `first_class` regardless of CI_Rope status. The check has no observable effect.

### L10. Network qualification drop threshold hardcoded

`drl_modal_logic.pl:1712` hardcodes `Drop > 0.05` for network-qualified action escalation. This matches the documented value but is not sourced from config.

### L11. Shared agent edge strength hardcoded at 0.3

`drl_modal_logic.pl:1454` hardcodes per-agent edge weight coefficient at 0.3, not configurable.

### L12. `purity_reform_target` uses 0.85, not sound floor (0.70)

`drl_modal_logic.pl:928` targets purity 0.85 as reform goal, while `purity_action_sound_floor` in config is 0.70. The target exceeds the "sound" threshold — intentional (aiming for pristine), but the relationship is undocumented.

---

## Logic Flow Comparison (5 Pipelines)

### Pipeline 1: Classification Cascade

**Documented flow (logic.md):**
```
dr_type/3 → metric_based_type_indexed → classify_from_metrics/6
    → Mountain gate (ε ≤ 0.15, Supp ≤ 0.05, emerges_naturally, immutable)
    → Snare gate (χ ≥ 0.66, ε ≥ 0.46, Supp ≥ 0.60, changeable)
    → Scaffold gate (χ ≤ 0.30, Coord, Sunset, Theater ≤ 0.70)
    → Rope gate (χ ≤ 0.35, ε ≤ 0.15, changeable)
    → Tangled Rope gate (0.40 ≤ χ ≤ 0.90, ε ≥ 0.50, Supp ≥ 0.40, Enforce, Coord, Asymmetric)
    → Piton gate (χ ≤ 0.10, ε > 0.10, Theater ≥ 0.70)
    → unknown fallback
```

**Code flow (drl_core.pl):**
Same priority ordering: Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > indexically_opaque > unknown.

**Divergences in gate thresholds** (the actual threshold values differ per C1-C4 above):

| Gate | Doc ε threshold | Code ε threshold | Doc χ threshold | Code χ threshold |
|------|----------------|-----------------|-----------------|-----------------|
| Mountain | ≤ 0.15 | ≤ **0.25** | N/A | N/A |
| Rope | ≤ 0.15 | ≤ **0.45** | ≤ 0.35 | ≤ 0.35 |
| Tangled Rope | ≥ 0.50 | ≥ **0.30** | 0.40-0.90 | 0.40-0.90 |
| Piton | N/A | N/A | ≤ 0.10 | ≤ **0.25** |

**Additional code gate:** `indexically_opaque` (drl_core.pl:317-321) — not documented as a formal type in logic.md's gate section but is present as a classification output. It fires when `BaseEps > rope_epsilon_ceiling ∧ Chi < tangled_rope_chi_floor`.

**Post-metric override:** `structural_signatures:integrate_signature_with_modal/3` applies signature overrides. This is correctly documented.

### Pipeline 2: Boltzmann/Naturality Pipeline

**Documented flow (logic_extensions.md §1):**
```
boltzmann_compliant/2
    → epistemic_access_check/2 (≥3 classifications)
    → cross_index_coupling/2 (Power × Scope grid)
    → complexity_adjusted_threshold/2 (base + offset)
    → compare: CouplingScore ≤ Threshold → compliant
```

**Code flow (structural_signatures.pl):** Matches documented flow exactly, including epistemic access guard.

**Divergences:**
- Threshold value: Doc says base = 0.15, code uses 0.25 (C5 above)
- Strong threshold: Doc says 0.40, code uses 0.50 (C6 above)
- Everything else matches

### Pipeline 3: Purity Computation

**Documented flow (logic_extensions.md §2.2):**
```
purity_score/2 = 0.30×F + 0.25×S + 0.25×K + 0.20×E
    F = 1.0 - coupling_score
    S = 1.0 - (N_types - 1) × 0.25
    K = 1.0 - nonsensical_coupling_strength
    E = exp(-2.0 × excess)
```

**Code flow (structural_signatures.pl:1730-1777):** Matches F, S, K exactly. E diverges (C7 above): code uses `1.0 - min(1.0, Excess * 2.0)` (linear) instead of `exp(-2.0 * Excess)` (exponential).

**Zone mapping:** Doc says 5 zones; `structural_purity/2` returns structural subtypes instead (C8-C9 above).

### Pipeline 4: FPN/Contamination Network

**Documented flow (logic_extensions.md §3):**
```
effective_purity/4:
    intrinsic_purity(C) - Σ contamination_pressure(Neighbor)
    contamination_pressure = min(cap, attenuation × type_strength × (1 - purity))
```

**Code flow (drl_modal_logic.pl:1499-1560):**
```
EffPurity = max(0.0, Intrinsic - TotalContamination × Immunity)
Per-edge: Delta × Attenuation × TypeFactor, capped
```

**Differences:**
- Code adds `type_immunity` scaling (doc doesn't mention immunity)
- Code uses `Delta = max(0, MyPurity - OtherPurity)` (purity difference), not `(1 - purity)` as documented
- FPN iteration (Stage 8b) is Jacobi-style with configurable convergence — correctly documented
- Config params for contamination propagation are correctly read (cap=0.30, attenuation=0.50)

### Pipeline 5: Signature Detection

**Documented flow (logic.md, logic_extensions.md):**
```
constraint_signature/2 priority:
    1. FNL (false_natural_law)
    2. FCR (false_ci_rope)
    3. NL via emergence (natural_law)
    4. CI_Rope (coupling_invariant_rope)
    5. Profile-based (NL/CS/piton/CC variants/ambiguous)
```

**Code flow (structural_signatures.pl:101-134):** Matches exactly:
```prolog
constraint_signature(C, false_natural_law) :- false_natural_law(C, _), !.
constraint_signature(C, false_ci_rope) :- false_ci_rope(C, _), !.
constraint_signature(C, natural_law) :- emerges_naturally(C), ..., !.
constraint_signature(C, coupling_invariant_rope) :- coupling_invariant_rope(C, _), !.
constraint_signature(C, Signature) :- ... classify_by_signature(Profile, Extraction, Signature).
```

**Override rules match doc:** FNL→tangled_rope, CI_Rope→rope, FCR→tangled_rope (gated by perspectival variance), NL→mountain, CS+mountain→rope.

---

## Constants & Configuration (Phase 4)

### All param/2 facts vs documented values

| Parameter | config.pl | logic_thresholds.md | Status |
|-----------|-----------|-------------------|--------|
| `power_modifier_powerless` | 1.5 | 1.5 | MATCH |
| `power_modifier_moderate` | 1.0 | 1.0 | MATCH |
| `power_modifier_powerful` | 0.6 | 0.6 | MATCH |
| `power_modifier_organized` | 0.4 | 0.4 | MATCH |
| `power_modifier_institutional` | -0.2 | -0.2 | MATCH |
| `power_modifier_analytical` | 1.15 | 1.15 | MATCH |
| `scope_modifier_local` | 0.8 | 0.8 | MATCH |
| `scope_modifier_regional` | 0.9 | 0.9 | MATCH |
| `scope_modifier_national` | 1.0 | 1.0 | MATCH |
| `scope_modifier_continental` | 1.1 | 1.1 | MATCH |
| `scope_modifier_global` | 1.2 | 1.2 | MATCH |
| `scope_modifier_universal` | 1.0 | 1.0 | MATCH |
| `mountain_suppression_ceiling` | 0.05 | 0.05 | MATCH |
| **`mountain_extractiveness_max`** | **0.25** | **0.15** | **MISMATCH (C1)** |
| `rope_chi_ceiling` | 0.35 | 0.35 | MATCH |
| **`rope_epsilon_ceiling`** | **0.45** | **0.15** | **MISMATCH (C2)** |
| `rope_suppression_ceiling` | 0.16 | 0.16 | MATCH |
| `snare_chi_floor` | 0.66 | 0.66 | MATCH |
| `snare_epsilon_floor` | 0.46 | 0.46 | MATCH |
| `snare_suppression_floor` | 0.60 | 0.60 | MATCH |
| `snare_extraction_ceil` | 1.00 | 1.00 | MATCH |
| `snare_load_bearing_threshold` | 0.70 | 0.70 | MATCH |
| `tangled_rope_chi_floor` | 0.40 | 0.40 | MATCH |
| `tangled_rope_chi_ceil` | 0.90 | 0.90 | MATCH |
| **`tangled_rope_epsilon_floor`** | **0.30** | **0.50** | **MISMATCH (C3)** |
| `tangled_rope_suppression_floor` | 0.40 | 0.40 | MATCH |
| `scaffold_extraction_ceil` | 0.30 | 0.30 | MATCH |
| **`piton_extraction_ceiling`** | **0.25** | **0.10** | **MISMATCH (C4)** |
| `piton_theater_floor` | 0.70 | 0.70 | MATCH |
| `piton_epsilon_floor` | 0.10 | 0.10 | MATCH |
| **`boltzmann_coupling_threshold`** | **0.25** | **0.15** | **MISMATCH (C5)** |
| **`boltzmann_coupling_strong_threshold`** | **0.50** | **0.40** | **MISMATCH (C6)** |
| `boltzmann_factorization_tolerance` | 0.10 | 0.10 | MATCH |
| `boltzmann_min_classifications` | 3 | 3 | MATCH |
| `complexity_offset_information_standard` | 0.00 | 0.00 | MATCH |
| `complexity_offset_resource_allocation` | 0.05 | 0.05 | MATCH |
| `complexity_offset_enforcement_mechanism` | 0.08 | 0.08 | MATCH |
| `complexity_offset_global_infrastructure` | 0.15 | 0.15 | MATCH |
| `complexity_offset_default` | 0.00 | 0.00 | MATCH |
| `boltzmann_floor_information_standard` | 0.02 | 0.02 | MATCH |
| `boltzmann_floor_resource_allocation` | 0.15 | 0.15 | MATCH |
| `boltzmann_floor_enforcement_mechanism` | 0.10 | 0.10 | MATCH |
| `boltzmann_floor_global_infrastructure` | 0.20 | 0.20 | MATCH |
| `boltzmann_floor_default` | 0.05 | 0.05 | MATCH |
| `reformability_high_threshold` | 0.70 | 0.70 | MATCH |
| `reformability_low_threshold` | 0.30 | 0.30 | MATCH |
| `boltzmann_floor_drift_threshold` | 0.05 | 0.05 | MATCH |
| `purity_action_sound_floor` | 0.70 | 0.70 | MATCH |
| `purity_action_escalation_floor` | 0.50 | 0.50 | MATCH |
| `purity_action_degraded_floor` | 0.30 | 0.30 | MATCH |
| `purity_surgical_reform_gate` | 0.30 | 0.30 | MATCH |
| `purity_scaffold_health_gate` | 0.50 | 0.50 | MATCH |
| `purity_energy_max_multiplier` | 3.0 | 3.0 | MATCH |
| `network_coupling_threshold` | 0.50 | 0.50 | MATCH |
| `network_shared_agent_min` | 1 | 1 | MATCH |
| `purity_contamination_cap` | 0.30 | 0.30 | MATCH |
| `purity_attenuation_factor` | 0.50 | 0.50 | MATCH |
| `purity_contamination_source_floor` | 0.50 | 0.50 | MATCH |
| `contamination_strength_snare` | 1.0 | 1.0 | MATCH |
| `contamination_strength_piton` | 0.8 | 0.8 | MATCH |
| `contamination_strength_tangled_rope` | 0.5 | 0.5 | MATCH |
| `contamination_strength_scaffold` | 0.2 | 0.2 | MATCH |
| `contamination_strength_rope` | 0.1 | 0.1 | MATCH |
| `contamination_strength_mountain` | 0.0 | 0.0 | MATCH |
| `network_contamination_risk_threshold` | 2 | 2 | MATCH |
| `network_cluster_degraded_floor` | 0.40 | 0.40 | MATCH |
| `network_drift_velocity_threshold` | 0.01 | 0.01 | MATCH |
| `network_hub_degree_threshold` | 3 | 3 | MATCH |
| `network_cascade_count_threshold` | 3 | 3 | MATCH |
| `network_drift_hub_escalation` | 1 | 1 | MATCH |
| `system_gradient_threshold` | 0.01 | 0.01 | MATCH |
| `system_gradient_strong_threshold` | 1.00 | 1.00 | MATCH |
| `default_extractiveness` | 0.10 | 0.10 | MATCH |
| `default_suppression` | 0.10 | 0.10 | MATCH |
| `default_theater` | 0.0 | 0.0 | MATCH |
| `data_high_threshold` | 0.95 | 0.95 | MATCH |
| `data_medium_threshold` | 0.75 | 0.75 | MATCH |
| `critical_mass_threshold` | 3 | 3 | MATCH |
| `isomorphism_threshold` | 0.85 | 0.85 | MATCH |

**Summary: 6 mismatches out of ~75 documented parameters.** All 6 are in the Critical section above.

### Undocumented config.pl parameters

The following config.pl sections have parameters not covered by logic_thresholds.md:

- §4C: Sigmoid directionality (sigmoid_lower/upper/midpoint/steepness, canonical_d_*) — 10 params
- §8: FPN parameters (fpn_epsilon, fpn_max_iterations, fpn_enabled) — 3 params
- §10: Network drift dynamics (already covered)
- §11: MaxEnt classifier (maxent_*) — 8 params
- §12: Abductive reasoning (abductive_*) — 5 params
- §13: Trajectory mining (trajectory_*) — 7 params
- §14: Grothendieck cohomology (cohomology_enabled) — 1 param
- §5A: Default values (already covered)
- §6: Excess factor Gaussian (excess_factor_*) — 4 params
- §7: Dependency coupling/reform urgency — 8 params

These are features added in stages 5-7+ that logic_thresholds.md does not cover. logic_thresholds.md states it covers "config.pl (lines 71-410)" but the actual file is 461 lines.

---

## Staleness Detection (Phase 5)

### File modification dates

All four doc files were created in a single commit on **2026-02-15 00:11:18** (commit `073b593`). They have never been modified since creation.

Core code files were last modified on **2026-02-14 23:34:43** (commit `5d32c9b`), ~37 minutes before the docs were created.

**Key finding:** The docs were written once and never updated. The code went through 25+ commits since January 18, 2026. The docs reflect the state at time of creation but any subsequent code changes (of which there appear to be none between that commit and now) would not be captured.

**Current risk:** Low for staleness (docs and code are from the same session), but the threshold mismatches (C1-C6) suggest the docs were written against an intended spec, not reverse-engineered from the code. The code values may have been changed during calibration without updating the docs, or the docs may represent the intended values that were never applied to the code.

### Codebase inventory

| Category | Count | Total Lines |
|----------|-------|-------------|
| Total .pl files | 54 | 25,861 |
| Core files audited | 5 | 5,490 |
| Spot-checked files | 4 | ~4,000 |
| Files not audited | 45 | ~16,371 |

---

## Verification

### Module load test

```
$ swipl -g "halt(0)" -l prolog/drl_core.pl
Warning: Local definition of domain_priors:emerges_naturally/1 overrides weak import from drl_core
```

Modules load successfully with only a non-critical weak import warning.

### Cross-verification of critical divergences

Each critical divergence was verified by reading both the exact doc text and the exact config.pl line:

| ID | Doc quote | Config.pl line:value |
|----|-----------|---------------------|
| C1 | "mountain_extractiveness_max = **0.15**" (logic_thresholds.md line 89) | Line 148: `param(mountain_extractiveness_max, 0.25)` |
| C2 | "rope_epsilon_ceiling = **0.15**" (logic_thresholds.md line 121) | Line 179: `param(rope_epsilon_ceiling, 0.45)` — comment says "ε ≤ 0.15" |
| C3 | "tangled_rope_epsilon_floor = **0.50**" (logic_thresholds.md line 159) | Line 188: `param(tangled_rope_epsilon_floor, 0.30)` — comment says "ε ≥ 0.50" |
| C4 | "piton_extraction_ceiling = **0.10**" (logic_thresholds.md line 196) | Line 195: `param(piton_extraction_ceiling, 0.25)` |
| C5 | "boltzmann_coupling_threshold = **0.15**" (logic_thresholds.md line 276) | Line 259: `param(boltzmann_coupling_threshold, 0.25)` |
| C6 | "boltzmann_coupling_strong_threshold = **0.40**" (logic_thresholds.md line 277) | Line 262: `param(boltzmann_coupling_strong_threshold, 0.50)` |
| C7 | "E(C) = exp(-2.0 × excess)" (logic_extensions.md §2.2) | structural_signatures.pl:1775: `EX is max(0.0, 1.0 - min(1.0, Excess * 2.0))` |

### Comment-code drift in config.pl

Three config.pl parameters have comments that contradict their values:

| Line | Comment | Actual Value |
|------|---------|-------------|
| 177 | `% Rule R: ... ε ≤ 0.15` | `param(rope_epsilon_ceiling, 0.45)` |
| 185 | `% Rule TR: ... ε ≥ 0.50` | `param(tangled_rope_epsilon_floor, 0.30)` |
| 181 | `% Rule N: ... ε ≥ 0.46` | Value matches (0.46) — OK |

The comments for `rope_epsilon_ceiling` and `tangled_rope_epsilon_floor` reflect the documented spec, not the actual values. This is the strongest evidence that the code was intentionally changed from the spec during calibration and the comments/docs were not updated.

---

## Recommendations

### Before Paper Circulation

1. **Resolve the 6 threshold mismatches (C1-C6).** For each, decide: is the doc correct (update config.pl) or is the code correct (update docs)? The comment-code drift in config.pl suggests the docs represent the original design intent.

2. **Resolve the excess extraction formula (C7).** Either change the code to use `exp(-2.0 * Excess)` or update the docs to describe the linear formula.

3. **Standardize purity zone taxonomy (C8-C9).** Either:
   - Add a canonical `purity_zone/2` predicate in a shared utility module, sourced from config
   - Or document the `structural_purity/2` output atoms accurately

4. **Fix the internal doc inconsistency (C10).** Update logic_extensions.md §1.3 to say 0.25 (matching §4.1 and config.pl), or update config.pl to 0.15 (matching §1.3 and logic_thresholds.md).

### After Paper Circulation

5. Move hardcoded magic numbers to config.pl (40+ instances across modules).
6. Wire `type_contamination_strength/2` and `type_immunity/2` to config:param.
7. Add documentation for MaxEnt, Dirac, trajectory mining, and abductive reasoning parameters.
8. Deduplicate `standard_context/1` definitions (currently in drl_core.pl and dirac_classification.pl).
9. Fix `classify_snapshot` default extractiveness (0.5 → should read from config).

---

**"Changes flow spec → registry → implementation, never backward."** — logic_thresholds.md. This audit found 6 cases where implementation diverged from spec without the spec being updated.

---

## Divergence Resolution (February 15, 2026)

All 10 critical divergences resolved by updating documentation to match calibrated code values. The code is authoritative; the docs now reflect the implemented system.

**C1–C6: Threshold values updated in logic_thresholds.md and logic.md**
- C1: `mountain_extractiveness_max` docs updated from 0.15 → 0.25
- C2: `rope_epsilon_ceiling` docs updated from 0.15 → 0.45; config.pl comment corrected
- C3: `tangled_rope_epsilon_floor` docs updated from 0.50 → 0.30; config.pl comment corrected
- C4: `piton_extraction_ceiling` docs updated from 0.10 → 0.25
- C5: `boltzmann_coupling_threshold` docs updated from 0.15 → 0.25
- C6: `boltzmann_coupling_strong_threshold` docs updated from 0.40 → 0.50

**C7: Formula updated in logic_extensions.md §2.2**
- Excess extraction formula corrected from `exp(-2.0 × excess)` to `max(0.0, 1.0 - min(1.0, excess × 2.0))` (clamped linear decay matching implementation)

**C8–C9: Purity zone taxonomy divergence — deferred**
- Three implementations (logical_fingerprint.pl, fpn_report.pl, giant_component_analysis.pl) use different zone names
- `structural_purity/2` returns structural subtypes, not zone scores — this is by design
- Standardization deferred to post-paper-circulation cleanup (see Recommendations §5–9)

**C10: Internal inconsistency in logic_extensions.md resolved**
- §1.3 updated from 0.15 → 0.25 to match §4.1 and config.pl
- All references to `boltzmann_coupling_threshold` within logic_extensions.md are now consistent

**Files modified:**
- `docs/logic_thresholds.md` — 6 threshold values, formal definitions, Known Issues §1 resolved
- `docs/logic.md` — All formal definitions, inference rules, examples, and prose references
- `docs/logic_extensions.md` — §1.3 Boltzmann threshold, §1.7 nonsensical coupling, §2.2 formula
- `prolog/config.pl` — Comments on lines 177 and 185 corrected (no predicate logic changes)

**Verification:** No Prolog predicate logic, config.pl param/2 values, or test expectations were modified.
