
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

============================================================
  FNL SHADOW MODE DIAGNOSTIC TRACE
============================================================

[STEP 1] Loading fnl_shadow_probe through pipeline...

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/fnl_shadow_probe.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: fnl_shadow_probe...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.5 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.5 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.5 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.5 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.5 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.5 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: fnl_shadow_probe...

>>> INITIATING DR-AUDIT SUITE: fnl_shadow_probe

[REPAIR] Auditing vectors for: fnl_shadow_probe...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: fnl_shadow_probe (0-10)
Checking Interval: fnl_shadow_probe (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: fnl_shadow_probe

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): tangled_rope

From ANALYTICAL perspective: tangled_rope

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX OK] fnl_shadow_probe from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] fnl_shadow_probe from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX OK] fnl_shadow_probe from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: fnl_shadow_probe ===

[CONSTRAINT INVENTORY]
  - fnl_shadow_probe: hybrid_extraction (Intensity: 0.30)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 2 | Watch: 0

  fnl_shadow_probe:
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.25,0.3)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4525,decline_signals,[coupling_above_threshold(1.0),excess_above_floor(0.15)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  fnl_shadow_probe -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  fnl_shadow_probe (ε=0.30):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.30 × 1.36 × 0.80 = 0.326
    moderate@national: d=0.700 f(d)=1.11 χ = 0.30 × 1.11 × 1.00 = 0.332
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.30 × -0.04 × 1.00 = -0.013
    analytical@global: d=0.720 f(d)=1.14 χ = 0.30 × 1.14 × 1.20 = 0.411

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: fnl_shadow_probe ===
  Shift (computed via dr_type/3):
    powerless=unknown  moderate=rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [no_exit_for_victims]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=low  suppression=high
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,global-local,1.0),coupled(power_scope,analytical,global-national,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.453 (contaminated)


--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: fnl_shadow_probe
  Claimed Type: tangled_rope
  Perspectives:
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))]: tangled_rope (Matches Claim)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))]: rope (Mismatch)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: tangled_rope (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: fnl_shadow_probe]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  fnl_shadow_probe: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for fnl_shadow_probe: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([rope,tangled_rope]),excess_above_floor(0.15),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_apparatus
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.50

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: fnl_shadow_probe
    - Individual (Powerless): tangled_rope [d=0.900 f(d)=1.36 χ=0.33]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.01 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.34 (moderate)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: fnl_shadow_probe]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_perspectival_fnl_shadow_probe (conceptual)
     Question: Constraint fnl_shadow_probe appears as tangled_rope to individuals but rope to institutions...
     Source: gap(general_type_mismatch,tangled_rope,rope)


[OMEGA TRIAGE & PRIORITIZATION]

  [high] 1 omega(s):
    - omega_perspectival_fnl_shadow_probe (conceptual)
      Constraint fnl_shadow_probe appears as tangled_rope to individuals but rope to institutions...

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_perspectival_fnl_shadow_probe] CONCEPTUAL CLARIFICATION
  │  Constraint: fnl_shadow_probe
  │  Gap: Constraint fnl_shadow_probe appears as tangled_rope to individuals but rope to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive fnl_shadow_probe
  │     - Identify source of divergence
  │  2. Gather evidence:
  │     - Empirical metrics (suppression, extraction, resistance)
  │     - Historical behavior patterns
  │  3. Create indexical classification:
  │     - From powerless context: classify as X
  │     - From institutional context: classify as Y
  │     - Add explicit context annotations
  └─

====================================================

============================================================
  DIAGNOSTIC RESULTS
============================================================

[STEP 2] Metric-based classification (pre-override):
  metric_based_type_indexed = tangled_rope

[STEP 3] Structural signature:
  constraint_signature = false_ci_rope

[STEP 4] Boltzmann compliance:
  boltzmann_compliant = non_compliant(1.0,0.3)

[STEP 5] Cross-index coupling:
  cross_index_coupling = 1.0

[STEP 6] Final classification (post-override via dr_type/3):
  dr_type = tangled_rope

============================================================
  OVERRIDE ANALYSIS
============================================================

  MetricType  = tangled_rope
  Signature   = false_ci_rope
  FinalType   = tangled_rope

  >>> NO OVERRIDE DETECTED <<<
  MetricType == FinalType. Either the override did not
  fire, or both paths agree on the same type.

============================================================
  OVERRIDE RULE STATUS
============================================================

  Rule NL  (natural_law -> mountain):      ACTIVE (maps to mountain)
  Rule FNL (false_natural_law -> tangled):  ACTIVE (maps to tangled_rope)
  Rule CIR (coupling_invariant -> rope):    ACTIVE (maps to rope)
  Rule FCR (false_ci_rope -> tangled):      ACTIVE (maps to tangled_rope)

============================================================
  END OF DIAGNOSTIC
============================================================

# Omega1 Audit: The 17 Unknown Constraints

*Investigates why 17 constraints classify as `unknown` at the*
*analytical/global context, diagnoses their blocking gates,*
*and recommends classification fixes.*

---

## Summary

- **Total corpus**: 469 constraints
- **Unknowns at analytical/global**: 17 (3.6%)
- **Unknowns at institutional/local**: 0
- **Unknowns at moderate/national**: 11
- **Moderate unknowns are a subset of analytical unknowns**: yes
- **Self-consistency check**: PASS (all 17 unknowns confirmed via classify_from_metrics/6)

## Metric Distribution of the Unknowns

| Metric | Min | Q1 | Median | Q3 | Max | Mean |
|--------|-----|----|---------|----|-----|------|
| BaseEps | 0.300 | 0.400 | 0.490 | 0.700 | 0.900 | 0.544 |
| Chi (analytical/global) | 0.411 | 0.548 | 0.671 | 0.959 | 1.233 | 0.745 |
| Suppression | 0.000 | 0.200 | 0.400 | 0.500 | 0.700 | 0.365 |
| Theater | 0.000 | 0.050 | 0.110 | 0.150 | 0.900 | 0.184 |

## Gap Region Distribution

| Region | Description | Count |
|--------|-------------|-------|
| region_a_chi_gap | Chi in (0.35, 0.40): between rope ceiling and tangled_rope floor | 0 |
| region_b_low_epsilon | Chi >= 0.40, BaseEps < 0.30: tangled_rope epsilon floor violation | 0 |
| region_c_low_suppression | Chi >= 0.40, Supp < 0.40: tangled_rope suppression floor violation | 8 |
| region_d_missing_flags | Chi >= 0.40, metrics met, missing structural flags | 5 |
| region_e_near_snare | Chi >= 0.66, BaseEps < 0.46: near-snare with low epsilon | 0 |
| region_f_other | Other / uncategorized | 4 |

### Region Membership

**region_c_low_suppression** (Chi >= 0.40, Supp < 0.40: tangled_rope suppression floor violation): 8

- `glp1_payload_efficiency_pivot` (eps=0.350, chi=0.479, supp=0.200)
- `gradient_descent_optimization` (eps=0.300, chi=0.411, supp=0.200)
- `pareto_principle` (eps=0.400, chi=0.548, supp=0.300)
- `platonic_coparenting_decoupling` (eps=0.480, chi=0.658, supp=0.350)
- `recipe_scaling_ai` (eps=0.490, chi=0.671, supp=0.250)
- `repair_probe_incomplete` (eps=0.550, chi=0.759, supp=0.000)
- `shitty_feedback_handling` (eps=0.400, chi=0.548, supp=0.300)
- `skills_based_hiring` (eps=0.300, chi=0.411, supp=0.200)

**region_d_missing_flags** (Chi >= 0.40, metrics met, missing structural flags): 5

- `gamblers_ruin_stochastic_extinction` (eps=0.900, chi=1.233, supp=0.500)
- `hawthorne_effect` (eps=0.400, chi=0.548, supp=0.500)
- `hegemonic_entropy_2026` (eps=0.720, chi=0.986, supp=0.550)
- `qwerty_vs_dvorak` (eps=0.400, chi=0.548, supp=0.700)
- `russells_paradox_self_reference` (eps=0.700, chi=0.959, supp=0.400)

**region_f_other** (Other / uncategorized): 4

- `hydra_game` (eps=0.700, chi=0.959, supp=0.500)
- `moltbook_agent_theater` (eps=0.700, chi=0.959, supp=0.450)
- `tragedy_of_the_commons` (eps=0.700, chi=0.959, supp=0.400)
- `transformer_self_attention` (eps=0.750, chi=1.027, supp=0.400)

## Blocking Gate Frequency

How many of the unknowns fail each specific gate condition:

| Gate | Count | Description |
|------|-------|-------------|
| not_mountain_supp | 16 | Supp > 0.05 |
| not_mountain_eps | 17 | BaseEps > 0.25 |
| snare_chi_below_066 | 8 | Chi < 0.66 |
| snare_eps_below_046 | 7 | BaseEps < 0.46 |
| snare_supp_below_060 | 16 | Supp < 0.60 |
| scaffold_chi_above_030 | 17 | Chi > 0.30 |
| scaffold_no_coordination | 1 | No has_coordination_function |
| scaffold_no_temporality | 7 | No sunset clause and requires enforcement |
| scaffold_theater_above_070 | 2 | Theater > 0.70 |
| rope_chi_above_035 | 17 | Chi > 0.35 |
| rope_eps_above_045 | 10 | BaseEps > 0.45 (when Chi > 0) |
| tangled_chi_above_090 | 7 | Chi > 0.90 |
| tangled_supp_below_040 | 8 | Supp < 0.40 |
| tangled_no_enforcement | 8 | No requires_active_enforcement |
| tangled_no_coordination | 1 | No has_coordination_function |
| tangled_no_asymmetry | 1 | No has_asymmetric_extraction |
| piton_chi_above_025 | 17 | Chi > 0.25 |
| piton_theater_below_070 | 15 | Theater < 0.70 |
| opaque_eps_at_or_below_045 | 7 | BaseEps <= 0.45 |
| opaque_chi_at_or_above_040 | 17 | Chi >= 0.40 |

## Primary Blocking Gate (What Each Unknown "Wants to Be")

The primary gate is the first type in the classifier chain where the constraint
almost qualifies but fails one condition.

| Primary Gate | Count | Interpretation |
|--------------|-------|----------------|
| near_tangled_no_enforcement | 2 | Would be tangled_rope but no requires_active_enforcement |
| near_tangled_supp_too_low | 6 | Would be tangled_rope but Supp < 0.40 |
| near_snare_supp_too_low | 9 | Would be snare but Supp < 0.60 |

### Detailed Breakdown

**near_tangled_no_enforcement** (Would be tangled_rope but no requires_active_enforcement): 2 constraints
- `hawthorne_effect` (eps=0.400, chi=0.548, supp=0.500, theater=0.190)
- `qwerty_vs_dvorak` (eps=0.400, chi=0.548, supp=0.700, theater=0.120)

**near_tangled_supp_too_low** (Would be tangled_rope but Supp < 0.40): 6 constraints
- `glp1_payload_efficiency_pivot` (eps=0.350, chi=0.479, supp=0.200, theater=0.120)
- `gradient_descent_optimization` (eps=0.300, chi=0.411, supp=0.200, theater=0.100)
- `pareto_principle` (eps=0.400, chi=0.548, supp=0.300, theater=0.060)
- `platonic_coparenting_decoupling` (eps=0.480, chi=0.658, supp=0.350, theater=0.150)
- `shitty_feedback_handling` (eps=0.400, chi=0.548, supp=0.300, theater=0.150)
- `skills_based_hiring` (eps=0.300, chi=0.411, supp=0.200, theater=0.110)

**near_snare_supp_too_low** (Would be snare but Supp < 0.60): 9 constraints
- `gamblers_ruin_stochastic_extinction` (eps=0.900, chi=1.233, supp=0.500, theater=0.050)
- `hegemonic_entropy_2026` (eps=0.720, chi=0.986, supp=0.550, theater=0.780)
- `hydra_game` (eps=0.700, chi=0.959, supp=0.500, theater=0.000)
- `moltbook_agent_theater` (eps=0.700, chi=0.959, supp=0.450, theater=0.900)
- `recipe_scaling_ai` (eps=0.490, chi=0.671, supp=0.250, theater=0.100)
- `repair_probe_incomplete` (eps=0.550, chi=0.759, supp=0.000, theater=0.150)
- `russells_paradox_self_reference` (eps=0.700, chi=0.959, supp=0.400, theater=0.000)
- `tragedy_of_the_commons` (eps=0.700, chi=0.959, supp=0.400, theater=0.100)
- `transformer_self_attention` (eps=0.750, chi=1.027, supp=0.400, theater=0.050)

## Cross-Context Transition Map

How each unknown is classified from different observer perspectives:

| Constraint | Institutional/Local | Moderate/National | Analytical/Global |
|------------|---------------------|-------------------|-------------------|
| gamblers_ruin_stochastic_extinction | scaffold | unknown | unknown |
| glp1_payload_efficiency_pivot | scaffold | unknown | unknown |
| gradient_descent_optimization | scaffold | rope | unknown |
| hawthorne_effect | scaffold | unknown | unknown |
| hegemonic_entropy_2026 | rope | unknown | unknown |
| hydra_game | rope | tangled_rope | unknown |
| moltbook_agent_theater | rope | tangled_rope | unknown |
| pareto_principle | rope | unknown | unknown |
| platonic_coparenting_decoupling | scaffold | unknown | unknown |
| qwerty_vs_dvorak | scaffold | unknown | unknown |
| recipe_scaling_ai | scaffold | unknown | unknown |
| repair_probe_incomplete | rope | unknown | unknown |
| russells_paradox_self_reference | scaffold | unknown | unknown |
| shitty_feedback_handling | rope | unknown | unknown |
| skills_based_hiring | scaffold | rope | unknown |
| tragedy_of_the_commons | rope | tangled_rope | unknown |
| transformer_self_attention | rope | tangled_rope | unknown |

### Transition Pattern Summary

| Institutional -> Moderate -> Analytical | Count |
|----------------------------------------|-------|
| rope -> tangled_rope -> unknown | 4 |
| rope -> unknown -> unknown | 4 |
| scaffold -> rope -> unknown | 2 |
| scaffold -> unknown -> unknown | 7 |

## Structural Flags Summary

| Flag | Present | Absent | Present % |
|------|---------|--------|-----------|
| has_coordination_function | 16 | 1 | 94.1% |
| has_asymmetric_extraction | 16 | 1 | 94.1% |
| requires_active_enforcement | 9 | 8 | 52.9% |
| emerges_naturally | 5 | 12 | 29.4% |
| natural_law_without_beneficiary | 0 | 17 | 0.0% |

## Indexically Opaque Impossibility at Analytical Context

**Claim**: The `indexically_opaque` clause (drl_core.pl:314-318) can NEVER fire at the analytical/global context.

**Proof**:
- The clause requires: `BaseEps > 0.45 AND Chi < 0.40`
- At analytical/global: `Chi = BaseEps * f(0.725) * 1.20 = BaseEps * 1.380`
- For `Chi < 0.40`: `BaseEps < 0.40 / 1.380 = 0.290`
- But `BaseEps > 0.45` is simultaneously required
- `0.290 < 0.45` -- **contradiction**

**Empirical verification**:
- Constraints satisfying BOTH BaseEps > 0.45 AND Chi < 0.40: **0** (expected: 0)
- **Confirmed**: The opaque clause is structurally impossible at analytical context.

## Structural Signature Analysis

The `dr_type/3` pipeline passes metric-based type through `integrate_signature_with_modal/3`.
When the metric layer returns `unknown`, certain signatures can rescue the classification
(e.g., `coordination_scaffold` -> `rope`, `constructed_high_extraction` -> `snare`).
The 39 constraints that remain `unknown` after this stage have signatures that do NOT rescue.

| Signature | Count |
|-----------|-------|
| false_ci_rope | 17 |

For `unknown` metric type, the rescue rules are:
- `coordination_scaffold` -> rope
- `constructed_low_extraction` -> rope
- `constructed_high_extraction` -> snare
- `constructed_constraint` -> tangled_rope
- `piton_signature` -> piton
- `ambiguous` -> unknown (no rescue)
- Any other signature with no `unknown` rescue rule -> keeps unknown

## Finding Classification

### Distribution

- Region A (chi gap): 0 (0.0%)
- Region B (low epsilon): 0 (0.0%)
- Region C (low suppression): 8 (47.1%)
- Region D (missing flags): 5 (29.4%)
- Region E (near-snare): 0 (0.0%)
- Region F (other): 4 (23.5%)

### Verdict

**Category B -- Multiple Distinct Gaps**: The unknowns split across several regions with no single dominant cause. Region A=0, B=0, C=8, D=5, E=0, F=4. Each region may need separate treatment.

### Recommendations

2. **Missing flags (Region D)**: Review these testsets and add missing structural declarations (has_coordination_function, requires_active_enforcement, has_asymmetric_extraction) where warranted by the domain.

3. **Low suppression (Region C)**: These constraints have tangled_rope chi and epsilon but suppression < 0.40. Consider whether they represent a genuine category (extraction without enforcement) or data errors.

## Embedded Prolog Facts

```prolog
%% omega1_unknown(Constraint, AnalyticalContext, PrimaryBlockingGate).
omega1_unknown(gamblers_ruin_stochastic_extinction, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(glp1_payload_efficiency_pivot, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(gradient_descent_optimization, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(hawthorne_effect, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_no_enforcement).
omega1_unknown(hegemonic_entropy_2026, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(hydra_game, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(moltbook_agent_theater, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(pareto_principle, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(platonic_coparenting_decoupling, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(qwerty_vs_dvorak, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_no_enforcement).
omega1_unknown(recipe_scaling_ai, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(repair_probe_incomplete, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(russells_paradox_self_reference, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(shitty_feedback_handling, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(skills_based_hiring, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(tragedy_of_the_commons, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(transformer_self_attention, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).

%% omega1_transition(Constraint, InstitutionalType, ModerateType, AnalyticalType).
omega1_transition(gamblers_ruin_stochastic_extinction, scaffold, unknown, unknown).
omega1_transition(glp1_payload_efficiency_pivot, scaffold, unknown, unknown).
omega1_transition(gradient_descent_optimization, scaffold, rope, unknown).
omega1_transition(hawthorne_effect, scaffold, unknown, unknown).
omega1_transition(hegemonic_entropy_2026, rope, unknown, unknown).
omega1_transition(hydra_game, rope, tangled_rope, unknown).
omega1_transition(moltbook_agent_theater, rope, tangled_rope, unknown).
omega1_transition(pareto_principle, rope, unknown, unknown).
omega1_transition(platonic_coparenting_decoupling, scaffold, unknown, unknown).
omega1_transition(qwerty_vs_dvorak, scaffold, unknown, unknown).
omega1_transition(recipe_scaling_ai, scaffold, unknown, unknown).
omega1_transition(repair_probe_incomplete, rope, unknown, unknown).
omega1_transition(russells_paradox_self_reference, scaffold, unknown, unknown).
omega1_transition(shitty_feedback_handling, rope, unknown, unknown).
omega1_transition(skills_based_hiring, scaffold, rope, unknown).
omega1_transition(tragedy_of_the_commons, rope, tangled_rope, unknown).
omega1_transition(transformer_self_attention, rope, tangled_rope, unknown).
```

---

*End of Omega1 audit*
