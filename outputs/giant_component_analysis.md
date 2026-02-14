
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
# Giant Component Analysis: Erdos-Renyi Phase Transition

*Investigates whether the constraint network exhibits a phase transition*  
*in connected component structure as coupling threshold varies.*

---


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

## Phase 1: Network Topology at Default Threshold

**Context**: analytical/global (default)  
**Coupling threshold**: 0.500

### Network Summary

| Metric | Value |
|--------|-------|
| Total nodes (constraints) | 469 |
| Connected nodes (degree > 0) | 198 |
| Isolated nodes (degree 0) | 271 |
| Edges | 316 |
| Graph density | 0.002879 |
| Average degree | 1.35 |
| Connected components | 398 |
| E-R critical edge count (n/2) | 234.5 |

### Degree Distribution

| Stat | Value |
|------|-------|
| N | 469 |
| Min | 0 |
| Q1 | 0 |
| Median | 0 |
| Q3 | 1 |
| Max | 12 |
| Mean | 0.93 |

#### Degree Histogram

| Degree Range | Count |
|-------------|-------|
| 0 (isolated) | 271 |
| 1 | 125 |
| 2-3 | 42 |
| 4-6 | 22 |
| 7-10 | 0 |
| 11-20 | 9 |
| 21+ | 0 |

### Connected Components

**398 components** found.

**Largest component**: 15 nodes (3.2% of network)

**No giant component.** The network is fragmented at this threshold.

#### Top Components by Size

| Rank | Size | Fraction |
|------|------|----------|
| 1 | 15 | 0.032 |
| 2 | 12 | 0.026 |
| 3 | 8 | 0.017 |
| 4 | 7 | 0.015 |
| 5 | 7 | 0.015 |
| 6 | 7 | 0.015 |
| 7 | 7 | 0.015 |
| 8 | 6 | 0.013 |
| 9 | 6 | 0.013 |
| 10 | 5 | 0.011 |
| 11 | 5 | 0.011 |
| 12 | 4 | 0.009 |
| 13 | 4 | 0.009 |
| 14 | 4 | 0.009 |
| 15 | 4 | 0.009 |
### Type Distribution

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 1 | 0.002 |
| rope | 48 | 0.102 |
| scaffold | 10 | 0.021 |
| tangled_rope | 159 | 0.339 |
| snare | 212 | 0.452 |
| unknown | 39 | 0.083 |

### Purity Landscape

#### Intrinsic Purity (415 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.276 |
| Q1 | 0.354 |
| Median | 0.575 |
| Q3 | 0.575 |
| Max | 1.000 |
| Mean | 0.536 |

#### Effective Purity (415 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.276 |
| Q1 | 0.354 |
| Median | 0.575 |
| Q3 | 0.575 |
| Max | 1.000 |
| Mean | 0.533 |

#### Purity Zone Distribution

| Zone | Intrinsic | Effective | Shift |
|------|-----------|-----------|-------|
| Sound (>= 0.70) | 61 | 61 | 0 |
| Borderline (0.50 - 0.70) | 170 | 169 | 1 |
| Warning (0.30 - 0.50) | 181 | 182 | -1 |
| Degraded (< 0.30) | 3 | 3 | 0 |

**1 constraints shifted purity zone** due to network contamination effects.

### Super-spreaders (Highest Contamination Potential)

| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |
|------------|------|--------|------------|------------|-----------|
| integrated_digital_governance_stack | snare | 6 | 1.00 | 0.571 | 6.00 |
| local_vs_global_optima | tangled_rope | 11 | 0.50 | 0.412 | 5.50 |
| policy_lag_catastrophe | snare | 5 | 1.00 | 0.565 | 5.00 |
| ulysses_school_1904 | snare | 4 | 1.00 | 0.373 | 4.00 |
| hypernormie_equilibrium | snare | 3 | 1.00 | 0.560 | 3.00 |
| lowenheim_skolem_theorem | tangled_rope | 5 | 0.50 | -1.000 | 2.50 |
| universal_mathematics_communication | scaffold | 11 | 0.20 | -1.000 | 2.20 |
| erised_expectation | snare | 2 | 1.00 | 0.354 | 2.00 |
| network_effects | tangled_rope | 3 | 0.50 | 0.372 | 1.50 |
| material_tensile_strength | rope | 12 | 0.10 | 0.949 | 1.20 |
| open_source_commons | rope | 11 | 0.10 | 0.949 | 1.10 |
| constraint_interaction_explosion | snare | 1 | 1.00 | 0.575 | 1.00 |
| frankenstein_creation_hubris | tangled_rope | 1 | 0.50 | 0.575 | 0.50 |
| fundamental_theorem_of_algebra | scaffold | 1 | 0.20 | -1.000 | 0.20 |
| legacy_system_technical_debt | rope | 1 | 0.10 | 0.936 | 0.10 |


---

## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)

Sweeping `network_coupling_threshold` from 0.10 to 0.90 in steps of 0.05.
For each threshold, only `inferred_coupling` edges are filtered; `explicit` and `shared_agent` edges always survive.

### Sweep Results

| Threshold | Edges | Components | Largest | Fraction |
|-----------|-------|------------|---------|----------|
| 0.100 | 316 | 398 | 15 | 0.032 |
| 0.150 | 316 | 398 | 15 | 0.032 |
| 0.200 | 316 | 398 | 15 | 0.032 |
| 0.250 | 316 | 398 | 15 | 0.032 |
| 0.300 | 316 | 398 | 15 | 0.032 |
| 0.350 | 316 | 398 | 15 | 0.032 |
| 0.400 | 316 | 398 | 15 | 0.032 |
| 0.450 | 316 | 398 | 15 | 0.032 |
| 0.500 | 316 | 398 | 15 | 0.032 |
| 0.550 | 316 | 398 | 15 | 0.032 |
| 0.600 | 316 | 398 | 15 | 0.032 |
| 0.650 | 316 | 398 | 15 | 0.032 |
| 0.700 | 316 | 398 | 15 | 0.032 |
| 0.750 | 316 | 398 | 15 | 0.032 |
| 0.800 | 316 | 398 | 15 | 0.032 |
| 0.850 | 316 | 398 | 15 | 0.032 |
| 0.900 | 316 | 398 | 15 | 0.032 |

**Erdos-Renyi prediction**: For a random graph with n=469 nodes, the giant component emerges when the number of edges exceeds n/2 = 234.5.

### Phase Transition Analysis

**Steepest jump**: threshold 0.000 (fraction=0.000) -> 0.000 (fraction=0.000), delta = 0.000

**Critical threshold (midpoint of steepest jump)**: 0.000

**Transition width**: Could not identify clean 10%->50% crossing range.

### Comparison to Erdos-Renyi Prediction

- **ER critical edge count**: 234.5 (for n=469 nodes)

**Verdict**: **No clear phase transition** (delta < 0.05). The network may be naturally resilient to cascading connectivity, or the edge types may be too heterogeneous for a clean ER transition.


---

## Phase 3: Contamination Through the Giant Component

**Threshold**: 0.500 (default)

**No significant component found** at threshold 0.500. The largest component contains fewer than 10% of nodes.

This means the network is naturally fragmented at the current coupling threshold. Contamination cannot cascade across the full network because constraints are organized into small, isolated clusters.

### Contamination at Lower Threshold

No giant component (>25% of nodes) found at any threshold from 0.10 to 0.50.
The network is inherently fragmented.

---

## Phase 4: Context Comparison

The edge set is context-independent (edges come from `affects_constraint`, `infer_structural_coupling`, and `shared_agent_link` — none of which depend on observer context). What changes across contexts is the **type classification** and hence the **contamination dynamics**.

**Fixed topology**: 316 edges, 398 components, largest = 15 nodes (threshold = 0.500)

### Type Distribution by Context

| Type | Institutional/Local | Moderate/National | Analytical/Global (default) |
|------|------|------|------|
| mountain | 0 | 0 | 1 |
| rope | 343 | 57 | 48 |
| scaffold | 36 | 12 | 10 |
| tangled_rope | 90 | 199 | 159 |
| piton | 0 | 0 | 0 |
| snare | 0 | 168 | 212 |
| indexically_opaque | 0 | 0 | 0 |
| unknown | 0 | 33 | 39 |

### Contamination Source Comparison

Number of constraints that are active contamination sources (type strength > 0, acts as contamination emitter) by context:

| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |
|---------|-------|-------|-------------|----------|---------------|
| Institutional/Local | 0 | 0 | 90 | 36 | 126 |
| Moderate/National | 168 | 0 | 199 | 12 | 379 |
| Analytical/Global (default) | 212 | 0 | 159 | 10 | 381 |

### Key Finding

Since edges are context-independent, the network topology (connected components, component sizes, degree distribution) is identical across all contexts. What changes is WHICH nodes are contamination sources. A constraint classified as a snare from one context (high contamination strength = 1.0) may be classified as a rope from another (low strength = 0.1). This means the effective contamination pressure varies by context even though the network structure does not.

---

## Embedded Prolog Facts

```prolog
%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)
gc_sweep_result(0.100, 316, 398, 15, 0.032).
gc_sweep_result(0.150, 316, 398, 15, 0.032).
gc_sweep_result(0.200, 316, 398, 15, 0.032).
gc_sweep_result(0.250, 316, 398, 15, 0.032).
gc_sweep_result(0.300, 316, 398, 15, 0.032).
gc_sweep_result(0.350, 316, 398, 15, 0.032).
gc_sweep_result(0.400, 316, 398, 15, 0.032).
gc_sweep_result(0.450, 316, 398, 15, 0.032).
gc_sweep_result(0.500, 316, 398, 15, 0.032).
gc_sweep_result(0.550, 316, 398, 15, 0.032).
gc_sweep_result(0.600, 316, 398, 15, 0.032).
gc_sweep_result(0.650, 316, 398, 15, 0.032).
gc_sweep_result(0.700, 316, 398, 15, 0.032).
gc_sweep_result(0.750, 316, 398, 15, 0.032).
gc_sweep_result(0.800, 316, 398, 15, 0.032).
gc_sweep_result(0.850, 316, 398, 15, 0.032).
gc_sweep_result(0.900, 316, 398, 15, 0.032).
```

---

*End of giant component analysis*
