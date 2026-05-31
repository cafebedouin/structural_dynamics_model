CORPUS CONTEXT: 195 constraints
  Types: 5 mountain, 13 rope, 151 tangled_rope, 23 snare, 1 piton, 1 scaffold
  Network stability: cascading | 181 omegas (164 critical)
  Confidence: 162 deep (84%) | 8 moderate (4%) | 24 borderline (12%)
  CS patterns: 148 classified | 1 anchored_fixity_brittle, 24 anchored_fixity_with_accretion, 10 diffuse_reconstruction, 2 implicit_practice, 78 interpretive_accretion, 1 marked_revision | 56 verdicts fired
  CS grounding mismatches: 105 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/generational_economic_decline.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: generational_economic_decline...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: generational_economic_decline

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: generational_economic_decline (0-30)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] generational_economic_decline from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=mountain, computed=tangled_rope
  [INDEX MISMATCH] generational_economic_decline from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=mountain, computed=tangled_rope
  [INDEX MISMATCH] generational_economic_decline from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=scaffold
  [INDEX MISMATCH] generational_economic_decline from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  generational_economic_decline:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,30,0.05,0.08)

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  generational_economic_decline -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  generational_economic_decline (ε=0.08):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.08 × 1.39 × 0.80 = 0.089
    moderate@national: d=0.650 f(d)=1.01 χ = 0.08 × 1.01 × 1.00 = 0.081
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.08 × -0.04 × 1.00 = -0.003
    analytical@global: d=0.720 f(d)=1.14 χ = 0.08 × 1.14 × 1.20 = 0.110

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: generational_economic_decline ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=scaffold  institutional=scaffold  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries,natural]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=negligible  suppression=negligible
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     1.000 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        false_summit_mountain
    Purity:           1.0 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     2 — extraction_accumulation, network_drift


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   1.0000
    Effective purity:   0.9604
    Propagation delta:  -0.0396

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | monetary_discretion_expansion | tangled_rope | shared_beneficiary | 0.30 | 0.3405 |

  Purity degraded from 1.0000 to 0.9604 by contamination from 1 neighbor(s), primarily monetary_discretion_expansion (shared_beneficiary, purity 0.3405).

--- HUSK SIGNATURE ---

  Existence:  NO  (EP does not fall: 0.9604 → 0.9604)

  Saturation boundary:    -0.4200
    full glide regime: native floor (0.15) ≥ -0.4200 — EX glides across floor
    [floor-invariant, series-determined; for generated constraints reflects
     ε authoring, not an observed property of the underlying phenomenon]

  Saturation crossover:   NOT REACHED  (max ε < 0.65 — EX was positive throughout;
                           EP decay, if any, came from F/SI/CC, not EX fall)

  EP at native floor (0.15):
    NOT cross-comparable across constraint families; native floor varies by coordination type.
    [context: powerless canonical — victim perspective]
    t=  0  EP=0.9604
    t= 15  EP=0.9604
    t= 30  EP=0.9604

--- ORBIT CONTEXT ---

  Orbit Signature:    [scaffold, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), scaffold (moderate)
    Confidence:       0.9576 (deep)
    Rival Type:       rope (P=0.0423)
    Margin:           +0.9153
    Boundary:         mountain->rope
    H^1 band:         4 — Hub 2 (effective immutability) drives a 2+2 split: powerless, analytical see tangled_rope; moderate, institutional see scaffold.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9576 (deep)
  Rival Type:    rope (P=0.0423)
  Margin:        +0.9153
  Entropy:       0.0985
  Distribution:  mountain: 0.958, rope: 0.042, scaffold: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      mountain (P=0.9570)
  Entropy:       0.0998
  Distribution:  mountain: 0.957, rope: 0.043, scaffold: 0.000

  Classical/Indexed TV Distance: 0.0006 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): not computed
  (run python3 python/sweeps/epsilon_sensitivity.py to compute)

--- ABDUCTIVE FLAGS ---

  **4 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | signature_override_artifact | 0.95 | hard_disagreement_with_override | artifact | Metric disagreement explained by a known signature override — architectural artifact, not a genuine anomaly. |
  | hub_conflict | 0.83 | hub_conflict_band | genuine | Hub 1 and Hub 2 produce conflicting classification signals at this constraint. |
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |
  | classical_oracle_failure | 0.75 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.00185733, Var_fd=0.00191768 (103.2%), Var_scope=0.00009805 (5.3%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.089148 | 1.392945 | 0.8000 |
| moderate | 0.080689 | 1.008614 | 1.0000 |
| institutional | -0.003380 | -0.042252 | 1.0000 |
| analytical | 0.109594 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (6 subsystems):
    signature, boltzmann, purity, fingerprint_voids, drift, fcr_gate

  Expected Conflicts (5):
    maxent: signature_override_artifact
      MaxEnt disagrees because signature override forces type
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(signature_override_artifact,0.95,hard_disagreement_with_override,artifact),trigger(hub_conflict,0.83,hub_conflict_band,genuine),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine),trigger(classical_oracle_failure,0.75,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  T6 (Hub Correspondence — Hub 2): H^1 = 4 maps to Hub 2 (effective immutability). Two pairs of observers disagree, producing a 2+2 classification split driven by the immutability axis.

  **5 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 30
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for generational_economic_decline
  ! ALERT [severe]: type_1_false_summit detected for generational_economic_decline

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  generational_economic_decline: false_summit_mountain (confidence: medium)
    → FALSE SUMMIT MOUNTAIN signature for generational_economic_decline: Meets all mountain metric thresholds (low extractiveness, low suppression, emerges naturally) but has 3 identifiable beneficiaries. Genuine natural laws have zero beneficiaries. This constraint has been naturalized — its constructed origin has become invisible. Coupling score=0.000.

Aggregate Magnitude (Kappa) at Tn: 0.39

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [natural_vs_constructed_decline] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether decline is natural economic law or policy-constructed outcome
  │
  │  RESOLUTION STRATEGY:
  │  1. Design measurement protocol for unknown
  │  2. Collect data from N=30+ real-world instances
  │  3. Calculate empirical metrics:
  │     - suppression_requirement (enforcement needed)
  │     - resistance_to_change (pushback level)
  │     - base_extractiveness (asymmetric benefit flow)
  │  4. Update constraint_metric/3 declarations with data
  │  5. Re-run classification to resolve perspectival gap
  └─

  ┌─ [policy_reversibility_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Policy intervention magnitude needed to reverse decline
  │
  │  RESOLUTION STRATEGY:
  │  1. Design measurement protocol for unknown
  │  2. Collect data from N=30+ real-world instances
  │  3. Calculate empirical metrics:
  │     - suppression_requirement (enforcement needed)
  │     - resistance_to_change (pushback level)
  │     - base_extractiveness (asymmetric benefit flow)
  │  4. Update constraint_metric/3 declarations with data
  │  5. Re-run classification to resolve perspectival gap
  └─

  ┌─ [beneficiary_awareness] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether beneficiaries consciously maintain or unconsciously naturalize the constraint
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive unknown
  │     - Identify source of divergence
  │  2. Gather evidence:
  │     - Empirical metrics (suppression, extraction, resistance)
  │     - Historical behavior patterns
  │  3. Create indexical classification:
  │     - From powerless context: classify as X
  │     - From institutional context: classify as Y
  │     - Add explicit context annotations
  └─

  ┌─ [demographic_vs_policy_decomposition] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Decomposition of decline into demographic vs policy components
  │
  │  RESOLUTION STRATEGY:
  │  1. Design measurement protocol for unknown
  │  2. Collect data from N=30+ real-world instances
  │  3. Calculate empirical metrics:
  │     - suppression_requirement (enforcement needed)
  │     - resistance_to_change (pushback level)
  │     - base_extractiveness (asymmetric benefit flow)
  │  4. Update constraint_metric/3 declarations with data
  │  5. Re-run classification to resolve perspectival gap
  └─

====================================================
