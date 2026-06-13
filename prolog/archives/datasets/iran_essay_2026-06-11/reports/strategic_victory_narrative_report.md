CORPUS CONTEXT: 7 constraints
  Types: 4 mountain, 1 tangled_rope, 2 snare
  Network stability: stable | 0 omegas (0 critical)
  MaxEnt bands (corpus): 2 deep (29%) | 0 moderate (0%) | 5 borderline (71%)
  CS grounding mismatches: 0 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/strategic_victory_narrative.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: strategic_victory_narrative...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: strategic_victory_narrative

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: strategic_victory_narrative (0-25)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in strategic_victory_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 25 in strategic_victory_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in strategic_victory_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 25 in strategic_victory_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in strategic_victory_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 25 in strategic_victory_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in strategic_victory_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 25 in strategic_victory_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] strategic_victory_narrative: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  strategic_victory_narrative:
    [warning | confidence: low] metric_substitution
        Evidence: evidence(theater_delta,0,25,0.38,0.58)
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,25,0.52,0.68)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.4291666666666667,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.63)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  strategic_victory_narrative -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  strategic_victory_narrative (ε=0.68):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.68 × 0.65 × 0.80 = 0.354
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.350 f(d)=0.29 χ = 0.68 × 0.29 × 1.00 = 0.198
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: strategic_victory_narrative ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=snare  institutional=naturalized  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.750, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(0.75,0.25))
  Purity:     0.429 (contaminated)




╔═══════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                          ║
║  12/12 subsystems — 2 tension(s) (abductive, signature)   ║
║  ! [informational] perspectival_incoherence (perspectival)║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)   ║
╚═══════════════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        constructed_high_extraction
    Purity:           0.429167 (contaminated)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Live index:       both (scope=4, power=2)
    Drift events:     4 — metric_substitution [warning], extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning]
    Tangled psi:      0.9990 (snare_leaning)
    Coalition:        split_field


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4292
    Effective purity:   0.4292
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, snare, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), snare (moderate), naturalized (institutional)
    MaxEnt P(claimed): 0.0007 (borderline)
    Rival Type:       snare (P=0.9993)
    Margin:           -0.9986
    Boundary:         tangled_rope->snare
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: moderate, analytical → snare; powerless → tangled_rope; institutional → naturalized.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: snare at P=0.9993 (pipeline says tangled_rope)
  MaxEnt P(claimed): 0.0007 (borderline)
  Rival Type:    snare (P=0.9993)
  Margin:        -0.9986
  Entropy:       0.0033
  Distribution:  snare: 0.999, tangled_rope: 0.001, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.9999)
  Entropy:       0.0006
  Distribution:  snare: 1.000, tangled_rope: 0.000, piton: 0.000

  Classical/Indexed TV Distance: 0.0006 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): not computed
  (run python3 python/sweeps/epsilon_sensitivity.py to compute)

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_shadow_divergence | 0.85 | shadow_override_tension | genuine | MaxEnt strongly favors a type different from signature override target — override may mask metric-preferred classification. |
  | classical_oracle_failure | 0.78 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.08715318, Var_fd=0.07624161 (87.5%), Var_scope=0.00878278 (10.1%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.353600 | 1.358606 | 0.8000 |
| moderate | 0.752415 | 1.106492 | 1.0000 |
| institutional | 0.198142 | 0.291386 | 1.0000 |
| analytical | 0.931553 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    maxent, purity, dirac, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (3):
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (2):
    abductive: abductive_tension([trigger(maxent_shadow_divergence,0.85,shadow_override_tension,genuine),trigger(snare_leaning_tangled,0.75,high_snare_psi,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])
    signature: override_mismatch(constructed_high_extraction,snare)

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Both Hubs): H^1 >= 5 means both Hub 1 (power-scaled extraction) and Hub 2 (effective immutability) contribute to classification fracture. Three or more distinct types appear across observers.

  **6 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 25
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Grid diet:      authored 0/32, injected 0, imputed 0 (OQ-93) [CONDITIONAL: grid authored 0/32]

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for strategic_victory_narrative

[STRUCTURAL SIGNATURE ANALYSIS]
  strategic_victory_narrative: constructed_high_extraction (confidence: high)
    → CONSTRUCTED HIGH-EXTRACTION signature for strategic_victory_narrative: Enforcement present (suppression=0.72, resistance=0.71) with high extraction (0.68). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [coordination_vs_delusion] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: strategic_victory_narrative
  │  Question: Is the strategic victory narrative a functional coordination mechanism that genuinely aligns constituencies around a defensible long-term strategy, or is it a collective delusion that prevents the regime from adjusting to strategic failure?
  │
  │  RESOLUTION METHOD (authored):
  │  Longitudinal outcome analysis: if the narrative enables the regime to sustain regional influence and domestic stability over decades despite costs, the coordination function is real; if the narrative leads to catastrophic miscalculation or collapse, it was extractive self-deception.
  │
  │  IMPLICATIONS (authored):
  │  If the narrative is functional coordination, the extraction is the price of holding a coalition together under adversity; if it is delusion, the extraction is pure cost with no offsetting benefit, and the constraint should reclassify toward snare.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [suppression_internalization] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: strategic_victory_narrative
  │  Question: Is the measured suppression primarily structural (external enforcement preventing access to contradictory information) or internalized (domestic constituencies genuinely believe the narrative and suppress their own doubts)?
  │
  │  RESOLUTION METHOD (authored):
  │  Post-regime-change information access: if suppression persists after enforcement machinery is removed, it was substantially internalized; if it collapses immediately, it was structural.
  │
  │  IMPLICATIONS (authored):
  │  If internalized, the constraint's effective suppression is higher than the structural measure suggests, and the identity_locked exit option for domestic_regime_stability is more binding than it appears. If purely structural, the constraint is more fragile than the suppression metric indicates.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [victory_metric_ambiguity] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: strategic_victory_narrative
  │  Question: What constitutes 'strategic victory' in the Iranian framing: is it a coherent alternative metric (influence, deterrence, axis cohesion) or is it an unfalsifiable claim that redefines success post-hoc to match whatever outcomes occur?
  │
  │  RESOLUTION METHOD (authored):
  │  Falsifiability test: identify what observable outcomes the Iranian state would accept as strategic defeat; if no such outcomes exist, the victory claim is unfalsifiable and purely extractive.
  │
  │  IMPLICATIONS (authored):
  │  If the victory metric is coherent and falsifiable, the narrative has genuine epistemic content and the coordination function is stronger; if unfalsifiable, the narrative is pure performance and the theater_ratio should be higher.
  │
  │  Confidence without resolution: medium
  └─

====================================================
