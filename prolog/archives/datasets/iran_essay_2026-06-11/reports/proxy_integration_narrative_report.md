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
[SCENARIO MANAGER] Loading: testsets/proxy_integration_narrative.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: proxy_integration_narrative...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: proxy_integration_narrative

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: proxy_integration_narrative (0-35)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in proxy_integration_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 35 in proxy_integration_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in proxy_integration_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 35 in proxy_integration_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in proxy_integration_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 35 in proxy_integration_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in proxy_integration_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 35 in proxy_integration_narrative (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] proxy_integration_narrative: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  proxy_integration_narrative:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,35,0.58,0.78)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.575,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.76)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  proxy_integration_narrative -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  proxy_integration_narrative (ε=0.78):
    powerless→organized@local: d=0.850 f(d)=1.31 χ = 0.78 × 1.31 × 0.80 = 0.820
    moderate@national: d=0.700 f(d)=1.11 χ = 0.78 × 1.11 × 1.00 = 0.863
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.78 × -0.04 × 1.00 = -0.033
    analytical@global: d=0.720 f(d)=1.14 χ = 0.78 × 1.14 × 1.20 = 1.069

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: proxy_integration_narrative ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.25))
  Purity:     0.575 (borderline)




╔═══════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                          ║
║  12/12 subsystems — 2 tension(s) (abductive, signature)   ║
║  ! [informational] perspectival_incoherence (perspectival)║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)   ║
╚═══════════════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        constructed_high_extraction
    Purity:           0.575 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Live index:       power (scope=0, power=6)
    Drift events:     3 — extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning]


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.5750
    Effective purity:   0.5750
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional)
    MaxEnt P(claimed): 0.9996 (deep)
    Rival Type:       tangled_rope (P=0.0004)
    Margin:           +0.9992
    Boundary:         snare->tangled_rope
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 0.9996 (deep)
  Rival Type:    tangled_rope (P=0.0004)
  Margin:        +0.9992
  Entropy:       0.0019
  Distribution:  snare: 1.000, tangled_rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=1.0000)
  Entropy:       0.0001
  Distribution:  snare: 1.000, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0004 (near_zero)
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
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.17811587, Var_fd=0.18234604 (102.4%), Var_scope=0.00966241 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.820270 | 1.358606 | 0.8000 |
| moderate | 0.863064 | 1.106492 | 1.0000 |
| institutional | -0.032957 | -0.042252 | 1.0000 |
| analytical | 1.068546 | 1.141609 | 1.2000 |


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
    abductive: abductive_tension([trigger(maxent_shadow_divergence,0.85,shadow_override_tension,genuine),trigger(snare_leaning_tangled,0.75,high_snare_psi,genuine),trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])
    signature: override_mismatch(constructed_high_extraction,snare)

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **6 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 35
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Grid diet:      authored 0/32, injected 0, imputed 0 (OQ-93) [CONDITIONAL: grid authored 0/32]

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for proxy_integration_narrative

[STRUCTURAL SIGNATURE ANALYSIS]
  proxy_integration_narrative: constructed_high_extraction (confidence: high)
    → CONSTRUCTED HIGH-EXTRACTION signature for proxy_integration_narrative: Enforcement present (suppression=0.81, resistance=0.72) with high extraction (0.78). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  proxy_integration_narrative: R5 ZOMBIE CROSSCHECK: authored_zombie_uncorroborated

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [autonomy_measurement_ambiguity] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: proxy_integration_narrative
  │  Question: What observable organizational behaviors would definitively establish Hezbollah's strategic autonomy versus full integration, and do existing data meet that standard?
  │
  │  RESOLUTION METHOD (authored):
  │  Systematic comparison of Hezbollah strategic decisions with Iranian stated preferences across multiple domains (Lebanese domestic politics, Syrian intervention timing, ceasefire decisions, political coalition formation). Documented divergences would establish autonomy; perfect alignment would support integration.
  │
  │  IMPLICATIONS (authored):
  │  If systematic divergences are documented (as academic literature claims), the integration narrative is empirically false and its persistence is pure extraction. If perfect alignment is documented, the narrative is descriptively accurate and extraction is lower than measured.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [beneficiary_coordination_vs_extraction] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: proxy_integration_narrative
  │  Question: Do the multiple beneficiaries (Iran, Israel, Western policy) coordinate to maintain the narrative, or does it persist through independent convergent interests?
  │
  │  RESOLUTION METHOD (authored):
  │  Analysis of information operations, policy coordination channels, and narrative synchronization patterns. Evidence of coordination would establish active conspiracy; independent convergence would indicate structural incentive alignment.
  │
  │  IMPLICATIONS (authored):
  │  Coordination would increase suppression score and establish the narrative as actively maintained conspiracy. Independent convergence would suggest the narrative is an emergent property of aligned incentives, reducing agency but not extraction.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [identity_lock_mechanism] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: proxy_integration_narrative
  │  Question: Is Hezbollah's identity-locked exit due to operational dependency on Iranian support, ideological commitment to resistance axis framing, or both?
  │
  │  RESOLUTION METHOD (authored):
  │  Counterfactual analysis of alternative support sources and ideological reframing possibilities. If operational dependency alone binds, alternative patrons could break the lock. If ideological commitment binds, the lock persists regardless of material support.
  │
  │  IMPLICATIONS (authored):
  │  Pure operational dependency would make the lock contingent and potentially breakable through alternative support. Ideological fusion would make the lock structural and permanent, increasing effective extraction by eliminating even theoretical exit.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [narrative_vs_reality_gap_stability] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: proxy_integration_narrative
  │  Question: Can the integration narrative persist indefinitely despite contradictory evidence, or does accumulating divergence eventually force narrative revision?
  │
  │  RESOLUTION METHOD (authored):
  │  Historical analysis of similar geopolitical narratives that persisted despite counter-evidence. Identification of conditions under which narrative-reality gaps become unsustainable (policy failure, credibility collapse, beneficiary interest shift).
  │
  │  IMPLICATIONS (authored):
  │  If gaps can persist indefinitely, suppression is sustainable and extraction continues. If gaps eventually force revision, the constraint has a natural lifecycle limit and current extraction is time-bounded.
  │
  │  Confidence without resolution: low
  └─

====================================================
