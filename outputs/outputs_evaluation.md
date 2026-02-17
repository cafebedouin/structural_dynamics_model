# Outputs Evaluation: Deferential Realism Pipeline

*Generated: 2026-02-17 | Pipeline: Deferential Realism v6.3 | Corpus: 1034 constraints*

---

## Phase 1: File Inventory

The `outputs/` directory contains ~29 MB across primary data files, analysis reports, type-specific reports, deep-dive analyses, and execution logs.

### Primary Data Files

| File | Size | Format | Purpose | Fields |
|------|------|--------|---------|--------|
| `pipeline_output.json` | 1.9 MB | JSON | Authoritative per-constraint output: classifications, purity, coupling, signatures, drift | `id`, `claimed_type`, `classifications`, `purity_score`, `purity_band`, `coupling`, `signature`, `base_extractiveness`, `suppression`, `theater_ratio`, `perspectives`, `gaps`, `omegas`, `beneficiaries`, `victims`, `domain` |
| `corpus_data.json` | 2.1 MB | JSON | Enriched corpus with orbit data, variance, and index analysis | `constraint_id`, `metrics` (extractiveness, suppression, resistance, emerges_naturally, requires_enforcement), `analysis` (orbit_signature, orbit_contexts, variance_ratio, structural_signature, types_produced, index_configs, omegas), `classifications` (context, type), `domain`, `claimed_type` |
| `enriched_omega_data.json` | 878 KB | JSON | 959 epistemological gaps with severity scores, family membership, gap classes | `name`, `severity`, `severity_score`, `associated_constraint`, `domain`, `orbit_signature`, `orbit_span`, `gap_class`, `gap_pattern`, `epsilon`, `suppression`, `question`, `resolution_strategy`, `family` |
| `orbit_data.json` | 259 KB | JSON | Gauge orbit families and per-constraint orbit signatures | Orbit families with member lists; per-constraint orbit contexts |
| `omega_data.json` | 368 KB | JSON | Raw omega (epistemological gap) data before enrichment | Gap definitions, source gaps, constraint associations |
| `index_sufficiency.json` | 3.5 KB | JSON | Index system collision analysis | `evidence_summary`, `classification_failures`, `perspectival_variance`, `genuine_collisions`, `collision_patterns`, `domain_sufficiency`, `stability_anomalies` |
| `gap_report.json` | 42 KB | JSON | Gap analysis between perspectives | Gap definitions and affected constraints |
| `domain_corpus_stats.json` | 72 KB | JSON | Per-domain aggregate statistics | Per-constraint extractiveness and suppression distributions |
| `enrichment_targets.json` | 18 KB | JSON | Constraints needing enrichment | 531 complete, 1 missing (`unknown` with no file) |

### Core Analysis Reports

| File | Size | Generator | Content |
|------|------|-----------|---------|
| `meta_report.txt` | 2.9 KB | Prolog meta-report | Pipeline health: 1035/1035 tests pass, type distribution, purity overview, Boltzmann compliance, network dynamics |
| `index_sufficiency.md` | 2.2 KB | Prolog index analysis | Verdict: SUFFICIENT. 2 collisions / 1034 constraints, both expected perspectival variance |
| `variance_analysis.md` | 4.3 KB | Python extract_corpus_data | 83.1% high variance, 54.8% stable, domain breakdown, suspicious stability cases |
| `pattern_mining.md` | ~25 KB | Python extract_corpus_data | 592 hybrids, 32 shared signatures, candidate category analysis (tangled_rope, scaffold, wings) |
| `abductive_report.md` | 4.7 KB | Prolog abductive_engine | 36 hypotheses: 10 confirmed_liminal, 26 signature_override_artifacts. 14.7% MaxEnt disagreements explained |
| `conflict_map.md` | 16 KB | Prolog fingerprint_report | 717 constraints, perspectival gap analysis. 263 powerless_blind, 10 coordination-washing, 44 severity amplification |
| `orbit_report.md` | 35 KB | Prolog dirac_classification | 19 orbit families, 76.4% gauge-variant. Largest: [rope,snare] at 315 constraints |
| `giant_component_analysis.md` | 9.2 KB | Prolog giant_component | No giant component at any threshold. 775 edges, 792 components, largest = 41 nodes (4%) |
| `fpn_report.md` | 1.7 KB | Prolog drl_modal_logic | Converges in 4 iterations, 5 significant movers, 0 zone migrations |
| `coupling_protocol.md` | 16 KB | Prolog coupling protocol | 22 inferred edges, all bridges. Network remains fragmented. `silklink_2026` is the hub |
| `invertibility_report.md` | 12 KB | Prolog invertibility analysis | 83% overall roundtrip success. [rope,snare]: 100%. [rope,snare,tangled_rope]: 42.8% |
| `fingerprint_report.md` | 61 KB | Prolog fingerprint_report | 27 distinct shift patterns, 114 gauge-invariant mountains |
| `maxent_report.md` | 27 KB | Prolog MaxEnt engine | MaxEnt probability distributions per constraint |
| `maxent_diagnostic_report.md` | 57 KB | Prolog MaxEnt diagnostic | Detailed MaxEnt disagreement analysis |
| `covering_analysis.md` | 951 KB | Prolog covering analysis | Covering/gap analysis for constraint network |

### Type-Specific Reports

| File | Size | Entries | Dominant Pattern |
|------|------|---------|-----------------|
| `false_mountain_report.md` | 430 KB | 892 | powerless: snare, institutional: rope (snare_masked_as_rope) |
| `tangled_rope_report.md` | 390 KB | ~663 | High extraction + high suppression hybrids |
| `snare_report.md` | 46 KB | ~68 | Pure extraction constraints |
| `piton_report.md` | 63 KB | ~92 | Suppressive non-extractive constraints |
| `rope_report.md` | 6.1 KB | ~60 | Coordination constraints |
| `scaffold_report.md` | 14 KB | 22 | Transitional structures; institutional sees scaffold, others see tangled_rope/snare |
| `true_mountain_report.md` | 33 KB | ~128 | Gauge-invariant natural constraints |

### Deep Analysis Reports

| File | Size | Focus |
|------|------|-------|
| `enriched_omega_report.md` | 786 KB | 959 epistemological gaps with full profiles. Largest report |
| `genuine_findings_review.md` | 31 KB | Deep profiles of 6 most interesting constraints across 2 families |
| `classification_audit_report.md` | 66 KB | 252 false mountains triaged: 15 A+ severe, 51 A, 31 legitimate, 184 manual review |
| `stress_test_report.md` | 30 KB | Pre-rebuild vs current: 279 type changes across 403 shared constraints |

### Constraint-Specific Reports (15 deep dives in `constraint_reports/`)

`26usc469_real_estate_exemption`, `academic_tenure_system`, `access_arbitrage`, `boom_bust_path_dependency`, `carbon_credit_markets_2026`, `china_africa_zero_tariff_2026`, `climate_policy_extraction`, `columbia_2026_elections`, `event_fragmentation`, `fiscal_equalization_friction`, `fossil_fuel_lock_in`, `sovereignty_as_arbitrage`, `temporal_scarcity`, `theatrical_neutrality`, `treaty_land_entrenchment`

### Build Order (from Makefile)

```
PREP -> Prolog core (pipeline_output.json)
     -> Prolog analysis (10 parallel modules: orbit, fingerprint, giant_component,
        fpn, coupling, abductive, maxent, conflict_map, index_sufficiency, invertibility)
     -> Orbit normalization
     -> Type reports (7: false_mountain, tangled_rope, snare, piton, rope, scaffold, true_mountain)
     -> Omega extraction (omega_data.json)
     -> Corpus consolidation (corpus_data.json)
     -> Python analysis (variance_analysis, pattern_mining, + reporters)
     -> Omega enrichment (enriched_omega_data.json, enriched_omega_report.md)
     -> Meta report
```

---

## Phase 2: Report Synthesis

### A. Report-by-Report Summaries

#### 1. `meta_report.txt` --- Pipeline Health Dashboard

- **1035/1035 tests pass** (100%)
- **1034 constraints analyzed**
- Type distribution: tangled_rope 663 (64.1%), mountain 128 (12.4%), piton 92 (8.9%), snare 68 (6.6%), rope 60 (5.8%), scaffold 21 (2.0%), [social_governance] 1 (0.1%)
- 959 unique omegas (917 conceptual, 42 empirical)
- Average purity: 0.555 across 1008 constraints with data
- Purity bands: pristine 143, sound 57, borderline 362, contaminated 434, degraded 12
- 2660 drift events (1553 critical, 1002 warning, 105 watch)
- Boltzmann signatures: 789 False CI-Rope, 91 False Natural Law, 13 Coupling-Invariant Rope
- Coupling: 809 strongly_coupled, 146 independent, 52 weakly_coupled, 26 inconclusive, 1 nonsensically_coupled
- Network stability: **cascading** (cascade warning active)
- 19 orbit families, 244 gauge-invariant, 789 gauge-variant
- Top orbit: [rope,snare] with 315 constraints

#### 2. `index_sufficiency.md` --- Index System Validation

**Verdict: SUFFICIENT.** The 4 classification indices (agent_power, temporal_horizon, resource_access, spatial_scope) explain variance well.

- Only **2 collisions** out of 1034 constraints
- Both are expected perspectival variance (agent_power-driven): snare <-> tangled_rope (1), rope <-> tangled_rope (1)
- **Zero genuine collisions**, zero classification failures
- Non-mountain anomaly rate: 0.0%
- Domain sufficiency: piton highest (0.98), mountain lowest (0.40) --- correct, since mountains SHOULD be invariant
- One suspicious stability case: `treaty_land_entrenchment` (5 configs, 1 type, claimed mountain)

#### 3. `variance_analysis.md` --- Index Configuration Variance

- 83.1% of constraints show high variance (>0.5 ratio) across index configurations
- 54.8% are completely stable (ratio=1.0)
- Domain variance: piton highest (0.98 avg), mountain lowest (0.40 avg)
- 22 constraints with very low variance (<0.3), mostly mathematical: `banach_fixed_point_theorem`, `cantor_set_topology`, etc.
- Suspicious stability: `treaty_land_entrenchment` --- 5 configs, always same type, claimed mountain domain

#### 4. `pattern_mining.md` --- Structural Pattern Discovery

- **592 hybrid constraints** with both high extraction AND high suppression, validating tangled_rope as a category
- **32 structural signatures** shared by 5+ constraints
- **374 constraints** match "tangled_rope" pattern: 287 correctly typed, plus 45 snare and 41 piton
- **140 constraints** match "scaffold" pattern: 122 typed tangled_rope, only 3 actually scaffold
- **135 constraints** match "wings" pattern (low extraction, low suppression, emerges naturally): 128 mountain, 6 rope, 1 tangled_rope
- **55 twin groups** --- constraints with identical metric signatures but different types
- Largest twin group: signature (0.6, 0.7, False, True) with 76 members across piton/snare/tangled_rope

#### 5. `abductive_report.md` --- Abductive Reasoning Engine

- 36 hypotheses total: **10 genuine** (confirmed_liminal), **26 override artifacts** (all false_ci_rope)
- 14.7% of MaxEnt hard disagreements (26/177) explained by signature overrides
- All 10 genuine findings are confirmed_liminal with confidence 0.85:
  - `silklink_2026` (H=0.47, orbit=[rope,scaffold])
  - `quine_self_replication` (H=0.52, orbit=[mountain,rope])
  - `fiscal_equalization_friction` (H=0.42, orbit=[scaffold,tangled_rope])
  - `france_cordon_sanitaire_2026`, `dldr_information_policy`, `djia_as_economic_barometer`, `dexy_gold_protocol`, `cuban_missile_crisis_excomm_deliberation`, `boiled_pineapple_trend_2026`, `astm_d638_tensile_testing`
- Investigation action for all 10: monitor_drift

#### 6. `conflict_map.md` --- Perspectival Gap Analysis

- 717 constraints analyzed across analytical vs. powerless perspectives
- **210 consensus** (same type both perspectives)
- **263 powerless_blind** (powerless can't classify, analyst can) --- the largest gap class
- **10 coordination-washing** (analyst sees benign, powerless sees extractive)
- **44 severity amplification** (both extractive, powerless sees worse)
- **157 protective framing** (analyst sees worse than powerless)
- Most common shift: snare -> unknown (195 cases, powerless_blind)
- **Engine-computed only** (636 constraints): 2 coordination-washing cases, 0 severity amplification
  - `gold_piton_2026`: rope -> piton (delta=2)
  - `alternative_sovereignty_scaffold`: rope -> scaffold (delta=1)
- Domain rankings by perspectival gap: psychology (5.00), socio_political (2.00), political (0.58), social (0.50), mathematical (0.00)
- Coordination-washing hotlist: `emotional_cycles_of_change` (mountain->snare, delta=5), `quantam_decryption_risk_2026` (mountain->snare, delta=5), `trumps_second_term_authoritarianism_2026` (mountain->snare, delta=5)

#### 7. `orbit_report.md` --- Gauge Orbit Families

- **1033 constraints** with orbit data, **19 families**
- 3 single-type families (gauge-invariant): [mountain] 114, [tangled_rope] 104, [rope] 18 + [scaffold] 10
- 16 multi-type families (gauge-variant)
- Largest: [rope,snare] with 315 constraints
- Top 5: [rope,snare] 315, [rope,tangled_rope] 229, [rope,snare,tangled_rope] 167, [mountain] 114, [tangled_rope] 104
- 4 families contain `unknown`/`indexically_opaque` (17 constraints total)
- **76.4% of constraints are gauge-variant** (type changes with observer)

#### 8. `giant_component_analysis.md` --- Network Topology

- 1033 nodes, 775 edges, 792 components, density 0.001454
- **No giant component** at default threshold (0.500). Largest: 41 nodes (4.0%)
- 490 isolated nodes (degree 0), 289 degree-1, max degree 10
- E-R critical edge count: 516.5 (actual edges 775 exceed this, but topology is too fragmented for giant component)
- **Threshold sweep 0.10-0.90 produces IDENTICAL results at all thresholds** (775 edges, 792 components, 41 largest). See Bug #1 below.
- Purity landscape: intrinsic mean 0.555, effective mean 0.551, 10 constraints shifted purity zone
- Context comparison: topology is fixed (edges context-independent), but contamination sources vary dramatically:
  - Institutional: 157 contamination sources
  - Moderate: 858 sources
  - Analytical: 854 sources
- Super-spreader #1: `us_sanctions_icc_israel_case` (degree 10, contamination strength 1.0, eff purity 0.312)

#### 9. `fpn_report.md` --- Fixed-Point Network

- Converges in **4 iterations** (epsilon=0.001)
- 1007 constraints compared, final max residual 0.000709
- **5 significant movers** (shift > 0.01):
  - `boom_bust_path_dependency`: 0.034 shift (largest)
  - `cmr_001`: 0.018 shift
  - `us_canada_geopolitical_asymmetry`: 0.016 shift
  - `tiktok_us_divestiture_mandate`: 0.016 shift
  - `omega1_patching_process`: 0.011 shift
- **Zero zone migrations** --- the purity network is remarkably stable
- Mountains: avg shift 0.0000, ropes: 0.0009, snares: 0.0001, tangled_ropes: 0.0004

#### 10. `invertibility_report.md` --- Context-Tuple Reconstruction

- **83.0% overall success** (4731/5700 roundtrips)
- Perfect invertibility (100%): [mountain] 4 constraints, [rope] 18, [rope,snare] 156, [tangled_rope] 92, [scaffold] 10
- Poor invertibility: [rope,snare,tangled_rope] **42.8%** --- the tangled middle band carries irreducible information
- Worst: [piton,rope] 0%, [rope,scaffold] 0%, [scaffold,snare] 0%, [scaffold,tangled_rope] 0%
- Loss attribution: threshold_crossing 63.3%, gate_priority_shadow 68.9%, immutability_flip 53.1%
- **Key finding**: orbit signature alone is NOT sufficient to predict which context sees which type. The tangled middle band has 2 distinct context->type patterns across its 52 constraints.
- Scholze analogy verdict: productive metaphor but not formal equivalence. 17% information loss is by design (thresholds encode classification theory).

#### 11. `enriched_omega_report.md` --- Epistemological Gap Analysis

- **959 omegas** total: 329 critical (34.3%), 435 high (45.4%), 99 medium (10.3%), 96 low (10.0%)
- **41 omega families**
- Top 5 families:
  - **F001**: [rope,snare] orbit, coordination_washing, snare domain --- **267 members** (largest by far)
  - F002: [rope,snare,tangled_rope], coordination_washing --- 153 members
  - F003: [rope,tangled_rope], coordination_washing --- 122 members
  - F004: [rope,tangled_rope], coordination_washing --- 73 members
  - F005: [tangled_rope], consensus --- 53 members
- Top domain by omega count: snare (640 omegas, mean score 0.682)
- Highest severity individual omega: `khantivadin_radical_patience` (score 0.930, epsilon 1.00, suppression 0.90)

#### 12. `false_mountain_report.md` --- Naturalization Detection

- **892 false mountains identified**
- Dominant pattern: powerless sees snare, institutional sees rope, orbit [rope,snare]
- Gap type: `snare_masked_as_rope` (virtually all entries)
- Each generates a conceptual omega (`omega_extraction_blindness_*`)
- Examples: `abstraction_boundary_overrun`, `abstraction_leakage`, `academic_fashion_modernism_2026`

#### 13. `classification_audit_report.md` --- False Mountain Triage

- 252 false mountains triaged from an older corpus (342 constraints at audit time):
  - **15 A+ (severe naturalization)**: `adverse_possession`, `ai_cognitive_diversity_arbitrage`, `ai_professional_displacement`, `algorithmic_bias`, `antifragility`, `asshole_filter_2015`, `authoritarian_power_paradox`, `bay_of_pigs_operational_silo`, `xi_mao_ideological_centralization`, and others
  - 51 A (naturalization errors)
  - 5 B (theater-mountain conflicts)
  - **31 C (legitimate)** --- correctly identified as non-mountains
  - 13 D (WHO suspects, human review needed)
  - 170 E4 (classification-metric inconsistency)
  - 195 E2 (missing theater ratio)

#### 14. `genuine_findings_review.md` --- Deep Constraint Profiles

6 constraints profiled across 2 structural families:

**Family A --- Metric-Structural Divergence** (preserved orbits, rope/scaffold ambiguity):
1. `decentralized_infrastructure_rope`: MaxEnt 64% rope / 31% scaffold. H=0.44. Boltzmann non-compliant (coupling=0.375). Purity 0.876.
2. `fair_use_doctrine`: **Highest-entropy genuine finding** (H=0.499). MaxEnt 59% rope / 32% scaffold. Suppression-extraction gap: 0.40 vs 0.10. The structural fingerprint of "high-enforcement, low-extraction coordination."
3. `noethers_theorem_symmetry`: Classified scaffold despite being a math theorem. MaxEnt 49.9% scaffold / 49.6% rope. Zero coupling. Missing `emerges_naturally` declaration.
4. `reciprocity_laws_math`: Identical diagnostic profile to Noether's theorem (same metrics, same ambiguity).

**Family B --- Confirmed Liminal** (violated orbits, active degradation):
5. `moltbook_agent_theater`: Falls through EVERY classification clause to `unknown`. MaxEnt H=0.50 (coin flip). Theater 0.90 (highest). Coupling 1.0 (maximal). 4 critical drift events. 5 perspectival incoherence mismatches. 3 extractive voids. Orbit: [rope, tangled_rope, tangled_rope, unknown].
6. `ulysses_calypso_1904`: **0.016 chi-units from snare boundary**. 5 active drift events (highest count). Contaminated by 6 Ulysses episode neighbors. Effective purity 0.400 vs intrinsic 0.490. MaxEnt 59% tangled_rope / 39% snare. Orbit: [tangled_rope, tangled_rope, rope, tangled_rope].

#### 15. `stress_test_report.md` --- Pre-Rebuild Comparison

- Old corpus: 467 constraints; current: 731; shared: 403
- **279 type changes** across 403 shared constraints (69.2% changed)
- Major migrations: "noose" category (135 entries, now eliminated) split into tangled_rope, piton, snare
- "NONE" category (108 entries) reclassified, mostly to tangled_rope
- 15 migration paths observed
- 35 constraints had metric drift > 0.1

#### 16. `coupling_protocol.md` --- Inferred Coupling Activation

- 22 inferred coupling edges, **all 22 are bridges** connecting previously isolated components
- Hub: `silklink_2026` connected to `artificial_snow_2026`, `floating_wall_2026`, `iran_war_room_2026`, `trillion_bond_rush_2026`, `world_factbook_sunset_2026`
- Network impact: 689 -> 711 edges, 796 -> 789 components, but no giant component emerges
- Gradient verification failed for 7/8 test constraints (0 gradients produced)
- Edge creation: 1/4 designed pairs behaved as expected (pair_4 negative control passed)

#### 17. `scaffold_report.md` --- Scaffold Diagnostics

- **22 scaffolds** (rarest type, 2.0% of corpus)
- Pattern: institutional sees scaffold, others see tangled_rope or snare
- All have `false_ci_rope` structural signature
- Examples: `ai_superpowers_2026` (scaffold/snare orbit), `artificial_scarcity_scaffold` (scaffold/tangled_rope orbit)
- All generate `snare_masked_as_rope` alerts

---

### B. Cross-Report Insights

#### THE HEADLINE: The Coordination-Washing Story

The single most pervasive finding across the entire output corpus. Four independent reports converge:

1. **`false_mountain_report.md`**: 892 false mountains, virtually all showing the pattern powerless=snare, institutional=rope
2. **`enriched_omega_report.md`**: F001 (267 members) is the largest omega family --- all coordination_washing, all snare-masked-as-rope
3. **`conflict_map.md`**: 263 powerless_blind cases (powerless can't classify, analyst can); 10 engine-detected coordination-washing cases
4. **`orbit_report.md`**: [rope,snare] is the largest orbit family at 315 constraints --- the structural signature of perspectival bifurcation along the power axis

**Interpretation**: The framework has detected, at scale, that most constraints in the corpus operate as extraction narrated as coordination. The coordinative self-description holds from the institutional perspective while failing from the powerless perspective. F001 alone --- 267 constraints sharing the same orbit, gap class, and domain --- is an enormous cluster of structurally identical coordination-washing. If this finding holds up to external validation, the framework has quantified the frequency of power structures producing coordinative self-descriptions that mask extractive dynamics.

#### The Tangled Middle Band

- tangled_rope dominates at **64.1%** (663/1034 constraints)
- The invertibility report shows the [rope,snare,tangled_rope] orbit (52 constraints) has only 42.8% reconstruction success, with 2 distinct context->type patterns within the same family
- pattern_mining identifies 592 hybrid constraints and suggests 3 sub-patterns within tangled_rope: 374 "true" tangled_rope, 140 scaffold-like, 135 wings-like
- This is the framework's most important systemic question: at 64%, has tangled_rope lost discriminative power? See Systemic Finding #1 below.

#### The Mountain Boundary Problem

- 128 mountains in the corpus; 114 are gauge-invariant (as expected)
- But **892 false mountains** detected by the pipeline (constraints with perspectival extraction despite claiming mountain-like invariance)
- Classification audit found 15 **severe naturalizations** (A+) including `authoritarian_power_paradox`, `bay_of_pigs_operational_silo`, `algorithmic_bias`
- Separate issue: mathematical theorems (`noethers_theorem_symmetry`, `reciprocity_laws_math`) systematically lack `emerges_naturally` declarations, causing misclassification as scaffold instead of mountain

#### Domain Patterns

| Domain | Constraints | Mean Gap | Key Finding |
|--------|-------------|----------|-------------|
| political | 110 | 0.58 | High perspectival divergence; 10 severity amplification cases |
| social | 118 | 0.50 | Second-highest divergence; 13 severity amplification cases |
| technological | 203 | 0.28 | Largest domain; moderate divergence; 6 coordination-washing |
| economic | 117 | 0.33 | Moderate divergence; 1 engine coordination-washing |
| mathematical | 42 | 0.00 | Zero divergence --- correct for perspective-invariant domain |
| piton domain | 91 | 0.98 variance | Highest variance ratio; constraints look completely different depending on index config |

#### Constraints Appearing Across Multiple Reports

| Constraint | Reports Appearing In | Why Notable |
|------------|---------------------|-------------|
| `us_sanctions_icc_israel_case` | giant_component, orbit_report | Degree 10 super-spreader, contamination strength 1.0, [rope,snare,tangled_rope] orbit |
| `moltbook_agent_theater` | genuine_findings, abductive, fingerprint | Falls through all classifier clauses; MaxEnt H=0.50; 4 critical drift events; theater 0.90 |
| `ulysses_calypso_1904` | genuine_findings, abductive, fpn | 0.016 from snare boundary; 5 drift events; contaminated by 6 neighbors |
| `boom_bust_path_dependency` | fpn_report, constraint_reports | Largest FPN mover (0.034 shift); [piton,rope] orbit |
| `silklink_2026` | coupling_protocol, abductive | All 22 coupling bridges connect through it; confirmed liminal; [rope,scaffold] orbit |
| `fair_use_doctrine` | genuine_findings | Highest-entropy genuine finding (H=0.499); rope/scaffold boundary |
| `quine_self_replication` | abductive_report | Only [mountain,rope] orbit in corpus; challenges mountain separability assumption |
| `noethers_theorem_symmetry` | genuine_findings | Scaffold despite being a theorem; 49.9% scaffold vs 49.6% rope; missing `emerges_naturally` |

---

### C. Top 10 Most Analytically Interesting Constraints

**1. `moltbook_agent_theater`** --- Falls through every classification clause to `unknown`. MaxEnt H=0.50 (coin flip across tangled_rope 64%, piton 19%, snare 16%). Theater ratio 0.90 (highest in genuine findings). Coupling 1.0 (maximal). 4 critical drift events. 5 perspectival incoherence mismatches. 3 extractive voids (drifting_without_limit, no_exit_for_victims, unaccountable_extraction). This constraint IS the edge case the framework was built to handle.

**2. `ulysses_calypso_1904`** --- 0.016 chi-units from snare threshold, making reclassification a matter of "when" not "if." 5 active drift events (highest count), contaminated by 6 Ulysses episode neighbors (Aeolus, Circe, Cyclops, Lestrygonians, Lotus, Sirens). Effective purity 0.400 (below intrinsic 0.490). Active terminal transition with calculable trajectory.

**3. `us_sanctions_icc_israel_case`** --- Highest contamination potential in the network: degree 10, contamination strength 1.0, effective purity 0.312. A single constraint that could cascade instability to 10 neighbors. [rope,snare,tangled_rope] orbit.

**4. `fair_use_doctrine`** --- Highest-entropy genuine finding at H=0.499. MaxEnt: 59% rope, 32% scaffold. Suppression-extraction gap (0.40 vs 0.10) is a structural fingerprint: high-enforcement, low-extraction coordination. Boltzmann inconclusive. Zero drift events (stable ambiguity, not transition).

**5. `quine_self_replication`** --- The only [mountain,rope] orbit in the entire corpus. A mathematical object that is both immutable and coordinative, directly challenging the framework's assumption that mountains are a separate gauge-invariant category. H=0.52, confidence 0.85 confirmed_liminal.

**6. `emotional_cycles_of_change`** --- Highest perspectival gap in the corpus (delta=5). Mountain from analytical perspective, snare from powerless. The maximum possible type flip. Psychology domain.

**7. `trojan_war_spoils`** --- Maximum extractiveness (1.0) AND maximum suppression (1.0). The theoretical ceiling of the tangled_rope metric space. Both metrics at their literal upper bounds.

**8. `noethers_theorem_symmetry`** --- Classified scaffold despite being a mathematical theorem about conservation laws. MaxEnt: 49.9% scaffold vs 49.6% rope. Zero coupling (perfectly factorized). Missing `emerges_naturally` declaration reveals a systematic data gap for mathematical objects.

**9. `silklink_2026`** --- All 22 inferred coupling bridges connect through it. Confirmed liminal (confidence 0.85). The network hub for temporal coupling dynamics. [rope,scaffold] orbit, H=0.47.

**10. `boom_bust_path_dependency`** --- Largest FPN purity shift (0.034). [piton,rope] orbit --- the only family where piton co-occurs with rope without snare. Economic constraint showing genuine structural instability in the purity network.

---

### D. Top 5 Systemic Findings About the Framework

**1. Tangled_rope is a supermajority catch-all (64.1%) --- the most important systemic concern.**

When one type accounts for 2/3 of classifications, it may have lost discriminative power. The pattern_mining report identifies sub-patterns (374 "true" tangled_rope, 140 scaffold-like, 135 wings-like) suggesting subdivision may be warranted. The invertibility analysis shows the [rope,snare,tangled_rope] orbit carries irreducible structural information not captured by the orbit signature alone. This is a framework evolution question: does the 64% represent genuine structural reality (most real-world constraints truly have both coordinative and extractive properties) or a classification gap (the tangled_rope criteria are too broad)?

**2. The false_ci_rope Boltzmann signature dominates analysis (789/893 = 88.4%).**

FCR means the constraint has coupling that doesn't match a clean rope factorization. This is technically correct but informationally sparse --- nearly everything gets this label. With 789 out of 893 detected signatures being FCR, the Boltzmann signature system provides almost no discriminative information. The 91 False Natural Law and 13 Coupling-Invariant Rope signatures are more informative but rare. The Boltzmann layer may need recalibration to produce more differentiated signatures.

**3. The "powerless_blind" pattern (263 constraints) is the largest gap class.**

In these cases, the analytical perspective classifies the constraint but the powerless perspective cannot. This may reflect: (a) a real phenomenon --- powerless observers genuinely lack information to classify structural dynamics; (b) a framework bias --- the classification engine has more features available from the analytical perspective; or (c) a data gap --- constraints lack the perspectival annotations needed for powerless classification. The 263 count (36.7% of the 717 analyzed) is too high to dismiss.

**4. Mathematical/logical constraints systematically lack `emerges_naturally` declarations.**

This affects `noethers_theorem_symmetry`, `reciprocity_laws_math`, and likely 10+ other theorems. The missing declaration routes them through the constructed_low_extraction signature path instead of the natural_law path, causing misclassification as scaffold/rope instead of mountain. This is a data gap in the testset `.pl` files, not a logic error in the classification engine.

**5. BUG: Giant component threshold sweep produces identical results at all thresholds.**

The sweep from 0.10 to 0.90 returns exactly 775 edges, 792 components, and largest-component=41 at every single one of 17 threshold values. This is almost certainly a bug in the sweep logic: the threshold filter is not actually filtering inferred edges (all 775 edges survive regardless of threshold). The report itself identifies this ("Transition width: Could not identify clean 10%->50% crossing range") but does not flag it as a bug. The fragmentation finding at the default threshold (0.500) may still be correct --- the 22 inferred edges from the coupling protocol all have strength >= 0.50 --- but the sweep that was supposed to confirm robustness across thresholds is non-functional and needs investigation.

---

## Phase 3: Gap Analysis --- Questions You Can and Cannot Answer

| # | Question | Status | Source | Notes |
|---|----------|--------|--------|-------|
| 1 | Show all constraints where powerless sees snare but institutional sees rope | **DERIVABLE** | `corpus_data.json` classifications array (context+type per entry), or `pipeline_output.json` perspectives | Filter where classification context=powerless has type=snare AND context=institutional has type=rope |
| 2 | What's the average extractiveness of constraints in the geopolitical domain? | **DERIVABLE** | `corpus_data.json` metrics.extractiveness + domain field | Simple filter + aggregate |
| 3 | Which constraints changed classification between pipeline runs? | **ANSWERABLE** | `stress_test_report.md` | Full migration matrix for 403 shared constraints across 15 migration paths |
| 4 | List all constraints with omega severity > 0.9 | **DERIVABLE** | `enriched_omega_data.json` severity_score field | Filter omegas where severity_score > 0.9 |
| 5 | Which orbit families have the highest average extractiveness? | **DERIVABLE** | Join `orbit_data.json` + `corpus_data.json` | Group by orbit_signature, aggregate metrics.extractiveness |
| 6 | Show the 10 highest-contamination super-spreader constraints | **ANSWERABLE** | `giant_component_analysis.md` super-spreader table | Top: `us_sanctions_icc_israel_case` (degree 10), `us_usmca_china_leverage` (degree 9) |
| 7 | Compare all perspectives for a specific constraint | **DERIVABLE** | `pipeline_output.json` per_constraint -> perspectives + classifications | Lookup by constraint id, enumerate all perspective entries |
| 8 | Which constraints share an omega family with constraint X? | **DERIVABLE** | `enriched_omega_data.json` families section | Lookup family by constraint, return family.members |
| 9 | What is the purity score distribution for political constraints? | **DERIVABLE** | `pipeline_output.json` purity_score + domain | Filter domain=political, aggregate purity_score |
| 10 | Show all liminal constraints with drift events | **PARTIALLY ANSWERABLE** | `abductive_report.md` lists 10 confirmed_liminal; `genuine_findings_review.md` profiles 6 with full drift details | Gap: 4 confirmed_liminal constraints lack detailed drift profiles |
| 11 | What's the correlation between extractiveness and coupling score? | **DERIVABLE** | `pipeline_output.json` has base_extractiveness + coupling per constraint | Compute Pearson/Spearman on the two columns |
| 12 | Which constraints appear in both false_mountain_report AND omega critical list? | **DERIVABLE** | Cross-reference `false_mountain_report.md` constraint IDs with `enriched_omega_data.json` severity=critical entries | No existing tool does this; requires scripted cross-reference |
| 13 | Track a constraint's trajectory over time (run-to-run metrics) | **REQUIRES NEW INFRASTRUCTURE** | Only `stress_test_report.md` has old-vs-new | No time-series data stored; only single snapshot per pipeline run |
| 14 | Group constraints by beneficiary type and show average metrics | **DERIVABLE** | `pipeline_output.json` has beneficiaries + base_extractiveness, suppression, etc. | Group by beneficiaries, aggregate metrics |
| 15 | Which domains have the most coordination-washing? | **PARTIALLY ANSWERABLE** | `conflict_map.md` engine-only: 2 coordination-washing cases; `enriched_omega_data.json` has gap_class per omega | Comprehensive answer requires grouping omegas by domain + filtering gap_class=coordination_washing |

### Status Distribution

- **ANSWERABLE** (directly in existing reports): 2 questions (#3, #6)
- **DERIVABLE** (requires programmatic query of JSON data): 9 questions (#1, #2, #4, #5, #7, #8, #9, #11, #14)
- **PARTIALLY ANSWERABLE** (partial data exists): 3 questions (#10, #12, #15)
- **REQUIRES NEW INFRASTRUCTURE**: 1 question (#13)

The dominant gap: 9 of 15 questions are DERIVABLE but cannot be answered without writing code to query the JSON files. This is the primary motivation for a query engine.

---

## Phase 4: Query Engine Assessment

### What Would It Query?

The right data substrate is a join of three files:

1. **`corpus_data.json`** as the primary table (1034 rows): all constraint metadata, metrics, orbit data, variance, classifications per context
2. **`enriched_omega_data.json`** for omega enrichment: severity scores, gap classes, family membership (959 omegas mapped to constraints)
3. **`pipeline_output.json`** for purity/coupling/Boltzmann data: purity_score, purity_band, coupling, signature, drift events, beneficiaries, victims

Together these three files contain all analytically relevant dimensions. No other file adds dimensions not already present.

### Recommended Form: Python CLI Tool with pandas

**Why not SQLite?** The data is naturally hierarchical --- classifications is an array of {context, type} objects; orbit_signature is a list; beneficiaries/victims are lists. Flattening this into a relational schema adds overhead with no benefit for a single-researcher workflow.

**Why not Jupyter notebook?** Good for ad-hoc exploration but doesn't integrate into the Makefile pipeline or support repeatable scripted queries from the command line.

**Why not a web dashboard (React, Streamlit)?** High implementation effort, low marginal value over CLI for a single researcher. The audience is one person running queries.

**Why Python CLI?**
- The pipeline is already Python + Prolog; a CLI tool fits the existing toolchain
- pandas DataFrames handle nested JSON well with `json_normalize`
- Can be integrated into the Makefile as a post-pipeline query step
- Supports both interactive one-off queries and scripted batch analysis
- argparse provides a natural filter-composition interface
- Dependencies: only `pandas` + `argparse` (argparse is stdlib)

### Highest-Value Queries (Ranked)

**1. Cross-perspective filter (KILLER FEATURE)**

```
query.py --perspective powerless snare --perspective institutional rope --table
```

This is the query you cannot do by grepping reports. It directly serves the coordination-washing analysis and would make exploring the 267-member F001 cluster trivial. It answers gap analysis question #1 and enables systematic investigation of the headline finding.

**2. Domain aggregate**

```
query.py --domain geopolitical --aggregate extractiveness,suppression
```

Answers questions #2 and #9. Enables quick domain comparisons without manual report-reading.

**3. Omega filter**

```
query.py --omega-severity critical --omega-family F001 --table
```

Enables exploration of the 267-member coordination-washing cluster, the 329 critical-severity omegas, and cross-family comparisons.

**4. Multi-report cross-reference**

```
query.py --in-report false_mountain --in-report omega_critical --list
```

Answers question #12. The only way to currently cross-reference findings across reports is manual grep.

**5. Constraint detail**

```
query.py --detail moltbook_agent_theater
```

Answers question #7. Full constraint profile: all perspectives, all metrics, orbit, purity, drift events, omegas, beneficiaries, victims.

### Minimum Viable Version (v0.1) Specification

```
query.py [OPTIONS]

Data loading:
  Reads corpus_data.json + enriched_omega_data.json + pipeline_output.json on startup
  Builds a single pandas DataFrame with ~1034 rows
  Join key: constraint_id / id / associated_constraint

Filters (all combinable with AND logic):
  --type TYPE              Filter by claimed_type (e.g., tangled_rope, snare)
  --domain DOMAIN          Filter by domain (e.g., political, mathematical)
  --min-extractiveness N   Filter by extractiveness >= N
  --max-extractiveness N   Filter by extractiveness <= N
  --min-suppression N      Filter by suppression >= N
  --max-suppression N      Filter by suppression <= N
  --orbit-contains TYPE    Filter where orbit_signature includes TYPE
  --perspective CTX TYPE   Filter where perspective CTX classifies as TYPE
                           (repeatable: --perspective powerless snare --perspective institutional rope)
  --omega-severity SEV     Filter constraints with omega at severity SEV (critical/high/medium/low)
  --omega-family FAM       Filter constraints in omega family FAM (e.g., F001)
  --purity-band BAND       Filter by purity_band (pristine/sound/borderline/contaminated/degraded)
  --signature SIG          Filter by structural signature (e.g., false_ci_rope, false_natural_law)
  --min-coupling N         Filter by coupling score >= N
  --has-drift              Filter to constraints with active drift events

Output modes:
  --list                   Print constraint IDs one per line (default)
  --count                  Print count only
  --table                  Print table with key metrics (id, type, domain, extractiveness,
                           suppression, orbit, purity_band)
  --aggregate FIELD        Print mean/median/std for FIELD across filtered set
  --detail CONSTRAINT_ID   Print full profile for one constraint (all fields, all perspectives)
  --json                   Output as JSON for piping to other tools
  --csv                    Output as CSV for spreadsheet analysis

Example queries:
  # The coordination-washing cluster
  query.py --perspective powerless snare --perspective institutional rope --table

  # Political domain metrics
  query.py --domain political --aggregate extractiveness

  # All constraints with opaque orbits
  query.py --orbit-contains indexically_opaque --list

  # High-extraction tangled_ropes
  query.py --type tangled_rope --min-extractiveness 0.9 --table

  # Deep dive on a specific constraint
  query.py --detail moltbook_agent_theater

  # Critical omega family exploration
  query.py --omega-family F001 --omega-severity critical --count

  # Degraded purity with active drift
  query.py --purity-band degraded --has-drift --table
```

**Estimated size**: ~250 lines of Python. No database, no web framework, no dependencies beyond pandas + argparse.

**Integration**: Add a `query` target to the Makefile that runs after the pipeline completes. The query tool reads the same output files the pipeline produces, so it's always in sync.

---

*End of outputs evaluation.*
