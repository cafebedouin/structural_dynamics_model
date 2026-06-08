% ============================================================================
% CONSTRAINT STORY: service_fragility_prevention_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_service_fragility_prevention_shift, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: service_fragility_prevention_shift
 *   human_readable: NHS Service Fragility from Prevention-Reactive Resource Reallocation
 *   domain: healthcare_policy/genomic_medicine/resource_allocation
 *
 * SUMMARY:
 *   The NHS resource reallocation from reactive to preventative care, driven
 *   by genomic medicine and AI health prediction platforms, creates
 *   structural service fragility when prevention fails to reduce demand as
 *   expected. This constraint exemplifies extraction through temporal
 *   mismatch: capacity is reduced immediately and irreversibly while demand
 *   reduction is delayed, uncertain, and potentially insufficient. The policy
 *   narrative captures political capital and legitimacy from prevention-first
 *   framing; genomic medicine and AI vendors capture contracts and market
 *   expansion; patients needing reactive care bear the cost through degraded
 *   service quality, increased waiting times, and reduced surge capacity. The
 *   theater component manifests as substitution of activity metrics
 *   (screenings performed, risk scores generated) for outcome metrics (actual
 *   disease incidence reduction, reactive demand decrease). The constraint is
 *   downstream of clinical_deskilling_automation: as AI systems automate
 *   clinical decision-making, the workforce capacity to restore reactive
 *   services after reallocation becomes further degraded, making the capacity
 *   shift increasingly irreversible. The structural trap: prevention must
 *   work quickly and effectively enough to reduce demand before the gap
 *   between reduced capacity and unreduced need causes system failure, but
 *   the evidence base for this timeline is insufficient and the reallocation
 *   is politically difficult to reverse.
 *
 * KEY AGENTS:
 *   - Patients Needing Reactive Care: Primary victim (powerless/trapped) — cannot exit NHS system, bear full cost of capacity reduction through degraded service quality and increased waiting times
 *   - Acute Care Clinicians: Secondary victim (moderate/constrained) — face intensified workload and moral injury as capacity shrinks while demand persists; constrained by professional obligations and career investment
 *   - NHS Trust Management: Mixed position (organized/constrained) — coordinate reallocation under policy mandate, benefit from political alignment but bear operational risk of service failure
 *   - Prevention Policy Narrative: Primary beneficiary (institutional/arbitrage) — captures political capital and research funding from prevention-first framing; can exit to other policy domains if implementation fails
 *   - Genomic Medicine and AI Vendor Sector: Primary beneficiary (institutional/arbitrage) — captures contracts and market expansion from NHS adoption; global arbitrage options if UK market fails
 *   - Public Health Advocacy Coalition: Organized agents (organized/mobile) — see reallocation as temporary transition with sunset logic; can shift advocacy focus if prevention fails
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes genuine coordination function alongside asymmetric extraction and structural fragility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(service_fragility_prevention_shift, 0.68).
domain_priors:suppression_score(service_fragility_prevention_shift, 0.72).
domain_priors:theater_ratio(service_fragility_prevention_shift, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(service_fragility_prevention_shift, extractiveness, 0.68).
narrative_ontology:constraint_metric(service_fragility_prevention_shift, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(service_fragility_prevention_shift, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(service_fragility_prevention_shift, snare).
narrative_ontology:human_readable(service_fragility_prevention_shift, "NHS Service Fragility from Prevention-Reactive Resource Reallocation").
narrative_ontology:topic_domain(service_fragility_prevention_shift, "healthcare_policy/genomic_medicine/resource_allocation").

domain_priors:requires_active_enforcement(service_fragility_prevention_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(service_fragility_prevention_shift, prevention_focused_policy_narrative).
narrative_ontology:constraint_beneficiary(service_fragility_prevention_shift, genomic_medicine_sector).
narrative_ontology:constraint_beneficiary(service_fragility_prevention_shift, ai_health_technology_vendors).
narrative_ontology:constraint_victim(service_fragility_prevention_shift, patients_needing_reactive_care).
narrative_ontology:constraint_victim(service_fragility_prevention_shift, emergency_department_capacity).
narrative_ontology:constraint_victim(service_fragility_prevention_shift, acute_care_workforce).
narrative_ontology:constraint_vindicates(service_fragility_prevention_shift, prevention_superiority_doctrine).
narrative_ontology:constraint_vindicates(service_fragility_prevention_shift, predictive_medicine_inevitability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT NEEDING REACTIVE CARE (SNARE) — Trapped by geographic and economic constraints in NHS catchment area. Cannot exit to private care or alternative systems. Experiences maximum extraction as capacity is reallocated away from acute services before prevention reduces demand. Waiting times increase, service quality degrades, no alternative pathway available.
constraint_indexing:constraint_classification(service_fragility_prevention_shift, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ACUTE CARE CLINICIAN (SNARE) — Constrained by professional obligations and career investment in NHS system. Faces increasing workload as capacity shrinks while demand remains stable or increases. Cannot easily exit to private sector (ethical constraints, pension lock-in, professional identity). Experiences extraction through intensified work conditions and moral injury from inability to provide adequate care.
constraint_indexing:constraint_classification(service_fragility_prevention_shift, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NHS TRUST MANAGEMENT (TANGLED ROPE) — Organized institutional actor coordinating resource allocation under policy mandate. Benefits from alignment with national prevention strategy (funding, political support) but also bears risk of service failure if prevention doesn't deliver. Constrained by political directives and performance targets. Mixed coordination (implementing policy) and extraction (bearing downside risk without control over prevention effectiveness).
constraint_indexing:constraint_classification(service_fragility_prevention_shift, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PREVENTION POLICY NARRATIVE (ROPE) — Primary beneficiary. Institutional actor with arbitrage-level exit (can shift to different policy domains if this fails). Captures political capital, research funding, and legitimacy from prevention-first framing. Experiences constraint as pure coordination: aligning resources with 'evidence-based' prevention is presented as rational optimization. Extraction flows toward this narrative, not away from it.
constraint_indexing:constraint_classification(service_fragility_prevention_shift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GENOMIC MEDICINE AND AI VENDOR SECTOR (ROPE) — Institutional beneficiary with global arbitrage options. Captures contracts, market expansion, and legitimacy from NHS adoption of predictive technologies. Can exit to other markets if NHS implementation fails. Experiences constraint as coordination mechanism enabling market access. Net beneficiary with minimal extraction.
constraint_indexing:constraint_classification(service_fragility_prevention_shift, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC HEALTH ADVOCACY COALITION (SCAFFOLD) — Organized agents (academic public health, prevention advocacy groups) see the reallocation as temporary transition toward genuinely preventative system. Sunset logic: if genomic prediction and AI-driven prevention work as claimed, reactive demand will fall and the capacity gap will close within 10-15 years. Mobile exit options (can shift advocacy focus if prevention fails). Low effective extraction because coalition has agency and sees legitimate transition pathway.
constraint_indexing:constraint_classification(service_fragility_prevention_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes genuine coordination function (prevention is legitimately valuable) alongside asymmetric extraction (patients bear downside risk of prevention failure while policy narrative and vendors capture upside). Reallocation creates structural fragility: irreversible capacity reduction before demand reduction is empirically confirmed. Theater component: prevention metrics (screenings performed, risk scores generated) substitute for outcome metrics (actual disease incidence reduction, reactive demand decrease).
constraint_indexing:constraint_classification(service_fragility_prevention_shift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(service_fragility_prevention_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(service_fragility_prevention_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(service_fragility_prevention_shift, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(service_fragility_prevention_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(service_fragility_prevention_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from patients needing reactive care through degraded service quality, increased waiting times, and reduced surge capacity. Extraction accumulates over the interval as capacity reduction proceeds while demand reduction lags or fails to materialize. The extraction is asymmetric: policy narrative and vendors capture upside (political capital, contracts, legitimacy) while patients bear downside risk. The value reflects substantial but not maximal extraction — some patients benefit from prevention interventions, and the coordination function (prevention is legitimately valuable) is real. Suppression (0.72): High. Patients are trapped by geographic and economic constraints in NHS catchment area. Cannot exit to private care (cost prohibitive for most) or alternative systems (no comparable universal healthcare in UK). Clinicians are constrained by professional obligations, pension lock-in, and career investment. Trust management is constrained by political directives and performance targets. Suppression increases over the interval as capacity reduction becomes increasingly irreversible (workforce attrition, facility repurposing, training pipeline lag). Theater ratio (0.58): Moderate-high. Prevention activity metrics (genomic screenings performed, AI risk scores generated, lifestyle interventions delivered) increasingly substitute for outcome metrics (disease incidence reduction, reactive demand decrease). The theater is not total — some prevention interventions have genuine evidence base — but the gap between activity measurement and outcome validation is substantial and growing. Theater increases over the interval as pressure to demonstrate prevention 'success' intensifies while outcome data remains insufficient.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by structural position relative to the extraction flow. Patients needing reactive care experience pure extraction (Snare) — trapped, bearing full cost, no benefit. Acute care clinicians also experience Snare — constrained exit, intensified workload, moral injury. NHS Trust management experiences mixed coordination and extraction (Tangled Rope) — implementing policy mandate (coordination) while bearing operational risk without control over prevention effectiveness (extraction). Prevention policy narrative and genomic/AI vendor sector experience pure coordination (Rope) — net beneficiaries capturing political capital and contracts with arbitrage-level exit options. Public health advocacy coalition sees temporary transition with sunset logic (Scaffold) — if prevention works as claimed, the capacity gap will close within 10-15 years. Analytical observer recognizes the hybrid structure (Tangled Rope) — genuine coordination function (prevention is valuable) alongside asymmetric extraction (patients bear downside risk while beneficiaries capture upside) and structural fragility (irreversible capacity reduction before demand reduction is empirically confirmed). The perspectival gap is not a disagreement about facts but a structural consequence of different positions in the extraction topology.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the constraint's extraction flow. Patients needing reactive care are full targets (d → 1.0): victims with trapped exit options, bearing maximum cost through degraded service quality. Acute care clinicians are high-d targets (d → 0.8): victims with constrained exit, bearing intensified workload and moral injury. NHS Trust management is moderate-d (d → 0.5): mixed beneficiary (political alignment, funding) and victim (operational risk), with constrained exit. Prevention policy narrative is low-d beneficiary (d → 0.2): institutional actor with arbitrage exit, capturing political capital and legitimacy. Genomic/AI vendor sector is low-d beneficiary (d → 0.15): institutional actor with global arbitrage, capturing contracts and market expansion. Public health advocacy coalition is low-d (d → 0.3): organized beneficiaries with mobile exit, seeing legitimate transition. Analytical observer is neutral (d → 0.5): recognizing both coordination function and extraction mechanism. The directionality derivation reflects that extraction flows from trapped/constrained agents (patients, clinicians) toward institutional beneficiaries with arbitrage options (policy narrative, vendors), with organized agents (Trust management, advocacy coalition) occupying intermediate positions based on their mixed structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Snare classification from the patient perspective and Rope classification from the beneficiary perspective are both structurally accurate readings of the same constraint from different positions in the extraction topology. The mandate (reallocate resources from reactive to preventative care to improve population health) has not outlived its function — prevention is legitimately valuable and the coordination function is real. But the implementation creates asymmetric extraction: capacity is reduced immediately and irreversibly while demand reduction is delayed, uncertain, and potentially insufficient. Patients bear the downside risk (degraded service quality if prevention fails) while policy narrative and vendors capture the upside (political capital and contracts regardless of prevention effectiveness). The Tangled Rope classification from the analytical perspective captures this hybrid structure: genuine coordination (prevention) entangled with extraction (temporal mismatch, irreversible capacity reduction, metric substitution). The theater component (activity metrics substituting for outcome metrics) is a secondary extraction mechanism layered on top of the primary temporal mismatch. The constraint is not a false summit (prevention is not a natural law being naturalized) but a genuine hybrid where coordination and extraction coexist and are experienced differently based on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prevention_demand_reduction_timeline,
    'What is the empirically validated timeline for genomic and AI-driven prevention to reduce reactive care demand sufficiently to justify capacity reallocation?',
    'Longitudinal cohort studies tracking disease incidence and healthcare utilization in populations receiving genomic risk assessment and AI-driven prevention interventions vs matched controls; minimum 10-year follow-up required to capture chronic disease trajectories',
    'If timeline < 5 years: reallocation is rational optimization (Rope from more perspectives). If timeline > 15 years or reduction magnitude < 30%: reallocation creates sustained service fragility (Snare from more perspectives). Current evidence base insufficient to distinguish.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prevention_demand_reduction_timeline, empirical, 'Timeline for prevention to reduce reactive demand sufficiently to justify capacity cuts').

omega_variable(
    capacity_reallocation_reversibility,
    'Is NHS capacity reallocation from reactive to preventative services reversible if prevention fails to reduce demand as expected?',
    'Analysis of workforce retention, facility repurposing costs, training pipeline lag, and political feasibility of reversing prevention-first policy. Historical case studies of healthcare capacity restoration after reallocation.',
    'If reversible within 2-3 years: risk is manageable transition (Scaffold confirmed). If irreversible or requiring 5+ years: creates structural trap where patients bear permanent extraction even if prevention fails (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_reallocation_reversibility, empirical, 'Whether capacity reallocation can be reversed if prevention fails').

omega_variable(
    prevention_metric_substitution,
    'Are prevention activity metrics (genomic screenings performed, AI risk scores generated, lifestyle interventions delivered) valid proxies for outcome metrics (disease incidence reduction, reactive care demand decrease)?',
    'Correlation analysis between prevention activity volume and downstream health outcomes; identification of Goodhart drift where activity metrics are optimized without corresponding outcome improvement; comparison of predicted vs actual demand reduction',
    'If metrics are valid proxies: theater_ratio should be lower (0.3-0.4). If metrics substitute for outcomes without causal link: theater_ratio is accurately high (0.5-0.7) and extraction mechanism is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prevention_metric_substitution, empirical, 'Whether prevention activity metrics validly proxy for outcome metrics').

omega_variable(
    genomic_prediction_accuracy_threshold,
    'What prediction accuracy threshold for genomic and AI risk models is required to justify population-level resource reallocation?',
    'Decision-theoretic analysis of false positive and false negative costs in prevention vs reactive care; sensitivity analysis of capacity requirements under different prediction accuracy scenarios; ethical framework for acceptable risk distribution',
    'If current accuracy (typically 60-75% for polygenic risk scores) is sufficient: reallocation is justified coordination. If threshold requires >85% accuracy not yet achieved: reallocation is premature extraction from patients who will need reactive care despite prevention efforts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genomic_prediction_accuracy_threshold, preference, 'Prediction accuracy threshold required to justify capacity reallocation').

omega_variable(
    demand_spike_resilience,
    'Can NHS system absorb demand spikes (pandemics, seasonal surges, demographic shifts) after reactive capacity reduction?',
    'Stress testing of post-reallocation capacity against historical demand variability; modeling of surge capacity restoration time; analysis of COVID-19 response capacity under different baseline capacity scenarios',
    'If system retains surge capacity: suppression is lower than measured (0.5-0.6). If system cannot absorb spikes: suppression is accurately high (0.7-0.8) and fragility is structural feature, not temporary transition state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demand_spike_resilience, empirical, 'Whether system can handle demand spikes after capacity reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(service_fragility_prevention_shift, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sfps_tr_t0, service_fragility_prevention_shift, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sfps_tr_t2, service_fragility_prevention_shift, theater_ratio, 2, 0.42).
narrative_ontology:measurement(sfps_tr_t4, service_fragility_prevention_shift, theater_ratio, 4, 0.51).
narrative_ontology:measurement(sfps_tr_t6, service_fragility_prevention_shift, theater_ratio, 6, 0.56).
narrative_ontology:measurement(sfps_tr_t8, service_fragility_prevention_shift, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(sfps_extract_baseline, service_fragility_prevention_shift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sfps_be_t2, service_fragility_prevention_shift, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sfps_be_t4, service_fragility_prevention_shift, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(sfps_be_t6, service_fragility_prevention_shift, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(sfps_be_t8, service_fragility_prevention_shift, base_extractiveness, 8, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sfps_su_t0, service_fragility_prevention_shift, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sfps_su_t2, service_fragility_prevention_shift, suppression_requirement, 2, 0.55).
narrative_ontology:measurement(sfps_su_t4, service_fragility_prevention_shift, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(sfps_su_t6, service_fragility_prevention_shift, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(sfps_su_t8, service_fragility_prevention_shift, suppression_requirement, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(service_fragility_prevention_shift, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of clinical_deskilling_automation: as AI systems automate clinical decision-making, the workforce capacity to restore reactive services after reallocation becomes further degraded, making the capacity shift increasingly irreversible. The upstream constraint (deskilling) amplifies the suppression component of this constraint (reduced reversibility) but does not change the base extractiveness or theater_ratio, which are properties of the resource reallocation policy itself rather than the automation that enables it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
