% ============================================================================
% CONSTRAINT STORY: biomarker_discovery_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biomarker_discovery_capture, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: biomarker_discovery_capture
 *   human_readable: Biomarker Discovery Capture in Precision Medicine
 *   domain: biomedical_research/diagnostic_innovation
 *
 * SUMMARY:
 *   Biomarker discovery capture describes the structural asymmetry between
 *   early-stage biomarker identification in research cohorts and the
 *   multi-year validation process required to establish clinical utility.
 *   Discovery groups publish candidate biomarkers based on association
 *   studies in convenience samples; regulatory and clinical translation
 *   requires prospective validation in diverse populations with long
 *   follow-up. This creates a temporal extraction window: early-published
 *   markers capture journal visibility, funding priority, and clinical
 *   authority through media and physician adoption long before independent
 *   validation. The constraint exhibits mixed coordination (genuine need to
 *   communicate biomarker candidates) and extraction (capital concentration
 *   in early discovery, withholding of negative results, marketing hype
 *   exceeding evidence). Theater ratio increases over time as complexity of
 *   claims outpaces capability of review processes. Measurements show partial
 *   recovery as federated validation infrastructure matures, suggesting
 *   scaffold rather than permanent snare.
 *
 * KEY AGENTS:
 *   - Early Discovery Research Groups: Primary beneficiary (institutional/arbitrage) — capture publication priority, citation inflation, and premium funding access during discovery window
 *   - Diagnostic Device Manufacturers: Secondary beneficiary (organized/constrained) — depend on early-stage claims for product pipeline; benefit from marketing hype during early adoption but constrained by regulatory accountability
 *   - Patient Populations: Primary victim (powerless/trapped) — exposed to unvalidated biomarker claims through clinical hype and media coverage; trapped by asymmetric information and clinical authority framing
 *   - Competing Research Groups: Secondary victim (moderate/constrained) — face publication bias against negative findings and funding concentration in early-discovery labs; also benefit from early biomarker claims as reference standards
 *   - Clinical Validation Infrastructure: Organized actors (organized/mobile) — multi-site cohort studies, biobanks, EHR-linked registries building alternative verification pathways with sunset logic
 *   - Publication Peer Review System: Institutional actor (institutional/arbitrage) — maintains performative certification of biomarker discovery; cannot verify patient cohort homogeneity or long-term clinical outcomes (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing replication lag as inherent to medicine rather than contingent on publication incentives and funding structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biomarker_discovery_capture, 0.54).
domain_priors:suppression_score(biomarker_discovery_capture, 0.62).
domain_priors:theater_ratio(biomarker_discovery_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biomarker_discovery_capture, extractiveness, 0.54).
narrative_ontology:constraint_metric(biomarker_discovery_capture, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(biomarker_discovery_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biomarker_discovery_capture, tangled_rope).
narrative_ontology:human_readable(biomarker_discovery_capture, "Biomarker Discovery Capture in Precision Medicine").
narrative_ontology:topic_domain(biomarker_discovery_capture, "biomedical_research/diagnostic_innovation").

domain_priors:requires_active_enforcement(biomarker_discovery_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biomarker_discovery_capture, early_discovery_groups).
narrative_ontology:constraint_beneficiary(biomarker_discovery_capture, diagnostic_device_manufacturers).
narrative_ontology:constraint_victim(biomarker_discovery_capture, patient_populations).
narrative_ontology:constraint_victim(biomarker_discovery_capture, clinical_validation_infrastructure).
narrative_ontology:constraint_victim(biomarker_discovery_capture, competing_research_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT POPULATION (SNARE) — Patients cannot exit reliance on biomarkers claimed in early discovery phase. Marketing hype creates expectation that unvalidated markers will guide treatment decisions. Trapped by asymmetric information and clinical authority. Bear full extraction cost through delayed access to truly validated diagnostics and exposure to false positive management.
constraint_indexing:constraint_classification(biomarker_discovery_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING RESEARCH GROUPS (TANGLED ROPE) — Constrained by publication bias toward positive findings and funding concentration in early-discovery labs. Also benefit from coordination: early-published biomarkers provide reference standards and validation targets for broader cohort studies. Significant extraction but partial access to coordination benefits.
constraint_indexing:constraint_classification(biomarker_discovery_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY DISCOVERY RESEARCH GROUP (ROPE) — Benefits from first-mover publication advantage, citation inflation, and premium funding access. Experiences the constraint as coordination: communicating biomarker candidates enables follow-up research and clinical translation. Net beneficiary with high agency and multiple exit options (replication, commercial licensing, method licensing).
constraint_indexing:constraint_classification(biomarker_discovery_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIAGNOSTIC DEVICE INDUSTRY (TANGLED ROPE) — Depends on early-stage biomarker claims for product development pipeline. Organizes coordinated validation efforts (MultiHub consortia, FDA premarket review). Also benefits from marketing hype during early adoption phase. Constrained by regulatory timelines and clinical outcome accountability. Mixed extraction and coordination.
constraint_indexing:constraint_classification(biomarker_discovery_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLICATION PEER REVIEW SYSTEM (PITON) — Biomarker discovery papers are largely performative in validation terms: reviewers assess statistical novelty and mechanistic plausibility but cannot verify patient cohort homogeneity, confounding variable control, or reproducibility in independent cohorts. The review process persists through institutional inertia (career incentives tied to journal impact) despite low functional verification capability. Theater ratio high because the ritual certifies discovery without establishing clinical utility.
constraint_indexing:constraint_classification(biomarker_discovery_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLINICAL VALIDATION INFRASTRUCTURE (SCAFFOLD) — Multi-site prospective cohort studies, biobanks, and federated patient registries are building alternative verification pathways with explicit sunset: as biomarkers mature through validation, clinical integration protocols replace the discovery-phase hype cycle. Organized agents (NIH biobanks, EHR-linked registries) have agency and see exit path. Suppression declines as validation infrastructure matures.
constraint_indexing:constraint_classification(biomarker_discovery_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, biomarker discovery always precedes clinical validation — the gap is inherent to how medicine advances. No agent can exit the replication lag. But structural data contradicts: the extraction is contingent on publication incentives, funding concentration, and marketing dynamics. Engine will compute false summit, revealing naturalization of contingent institutional arrangement.
constraint_indexing:constraint_classification(biomarker_discovery_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biomarker_discovery_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biomarker_discovery_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biomarker_discovery_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biomarker_discovery_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biomarker_discovery_capture, TR),
    TR >= 0.70.

:- end_tests(biomarker_discovery_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. Early discovery groups capture career and funding benefits during 3-6 year validation window. The extraction is substantial but not maximal because much of the benefit is legitimate first-mover reward for high-risk research identifying genuinely novel signals. Publication hype and media coverage amplify extraction beyond what basic citation advantage would produce. The value reflects that extraction is real (patients are harmed by unvalidated claims) but partially offset by coordination value (early biomarkers do enable follow-up research). Suppression (0.62): Moderate-high. Significant barriers to independent validation include requirement for large prospective cohorts, long follow-up periods, access to biospecimens, data integration complexity, and publication bias against negative findings. But barriers are not absolute — federated validation infrastructure is reducing them. Suppression value reflects institutional constraints rather than total prevention. Theater ratio (0.68): High and increasing. Early peer review for biomarker papers assesses statistical novelty and mechanistic plausibility but cannot verify patient cohort quality, confounding control, or reproducibility in diverse populations. The performative content increases as biomarker complexity outpaces reviewer capacity. Measurement trajectory (0.45→0.68) reflects growing gap between discovery-phase standards and validation-phase requirements. Partial decline at t=9 (0.62) reflects emerging federated validation infrastructure beginning to provide alternative non-performative verification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival divergence across power and exit dimensions. Early discovery groups see coordination (Rope) — they solve legitimate problem of communicating biomarker candidates to enable follow-up research. Device manufacturers see mixed coordination and constraint (Tangled Rope) — genuine need to develop diagnostics but constrained by regulatory timelines and clinical outcome accountability. Competing research groups see extraction (Tangled Rope) — publication bias and funding concentration disadvantage them despite coordination benefits. Clinical validation infrastructure sees temporary coordination failure with sunset (Scaffold) — prospective validation protocols and federated networks are building verification pathways that will eventually replace discovery-phase hype. Patients see pure extraction (Snare) — they are trapped by clinical authority framing and unvalidated claims with no ability to assess or exit. Publication system sees its own ritual as performative (Piton) — maintains certification process despite low verification capability. Analytical observer risks seeing natural law (Mountain) — 'biomarker discovery precedes validation in medicine' — but structural data reveals this as naturalized institutional choice: extraction is contingent on publication incentives, funding concentration, and marketing dynamics, not inherent to scientific progress.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's structural position determines directionality. Early discovery groups with arbitrage exit options (can license to manufacturers, commercialize, or establish themselves as field leaders) experience low effective extraction. Institutional beneficiaries with multiple exit paths face low d values. Competing moderate-power research groups constrained by publication bias experience high d values reflecting their victim status, partially offset by coordination benefits from early biomarker claims (reference standards). Patients with no exit options and no information advantage experience maximum d (trapped). Clinical validation infrastructure with organizational capacity and real exit paths (building alternative verification systems) experiences lower d reflecting agency. The device industry's mixed position (beneficiary of marketing hype but constrained by regulatory and clinical outcome liability) produces intermediate d reflecting their organized but constrained position. Publication system's arbitrage position (benefits from high-impact submissions, maintains career incentive structure) produces low d despite its performative dysfunction—the system itself is not trapped, even though it perpetuates theater.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival presheaf. The apparent conflict between coordination and extraction resolves by recognizing that both are real but at different scales. At the research communication scale (early discovery group perspective), the constraint is largely coordination (Rope) — sharing biomarker candidates enables follow-up research. At the patient access scale (patient perspective), the same constraint is extraction (Snare) — unvalidated claims are marketed as clinical guidance, delaying access to validated alternatives. At the validation infrastructure scale (clinical validation perspective), the constraint is temporary coordination failure with sunset (Scaffold) — federated networks will eventually provide independent verification. The mandatrophy dissolves by accepting that the constraint is Tangled Rope at the institutional level (mixing genuine coordination of biomarker candidates with asymmetric extraction of benefit during validation window) AND that the analytical observer's 'natural law' reading is a false summit—the replication lag is not inherent but contingent on institutional structures. The resolution: measure extractiveness as the extraction component of the tangled coordination, acknowledge the sunset structure in validation infrastructure development, and flag the publication/review system's performative theater as a secondary piton degradation. All six perspectives are epistemically legitimate readings of the same constraint from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    validation_timeline_adequacy,
    'What prospective validation timeline suffices to establish clinical utility versus extractive hype?',
    'Longitudinal outcomes data: correlation between early-discovery biomarker claims and long-term patient outcomes in independent cohorts; time-to-clinical-integration distribution analysis',
    'If adequacy threshold < 2 years: many legitimate discoveries misclassified as extraction. If threshold > 7 years: extractive unvalidated markers persist in clinical use.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(validation_timeline_adequacy, empirical, 'Timeline threshold for clinically actionable biomarker validation').

omega_variable(
    confounding_variable_saturation,
    'Are competing biomarker claims genuinely measuring distinct biological processes or redundantly measuring shared confounding variables (age, comorbidity, treatment history)?',
    'Multivariate decomposition in independent cohorts; correlation matrix analysis of competing biomarker claims against clinical and demographic variables; mediation analysis',
    'If distinct: biomarker pluralism is coordination (Rope). If confounded: most competing claims are extraction mechanisms targeting different funding pools.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confounding_variable_saturation, empirical, 'Whether competing biomarkers represent distinct or confounded biological processes').

omega_variable(
    commercial_incentive_attenuation,
    'Do device manufacturer licensing agreements attenuate early-discovery group publication bias or amplify withholding of negative results and competing markers?',
    'Analysis of publication timing relative to patent filing and licensing events; comparison of positive vs negative result publication rates before and after commercial licensing; investigation of concurrent unreplicated markers in non-licensed research',
    'If attenuated: commercial integration accelerates validation. If amplified: commercial structure becomes extraction mechanism, victims expand to include scientific commons and competing researchers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_incentive_attenuation, empirical, 'Whether commercial licensing increases or decreases publication bias in biomarker discovery').

omega_variable(
    federated_validation_efficacy,
    'Can federated clinical validation networks (EHR-linked registries, biobanks) actually replicate biomarker claims at scale, or do they face insurmountable data quality and phenotyping variance issues?',
    'Prospective replication studies in multi-site federated networks; success rates of biomarkers passing independent validation; systematic comparison of centralized vs federated validation outcomes',
    'If efficacious: scaffold perspective confirmed—validation infrastructure will eventually replace discovery-phase hype. If inadequate: validation infrastructure becomes performative, scaffold degraded to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federated_validation_efficacy, empirical, 'Whether federated clinical networks can effectively replicate biomarker discoveries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biomarker_discovery_capture, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biomark_tr_t0, biomarker_discovery_capture, theater_ratio, 0, 0.45).
narrative_ontology:measurement(biomark_tr_t3, biomarker_discovery_capture, theater_ratio, 3, 0.58).
narrative_ontology:measurement(biomark_tr_t6, biomarker_discovery_capture, theater_ratio, 6, 0.68).
narrative_ontology:measurement(biomark_tr_t9, biomarker_discovery_capture, theater_ratio, 9, 0.62).

% Extraction over time
narrative_ontology:measurement(biomark_be_t0, biomarker_discovery_capture, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(biomark_be_t3, biomarker_discovery_capture, base_extractiveness, 3, 0.43).
narrative_ontology:measurement(biomark_be_t6, biomarker_discovery_capture, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(biomark_be_t9, biomarker_discovery_capture, base_extractiveness, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biomarker_discovery_capture, information_standard).
narrative_ontology:affects_constraint(biomarker_discovery_capture, clinical_trial_phase_duration_capture).
narrative_ontology:affects_constraint(biomarker_discovery_capture, pharmaceutical_regulatory_capture).

% DUAL FORMULATION NOTE:
% Biomarker discovery capture is downstream of specific biomarker claims but represents a distinct structural constraint on research incentive alignment. Upstream constraints have extractiveness values reflecting empirical status of specific biomarkers; this constraint has extractiveness reflecting institutional structures (publication incentives, funding concentration, marketing dynamics) that apply across all biomarker research.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
