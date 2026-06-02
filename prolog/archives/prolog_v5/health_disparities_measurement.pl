% ============================================================================
% CONSTRAINT STORY: health_disparities_measurement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_health_disparities_measurement, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: health_disparities_measurement
 *   human_readable: Health Disparities Measurement Infrastructure
 *   domain: public_health/health_equity/data_infrastructure
 *
 * SUMMARY:
 *   The health disparities measurement infrastructure constitutes a
 *   foundational constraint in public health and health equity research. It
 *   defines which populations are 'visible' in health statistics, which
 *   health differences are recognized as 'disparities' requiring action, and
 *   which causal factors are measurable and hence discussable in policy. This
 *   constraint exhibits all six DR types from different perspectives:
 *   underrepresented populations experience it as a Snare (invisible in
 *   data), equity researchers see mixed coordination and extraction (Tangled
 *   Rope), healthcare institutions see coordination value (Rope), the equity
 *   movement sees a temporary measurement problem being solved (Scaffold),
 *   legacy classification systems persist through inertia (Piton), and the
 *   analytical view risks naturalizing measurement limitations as inherent
 *   scientific constraints (false Mountain). The theater ratio (0.64)
 *   reflects that much official disparity reporting is performative:
 *   disaggregated statistics that document inequities without causal analysis
 *   or targeted intervention; metrics that measure outcomes rather than
 *   mechanisms; compliance activities by dominant institutions that document
 *   'awareness' of disparities while preserving the institutional
 *   arrangements that produce them. Theater has increased over the 20-year
 *   interval as disparity measurement has proliferated while outcome gaps
 *   have persisted or widened in some populations.
 *
 * KEY AGENTS:
 *   - Underrepresented Populations: Primary victim (powerless/trapped) — invisible in data systems that lack collection mechanisms; subject to proxies (demographic categories) that obscure causal mechanisms; no structural power to demand measurement redesign
 *   - Health Equity Researchers: Secondary victim and beneficiary (moderate/constrained) — benefit from funding, data access, and career opportunities in disparity research; constrained by existing measurement infrastructure that shapes what questions are answerable; high switching cost to alternative frameworks
 *   - Healthcare Institutions and Regulators: Primary beneficiary (institutional/arbitrage) — benefit from standardized measurement enabling compliance, benchmarking, and operational efficiency; arbitrage optionality enables adaptation to new measurement standards without fundamental restructuring
 *   - Data Stewards and EHR Vendors: Primary beneficiary (institutional/arbitrage) — benefit from standardized data schemas; have arbitrage optionality to update systems when standards change
 *   - Health Data Equity Movement: Organized coalition (organized/mobile) — SDOH measurement advocates, participatory data platforms, patient-led research networks; building alternative measurement frameworks (decentralized data governance, cultural adaptation); perceive sunset path through data democratization
 *   - Legacy Classification Systems: Institutional actors (institutional/arbitrage) — ICD-10, EHR fields, validated scales; persist through switching costs and inertia despite acknowledged harms; maintain arbitrage optionality to 'update' while preserving core structure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent measurement choices (whose dimensions, whose populations) as inherent scientific limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(health_disparities_measurement, 0.52).
domain_priors:suppression_score(health_disparities_measurement, 0.58).
domain_priors:theater_ratio(health_disparities_measurement, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(health_disparities_measurement, extractiveness, 0.52).
narrative_ontology:constraint_metric(health_disparities_measurement, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(health_disparities_measurement, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(health_disparities_measurement, tangled_rope).
narrative_ontology:human_readable(health_disparities_measurement, "Health Disparities Measurement Infrastructure").
narrative_ontology:topic_domain(health_disparities_measurement, "public_health/health_equity/data_infrastructure").

domain_priors:requires_active_enforcement(health_disparities_measurement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(health_disparities_measurement, dominant_demographic_groups).
narrative_ontology:constraint_beneficiary(health_disparities_measurement, healthcare_institutions).
narrative_ontology:constraint_beneficiary(health_disparities_measurement, data_stewards).
narrative_ontology:constraint_victim(health_disparities_measurement, underrepresented_populations).
narrative_ontology:constraint_victim(health_disparities_measurement, health_equity_research).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERREPRESENTED POPULATIONS (SNARE) — Lack structural power to exit or challenge measurement schemes that obscure their health needs. Data gaps and measurement blindness make their disparities invisible in official health metrics. Trapped by institutional definitions of 'what counts' as health. Maximum experienced extraction with no agency.
constraint_indexing:constraint_classification(health_disparities_measurement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEALTH EQUITY RESEARCHERS (TANGLED ROPE) — Benefit from the measurement infrastructure (access to datasets, funding streams dedicated to disparity research) while simultaneously constrained by its limitations. High cost to exit (loss of access, lost career investment in existing frameworks). Extract coordination value from the infrastructure while bearing the cost of its blind spots.
constraint_indexing:constraint_classification(health_disparities_measurement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE INSTITUTIONS (ROPE) — Benefit from standardized measurement schemas that simplify compliance reporting and aggregate statistics. Experience the constraint as coordination: uniform measurement enables benchmarking, funding allocation, and comparative effectiveness research. Net beneficiary with arbitrage optionality.
constraint_indexing:constraint_classification(health_disparities_measurement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEALTH DATA EQUITY MOVEMENT (SCAFFOLD) — Organized coalition (SDOH data standards, disaggregation mandates, patient-led research networks) building alternative measurement frameworks with sunset clause. Lower effective extraction because organized agents have agency and perceive exit path through data democratization, participatory measurement design, and decentralized data governance. Theater ratio declining as alternative pathways mature.
constraint_indexing:constraint_classification(health_disparities_measurement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CLINICAL CLASSIFICATION SYSTEMS (PITON) — ICD-10, EHR data fields, and race-based clinical algorithms persist through institutional inertia despite documented harms and measurement inadequacy. The classification systems themselves acknowledge the theater (e.g., race as proxy for ancestry, documented bias in pain assessment algorithms). Maintained because alternatives haven't fully replaced them and switching costs are high, not because they function well.
constraint_indexing:constraint_classification(health_disparities_measurement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MEASUREMENT INVARIANCE VIEW (MOUNTAIN) — From a universal perspective, some measurement gap between health constructs and observables is inherent: health is multidimensional, cultural contexts shape what counts as 'health,' and no single metric captures lived experience. This perspective risks naturalizing what are actually choice-laden institutional arrangements (whose dimensions to measure, whose populations to include) as unavoidable scientific limits. Engine will detect false summit.
constraint_indexing:constraint_classification(health_disparities_measurement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(health_disparities_measurement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(health_disparities_measurement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(health_disparities_measurement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(health_disparities_measurement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(health_disparities_measurement, TR),
    TR >= 0.70.

:- end_tests(health_disparities_measurement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting that the measurement infrastructure allocates visibility (and hence resources, research attention, policy focus) asymmetrically. Dominant demographic groups are well-measured, so health differences within these groups are documented and acted upon. Underrepresented populations are poorly measured, so gaps in health for these groups are statistically invisible or attributed to demographic 'groupness' rather than structural causes. The extraction is not total — some health disparity measurement infrastructure genuinely supports equity research and advocacy — but a significant portion is theater (documentation without action, metrics without mechanisms, compliance without change). Suppression (0.58): Moderate-high. Barriers to alternative measurement include: switching costs (existing EHR systems, validated instruments, established research pipelines), institutional inertia (classification systems embedded in regulatory and clinical practice), epistemic authority (established measurement frameworks have legitimacy; alternatives face burden of proof), and concentration of measurement authority (standardization bodies, funding agencies, institutional data governance committees are dominated by established players). But suppression is not total — participatory measurement, SDOH data standards, and patient-led research networks are gaining traction, indicating incomplete suppression. Theater ratio (0.64): Moderate-high, reflecting that disparity reporting is substantially performative. Many institutions report disparities in marketing and compliance documentation without changing clinical algorithms, resource allocation, or research priorities. The theater has increased over time as disparity metrics have proliferated while outcome gaps persist, indicating that measurement theater is substituting for structural change. Institutions are visibly 'measuring the problem' as a substitute for solving it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across three dimensions: (1) Visibility: dominant groups experience measurement as coordination and efficiency; underrepresented groups experience it as invisibility and erasure. (2) Exit optionality: institutional actors have arbitrage optionality enabling adaptation; underrepresented populations are trapped in measurement systems designed without their input. (3) Causality: equity researchers experience the framework as constraining explanation (forced to use demographic proxies); data stewards experience it as enabling operationalization (standardized fields and comparability). The powerless perspective (Snare) is the population most harmed but least heard in measurement design. The institutional perspectives (Rope, Piton) dominate actual measurement governance and can absorb new standards without fundamental change. The organized coalition perspective (Scaffold) perceives a real exit path through participatory design and data democratization but faces institutional resistance. The analytical view risks Romanticizing measurement limitations as inherent, when they are actually contingent on who designs systems and whose needs are centered.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position and their relationship to the extraction flow. Underrepresented populations have zero arbitrage optionality — they cannot exit the measurement system or demand to be measured differently without institutional sanction. Trapped status produces maximum d and high f(d). Health equity researchers have constrained optionality — they can work within the existing framework with high career reward or exit to alternative measurement frameworks with low resource access. Constrained status produces moderate d. Healthcare institutions have arbitrage optionality — they can adopt new measurement standards, update EHRs, and maintain operational compliance under multiple frameworks. Arbitrage status produces low d. The Data Equity Movement has mobile optionality — they can build alternative systems (decentralized platforms, cultural measurement variants) and gradually shift authority away from legacy systems. Mobile status produces moderate d. Legacy classification systems maintain arbitrage optionality despite their acknowledged harms because switching costs are distributed across the ecosystem and institutional benefits are concentrated. The analytical observer's d is derived from the canonical analytical fallback (0.73) because no specific beneficiary/victim relationship applies — the observer is outside the constraint's extraction flow but sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   CLASSIFICATION AMBIGUITY: The constraint could be classified as pure Snare (measurement is extractive theater that victimizes underrepresented populations and prevents structural change) or as Tangled Rope (measurement provides genuine coordination value for health research infrastructure while embedding asymmetric extraction). The mandatrophy is resolved by indexing to structural position: for trapped populations, it is Snare. For institutions and researchers, it is Tangled Rope or Rope. For organized equity movements, it is Scaffold. No single type collapses to the correct answer; the presheaf over observation sites captures the true structure. The false Mountain perspective (measurement limitations are inherent to science) is revealed as naturalization when the analytical observer's structural data are examined: measurement choices (whose populations to include, which dimensions to measure, whether to measure mechanisms or just outcomes) are contingent on institutional authority and incentives, not laws of science.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_classification_sufficiency,
    'Do current demographic categories (race, ethnicity, gender, SES) adequately capture the social determinants that actually produce health disparities, or do they function primarily as proxies that obscure causal mechanisms?',
    'Comparison of variance explained: standard demographic-based disparity estimates vs. social determinants-based estimates (housing, food security, pollution exposure, social capital); analysis of misclassification rates when demographic proxies substitute for direct SDOH measurement',
    'If adequate: measurement framework is a coordination problem (Rope from more perspectives). If inadequate: framework is an extraction mechanism that naturalizes causes while measuring consequences (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_classification_sufficiency, empirical, 'Whether demographic categories adequately capture causal mechanisms of health disparities').

omega_variable(
    invisibility_versus_noise,
    'Are data gaps for underrepresented populations caused by insufficient sample size (noise — solvable with more data) or structural exclusion from measurement systems (invisibility — requires reconceptualization)?',
    'Analysis of sampling schemas: are underrepresented groups proportionally represented in source datasets but analytically disaggregated? Or are they absent from collection entirely? Comparison of missing-data mechanisms across populations.',
    'If noise: increasing sample size and standardization resolves disparities (Rope). If invisibility: requires radical reconstruction of measurement systems (Snare becomes visible, demanding Tangled Rope or Scaffold responses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisibility_versus_noise, empirical, 'Whether measurement gaps are sample size issues or structural exclusion').

omega_variable(
    algorithm_bias_versus_underlying_disparity,
    'When clinical algorithms show differential accuracy by race/ethnicity (e.g., kidney function estimators, pain severity scales), does this reflect real biological or social differences (measurement should capture these) or algorithmic bias projecting population labels onto individual outcomes?',
    'Causal decomposition analysis: what portion of differential performance is explained by unmeasured confounding vs. algorithm design choices? Prospective validation in settings where causal confounders are controlled or measured directly.',
    'If biological/social differences: algorithms should incorporate demographic terms (current practice justified). If algorithmic bias: demographic terms amplify rather than capture disparity (Snare extraction of inferential authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_bias_versus_underlying_disparity, empirical, 'Whether differential algorithm performance reflects true differences or algorithmic bias').

omega_variable(
    measurement_standardization_tradeoff,
    'Does standardization of health measurement (uniform metrics, comparable data) inherently require suppressing cultural and contextual variation in what health means, or can cultural validity and statistical comparability coexist?',
    'Comparison of measurement outcomes: systems with cultural adaptation and local control (participatory measurement) vs. centralized standardization; assessment of whether either sacrifices equity or validity.',
    'If incompatible: measurement and equity are in tension (Snare/Tangled Rope extraction is unavoidable, though mitigatable). If compatible: measurement can be redesigned as Scaffold toward participatory systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_standardization_tradeoff, conceptual, 'Whether measurement standardization and cultural validity are compatible').

omega_variable(
    benefit_capture_by_dominant_groups,
    'Do investments in health disparity measurement primarily benefit underrepresented populations through improved targeting and advocacy, or primarily benefit dominant groups and institutions through demonstrating ''we''re measuring the problem'' while perpetuating structural causes?',
    'Longitudinal outcome analysis: correlation between disparity measurement infrastructure expansion and actual reduction in health gaps for underrepresented populations vs. increase in research funding, publications, and institutional prestige for measurement entities.',
    'If populations benefit: measurement is genuine coordination (Rope with equity function). If institutions benefit more: measurement is extraction mechanism (Tangled Rope or Snare, with measurement itself as extraction theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_capture_by_dominant_groups, empirical, 'Who benefits from health disparity measurement infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(health_disparities_measurement, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hdm_tr_t0, health_disparities_measurement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hdm_tr_t10, health_disparities_measurement, theater_ratio, 10, 0.56).
narrative_ontology:measurement(hdm_tr_t20, health_disparities_measurement, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(hdm_be_t0, health_disparities_measurement, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hdm_be_t10, health_disparities_measurement, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hdm_be_t20, health_disparities_measurement, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(health_disparities_measurement, information_standard).
narrative_ontology:affects_constraint(health_disparities_measurement, clinical_algorithm_bias).
narrative_ontology:affects_constraint(health_disparities_measurement, health_equity_funding_allocation).
narrative_ontology:affects_constraint(health_disparities_measurement, population_representation_in_research).

% DUAL FORMULATION NOTE:
% Health disparities measurement is upstream of specific algorithmic bias constraints (kidney function estimators, pain scales) and institutional allocation constraints (research funding, resource distribution). This constraint defines what counts as a disparity and hence what can be acted upon. Decomposition: measurement_framework (ε≈0.52, Tangled Rope, focus on infrastructure and standards) vs. demographic_classification (ε≈0.65, Snare, focus on epistemic violence of racial/ethnic categorization). Both share beneficiaries/victims but differ in scope and resolution pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(health_disparities_measurement, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
