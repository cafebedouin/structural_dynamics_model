% ============================================================================
% CONSTRAINT STORY: structural_determinants_data_availability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_determinants_data_availability, []).

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
 *   constraint_id: structural_determinants_data_availability
 *   human_readable: Structural Determinants Data Availability Constraint
 *   domain: epidemiology/public_health/data_governance
 *
 * SUMMARY:
 *   Structural determinants research requires access to granular
 *   sociodemographic data (income, education, race/ethnicity, occupation,
 *   geographic location) to understand health inequities and their root
 *   causes. This data is often tightly controlled by institutional
 *   gatekeepers (government agencies, large research centers, data
 *   corporations) through privacy justifications, intellectual property
 *   claims, and licensing restrictions. The constraint operates as a tangled
 *   hybrid: genuine coordination is present (shared data standards, ethics
 *   review, protection of human subjects) but is coupled with asymmetric
 *   extraction (selective access for privileged institutions, research
 *   capacity concentrated in elite centers, disadvantaged populations
 *   producing data they cannot access for their own health improvement). The
 *   constraint's extractiveness has risen from 0.35 to 0.58 over the
 *   measurement interval as data governance has become more restrictive and
 *   institutional consolidation has increased. Theater ratio has risen from
 *   0.52 to 0.68 as procedural compliance (ethics review, data governance
 *   committees) has expanded without proportional strengthening of actual
 *   subject protection or research equity.
 *
 * KEY AGENTS:
 *   - Data Controllers (Institutional/Arbitrage): Government health agencies, large research medical centers, data corporations — control access, benefit from licensing fees and research priority
 *   - Equity Research Community (Powerless/Trapped): Independent researchers, researchers at under-resourced institutions, community-based researchers — cannot access data needed for structural determinants work; trapped by institutional policies
 *   - Mid-Tier Research Institutions (Moderate/Constrained): Regional universities, minority-serving institutions, international research centers — face high barriers and costs but have some negotiating power through partnerships
 *   - Disadvantaged Populations (Powerless/Trapped): Communities experiencing health inequities who generate data but cannot access aggregated findings or use it for their own health advocacy
 *   - Open Data Coalition (Organized/Constrained): Public health agencies, data liberation advocates, open-science networks — working toward alternative pathways with sunset timelines
 *   - Traditional IRB/Ethics Framework (Institutional/Arbitrage): Ethics committees, institutional governance structures — maintain oversight authority; see declining functional utility relative to procedural burden
 *   - Analytical Observer (Analytical/Analytical): Sees the natural law claim as a false summit — privacy protection is necessary but current institutional arrangements are contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_determinants_data_availability, 0.58).
domain_priors:suppression_score(structural_determinants_data_availability, 0.62).
domain_priors:theater_ratio(structural_determinants_data_availability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_determinants_data_availability, extractiveness, 0.58).
narrative_ontology:constraint_metric(structural_determinants_data_availability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(structural_determinants_data_availability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_determinants_data_availability, tangled_rope).
narrative_ontology:human_readable(structural_determinants_data_availability, "Structural Determinants Data Availability Constraint").
narrative_ontology:topic_domain(structural_determinants_data_availability, "epidemiology/public_health/data_governance").

domain_priors:requires_active_enforcement(structural_determinants_data_availability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_determinants_data_availability, data_controllers).
narrative_ontology:constraint_beneficiary(structural_determinants_data_availability, institutional_researchers).
narrative_ontology:constraint_beneficiary(structural_determinants_data_availability, regulatory_agencies).
narrative_ontology:constraint_victim(structural_determinants_data_availability, equity_research_community).
narrative_ontology:constraint_victim(structural_determinants_data_availability, public_health_capacity).
narrative_ontology:constraint_victim(structural_determinants_data_availability, disadvantaged_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EQUITY RESEARCH COMMUNITY (SNARE) — Cannot conduct structural determinants research without access to granular sociodemographic data. Trapped by institutional data governance policies that concentrate access in privileged institutions. Maximum extraction: unable to exit without abandoning research agenda. No meaningful coordination function perceived.
constraint_indexing:constraint_classification(structural_determinants_data_availability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER RESEARCH INSTITUTIONS (TANGLED ROPE) — Face high costs to access structural determinants data (data licensing fees, institutional partnerships, ethics review delays) but also benefit from standardized data frameworks and collaborative access arrangements. Constrained by resource requirements and regulatory compliance overhead. Both coordination function (shared data standards) and asymmetric extraction evident.
constraint_indexing:constraint_classification(structural_determinants_data_availability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DATA CONTROLLERS (ROPE) — Experience the constraint as pure coordination: controlling access to granular data enables standardized research workflows, centralized governance, and consistent methodologies. Net beneficiaries with arbitrage options (can sell data, control licensing, shape research agendas). Minimal extraction cost experienced.
constraint_indexing:constraint_classification(structural_determinants_data_availability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN DATA COALITION (ORGANIZED) — See the data access bottleneck as temporary and solvable through federated data platforms, differential privacy, and synthetic data approaches. Sunset logic: as technical standards mature (estimated 10-15 years), direct data access restrictions become less necessary. Sunset clause: commitment to expanding open-access data infrastructure with declining access barriers over time.
constraint_indexing:constraint_classification(structural_determinants_data_availability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL IRB/ETHICS FRAMEWORK (PITON) — Original function (protecting human subjects) has atrophied; replaced by theater: lengthy ethics reviews that delay research but provide minimal substantive protection for data subjects. The framework persists through institutional inertia despite declining functional utility. Theater ratio high (0.68) because much of the regulatory burden is procedural rather than protective.
constraint_indexing:constraint_classification(structural_determinants_data_availability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, some data privacy protection is inherent to protecting human subjects: you cannot eliminate all confidentiality requirements without violating fundamental ethical principles. This perspective sees the data access limitation as an immutable law of ethical research. However, the structural data contradicts this classification — the engine will compute this as a false summit, revealing that specific institutional arrangements (centralized access control, restrictive licensing, data silos) are contingent, not necessary.
constraint_indexing:constraint_classification(structural_determinants_data_availability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_determinants_data_availability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_determinants_data_availability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_determinants_data_availability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_determinants_data_availability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_determinants_data_availability, TR),
    TR >= 0.70.

:- end_tests(structural_determinants_data_availability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The data access bottleneck extracts research capacity from equity researchers and channels it toward privileged institutions. However, extraction is not total (0.66+ snare territory) because genuine coordination functions exist: standardized data formats do enable comparison; ethics review does protect against research harms; some spillover benefits reach less-privileged researchers. The rising trajectory (0.35 → 0.58) reflects increasing consolidation of data control and tightening of access policies over the measurement interval, consistent with platform-scale data concentration. Suppression (0.62): High. Barriers include: institutional licensing fees ($10k-$100k+ annually), data use agreement restrictions that require institutional affiliation and ethics approval, technical barriers (proprietary data formats, limited API access), and career risk (publication restrictions, data ownership disputes). These are substantial but not absolute — some data is open; some researchers have enough institutional power to negotiate access. Theater ratio (0.68): Moderately high. Much of the governance overhead is procedural: ethics committees that duplicate risk assessment, data governance committees that formalize existing access decisions, compliance reporting that serves accountability theater more than actual subject protection. The performative content has increased as institutional bureaucracy has grown without corresponding increases in actual protection.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits substantial perspectival disagreement. Data controllers see coordination (Rope) — they experience data governance as solving legitimate coordination problems (consistent methodologies, subject protection). Equity researchers see extraction (Snare) — they are trapped without alternatives. Mid-tier institutions see mixed dynamics (Tangled Rope) — they have some power but still face asymmetric costs. The open data coalition sees a solvable problem with sunset logic (Scaffold) — federated approaches and differential privacy can replace direct access restrictions within 10-15 years. The traditional ethics framework sees its own theater (Piton) — the oversight apparatus persists through inertia despite declining functional utility. The analytical observer risks a false summit (Mountain) — naturalizing contingent institutional arrangements as necessary privacy protection. The perspectival gaps reveal the constraint's hybrid nature and the conflicting interests at stake.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: who benefits from data restrictions, who bears costs, and what are their exit options. Data controllers (institutional/arbitrage) have low d (beneficiaries with exit) — they experience low effective extraction. Trapped researchers (powerless/trapped) have high d — maximum experienced extraction. Mid-tier institutions (moderate/constrained) have moderate d — partial exit options reduce experienced extraction but barriers remain significant. Disadvantaged populations (powerless/trapped) have high d — they bear extraction costs while generating the data they cannot access. The scaffold coalition (organized/constrained) has moderate-low d — they have agency and see exit pathways within a finite horizon. The derivation captures institutional power asymmetries: beneficiaries experience rope even as victims experience snare from the same structural arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how institutional arrangements naturalize contingent gatekeeping as necessary protection. The false summit (mountain classification from the analytical perspective) occurs when observers assume that 'privacy is immutable' therefore 'data restrictions are necessary' therefore 'current access policies are inevitable.' The structural data contradicts this chain: privacy protection (genuinely necessary) is being bundled with institutional gatekeeping (contingent) and research capacity concentration (extractive). The mandatrophy is resolved by decomposing the constraint into its coordination and extraction functions: the coordination function (privacy protection) is modest in extractiveness; the extraction function (research gatekeeping, access concentration) is substantial. The tangled rope classification captures both functions operating simultaneously. The scaffold perspective provides the real resolution: alternative pathways (federated data, differential privacy, synthetic data) can provide privacy protection without centralized gatekeeping. The sunset logic is testable: as technical alternatives mature, the justification for current access restrictions weakens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_extraction_boundary,
    'What level of privacy protection is inherently necessary versus what is unnecessary institutional gatekeeping?',
    'Comparative analysis of privacy approaches: countries/systems with open data, differential privacy, federated learning; measurement of actual privacy breaches and harms prevented vs. research capacity lost',
    'If boundary is very restrictive: most data access limitations are institutional extraction (snare dominates). If boundary is permissive: most limitations are justified privacy protection (rope dominates). Classification distribution shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_extraction_boundary, empirical, 'Boundary between necessary privacy and institutional gatekeeping').

omega_variable(
    disadvantaged_population_harm_measurement,
    'Can we measure the public health harm from lack of structural determinants data availability to disadvantaged populations?',
    'Epidemiological studies quantifying: mortality/morbidity differences in regions with vs without accessible structural determinants data; policy responsiveness analysis; equity outcome tracking',
    'If measurable large harm: victim status of disadvantaged populations is concrete and severe (snare classification strengthens). If harm is diffuse/unmeasurable: victim status is more abstract and coalition power is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disadvantaged_population_harm_measurement, empirical, 'Quantified public health harm from data unavailability').

omega_variable(
    synthetic_data_sufficiency,
    'Can synthetic data generation and differential privacy techniques genuinely replace direct access to granular sociodemographic data for structural determinants research?',
    'Validation studies comparing results from synthetic vs real data on known structural determinants effects; accuracy metrics on intersection-level estimates; research outcomes in jurisdictions with open vs closed data access',
    'If sufficiency confirmed: scaffold sunset logic is valid (alternative pathways exist within 10-15 years). If insufficient: data access remains necessary long-term and the snare dynamic persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_sufficiency, empirical, 'Whether synthetic data can substitute for direct data access').

omega_variable(
    institutional_capture_extent,
    'To what degree are data governance restrictions driven by privacy protection versus by institutional interests in maintaining research gatekeeping?',
    'Comparative institutional analysis: decision-making patterns for data access requests; correlation between researcher affiliation/power and approval rates; regulatory capture indicators',
    'If privacy-driven: classification shifts toward rope (coordination-dominant). If capture-driven: classification strengthens toward snare (extraction-dominant). Directionality calculations shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_extent, conceptual, 'Extent of institutional capture in data governance decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_determinants_data_availability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdda_tr_t0, structural_determinants_data_availability, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sdda_tr_t5, structural_determinants_data_availability, theater_ratio, 5, 0.62).
narrative_ontology:measurement(sdda_tr_t10, structural_determinants_data_availability, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(sdda_be_t0, structural_determinants_data_availability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sdda_be_t5, structural_determinants_data_availability, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sdda_be_t10, structural_determinants_data_availability, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_determinants_data_availability, resource_allocation).
narrative_ontology:affects_constraint(structural_determinants_data_availability, health_equity_measurement_gap).
narrative_ontology:affects_constraint(structural_determinants_data_availability, institutional_data_silos).
narrative_ontology:affects_constraint(structural_determinants_data_availability, research_capacity_concentration).

% DUAL FORMULATION NOTE:
% Structural determinants data availability is part of a constraint family addressing health equity data infrastructure. It is upstream of specific health outcome measurement constraints but downstream of institutional data consolidation dynamics. The family includes: data_silos (institutional arrangements creating barriers), this constraint (access/governance policies), and outcome_measurement_gaps (inability to track structural determinants effects at population level). Each has distinct ε; together they form an ecosystem of mutually-reinforcing extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_determinants_data_availability, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
