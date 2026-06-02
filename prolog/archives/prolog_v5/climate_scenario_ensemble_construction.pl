% ============================================================================
% CONSTRAINT STORY: climate_scenario_ensemble_construction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_scenario_ensemble_construction, []).

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
 *   constraint_id: climate_scenario_ensemble_construction
 *   human_readable: Climate Scenario Ensemble Construction in IPCC Assessment Process
 *   domain: climate_science/assessment_governance
 *
 * SUMMARY:
 *   The IPCC scenario ensemble construction process exhibits a structural
 *   tension between scientific integrity and political feasibility that
 *   creates a hybrid coordination-extraction constraint. The ensemble (a set
 *   of modeled climate pathways ranging from ~1.5°C to ~4°C warming) is
 *   presented as representing genuine scientific uncertainty and policy
 *   alternatives. However, the selection and weighting of scenarios is
 *   substantially determined by political negotiation among national
 *   governments rather than by scientific plausibility criteria alone. This
 *   creates an extractive mechanism: high-emission scenarios unlikely under
 *   current climate commitments remain in the ensemble, delaying urgent
 *   mitigation policy while maintaining the appearance of legitimate
 *   scientific debate. Simultaneously, the constraint has genuine
 *   coordination function — the ensemble enables international comparison,
 *   facilitates policy dialogue, and provides stable technical infrastructure
 *   for climate research. The constraint thus exhibits tangled rope
 *   characteristics: significant extraction alongside genuine coordination
 *   benefit, requiring active institutional enforcement, with beneficiaries
 *   (developed nations, carbon-intensive industries, modeling institutions)
 *   and victims (climate science integrity, vulnerable populations,
 *   low-emission pathway research).
 *
 * KEY AGENTS:
 *   - Climate Science Epistemic Reliability: Primary victim (powerless/trapped) — bears cost of implausible scenarios in ensemble; no decision power in selection process
 *   - Climate-Vulnerable Populations: Primary victim (powerless/trapped) — intergenerational and geographic exposure; political marginalization in ensemble construction; policy delayed by high-emission scenario inclusion
 *   - Low-Emission Pathway Researchers: Secondary victim (moderate/constrained) — constrained by institutional gatekeeping; funding and publication dependent on ensemble participation; marginalized by scenarios weighted toward feasibility for developed nations
 *   - Scenario Modeling Institutions: Primary beneficiary (institutional/arbitrage) — technical authority consolidated; stable funding; international collaboration enabled; no suppression experienced
 *   - Developed Nation Governments: Primary beneficiary (institutional/arbitrage) — ensemble enables deferral of costly commitments; high-emission scenarios justify 'exploration' of mitigation extension; institutional arbitrage capacity
 *   - Carbon-Intensive Industries: Secondary beneficiary (institutional/arbitrage) — ensemble scenarios provide technical legitimacy for business-as-usual extension; institutional arbitrage capacity
 *   - IPCC Assessment Process: Institutional actor (institutional/arbitrage) — maintains performative scientific deliberation covering actual political consensus-building
 *   - Climate Justice Networks: Organized victim (organized/constrained) — benefit from public data access; constrained by pressure to 'work within the assessment' rather than challenge it fundamentally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_scenario_ensemble_construction, 0.52).
domain_priors:suppression_score(climate_scenario_ensemble_construction, 0.58).
domain_priors:theater_ratio(climate_scenario_ensemble_construction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_scenario_ensemble_construction, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_scenario_ensemble_construction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_scenario_ensemble_construction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_scenario_ensemble_construction, tangled_rope).
narrative_ontology:human_readable(climate_scenario_ensemble_construction, "Climate Scenario Ensemble Construction in IPCC Assessment Process").
narrative_ontology:topic_domain(climate_scenario_ensemble_construction, "climate_science/assessment_governance").

domain_priors:requires_active_enforcement(climate_scenario_ensemble_construction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_scenario_ensemble_construction, developed_nation_governments).
narrative_ontology:constraint_beneficiary(climate_scenario_ensemble_construction, carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(climate_scenario_ensemble_construction, scenario_modeling_institutions).
narrative_ontology:constraint_victim(climate_scenario_ensemble_construction, climate_science_integrity).
narrative_ontology:constraint_victim(climate_scenario_ensemble_construction, low_emission_pathway_research).
narrative_ontology:constraint_victim(climate_scenario_ensemble_construction, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE SCIENCE EPISTEMIC RELIABILITY (SNARE) — Trapped within ensemble selection process; bears full cost of implausible high-emission scenarios presented as policy-neutral. Structural reliability of climate science is subordinated to political tractability demands. No exit mechanism; maximum experienced extraction.
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Trapped by assessment process that includes high-emission scenarios unlikely under current commitments, delaying serious mitigation policy. Demographic and geographic exposure combined with political marginalization create complete suppression. No decision power in ensemble construction; maximum intergenerational extraction.
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LOW-EMISSION PATHWAY RESEARCHERS (TANGLED ROPE) — Constrained by funding dependence and publication venue gatekeeping; receive genuine coordination benefit (access to ensemble infrastructure, data sharing, comparative analysis) alongside extraction (marginalization of deep-decarbonization research, subordination to ensemble consensus). Significant agency but high costs for deviation.
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SCENARIO MODELING INSTITUTIONS (ROPE) — Institutional beneficiaries with arbitrage capacity. Ensemble construction creates stable funding, publication pipeline, and technical authority. Experience constraint as pure coordination: ensemble standards enable research, data sharing, and international collaboration. No structural extraction experienced from their position.
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPED NATION GOVERNMENTS (ROPE) — Institutional beneficiaries. Ensemble structure with high-emission scenarios enables deferral of costly mitigation commitments ('scenarios under exploration') while maintaining scientific legitimacy. Pure coordination: aligns climate science communication with political convenience. Exit via arbitrage (can commission alternative assessments if unsatisfied).
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CARBON-INTENSIVE INDUSTRIES (ROPE) — Institutional beneficiaries with arbitrage capacity. Ensemble structure with feasible high-emission pathways reduces pressure for near-term transformation. Experience constraint as coordination: provides technical legitimacy for 'business-as-usual extension' scenarios. No extraction experienced; pure benefit.
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: IPCC ASSESSMENT PROCESS RITUAL (PITON) — The Working Group III ensemble construction appears as technical deliberation but operates largely as a political theater for consensus-building. The scenario selection process is substantially performative: prior political agreement on feasible ranges (constrained by negotiation, not science) shapes which model runs are presented. Theater ratio reflects that process maintains appearance of scientific neutrality while actual selection is political. Degraded function sustained by institutional inertia.
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: CLIMATE JUSTICE AND SCIENCE ADVOCACY NETWORKS (TANGLED ROPE) — Organized but constrained agents. Benefit from ensemble process (public data access, internationally comparable scenarios enable advocacy framing). Also experience extraction: institutional pressure to 'work within the assessment' reduces incentives for radical science (beyond IPCC bounds) and for direct political mobilization. Mixed benefit and constraint.
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER - NATURAL LAW VIEW (MOUNTAIN) — Claims that some ensemble diversity is inherent to climate science: model uncertainty, parameter sensitivity, and initial condition divergence create inevitable scenario spread. This perspective risks naturalizing what is actually a constrained choice: the model ensemble could be weighted by empirical validation, but instead is weighted by political acceptability. The mountain classification is a false summit — the engine will detect this as naturalization of contingent institutional politics.
constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_scenario_ensemble_construction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_scenario_ensemble_construction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_scenario_ensemble_construction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_scenario_ensemble_construction, TR),
    TR >= 0.70.

:- end_tests(climate_scenario_ensemble_construction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The ensemble structure creates asymmetric advantage for beneficiary nations (developed economies can defer mitigation; carbon-intensive industries can plan extensions) while imposing costs on victims (delayed policy, marginalized research, accumulated climate risk). The value reflects genuine coordination benefit (the ensemble enables policy dialogue) alongside significant extraction (the composition is politically determined, not scientifically optimized). The measurement shows extractiveness increasing from 0.35 to 0.52 over the interval, reflecting accumulation of extraction as scenario weighting has shifted toward political acceptability and away from empirical climate sensitivity research. Suppression (0.58): Moderate-high. Suppression mechanisms include: (1) institutional gatekeeping (access to ensemble infrastructure mediated by IPCC participation), (2) publication bias (scenarios outside ensemble range are harder to publish in high-impact venues), (3) funding concentration (climate research funding concentrated in institutions participating in ensemble), (4) rhetorical closure (ensemble framing creates appearance of settled options, limiting radical science outside bounds). Suppression is not total — alternative assessments exist, independent research proceeds — but institutional pressure is substantial. Theater ratio (0.65): Moderate-high and increasing. The ensemble selection process is substantially performative: (1) the working group composition itself is negotiated by governments (not purely scientific delegation), (2) scenario weighting reflects political agreement more than empirical climate sensitivity, (3) the appearance of scientific neutrality masks actual political consensus-building, (4) high-emission scenarios receive policy emphasis disproportionate to their assigned probability. The increasing theater reflects that as political stakes have risen (Paris Agreement, Net Zero commitments), the political nature of scenario selection has become more transparent while the performative scientific framing persists.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim perspectives is approximately 1.5 classification types (Rope vs. Snare). Beneficiaries see coordination that enables dialogue and policy development. Victims see extraction that delays policy while maintaining false appearance of legitimate scientific debate. The gap reveals that the ensemble constraint has genuine coordination function (it does enable dialogue) that is captured by beneficiaries for extractive purposes. This is the signature of Tangled Rope: the coordination mechanism is real, the extraction is real, and they are not separable. The piton perspective identifies that the ensemble's coordination function is partly performative — the ritual of 'evidence-based policy' masks actual political negotiation. The organized victim perspective (climate justice networks) shows intermediate experience: genuine benefit from data access and technical legitimacy, but constrained by pressure to remain within institutional bounds.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary institutions experience directionality d ≈ 0.10 (they benefit from the constraint, exit is costless for them — they have institutional arbitrage capacity). The victim populations experience d ≈ 0.95 (they bear maximum cost, exit is impossible for them). Modeling institutions experience d ≈ 0.05 (strong beneficiaries with full arbitrage capacity). Low-emission pathway researchers experience d ≈ 0.65 (constrained victims with some agency through publication and collaboration but facing institutional gatekeeping). The engine applies the sigmoid f(d) to produce effective extraction per agent: beneficiaries get negative or near-zero χ (institutional support for the constraint), victims get χ ≈ 1.0+ (maximum experienced extraction), constrained researchers get χ ≈ 0.80 (high extraction with some agency). The scenario-weighting toward political acceptability creates the directionality pattern: it benefits those with institutional power (low d, low χ) and extracts from those without (high d, high χ).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating why it cannot be classified as pure extraction (Snare) despite high suppression (0.58) and high experienced extraction from victim perspectives (χ ≥ 0.70). The genuine coordination function — the ensemble structure does enable international climate dialogue, does provide comparable scenarios for policy analysis, does facilitate research collaboration — is inseparable from the extraction mechanism. The beneficiaries benefit from coordination. The victims are extracted from through the same mechanism. This is not a case where one observer sees Rope and another sees Snare: it is a case where all observant actors see both coordination and extraction in the same institutional structure. The Tangled Rope classification captures this: the constraint exhibits χ = 0.52 × f(d) × σ(S) where d varies wildly by agent (0.05 for beneficiaries, 0.95 for victims) but the constraint type is stable because the coordination function is genuine. If the ensemble were pure political theater with no coordination, it would classify as Snare from the victim perspective (no coordination to justify the suppression). The genuine coordination function — even though it is captured and weaponized for extraction — prevents the Snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ensemble_weighting_criterion,
    'What principle determines which scenarios are included in the ensemble and how heavily they are weighted: scientific plausibility, policy tractability, or negotiated balance?',
    'Comparative analysis of ensemble composition across IPCC Assessment Reports; correlation between political shifts in mitigation commitments and ensemble reweighting; interview data from Working Group III leads on selection criteria',
    'If scientifically plausible: ensemble represents genuine uncertainty (Rope from most perspectives). If policy-tractable: ensemble represents political compromise (Snare/Tangled Rope). If negotiated balance: constraint is extractive (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ensemble_weighting_criterion, empirical, 'What determines scenario ensemble selection and weighting').

omega_variable(
    low_emission_pathway_suppression_mechanism,
    'Is the underrepresentation of deep-decarbonization scenarios (1.5°C pathways) due to scientific implausibility, resource constraints, or deliberate institutional suppression?',
    'Publication audit: comparison of 1.5°C and 2°C pathway research funding and journal acceptance rates; analysis of rejected model runs and their scientific justifications; historical comparison of scenario feasibility claims vs. subsequent empirical validation',
    'If implausible: classification confirmed as legitimate constraint. If resource-constrained: engineering problem (reallocate funding). If institutionally suppressed: suppression value increases, extraction confirmed, constraint reclassifies toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(low_emission_pathway_suppression_mechanism, empirical, 'Whether low-emission pathway suppression is scientific or institutional').

omega_variable(
    ensemble_performativity_threshold,
    'At what point does ensemble diversity become theater (multiple scenarios conveying false impression of open possibilities) rather than genuine uncertainty representation?',
    'Analysis of IPCC policy-relevant framing: do ensemble members with <10% probability receive equal emphasis to central estimates? Do high-emission scenarios receive policy weight disproportionate to scientific plausibility? Comparison of media coverage vs. WG3 likelihood statements',
    'If theater > 0.70: piton classification confirmed. If theater < 0.50: ensemble is genuine uncertainty representation. Theater threshold determines whether constraint is degraded ritual (piton) or active extraction mechanism (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ensemble_performativity_threshold, empirical, 'Threshold for ensemble diversity becoming performative').

omega_variable(
    vulnerable_population_representation_gap,
    'Do climate-vulnerable populations have meaningful input into scenario selection, or are their interests subordinated to developed-nation political preferences in ensemble construction?',
    'Analysis of IPCC working group composition; documentation of which nations/constituencies proposed which scenarios; comparison of vulnerability research integration vs. scenario weight; interview data from developing-nation representatives on negotiation dynamics',
    'If meaningful input: victim classification questionable (reclassify to constrained). If subordinated: snare classification confirmed, extraction mechanism is structural inequality in assessment governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_representation_gap, empirical, 'Degree of vulnerable population input into scenario selection').

omega_variable(
    alternative_assessment_viability,
    'Could independent scientific assessments (outside IPCC framework) provide higher-resolution deep-decarbonization scenarios without political pressure for ensemble balance?',
    'Comparative analysis of UNEP Emissions Gap Reports, World Bank climate assessments, and other non-IPCC products; evaluation of whether alternative assessments have equivalent policy influence; analysis of incentive structures that concentrate climate authority in IPCC',
    'If viable alternatives exist: beneficiary agents have capacity to escape via arbitrage (reclassify rope agents to rope + arbitrage capacity). If IPCC dominance is structural: constraint is locking (more snare-like). If viable but politically suppressed: suppression mechanism confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_assessment_viability, empirical, 'Whether viable alternative climate assessment frameworks exist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_scenario_ensemble_construction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_ens_tr_t0, climate_scenario_ensemble_construction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(clim_ens_tr_t3, climate_scenario_ensemble_construction, theater_ratio, 3, 0.59).
narrative_ontology:measurement(clim_ens_tr_t6, climate_scenario_ensemble_construction, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(clim_ens_be_t0, climate_scenario_ensemble_construction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_ens_be_t3, climate_scenario_ensemble_construction, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(clim_ens_be_t6, climate_scenario_ensemble_construction, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_scenario_ensemble_construction, information_standard).
narrative_ontology:affects_constraint(climate_scenario_ensemble_construction, mitigation_policy_delay_mechanism).
narrative_ontology:affects_constraint(climate_scenario_ensemble_construction, climate_science_authority_concentration).

% DUAL FORMULATION NOTE:
% The ensemble construction constraint is distinct from but structurally upstream of mitigation policy delay (the ensemble's high-emission scenarios enable deferral of urgent action) and from climate science authority concentration (the IPCC ensemble consolidates technical authority in a structure responsive to political consensus). These constraints form a family: the ensemble enables the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_scenario_ensemble_construction, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
