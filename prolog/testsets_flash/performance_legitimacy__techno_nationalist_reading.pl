% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes a techno-nationalist reading of performance
 *   legitimacy, where a state's right to rule is justified by its ability to
 *   achieve technological self-sufficiency and global leadership in strategic
 *   industries. This involves massive directed investment, export controls,
 *   and prioritizing supply chain resilience, often at the expense of
 *   market-driven allocation and consumer sectors. It is one reading of the
 *   broader 'performance_legitimacy' kernel, which also includes quantitative
 *   growth, qualitative development, and livelihood security readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.7).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.85).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '2b6b76cc-1190-4021-95d6-473cfc06199f').
narrative_ontology:cs_kernel_codification('2b6b76cc-1190-4021-95d6-473cfc06199f', formalized).
narrative_ontology:cs_authority_grounding('2b6b76cc-1190-4021-95d6-473cfc06199f', lineage).
narrative_ontology:cs_interpretation_layer_present('2b6b76cc-1190-4021-95d6-473cfc06199f').
narrative_ontology:cs_reading_relation('2b6b76cc-1190-4021-95d6-473cfc06199f', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('2b6b76cc-1190-4021-95d6-473cfc06199f', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('2b6b76cc-1190-4021-95d6-473cfc06199f', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('2b6b76cc-1190-4021-95d6-473cfc06199f', foundational, technological_sovereignty_is_national_security).
narrative_ontology:cs_axiom_status(technological_sovereignty_is_national_security, holdable).
narrative_ontology:cs_axiom_grounding('2b6b76cc-1190-4021-95d6-473cfc06199f', technological_sovereignty_is_national_security, instrumental).
narrative_ontology:cs_axiom('2b6b76cc-1190-4021-95d6-473cfc06199f', foundational, state_directed_investment_for_strategic_industries).
narrative_ontology:cs_axiom_status(state_directed_investment_for_strategic_industries, holdable).
narrative_ontology:cs_axiom_grounding('2b6b76cc-1190-4021-95d6-473cfc06199f', state_directed_investment_for_strategic_industries, conventional).
narrative_ontology:cs_reference_frame('2b6b76cc-1190-4021-95d6-473cfc06199f', national_security_imperative).
narrative_ontology:cs_drift_state('2b6b76cc-1190-4021-95d6-473cfc06199f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2b6b76cc-1190-4021-95d6-473cfc06199f', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_tech_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_industrial_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planning_agencies).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, foreign_tech_firms).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates national resources towards strategic goals (e.g., securing critical supply chains, fostering innovation in key sectors) while simultaneously extracting from other sectors (e.g., diverting capital from consumer industries, suppressing market signals) and requiring active enforcement to maintain. Extractiveness is high due to the non-market allocation of resources and the costs imposed on suppressed sectors. Suppression is very high as market mechanisms and foreign competition are actively curtailed. Theater ratio is low, as the stated goals of national security and great-power status are genuinely pursued, even if the methods are inefficient or extractive.
 *
 * PERSPECTIVAL GAP:
 *   State planning agencies and national champion tech firms experience this as a necessary coordination mechanism for national survival and prosperity, justifying the costs. Market-driven allocation and consumer sectors, however, experience it as a highly extractive and suppressive force, diverting resources and limiting their growth. Foreign tech firms are simply excluded, seeing it as protectionism.
 *
 * DIRECTIONALITY LOGIC:
 *   National champion tech firms and the defense industrial complex are primary beneficiaries (d near 0.0) as they receive massive directed investment and protection. State planning agencies are agenda-setters and beneficiaries, as their power and mandate are amplified (d near 0.1). Market-driven allocation and consumer sectors are victims (d near 1.0) as resources are diverted from them and their growth is suppressed. Foreign tech firms are also victims, as they are actively excluded (d near 1.0).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of national interest, or a cover for rent-seeking by specific industrial sectors?',
    'Independent audits of directed investment efficacy and comparative analysis with market-driven outcomes in similar sectors.',
    'If rent-seeking, the constraint''s extractiveness is higher and its coordination function is weaker, pushing classification towards Snare. If genuine national interest, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Ambiguity between national interest and sectoral rent-seeking within the techno-nationalist reading.').

omega_variable(
    sibling_reading_impact_livelihood_security,
    'How would a ''livelihood_security_reading'' of performance legitimacy alter the structural priorities of this techno-nationalist constraint?',
    'Analysis of policy shifts in response to public demand for social welfare vs. strategic industry investment.',
    'A stronger livelihood_security_reading would likely reduce directed investment in strategic industries, reallocating resources to social programs, thereby reducing extraction from market-driven allocation and consumer sectors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_livelihood_security, conceptual, 'Impact of livelihood_security_reading on techno-nationalist priorities.').

omega_variable(
    sibling_reading_impact_quantitative_growth,
    'What is the tension between the techno-nationalist reading''s focus on strategic industries and a ''quantitative_growth_reading'' prioritizing overall GDP expansion?',
    'Economic modeling comparing growth rates under strategic industry focus versus broad-based market liberalization.',
    'A dominant quantitative_growth_reading would likely shift investment away from less efficient strategic sectors towards high-growth consumer or export-oriented industries, potentially reducing suppression of market signals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_quantitative_growth, conceptual, 'Tension between techno-nationalist and quantitative_growth readings.').

omega_variable(
    sibling_reading_impact_qualitative_development,
    'How does the ''qualitative_development_reading'' (innovation, sustainability) interact with the techno-nationalist drive for strategic industry leadership?',
    'Policy analysis of environmental regulations and innovation incentives in strategic sectors versus broader economic transformation.',
    'A stronger qualitative_development_reading might impose stricter environmental or efficiency standards on strategic industries, potentially increasing costs for national champions but aligning long-term goals. It could also shift focus from raw technological leadership to sustainable innovation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_qualitative_development, conceptual, 'Interaction between techno-nationalist and qualitative_development readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__techno_nationalist_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__techno_nationalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__techno_nationalist_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 15, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 15, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
