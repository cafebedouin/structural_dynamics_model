% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy: Quantitative Growth Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes a political-economic system where the
 *   legitimacy of the ruling authority is primarily derived from its ability
 *   to deliver high GDP growth rates. This 'quantitative growth reading'
 *   prioritizes economic expansion and job creation, often through
 *   investment-driven models, even if it entails costs like export
 *   dependency, industrial overcapacity, or environmental degradation. The
 *   constraint is actively enforced through policy directives, performance
 *   evaluations for officials, and suppression of dissenting views that
 *   challenge the growth-first paradigm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.75).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy: Quantitative Growth Reading").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '5397668a-11f2-4874-879c-e15a21395cf0').
narrative_ontology:cs_kernel_codification('5397668a-11f2-4874-879c-e15a21395cf0', formalized).
narrative_ontology:cs_authority_grounding('5397668a-11f2-4874-879c-e15a21395cf0', lineage).
narrative_ontology:cs_interpretation_layer_present('5397668a-11f2-4874-879c-e15a21395cf0').
narrative_ontology:cs_reading_relation('5397668a-11f2-4874-879c-e15a21395cf0', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_reading_relation('5397668a-11f2-4874-879c-e15a21395cf0', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('5397668a-11f2-4874-879c-e15a21395cf0', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('5397668a-11f2-4874-879c-e15a21395cf0', foundational, gdp_growth_is_primary_legitimacy_metric).
narrative_ontology:cs_axiom_status(gdp_growth_is_primary_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('5397668a-11f2-4874-879c-e15a21395cf0', gdp_growth_is_primary_legitimacy_metric, conventional).
narrative_ontology:cs_axiom('5397668a-11f2-4874-879c-e15a21395cf0', foundational, economic_stability_requires_continuous_expansion).
narrative_ontology:cs_axiom_status(economic_stability_requires_continuous_expansion, holdable).
narrative_ontology:cs_axiom_grounding('5397668a-11f2-4874-879c-e15a21395cf0', economic_stability_requires_continuous_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('5397668a-11f2-4874-879c-e15a21395cf0', post_reform_era_growth_consensus).
narrative_ontology:cs_drift_state('5397668a-11f2-4874-879c-e15a21395cf0', contemporary_era_of_sustainability_calls, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5397668a-11f2-4874-879c-e15a21395cf0', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_advocates).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, labor_migrants).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, small_and_medium_enterprises).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates economic activity and provides jobs (a coordination function), but it does so with significant asymmetric extraction. The extraction comes from externalizing costs onto the environment, future generations, and often labor, while concentrating benefits on specific industrial sectors and officials whose careers depend on meeting growth targets. The high suppression (0.75) reflects the active enforcement of policies that prioritize growth and the suppression of alternative development models or critical voices. The rising extractiveness and suppression over time indicate an intensification of this growth-first approach, with increasing costs borne by victims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the industrial-export complex and local government officials, this is a necessary and effective coordination mechanism for national development and stability. From the perspective of environmental advocates or marginalized labor, it is a highly extractive system that sacrifices long-term well-being for short-term, unevenly distributed gains. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The industrial-export complex and local government officials are clear beneficiaries (d near 0.0) as their power and resources are directly tied to the growth model. Environmental advocates and labor migrants are victims (d near 1.0) as they bear the costs of pollution, resource depletion, and exploitative labor practices. SMEs are also victims, often struggling to compete with state-backed giants in a growth-driven economy. The general populace is a mixed bag, benefiting from job creation and rising living standards, but also bearing diffuse costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (delivering prosperity and stability) is still 'live' in the sense that growth is still desired, but its 'quantitative' interpretation has arguably outlived its optimal function. The persistence of this specific reading, despite calls for 'high-quality development' or 'livelihood security,' suggests a form of mandatrophy where the means (raw GDP growth) have become an end, sustained by the beneficiaries of the existing model. The classification as Tangled Rope, rather than Snare, acknowledges the genuine coordination function (jobs, economic activity) while highlighting the significant, actively enforced extraction that prevents a shift to less extractive development models.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint primarily driven by the ''quantitative_growth_reading'' of performance legitimacy, or are other readings (e.g., ''livelihood_security_reading'') also significantly shaping policy?',
    'Analysis of policy documents, budget allocations, and official rhetoric for explicit prioritization of GDP growth over other development metrics. If other metrics consistently override growth targets, reclassify as a different reading.',
    'If other readings are dominant, the constraint''s true beneficiaries and victims may shift, potentially altering its classification from Tangled Rope to a different type, or revealing a more complex, multi-layered constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the dominant reading of performance legitimacy.').

omega_variable(
    growth_vs_sustainability_tradeoff,
    'To what extent is the pursuit of high GDP growth rates inherently in conflict with environmental sustainability and social equity?',
    'Empirical studies correlating GDP growth with environmental degradation and Gini coefficient changes over time. Policy analysis of ''green GDP'' initiatives and their implementation success.',
    'If the conflict is inherent and severe, the ''quantitative_growth_reading'' is fundamentally extractive from the environment and future generations, reinforcing its Tangled Rope nature. If trade-offs are manageable, it might lean more towards a Rope with externalized costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_vs_sustainability_tradeoff, empirical, 'Trade-off between quantitative growth and other development goals.').

omega_variable(
    legitimacy_source_shift,
    'Is the state''s legitimacy genuinely derived from quantitative growth, or is this a convenient narrative to maintain an existing power structure that benefits from this growth?',
    'Public opinion surveys on sources of state legitimacy, analysis of social unrest triggers, and historical comparison with periods of low growth. If legitimacy persists despite low growth, the grounding is not purely quantitative.',
    'If growth is merely a cover, the constraint is more Snare-like, with the coordination story (stability through growth) serving as a justification for extraction. If it''s a genuine, albeit contested, source of legitimacy, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_shift, empirical, 'Ambiguity in the true source of state legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__quantitative_growth_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__quantitative_growth_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__quantitative_growth_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel. Its focus on quantitative growth rates structurally influences and often competes with other readings like 'livelihood_security_reading' and 'qualitative_development_reading' by allocating resources and political capital preferentially.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
