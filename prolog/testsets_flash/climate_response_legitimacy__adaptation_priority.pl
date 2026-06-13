% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response Legitimacy: Adaptation Priority
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint describes the 'adaptation priority' reading of legitimate
 *   climate response, which accepts a warming trajectory and focuses on
 *   protecting vulnerable populations through resilience and adaptive
 *   capacity. This approach allows wealthier nations to largely maintain
 *   their development models and defer costly mitigation, while low-income
 *   regions bear the immediate impacts and accumulate adaptation deficits.
 *   The constraint is framed as a necessary coordination mechanism but
 *   operates with significant asymmetric extraction.
 *
 * KEY AGENTS:
 *   - wealthy_nations: Agenda setter (institutional/mobile) — benefits from deferred mitigation
 *   - vulnerable_low_income_regions: Payer (powerless/trapped) — bears immediate impacts and adaptation costs
 *   - fossil_fuel_industries: Beneficiary (institutional/arbitrage) — benefits from continued operations
 *   - future_generations: Victim (powerless/trapped) — bears compounded long-term costs
 *   - climate_scientists: Observer (analytical/analytical) — provides data, not policy
 *   - global_north_consumers: Beneficiary (organized/constrained) — benefits from status quo, insulated from impacts
 *   - climate_justice_advocates: Excluded (moderate/constrained) — marginalized voices for systemic change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.75).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response Legitimacy: Adaptation Priority").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '70932df0-60ad-4dc3-86c4-5c10c41dcafc').
narrative_ontology:cs_kernel_codification('70932df0-60ad-4dc3-86c4-5c10c41dcafc', distributed).
narrative_ontology:cs_authority_grounding('70932df0-60ad-4dc3-86c4-5c10c41dcafc', extraction).
narrative_ontology:cs_interpretation_layer_present('70932df0-60ad-4dc3-86c4-5c10c41dcafc').
narrative_ontology:cs_reading_relation('70932df0-60ad-4dc3-86c4-5c10c41dcafc', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('70932df0-60ad-4dc3-86c4-5c10c41dcafc', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('70932df0-60ad-4dc3-86c4-5c10c41dcafc', foundational, warming_trajectory_is_accepted).
narrative_ontology:cs_axiom_status(warming_trajectory_is_accepted, holdable).
narrative_ontology:cs_axiom_grounding('70932df0-60ad-4dc3-86c4-5c10c41dcafc', warming_trajectory_is_accepted, conventional).
narrative_ontology:cs_axiom('70932df0-60ad-4dc3-86c4-5c10c41dcafc', foundational, vulnerable_populations_must_be_protected).
narrative_ontology:cs_axiom_status(vulnerable_populations_must_be_protected, holdable).
narrative_ontology:cs_axiom_grounding('70932df0-60ad-4dc3-86c4-5c10c41dcafc', vulnerable_populations_must_be_protected, deontological).
narrative_ontology:cs_reference_frame('70932df0-60ad-4dc3-86c4-5c10c41dcafc', pragmatic_adaptation_framework).
narrative_ontology:cs_drift_state('70932df0-60ad-4dc3-86c4-5c10c41dcafc', contemporary_climate_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70932df0-60ad-4dc3-86c4-5c10c41dcafc', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, vulnerable_low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, climate_migrants).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because the adaptation deficit for vulnerable regions is substantial ($350B gap), and the costs to future generations are compounded. Suppression (0.75) is high because the policy framework actively suppresses alternatives like aggressive mitigation or 'loss and damage' reparations, and vulnerable regions have limited capacity to resist. Theater ratio (0.40) is moderate and rising, as the 'protection' narrative increasingly masks the underlying deferral of responsibility and continued emissions. The measurements show a clear trend of increasing extractiveness and suppression over time, reflecting the growing costs of adaptation and the hardening of the policy framework against mitigation alternatives.
 *
 * PERSPECTIVAL GAP:
 *   Wealthy nations and fossil fuel industries perceive this as a legitimate, pragmatic response to an unavoidable problem, coordinating necessary adaptation. Vulnerable regions and future generations experience it as a form of structural extraction, where their well-being is sacrificed to preserve the economic models of the powerful. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a Rope-like function and victims experiencing a Snare-like dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and fossil fuel industries are clear beneficiaries (d near 0.0) as they avoid immediate, costly mitigation. Vulnerable low-income regions and future generations are primary targets (d near 1.0) as they bear the direct and deferred costs. Global North consumers are beneficiaries, enjoying the status quo. Climate justice advocates are excluded, their proposals suppressed. Climate scientists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates some adaptation efforts (preventing immediate humanitarian collapse) while simultaneously extracting from vulnerable populations and future generations by deferring mitigation. The mandatrophy analysis would focus on whether the 'unavoidable warming' premise has become a cover for continued emissions, and whether the coordination function (adaptation) is being used to justify the extraction (delayed mitigation). The rising extractiveness and theater ratio suggest a drift towards a more extractive function over time, indicating that the mandate may be outliving its original, more balanced, coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_deficit_causality,
    'Is the growing adaptation deficit in vulnerable regions primarily a consequence of insufficient funding for adaptation, or a structural outcome of continued high emissions by wealthy nations?',
    'Economic modeling that isolates the impact of funding levels versus global emissions trajectories on adaptation capacity and costs in vulnerable regions.',
    'If primarily due to insufficient funding, the constraint''s coordination function (resource allocation) is underperforming. If primarily due to continued emissions, the constraint''s extractive nature (deferring mitigation costs) is amplified, and the ''adaptation'' framing is more theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_deficit_causality, empirical, 'Determines whether the adaptation deficit is a funding problem or an emissions problem.').

omega_variable(
    intergenerational_cost_discounting,
    'To what extent does the current policy framework implicitly discount the costs borne by future generations, and how would a lower discount rate alter the perceived legitimacy of adaptation-first approaches?',
    'Ethical and economic analysis applying various intergenerational discount rates to the long-term costs and benefits of adaptation-priority versus mitigation-priority policies.',
    'A lower discount rate would significantly increase the perceived costs of deferred mitigation, potentially reclassifying the adaptation-priority approach as more extractive and less legitimate from an intergenerational perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_discounting, conceptual, 'Examines the ethical discounting of future costs in climate policy.').

omega_variable(
    framing_of_unavoidable_warming,
    'Is the ''unavoidable warming'' premise a genuine scientific constraint, or has it become a rhetorical tool to justify delayed mitigation and shift responsibility?',
    'Content analysis of policy documents and public discourse, combined with expert elicitation on the scientific certainty and policy implications of ''unavoidable'' warming versus ''avoidable'' warming through aggressive action.',
    'If primarily rhetorical, the constraint''s ''naturalness'' claim is undermined, increasing its perceived extractiveness and suppression of mitigation alternatives. If genuinely scientific, it reinforces the coordination function of adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_unavoidable_warming, conceptual, 'Assesses the rhetorical vs. scientific status of ''unavoidable warming''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_response_legitimacy__adaptation_priority, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(clim_tr_t2010, climate_response_legitimacy__adaptation_priority, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(clim_tr_t2020, climate_response_legitimacy__adaptation_priority, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(clim_tr_t2030, climate_response_legitimacy__adaptation_priority, theater_ratio, 2030, 0.4).
narrative_ontology:measurement(clim_tr_t2040, climate_response_legitimacy__adaptation_priority, theater_ratio, 2040, 0.45).
narrative_ontology:measurement(clim_tr_t2050, climate_response_legitimacy__adaptation_priority, theater_ratio, 2050, 0.5).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(clim_be_t2010, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(clim_be_t2020, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(clim_be_t2030, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(clim_be_t2040, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2040, 0.7).
narrative_ontology:measurement(clim_be_t2050, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2050, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(clim_su_t2010, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(clim_su_t2020, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(clim_su_t2030, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2030, 0.75).
narrative_ontology:measurement(clim_su_t2040, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2040, 0.78).
narrative_ontology:measurement(clim_su_t2050, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2050, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, global_carbon_markets).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, international_development_aid).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_migrant_policies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Climate Response Legitimacy' kernel. It focuses on adaptation, while 'mitigation_priority' focuses on emissions reduction and 'degrowth_transformation' on systemic economic change. Each reading has distinct beneficiaries, victims, and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
