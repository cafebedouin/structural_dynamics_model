% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Nuclear Risk Acceptability: Comparative Risk Dominant Reading
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint represents the 'comparative risk dominant' reading of
 *   acceptable risk for energy, where nuclear power's risks are deemed
 *   acceptable primarily in comparison to the risks posed by fossil fuels and
 *   climate change. It does not posit an absolute threshold for nuclear
 *   safety but rather a relative one. The victim set includes
 *   climate-vulnerable populations (who benefit from nuclear's climate impact
 *   reduction) and future generations (who bear the waste burden), reflecting
 *   the complex trade-offs inherent in this framework. Temporal urgency
 *   regarding climate change often overrides intergenerational waste
 *   concerns.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.4).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.3).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Nuclear Risk Acceptability: Comparative Risk Dominant Reading").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '62e61379-9f04-42e2-94de-c94813d182e0').
narrative_ontology:cs_kernel_codification('62e61379-9f04-42e2-94de-c94813d182e0', formalized).
narrative_ontology:cs_authority_grounding('62e61379-9f04-42e2-94de-c94813d182e0', expertise).
narrative_ontology:cs_interpretation_layer_present('62e61379-9f04-42e2-94de-c94813d182e0').
narrative_ontology:cs_reading_relation('62e61379-9f04-42e2-94de-c94813d182e0', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('62e61379-9f04-42e2-94de-c94813d182e0', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('62e61379-9f04-42e2-94de-c94813d182e0', foundational, risk_is_relative_to_alternatives).
narrative_ontology:cs_axiom_status(risk_is_relative_to_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('62e61379-9f04-42e2-94de-c94813d182e0', risk_is_relative_to_alternatives, empirically_contingent).
narrative_ontology:cs_axiom('62e61379-9f04-42e2-94de-c94813d182e0', foundational, temporal_urgency_of_climate_mitigation_dominates).
narrative_ontology:cs_axiom_status(temporal_urgency_of_climate_mitigation_dominates, holdable).
narrative_ontology:cs_axiom_grounding('62e61379-9f04-42e2-94de-c94813d182e0', temporal_urgency_of_climate_mitigation_dominates, instrumental).
narrative_ontology:cs_reference_frame('62e61379-9f04-42e2-94de-c94813d182e0', rational_comparative_risk_assessment).
narrative_ontology:cs_drift_state('62e61379-9f04-42e2-94de-c94813d182e0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('62e61379-9f04-42e2-94de-c94813d182e0', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, local_communities_near_nuclear_sites).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).
:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the transfer of long-term waste burdens and localized risks. Suppression (0.3) is low, as the framework is largely accepted by policymakers and many environmental groups, though it faces resistance from anti-nuclear activists. Theater ratio (0.1) is low, as the comparative risk assessment is a genuine, if contested, analytical approach. The claimed type is 'rope' because it genuinely coordinates a complex policy problem, even with some inherent extraction.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear energy proponents and climate-vulnerable populations perceive this as a necessary and beneficial coordination mechanism, enabling climate action. Local communities near nuclear sites and future generations (represented by proxy) experience it as an extractive mechanism that externalizes risks. This divergence is central to the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear energy proponents and climate-vulnerable populations are beneficiaries, as the framework supports their goals. Local communities and future generations are victims, bearing the costs. Fossil fuel industries are excluded, as their risks are the 'comparison' point. Regulators are observers, applying the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (addressing climate change through comparative risk assessment) is still live. The classification as 'rope' prevents mislabeling it as pure extraction, acknowledging its genuine coordination function in a complex policy space, while still identifying its extractive elements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comparative_vs_absolute_risk,
    'Is nuclear risk truly acceptable only in a comparative sense, or should there be an absolute threshold for safety and waste management, regardless of alternatives?',
    'A global consensus on absolute safety standards for nuclear power, or a shift in public and political discourse that prioritizes absolute safety over comparative climate benefits.',
    'If an absolute threshold becomes dominant, the constraint would reclassify, likely increasing extractiveness and suppression as nuclear projects would face higher hurdles or be deemed unacceptable, shifting towards a ''snare'' for proponents or a ''mountain'' for safety advocates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(comparative_vs_absolute_risk, conceptual, 'Ambiguity between relative and absolute risk acceptability for nuclear energy.').

omega_variable(
    intergenerational_equity_weighting,
    'How should the risks to future generations from nuclear waste be weighted against the immediate and near-term benefits of climate change mitigation?',
    'Development of robust, intergenerationally equitable discount rates or ethical frameworks that explicitly value future generations'' well-being in risk assessments.',
    'A higher weighting for future generations'' risks would increase the perceived extractiveness of this constraint, potentially shifting it towards a ''tangled_rope'' or ''snare'' for current beneficiaries, as the costs borne by future generations would be more explicitly accounted for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_weighting, preference, 'Ethical weighting of intergenerational risks and benefits.').

omega_variable(
    kernel_reading_comparative_risk_dominant,
    'Is this constraint a genuine ''comparative risk dominant'' reading of acceptable energy risk, or is it a ''catastrophic tail dominant'' reading in disguise, with the tail risks of climate change being the dominant factor?',
    'Analysis of policy decisions: if decisions consistently prioritize avoiding climate catastrophe even at the expense of increasing other, smaller risks, it leans towards catastrophic tail dominance. If decisions are more balanced across a range of risks, it supports comparative risk dominance.',
    'If reclassified as ''catastrophic tail dominant'', the constraint''s extractiveness might be higher for those bearing the tail risks, and its justification would shift from broad comparison to specific catastrophic avoidance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_comparative_risk_dominant, conceptual, 'This constraint is one reading of the ''acceptable_risk_for_energy'' kernel, specifically the ''comparative_risk_dominant'' reading. Sibling readings include ''catastrophic_tail_dominant'' and ''expected_value_dominant''. This reading emphasizes the contingency of nuclear acceptability on fossil fuel alternatives, includes climate-vulnerable populations in the victim set, and prioritizes temporal urgency over intergenerational waste concerns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1980, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t1980, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1980, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, climate_change_mitigation_targets).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_subsidies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
