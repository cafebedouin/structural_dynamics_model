% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Bodily Autonomy Primacy in Vaccine Mandate Legitimacy
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the reading of vaccine mandate legitimacy
 *   where bodily autonomy is considered an absolute, self-sovereign right,
 *   making any state coercion categorically impermissible, regardless of
 *   public health outcomes. It is framed as a natural law, an unchangeable
 *   principle. This reading places immunocompromised individuals and other
 *   vulnerable populations at high exposure risk into the victim set, as
 *   their protection through collective immunity is foreclosed. Liberty
 *   advocacy movements and individuals opposed to mandates are the primary
 *   beneficiaries of this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.85).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.95).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Bodily Autonomy Primacy in Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '4a9e88a4-9493-493a-b63a-026f3bdae193').
narrative_ontology:cs_kernel_codification('4a9e88a4-9493-493a-b63a-026f3bdae193', formalized).
narrative_ontology:cs_authority_grounding('4a9e88a4-9493-493a-b63a-026f3bdae193', lineage).
narrative_ontology:cs_interpretation_layer_present('4a9e88a4-9493-493a-b63a-026f3bdae193').
narrative_ontology:cs_reading_relation('4a9e88a4-9493-493a-b63a-026f3bdae193', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4a9e88a4-9493-493a-b63a-026f3bdae193', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('4a9e88a4-9493-493a-b63a-026f3bdae193', foundational, bodily_autonomy_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('4a9e88a4-9493-493a-b63a-026f3bdae193', bodily_autonomy_absolute, deontological).
narrative_ontology:cs_axiom('4a9e88a4-9493-493a-b63a-026f3bdae193', foundational, state_coercion_categorically_impermissible_in_medical_matters).
narrative_ontology:cs_axiom_status(state_coercion_categorically_impermissible_in_medical_matters, holdable).
narrative_ontology:cs_axiom_grounding('4a9e88a4-9493-493a-b63a-026f3bdae193', state_coercion_categorically_impermissible_in_medical_matters, deontological).
narrative_ontology:cs_reference_frame('4a9e88a4-9493-493a-b63a-026f3bdae193', unconditional_individual_sovereignty).
narrative_ontology:cs_drift_state('4a9e88a4-9493-493a-b63a-026f3bdae193', contemporary_pandemic_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4a9e88a4-9493-493a-b63a-026f3bdae193', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_opposed_to_mandates).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations_at_high_exposure_risk).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the principle, when applied, extracts the right to collective protection from vulnerable groups. Suppression (0.95) is also very high, as this reading actively suppresses any alternative public health interventions that would infringe on individual autonomy, regardless of their efficacy. The theater ratio is low (0.1) because the principle is genuinely held and applied, not merely performed. Accessibility collapse is high (0.9) as it fundamentally collapses the legitimacy of any alternative public health policy that involves coercion. Resistance (0.7) is high from those who advocate for public health measures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of liberty advocates, this is a Mountain, an unassailable principle. From the perspective of public health officials and vulnerable populations, it operates as a Snare, extracting collective safety and imposing significant risks. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals opposed to mandates and liberty advocacy movements are beneficiaries (d=0.0-0.1) as their core values are upheld and their freedom from state intervention is maximized. Immunocompromised individuals and vulnerable populations are victims (d=0.9-1.0) as they bear the direct costs of increased exposure risk due to the absence of mandates. Public health authorities are targets (d=0.8-0.9) as their ability to implement protective measures is severely constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, framed as a Mountain, asserts an eternal and unchanging principle. Mandatrophy analysis would focus on whether the 'naturalness' of this absolute autonomy is genuinely universal or a historically contingent construct. If it were found to be a constructed principle whose original justification (e.g., protection against specific historical abuses) has atrophied, but it persists due to institutional inertia or concentrated beneficiaries, it would reclassify from a Mountain to a Snare or Tangled Rope. The current framing resists such analysis by asserting natural law status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_autonomy,
    'Is the absolute primacy of bodily autonomy a natural law, or a constructed legal/ethical principle that benefits identifiable agents?',
    'Philosophical and legal analysis of the historical evolution of autonomy concepts, and empirical analysis of the social and political movements that champion absolute autonomy in specific contexts.',
    'If a natural law, the constraint is a genuine Mountain. If constructed, and benefits accrue to specific groups, it would reclassify as a Tangled Rope or Snare, depending on the degree of extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_autonomy, conceptual, 'Ambiguity between natural law and constructed principle for bodily autonomy.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''bodily_autonomy_primacy_reading'' of the ''vaccine_mandate_legitimacy'' kernel. What would change if a sibling reading were adopted?',
    'Adoption of ''public_health_primacy_reading'' would shift the victim set (unvaccinated become targets, not beneficiaries) and reframe the constraint as a Rope or Tangled Rope. Adoption of ''risk_stratification_reading'' would introduce proportionality tests, making blanket mandates Snares but targeted ones potentially Ropes.',
    'The classification of the constraint and the identification of beneficiaries/victims would fundamentally change, reflecting a different balance of rights and duties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the vaccine mandate legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 10, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 5, 0.92).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 10, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, identity_coordination).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vaccine_mandate_legitimacy' kernel. This reading asserts the absolute primacy of bodily autonomy, foreclosing state coercion. The other readings (public_health_primacy_reading, risk_stratification_reading) offer alternative frameworks for mandate legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
