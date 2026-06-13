% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Commitment (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the Aneyoshi Stone as a live land-use rule,
 *   where its directive to build above a certain elevation retained
 *   operational force in building location decisions for 78 years, from the
 *   1933 tsunami to the 2011 tsunami. In this reading, the stone functions as
 *   an active regulatory mechanism, and compliance with its directive is
 *   causally linked to the survival of the village in 2011. The constraint is
 *   presented as a Mountain due to its direct connection to the physical
 *   reality of tsunami hazards and its low extractiveness, with residents
 *   benefiting from its protective function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Commitment (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'c6fe876b-c6cb-48d4-93e8-91a57340b33c').
narrative_ontology:cs_kernel_codification('c6fe876b-c6cb-48d4-93e8-91a57340b33c', fixed_text).
narrative_ontology:cs_authority_grounding('c6fe876b-c6cb-48d4-93e8-91a57340b33c', lineage).
narrative_ontology:cs_interpretation_layer_present('c6fe876b-c6cb-48d4-93e8-91a57340b33c').
narrative_ontology:cs_reading_relation('c6fe876b-c6cb-48d4-93e8-91a57340b33c', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('c6fe876b-c6cb-48d4-93e8-91a57340b33c', foundational, stone_directive_is_active_land_use_rule).
narrative_ontology:cs_axiom_status(stone_directive_is_active_land_use_rule, holdable).
narrative_ontology:cs_axiom_grounding('c6fe876b-c6cb-48d4-93e8-91a57340b33c', stone_directive_is_active_land_use_rule, empirically_contingent).
narrative_ontology:cs_axiom('c6fe876b-c6cb-48d4-93e8-91a57340b33c', secondary, compliance_ensures_survival).
narrative_ontology:cs_axiom_status(compliance_ensures_survival, holdable).
narrative_ontology:cs_axiom_grounding('c6fe876b-c6cb-48d4-93e8-91a57340b33c', compliance_ensures_survival, empirically_contingent).
narrative_ontology:cs_reference_frame('c6fe876b-c6cb-48d4-93e8-91a57340b33c', post_1933_tsunami_rebuilding_directive).
narrative_ontology:cs_drift_state('c6fe876b-c6cb-48d4-93e8-91a57340b33c', pre_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c6fe876b-c6cb-48d4-93e8-91a57340b33c', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that the stone's directive imposes minimal cost, primarily restricting building location to higher ground, which is a direct safety benefit. Suppression (0.1) is low because compliance is largely voluntary, driven by a clear understanding of the hazard and intergenerational transmission of knowledge, rather than active coercion. The theater ratio is zero, as the stone's function is entirely practical and effective. Accessibility collapse is high (0.9) because the alternative (building below the stone's height) is understood to be catastrophically risky, effectively collapsing that option. Resistance is low (0.05) because the rule is widely accepted as essential for survival.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Aneyoshi residents, the stone's directive is a clear, life-saving rule, a 'natural law' of their environment. From an external, more skeptical perspective (e.g., the 'commemorative husk' reading), the stone might be seen as a symbolic artifact whose behavioral force is either imagined or has decayed. This story instantiates the 'behavioral competence' reading, where the stone's directive is genuinely operative.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are the primary beneficiaries (d=0.0) as the stone's directive directly protects their lives and property from tsunamis. There are no identifiable victims in this reading, as the constraint is understood to be a necessary adaptation to a natural hazard, not an extractive mechanism. The constraint subsidizes the residents by enabling their continued habitation in a high-risk area.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Aneyoshi Stone a live land-use rule (behavioral competence reading) or a commemorative husk (commemorative husk reading)?',
    'Empirical observation of land-use decisions post-2011 tsunami: if new construction continues to respect the stone''s elevation, the behavioral competence reading is strengthened. If construction disregards it, the commemorative husk reading is strengthened.',
    'If the behavioral competence reading is correct, the constraint is a Mountain (or Rope) that genuinely protects residents. If the commemorative husk reading is correct, it is a Piton or Snare, where the ''natural law'' claim is cover for inertia or extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Ambiguity between active land-use rule and symbolic memorial.').

omega_variable(
    natural_law_vs_social_norm,
    'Is the Aneyoshi Stone''s directive a natural law (unavoidable consequence of geography) or a deeply internalized social norm (cultural choice)?',
    'Cross-cultural comparison with similar coastal communities lacking such directives: if they consistently suffer higher casualties from tsunamis, it suggests the stone''s rule approximates a natural law. If not, it''s a powerful social norm.',
    'If a natural law, its Mountain classification is robust. If a social norm, it''s a highly effective Rope, but its persistence is contingent on cultural transmission, not physical inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_norm, conceptual, 'Distinction between a natural hazard constraint and a culturally enforced safety norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.0).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.0).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'Aneyoshi Stone Commitment' kernel. This 'behavioral competence' reading emphasizes the stone's active role in land-use decisions and village survival, contrasting with the 'commemorative husk' reading which views it as a symbolic artifact with decayed behavioral force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
