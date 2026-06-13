% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint story represents the 'behavioral competence' reading of
 *   the Aneyoshi tsunami stone land-use prohibition. In this reading, the
 *   stone functions as a live, operationally enforced rule, guiding
 *   settlement away from hazardous zones based on historical tsunami data.
 *   The prohibition was actively maintained and respected for 78 years,
 *   demonstrating a successful intergenerational transmission of risk
 *   knowledge and a commitment to physical safety. The constraint is
 *   primarily driven by the natural law of tsunami physics, with social
 *   practice reinforcing its effect.
 *
 * KEY AGENTS:
 *   - coastal_residents: Beneficiary (moderate/constrained) — protected by the prohibition
 *   - local_authorities: Agenda Setter (institutional/constrained) — maintain the stone and enforce land-use rules
 *   - tsunami_physics: Observer (universal/analytical) — the underlying natural law that the stone warns against
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f').
narrative_ontology:cs_kernel_codification('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', fixed_text).
narrative_ontology:cs_authority_grounding('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', practice).
narrative_ontology:cs_interpretation_layer_present('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f').
narrative_ontology:cs_reading_relation('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', aneyoshi_land_use_prohibition__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', foundational, tsunami_risk_is_immutable).
narrative_ontology:cs_axiom_status(tsunami_risk_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', tsunami_risk_is_immutable, empirically_contingent).
narrative_ontology:cs_axiom('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', foundational, intergenerational_risk_transmission_is_effective).
narrative_ontology:cs_axiom_status(intergenerational_risk_transmission_is_effective, holdable).
narrative_ontology:cs_axiom_grounding('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', intergenerational_risk_transmission_is_effective, empirically_contingent).
narrative_ontology:cs_reference_frame('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', tsunami_hazard_avoidance_practice).
narrative_ontology:cs_drift_state('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ca3a1dab-3d8c-455f-8e9b-0ff3bb75295f', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, coastal_residents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint primarily serves to protect life and property, with minimal cost beyond foregoing settlement in a dangerous area. Suppression is low (0.1) as compliance is largely voluntary, driven by a shared understanding of risk rather than active coercion. Theater ratio is negligible (0.02) because the stone's function is direct and effective. Accessibility collapse is high (0.95) as the physical reality of tsunami risk makes alternatives to avoiding the zone nearly impossible. Resistance is very low (0.01) due to the clear and present danger the stone warns against.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of coastal residents, the stone is a protective measure, a 'mountain' of safety. From an analytical perspective, it is a successful instance of intergenerational risk communication, where the natural law is effectively translated into social practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal residents are beneficiaries (d=0.0) as the constraint directly protects them from harm. Local authorities, while administering the rule, are also beneficiaries of the safety and stability it provides. Tsunami physics is the ultimate 'beneficiary' in the sense that its laws are respected, but it is not an agent. No identifiable victims exist in this reading, as the prohibition prevents harm rather than imposing it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a vital safety mechanism as an inert or extractive constraint. The 'mandate' of the stone (to prevent settlement in tsunami-prone areas) remains 'live' because the underlying natural hazard persists. The low extractiveness and high accessibility collapse confirm its function as a genuine protective measure, not a decaying or captured one. The alternative 'commemorative_husk_reading' would classify it as a Piton, highlighting the importance of empirical observation of behavioral impact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the Aneyoshi land-use prohibition a genuine natural law (tsunami physics) or a socially constructed rule that benefits identifiable agents?',
    'Analysis of the stone''s efficacy in preventing settlement in hazardous zones, independent of active enforcement, and comparison with areas lacking such markers.',
    'If primarily natural law, its classification as Mountain is robust. If primarily social construct, the presence of beneficiaries (coastal_residents) would trigger a False Summit Mountain reclassification to Tangled Rope, indicating a coordination mechanism with latent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between physical constraint and social rule.').

omega_variable(
    behavioral_competence_vs_commemorative_husk,
    'Is the Aneyoshi stone a live land-use rule with behavioral force, or has it decayed into a commemorative husk without operational impact?',
    'Empirical observation of settlement patterns in the prohibited zone over time, post-tsunami reconstruction decisions, and interviews with residents regarding their understanding of the stone''s purpose. This reading (behavioral_competence_reading) asserts the former; the sibling reading (commemorative_husk_reading) asserts the latter.',
    'If the behavioral competence reading is correct, the constraint is a Mountain (or False Summit Mountain). If the commemorative husk reading is correct, the constraint is a Piton, as its functional mandate has atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_competence_vs_commemorative_husk, empirical, 'Contested operational status of the land-use prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 39, 0.02).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 78, 0.02).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 39, 0.05).
narrative_ontology:measurement(aney_be_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 78, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(aney_su_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 39, 0.1).
narrative_ontology:measurement(aney_su_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 78, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'aneyoshi_land_use_prohibition' kernel. The 'commemorative_husk_reading' is a sibling constraint that views the stone as a historical memorial without behavioral force, leading to a Piton classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
