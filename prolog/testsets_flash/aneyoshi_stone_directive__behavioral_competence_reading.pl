% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive (Behavioral Competence Reading)
 *   domain: disaster_anthropology/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi Stone Directive, a series of stone markers in coastal Japan,
 *   indicates the maximum historical tsunami inundation line, serving as a
 *   binding land-use constraint. This 'behavioral competence' reading
 *   interprets the directive as a continuously active, physically grounded
 *   constraint that guides safe settlement patterns, with its authority
 *   derived from the immutable reality of tsunami risk. It is a mountain
 *   because its force comes from natural law, not human enforcement, and
 *   compliance is a direct response to physical reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '57fe66f6-1a0f-4e92-9d6c-a6b269098f2d').
narrative_ontology:cs_kernel_codification('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', fixed_text).
narrative_ontology:cs_authority_grounding('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', practice).
narrative_ontology:cs_interpretation_layer_present('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d').
narrative_ontology:cs_reading_relation('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', aneyoshi_stone_directive__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', foundational, tsunami_risk_is_immutable).
narrative_ontology:cs_axiom_status(tsunami_risk_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', tsunami_risk_is_immutable, empirically_contingent).
narrative_ontology:cs_axiom('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', foundational, historical_inundation_lines_are_binding).
narrative_ontology:cs_axiom_status(historical_inundation_lines_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', historical_inundation_lines_are_binding, conventional).
narrative_ontology:cs_reference_frame('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', pre_tsunami_behavioral_competence).
narrative_ontology:cs_drift_state('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('57fe66f6-1a0f-4e92-9d6c-a6b269098f2d', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint primarily imposes a safety measure, not a rent. Suppression is low (0.1) as adherence is largely voluntary, driven by self-preservation and cultural memory rather than active coercion. Theater ratio is low (0.05) as the stones serve a direct, functional purpose. The constraint's persistence is due to the recurring physical threat, not institutional inertia. The metrics remain stable over time, reflecting the unchanging nature of the underlying physical constraint.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap in this reading, as the constraint's force is understood to derive from an objective physical reality. All actors, regardless of their immediate role, ultimately face the same physical limits. The divergence from the 'commemorative husk' reading lies in whether the stones retain active behavioral force or are merely historical artifacts.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal residents are payers in the sense that they bear the 'cost' of adhering to safer, less convenient land use, but they are also beneficiaries of the safety provided. Local government acts as an agenda-setter by incorporating the directive into planning, but its authority is ultimately derived from the physical reality of tsunami risk. Tsunami risk itself is an 'observer' in an analytical sense, representing the ultimate, impersonal force that makes the constraint necessary. Future generations are clear beneficiaries of the safety preserved by the directive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_passive_compliance,
    'Is the observed compliance with the stone directive an active, conscious behavioral response to the perceived tsunami risk, or a passive, inertial adherence to historical settlement patterns?',
    'Sociological studies of community decision-making, interviews with residents, and analysis of building permit applications in relation to the markers. If new construction consistently respects the line without explicit enforcement, it suggests active competence.',
    'If compliance is active, this reading''s ''mountain'' classification is strengthened, as the constraint''s force is directly perceived. If passive, it leans towards the ''commemorative_husk_reading'', where the stones are inert.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_passive_compliance, empirical, 'Distinguishes between active behavioral competence and passive historical inertia in adherence to the directive.').

omega_variable(
    natural_law_vs_cultural_norm,
    'To what extent is the Aneyoshi Stone Directive a ''natural law'' (a direct reflection of physical reality) versus a ''cultural norm'' (a socially constructed rule that happens to align with physical reality)?',
    'Comparative analysis with other tsunami-prone regions lacking such directives: do they spontaneously develop similar land-use patterns after disasters, or do they require active policy intervention? If similar patterns emerge, it supports the natural law aspect.',
    'If primarily natural law, the ''mountain'' classification is robust. If significantly a cultural norm, it introduces a ''rope'' or ''tangled_rope'' element, as cultural norms require social maintenance, even if aligned with physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_cultural_norm, conceptual, 'Clarifies the balance between physical necessity and cultural construction in the directive''s authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
