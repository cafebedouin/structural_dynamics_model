% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents a specific reading within Jewish Halakhic
 *   (religious law) discourse concerning the obligation of Temple sacrifices
 *   after the destruction of the Second Temple. This 'messianic suspension'
 *   reading posits that the divine obligation for sacrifices is not abrogated
 *   or transformed into other practices (like prayer or study as a
 *   substitute), but rather divinely suspended until the messianic era and
 *   the rebuilding of the Temple. The current obligation is to maintain the
 *   knowledge and operational readiness for that future time through study,
 *   rather than to perform the sacrifices themselves. This reading is a
 *   Mountain because it asserts a divine decree as an unchangeable fact, with
 *   minimal extraction as the obligation is in abeyance, not violated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '8475c1a5-bea0-46c2-a183-ac41d6345180').
narrative_ontology:cs_kernel_codification('8475c1a5-bea0-46c2-a183-ac41d6345180', fixed_text).
narrative_ontology:cs_authority_grounding('8475c1a5-bea0-46c2-a183-ac41d6345180', lineage).
narrative_ontology:cs_interpretation_layer_present('8475c1a5-bea0-46c2-a183-ac41d6345180').
narrative_ontology:cs_reading_relation('8475c1a5-bea0-46c2-a183-ac41d6345180', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('8475c1a5-bea0-46c2-a183-ac41d6345180', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('8475c1a5-bea0-46c2-a183-ac41d6345180', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('8475c1a5-bea0-46c2-a183-ac41d6345180', foundational, divine_suspension_of_mitzvah).
narrative_ontology:cs_axiom_status(divine_suspension_of_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('8475c1a5-bea0-46c2-a183-ac41d6345180', divine_suspension_of_mitzvah, theological).
narrative_ontology:cs_axiom('8475c1a5-bea0-46c2-a183-ac41d6345180', foundational, study_as_operational_readiness).
narrative_ontology:cs_axiom_status(study_as_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('8475c1a5-bea0-46c2-a183-ac41d6345180', study_as_operational_readiness, conventional).
narrative_ontology:cs_reference_frame('8475c1a5-bea0-46c2-a183-ac41d6345180', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('8475c1a5-bea0-46c2-a183-ac41d6345180', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8475c1a5-bea0-46c2-a183-ac41d6345180', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_israel).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_scholars).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__messianic_suspension_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.1) because the obligation is suspended, not actively extracting from non-performance; the 'cost' is the maintenance of study, which is framed as a positive act of readiness. Suppression is low (0.05) as there's no active coercion against performing sacrifices (it's impossible) or against studying them. Theater ratio is low (0.1) because the study is genuinely aimed at maintaining operational readiness, not merely symbolic performance. Accessibility collapse is high (0.9) because the physical performance of sacrifices is genuinely impossible without the Temple. Resistance is low (0.02) as this reading is widely accepted within Orthodox Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a divine truth, a Mountain. From other readings (e.g., 'study as exercise' or 'performance only'), the nature of the obligation and its current fulfillment would be experienced differently, leading to different classifications. This story focuses solely on the 'messianic suspension' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are the agenda-setters, interpreting and transmitting the divine law. Future generations of Israel are the beneficiaries, inheriting the preserved knowledge and the expectation of restoration without current burden. The contemporary Jewish community bears the diffuse cost of supporting this scholarly tradition. The divine command is an analytical observer, the ultimate source of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_suspension_vs_human_interpretation,
    'Is the suspension of sacrifice obligation a direct divine decree, or an interpretive construct by human scholars to navigate an impossible situation?',
    'Theological consensus across diverse halakhic traditions, or a future messianic event that explicitly clarifies the divine will.',
    'If purely a human construct, the ''emerges_naturally'' claim would be weakened, potentially reclassifying it from Mountain to a more constructed type (e.g., Rope or Tangled Rope, depending on beneficiary structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_suspension_vs_human_interpretation, conceptual, 'Ambiguity between divine decree and human interpretation of the suspension.').

omega_variable(
    study_as_readiness_vs_substitution,
    'To what extent does the ''study maintains operational readiness'' function genuinely prepare for future performance, versus serving as a psychological or communal substitute for the actual mitzvah?',
    'Empirical analysis of the content and methodology of study: does it focus on practical, performative details, or more abstract, theoretical aspects? Comparison with pre-Temple destruction study methods.',
    'If study is primarily a substitution, the ''low extractiveness'' claim might be challenged, as it would imply a ''cost'' of not performing the actual mitzvah, potentially shifting the classification towards a more extractive type if the substitution is enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_readiness_vs_substitution, empirical, 'The true function of study: readiness or substitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.1).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.1).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2000, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_kernel', which is contested across several halakhic and theological interpretations. This reading emphasizes divine suspension and operational readiness through study.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
