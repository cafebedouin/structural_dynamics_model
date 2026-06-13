% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Corpus: Study as Exercise of Mitzvah
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the rabbinic Jewish reading that the
 *   intellectual and spiritual study of the Kodashim (sacrificial) corpus is
 *   itself a fulfillment of the mitzvah (divine commandment) of sacrifice. It
 *   provides a means for continuous engagement with divine law and
 *   maintenance of cosmic order in the absence of the Temple and physical
 *   sacrifices. This reading is a core component of post-Temple Jewish
 *   practice, transforming a literal ritual into an ongoing
 *   intellectual-spiritual discipline.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.01).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.01).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Corpus: Study as Exercise of Mitzvah").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '5ed1079c-bf7f-4858-87b1-d1112c537d47').
narrative_ontology:cs_kernel_codification('5ed1079c-bf7f-4858-87b1-d1112c537d47', fixed_text).
narrative_ontology:cs_authority_grounding('5ed1079c-bf7f-4858-87b1-d1112c537d47', lineage).
narrative_ontology:cs_interpretation_layer_present('5ed1079c-bf7f-4858-87b1-d1112c537d47').
narrative_ontology:cs_reading_relation('5ed1079c-bf7f-4858-87b1-d1112c537d47', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('5ed1079c-bf7f-4858-87b1-d1112c537d47', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('5ed1079c-bf7f-4858-87b1-d1112c537d47', foundational, study_is_equivalent_to_action).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_action, holdable).
narrative_ontology:cs_axiom_grounding('5ed1079c-bf7f-4858-87b1-d1112c537d47', study_is_equivalent_to_action, theological).
narrative_ontology:cs_axiom('5ed1079c-bf7f-4858-87b1-d1112c537d47', secondary, divine_will_accommodates_circumstance).
narrative_ontology:cs_axiom_status(divine_will_accommodates_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('5ed1079c-bf7f-4858-87b1-d1112c537d47', divine_will_accommodates_circumstance, theological).
narrative_ontology:cs_reference_frame('5ed1079c-bf7f-4858-87b1-d1112c537d47', rabbinic_interpretive_continuity).
narrative_ontology:cs_drift_state('5ed1079c-bf7f-4858-87b1-d1112c537d47', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5ed1079c-bf7f-4858-87b1-d1112c537d47', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, talmudic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is negligible (0.01) because this reading imposes no material cost or deprivation; rather, it offers a path to spiritual fulfillment. Suppression is low (0.05) as adherence is voluntary and driven by theological conviction, not coercion. Theater ratio is zero (0.0) as the study is considered genuinely efficacious, not merely performative. Accessibility collapse is high (0.9) because, within this framework, there are few alternatives to study for fulfilling the mitzvah in the present era. Resistance is low (0.02) as this reading is widely accepted within mainstream Rabbinic Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the scholars and the community, this is a pure coordination mechanism, enabling religious life. From an external, purely historical-critical perspective, it is an interpretive adaptation to changed circumstances, but still not extractive. The core claim of fulfillment through study is central to its non-extractive nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Talmudic scholars are the agenda-setters and primary beneficiaries, as their intellectual-spiritual work is directly validated and elevated by this reading. The broader Jewish community is also a beneficiary, gaining a path to spiritual continuity. There are no victims, as no one is deprived or coerced by this interpretive framework; it offers a solution, not an imposition. Divine Will is a non-agent beneficiary, representing the theological fulfillment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_interpretive_fulfillment,
    'Is the ''study as exercise'' reading a complete fulfillment of the mitzvah of sacrifice, or a necessary but incomplete substitute for physical performance?',
    'Theological consensus shift or a future messianic era where physical sacrifice is restored, allowing for direct comparison of fulfillment modes.',
    'If incomplete, the constraint''s ''fulfillment'' aspect would be reclassified as a temporary scaffold, and the ''performance_only'' reading would gain theological weight, potentially increasing perceived extraction for those who cannot perform physical sacrifice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literal_vs_interpretive_fulfillment, conceptual, 'Ambiguity regarding the completeness of interpretive fulfillment versus literal ritual performance.').

omega_variable(
    identity_lock_strength,
    'To what extent is the ''identity_locked'' exit option for scholars and the community a result of genuine spiritual conviction versus institutional inertia or social pressure?',
    'Sociological studies of religious adherence and apostasy within the community, particularly among those who disengage from traditional practice.',
    'If primarily institutional inertia, the ''suppression'' metric might be understated, as internalized social pressure would contribute to the constraint''s persistence, even if not overtly coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The balance between genuine conviction and social/institutional factors in maintaining adherence to the ''study as exercise'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.0).
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__study_as_exercise, theater_ratio, 500, 0.0).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__study_as_exercise, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__study_as_exercise, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__study_as_exercise, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__study_as_exercise, base_extractiveness, 500, 0.01).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.01).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.01).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__study_as_exercise, base_extractiveness, 2000, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__study_as_exercise, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__study_as_exercise, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__study_as_exercise, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__study_as_exercise, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__study_as_exercise, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_corpus' kernel. This 'study_as_exercise' reading asserts that intellectual engagement with the laws of sacrifice is a complete fulfillment of the mitzvah, providing spiritual continuity. It is linked to the 'performance_only' reading (which awaits literal restoration) and the 'substitution_archive' reading (which views Kodashim as a superseded historical record).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
