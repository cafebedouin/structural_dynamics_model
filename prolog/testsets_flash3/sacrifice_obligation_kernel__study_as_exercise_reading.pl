% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study of Sacrifice Law as Mitzvah Fulfillment (Study-as-Exercise Reading)
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents the 'study-as-exercise' reading of the
 *   sacrifice obligation kernel within Jewish law. It posits that the
 *   intellectual engagement with the laws of sacrifice constitutes a genuine
 *   fulfillment of the mitzvah, particularly in the absence of the Temple.
 *   This reading is a foundational aspect of rabbinic Judaism, providing a
 *   viable and accessible mode of religious practice. It is classified as a
 *   Mountain due to its deep integration into the halakhic system and its
 *   near-universal acceptance within mainstream Judaism, effectively
 *   collapsing alternatives for fulfillment. The beneficiaries are the
 *   rabbinic authority, which maintains its interpretive role, and the Jewish
 *   community, which gains a continuous path to religious observance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study of Sacrifice Law as Mitzvah Fulfillment (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__study_as_exercise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '394319d4-e425-4409-a6a4-60da06912e31').
narrative_ontology:cs_kernel_codification('394319d4-e425-4409-a6a4-60da06912e31', fixed_text).
narrative_ontology:cs_authority_grounding('394319d4-e425-4409-a6a4-60da06912e31', lineage).
narrative_ontology:cs_interpretation_layer_present('394319d4-e425-4409-a6a4-60da06912e31').
narrative_ontology:cs_reading_relation('394319d4-e425-4409-a6a4-60da06912e31', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('394319d4-e425-4409-a6a4-60da06912e31', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('394319d4-e425-4409-a6a4-60da06912e31', sacrifice_obligation_kernel__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('394319d4-e425-4409-a6a4-60da06912e31', foundational, intellectual_engagement_as_spiritual_fulfillment).
narrative_ontology:cs_axiom_status(intellectual_engagement_as_spiritual_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('394319d4-e425-4409-a6a4-60da06912e31', intellectual_engagement_as_spiritual_fulfillment, deontological).
narrative_ontology:cs_axiom('394319d4-e425-4409-a6a4-60da06912e31', secondary, halakha_adapts_to_historical_conditions).
narrative_ontology:cs_axiom_status(halakha_adapts_to_historical_conditions, holdable).
narrative_ontology:cs_axiom_grounding('394319d4-e425-4409-a6a4-60da06912e31', halakha_adapts_to_historical_conditions, conventional).
narrative_ontology:cs_reference_frame('394319d4-e425-4409-a6a4-60da06912e31', rabbinic_halakhic_continuity).
narrative_ontology:cs_drift_state('394319d4-e425-4409-a6a4-60da06912e31', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('394319d4-e425-4409-a6a4-60da06912e31', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, individual_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, torah_study_as_ultimate_value).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_adaptability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and adjudicates the halakha, defining what constitutes fulfillment of the mitzvah in the absence of the Temple. Benefits from the interpretive monopoly and the elevation of intellectual engagement as a primary religious act.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives a clear, accessible path to fulfill a central religious obligation through study, fostering continuity and intellectual engagement. This reading provides a meaningful way to engage with the mitzvah in the diaspora.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_community, beneficiary,
    organized, generational, identity_locked, global).

% Finds spiritual fulfillment and a sense of continuity with tradition through the study of sacrifice laws, even without the physical Temple. This provides a concrete religious practice accessible to all.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, individual_practitioners, beneficiary,
    moderate, biographical, identity_locked, local).

% Believe that only physical performance of sacrifices can fulfill the mitzvah, viewing study as preparatory but not substitutive. Their view is marginalized by the dominant rabbinic interpretation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, performance_only_advocates, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally accessible and intellectually rigorous method for the Jewish community to engage with and fulfill the mitzvah of sacrifices, ensuring continuity of religious practice in the absence of the Temple.
% TRANSFER_FUNCTION: Transforms the physical obligation of sacrifice into an intellectual and spiritual engagement, transferring the locus of fulfillment from a specific ritual act to ongoing study and interpretation.
% ABSENT_VOICES: Advocates for a 'performance-only' reading of the mitzvah are structurally excluded from the dominant halakhic discourse, as their position would invalidate the current mode of fulfillment for the vast majority of the community.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the Jewish community would lose a primary mode of engaging with the mitzvah of sacrifices, but the underlying obligation and the physical impossibility of performance would remain. The world would not rearrange, but a significant spiritual and intellectual practice would be lost.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the physical performance of sacrifices impossible, creating a crisis of how to fulfill a central divine commandment.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts and ongoing theological discourse within the Jewish community universally corroborate the problem of Temple destruction and the subsequent need for alternative modes of mitzvah fulfillment. This is not contested by any party.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__study_as_exercise_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because this reading provides a legitimate and accessible path to mitzvah fulfillment without imposing costs or suppressing alternatives for those who accept its premise. Suppression is minimal (0.05) as it primarily involves the internal coherence of a theological system, not active coercion. Accessibility collapse is high (0.95) because, for adherents of this reading, there are no other currently viable ways to fulfill the sacrifice mitzvah. Resistance is low (0.05) as this reading is widely accepted. The long interval reflects its historical persistence since the Temple's destruction.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap for those who accept this reading, as it offers a coherent and widely accepted path to religious observance. The gap primarily exists with those who adhere to alternative readings, who would experience this as a suppression of their preferred mode of fulfillment, but within this reading's frame, their alternatives are genuinely collapsed.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority acts as the agenda-setter, defining and upholding this interpretation, benefiting from its central role. The Jewish community and individual practitioners are beneficiaries, as this reading provides a meaningful and accessible religious practice. Advocates of alternative readings (e.g., 'performance-only') are excluded, as their positions are not recognized as valid within this dominant framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    halakhic_status_of_study,
    'Is the study of sacrifice law truly equivalent to performance, or is it a temporary substitute?',
    'Theological consensus shift or a future event (e.g., rebuilding of the Temple) that would re-enable physical performance, forcing a re-evaluation of the equivalence.',
    'If deemed a temporary substitute, the extractiveness might remain low, but the ''claimed_type'' could shift towards ''scaffold'' or ''piton'' if the temporary nature is forgotten or becomes performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(halakhic_status_of_study, conceptual, 'Ambiguity regarding the ultimate halakhic status of study as fulfillment.').

omega_variable(
    interpretive_monopoly_legitimacy,
    'Does the rabbinic authority''s interpretive monopoly on this reading genuinely serve the community''s spiritual needs, or does it primarily consolidate power?',
    'Historical analysis of dissenting voices and their suppression, or a shift in communal acceptance of rabbinic authority''s role in defining mitzvah fulfillment.',
    'If primarily power consolidation, the ''extractiveness'' could be re-evaluated upward, and the ''claimed_type'' might shift towards ''tangled_rope'' for the rabbinic authority seat, as the coordination function would be intertwined with asymmetric benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_monopoly_legitimacy, preference, 'Questioning the underlying legitimacy of the rabbinic interpretive monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t650, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 650, 0.0).
narrative_ontology:measurement(sacr_tr_t1300, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1300, 0.0).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1950, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t650, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 650, 0.0).
narrative_ontology:measurement(sacr_be_t1300, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1300, 0.0).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1950, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(sacr_su_t650, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 650, 0.05).
narrative_ontology:measurement(sacr_su_t1300, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1300, 0.05).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1950, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'sacrifice_obligation_kernel'. Each reading offers a distinct interpretation of how the mitzvah of sacrifices is fulfilled in the absence of the Temple, with differing implications for practice and authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
