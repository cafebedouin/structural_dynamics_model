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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Study as Fulfillment of Sacrifice Obligation (Halakhic Reading)
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'study as exercise' reading of the
 *   sacrifice obligation kernel. It posits that intellectual engagement with
 *   the laws of sacrifice constitutes a genuine fulfillment of the mitzvah
 *   (divine commandment) in the absence of the Temple. This reading provides
 *   a vital mechanism for continuity of religious practice and identity,
 *   transforming an otherwise unfulfillable obligation into an accessible
 *   one. The constraint is characterized by very low extractiveness and
 *   suppression, as it is largely an interpretive norm that benefits its
 *   adherents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.1).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study as Fulfillment of Sacrifice Obligation (Halakhic Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '5345b9ea-3402-4114-b8de-cca988ff01a1').
narrative_ontology:cs_kernel_codification('5345b9ea-3402-4114-b8de-cca988ff01a1', fixed_text).
narrative_ontology:cs_authority_grounding('5345b9ea-3402-4114-b8de-cca988ff01a1', lineage).
narrative_ontology:cs_interpretation_layer_present('5345b9ea-3402-4114-b8de-cca988ff01a1').
narrative_ontology:cs_reading_relation('5345b9ea-3402-4114-b8de-cca988ff01a1', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('5345b9ea-3402-4114-b8de-cca988ff01a1', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('5345b9ea-3402-4114-b8de-cca988ff01a1', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('5345b9ea-3402-4114-b8de-cca988ff01a1', foundational, torah_study_is_equivalent_to_action).
narrative_ontology:cs_axiom_status(torah_study_is_equivalent_to_action, holdable).
narrative_ontology:cs_axiom_grounding('5345b9ea-3402-4114-b8de-cca988ff01a1', torah_study_is_equivalent_to_action, deontological).
narrative_ontology:cs_axiom('5345b9ea-3402-4114-b8de-cca988ff01a1', foundational, halakha_adapts_to_circumstance).
narrative_ontology:cs_axiom_status(halakha_adapts_to_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('5345b9ea-3402-4114-b8de-cca988ff01a1', halakha_adapts_to_circumstance, theological).
narrative_ontology:cs_reference_frame('5345b9ea-3402-4114-b8de-cca988ff01a1', post_temple_rabbinic_halakha).
narrative_ontology:cs_drift_state('5345b9ea-3402-4114-b8de-cca988ff01a1', contemporary_diaspora_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5345b9ea-3402-4114-b8de-cca988ff01a1', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, students_of_torah).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and transmits the halakhic framework, providing a legitimate path for fulfilling the sacrifice obligation. Gains legitimacy and influence by maintaining the continuity of religious practice through interpretation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, beneficiary).

% Fulfill a core religious obligation through their intellectual engagement with sacred texts, integrating their study with their spiritual life and maintaining their religious identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, students_of_torah, beneficiary,
    moderate, biographical, identity_locked, global).

% Are provided with a clear, accessible, and authoritative path to fulfill a central divine commandment, maintaining their connection to tradition and avoiding a crisis of unfulfillable mitzvot.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, lay_adherents, beneficiary,
    powerless, biographical, constrained, global).

% Believe that only physical performance of sacrifices fulfills the mitzvah and are marginalized by this reading, which deems their desired action currently impossible or insufficient for full fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, performance_only_advocates, excluded,
    organized, generational, constrained, global).

% Believe the sacrifice obligation is entirely suspended until the messianic era and that study, while meritorious, does not constitute fulfillment. This reading's claim of current fulfillment contradicts their view.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_suspension_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__study_as_exercise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a halakhically legitimate and accessible means for adherents to fulfill the divine commandment of sacrifices in the absence of the Temple, ensuring continuity of religious practice.
% TRANSFER_FUNCTION: Transforms the locus of religious fulfillment from physical ritual to intellectual engagement; transfers authority for defining and mediating this fulfillment to rabbinic interpretation and scholarship.
% ABSENT_VOICES: Advocates for the 'performance only' or 'messianic suspension' readings are structurally excluded from the dominant discourse that defines current halakhic fulfillment. They would argue that study, while valuable, does not genuinely fulfill the mitzvah.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, a core religious obligation would become unfulfillable for centuries, leading to a profound crisis of faith, practice, and identity within Jewish communities. The entire structure of post-Temple halakha would be destabilized.
% FOUNDING_PROBLEM: How to maintain the continuity of divine commandments, specifically the sacrifice obligation, and provide a path for religious fulfillment after the destruction of the Second Temple rendered physical performance impossible.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts (Talmud, Midrash), centuries of communal religious practice, and theological discourse from across diverse Jewish movements corroborate the ongoing nature of the problem and the interpretive solution. No external, non-benefiting parties are required to attest to this internal religious problem.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because this reading provides a solution to an otherwise intractable problem, offering a net benefit to all participants without imposing significant costs. Suppression is low (0.1) as it's an interpretive framework, not enforced by coercion, though alternative readings are marginalized. Theater ratio is also very low (0.05) because the act of study is considered genuine fulfillment, not a mere performance or placeholder. Accessibility collapse is high (0.8) because the original form of the mitzvah (physical sacrifice) is impossible, but the interpretive solution (study) is highly accessible. Resistance is low (0.1) due to the widespread acceptance of this reading within many Jewish communities.
 *
 * PERSPECTIVAL GAP:
 *   While this reading is largely seen as beneficial by its adherents, advocates of the 'performance only' or 'messianic suspension' readings would experience this constraint differently. They might view it as an insufficient or even illegitimate substitute for the true mitzvah, or as an unwarranted claim of fulfillment in a period of divine suspension. The engine's per-seat classification would reflect these divergent experiences based on their structural relationship to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority acts as both agenda-setter and beneficiary, as their interpretive monopoly defines the legitimate path for fulfillment, thereby reinforcing their institutional role. Students of Torah and lay adherents are beneficiaries, as they gain a means to fulfill a central religious obligation and maintain their connection to tradition. There are no direct victims, as the transformation of the mitzvah is seen as an authorized adaptation rather than an extractive imposition. Advocates of alternative readings are 'excluded' as their views are not the dominant interpretive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively prevents mandatrophy. The original mandate (physical sacrifice) became functionally obsolete with the Temple's destruction. This reading transforms the mandate, ensuring its continued relevance and preventing the obligation from atrophying into a mere historical curiosity. It is a successful adaptation that maintains the vitality of the religious system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_intent_vs_human_interpretation,
    'To what extent does this reading accurately reflect divine intent for the sacrifice obligation, versus being a necessary human adaptation to historical circumstances?',
    'Theological and philosophical analysis of scriptural hermeneutics and the nature of divine law; comparative study of how other religious traditions adapt core commandments to changed realities.',
    'If primarily a human adaptation, the constraint''s ''naturalness'' (emerges_naturally) would be lower, and its reliance on rabbinic authority for legitimacy would be more pronounced. If seen as divinely sanctioned transformation, its ''rope'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_intent_vs_human_interpretation, conceptual, 'Ambiguity regarding the source and authority of the interpretive transformation.').

omega_variable(
    interpretive_monopoly_extraction,
    'Does the ''interpretive monopoly'' of rabbinic authority, even with low extractiveness, constitute a subtle form of extraction by channeling religious energy and resources primarily into rabbinic-controlled study?',
    'Sociological study of resource allocation within religious communities, examining whether alternative forms of religious expression or leadership are systematically disadvantaged or suppressed by the emphasis on study as fulfillment.',
    'If significant channeling of resources is detected, the effective extractiveness for other forms of religious engagement would be higher, potentially shifting the classification towards a ''tangled_rope'' for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_extraction, empirical, 'Potential for subtle extraction through control over the definition of religious fulfillment.').

omega_variable(
    status_of_sibling_readings,
    'Are the ''performance_only_reading'' and ''messianic_suspension_reading'' truly ''excluded'' or do they represent live, albeit minority, halakhic positions that challenge the universality of the ''study as exercise'' reading?',
    'Analysis of contemporary halakhic literature, communal practice, and the demographics of adherence to these alternative readings. Examination of whether these readings are actively suppressed or merely less prevalent.',
    'If these readings are found to be more robust and actively contested than currently assessed, the ''resistance'' metric for this constraint might be higher, and the ''excluded'' status of their advocates would be re-evaluated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_of_sibling_readings, empirical, 'The actual status and influence of alternative interpretations of the sacrifice obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 250, 0.05).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(sacr_tr_t750, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 750, 0.05).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t250, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 250, 0.05).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(sacr_be_t750, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 750, 0.05).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sacr_su_t250, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 250, 0.1).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(sacr_su_t750, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 750, 0.1).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'sacrifice_obligation_kernel'. Each reading offers a distinct interpretation of the divine commandment, with differing implications for fulfillment, beneficiaries, and victims. This reading focuses on study as a legitimate form of exercise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
