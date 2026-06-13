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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study of Sacrifice Law as Mitzvah Fulfillment (Study-as-Exercise Reading)
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'study-as-exercise' reading of the
 *   sacrifice obligation kernel within Jewish law. It posits that the
 *   intellectual engagement with the laws of sacrifice, particularly in the
 *   absence of the Temple, constitutes a legitimate and fulfilling exercise
 *   of the mitzvah (divine commandment). This reading emerged as a dominant
 *   interpretive framework after the destruction of the Second Temple,
 *   providing a means for the Jewish people to continue observing the
 *   commandment in a transformed, non-physical manner. It is a foundational
 *   tenet of much of rabbinic Judaism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.1).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study of Sacrifice Law as Mitzvah Fulfillment (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority/commitment_system").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__study_as_exercise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, 'c1f938a0-c69c-43d7-b2e9-6818bf1621fc').
narrative_ontology:cs_kernel_codification('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', formalized).
narrative_ontology:cs_authority_grounding('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', lineage).
narrative_ontology:cs_interpretation_layer_present('c1f938a0-c69c-43d7-b2e9-6818bf1621fc').
narrative_ontology:cs_reading_relation('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', foundational, study_is_equivalent_to_action).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_action, holdable).
narrative_ontology:cs_axiom_grounding('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', study_is_equivalent_to_action, deontological).
narrative_ontology:cs_axiom('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', secondary, divine_will_accommodates_historical_circumstance).
narrative_ontology:cs_axiom_status(divine_will_accommodates_historical_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', divine_will_accommodates_historical_circumstance, theological).
narrative_ontology:cs_reference_frame('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', rabbinic_post_temple_halakha).
narrative_ontology:cs_drift_state('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c1f938a0-c69c-43d7-b2e9-6818bf1621fc', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits Halakha, including the laws of sacrifice. This reading legitimizes their intellectual work as direct religious fulfillment and reinforces their role as arbiters of religious practice in the post-Temple era.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Engage in intensive study of sacred texts, including the laws of sacrifice. This reading provides them with a direct path to religious fulfillment through their primary activity, integrating their intellectual and spiritual lives.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_students, beneficiary,
    moderate, biographical, identity_locked, local).

% Devote their lives to the analysis and development of Jewish law. This reading validates their scholarly pursuits as a central religious act, ensuring the continuity and relevance of their field.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_scholars, beneficiary,
    organized, generational, identity_locked, global).

% Are taught that studying the laws of sacrifice is a meritorious act, even if they do not engage in deep scholarly analysis. This provides a accessible means for them to connect with the mitzvah.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, lay_adherents, beneficiary,
    powerless, biographical, constrained, local).

% Believe that physical sacrifices will only resume with the coming of the Messiah and the rebuilding of the Temple. While they may engage in study, they do not view it as a substitute for actual performance, and would object to the 'study-as-exercise' reading as diminishing the future physical mitzvah.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_restorationists, excluded,
    moderate, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and accessible path for Jewish people to fulfill the divine commandment of sacrifice in the absence of the Temple, maintaining religious continuity and identity through intellectual engagement.
% TRANSFER_FUNCTION: Transforms the 'performance' of a physical ritual into the 'engagement' of intellectual study, transferring the locus of fulfillment from the physical realm to the cognitive/spiritual realm.
% ABSENT_VOICES: Adherents of the 'performance-only' reading, who believe only physical sacrifice fulfills the mitzvah, are largely marginalized in mainstream discourse. They would argue that study is preparatory but not equivalent to the actual commandment, and that this reading dilutes the true meaning of the mitzvah.
% DISAPPEARANCE_RATIONALE: If this reading vanished, a core mechanism for religious continuity and identity in post-Temple Judaism would be lost. The religious lives of millions would be profoundly altered, and the role of rabbinic scholarship would be fundamentally undermined, leading to a significant reorganization of religious practice and meaning.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the physical performance of the sacrifice mitzvah impossible, creating a profound crisis of religious observance and continuity for the Jewish people.
% FOUNDING_PROBLEM_CORROBORATION: The problem of physical sacrifice being impossible remains live, as the Temple has not been rebuilt. Rabbinic literature across centuries, historical accounts of Jewish life post-Temple, and contemporary theological discourse all corroborate the ongoing nature of this challenge and the interpretive solution offered by this reading.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

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
 *   The constraint is classified as a Mountain because, within this reading, the equivalence of study and performance is treated as a divinely sanctioned, unchangeable truth under current conditions. Its extractiveness is negligible (0.05) because it primarily offers a path to fulfillment rather than imposing a burden. Suppression is low (0.1) as it is an interpretive opening, not a coercive closure. Theater ratio is 0.0 as the study is considered genuine fulfillment, not a performance. Accessibility collapse is high (0.9) because, for adherents of this reading, physical sacrifice is not an accessible alternative, making study the only viable path to fulfillment. Resistance is low (0.05) as this reading is widely accepted within mainstream rabbinic Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority and halakhic scholars, this constraint is a profound theological truth and a source of continuity. From the perspective of those who might adhere to a 'performance-only' reading, it would be seen as a deviation or a temporary measure, not a genuine fulfillment. However, within the 'study-as-exercise' framework itself, there is no significant perspectival gap among its adherents; it is a shared understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority, yeshiva students, and halakhic scholars are beneficiaries (d near 0.0) as this reading legitimizes their primary mode of religious engagement and reinforces their interpretive authority. There are no victims (d near 1.0) because the constraint offers a path to fulfillment, rather than extracting from anyone. The 'suspension' of physical sacrifice is understood as a divinely authorized transformation, not an extraction from those who cannot perform it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves a potential mandatrophy by transforming the mandate itself. The original mandate (physical sacrifice) became impossible. Rather than allowing the mandate to atrophy, this reading redefines its fulfillment, ensuring its continued 'live' status through intellectual engagement. This prevents the constraint from becoming a Piton (theatrical maintenance of an atrophied function) or a Snare (coercive enforcement of an impossible demand). The transformation is presented as a legitimate, divinely sanctioned adaptation, not a cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine transformation of the sacrifice obligation, or a temporary substitute for physical performance?',
    'Theological consensus on the nature of mitzvah fulfillment in the absence of the Temple; analysis of halakhic responsa regarding the equivalence of study and action.',
    'If a genuine transformation, the constraint is a Mountain (study is the mitzvah). If a temporary substitute, it is a Scaffold (transitional support until performance is possible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''study_as_exercise_reading'' of the ''sacrifice_obligation_kernel''. Sibling readings (performance_only_reading, messianic_suspension_reading, symbolic_archive_reading) would alter the classification by changing the nature of the obligation itself.').

omega_variable(
    natural_law_vs_constructed_transformation,
    'Is the equivalence of study and performance a natural theological truth, or a rabbinically constructed adaptation to historical circumstances?',
    'Analysis of foundational texts and their reception history; examination of the historical context of the Temple''s destruction and subsequent rabbinic rulings.',
    'If natural law, it reinforces the Mountain classification. If constructed, it introduces an element of human agency and potential for extraction, pushing towards a Tangled Rope for rabbinic authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_transformation, conceptual, 'Ambiguity between inherent theological truth and rabbinic interpretive construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_kernel'. Its ε value is low because study is considered genuine fulfillment. Sibling readings (performance_only_reading, messianic_suspension_reading, symbolic_archive_reading) have different ε values and classifications based on their interpretation of the obligation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
