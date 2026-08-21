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
 *   This reading is a foundational element of post-Temple Judaism, providing
 *   a mechanism for continuity and meaning. It is classified as a Mountain
 *   due to its deep integration into the halakhic framework and its perceived
 *   naturalness as an adaptation to historical circumstances. Extraction is
 *   zero because study is presented as the legitimate form of the mitzvah,
 *   not a substitute that extracts from an ideal. Rabbinic authority benefits
 *   by solidifying its interpretive role.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.0).
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
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0').
narrative_ontology:cs_kernel_codification('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', fixed_text).
narrative_ontology:cs_authority_grounding('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', lineage).
narrative_ontology:cs_interpretation_layer_present('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0').
narrative_ontology:cs_reading_relation('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', foundational, intellectual_engagement_as_fulfillment).
narrative_ontology:cs_axiom_status(intellectual_engagement_as_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', intellectual_engagement_as_fulfillment, deontological).
narrative_ontology:cs_reference_frame('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', post_temple_halakhic_adaptation).
narrative_ontology:cs_drift_state('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('09e5bf4f-6c65-46c1-9e5e-8bbc0a437ca0', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, individual_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and adjudicates the halakha, including the nature of mitzvah fulfillment. This reading solidifies their interpretive monopoly on what constitutes legitimate religious practice in the absence of the Temple. Their authority is grounded in the continuity of tradition and scholarship.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives a clear, accessible, and intellectually engaging path to fulfill a central religious obligation that is otherwise impossible to perform. This reading provides continuity and meaning, preventing a sense of religious void or unfulfilled duty. Their identity is deeply intertwined with halakhic observance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_community, beneficiary,
    organized, generational, identity_locked, global).

% Are empowered to engage with the mitzvah of sacrifices through study, transforming an otherwise inaccessible commandment into a vibrant intellectual and spiritual pursuit. This provides a sense of active participation and continuity with tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, individual_practitioners, beneficiary,
    moderate, biographical, identity_locked, local).

% Believe that only physical performance can fulfill the sacrifice mitzvah, viewing study as preparatory but not substitutive. They are excluded from the dominant interpretive framework that validates study as fulfillment, but their views persist within certain segments of the community.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, performance_only_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally accessible and intellectually rigorous means for the Jewish community to fulfill the mitzvah of sacrifices in the absence of the Temple, ensuring continuity of religious practice and identity.
% TRANSFER_FUNCTION: Transforms the obligation of physical sacrifice into an intellectual and spiritual engagement, transferring the locus of fulfillment from a physical act to scholarly study, thereby transferring authority over fulfillment from Temple priests to rabbinic scholars.
% ABSENT_VOICES: Advocates for a 'performance-only' reading of the mitzvah are structurally excluded from the interpretive consensus that validates study as fulfillment. They would argue that this reading dilutes the true nature of the commandment, but their position is marginalized within mainstream halakhic discourse.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Jewish community would face a profound crisis of religious identity and practice, as a central mitzvah would become unfulfillable. This would necessitate a radical re-evaluation of halakha and communal purpose, leading to significant rearrangement of religious life.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the physical performance of sacrifices impossible, creating a profound void in Jewish religious life and raising questions about the continued relevance and fulfillment of a core divine commandment.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts and ongoing theological discourse universally attest to the problem of Temple destruction and the subsequent need to adapt halakhic practice. The problem remains live as long as the Temple is not rebuilt, and the community continues to seek meaningful ways to engage with the mitzvah.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The constraint's extractiveness is 0.0 because, from this reading's perspective, study is not a 'lesser' or 'extracted' form of the mitzvah, but its full and legitimate expression under current conditions. There are no victims, as the transformation of the obligation is seen as divinely sanctioned and beneficial for the community. Suppression is 0.0 because adherence is voluntary and deeply integrated into religious identity, not coerced. Theater ratio is 0.0 as the study is considered genuinely functional. Accessibility collapse is high (0.95) because, for those who accept this reading, there is no viable alternative to study for fulfilling the mitzvah in the absence of the Temple. Resistance is low (0.05) because this reading is widely accepted within mainstream Judaism, though some fringe groups may advocate for literal performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority and the Jewish community, this constraint is a natural and essential adaptation, a Mountain that enables continued religious life. From the perspective of 'performance-only' advocates (an excluded voice), it might be seen as a form of 'theatrical' or 'symbolic' fulfillment, but this reading's framework does not acknowledge that as a valid perspective on its own operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority is a beneficiary as this reading reinforces their role as interpreters and adjudicators of halakha, providing a framework for continued religious observance. The Jewish community and individual practitioners are also beneficiaries, as they gain a meaningful and accessible path to fulfill a central commandment, fostering continuity and identity. There are no victims, as the transformation of the mitzvah is seen as a necessary and positive adaptation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    halakhic_status_of_study,
    'Is the intellectual engagement with sacrifice law truly equivalent to physical performance in halakhic terms, or is it a temporary substitute?',
    'Theological consensus development over centuries, or a definitive halakhic ruling by a universally recognized authority (e.g., a Sanhedrin) if such were to be re-established.',
    'If deemed a temporary substitute, the extractiveness might be re-evaluated as non-zero (a ''lesser'' fulfillment), and the claimed type might shift towards a Scaffold or Tangled Rope, implying an eventual return to physical performance. If confirmed as full equivalence, the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(halakhic_status_of_study, conceptual, 'Ambiguity regarding the equivalence of study to physical performance for mitzvah fulfillment.').

omega_variable(
    interpretive_monopoly_legitimacy,
    'Does the rabbinic authority''s interpretive monopoly on this reading genuinely serve the community''s spiritual needs, or does it primarily consolidate power?',
    'Sociological study of community engagement and satisfaction with this mode of fulfillment, combined with historical analysis of power dynamics within rabbinic institutions.',
    'If primarily power consolidation, the ''beneficiary'' role of rabbinic authority would be re-evaluated as more extractive, potentially shifting the constraint towards a Tangled Rope or Snare, as the coordination function would be seen as cover for institutional gain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_monopoly_legitimacy, empirical, 'Whether rabbinic authority''s benefit from this reading is genuine coordination or institutional extraction.').


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
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(sacr_su_t650, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 650, 0.0).
narrative_ontology:measurement(sacr_su_t1300, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1300, 0.0).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1950, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
