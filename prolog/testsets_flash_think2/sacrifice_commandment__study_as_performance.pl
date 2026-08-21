% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of Commandment
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This constraint describes the religious interpretation that the
 *   intellectual study of sacrifice laws is itself a fulfillment of the
 *   divine commandment, particularly relevant in the absence of the Temple.
 *   It is a core practice for maintaining religious identity and continuity.
 *   This story instantiates the 'study_as_performance' reading of the
 *   'sacrifice_commandment' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.1).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Fulfillment of Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious_studies/halakhic_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'd7980f75-7bc3-454f-93a2-70f6a60e490e').
narrative_ontology:cs_kernel_codification('d7980f75-7bc3-454f-93a2-70f6a60e490e', fixed_text).
narrative_ontology:cs_authority_grounding('d7980f75-7bc3-454f-93a2-70f6a60e490e', lineage).
narrative_ontology:cs_interpretation_layer_present('d7980f75-7bc3-454f-93a2-70f6a60e490e').
narrative_ontology:cs_reading_relation('d7980f75-7bc3-454f-93a2-70f6a60e490e', sacrifice_commandment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('d7980f75-7bc3-454f-93a2-70f6a60e490e', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('d7980f75-7bc3-454f-93a2-70f6a60e490e', foundational, intellectual_engagement_is_worship).
narrative_ontology:cs_axiom_status(intellectual_engagement_is_worship, holdable).
narrative_ontology:cs_axiom_grounding('d7980f75-7bc3-454f-93a2-70f6a60e490e', intellectual_engagement_is_worship, deontological).
narrative_ontology:cs_reference_frame('d7980f75-7bc3-454f-93a2-70f6a60e490e', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('d7980f75-7bc3-454f-93a2-70f6a60e490e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d7980f75-7bc3-454f-93a2-70f6a60e490e', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, lay_adherents).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals dedicate themselves to the intellectual study of sacrifice laws, believing this act itself constitutes a form of worship and fulfillment of the divine commandment. They derive spiritual merit and communal standing from this practice.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, agenda_setter,
    organized, generational, identity_locked, global).

% While not actively engaged in deep scholarly study, they benefit spiritually from the continued practice and interpretation of the commandment by scholars, which maintains the vitality and relevance of their religious tradition. They are guided by the interpretations of religious authorities.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, lay_adherents, beneficiary,
    moderate, biographical, constrained, local).

% These leaders and institutions interpret, transmit, and uphold the tradition that intellectual study fulfills the sacrifice commandment. They provide the framework and legitimacy for this practice, ensuring its continuity and guiding the community.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, religious_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% These scholars study the historical, sociological, and theological aspects of this interpretation without necessarily adhering to its religious tenets. They analyze its function within the religious tradition and its impact on community identity.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, secular_academics, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intellectual and spiritual efforts of a religious community to maintain a central divine commandment and group identity in the absence of the physical means to perform sacrifices.
% TRANSFER_FUNCTION: Transfers spiritual merit, communal identity, and continuity of religious practice from the divine source to scholar-worshippers and lay adherents through intellectual engagement with sacred texts.
% ABSENT_VOICES: Those who insist that the sacrifice commandment requires only physical execution and is therefore suspended without the Temple, or those who view study purely as archival maintenance for a future messianic era. They are excluded by the dominant interpretive framework.
% DISAPPEARANCE_RATIONALE: If the belief that study fulfills the sacrifice commandment vanished, the religious community would face a profound crisis of identity and practice, losing a central mode of divine connection and communal cohesion. The entire structure of post-Temple religious life would need to be re-evaluated.
% FOUNDING_PROBLEM: The destruction of the Temple and the subsequent inability to perform physical sacrifices, leaving a central divine commandment seemingly unfulfilled and threatening the continuity of religious practice.
% FOUNDING_PROBLEM_CORROBORATION: Centuries of rabbinic commentary, foundational religious texts (Talmud), and the continuous lived practice of Jewish communities globally attest to this problem and its resolution through study. This is corroborated by historical and theological analyses from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the practice is considered intrinsically valuable worship, not a means of extracting resources from participants. Suppression is low (0.1) as it's an internal, voluntary intellectual and spiritual practice, not enforced by external coercion. Theater ratio is low (0.05) because the engagement is genuine and functional for its stated purpose of divine fulfillment and communal identity. Accessibility collapse is high (0.8) because for adherents, there are few alternatives to this intellectual engagement for fulfilling the commandment in the absence of the Temple. Resistance is low (0.05) as this interpretation is widely accepted within the tradition.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the sacrifice commandment, such as 'performance_only' (requiring physical execution) or 'archive_maintenance' (study purely for future restoration), would experience this constraint differently. The 'performance_only' reading would see this as an invalid substitute, while the 'archive_maintenance' reading might view it as a secondary, rather than primary, fulfillment. This story focuses solely on the 'study_as_performance' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholar-worshippers and religious authorities are beneficiaries and agenda-setters, as they actively engage in and define the practice, deriving spiritual and communal benefits. Lay adherents are beneficiaries, receiving spiritual guidance and continuity. There are no identifiable victims, as the practice is seen as purely beneficial and voluntary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine fulfillment of the sacrifice commandment, or merely a substitute for physical performance?',
    'Theological and halakhic consensus over centuries, or a future messianic restoration of the Temple that would re-enable physical sacrifices and test the continued validity of study as fulfillment.',
    'If deemed a mere substitute, the constraint''s spiritual efficacy would be diminished, potentially leading to a re-evaluation of its role in religious life. If confirmed as genuine fulfillment, its status as a core practice is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the nature of fulfillment (spiritual vs. physical) for the sacrifice commandment.').

omega_variable(
    fulfillment_scope_ambiguity,
    'Does intellectual study fully encompass the spiritual and communal dimensions of physical sacrifice, or does it fulfill only a subset of the commandment''s original intent?',
    'Comparative theological analysis of the spiritual impact of study versus historical accounts of physical sacrifice, or the emergence of new interpretive traditions.',
    'If study is found to fulfill only a subset, it might lead to a search for additional practices to bridge the perceived gap, potentially altering the constraint''s role. If it fully encompasses, its current status is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_scope_ambiguity, conceptual, 'The extent to which study fulfills the full scope of the sacrifice commandment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_commandment__study_as_performance, theater_ratio, 25, 0.05).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_commandment__study_as_performance, theater_ratio, 50, 0.05).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_commandment__study_as_performance, theater_ratio, 75, 0.05).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__study_as_performance, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t25, sacrifice_commandment__study_as_performance, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(sacr_be_t50, sacrifice_commandment__study_as_performance, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(sacr_be_t75, sacrifice_commandment__study_as_performance, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__study_as_performance, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__study_as_performance, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sacr_su_t25, sacrifice_commandment__study_as_performance, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(sacr_su_t50, sacrifice_commandment__study_as_performance, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(sacr_su_t75, sacrifice_commandment__study_as_performance, suppression_requirement, 75, 0.1).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__study_as_performance, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'sacrifice_commandment' kernel, each representing a different structural interpretation of how the commandment is fulfilled in the absence of the Temple. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
