% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Divine Command Performance
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'study_as_exercise' reading of the
 *   'temple_sacrifice_commitment' kernel. It posits that the intellectual
 *   engagement with the laws of Temple sacrifice is itself a direct and
 *   intrinsically valuable fulfillment of divine command, occupying the
 *   commitment in the absence of material conditions for performance. This
 *   reading emphasizes the spiritual and communal value of study as a primary
 *   mode of worship and covenant fidelity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study of Sacrifice Law as Divine Command Performance").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '96c4a9cc-9f63-4c13-b260-9f12443584d8').
narrative_ontology:cs_kernel_codification('96c4a9cc-9f63-4c13-b260-9f12443584d8', fixed_text).
narrative_ontology:cs_authority_grounding('96c4a9cc-9f63-4c13-b260-9f12443584d8', lineage).
narrative_ontology:cs_interpretation_layer_present('96c4a9cc-9f63-4c13-b260-9f12443584d8').
narrative_ontology:cs_reading_relation('96c4a9cc-9f63-4c13-b260-9f12443584d8', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('96c4a9cc-9f63-4c13-b260-9f12443584d8', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('96c4a9cc-9f63-4c13-b260-9f12443584d8', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('96c4a9cc-9f63-4c13-b260-9f12443584d8', foundational, study_is_divine_performance).
narrative_ontology:cs_axiom_status(study_is_divine_performance, holdable).
narrative_ontology:cs_axiom_grounding('96c4a9cc-9f63-4c13-b260-9f12443584d8', study_is_divine_performance, theological).
narrative_ontology:cs_axiom('96c4a9cc-9f63-4c13-b260-9f12443584d8', secondary, divine_command_transcends_materiality).
narrative_ontology:cs_axiom_status(divine_command_transcends_materiality, holdable).
narrative_ontology:cs_axiom_grounding('96c4a9cc-9f63-4c13-b260-9f12443584d8', divine_command_transcends_materiality, theological).
narrative_ontology:cs_reference_frame('96c4a9cc-9f63-4c13-b260-9f12443584d8', post_temple_rabbinic_tradition).
narrative_ontology:cs_drift_state('96c4a9cc-9f63-4c13-b260-9f12443584d8', contemporary_halakhic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('96c4a9cc-9f63-4c13-b260-9f12443584d8', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__study_as_exercise, non_studying_members).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, intellectual_engagement_as_worship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective of individuals who engage in dedicated study of sacrifice laws, viewing this intellectual engagement as a direct and intrinsically valuable fulfillment of divine command and a primary mode of maintaining covenant fidelity in the absence of the Temple.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, studying_community, agenda_setter).

% The ultimate source of the divine imperative, whose will is interpreted and fulfilled through the act of study according to this reading. It is not an agent in the human sense but the object of fidelity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, divine_command, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, divine_command).

% Members of the broader community who do not actively engage in the intensive study of sacrifice laws. While not directly 'victims' in an extractive sense, they may bear a social or spiritual cost of non-participation, feeling a distance from the community's primary mode of commitment or missing out on the intrinsic benefits of deep engagement.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, non_studying_members, payer,
    moderate, biographical, constrained, local).

% Those who adhere to the 'performance_only' reading of the kernel, believing that divine command regarding sacrifice can only be fulfilled through material instantiation. From their perspective, study is archival, not active performance, and they are excluded from this reading's definition of covenant fulfillment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, performance_only_adherents, excluded,
    powerful, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective spiritual and intellectual life of the community, providing a shared framework for maintaining covenant fidelity and identity through study in the absence of the Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit, communal identity, and intellectual engagement from individual members to the collective, reinforcing the covenant and fulfilling divine command.
% ABSENT_VOICES: Adherents of the 'performance_only' reading are structurally excluded from this reading's definition of active commitment; they would argue that study is insufficient for divine command fulfillment.
% DISAPPEARANCE_RATIONALE: If the understanding of study as performance vanished, the community's primary mode of covenant fidelity and spiritual occupation would collapse, leading to a profound crisis of identity and purpose, and a re-evaluation of how divine command is fulfilled.
% FOUNDING_PROBLEM: How to maintain active covenant fidelity and fulfill divine command regarding Temple sacrifices after the destruction of the Temple, when material performance became impossible.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts and commentaries from the Talmudic era onwards attest to the shift in emphasis from material sacrifice to study and prayer as primary modes of worship, corroborated by centuries of continuous scholarly tradition within the community, which views this as a direct continuation of covenantal obligation.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.0, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because study is viewed as an intrinsic good and a direct fulfillment, not a means of extraction. Theater ratio is zero as the activity is genuinely functional for this reading. Suppression is low but rises slightly over time (0.05 to 0.1) reflecting increasing social and communal pressure to participate in study as a core identity marker. Accessibility collapse is high (0.9) because for this reading, the alternative of *not* studying is seen as a collapse of commitment to the divine command.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'studying_community', this constraint is a pure coordination mechanism for spiritual and communal life. From the 'performance_only_adherents' perspective, this reading fundamentally misinterprets the nature of divine command, seeing it as a pragmatic adaptation rather than a true fulfillment.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'studying_community' is the primary beneficiary, receiving the intrinsic value of covenant fidelity and spiritual fulfillment. There are no direct 'victims' in an extractive sense, as the activity is self-fulfilling. 'Non_studying_members' may bear a diffuse cost of non-participation, but this is not a direct extraction. 'Divine_command' is an analytical observer, the source of the commitment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_as_fulfillment_vs_adaptation,
    'Is this reading a genuine, authorized fulfillment of divine command, or a pragmatic adaptation to the material impossibility of Temple sacrifice?',
    'Theological consensus across diverse rabbinic authorities and historical analysis of interpretive shifts. If a significant portion of the tradition views it as a ''next best'' rather than ''equivalent'' fulfillment, the reading''s intrinsic value claim is weakened.',
    'If primarily an adaptation, the ''intrinsic value'' claim for study might be re-evaluated, potentially introducing a low level of extractiveness (e.g., from those who feel compelled to study but do not find it fully fulfilling) or increasing the ''theater_ratio'' if it''s seen as a substitute performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_fulfillment_vs_adaptation, conceptual, 'Ambiguity regarding the ontological status of study as fulfillment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (social pressure to study) structural (community expectation, social exclusion) or internalized (personal spiritual drive, identity fusion)?',
    'Post-exit suppression trajectory: if individuals who leave the studying community continue to feel internal pressure or guilt, it suggests a significant internalized component. Ethnographic studies of community norms and individual motivations.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the individual carries the pressure with them. This would reinforce the ''identity_locked'' exit option for the studying community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for communal study.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.0).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.0).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2000, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 500, 0.07).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1000, 0.08).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1500, 0.09).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'temple_sacrifice_commitment' kernel, each representing a distinct interpretation of how divine command regarding sacrifice is fulfilled in the absence of the Temple.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
