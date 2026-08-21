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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Temple Sacrifice Commitment: Study as Exercise of Divine Command
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'study_as_exercise' reading of the
 *   'temple_sacrifice_commitment' kernel. In this reading, the intellectual
 *   engagement with the laws of temple sacrifice is understood as a direct
 *   and intrinsically valuable performance of the divine command, occupying
 *   the commitment in the absence of material conditions for actual
 *   sacrifice. It is a coordination mechanism for a community to maintain its
 *   covenant fidelity and identity through intellectual and spiritual means.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.1).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Temple Sacrifice Commitment: Study as Exercise of Divine Command").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, 'c59a42e6-ba4a-40b8-b9ae-9a31220f1b67').
narrative_ontology:cs_kernel_codification('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', fixed_text).
narrative_ontology:cs_authority_grounding('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', lineage).
narrative_ontology:cs_interpretation_layer_present('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67').
narrative_ontology:cs_reading_relation('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', foundational, study_is_performance_of_divine_command).
narrative_ontology:cs_axiom_status(study_is_performance_of_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', study_is_performance_of_divine_command, theological).
narrative_ontology:cs_reference_frame('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', post_temple_destruction_fidelity).
narrative_ontology:cs_drift_state('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', contemporary_halakhic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c59a42e6-ba4a-40b8-b9ae-9a31220f1b67', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, intellectual_engagement_as_worship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community that engages in the intellectual study of sacrifice law. They derive spiritual fulfillment and maintain their covenant fidelity through this practice, seeing it as a direct fulfillment of divine command in the absence of material conditions for sacrifice. Their identity is deeply intertwined with this commitment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, studying_community, agenda_setter).

% Scholars of religious law and commitment systems who analyze the structural function and theological claims of this reading. They are external to the commitment but can assess its internal coherence and social effects.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing fidelity of the community to the divine command regarding temple sacrifices, by providing a legitimate and intrinsically valuable mode of engagement (study) in the absence of material conditions for performance.
% TRANSFER_FUNCTION: Transfers spiritual merit, covenant fidelity, and intellectual effort from individuals to the collective tradition, ensuring the continuity of the divine command's relevance and the community's identity.
% ABSENT_VOICES: Adherents of the 'performance_only' reading, who would argue that study, while valuable, cannot substitute for material sacrifice and therefore does not fully occupy the divine command. They are absent from the internal logic of this reading, which asserts study *is* performance.
% DISAPPEARANCE_RATIONALE: If the understanding that 'study is performance' vanished, the studying community would face a profound crisis of identity and purpose. Their primary mode of covenant fidelity would be invalidated, leading to a re-evaluation of their relationship to divine command and potentially fragmenting the community.
% FOUNDING_PROBLEM: To maintain the community's active commitment and fidelity to the divine command regarding temple sacrifices after the destruction of the Temple, which rendered material performance impossible.
% FOUNDING_PROBLEM_CORROBORATION: Theological texts (e.g., Talmudic discussions, medieval commentaries), historical accounts of rabbinic responses to Temple destruction, and the continuous practice of study within the community, corroborated by external scholars of religious history.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is zero because study is considered an intrinsically valuable act of devotion, not a burden or a means of extraction. Suppression is low as participation is voluntary, driven by internal commitment rather than external coercion. Theater ratio is low because the study is a genuine, functional engagement with the divine command, not a performative substitute. Accessibility collapse is moderate-high because for adherents of this reading, alternatives to study for maintaining this specific commitment are conceptually 'collapsed' in favor of this path. Resistance is very low as it is a deeply valued and chosen practice.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap within this reading, as all participants share the understanding that study is a direct fulfillment of the divine command. However, a significant gap exists between this reading and other readings of the same kernel, particularly the 'performance_only' reading, which would view this as a substitute rather than a direct fulfillment.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community is both the beneficiary and the agenda-setter, defining and performing the study. Their identity is locked into this practice, making exit difficult without a fundamental shift in self-conception. There are no victims, as the constraint is understood as a source of spiritual benefit and fidelity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_full_occupation_vs_substitute,
    'Is the intellectual study of sacrifice law considered a full and complete occupation of the divine command, or a necessary but lesser substitute for material performance?',
    'Analysis of authoritative theological texts and contemporary rabbinic responsa regarding the spiritual efficacy and equivalence of study versus material sacrifice.',
    'If it is a full occupation, the zero extractiveness holds. If it is a lesser substitute, a subtle, internalized extractiveness might be present, reflecting the ''cost'' of not being able to perform the ideal, potentially shifting the classification towards a very low-extraction Tangled Rope or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_full_occupation_vs_substitute, conceptual, 'Ambiguity regarding the completeness of study as a fulfillment of divine command.').

omega_variable(
    conceptual_boundary_with_performance_only,
    'How robust is the conceptual boundary between ''study as exercise'' and ''performance only'' readings, given the shared textual kernel?',
    'Detailed comparative analysis of the hermeneutic principles and theological justifications employed by adherents of each reading, identifying points of irreconcilable logical contradiction versus mere disagreement.',
    'If the contradiction is less absolute than currently understood, the ''forecloses'' relation might weaken to ''coexists_with'', implying a more fluid interpretive landscape and potentially different network effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_boundary_with_performance_only, conceptual, 'Clarity of the conceptual distinction between this reading and the ''performance_only'' sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.05).
narrative_ontology:measurement(temp_tr_t250, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 250, 0.05).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.05).
narrative_ontology:measurement(temp_tr_t750, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 750, 0.05).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(temp_be_t250, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 250, 0.0).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(temp_be_t750, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 750, 0.0).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1000, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(temp_su_t250, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 250, 0.1).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(temp_su_t750, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 750, 0.1).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'temple_sacrifice_commitment' kernel. Each reading represents a different structural interpretation of how the divine command is engaged in the absence of material conditions for sacrifice, leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
