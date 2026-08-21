% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Sacrifice Obligation Continuity: Study as Performance
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the reading within a religious tradition that
 *   the study of sacrifice law is itself a fulfillment of the commandment to
 *   offer sacrifices, thereby ensuring the obligation's continuity through
 *   textual engagement. This reading emerged historically in response to the
 *   inability to perform physical sacrifices. It is a 'rope' because it
 *   provides a coordination function (how to fulfill a commandment) with low
 *   extraction, benefiting those who engage in study. This is one reading of
 *   the 'sacrifice_obligation_continuity' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Sacrifice Obligation Continuity: Study as Performance").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '6149a172-5029-4e87-81a0-36a6ef0aa1dd').
narrative_ontology:cs_kernel_codification('6149a172-5029-4e87-81a0-36a6ef0aa1dd', fixed_text).
narrative_ontology:cs_authority_grounding('6149a172-5029-4e87-81a0-36a6ef0aa1dd', lineage).
narrative_ontology:cs_interpretation_layer_present('6149a172-5029-4e87-81a0-36a6ef0aa1dd').
narrative_ontology:cs_reading_relation('6149a172-5029-4e87-81a0-36a6ef0aa1dd', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('6149a172-5029-4e87-81a0-36a6ef0aa1dd', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('6149a172-5029-4e87-81a0-36a6ef0aa1dd', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('6149a172-5029-4e87-81a0-36a6ef0aa1dd', foundational, textual_engagement_is_ritual_fulfillment).
narrative_ontology:cs_axiom_status(textual_engagement_is_ritual_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('6149a172-5029-4e87-81a0-36a6ef0aa1dd', textual_engagement_is_ritual_fulfillment, deontological).
narrative_ontology:cs_axiom('6149a172-5029-4e87-81a0-36a6ef0aa1dd', secondary, obligation_persists_through_intellectual_means).
narrative_ontology:cs_axiom_status(obligation_persists_through_intellectual_means, holdable).
narrative_ontology:cs_axiom_grounding('6149a172-5029-4e87-81a0-36a6ef0aa1dd', obligation_persists_through_intellectual_means, conventional).
narrative_ontology:cs_reference_frame('6149a172-5029-4e87-81a0-36a6ef0aa1dd', post_temple_rabbinic_tradition).
narrative_ontology:cs_drift_state('6149a172-5029-4e87-81a0-36a6ef0aa1dd', contemporary_global_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6149a172-5029-4e87-81a0-36a6ef0aa1dd', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, religious_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, devout_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their ongoing textual engagement and interpretation are recognized as a primary mode of fulfilling the commandment, validating their professional and spiritual practice. They benefit from the accessibility of this form of observance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, religious_scholars, beneficiary,
    institutional, generational, constrained, global).

% Find spiritual fulfillment and a path to observe ancient commandments through accessible study, rather than being limited to physical rituals that are currently impossible. This reading provides a continuous, actionable religious obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, devout_adherents, beneficiary,
    moderate, biographical, mobile, local).

% The community, through its interpretive traditions and educational institutions, upholds and propagates the understanding that study is a valid form of performance, thereby maintaining the continuity of religious practice and identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, religious_community, agenda_setter,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally accessible and intellectually rigorous means for adherents to fulfill a central religious commandment (sacrifice) in the absence of a physical temple, ensuring continuity of religious practice and identity.
% TRANSFER_FUNCTION: Transfers the locus of religious obligation from physical ritual to intellectual and spiritual engagement, making fulfillment accessible to all who can study, rather than only those with access to a specific site or ritual.
% ABSENT_VOICES: Those who adhere strictly to the 'performance_only' reading might argue that study, while meritorious, cannot substitute for physical ritual. Their voices are present in ongoing theological debate but do not alter the widespread acceptance of study as fulfillment.
% DISAPPEARANCE_RATIONALE: If the understanding that 'study is performance' vanished, a significant portion of religious life and scholarly endeavor would lose its primary justification as commandment fulfillment. Adherents would face a profound crisis of how to observe a central obligation, leading to a major reorganization of religious practice and identity.
% FOUNDING_PROBLEM: The destruction of the central temple rendered physical sacrifice impossible, creating a crisis of how to fulfill a core religious commandment and maintain continuity of religious life.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and centuries of rabbinic commentary attest to the historical problem and the development of study as a response. The problem remains live as long as physical sacrifice is impossible, as corroborated by ongoing theological discourse and the continued centrality of study in religious life.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because study is widely accessible and imposes minimal material cost. Suppression is low (0.05) as this reading is generally accepted and requires little coercion to maintain. Theater ratio is low (0.02) because the act of study is genuinely considered a meaningful spiritual act, not a mere performance. Accessibility collapse is low (0.1) as study is a highly accessible alternative. Resistance is low (0.05) because this reading is widely accepted and provides a viable path for observance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious scholars and devout adherents, this constraint is a pure rope, providing a vital pathway for spiritual fulfillment. From the perspective of those holding the 'performance_only' reading, it might be seen as a conceptual compromise, but not necessarily extractive, as study is still valued, just not as fulfillment.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and devout adherents are clear beneficiaries, as the constraint provides them with a means of fulfilling a core commandment. The religious community acts as an agenda-setter by upholding and transmitting this interpretive tradition. There are no direct victims, as the obligation is considered fulfilled, not evaded or extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a vital adaptation as either pure extraction or a degraded institution. The 'study as performance' reading is a robust solution to a persistent problem, not a theatrical maintenance of a dead mandate. Its mandate (fulfillment of sacrifice obligation) is very much alive, and the mechanism (study) is functional and widely adopted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''study_as_performance'' reading of the ''sacrifice_obligation_continuity'' kernel?',
    'Analysis of primary religious texts and authoritative commentaries to confirm the specific interpretive framework and its distinguishing features.',
    'If misidentified, the structural relationships to sibling readings (forecloses, coexists_with, influences) would be incorrect, leading to an inaccurate mapping of the commitment system''s internal dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated from the kernel.').

omega_variable(
    performance_only_vs_study_as_performance,
    'Does the ''performance_only'' reading logically foreclose ''study_as_performance'', or do they merely coexist as competing interpretations?',
    'Deep textual analysis of the foundational axioms of both readings to determine if their core premises are mutually exclusive within a single coherent theological framework.',
    'If ''performance_only'' forecloses ''study_as_performance'', then holding both within the same framework is a contradiction, indicating a deeper conceptual tension. If they merely coexist, the tension is one of preference or emphasis, not logical impossibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_vs_study_as_performance, conceptual, 'Examines the logical compatibility of this reading with the ''performance_only'' sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.02).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 25, 0.02).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 50, 0.02).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 75, 0.02).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 100, 0.02).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sacr_be_t25, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 25, 0.14).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(sacr_be_t75, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 75, 0.16).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(sacr_su_t25, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(sacr_su_t50, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(sacr_su_t75, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
