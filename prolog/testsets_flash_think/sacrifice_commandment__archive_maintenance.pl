% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Study of Sacrifice Law for Future Temple Restoration
 *   domain: religious/halakhic_theory
 *
 * SUMMARY:
 *   This constraint describes the halakhic practice of studying the laws of
 *   Temple sacrifices as a means of preserving technical knowledge for the
 *   future restoration of the Temple. This reading emphasizes preparation for
 *   a messianic era rather than viewing the study as a substitute for or
 *   fulfillment of the commandment in the present. It is one reading of the
 *   broader 'sacrifice_commandment' kernel, distinguishing itself from
 *   interpretations that see study as performance or that declare the
 *   commandment entirely suspended.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.45).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.2).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Study of Sacrifice Law for Future Temple Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic_theory").

narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'deac4c2e-1c40-4e7c-bb79-9f03ee9ce414').
narrative_ontology:cs_kernel_codification('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', fixed_text).
narrative_ontology:cs_authority_grounding('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', lineage).
narrative_ontology:cs_interpretation_layer_present('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414').
narrative_ontology:cs_reading_relation('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', sacrifice_commandment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_axiom('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', foundational, halakha_is_practical_and_future_oriented).
narrative_ontology:cs_axiom_status(halakha_is_practical_and_future_oriented, holdable).
narrative_ontology:cs_axiom_grounding('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', halakha_is_practical_and_future_oriented, conventional).
narrative_ontology:cs_axiom('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', foundational, knowledge_preservation_is_commandment_fulfillment).
narrative_ontology:cs_axiom_status(knowledge_preservation_is_commandment_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', knowledge_preservation_is_commandment_fulfillment, theological).
narrative_ontology:cs_reference_frame('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', halakhic_continuity_in_exile).
narrative_ontology:cs_drift_state('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', contemporary_diaspora_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('deac4c2e-1c40-4e7c-bb79-9f03ee9ce414', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_generations_of_israel).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, contemporary_jewish_community).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, continuity_of_halakha).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, practical_messianic_preparation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars dedicate their lives to the intricate study of Temple service laws, preserving the technical knowledge required for future restoration. They benefit from intellectual engagement and maintaining the continuity of their tradition, but bear the cost of deferred gratification and the uncertainty of the Temple's return.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_scholars, agenda_setter,
    organized, biographical, identity_locked, global).

% These are the theoretical recipients of the preserved knowledge, who will be able to perform the Temple service correctly when the Temple is rebuilt. They benefit from the continuity of tradition and the practical readiness for messianic times, without bearing any current costs.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_generations_of_israel, beneficiary,
    powerless, generational, analytical, global).

% Members of the community support the institutions and individuals engaged in this study, often through donations or by dedicating their own time to learning. They bear the cost of resources and intellectual effort, but benefit from the sense of continuity, hope for redemption, and the maintenance of religious identity.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, contemporary_jewish_community, payer,
    moderate, biographical, constrained, global).

% These individuals or groups prioritize immediate action towards Temple rebuilding or other forms of messianic acceleration, viewing prolonged study as insufficient or a delay. They are excluded from the primary discourse of this constraint, which emphasizes preparation over immediate action, and would advocate for a different approach to the commandment.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messianic_activists, excluded,
    organized, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure the accurate and complete transmission of complex, highly technical ritual knowledge across generations, so that the Temple service can be correctly performed upon its future restoration.
% TRANSFER_FUNCTION: Transfers intellectual effort, time, and communal resources from current generations of scholars and community members to future generations, in the form of preserved and accessible halakhic knowledge.
% ABSENT_VOICES: Those who believe the commandment is entirely suspended without the Temple, or those who believe study itself constitutes fulfillment, are absent from this reading's core justification. Messianic activists who prioritize immediate physical action over preparatory study also represent an absent voice.
% DISAPPEARANCE_RATIONALE: If the practice of studying sacrifice laws for future restoration vanished, the intricate technical knowledge would be lost over time. This would render the correct performance of Temple service impossible upon restoration, fundamentally altering the religious future and the continuity of halakhic tradition.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the subsequent inability to perform divine sacrifices, creating a profound gap in the fulfillment of a central commandment and threatening the loss of its practical knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts (e.g., Talmudic discussions, Maimonides' Mishneh Torah), contemporary halakhic authorities, and the ongoing physical absence of the Temple itself corroborate that the founding problem remains live. This is attested by a broad consensus across traditional Jewish legal scholarship, not just by the direct beneficiaries of the study.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the significant investment of time and intellectual effort by scholars and the community, with the primary benefit deferred to a future generation. Suppression (0.20) is low, as this is a widely accepted and voluntary religious practice, not coercively enforced. Theater ratio (0.10) is low because the study is genuinely aimed at practical knowledge preservation, not mere symbolic performance. Accessibility collapse (0.30) is low as other forms of religious engagement exist, and resistance (0.15) is minimal within the communities that embrace this approach. The 'scaffold' classification is chosen because the constraint is explicitly transitional, supporting a future state (Temple restoration) that serves as its declared sunset.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic scholars and future generations, this constraint is a vital, constructive scaffold ensuring the continuity of divine service. From the perspective of messianic activists, it might be seen as an insufficient or even delaying mechanism, failing to address the urgency of redemption. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars act as agenda-setters, defining and perpetuating this form of study, and are beneficiaries of intellectual engagement and traditional continuity. Future generations are the primary beneficiaries, receiving the preserved knowledge. The contemporary Jewish community acts as a payer, investing resources and time. Messianic activists are excluded, as their focus on immediate action diverges from this reading's emphasis on preparation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a 'scaffold' prevents mislabeling it as a 'rope' (which implies steady-state coordination) or a 'piton' (which implies atrophied function). The 'has_sunset_clause: true' (Temple restoration) explicitly acknowledges its transitional nature and prevents it from being seen as a permanent, self-justifying arrangement, even with its deferred benefits. The founding problem is 'live', further supporting its active, transitional role.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    present_value_vs_future_utility,
    'What is the actual present-day spiritual or communal value derived from this study, independent of its future utility?',
    'Sociological studies of religious practice, theological analysis of the role of study in Jewish thought, and surveys of community members'' perceived benefits.',
    'If significant present value is found, the constraint''s ''scaffold'' nature might be less pronounced, leaning more towards a ''rope'' with dual present/future function. If present value is negligible, the ''scaffold'' classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_value_vs_future_utility, empirical, 'Ambiguity regarding the immediate vs. deferred benefits of the study.').

omega_variable(
    study_as_preparation_vs_fulfillment,
    'Is the act of studying sacrifice laws a form of preparation for future performance, or does it, in some sense, fulfill the commandment in the present?',
    'Further halakhic and theological discourse, potentially influenced by shifts in communal understanding or messianic expectations.',
    'If study is re-interpreted as present fulfillment, this reading would converge with ''study_as_performance'', potentially shifting its classification towards a ''rope'' or even a ''mountain'' (if seen as an immutable form of worship). If it remains strictly preparatory, the ''scaffold'' classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_preparation_vs_fulfillment, conceptual, 'Conceptual ambiguity regarding the nature of study in relation to the commandment.').

omega_variable(
    kernel_reading_sacrifice_commandment,
    'This constraint is the ''archive_maintenance'' reading of the ''sacrifice_commandment'' kernel. How do its structural properties differ from sibling readings?',
    'Comparative analysis with ''performance_only'' and ''study_as_performance'' readings, focusing on their respective extractiveness, suppression, and beneficiary/victim structures.',
    'The ''archive_maintenance'' reading''s moderate extractiveness and scaffold nature contrast with the ''performance_only'' reading (likely a ''piton'' or ''mountain'' of suspension) and ''study_as_performance'' (likely a ''rope'' or ''tangled_rope'' with present benefits).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sacrifice_commandment, conceptual, 'Documents this constraint as one reading within the ''sacrifice_commandment'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.09).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__archive_maintenance, theater_ratio, 60, 0.11).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__archive_maintenance, theater_ratio, 80, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__archive_maintenance, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__archive_maintenance, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__archive_maintenance, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__archive_maintenance, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__archive_maintenance, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(sacr_su_t40, sacrifice_commandment__archive_maintenance, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(sacr_su_t60, sacrifice_commandment__archive_maintenance, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(sacr_su_t80, sacrifice_commandment__archive_maintenance, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__archive_maintenance, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, information_standard).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'sacrifice_commandment' kernel, each with different structural properties and classifications. This reading focuses on knowledge preservation for future Temple restoration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
