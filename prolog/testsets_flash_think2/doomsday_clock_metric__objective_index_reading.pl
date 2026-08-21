% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock: Objective Index Reading
 *   domain: science_communication/risk_governance/epistemology
 *
 * SUMMARY:
 *   This constraint represents the 'objective index reading' of the Doomsday
 *   Clock, where its setting is understood as a direct, expert-synthesized
 *   reflection of measurable existential risk levels. This reading emphasizes
 *   the scientific and empirical basis of the Clock, positioning it as a
 *   neutral indicator. However, the structural delta for this reading
 *   highlights high suppression of normative framing, with scientific
 *   authority as the beneficiary and democratic accountability as the victim
 *   due to an expert monopoly on interpretation. The claimed type 'rope'
 *   reflects the ideal of scientific coordination, while the metrics capture
 *   the underlying extractive dynamics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.65).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.75).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock: Objective Index Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/risk_governance/epistemology").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '568104a3-3764-444c-9a88-19d7740d4c31').
narrative_ontology:cs_kernel_codification('568104a3-3764-444c-9a88-19d7740d4c31', formalized).
narrative_ontology:cs_authority_grounding('568104a3-3764-444c-9a88-19d7740d4c31', expertise).
narrative_ontology:cs_interpretation_layer_present('568104a3-3764-444c-9a88-19d7740d4c31').
narrative_ontology:cs_reading_relation('568104a3-3764-444c-9a88-19d7740d4c31', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('568104a3-3764-444c-9a88-19d7740d4c31', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('568104a3-3764-444c-9a88-19d7740d4c31', foundational, existential_risk_is_objectively_quantifiable).
narrative_ontology:cs_axiom_status(existential_risk_is_objectively_quantifiable, holdable).
narrative_ontology:cs_axiom_grounding('568104a3-3764-444c-9a88-19d7740d4c31', existential_risk_is_objectively_quantifiable, empirically_contingent).
narrative_ontology:cs_axiom('568104a3-3764-444c-9a88-19d7740d4c31', foundational, expert_consensus_reflects_empirical_truth).
narrative_ontology:cs_axiom_status(expert_consensus_reflects_empirical_truth, holdable).
narrative_ontology:cs_axiom_grounding('568104a3-3764-444c-9a88-19d7740d4c31', expert_consensus_reflects_empirical_truth, empirically_contingent).
narrative_ontology:cs_reference_frame('568104a3-3764-444c-9a88-19d7740d4c31', scientific_objectivity_framework).
narrative_ontology:cs_drift_state('568104a3-3764-444c-9a88-19d7740d4c31', contemporary_science_communication_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('568104a3-3764-444c-9a88-19d7740d4c31', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, scientific_authority).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_accountability_advocates).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, public_discourse_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional body (e.g., Bulletin of the Atomic Scientists) that sets the Doomsday Clock. It claims the authority to synthesize empirical indicators into an objective risk assessment, thereby shaping global discourse and policy recommendations. It benefits from the perceived objectivity and authority of its pronouncements.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, scientific_authority, agenda_setter,
    institutional, generational, constrained, global).

% The panel of scientists and experts whose consensus forms the basis of the Clock's setting. They benefit from their elevated status as authoritative interpreters of complex global risks, influencing policy and public opinion through their expert monopoly on interpretation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_community, beneficiary,
    organized, biographical, mobile, global).

% Advocates who argue for broader public participation and democratic oversight in defining and communicating existential risks. They bear the cost of having normative and political dimensions of risk assessment suppressed by a claim of purely objective, expert-driven indexing. Their input is often marginalized.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_accountability_advocates, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, democratic_accountability_advocates, excluded).

% The general public and various civil society groups who consume the Clock's message. They are presented with an 'objective' assessment that may implicitly contain normative framings they cannot easily challenge, limiting their agency in shaping the risk narrative.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, public_discourse_participants, payer,
    powerless, immediate, trapped, global).

% Government officials and international bodies who rely on the Clock as a high-profile, authoritative indicator of global threats. They benefit from a clear, expert-backed signal for policy prioritization, but may also find their policy options constrained by the 'objective' framing of risks.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_makers, beneficiary,
    institutional, biographical, constrained, national).

% Academics and researchers (e.g., in science and technology studies, risk communication) who critically analyze the methodology, framing, and impact of the Doomsday Clock. They observe the dynamics of expert authority and the suppression of alternative framings.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, critical_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, scientific_authority).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a unified, expert-driven, and purportedly objective assessment of global existential risks, thereby coordinating scientific understanding and guiding policy and public attention towards critical threats.
% TRANSFER_FUNCTION: Transfers interpretive authority over existential risk assessment from broader public and normative discourse to a specialized expert body, concentrating the power to define and prioritize global threats.
% ABSENT_VOICES: Normative ethicists, social scientists emphasizing framing effects, and public representatives who would argue for broader participation in risk definition and communication are structurally excluded. They would challenge the claim of pure objectivity and advocate for a more inclusive, deliberative process.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock's objective index reading vanished overnight, the singular, expert-backed metric for existential risk would disappear. This would likely lead to a proliferation of uncoordinated, potentially conflicting risk assessments, and policy makers and the public would lose a central, widely recognized reference point, potentially hindering coordinated action on global threats.
% FOUNDING_PROBLEM: To provide a clear, authoritative, and scientifically grounded warning about the most pressing global threats, particularly nuclear war, and later, climate change, in a way that could mobilize public and political action.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists (the agenda setter) attests to the problem's ongoing urgency and the Clock's continued relevance. However, critical scholars and some social scientists attest that while the *risks* are real, the Clock's claim to *objective indexing* and its effectiveness as a purely scientific communication tool are contested, suggesting the founding problem's status as 'objectively addressed' is no longer live.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost borne by those whose normative or democratic input into risk assessment is suppressed by the claim of objective indexing. Suppression (0.75) is high due to the active maintenance of an expert monopoly and the marginalization of alternative framings. The theater ratio (0.20) is low, as this reading genuinely attempts to ground the Clock in empirical science, even if its objectivity is contested. Accessibility collapse (0.70) is high because the expert claim to objectivity significantly reduces the perceived legitimacy of alternative interpretations. Resistance (0.55) is moderate, coming from critical scholars and advocates for broader public engagement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of scientific authority, the Clock is a vital coordination mechanism for global risk assessment. From the perspective of democratic accountability advocates, the same structure functions as an extractive mechanism, centralizing interpretive power and suppressing broader public deliberation. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Scientific authority and the expert community are beneficiaries (low d) as they gain interpretive power and influence. Democratic accountability advocates and public discourse participants are targets (high d) as their input is suppressed and they bear the cost of an expert-driven, potentially narrow, risk definition. Policy makers are beneficiaries (low d) of clear guidance but also face constraints from the 'objective' framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objective_vs_normative_entanglement,
    'Is the Doomsday Clock''s setting truly an objective index of risk, or is it inherently entangled with normative choices and political framings?',
    'Comparative analysis of expert consensus formation in other ''objective'' risk assessments, and detailed ethnographic studies of the Clock-setting process to identify implicit normative assumptions.',
    'If inherently entangled, the claim of pure objectivity is a cover story, increasing effective extraction and suppression. If genuinely separable, the constraint''s coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_vs_normative_entanglement, conceptual, 'Ambiguity between objective measurement and normative entanglement in risk assessment.').

omega_variable(
    expert_monopoly_efficacy,
    'Does the expert monopoly on Doomsday Clock interpretation genuinely enhance effective risk communication and policy response, or does it suppress necessary public deliberation and alternative solutions?',
    'Empirical studies comparing policy outcomes and public engagement in contexts with expert-only vs. deliberative risk communication models for similar threats.',
    'If it suppresses deliberation, the constraint''s effective suppression is higher and its coordination function is weaker. If it enhances efficacy, the expert monopoly is justified as a necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expert_monopoly_efficacy, empirical, 'Impact of expert monopoly on risk communication and democratic deliberation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__objective_index_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(doom_tr_t2005, doomsday_clock_metric__objective_index_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(doom_tr_t2010, doomsday_clock_metric__objective_index_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(doom_tr_t2015, doomsday_clock_metric__objective_index_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(doom_tr_t2020, doomsday_clock_metric__objective_index_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(doom_tr_t2025, doomsday_clock_metric__objective_index_reading, theater_ratio, 2025, 0.2).
narrative_ontology:measurement(doom_tr_t2030, doomsday_clock_metric__objective_index_reading, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(doom_be_t2005, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(doom_be_t2010, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(doom_be_t2015, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(doom_be_t2020, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(doom_be_t2025, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement(doom_be_t2030, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2030, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(doom_su_t2005, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(doom_su_t2010, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(doom_su_t2015, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(doom_su_t2020, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(doom_su_t2025, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2025, 0.75).
narrative_ontology:measurement(doom_su_t2030, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2030, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'doomsday_clock_metric' kernel, focusing on its claim to objective, expert-driven risk assessment. It is linked to sibling readings that emphasize its performative or hybrid nature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
