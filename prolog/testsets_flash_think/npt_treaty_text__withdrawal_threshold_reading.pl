% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold Interpretation
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint represents one reading of Article X of the Nuclear
 *   Non-Proliferation Treaty (NPT), focusing on the contested threshold for
 *   state withdrawal. It is interpreted as a 'tangled rope' because it
 *   genuinely coordinates the sovereign right to withdraw with
 *   non-proliferation goals, but also involves asymmetric extraction due to
 *   the ambiguity of the withdrawal process. The North Korea precedent (2003)
 *   significantly influenced the interpretation, increasing both perceived
 *   extractiveness and the suppression required to maintain the regime's
 *   stability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.68).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.78).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold Interpretation").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '125fac3c-efdb-49d8-87d2-eb817a26ff0a').
narrative_ontology:cs_kernel_codification('125fac3c-efdb-49d8-87d2-eb817a26ff0a', fixed_text).
narrative_ontology:cs_authority_grounding('125fac3c-efdb-49d8-87d2-eb817a26ff0a', lineage).
narrative_ontology:cs_interpretation_layer_present('125fac3c-efdb-49d8-87d2-eb817a26ff0a').
narrative_ontology:cs_reading_relation('125fac3c-efdb-49d8-87d2-eb817a26ff0a', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('125fac3c-efdb-49d8-87d2-eb817a26ff0a', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('125fac3c-efdb-49d8-87d2-eb817a26ff0a', foundational, sovereign_right_to_withdraw_conditional).
narrative_ontology:cs_axiom_status(sovereign_right_to_withdraw_conditional, holdable).
narrative_ontology:cs_axiom_grounding('125fac3c-efdb-49d8-87d2-eb817a26ff0a', sovereign_right_to_withdraw_conditional, conventional).
narrative_ontology:cs_axiom('125fac3c-efdb-49d8-87d2-eb817a26ff0a', foundational, regime_stability_requires_withdrawal_friction).
narrative_ontology:cs_axiom_status(regime_stability_requires_withdrawal_friction, holdable).
narrative_ontology:cs_axiom_grounding('125fac3c-efdb-49d8-87d2-eb817a26ff0a', regime_stability_requires_withdrawal_friction, conventional).
narrative_ontology:cs_reference_frame('125fac3c-efdb-49d8-87d2-eb817a26ff0a', conditional_sovereign_withdrawal_framework).
narrative_ontology:cs_drift_state('125fac3c-efdb-49d8-87d2-eb817a26ff0a', post_north_korea_withdrawal, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('125fac3c-efdb-49d8-87d2-eb817a26ff0a', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, npt_regime_stability_advocates).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, states_seeking_unilateral_exit).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_hardliners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, threshold_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States (e.g., Iran, potentially Japan/South Korea) that benefit from the credible, albeit ambiguous, option to withdraw from the NPT under Article X, preserving their sovereignty options while remaining within the treaty. They bear the cost of treaty obligations but gain strategic flexibility.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, threshold_states, payer).

% States (e.g., P5 members) and international organizations (e.g., IAEA) that prioritize the stability and integrity of the NPT regime. They benefit from the high political and practical threshold for withdrawal, which discourages proliferation, but must actively manage the ambiguity to prevent easy exits.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, npt_regime_stability_advocates, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, npt_regime_stability_advocates, beneficiary).

% States (e.g., North Korea) that have withdrawn or seek to withdraw from the NPT. They bear the costs of international condemnation, sanctions, and isolation, as the international community interprets Article X as requiring a high threshold for legitimate withdrawal. Their actions are often seen as violating the spirit, if not the letter, of the treaty.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, states_seeking_unilateral_exit, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, states_seeking_unilateral_exit, excluded).

% States and advocacy groups that demand a clear, unequivocally high threshold for NPT withdrawal to prevent proliferation. They bear the cost of the current ambiguity, which they see as undermining the non-proliferation norm, and actively lobby for stricter interpretations and enforcement.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_hardliners, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_hardliners, observer).

% Academics and legal experts who analyze the interpretation and application of Article X, particularly in light of state practice and international law. They do not directly benefit or pay but provide critical analysis of the constraint's operation and implications.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to manage their sovereign right to withdraw from a treaty while attempting to maintain international non-proliferation norms, by establishing a conditional exit mechanism.
% TRANSFER_FUNCTION: Transfers the burden of justifying withdrawal from the international community to the withdrawing state, but also transfers the risk of proliferation to the international community if the threshold for withdrawal is perceived as too low.
% ABSENT_VOICES: States that have already withdrawn or are considering withdrawal outside the NPT framework, who would argue for an unfettered right to sovereignty and self-determination, unconstrained by international treaty interpretations.
% DISAPPEARANCE_RATIONALE: If the Article X withdrawal clause vanished, states would either withdraw unilaterally without any international framework, leading to rapid proliferation, or be permanently bound, leading to severe sovereignty conflicts. The international security architecture would fundamentally shift.
% FOUNDING_PROBLEM: To balance the sovereign right of states to withdraw from treaties with the collective security interest in preventing nuclear proliferation, by establishing a conditional, but not impossible, exit mechanism.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN resolutions, and statements from both NWS and NNWS consistently acknowledge this ongoing tension as the core challenge of Article X, corroborating that the problem is live.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate-high because the ambiguity of Article X allows the NPT regime to impose significant costs on withdrawing states, while also providing a credible, albeit difficult, exit option for threshold states. Suppression is high due to the active diplomatic, economic, and military pressure exerted on states that attempt to withdraw without meeting the 'high threshold' interpretation. The theater ratio is moderate, reflecting genuine efforts to maintain the non-proliferation norm alongside performative enforcement actions. The temporal measurements show an increase in extractiveness and suppression following North Korea's withdrawal, as the international community reacted to reinforce the 'high threshold' interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of threshold states, Article X provides a necessary safety valve for sovereignty. From the perspective of non-proliferation hardliners, it is a dangerous loophole. The NPT regime itself attempts to balance these, but the interpretation of the withdrawal threshold is a constant site of contestation, leading to different computed classifications for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states are beneficiaries as the ambiguity preserves their sovereignty options. NPT regime stability advocates also benefit from the friction that discourages proliferation. States seeking unilateral exit are targets, facing severe consequences. Non-proliferation hardliners are also targets, as the ambiguity undermines their goal of an absolute non-proliferation norm. The engine will compute these divergent experiences from the structural data.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_threshold_ambiguity,
    'Is the Article X withdrawal threshold primarily high (regime stability priority) or low (sovereignty preservation priority)?',
    'Further state practice, international court rulings, or a new NPT review conference clarifying the conditions and consequences of withdrawal.',
    'If resolved as high, the constraint becomes more snare-like for withdrawing states; if low, it becomes more rope-like, with less extraction from states asserting sovereignty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(withdrawal_threshold_ambiguity, conceptual, 'The fundamental ambiguity in interpreting Article X''s withdrawal conditions.').

omega_variable(
    north_korea_precedent_impact,
    'To what extent did North Korea''s 2003 withdrawal establish a de facto precedent for future withdrawals, despite international condemnation?',
    'Analysis of subsequent state behavior and international responses to any future withdrawal attempts, or a formal re-evaluation of the precedent by international legal bodies.',
    'If the precedent is seen as strong, it lowers the effective threshold, reducing extraction for potential withdrawing states. If it''s seen as an outlier, the high threshold interpretation is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_korea_precedent_impact, empirical, 'The lasting effect of the North Korea withdrawal on Article X interpretation.').

omega_variable(
    sovereignty_vs_regime_priority,
    'Which normative principle (state sovereignty or NPT regime stability) holds ultimate priority in the interpretation of Article X withdrawal?',
    'A shift in the dominant international legal paradigm, or a consensus among NPT states on the hierarchy of these principles in this specific context.',
    'If sovereignty is prioritized, the constraint leans towards a rope; if regime stability, it leans towards a snare, with greater justification for extraction from withdrawing states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_regime_priority, preference, 'The underlying normative conflict driving the Article X interpretation debate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.35).
narrative_ontology:measurement(npt__tr_t2008, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(npt__tr_t2013, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2013, 0.28).
narrative_ontology:measurement(npt__tr_t2018, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2025, 0.26).

% Extraction over time
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.6).
narrative_ontology:measurement(npt__be_t2008, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2008, 0.63).
narrative_ontology:measurement(npt__be_t2013, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(npt__be_t2018, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.7).
narrative_ontology:measurement(npt__su_t2008, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2008, 0.73).
narrative_ontology:measurement(npt__su_t2013, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2013, 0.75).
narrative_ontology:measurement(npt__su_t2018, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2018, 0.77).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the NPT treaty text kernel, each with its own structural properties and classification. This reading focuses on Article X withdrawal, while siblings address NWS and NNWS obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
