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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold (Sovereignty Preservation Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the interpretation of NPT Article X that
 *   emphasizes a state's sovereign right to withdraw from the treaty,
 *   potentially at a lower threshold of 'supreme interests' being jeopardized
 *   than nuclear-weapon states (NWS) would prefer. This reading is often
 *   advanced by non-nuclear-weapon states (NNWS), particularly those with
 *   advanced civilian nuclear programs (threshold states), to maintain
 *   leverage and preserve their security options. The North Korean precedent,
 *   while condemned, demonstrates the practical ambiguity of the withdrawal
 *   process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.6).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.7).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold (Sovereignty Preservation Reading)").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '0f33935c-f1c9-4611-a03e-61df76678e82').
narrative_ontology:cs_kernel_codification('0f33935c-f1c9-4611-a03e-61df76678e82', fixed_text).
narrative_ontology:cs_authority_grounding('0f33935c-f1c9-4611-a03e-61df76678e82', lineage).
narrative_ontology:cs_interpretation_layer_present('0f33935c-f1c9-4611-a03e-61df76678e82').
narrative_ontology:cs_reading_relation('0f33935c-f1c9-4611-a03e-61df76678e82', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f33935c-f1c9-4611-a03e-61df76678e82', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('0f33935c-f1c9-4611-a03e-61df76678e82', foundational, sovereign_right_to_exit_treaty).
narrative_ontology:cs_axiom_status(sovereign_right_to_exit_treaty, holdable).
narrative_ontology:cs_axiom_grounding('0f33935c-f1c9-4611-a03e-61df76678e82', sovereign_right_to_exit_treaty, deontological).
narrative_ontology:cs_axiom('0f33935c-f1c9-4611-a03e-61df76678e82', foundational, national_security_as_supreme_interest).
narrative_ontology:cs_axiom_status(national_security_as_supreme_interest, holdable).
narrative_ontology:cs_axiom_grounding('0f33935c-f1c9-4611-a03e-61df76678e82', national_security_as_supreme_interest, conventional).
narrative_ontology:cs_reference_frame('0f33935c-f1c9-4611-a03e-61df76678e82', state_sovereignty_framework).
narrative_ontology:cs_drift_state('0f33935c-f1c9-4611-a03e-61df76678e82', post_north_korea_withdrawal, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0f33935c-f1c9-4611-a03e-61df76678e82', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states_seeking_leverage).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, international_non_proliferation_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with advanced civilian nuclear programs that could quickly develop nuclear weapons. They benefit from the ambiguity of Article X, as it provides a credible, albeit costly, exit option, enhancing their security leverage and preserving sovereignty.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    powerful, generational, constrained, regional).

% NNWS that use the potential for withdrawal, or the ambiguity of Article X, to press NWS for disarmament or security assurances. They benefit from the perceived flexibility of the treaty.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states_seeking_leverage, beneficiary,
    moderate, biographical, constrained, global).

% The five recognized NWS (US, Russia, UK, France, China) who view the NPT as foundational to global security. They bear the cost of regime instability and the erosion of non-proliferation norms when states threaten or execute withdrawal, as it challenges their preferred interpretation of a high withdrawal threshold.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states, payer,
    institutional, civilizational, constrained, global).

% The collective body of treaties, norms, and institutions designed to prevent the spread of nuclear weapons. It bears the cost of any interpretation that weakens the NPT's binding nature or creates loopholes for proliferation, as it undermines its foundational purpose.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_non_proliferation_regime, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__withdrawal_threshold_reading, international_non_proliferation_regime).

% The primary international body responsible for maintaining peace and security. It is tasked with responding to NPT withdrawals, often condemning them and imposing sanctions, thereby actively enforcing the high-threshold interpretation, even if the treaty text itself is ambiguous.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Academics and legal experts who analyze the NPT's text, negotiating history, and state practice to interpret Article X. They provide independent analysis of the legal validity and implications of different withdrawal thresholds.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal, albeit ambiguous, mechanism for states to exit the NPT under extraordinary circumstances, thereby coordinating the conditions under which a state might legitimately leave the non-proliferation regime without immediately triggering military intervention.
% TRANSFER_FUNCTION: Transfers a degree of sovereign flexibility and leverage to NNWS (especially threshold states) at the cost of increased uncertainty and potential instability for the NWS and the broader non-proliferation regime.
% ABSENT_VOICES: States that have already withdrawn from the NPT (e.g., North Korea) are absent from the interpretive debate within the treaty framework, but their actions serve as a precedent for this reading. States that might consider withdrawal in the future are also 'absent' but their potential actions are shaped by this interpretation.
% DISAPPEARANCE_RATIONALE: If the Article X withdrawal clause vanished, states would either be permanently bound (a major shift in sovereignty) or would simply denounce the treaty without any legal framework, leading to greater chaos and a complete breakdown of the non-proliferation regime. The current ambiguity, while problematic, provides a 'safety valve' that prevents immediate collapse.
% FOUNDING_PROBLEM: The NPT was designed to prevent nuclear proliferation while acknowledging state sovereignty. Article X was included to address the concern that states might face unforeseen security threats that would jeopardize their 'supreme interests,' requiring an exit mechanism to preserve their national security.
% FOUNDING_PROBLEM_CORROBORATION: Many NNWS, particularly those in volatile regions, continue to assert that their 'supreme interests' could be jeopardized by evolving security threats, making the withdrawal clause a live concern. This is corroborated by ongoing debates in international forums and the actions of states like Iran, which maintain a robust civilian nuclear program while facing regional security challenges. NWS, however, contest the ease with which 'supreme interests' can be invoked.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).

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
 *   The extractiveness (0.6) is moderate because while the NPT generally constrains NNWS, this reading of Article X provides a credible, albeit costly, exit option, allowing some states to extract concessions or maintain strategic ambiguity. Suppression (0.7) is high due to the diplomatic and economic pressure applied by NWS to deter withdrawal, but the legal ambiguity of Article X means this suppression is not absolute. The theater ratio (0.4) reflects the performative aspects of both NWS condemnation of withdrawals and NNWS declarations of 'supreme interests' being jeopardized, often without clear, universally accepted criteria.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of NWS, this reading of Article X is a dangerous loophole that undermines the non-proliferation regime, making it a Snare. From the perspective of threshold states, it is a necessary Rope or even a Scaffold, preserving sovereignty and providing a safety valve against perceived security threats. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and NNWS seeking leverage are beneficiaries (d near 0.0-0.3) as this reading preserves their options and bargaining power. NWS and the broader international non-proliferation regime are victims (d near 0.7-1.0) as it introduces instability and challenges the treaty's binding nature. The ambiguity of the withdrawal criteria allows for this asymmetric benefit/cost structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving state sovereignty and security) is still live, but its function is contested. NWS argue the original intent was a high bar for withdrawal to ensure regime stability. This reading, however, highlights the original intent of preserving sovereign exit, preventing the constraint from becoming a pure Snare by offering a (contested) pathway out. The North Korean precedent, while problematic, reinforces the practical reality of this pathway, preventing the constraint from being mislabeled as a pure Mountain of international law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_identification,
    'Is this constraint a genuine interpretation of NPT Article X, or a strategic reading by states seeking to preserve exit options?',
    'Analysis of state practice and legal arguments from non-aligned states, particularly those with latent nuclear capabilities, compared to the NWS''s interpretations.',
    'If a genuine interpretation, it highlights a structural ambiguity in the NPT. If strategic, it reveals a mechanism for states to leverage the treaty''s ambiguities for national security interests, potentially undermining the non-proliferation norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_kernel_reading_identification, conceptual, 'This constraint is one reading of the ''npt_treaty_text'' kernel, specifically the ''withdrawal_threshold_reading''. Sibling readings include ''nws_reading'' (non-proliferation as binding on NNWS, disarmament as aspirational) and ''nnws_reading'' (disarmament as binding obligation). This reading emphasizes the right of withdrawal as a sovereign prerogative, potentially at a lower threshold than NWS prefer, creating an ambiguous pathway for states like North Korea, Iran, Japan, or South Korea to maintain credible exit options.').

omega_variable(
    withdrawal_threshold_ambiguity,
    'What constitutes ''extraordinary events, related to the subject matter of this Treaty, have jeopardized the supreme interests of its country'' for NPT Article X withdrawal?',
    'International Court of Justice advisory opinion or a new UN Security Council resolution clarifying the criteria for legitimate withdrawal.',
    'A clear, high threshold would strengthen the non-proliferation regime and reduce leverage for threshold states. A low, ambiguous threshold maintains the current ''tangled rope'' dynamic, where states can credibly threaten withdrawal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(withdrawal_threshold_ambiguity, empirical, 'The core ambiguity in NPT Article X regarding the conditions for withdrawal. This ambiguity is central to the ''withdrawal_threshold_reading''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(npt__tr_t5, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(npt__be_t5, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(npt__be_t10, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 10, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(npt__su_t5, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(npt__su_t10, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'npt_treaty_text' kernel. This reading focuses on the interpretation of Article X withdrawal conditions, while 'npt_treaty_text__nws_reading' focuses on NWS obligations and 'npt_treaty_text__nnws_reading' focuses on NNWS disarmament obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
