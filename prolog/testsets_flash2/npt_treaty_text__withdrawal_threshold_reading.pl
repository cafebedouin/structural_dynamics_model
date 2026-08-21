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
 *   human_readable: NPT Article X Withdrawal Threshold (Sovereignty Preservation Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty preservation' reading of NPT
 *   Article X, which interprets the withdrawal threshold as relatively low,
 *   prioritizing a state's right to exit the treaty under national security
 *   exigencies. This reading is influenced by precedents like North Korea's
 *   withdrawal, which demonstrated a practical pathway for states to leave
 *   the NPT. It benefits threshold states by maintaining the credibility of
 *   their exit options, providing leverage against NWS. The constraint is
 *   claimed as a Tangled Rope due to its genuine coordination function
 *   (providing a withdrawal mechanism) coupled with asymmetric extraction
 *   (NWS bear the cost of increased proliferation risk, while threshold
 *   states gain leverage).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.65).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold (Sovereignty Preservation Reading)").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '3637c159-7518-4b05-8e90-2d08e5c7b349').
narrative_ontology:cs_kernel_codification('3637c159-7518-4b05-8e90-2d08e5c7b349', fixed_text).
narrative_ontology:cs_authority_grounding('3637c159-7518-4b05-8e90-2d08e5c7b349', lineage).
narrative_ontology:cs_interpretation_layer_present('3637c159-7518-4b05-8e90-2d08e5c7b349').
narrative_ontology:cs_reading_relation('3637c159-7518-4b05-8e90-2d08e5c7b349', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('3637c159-7518-4b05-8e90-2d08e5c7b349', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('3637c159-7518-4b05-8e90-2d08e5c7b349', foundational, sovereign_right_to_withdraw).
narrative_ontology:cs_axiom_status(sovereign_right_to_withdraw, holdable).
narrative_ontology:cs_axiom_grounding('3637c159-7518-4b05-8e90-2d08e5c7b349', sovereign_right_to_withdraw, deontological).
narrative_ontology:cs_axiom('3637c159-7518-4b05-8e90-2d08e5c7b349', foundational, national_security_exigency_justifies_withdrawal).
narrative_ontology:cs_axiom_status(national_security_exigency_justifies_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('3637c159-7518-4b05-8e90-2d08e5c7b349', national_security_exigency_justifies_withdrawal, instrumental).
narrative_ontology:cs_reference_frame('3637c159-7518-4b05-8e90-2d08e5c7b349', post_westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('3637c159-7518-4b05-8e90-2d08e5c7b349', post_north_korea_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3637c159-7518-4b05-8e90-2d08e5c7b349', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states_seeking_leverage).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, npt_regime_stability_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States (e.g., Iran, potentially Japan, South Korea) with the technical capacity to develop nuclear weapons but currently adhering to the NPT. They benefit from an ambiguous withdrawal pathway as it preserves their sovereign option to exit the treaty if security conditions change, providing leverage in international negotiations.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    powerful, generational, constrained, regional).

% NNWS that view the NPT as an unequal bargain and seek to maintain maximum flexibility, including the credible threat of withdrawal, to pressure NWS on disarmament commitments. An easier withdrawal path enhances their bargaining power.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states_seeking_leverage, beneficiary,
    moderate, biographical, constrained, global).

% The five NPT-recognized nuclear weapon states (US, UK, France, Russia, China). They bear the cost of an ambiguous or low withdrawal threshold as it undermines the non-proliferation norm, increases the risk of proliferation, and complicates their security planning. They prefer a high threshold to maintain regime stability.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% International organizations, NGOs, and states (both NWS and NNWS) that prioritize the stability and universality of the NPT regime. They view any ambiguity in Article X as a threat to the treaty's integrity and bear the cost of increased proliferation risk and erosion of the non-proliferation norm.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, npt_regime_stability_advocates, payer,
    organized, civilizational, identity_locked, global).

% The IAEA administers the NPT's safeguards system and plays a role in verifying compliance and reporting on withdrawal notifications. While not directly setting the threshold, its reporting and verification activities are central to the practical implications of any withdrawal. It seeks clarity and adherence to established procedures.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% A former NPT signatory that withdrew and developed nuclear weapons. Its precedent is central to the debate over withdrawal thresholds, demonstrating that a state can exit the treaty and pursue nuclearization, thereby influencing the 'sovereignty preservation' reading. It is excluded from the current NPT interpretive debate but its actions shape it.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, north_korea, excluded,
    powerful, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal mechanism for states to exit the NPT, acknowledging sovereign rights while attempting to manage the implications for international security. It coordinates the process of disengagement from treaty obligations.
% TRANSFER_FUNCTION: Transfers the burden of proof and international scrutiny onto withdrawing states, while simultaneously preserving the sovereign right of states to withdraw from treaties, thereby transferring a degree of leverage to threshold states.
% ABSENT_VOICES: States that have previously withdrawn from the NPT (e.g., North Korea) are absent from the interpretive debate, yet their actions provide the most significant empirical data for the 'low threshold' reading. Their perspective would emphasize the practical necessity of withdrawal for national security.
% DISAPPEARANCE_RATIONALE: If Article X vanished, states would lack a formal, internationally recognized mechanism for treaty withdrawal. This would either lead to unilateral abrogation (increasing instability) or a de facto permanent commitment, fundamentally altering the balance of sovereign rights and international obligations within the non-proliferation regime.
% FOUNDING_PROBLEM: To balance the sovereign right of states to withdraw from treaties with the need for a stable and predictable non-proliferation regime, ensuring that withdrawal is not arbitrary but also not impossible.
% FOUNDING_PROBLEM_CORROBORATION: NWS and NPT regime advocates attest the problem is live due to ongoing proliferation risks and challenges to the treaty's integrity. Threshold states also attest it is live, as they seek to preserve their sovereign options. The North Korean precedent provides empirical corroboration of the problem's enduring relevance.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.58, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because while the mechanism provides a necessary coordination function, the ambiguity around 'extraordinary events' allows for strategic exploitation by some states, imposing costs on others. Suppression (0.65) is high because NWS and regime advocates actively work to deter withdrawals and raise the perceived threshold, often through diplomatic pressure and sanctions. Theater ratio (0.25) is moderate, reflecting the ongoing diplomatic performance around the 'seriousness' of withdrawal, even as the practical pathway has been demonstrated. The slight oscillation in extractiveness and suppression reflects periods of heightened tension (e.g., specific proliferation crises) followed by periods of relative calm.
 *
 * PERSPECTIVAL GAP:
 *   NWS perceive this reading as highly extractive, undermining the non-proliferation norm they seek to uphold. Threshold states, however, see it as a necessary safeguard of their sovereignty and a tool for achieving a more equitable non-proliferation regime. The engine's classification will reflect this divergence based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and NNWS seeking leverage are beneficiaries (low d) as this reading preserves their sovereign options and provides bargaining power. NWS and NPT regime stability advocates are payers (high d) as they bear the costs of increased proliferation risk and regime instability. The IAEA, as an agenda-setter, seeks to manage the process but is also constrained by the political interpretations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_threshold_ambiguity,
    'Is the NPT Article X withdrawal threshold inherently ambiguous, or could it be clarified through legal interpretation or state practice to be definitively high or low?',
    'A definitive ruling by an international court (e.g., ICJ) on the interpretation of ''extraordinary events'' or a new, universally accepted state practice that establishes a clear precedent.',
    'If clarified as high, the constraint would shift towards a Snare for threshold states and a Rope for NWS; if clarified as low, it would become a Rope for threshold states and a Snare for NWS. The current ambiguity maintains its Tangled Rope nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threshold_ambiguity, conceptual, 'Ambiguity of the NPT Article X withdrawal threshold.').

omega_variable(
    north_korea_precedent_impact,
    'To what extent does North Korea''s successful withdrawal and nuclearization truly establish a ''low threshold'' precedent for other states, given its unique circumstances and international condemnation?',
    'Observation of future state withdrawals: if other states successfully withdraw and nuclearize with similar international consequences, the precedent is strengthened. If not, its generalizability is limited.',
    'If the precedent is strong, it reinforces the ''sovereignty preservation'' reading and its Tangled Rope classification. If weak, the perceived threshold for other states remains high, shifting the constraint towards a Snare for threshold states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_korea_precedent_impact, empirical, 'Impact of North Korea''s withdrawal on the perceived threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(npt__be_t10, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(npt__be_t20, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(npt__be_t30, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(npt__be_t40, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(npt__be_t50, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(npt__su_t10, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(npt__su_t20, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(npt__su_t30, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(npt__su_t40, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(npt__su_t50, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT treaty text kernel. This 'withdrawal threshold' reading focuses on Article X, while the 'NWS reading' and 'NNWS reading' focus on Articles I/II and VI respectively. All three are structurally linked as interpretations of the same foundational document.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
