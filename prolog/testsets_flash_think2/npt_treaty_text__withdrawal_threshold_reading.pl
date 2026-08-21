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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold (Sovereignty vs. Stability Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents a specific reading of Article X of the Nuclear
 *   Non-Proliferation Treaty (NPT), which governs state withdrawal. This
 *   reading focuses on the inherent tension between a state's sovereign right
 *   to withdraw from a treaty and the international community's interest in
 *   maintaining the stability of the non-proliferation regime. The ambiguity
 *   in Article X, particularly regarding the 'extraordinary events'
 *   justifying withdrawal, creates a contested threshold. The North Korean
 *   withdrawal in 2003 significantly impacted the practical interpretation
 *   and enforcement of this article, shifting the balance and creating a
 *   precedent for a lower effective threshold than some depositary states
 *   prefer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.55).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.7).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold (Sovereignty vs. Stability Reading)").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '7b22fed8-4657-4249-bd12-0d8633f6af5c').
narrative_ontology:cs_kernel_codification('7b22fed8-4657-4249-bd12-0d8633f6af5c', fixed_text).
narrative_ontology:cs_authority_grounding('7b22fed8-4657-4249-bd12-0d8633f6af5c', lineage).
narrative_ontology:cs_interpretation_layer_present('7b22fed8-4657-4249-bd12-0d8633f6af5c').
narrative_ontology:cs_reading_relation('7b22fed8-4657-4249-bd12-0d8633f6af5c', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b22fed8-4657-4249-bd12-0d8633f6af5c', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('7b22fed8-4657-4249-bd12-0d8633f6af5c', foundational, sovereign_right_to_withdraw_from_treaties).
narrative_ontology:cs_axiom_status(sovereign_right_to_withdraw_from_treaties, holdable).
narrative_ontology:cs_axiom_grounding('7b22fed8-4657-4249-bd12-0d8633f6af5c', sovereign_right_to_withdraw_from_treaties, deontological).
narrative_ontology:cs_axiom('7b22fed8-4657-4249-bd12-0d8633f6af5c', foundational, treaty_stability_requires_orderly_exit).
narrative_ontology:cs_axiom_status(treaty_stability_requires_orderly_exit, holdable).
narrative_ontology:cs_axiom_grounding('7b22fed8-4657-4249-bd12-0d8633f6af5c', treaty_stability_requires_orderly_exit, instrumental).
narrative_ontology:cs_reference_frame('7b22fed8-4657-4249-bd12-0d8633f6af5c', balanced_withdrawal_sovereignty).
narrative_ontology:cs_drift_state('7b22fed8-4657-4249-bd12-0d8633f6af5c', post_north_korea_withdrawal, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7b22fed8-4657-4249-bd12-0d8633f6af5c', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, npt_depositary_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, international_non_proliferation_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the original drafters and custodians of the NPT, they interpret Article X to prioritize regime stability, implying a high threshold for legitimate withdrawal. They benefit from the ambiguity by maintaining leverage over potential withdrawing states, but also bear the cost of managing the regime's integrity.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, npt_depositary_states, agenda_setter,
    institutional, generational, constrained, global).

% States with the technical capacity to develop nuclear weapons but currently adhering to the NPT. They benefit from the ambiguity in Article X, as it preserves a credible, albeit contested, exit option, enhancing their sovereignty and bargaining power without immediate proliferation. Examples include Iran, Japan, South Korea.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    organized, biographical, constrained, national).

% The vast majority of NPT signatories who have foresworn nuclear weapons. They bear the cost of an ambiguous withdrawal mechanism, as it creates uncertainty about the treaty's long-term stability and the reliability of non-proliferation commitments, potentially undermining their security assurances.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% The collective body of norms, treaties, and institutions designed to prevent the spread of nuclear weapons. It is a structural entity that is both upheld and challenged by the interpretation of Article X, experiencing the costs of ambiguity in terms of eroded credibility and stability.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_non_proliferation_regime, observer,
    institutional, civilizational, identity_locked, global).

% A former NPT signatory whose withdrawal in 2003 set a precedent for unilateral exit, challenging the 'high threshold' interpretation. Its actions contribute to the ambiguity of Article X, influencing the strategic calculations of other states and the enforcement efforts of the depositary states.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, north_korea, observer,
    powerful, biographical, trapped, national).

% Academics and legal experts who analyze the NPT's provisions, including Article X, and debate its interpretation. They provide critical analysis of the tension between state sovereignty and treaty stability, influencing policy discussions but not directly enforcing the constraint.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal framework for states to withdraw from the NPT, balancing the sovereign right to exit with the imperative of maintaining international peace and security by preventing proliferation.
% TRANSFER_FUNCTION: The ambiguity of Article X transfers the burden of justifying withdrawal, and the associated diplomatic and security costs, between the withdrawing state and the international community, depending on the prevailing interpretation and political will.
% ABSENT_VOICES: States advocating for a clearer, lower threshold for withdrawal (emphasizing sovereignty) or those demanding a higher, more restrictive threshold (emphasizing regime stability) are often marginalized in the ongoing interpretive contest, which tends to favor the status quo ambiguity.
% DISAPPEARANCE_RATIONALE: If the NPT's Article X and its interpretive contest vanished, the international non-proliferation regime would lose a critical, albeit imperfect, mechanism for managing state exits. This would likely lead to increased uncertainty, potential proliferation, and a fundamental reorganization of global security arrangements.
% FOUNDING_PROBLEM: The NPT was designed to be a perpetual treaty, but its drafters recognized the sovereign right of states to withdraw. Article X was included to provide a mechanism for withdrawal in extraordinary circumstances, balancing state sovereignty with the treaty's non-proliferation goals.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN officials, and non-proliferation experts widely corroborate that the tension between state sovereignty and treaty stability, as embodied in Article X, remains a live and contested issue, particularly in light of historical withdrawals and ongoing proliferation concerns.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the process of treaty withdrawal while simultaneously enabling asymmetric extraction through its ambiguity. Extractiveness (0.55) is moderate, reflecting the costs imposed on the non-proliferation regime by an unclear exit path, but also the benefits to threshold states. Suppression (0.70) is high, as depositary states actively enforce their interpretation of a 'high threshold' through diplomatic pressure and sanctions, even as the North Korean precedent challenges this. Theater ratio (0.40) is moderate, as the formal rhetoric of a high threshold often masks the practical reality of a more accessible, albeit costly, withdrawal pathway.
 *
 * PERSPECTIVAL GAP:
 *   Depositary states often frame Article X as a high barrier to withdrawal, essential for regime stability. However, threshold states and some international legal scholars perceive it as a mechanism that, through its ambiguity and precedent, offers a more accessible, albeit costly, path to exit, thereby preserving sovereignty. The engine's classification captures this divergence between the claimed high threshold and the practical, precedent-influenced reality.
 *
 * DIRECTIONALITY LOGIC:
 *   NPT Depositary States are agenda-setters and beneficiaries, as they control the interpretation and enforcement, using ambiguity to maintain leverage. Threshold states are beneficiaries, as the ambiguity preserves their credible exit option, enhancing their strategic position. Non-Nuclear Weapon States (NNWS) are payers, bearing the cost of regime instability and uncertainty. The international non-proliferation regime itself is a victim of the erosion of clear rules, even as it is the object of coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to balance sovereignty and stability remains live, but the interpretation and enforcement have drifted. The North Korean precedent has created a 'practice drift' where the effective threshold for withdrawal is lower than the 'high threshold' rhetoric suggests. This prevents mislabeling it as a pure Snare, as the coordination function (providing an exit mechanism) is still present, but it highlights the extractive aspects of the ambiguity and the costs to the broader non-proliferation regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_threshold_ambiguity,
    'What constitutes ''extraordinary events'' justifying NPT withdrawal under Article X, and how high is the de facto threshold for legitimate withdrawal?',
    'Analysis of state practice, UN Security Council resolutions, and International Court of Justice advisory opinions on treaty withdrawal in the context of security threats.',
    'A clearer, higher threshold would strengthen the non-proliferation regime and reduce extractiveness from NNWS; a lower, more accessible threshold would enhance state sovereignty but increase regime instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threshold_ambiguity, conceptual, 'Ambiguity of the NPT Article X withdrawal threshold.').

omega_variable(
    north_korea_precedent_impact,
    'To what extent has North Korea''s 2003 withdrawal from the NPT altered the effective interpretation and enforcement of Article X for other states?',
    'Comparative analysis of diplomatic responses to subsequent withdrawal threats or declarations, and changes in strategic calculations of threshold states.',
    'If the precedent is widely accepted, it lowers the effective threshold, increasing extractiveness from the regime but potentially benefiting threshold states. If it''s treated as an isolated case, the ''high threshold'' interpretation retains more force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(north_korea_precedent_impact, empirical, 'Impact of North Korea''s NPT withdrawal on Article X interpretation.').

omega_variable(
    sovereignty_vs_stability_balance,
    'What is the optimal balance between respecting state sovereignty (right to withdraw) and ensuring international regime stability (preventing proliferation) in the interpretation of Article X?',
    'International diplomatic consensus-building, potentially leading to a clarifying protocol or amendment to Article X, or a definitive ruling by an international tribunal.',
    'A shift towards sovereignty would reduce suppression but potentially increase proliferation risk; a shift towards stability would increase suppression but enhance regime robustness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_stability_balance, preference, 'Normative balance between state sovereignty and non-proliferation regime stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1992, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.45).
narrative_ontology:measurement(npt__tr_t2014, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2014, 0.42).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(npt__be_t1992, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1992, 0.5).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.58).
narrative_ontology:measurement(npt__be_t2014, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2014, 0.56).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(npt__su_t1992, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1992, 0.68).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.75).
narrative_ontology:measurement(npt__su_t2014, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2014, 0.72).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the NPT treaty text kernel, focusing on the interpretation of Article X withdrawal. Its structural properties and metrics differ significantly from the NWS and NNWS readings, which focus on disarmament obligations and non-proliferation commitments, respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
