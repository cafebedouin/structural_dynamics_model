% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right (Sovereignty Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'withdrawal sovereignty' reading
 *   of the 1970 Nuclear Non-Proliferation Treaty (NPT), focusing on Article
 *   X, which grants states the right to withdraw from the treaty if
 *   'extraordinary events' related to its subject matter have jeopardized
 *   their 'supreme interests.' This reading emphasizes the legitimate
 *   exercise of state sovereignty and views treaty obligations as contingent
 *   on the evolving security environment. It contrasts with readings that
 *   prioritize the NPT's nonproliferation function or disarmament
 *   obligations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.6).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.5).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right (Sovereignty Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '1e7dd119-ad25-4929-802b-0d72c33ad3cf').
narrative_ontology:cs_kernel_codification('1e7dd119-ad25-4929-802b-0d72c33ad3cf', fixed_text).
narrative_ontology:cs_authority_grounding('1e7dd119-ad25-4929-802b-0d72c33ad3cf', lineage).
narrative_ontology:cs_interpretation_layer_present('1e7dd119-ad25-4929-802b-0d72c33ad3cf').
narrative_ontology:cs_reading_relation('1e7dd119-ad25-4929-802b-0d72c33ad3cf', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e7dd119-ad25-4929-802b-0d72c33ad3cf', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_axiom('1e7dd119-ad25-4929-802b-0d72c33ad3cf', foundational, state_sovereignty_is_supreme).
narrative_ontology:cs_axiom_status(state_sovereignty_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('1e7dd119-ad25-4929-802b-0d72c33ad3cf', state_sovereignty_is_supreme, deontological).
narrative_ontology:cs_axiom('1e7dd119-ad25-4929-802b-0d72c33ad3cf', foundational, treaty_obligations_are_contingent_on_security).
narrative_ontology:cs_axiom_status(treaty_obligations_are_contingent_on_security, holdable).
narrative_ontology:cs_axiom_grounding('1e7dd119-ad25-4929-802b-0d72c33ad3cf', treaty_obligations_are_contingent_on_security, empirically_contingent).
narrative_ontology:cs_reference_frame('1e7dd119-ad25-4929-802b-0d72c33ad3cf', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('1e7dd119-ad25-4929-802b-0d72c33ad3cf', post_cold_war_security_environment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1e7dd119-ad25-4929-802b-0d72c33ad3cf', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, states_prioritizing_national_security).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, states_committed_to_strict_nonproliferation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As original drafters and custodians of the NPT, they acknowledge the Article X withdrawal right as a feature of sovereign states, even while seeking to maintain the overall nonproliferation regime. They benefit from the regime's stability but also from their own nuclear status.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% States with the technical capacity to develop nuclear weapons but currently non-NWS. They benefit from the option value of Article X, using the threat of withdrawal as leverage in security negotiations or as a last resort in deteriorating security environments. This reading legitimizes their strategic flexibility.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    powerful, biographical, constrained, regional).

% States that view their treaty obligations as contingent on their evolving national security environment. This reading provides a legal basis for prioritizing perceived existential threats over strict adherence to nonproliferation commitments, even if withdrawal is a costly option.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, states_prioritizing_national_security, beneficiary,
    moderate, biographical, constrained, national).

% The overall stability and credibility of the nuclear nonproliferation regime. This reading, by emphasizing the conditional nature of adherence, introduces uncertainty and potential for erosion, making the regime itself 'pay' in terms of reduced predictability and increased risk.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability).

% States that view the NPT as a cornerstone of international security and advocate for strict adherence to its nonproliferation norms. They bear the costs of regime instability and the erosion of trust when states exercise or threaten withdrawal, as it undermines the collective security framework.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, states_committed_to_strict_nonproliferation, payer,
    organized, generational, constrained, global).

% Advocates for universal nuclear disarmament and the strengthening of nonproliferation norms. From their perspective, the emphasis on sovereign withdrawal rights undermines the moral and legal imperative for disarmament and nonproliferation, but their voice is often marginalized in state-centric security debates.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, global_civil_society_disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% The International Atomic Energy Agency monitors compliance with NPT safeguards. While not directly involved in the political interpretation of Article X, its verification work is directly impacted by states' decisions to withdraw, requiring it to adapt its monitoring and reporting functions.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, iaea, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for states to exit the NPT, acknowledging their sovereign right to reassess treaty obligations in light of supreme national interests, thereby maintaining the treaty's overall legitimacy by not trapping states indefinitely.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority regarding treaty adherence from the collective regime to individual sovereign states, contingent on their security assessments. This transfers risk and responsibility back to the withdrawing state and the international community.
% ABSENT_VOICES: Global civil society and disarmament advocates are often excluded from the state-centric legal and political discourse surrounding Article X, which tends to prioritize state sovereignty and national security over universal disarmament norms.
% DISAPPEARANCE_RATIONALE: If the Article X withdrawal right vanished overnight, the NPT would fundamentally change from a voluntary, albeit binding, treaty into an immutable, perpetual obligation. Many states, particularly those in volatile security environments, might never have joined or would immediately seek to denounce the treaty, leading to a complete reorganization of the nonproliferation architecture.
% FOUNDING_PROBLEM: The NPT was designed to prevent the spread of nuclear weapons while acknowledging the sovereign right of states to make decisions vital to their national security, including the right to withdraw from treaties under extraordinary circumstances.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of sovereign states and the evolving nature of global security threats corroborate the ongoing relevance of a withdrawal mechanism. International legal scholars and diplomatic historians, outside the immediate beneficiaries, attest to the historical context of Article X as a necessary compromise to achieve broad adherence to the NPT.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a coordination function (allowing states to join a nonproliferation regime without permanently sacrificing sovereignty) but also involves asymmetric extraction (from the regime's stability) and requires active enforcement (diplomatic pressure, sanctions) to manage withdrawals. Extractiveness is moderate (0.60) from the perspective of states feeling constrained by the treaty despite the withdrawal right, as they perceive a 'cost' to their sovereign freedom of action. Suppression is moderate (0.50) because while the treaty aims to suppress proliferation, the explicit withdrawal clause provides a legal, albeit costly, exit. Theater ratio is low (0.10) as the withdrawal right is a genuine, functional aspect of international law, not mere performance. Accessibility collapse is moderate (0.40) because the withdrawal right means alternatives (developing nuclear weapons) are not completely foreclosed, only made conditional and costly. Resistance is moderate (0.50) as states have occasionally threatened or exercised this right, leading to significant international pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of threshold states or those prioritizing national security, the withdrawal right is a crucial safeguard of sovereignty, making the treaty less extractive. However, from the perspective of the nonproliferation regime itself and states committed to strict nonproliferation, the same right introduces instability and undermines collective security, making the treaty's 'tangle' more pronounced and its costs higher.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and those prioritizing national security are beneficiaries, as the withdrawal right provides them with strategic flexibility and leverage (low directionality). The nonproliferation regime's stability and states committed to strict nonproliferation are victims, as they bear the costs of potential withdrawals and regime erosion (high directionality). Nuclear weapon states, as agenda-setters, navigate between maintaining the regime and acknowledging sovereign rights, placing them closer to symmetric but with a slight beneficiary tilt due to their privileged status.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the NPT as a pure Snare by acknowledging the genuine coordination function of allowing sovereign states to join a nonproliferation regime with a legal exit. Conversely, it prevents mislabeling it as a pure Rope by recognizing the extractive element from the regime's stability and the active enforcement required to manage the consequences of withdrawal. The 'tangle' lies in the tension between sovereign rights and collective security.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_right_safety_valve_or_loophole,
    'Is the Article X withdrawal right a necessary safety valve that enhances the NPT''s long-term legitimacy and adherence, or is it a loophole that undermines the regime''s stability and encourages proliferation?',
    'Empirical analysis of state behavior post-withdrawal (e.g., North Korea) and counterfactual analysis of NPT adherence if Article X did not exist. Diplomatic and legal interpretations from a broad range of states and international bodies.',
    'If primarily a safety valve, the constraint''s suppression is lower but its coordination function is stronger. If primarily a loophole, the constraint''s extractiveness from the regime is higher, and its suppression of proliferation is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_right_safety_valve_or_loophole, conceptual, 'Ambiguity of Article X''s function in regime stability.').

omega_variable(
    security_environment_objectivity,
    'To what extent can ''extraordinary events'' jeopardizing ''supreme interests'' (the criteria for withdrawal) be objectively determined, versus being subject to subjective state interpretation and political opportunism?',
    'Development of international legal precedents or a UN Security Council mechanism for adjudicating the legitimacy of withdrawal claims, or a clear consensus among international legal experts on objective criteria.',
    'If criteria are largely subjective, the constraint''s extractiveness from the regime''s stability is higher, as withdrawals are less predictable. If objective criteria emerge, the constraint becomes more predictable and potentially less destabilizing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_environment_objectivity, empirical, 'Objectivity of withdrawal criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2024, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, ctbt_treaty_1996).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 1970 NPT Treaty, each with different ε values and structural implications. This reading emphasizes the sovereign right to withdraw (Article X), while others focus on nonproliferation (Articles I-II) or disarmament (Article VI).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
