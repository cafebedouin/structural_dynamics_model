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
 *   This constraint represents the 'withdrawal threshold' reading of NPT
 *   Article X, which focuses on the conditions and ease of a state's
 *   withdrawal from the treaty. This reading emphasizes the sovereign right
 *   of states to withdraw, potentially at a lower threshold than regime
 *   stability advocates prefer. The North Korean precedent (2003 withdrawal)
 *   significantly shaped this interpretation, demonstrating a pathway for
 *   states to exit the treaty, thereby increasing the perceived
 *   extractiveness for NWS and regime stability advocates, and creating
 *   leverage for threshold states. This reading is a Tangled Rope because it
 *   genuinely coordinates the sovereign right to withdraw with the need for
 *   some international stability, but it does so with asymmetric extraction,
 *   benefiting states seeking leverage at the expense of those prioritizing
 *   regime stability.
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
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '73d65c3f-7dc1-432e-8d12-79695fc8804f').
narrative_ontology:cs_kernel_codification('73d65c3f-7dc1-432e-8d12-79695fc8804f', fixed_text).
narrative_ontology:cs_authority_grounding('73d65c3f-7dc1-432e-8d12-79695fc8804f', lineage).
narrative_ontology:cs_interpretation_layer_present('73d65c3f-7dc1-432e-8d12-79695fc8804f').
narrative_ontology:cs_reading_relation('73d65c3f-7dc1-432e-8d12-79695fc8804f', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('73d65c3f-7dc1-432e-8d12-79695fc8804f', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('73d65c3f-7dc1-432e-8d12-79695fc8804f', foundational, sovereign_right_to_withdraw).
narrative_ontology:cs_axiom_status(sovereign_right_to_withdraw, holdable).
narrative_ontology:cs_axiom_grounding('73d65c3f-7dc1-432e-8d12-79695fc8804f', sovereign_right_to_withdraw, conventional).
narrative_ontology:cs_axiom('73d65c3f-7dc1-432e-8d12-79695fc8804f', secondary, withdrawal_as_legitimate_leverage).
narrative_ontology:cs_axiom_status(withdrawal_as_legitimate_leverage, holdable).
narrative_ontology:cs_axiom_grounding('73d65c3f-7dc1-432e-8d12-79695fc8804f', withdrawal_as_legitimate_leverage, instrumental).
narrative_ontology:cs_reference_frame('73d65c3f-7dc1-432e-8d12-79695fc8804f', unfettered_sovereign_withdrawal).
narrative_ontology:cs_drift_state('73d65c3f-7dc1-432e-8d12-79695fc8804f', post_north_korea_withdrawal, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('73d65c3f-7dc1-432e-8d12-79695fc8804f', '').
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

% States with advanced nuclear programs that could develop weapons quickly. They benefit from the ambiguity of Article X, which allows them to maintain a credible 'exit option' from the NPT, enhancing their leverage in international negotiations without immediate proliferation. Their exit is constrained by the political and economic costs of withdrawal.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    powerful, generational, constrained, global).

% NNWS that use the threat of NPT withdrawal, or the ambiguity of Article X, to gain concessions from NWS or other international actors. The low threshold reading supports their diplomatic strategy. Their exit is constrained by the political and economic repercussions of withdrawal.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states_seeking_leverage, beneficiary,
    moderate, biographical, constrained, global).

% The five recognized NWS (US, UK, France, Russia, China) who are obligated to pursue disarmament under Article VI, but prioritize non-proliferation by NNWS. They bear the cost of an ambiguous withdrawal clause that undermines the NPT's non-proliferation goals and creates security dilemmas. Their exit from the NPT is practically impossible due to its foundational role in global security.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states, payer,
    institutional, civilizational, constrained, global).

% International organizations, NGOs, and states that prioritize the stability and universality of the NPT regime. They bear the cost of any interpretation that weakens the NPT's ability to prevent proliferation, viewing a low withdrawal threshold as a threat to global security. Their exit is constrained by their commitment to the NPT's goals.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, npt_regime_stability_advocates, payer,
    organized, generational, constrained, global).

% The IAEA is responsible for verifying NPT compliance, but its mandate does not explicitly cover the legality or conditions of withdrawal under Article X. It administers safeguards but operates within the political interpretations of the treaty, navigating the ambiguity of withdrawal. Its exit is analytical, as it is an international body.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, analytical, global).

% NNWS that view Article VI disarmament as a binding obligation for NWS and are frustrated by the NPT's perceived asymmetry. While they might not directly benefit from a low withdrawal threshold, they are excluded from the high-level interpretive debates that shape its application, often feeling their concerns are secondary to NWS security priorities.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states_committed_to_disarmament, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to manage the tension between national sovereignty (right to withdraw) and international security (non-proliferation). It attempts to coordinate expectations around the conditions and consequences of treaty withdrawal.
% TRANSFER_FUNCTION: Transfers the burden of uncertainty and potential security risks from threshold states (who gain leverage) to NWS and regime stability advocates (who face increased proliferation concerns).
% ABSENT_VOICES: Non-nuclear weapon states committed to disarmament, who would argue for a clearer, more restrictive withdrawal process to strengthen the NPT's non-proliferation and disarmament pillars, are often marginalized in debates dominated by NWS and threshold states.
% DISAPPEARANCE_RATIONALE: If the current interpretation of Article X's withdrawal threshold vanished, the NPT's stability would be severely tested. Threshold states would either face immediate pressure to abandon their nuclear programs or would withdraw, leading to a rapid increase in proliferation risk and a fundamental reordering of global security alliances.
% FOUNDING_PROBLEM: The NPT was designed to prevent the spread of nuclear weapons while allowing states to pursue peaceful nuclear energy, and to balance the sovereign right of states to withdraw from treaties with the need for regime stability.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and diplomatic historians corroborate that balancing sovereignty and stability was a core tension during the NPT's drafting. The ongoing debates and the North Korean precedent confirm this problem remains live, with different parties emphasizing different aspects of the original compromise.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) is moderate because the ambiguity of Article X allows threshold states to maintain a credible nuclear option, extracting concessions or security guarantees from NWS. Suppression (0.65) is significant because NWS and international bodies actively work to deter withdrawals and impose costs on states that do, but the treaty text itself provides a clear (though contested) pathway. Theater ratio (0.25) reflects the diplomatic performances around withdrawal threats and the limited practical enforcement mechanisms beyond sanctions. The rise in extractiveness and suppression around 2003 reflects the impact of North Korea's withdrawal, which concretized the 'low threshold' interpretation for many.
 *
 * PERSPECTIVAL GAP:
 *   NWS and regime stability advocates perceive the current interpretation of Article X as highly extractive, undermining the treaty's core purpose. Threshold states, however, view it as a necessary safeguard of sovereignty and a tool for balancing power. The engine's per-seat classification will reflect this divergence, with NWS likely computing a Snare or Tangled Rope, while threshold states might see it as a Rope or even a Mountain (of sovereign right).
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and NNWS seeking leverage are beneficiaries (low d) as the ambiguous withdrawal clause provides them with strategic options. NWS and NPT regime stability advocates are targets (high d) as they bear the costs of increased proliferation risk and diplomatic instability. The IAEA, while an agenda-setter for safeguards, is caught in the middle, administering a system whose foundational rules are contested.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_threshold_legality,
    'Is the ''low threshold'' interpretation of Article X''s withdrawal clause legally sound under customary international law and the Vienna Convention on the Law of Treaties, or is it a politically expedient reading?',
    'An advisory opinion from the International Court of Justice or a definitive ruling by a UN Security Council resolution that clarifies the conditions and consequences of NPT withdrawal.',
    'A ruling for a high threshold would increase extractiveness for threshold states and reduce it for NWS, potentially reclassifying this reading towards a Snare for threshold states. A ruling for a low threshold would legitimize the current ambiguity, maintaining the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threshold_legality, conceptual, 'Legal vs. political basis of the withdrawal threshold interpretation.').

omega_variable(
    north_korea_precedent_impact,
    'To what extent did North Korea''s 2003 withdrawal from the NPT establish a de facto ''low threshold'' precedent for other states, and how reversible is this precedent?',
    'Analysis of subsequent state behavior and diplomatic statements regarding NPT withdrawal, and the success or failure of international efforts to deter or reverse future withdrawals.',
    'If the precedent is widely accepted and irreversible, the extractiveness for NWS remains high. If it is seen as an isolated case or successfully countered by international pressure, extractiveness for NWS could decrease, shifting the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(north_korea_precedent_impact, empirical, 'Empirical impact and reversibility of the North Korean withdrawal precedent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.6).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.7).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT treaty text kernel. This reading focuses on the withdrawal threshold, while 'npt_treaty_text__nws_reading' focuses on NWS obligations and 'npt_treaty_text__nnws_reading' focuses on NNWS obligations. All three are structurally linked as interpretations of the same foundational text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
