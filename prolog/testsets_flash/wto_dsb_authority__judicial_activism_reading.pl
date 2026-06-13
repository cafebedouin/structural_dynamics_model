% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Judicial Activism (Judicial Legislation Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'judicial activism' reading of the WTO
 *   Dispute Settlement Body's (DSB) authority, where panels are perceived to
 *   exceed their treaty mandate by creating new obligations through
 *   interpretive drift, effectively engaging in illegitimate judicial
 *   legislation. This reading emphasizes the resistance from sovereign member
 *   states and the contestation of the DSB's legitimacy, leading to active
 *   non-compliance and withdrawal from enforcement mechanisms. This is one
 *   reading of the 'wto_dsb_authority' kernel, distinct from
 *   'binding_referee_reading' and 'advisory_coordination_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.7).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.6).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism (Judicial Legislation Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '1f294978-fbb9-47d6-8c47-640d63999fb4').
narrative_ontology:cs_kernel_codification('1f294978-fbb9-47d6-8c47-640d63999fb4', fixed_text).
narrative_ontology:cs_authority_grounding('1f294978-fbb9-47d6-8c47-640d63999fb4', extraction).
narrative_ontology:cs_interpretation_layer_present('1f294978-fbb9-47d6-8c47-640d63999fb4').
narrative_ontology:cs_reading_relation('1f294978-fbb9-47d6-8c47-640d63999fb4', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f294978-fbb9-47d6-8c47-640d63999fb4', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('1f294978-fbb9-47d6-8c47-640d63999fb4', foundational, treaty_text_is_supreme).
narrative_ontology:cs_axiom_status(treaty_text_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('1f294978-fbb9-47d6-8c47-640d63999fb4', treaty_text_is_supreme, conventional).
narrative_ontology:cs_axiom('1f294978-fbb9-47d6-8c47-640d63999fb4', foundational, judicial_body_cannot_create_law).
narrative_ontology:cs_axiom_status(judicial_body_cannot_create_law, holdable).
narrative_ontology:cs_axiom_grounding('1f294978-fbb9-47d6-8c47-640d63999fb4', judicial_body_cannot_create_law, deontological).
narrative_ontology:cs_reference_frame('1f294978-fbb9-47d6-8c47-640d63999fb4', original_negotiated_mandate).
narrative_ontology:cs_drift_state('1f294978-fbb9-47d6-8c47-640d63999fb4', contemporary_wto_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1f294978-fbb9-47d6-8c47-640d63999fb4', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat_bureaucracy).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dispute_settlement_lawyers).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, sovereign_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, trade_liberalization_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dispute settlement process, benefits from its expansion and the perceived authority of its rulings. Seeks to maintain and expand the DSB's interpretive scope, which enhances its own institutional power and relevance.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat_bureaucracy, agenda_setter,
    institutional, generational, constrained, global).

% Profit from the complexity and volume of WTO dispute settlement cases, including those arising from contested interpretations. Their professional identity and livelihood are tied to the system's activity and the need for specialized legal expertise.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dispute_settlement_lawyers, beneficiary,
    organized, biographical, mobile, global).

% Are subjected to rulings that they perceive as exceeding the original treaty mandate, creating new obligations without their consent. They face pressure to comply or risk authorized retaliation, leading to active resistance and questioning of the DSB's legitimacy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, sovereign_member_states, payer,
    powerful, generational, constrained, global).

% Bear the direct economic costs of adverse DSB rulings, especially when these rulings impose new trade liberalization requirements or invalidate protective measures based on what they view as illegitimate judicial overreach. They lobby their governments for non-compliance or withdrawal.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_industries, payer,
    organized, biographical, trapped, national).

% The primary institutional actor responsible for the interpretive drift, issuing reports that are perceived as creating new law. Its members are appointed and operate with a degree of independence, contributing to the perception of judicial activism.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_appellate_body, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the DSB's expansive interpretations that push for greater trade liberalization, aligning with their ideological and economic goals. They support the DSB's authority and resist efforts to curb its interpretive scope.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_liberalization_advocates, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for resolving trade disputes between member states, aiming to bring national trade policies into conformity with WTO agreements and prevent unilateral trade actions.
% TRANSFER_FUNCTION: Transfers policy autonomy from sovereign member states to the WTO dispute settlement system, particularly when DSB panels issue rulings that create new obligations through expansive treaty interpretation. This can lead to economic costs for domestic industries and political costs for governments.
% ABSENT_VOICES: National legislatures and domestic regulatory bodies, whose policy-making authority is perceived as being usurped by the DSB's interpretive drift. They would argue for a more constrained, intergovernmental approach to trade law, emphasizing national sovereignty over judicial interpretation.
% DISAPPEARANCE_RATIONALE: If the DSB's perceived judicial activism vanished, member states would likely reassert greater control over trade policy, potentially leading to more unilateral actions or a return to purely diplomatic dispute resolution. The WTO's enforcement mechanism would be significantly weakened, and the global trade legal landscape would become more fragmented.
% FOUNDING_PROBLEM: The problem of states resorting to unilateral trade protectionism and retaliatory measures, leading to trade wars and undermining the stability of the multilateral trading system.
% FOUNDING_PROBLEM_CORROBORATION: While the core problem of preventing trade wars remains live, the specific issue of judicial overreach is attested by numerous member states (e.g., the US, India, South Africa) through official statements, non-compliance actions, and proposals for WTO reform. These sources, outside the direct beneficiaries of the DSB's expanded authority, corroborate the perception of a problem with the DSB's current operational status.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the perceived imposition of new obligations and the erosion of national policy space without explicit consent. Suppression (0.6) is moderate, as member states retain some sovereign capacity to resist, but face significant pressure and potential retaliation. Resistance (0.8) is high, manifesting as non-compliance, blocking Appellate Body appointments, and calls for reform. Accessibility collapse (0.4) is moderate, as states still have diplomatic and political avenues, but the DSB's rulings significantly constrain their options. Theater ratio (0.2) is low, as the DSB's actions are genuinely impactful, though their legitimacy is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the WTO Secretariat and dispute settlement lawyers, the DSB's actions are legitimate interpretations necessary for an effective multilateral trading system. From the perspective of sovereign member states and domestic industries, these actions are an illegitimate overreach, eroding national sovereignty and imposing unfair burdens. The engine's per-seat classification will reflect this divergence based on the declared roles and structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO Secretariat bureaucracy and dispute settlement lawyers are beneficiaries (d near 0.0) as they gain institutional power and professional opportunities from an expansive DSB role. Sovereign member states and domestic industries are victims (d near 1.0) as they bear the costs of perceived overreach and loss of policy autonomy. Trade liberalization advocates also benefit from the DSB's expansive interpretations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_vs_evolution,
    'Is the DSB''s interpretive practice an illegitimate ''judicial activism'' or a necessary ''evolution'' of international trade law to address new challenges?',
    'Analysis of treaty drafting history, subsequent state practice, and the ''ordinary meaning'' of treaty terms in context, as well as a comparative study of international judicial bodies'' interpretive scope.',
    'If deemed illegitimate activism, it strengthens the case for reform or withdrawal; if deemed legitimate evolution, it supports the DSB''s current authority and reduces perceived extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_drift_vs_evolution, conceptual, 'Ambiguity in the nature of treaty interpretation.').

omega_variable(
    sovereignty_vs_multilateralism,
    'To what extent does the DSB''s authority legitimately impinge on national sovereignty for the sake of multilateral trade stability?',
    'A political and legal consensus among member states on the acceptable balance between national policy space and international legal obligations, potentially through renegotiation of WTO agreements.',
    'A re-emphasis on sovereignty would reduce the perceived extraction and suppression from the DSB; a re-affirmation of multilateralism would legitimize the DSB''s current role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_multilateralism, preference, 'The normative trade-off between national sovereignty and multilateral governance.').

omega_variable(
    legitimacy_of_retaliation,
    'Is the authorization of trade retaliation by the DSB a legitimate enforcement tool or an illegitimate coercive mechanism when based on contested interpretations?',
    'A formal review and clarification of the conditions under which retaliation can be authorized, potentially limiting it to cases of clear, undisputed treaty violations.',
    'If deemed illegitimate, it would reduce the DSB''s effective suppression and extractiveness, as member states would be less compelled to comply with contested rulings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_retaliation, empirical, 'The legitimacy and coercive force of authorized trade retaliation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2020, 0.69).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.57).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2020, 0.59).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the WTO DSB's authority. This 'judicial activism' reading emphasizes the DSB's overreach and the resulting resistance from member states, contrasting with readings that emphasize its binding nature or advisory role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
