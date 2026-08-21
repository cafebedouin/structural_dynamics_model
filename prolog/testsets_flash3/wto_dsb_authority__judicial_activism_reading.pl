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
    narrative_ontology:epsilon_provenance/5,
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
 *   Dispute Settlement Body's (DSB) authority. From this perspective, DSB
 *   panels and the Appellate Body have exceeded their mandate, engaging in
 *   'judicial legislation' by creating new obligations through expansive
 *   interpretations of WTO agreements. This interpretive drift has led to
 *   active resistance from member states, contested legitimacy of rulings,
 *   and a perceived erosion of national policy autonomy. The constraint is
 *   classified as a Snare because its coordination function (dispute
 *   resolution) is seen as a cover for an extractive transfer of policy
 *   authority, maintained by active enforcement and suppression of
 *   alternatives (e.g., withdrawal from the Appellate Body).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.85).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.75).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism (Judicial Legislation Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, 'f83a56da-e3c6-45fb-8b36-64ac5fb36c55').
narrative_ontology:cs_kernel_codification('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', formalized).
narrative_ontology:cs_authority_grounding('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', extraction).
narrative_ontology:cs_interpretation_layer_present('f83a56da-e3c6-45fb-8b36-64ac5fb36c55').
narrative_ontology:cs_reading_relation('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', foundational, panels_cannot_create_new_obligations).
narrative_ontology:cs_axiom_status(panels_cannot_create_new_obligations, holdable).
narrative_ontology:cs_axiom_grounding('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', panels_cannot_create_new_obligations, conventional).
narrative_ontology:cs_axiom('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', foundational, national_policy_autonomy_is_supreme).
narrative_ontology:cs_axiom_status(national_policy_autonomy_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', national_policy_autonomy_is_supreme, deontological).
narrative_ontology:cs_reference_frame('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', original_treaty_mandate_limited_interpretation).
narrative_ontology:cs_drift_state('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', contemporary_jurisprudence, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f83a56da-e3c6-45fb-8b36-64ac5fb36c55', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat_bureaucracy).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, panels_and_appellate_body_members).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_states_targeted_by_rulings).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_states_seeking_policy_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from an expanded mandate for the DSB, increasing its institutional power and relevance within the WTO framework. It supports interpretations that enhance the DSB's judicial role, as this justifies its own growth and influence.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat_bureaucracy, beneficiary,
    institutional, generational, constrained, global).

% These individuals, acting as adjudicators, interpret WTO agreements. From this reading's perspective, they exceed their mandate by creating new obligations through expansive interpretations, effectively legislating from the bench. Their professional identity is tied to the perceived authority and impact of their rulings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, panels_and_appellate_body_members, agenda_setter,
    powerful, biographical, identity_locked, global).

% Are directly impacted by DSB rulings that impose new obligations or restrict policy space beyond what they believe was agreed in the treaties. They face pressure to comply or risk authorized retaliation, leading to active resistance and non-compliance.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_targeted_by_rulings, payer,
    powerful, immediate, constrained, national).

% These states view the DSB's interpretive drift as an illegitimate encroachment on their sovereign right to set domestic policy. They bear the cost of reduced policy flexibility and the erosion of the original treaty bargain, leading them to question the legitimacy of the system.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_seeking_policy_autonomy, payer,
    organized, generational, constrained, national).

% Analyze the evolution of DSB jurisprudence, often critiquing instances where panels appear to overstep their interpretive authority. They provide academic commentary on the legitimacy and implications of judicial activism within the WTO.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, international_trade_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The DSB mechanism was intended to provide a forum for resolving trade disputes, ensuring a stable and predictable multilateral trading system by interpreting existing agreements.
% TRANSFER_FUNCTION: From this reading's perspective, the constraint transfers policy-making authority from sovereign member states to unelected adjudicators, leading to new obligations and restrictions on national policy space.
% ABSENT_VOICES: The original drafters of the WTO agreements, who intended a more limited interpretive role for the DSB, are absent. Their original intent is overridden by expansive interpretations.
% DISAPPEARANCE_RATIONALE: If the DSB's perceived judicial activism vanished, member states would likely re-engage more constructively with the dispute settlement system, potentially leading to a more politically negotiated, rather than judicially imposed, resolution of trade disputes. The balance of power in international trade governance would shift significantly.
% FOUNDING_PROBLEM: To provide a rules-based mechanism for resolving trade disputes among member states, preventing unilateral retaliatory measures and ensuring the stability of the multilateral trading system.
% FOUNDING_PROBLEM_CORROBORATION: The WTO Secretariat and some member states argue the problem is live, requiring robust interpretation to adapt to new trade realities. However, many member states and legal scholars (outside the benefiting parties) argue the original problem of unilateralism is largely solved, and the current issue is the DSB exceeding its mandate, turning a coordination mechanism into a source of new, unagreed obligations.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the DSB's rulings, from this reading, impose significant costs on member states by restricting their policy space beyond agreed treaty terms. Suppression (0.75) is also high, as member states are pressured to comply with rulings under threat of authorized retaliation, limiting their exit options. The rising trend in both metrics reflects the increasing perception of overreach over time. Theater ratio (0.4) indicates that while some genuine dispute resolution occurs, a substantial portion of the DSB's activity is perceived as maintaining an expanded, illegitimate interpretive authority. Resistance is high (0.8) due to member states actively challenging rulings and blocking Appellate Body appointments.
 *
 * PERSPECTIVAL GAP:
 *   The 'judicial activism' reading highlights a significant perspectival gap between the adjudicators (who may see their role as necessary evolution of international law) and the member states (who see it as an illegitimate power grab). The engine's per-seat classification would reflect this divergence, with adjudicators potentially computing as beneficiaries of a 'rope' or 'tangled rope' (from their own framing) while member states compute as victims of a 'snare'.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO Secretariat and the adjudicators (panels/Appellate Body) are beneficiaries, gaining institutional power and professional influence from an expanded interpretive mandate. Member states targeted by rulings and those seeking policy autonomy are victims, bearing the costs of restricted sovereignty and new obligations. The directionality for these groups reflects their structural position as either gaining from or being extracted by the perceived overreach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_vs_evolution,
    'Is the DSB''s interpretive practice an illegitimate ''judicial legislation'' (as this reading claims) or a necessary ''evolution'' of international trade law to address new challenges?',
    'A global consensus among member states to either explicitly limit the DSB''s interpretive scope through treaty amendment or to formally endorse its current practice. Absent such consensus, the ambiguity persists.',
    'If confirmed as illegitimate judicial legislation, the constraint''s classification as a Snare is strengthened, and calls for reform or withdrawal from the system would intensify. If re-framed as legitimate evolution, the extractiveness and suppression metrics would be re-evaluated downwards, potentially shifting the classification towards a Tangled Rope or even Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_vs_evolution, conceptual, 'Ambiguity regarding the legitimacy of the DSB''s interpretive practices.').

omega_variable(
    sovereignty_vs_multilateralism,
    'To what extent does the DSB''s authority genuinely infringe on national sovereignty, versus representing a necessary pooling of sovereignty for the benefits of a stable multilateral trading system?',
    'Empirical analysis of policy space changes in member states post-DSB rulings, coupled with a normative assessment of the value trade-offs between national autonomy and multilateral cooperation.',
    'If infringement on sovereignty is deemed severe and illegitimate, the Snare classification is reinforced. If the pooling of sovereignty is seen as a net benefit, the perceived extractiveness would decrease, potentially shifting the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_multilateralism, preference, 'The normative trade-off between national sovereignty and multilateral governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_appellate_body_appointment_blockage).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dispute_settlement_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the WTO DSB's authority. It focuses on the perception of judicial overreach and illegitimate legislation. It is linked to the 'binding referee' and 'advisory coordination' readings, which offer alternative interpretations of the DSB's legitimate role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
