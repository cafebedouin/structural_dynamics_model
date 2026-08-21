% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Binding Referee Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint instantiates the 'binding referee' reading of the WTO DSB
 *   authority kernel, which views DSB rulings as legally binding and
 *   requiring member states to surrender policy discretion in WTO-covered
 *   domains. Sibling readings include the 'advisory coordination' reading
 *   (DSB as facilitator) and the 'judicial activism' reading (DSB as
 *   overstepping its mandate). From this 'binding referee' perspective, the
 *   DSB functions as a crucial enforcement mechanism for the rules-based
 *   multilateral trading system, ensuring compliance through authorized
 *   retaliation, even as it extracts significant policy autonomy from member
 *   states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.8).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.85).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Referee Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, 'ce1abdf0-875a-43a4-b7e7-0e0c00a31924').
narrative_ontology:cs_kernel_codification('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', formalized).
narrative_ontology:cs_authority_grounding('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', lineage).
narrative_ontology:cs_interpretation_layer_present('ce1abdf0-875a-43a4-b7e7-0e0c00a31924').
narrative_ontology:cs_reading_relation('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', foundational, treaty_obligations_are_supreme).
narrative_ontology:cs_axiom_status(treaty_obligations_are_supreme, holdable).
narrative_ontology:cs_axiom_grounding('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', treaty_obligations_are_supreme, deontological).
narrative_ontology:cs_axiom('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', foundational, sovereignty_is_pooled_for_trade).
narrative_ontology:cs_axiom_status(sovereignty_is_pooled_for_trade, holdable).
narrative_ontology:cs_axiom_grounding('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', sovereignty_is_pooled_for_trade, conventional).
narrative_ontology:cs_reference_frame('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', rules_based_multilateralism).
narrative_ontology:cs_drift_state('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ce1abdf0-875a-43a4-b7e7-0e0c00a31924', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, global_trading_system).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, successful_complainant_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, non_compliant_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, states_losing_disputes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, member_states_complainants).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, global_trading_community).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, member_states_respondents).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_industries_in_respondent_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The judicial-like bodies that hear disputes and issue rulings. They interpret treaty law and authorize retaliation, effectively setting the compliance agenda for member states.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_dsb_panels, agenda_setter,
    institutional, generational, analytical, global).

% States that bring disputes to the DSB and win. They benefit from the enforcement of treaty obligations against other members, gaining market access or policy space.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, member_states_complainants, beneficiary,
    organized, biographical, mobile, global).

% States that are subject to DSB rulings and are found in violation. They bear the cost of compliance, which often involves changing domestic laws or policies, or face authorized trade retaliation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, member_states_respondents, payer,
    organized, biographical, constrained, global).

% The collective of all trading nations and businesses that benefit from a stable, predictable, and rules-based international trading system, which the DSB's binding authority helps to maintain.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, global_trading_community, beneficiary,
    organized, civilizational, arbitrage, global).

% Industries within states that lose disputes, which may face increased competition, reduced subsidies, or other adverse impacts due to compliance with DSB rulings. Their fate is often determined by national policy changes.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_industries_in_respondent_states, payer,
    moderate, immediate, trapped, national).

% Academics and legal experts who analyze the DSB's jurisprudence and its impact on international law and state sovereignty. They provide critical commentary but do not directly participate in the dispute resolution process.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a neutral, binding, and enforceable mechanism for resolving trade disputes among WTO member states, ensuring the consistent application of multilateral trade rules and fostering a predictable global trading environment.
% TRANSFER_FUNCTION: Transfers policy discretion and potential economic benefits from member states found in violation of WTO agreements to the global trading system and successful complainant states, enforced through authorized trade retaliation if compliance is not achieved.
% ABSENT_VOICES: Domestic political factions within member states who prioritize national sovereignty and protectionist policies over international treaty obligations. They would argue against the DSB's authority to dictate domestic policy.
% DISAPPEARANCE_RATIONALE: If the DSB's binding authority vanished, the WTO's enforcement mechanism would collapse, leading to a return to unilateral trade measures, increased protectionism, and a significant erosion of the rules-based multilateral trading system, fundamentally reorganizing global trade relations.
% FOUNDING_PROBLEM: The lack of an effective, binding dispute resolution mechanism in international trade, leading to unilateral actions, retaliatory tariffs, and trade wars that destabilized the global economy and undermined multilateral cooperation.
% FOUNDING_PROBLEM_CORROBORATION: Many member states, particularly smaller economies, and international economic organizations (e.g., IMF, World Bank) consistently attest to the ongoing need for a binding dispute resolution system to ensure fair market access and prevent trade protectionism. Independent legal scholars also corroborate the historical problem and the DSB's role in addressing it.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the significant loss of policy discretion for member states, who must alter domestic laws or face trade sanctions. The high suppression (0.85) stems from the binding nature of rulings and the credible threat of authorized retaliation, which severely limits exit options for non-compliant states. The low theater ratio (0.1) indicates that the DSB's enforcement function is genuinely effective, with rulings typically leading to compliance or authorized countermeasures. The claimed type 'tangled_rope' acknowledges both the genuine coordination function (stable global trade) and the asymmetric extraction from states that lose disputes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states losing disputes, the DSB's authority can feel highly extractive and suppressive, forcing unwanted policy changes. However, from the perspective of the global trading system and successful complainants, this same authority is seen as essential for maintaining a fair and predictable rules-based order. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO DSB panels, as agenda-setters, benefit from the system's authority and legitimacy. Successful complainant states are direct beneficiaries, gaining market access or policy changes. Respondent states, particularly those found in violation, are clear targets, bearing the costs of compliance or retaliation. The global trading community benefits from the stability, but domestic industries within respondent states can be severely impacted. International law scholars act as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy from the 'binding referee' perspective. The founding problem of preventing trade wars and ensuring rules-based trade is considered 'live', and the DSB's binding authority is seen as actively addressing it. The high extractiveness and suppression are viewed as necessary costs for the coordination function, not as signs of an atrophied mandate. The system is actively enforced and its function is considered vital.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsb_mandate_scope_ambiguity,
    'Does the DSB''s authority genuinely derive from the explicit surrender of sovereignty in treaty law, or does it implicitly expand its mandate through interpretation?',
    'Detailed textual analysis of WTO agreements and negotiating history, combined with a comparative study of international dispute resolution bodies and their explicit mandates.',
    'If the DSB is found to have implicitly expanded its mandate, the ''binding referee'' reading''s legitimacy would be challenged, potentially shifting its classification towards a ''snare'' or ''tangled_rope'' with higher perceived extraction due to illegitimate authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dsb_mandate_scope_ambiguity, conceptual, 'Ambiguity regarding the precise scope and source of the DSB''s binding authority.').

omega_variable(
    compliance_vs_retaliation_efficacy,
    'To what extent does the threat of authorized retaliation genuinely compel compliance, versus merely providing a legal basis for trade wars?',
    'Empirical study of compliance rates following DSB rulings, correlation with authorized retaliation, and economic impact analysis of implemented retaliatory measures.',
    'If retaliation is found to be ineffective at compelling compliance and primarily serves as a tool for trade conflict, the ''suppression'' metric might be re-evaluated downward, and the ''extractiveness'' might be re-framed as a cost of conflict rather than a cost of compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_vs_retaliation_efficacy, empirical, 'The true efficacy of DSB-authorized retaliation in achieving compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__binding_referee_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__binding_referee_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__binding_referee_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__binding_referee_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__binding_referee_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(wto__tr_t2025, wto_dsb_authority__binding_referee_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2010, 0.81).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2020, 0.79).
narrative_ontology:measurement(wto__be_t2025, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2020, 0.84).
narrative_ontology:measurement(wto__su_t2025, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_trade_liberalization_commitments).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, international_investment_agreements).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'wto_dsb_authority' kernel, alongside 'advisory_coordination_reading' and 'judicial_activism_reading'. Each reading represents a distinct structural claim about the DSB's function and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
