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
 *   This constraint instantiates the 'binding referee' reading of the WTO
 *   DSB's authority, which views its rulings as legally binding obligations
 *   for member states, grounded in treaty law. From this perspective, member
 *   states have genuinely surrendered policy discretion within WTO-covered
 *   domains, and non-compliance constitutes a treaty violation. This
 *   contrasts with readings that see the DSB as merely advisory or as
 *   engaging in judicial overreach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.75).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.8).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Referee Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, 'f29d5f83-7e46-44d4-9f11-879f9843c292').
narrative_ontology:cs_kernel_codification('f29d5f83-7e46-44d4-9f11-879f9843c292', fixed_text).
narrative_ontology:cs_authority_grounding('f29d5f83-7e46-44d4-9f11-879f9843c292', lineage).
narrative_ontology:cs_interpretation_layer_present('f29d5f83-7e46-44d4-9f11-879f9843c292').
narrative_ontology:cs_reading_relation('f29d5f83-7e46-44d4-9f11-879f9843c292', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('f29d5f83-7e46-44d4-9f11-879f9843c292', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('f29d5f83-7e46-44d4-9f11-879f9843c292', foundational, dsb_rulings_are_legally_binding).
narrative_ontology:cs_axiom_status(dsb_rulings_are_legally_binding, holdable).
narrative_ontology:cs_axiom_grounding('f29d5f83-7e46-44d4-9f11-879f9843c292', dsb_rulings_are_legally_binding, conventional).
narrative_ontology:cs_axiom('f29d5f83-7e46-44d4-9f11-879f9843c292', foundational, member_states_ceded_sovereignty_for_trade_benefits).
narrative_ontology:cs_axiom_status(member_states_ceded_sovereignty_for_trade_benefits, holdable).
narrative_ontology:cs_axiom_grounding('f29d5f83-7e46-44d4-9f11-879f9843c292', member_states_ceded_sovereignty_for_trade_benefits, conventional).
narrative_ontology:cs_reference_frame('f29d5f83-7e46-44d4-9f11-879f9843c292', rules_based_multilateralism).
narrative_ontology:cs_drift_state('f29d5f83-7e46-44d4-9f11-879f9843c292', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f29d5f83-7e46-44d4-9f11-879f9843c292', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_member_states_as_system).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, successful_complainant_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, unsuccessful_respondent_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_industries_in_respondent_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability and predictability of a rules-based global trading system, which the DSB's binding authority helps to maintain. While individual states may lose cases, the collective benefits from reduced unilateralism.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_member_states_as_system, beneficiary,
    institutional, generational, constrained, global).

% Successfully use the DSB to challenge trade barriers or unfair practices by other member states, gaining market access or policy adjustments in their favor. They benefit directly from the binding nature of the rulings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, successful_complainant_states, beneficiary,
    institutional, biographical, mobile, global).

% Are found to be in violation of WTO agreements and are obligated to change their policies or face authorized trade retaliation. They experience a direct loss of policy discretion and potential economic costs.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, unsuccessful_respondent_states, payer,
    institutional, biographical, constrained, global).

% Administers the DSB process, provides legal and technical support to panels, and facilitates the implementation of rulings. Its legitimacy and function are tied to the binding nature of the DSB's decisions.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Are directly impacted by policy changes mandated by DSB rulings, often facing increased competition or new regulatory burdens. They have little direct recourse within the WTO system once their state has lost a case.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_industries_in_respondent_states, payer,
    organized, biographical, trapped, national).

% Analyze the legal implications, effectiveness, and legitimacy of DSB rulings and their impact on international law and state sovereignty. They provide independent commentary on the system's operation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for resolving trade disputes between member states, ensuring predictable application of treaty law and preventing unilateral trade actions, thereby coordinating global trade relations.
% TRANSFER_FUNCTION: Transfers policy discretion and the right to set certain trade-related regulations from individual member states to the collective authority of the WTO DSB, backed by the threat of authorized trade retaliation.
% ABSENT_VOICES: Domestic political actors (e.g., specific industries, labor unions, environmental groups) in respondent states whose policy preferences are overridden by DSB rulings. Their interests are represented by their state, but their specific policy goals may be sacrificed for compliance.
% DISAPPEARANCE_RATIONALE: If the DSB's binding authority and enforcement mechanisms vanished overnight, trade disputes would likely escalate into unilateral protectionism and retaliatory measures, undermining the rules-based global trading system and leading to significant economic disruption and uncertainty for international trade.
% FOUNDING_PROBLEM: Unilateral trade protectionism, discriminatory trade practices, and retaliatory measures that destabilized global trade relations and hindered economic growth in the post-WWII era, leading to a need for a multilateral dispute settlement mechanism.
% FOUNDING_PROBLEM_CORROBORATION: International trade economists, most WTO member states (especially those who frequently use the system), and historical analyses of pre-WTO trade relations corroborate the problem's ongoing relevance and the need for a dispute settlement system, even if they contest the DSB's specific powers or interpretations.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the significant loss of policy discretion for member states when a DSB ruling goes against them, coupled with the economic costs of compliance or retaliation. Suppression (0.80) is high due to the binding nature of rulings and the credible threat of authorized trade sanctions, which actively enforce compliance. The theater ratio is low (0.10) because the DSB's function is genuinely operational and its rulings are largely implemented, even if sometimes with delay or resistance. The increasing extractiveness and suppression over time reflect the growing impact and assertiveness of DSB rulings as the system matured.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the global trading system and successful complainants, the DSB's binding authority is a necessary coordination mechanism for stable trade. However, from the perspective of unsuccessful respondent states, the same structure operates as a highly extractive mechanism that curtails their sovereign policy space. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO member states, as a collective system, and successful complainant states are beneficiaries (low directionality) as they gain from trade stability and market access. Unsuccessful respondent states and their domestic industries are targets (high directionality) as they bear the costs of compliance and loss of policy autonomy. The WTO Secretariat acts as the agenda-setter, administering the system that produces these outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of preventing unilateral trade protectionism remains live, and the DSB's binding authority is seen, from this reading, as a necessary mechanism to address it. Therefore, there is no mandatrophy from this perspective; the constraint's function is considered active and essential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsb_authority_binding_vs_advisory,
    'Is the DSB''s authority truly binding, or do member states retain ultimate policy discretion, making rulings effectively advisory?',
    'Analysis of state compliance rates, the frequency and impact of authorized retaliation, and the legal interpretations of treaty obligations by international courts or legal bodies.',
    'If rulings are found to be merely advisory, the constraint''s effective extractiveness and suppression would be significantly lower, reclassifying it closer to a Rope or even a Piton if compliance is theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dsb_authority_binding_vs_advisory, empirical, 'Whether DSB rulings are genuinely binding or merely persuasive.').

omega_variable(
    dsb_judicial_activism_legitimacy,
    'Does the DSB exceed its treaty mandate by creating new obligations through interpretive drift, thus engaging in illegitimate judicial legislation?',
    'Comparative analysis of DSB panel and Appellate Body reports against the original text and negotiating history of WTO agreements, and assessment of member state consensus on interpretive scope.',
    'If widespread judicial activism is confirmed and deemed illegitimate, the constraint''s authority grounding would shift towards ''extraction'' (maintaining power through overreach), and its legitimacy would be severely undermined, potentially reclassifying it as a Snare from the perspective of affected states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dsb_judicial_activism_legitimacy, conceptual, 'The legitimacy of DSB interpretations against claims of judicial overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(wto__tr_t6, wto_dsb_authority__binding_referee_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(wto__tr_t12, wto_dsb_authority__binding_referee_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(wto__tr_t18, wto_dsb_authority__binding_referee_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(wto__tr_t24, wto_dsb_authority__binding_referee_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__binding_referee_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(wto__be_t6, wto_dsb_authority__binding_referee_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(wto__be_t12, wto_dsb_authority__binding_referee_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(wto__be_t18, wto_dsb_authority__binding_referee_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(wto__be_t24, wto_dsb_authority__binding_referee_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__binding_referee_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(wto__su_t6, wto_dsb_authority__binding_referee_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(wto__su_t12, wto_dsb_authority__binding_referee_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(wto__su_t18, wto_dsb_authority__binding_referee_reading, suppression_requirement, 18, 0.77).
narrative_ontology:measurement(wto__su_t24, wto_dsb_authority__binding_referee_reading, suppression_requirement, 24, 0.79).
narrative_ontology:measurement(wto__su_t30, wto_dsb_authority__binding_referee_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, global_trade_liberalization).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, national_trade_policy_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'wto_dsb_authority' kernel, focusing on its binding, referee-like function. Sibling readings include 'advisory_coordination_reading' and 'judicial_activism_reading', which offer alternative interpretations of the DSB's power and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
