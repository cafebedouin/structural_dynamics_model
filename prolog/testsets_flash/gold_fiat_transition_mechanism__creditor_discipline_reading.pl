% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Post-Gold Standard Creditor Discipline Constraint (Creditor Discipline Reading)
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint describes the shift in international monetary power
 *   dynamics following the abandonment of the gold standard, specifically
 *   from the perspective of creditor nations losing their disciplinary
 *   leverage. Under the gold standard, creditor nations could demand gold
 *   redemption for balance-of-payments surpluses, effectively vetoing debtor
 *   nations' expansionary fiscal policies. The transition to a fiat,
 *   reserve-currency-dominated system eliminated this veto power,
 *   transferring fiscal flexibility to debtor nations (especially the reserve
 *   currency issuer) and imposing a new form of discipline on non-reserve
 *   holders.
 *
 * KEY AGENTS:
 *   - creditor_nations: Primary victim (institutional/constrained) — lost leverage
 *   - debtor_nations: Primary beneficiary (institutional/mobile) — gained fiscal flexibility
 *   - reserve_currency_issuer: Primary beneficiary (institutional/arbitrage) — gained seigniorage and policy autonomy
 *   - non_reserve_currency_issuers: Secondary victim (institutional/constrained) — faced new forms of discipline
 *   - international_monetary_fund: Agenda setter (institutional/analytical) — administered the Bretton Woods system and its transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.85).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.9).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, snare).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Post-Gold Standard Creditor Discipline Constraint (Creditor Discipline Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2').
narrative_ontology:cs_kernel_codification('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', formalized).
narrative_ontology:cs_authority_grounding('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', extraction).
narrative_ontology:cs_interpretation_layer_present('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2').
narrative_ontology:cs_reading_relation('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', foundational, creditor_leverage_is_discipline).
narrative_ontology:cs_axiom_status(creditor_leverage_is_discipline, holdable).
narrative_ontology:cs_axiom_grounding('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', creditor_leverage_is_discipline, conventional).
narrative_ontology:cs_axiom('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', foundational, fiscal_flexibility_is_sovereignty).
narrative_ontology:cs_axiom_status(fiscal_flexibility_is_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', fiscal_flexibility_is_sovereignty, deontological).
narrative_ontology:cs_reference_frame('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', gold_standard_creditor_hegemony).
narrative_ontology:cs_drift_state('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', post_bretton_woods_collapse, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('877d0c9d-4a4f-4bd1-ab38-e7cb63e00ac2', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_currency_issuers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a stable international monetary system that facilitates trade and investment, replacing the gold standard's inherent deflationary bias and susceptibility to speculative attacks.
% TRANSFER_FUNCTION: Transfers the power to create international liquidity and manage balance-of-payments adjustments from creditor nations (via gold redemption) to the reserve currency issuer and debtor nations (via fiscal and monetary policy).
% ABSENT_VOICES: Advocates for a return to a gold-backed standard or a truly neutral international reserve asset (e.g., a global central bank currency) are largely excluded from mainstream policy debates, as their proposals challenge the fundamental structure of the current fiat system.
% DISAPPEARANCE_RATIONALE: If the post-gold standard creditor discipline vanished overnight (e.g., if gold suddenly became the sole international reserve again), the global financial system would undergo a massive, disruptive reorganization. Debtor nations would face immediate fiscal constraints, and the reserve currency issuer would lose its seigniorage benefits, leading to a complete reordering of international power dynamics.
% FOUNDING_PROBLEM: The gold standard imposed rigid constraints on domestic monetary policy, exacerbated economic downturns, and created an unstable international system prone to balance-of-payments crises and speculative attacks.
% FOUNDING_PROBLEM_CORROBORATION: Economists and central bankers widely corroborate the problems of the gold standard, citing historical evidence of its deflationary bias and crisis proneness. However, the 'live' status of the problem is contested by some who argue that the current fiat system has introduced new forms of instability and moral hazard, suggesting the 'solution' created new problems.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because creditor nations lost a significant tool for influencing international economic policy, effectively subsidizing debtor nations' fiscal autonomy. Suppression is high because the new system structurally prevents creditor nations from reasserting their former leverage; there is no 'exit' back to gold-backed discipline. Theater ratio is low as the constraint's operation is direct and functional, not performative. The increasing extractiveness and suppression over the interval reflect the gradual erosion of gold's influence and the hardening of the fiat system's disciplinary mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of creditor nations, this constraint is a Snare, as they lost a powerful mechanism for enforcing fiscal discipline. From the perspective of debtor nations, it is a Rope, as it enabled greater fiscal flexibility and autonomy. The reserve currency issuer experiences it as an arbitrage opportunity, gaining significant seigniorage and policy space.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve_currency_issuer and debtor_nations are beneficiaries (d near 0.0-0.2) as they gained fiscal flexibility and seigniorage. Creditor_nations and non_reserve_currency_issuers are victims (d near 0.8-1.0) as they lost leverage and faced new forms of discipline. The international_monetary_fund, as an agenda setter, sits closer to symmetric (d around 0.5), administering the system for all members, albeit with an inherent bias towards the dominant powers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not about mandatrophy; rather, it describes a fundamental shift in the underlying mechanism of international monetary discipline. The 'mandate' of international monetary stability persisted, but the means of achieving it, and the distribution of power within that system, fundamentally changed. The classification as a Snare for creditor nations prevents mislabeling this power shift as a neutral 'coordination' mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily about the elimination of creditor discipline, or is it better understood as an automatic physical constraint or a composite overdetermined event?',
    'Historical analysis of policy debates and diplomatic records focusing on the explicit motivations and perceived impacts of the gold standard''s abandonment on international power dynamics.',
    'If this ''creditor discipline'' reading is primary, the constraint is a Snare for creditor nations. If the ''automatic constraint'' reading is primary, it''s a Mountain. If ''composite overdetermination'' is primary, the constraint itself is a complex network of smaller constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''gold_fiat_transition_mechanism'' kernel, specifically the ''creditor_discipline_reading''.').

omega_variable(
    creditor_leverage_mechanism,
    'To what extent did the threat of gold redemption genuinely constrain debtor nations'' fiscal policy, versus serving as a symbolic justification for existing power imbalances?',
    'Counterfactual historical analysis comparing fiscal policy outcomes in periods of high vs. low gold redemption threat, controlling for other geopolitical factors.',
    'If the threat was a strong causal mechanism, the extractiveness and suppression metrics are accurate. If it was largely symbolic, the constraint''s true extractiveness might be lower, and its ''snare'' classification less robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_leverage_mechanism, empirical, 'Assessing the actual leverage of creditor nations under the gold standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1944, 1974).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gold_tr_t10, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(gold_tr_t20, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gold_tr_t30, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gold_be_t10, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(gold_be_t20, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(gold_be_t30, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gold_su_t10, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(gold_su_t20, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(gold_su_t30, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gold_fiat_transition_mechanism' kernel. Each reading offers a distinct structural interpretation of the transition's primary mechanism and impact, leading to different classifications and stakeholder dynamics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
