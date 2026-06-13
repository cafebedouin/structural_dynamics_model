% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility (Triffin's Dilemma Reading)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This constraint describes the dollar-gold convertibility under the
 *   Bretton Woods system, interpreted through the lens of Triffin's Dilemma.
 *   It posits that the system contained an inherent structural flaw: for the
 *   US dollar to serve as the global reserve currency, the US had to run
 *   persistent balance of payments deficits, which simultaneously undermined
 *   confidence in the dollar's convertibility to gold. This created an
 *   unavoidable tension that made the system inherently unstable and destined
 *   for collapse, extracting costs from both the US (loss of monetary policy
 *   autonomy) and creditor nations (uncertainty over dollar value).
 *
 * KEY AGENTS:
 *   - united_states_treasury: Primary victim (institutional/constrained) — faced the impossible trilemma.
 *   - creditor_nations_with_dollar_reserves: Primary victim (institutional/constrained) — faced uncertainty and potential losses.
 *   - international_monetary_fund: Agenda setter (institutional/constrained) — administered the system but could not resolve its fundamental flaw.
 *   - post_bretton_woods_floating_regime: Primary beneficiary (analytical/arbitrage) — emerged from the collapse, benefiting from the prior system's unsustainability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.85).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.75).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility (Triffin's Dilemma Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, 'da9873fd-52ca-4e01-bd51-ea2bb6188089').
narrative_ontology:cs_kernel_codification('da9873fd-52ca-4e01-bd51-ea2bb6188089', formalized).
narrative_ontology:cs_authority_grounding('da9873fd-52ca-4e01-bd51-ea2bb6188089', lineage).
narrative_ontology:cs_interpretation_layer_present('da9873fd-52ca-4e01-bd51-ea2bb6188089').
narrative_ontology:cs_reading_relation('da9873fd-52ca-4e01-bd51-ea2bb6188089', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('da9873fd-52ca-4e01-bd51-ea2bb6188089', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_axiom('da9873fd-52ca-4e01-bd51-ea2bb6188089', foundational, impossible_trilemma_structural_inevitability).
narrative_ontology:cs_axiom_status(impossible_trilemma_structural_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('da9873fd-52ca-4e01-bd51-ea2bb6188089', impossible_trilemma_structural_inevitability, empirically_contingent).
narrative_ontology:cs_axiom('da9873fd-52ca-4e01-bd51-ea2bb6188089', foundational, reserve_currency_dilemma_unresolvable).
narrative_ontology:cs_axiom_status(reserve_currency_dilemma_unresolvable, holdable).
narrative_ontology:cs_axiom_grounding('da9873fd-52ca-4e01-bd51-ea2bb6188089', reserve_currency_dilemma_unresolvable, empirically_contingent).
narrative_ontology:cs_reference_frame('da9873fd-52ca-4e01-bd51-ea2bb6188089', bretton_woods_design_flaw).
narrative_ontology:cs_drift_state('da9873fd-52ca-4e01-bd51-ea2bb6188089', post_1971_collapse, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('da9873fd-52ca-4e01-bd51-ea2bb6188089', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_with_dollar_reserves).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining dollar convertibility to gold while simultaneously running deficits to supply global liquidity. Faced the impossible trilemma of maintaining convertibility, domestic monetary policy autonomy, and fixed exchange rates.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury, payer,
    institutional, generational, constrained, global).

% Held large dollar reserves, which were essential for international trade but whose value was increasingly uncertain due to the growing US gold deficit. Faced the choice of demanding gold (triggering collapse) or holding depreciating dollars.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_with_dollar_reserves, payer,
    institutional, biographical, constrained, global).

% Administered the Bretton Woods system, including fixed exchange rates and convertibility. Its mandate was to maintain stability, but it lacked the structural power to resolve the inherent dilemma.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% The subsequent international monetary system of floating exchange rates, which emerged as a direct consequence of the collapse of dollar-gold convertibility. It 'benefited' by being the solution to the prior system's unsustainability.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime, beneficiary,
    analytical, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable international monetary system with fixed exchange rates and a reliable reserve currency (the dollar convertible to gold) for global trade and investment.
% TRANSFER_FUNCTION: Transferred the burden of maintaining global liquidity and exchange rate stability to the US (via deficits) and the risk of dollar devaluation to creditor nations (via holding reserves).
% ABSENT_VOICES: Advocates for a truly multilateral reserve asset (e.g., an expanded SDR role) or a return to a pure gold standard were present but marginalized by the existing institutional structure and the perceived necessity of the dollar's role. Their proposals would have offered alternative solutions to the dilemma.
% DISAPPEARANCE_RATIONALE: The collapse of dollar-gold convertibility in 1971 fundamentally reshaped the international monetary system, leading to floating exchange rates and a new era of global finance. The world did not merely 'stay the same' but underwent a significant structural rearrangement.
% FOUNDING_PROBLEM: The need for a stable international monetary system after World War II, avoiding the competitive devaluations and trade wars of the interwar period, and facilitating global reconstruction and trade.
% FOUNDING_PROBLEM_CORROBORATION: While the need for global monetary stability remains, the specific problem of post-WWII reconstruction and the design flaw of convertibility are widely considered 'dead' problems. Economic historians and international relations scholars (outside the direct beneficiaries of the Bretton Woods system) corroborate that the system's original mandate was superseded by its inherent contradictions, leading to its demise.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the system imposed unavoidable costs on both the US and creditor nations, leading to a systemic crisis. Suppression is high (0.75) because the fixed exchange rate regime and the dollar's reserve role severely constrained policy options for all participants; alternatives were suppressed by the very structure of the international monetary order. Theater ratio is low (0.1) because the system was genuinely functional for a time, but its inherent flaw meant that efforts to maintain convertibility became increasingly performative as the dilemma intensified. Accessibility collapse is high (0.9) as the structural flaw made any alternative within the convertibility framework impossible. Resistance is low (0.2) because the dilemma was a structural problem, not a policy choice that could be easily resisted by any single actor.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the US Treasury and creditor nations, the constraint was a Snare, trapping them in an unsustainable system. From the perspective of the post-Bretton Woods floating regime, the collapse of convertibility was a necessary, beneficial transition, making the prior system a 'beneficiary' of its own demise.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury and creditor nations are victims (d near 1.0) because they bore the direct costs of the dilemma. The IMF, as administrator, was constrained (d near 0.5) by the system's rules. The post-Bretton Woods floating regime is a beneficiary (d near 0.0) as its emergence was facilitated by the structural unsustainability of convertibility.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames convertibility as a system with an inherent design flaw, not a mandate that atrophied. The 'mandate' (stable exchange rates via gold convertibility) was structurally impossible to sustain in the long run. The classification as Snare reflects this inherent unsustainability and the unavoidable costs imposed on participants, rather than a simple decay of function. The 'mandatrophy resolved' flag would be true only after the system's collapse and the transition to a new regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_inevitability,
    'Was the collapse of dollar-gold convertibility truly an inherent structural inevitability, or could policy choices have sustained it longer?',
    'Counterfactual historical analysis comparing policy paths not taken with actual outcomes, focusing on the elasticity of demand for reserve currency vs. gold.',
    'If inevitable, the constraint was a Mountain-like Snare; if avoidable, it was a Tangled Rope sustained by policy inertia. This reading asserts inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, conceptual, 'The Triffin Dilemma as an inherent structural flaw vs. a policy-manageable challenge.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''triffin_structural_reading'' of the ''dollar_gold_convertibility'' kernel. How do other readings (strict_convertibility_reading, policy_flexible_reading) differ structurally?',
    'Analysis of historical documents, economic models, and policy debates from each reading''s proponents to identify their core premises and predicted outcomes.',
    'The strict_convertibility_reading would classify the constraint as a Rope (binding obligation for stability) with the US as the primary payer. The policy_flexible_reading would see it as a Scaffold (temporary support) with the US as agenda-setter, capable of adjusting convertibility to manage domestic goals. This reading emphasizes the systemic trap for all parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing the Triffin structural reading from strict convertibility and policy-flexible interpretations of dollar-gold convertibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1950, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t0, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(doll_tr_t5, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(doll_tr_t10, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(doll_tr_t15, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(doll_tr_t20, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(doll_be_t0, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(doll_be_t5, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(doll_be_t10, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(doll_be_t15, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(doll_be_t20, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t0, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(doll_su_t5, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(doll_su_t10, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(doll_su_t15, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(doll_su_t20, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, global_infrastructure).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel, focusing on its inherent structural unsustainability. It directly influences the emergence of the post-Bretton Woods floating exchange rate regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
