% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Contingent Choice Reading of Bretton Woods Transition
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story represents the 'contingent choice' reading of the
 *   Bretton Woods system's collapse, specifically focusing on the Nixon Shock
 *   of 1971. This reading posits that the transition from fixed exchange
 *   rates to a floating system was primarily a policy decision by the United
 *   States, rather than an inevitable structural collapse. It emphasizes the
 *   agency of US policymakers in choosing to prioritize domestic economic
 *   goals over international monetary stability, despite other available
 *   options. The constraint here is the Bretton Woods system itself, which
 *   was actively dismantled by a policy choice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.6).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.7).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Contingent Choice Reading of Bretton Woods Transition").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '7ba5653c-1204-4f62-a22d-d0d927cc88dd').
narrative_ontology:cs_kernel_codification('7ba5653c-1204-4f62-a22d-d0d927cc88dd', formalized).
narrative_ontology:cs_authority_grounding('7ba5653c-1204-4f62-a22d-d0d927cc88dd', extraction).
narrative_ontology:cs_interpretation_layer_present('7ba5653c-1204-4f62-a22d-d0d927cc88dd').
narrative_ontology:cs_reading_relation('7ba5653c-1204-4f62-a22d-d0d927cc88dd', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('7ba5653c-1204-4f62-a22d-d0d927cc88dd', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('7ba5653c-1204-4f62-a22d-d0d927cc88dd', foundational, policy_autonomy_over_international_stability).
narrative_ontology:cs_axiom_status(policy_autonomy_over_international_stability, holdable).
narrative_ontology:cs_axiom_grounding('7ba5653c-1204-4f62-a22d-d0d927cc88dd', policy_autonomy_over_international_stability, instrumental).
narrative_ontology:cs_axiom('7ba5653c-1204-4f62-a22d-d0d927cc88dd', foundational, counterfactual_policy_viability).
narrative_ontology:cs_axiom_status(counterfactual_policy_viability, holdable).
narrative_ontology:cs_axiom_grounding('7ba5653c-1204-4f62-a22d-d0d927cc88dd', counterfactual_policy_viability, empirically_contingent).
narrative_ontology:cs_reference_frame('7ba5653c-1204-4f62-a22d-d0d927cc88dd', us_unilateral_policy_sovereignty).
narrative_ontology:cs_drift_state('7ba5653c-1204-4f62-a22d-d0d927cc88dd', post_bretton_woods_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7ba5653c-1204-4f62-a22d-d0d927cc88dd', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_policy_makers).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_treasury).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, international_creditors).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, developing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Made the unilateral decision to suspend dollar convertibility, gaining significant policy autonomy and removing the constraint of gold reserves on domestic spending. They framed this as a necessary response to speculative attacks, but this reading emphasizes the choice aspect.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_policy_makers, agenda_setter,
    institutional, generational, arbitrage, global).

% Held large dollar reserves that were suddenly devalued against gold, incurring losses. Their options were limited to accepting the new regime or facing economic instability, as the US dollar remained the primary reserve currency.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_creditors, payer,
    organized, biographical, constrained, global).

% Were disproportionately affected by the instability and inflation that followed the transition, as their economies were often pegged to the dollar and lacked the financial instruments to hedge against currency fluctuations. They had no voice in the decision.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, developing_nations, payer,
    powerless, generational, trapped, global).

% The institution designed to manage the Bretton Woods system, it was forced to adapt to the new floating exchange rate regime. Its role shifted from enforcing fixed parities to managing currency crises in a more volatile environment.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Benefited from the removal of the gold constraint, allowing for greater fiscal flexibility and the ability to finance domestic and international commitments without the pressure of maintaining dollar convertibility.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_treasury, beneficiary,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system coordinated international monetary policy around fixed exchange rates and dollar-gold convertibility, providing stability for trade and investment.
% TRANSFER_FUNCTION: The transition transferred the burden of maintaining international monetary stability from the US (via gold convertibility) to other nations (via floating exchange rates and dollar reserve accumulation), while transferring policy autonomy to the US.
% ABSENT_VOICES: Developing nations, who bore significant costs from the instability, were not at the table when the decision was made. Their interests were not represented in the unilateral US action.
% DISAPPEARANCE_RATIONALE: The Bretton Woods system, as a fixed exchange rate regime, effectively disappeared with the Nixon Shock. The world rearranged into a floating exchange rate system, but the underlying economic and political power dynamics that enabled the US to make the unilateral decision remained.
% FOUNDING_PROBLEM: The Bretton Woods system was established to prevent the currency wars and economic instability of the interwar period, providing a stable framework for post-WWII reconstruction and global trade.
% FOUNDING_PROBLEM_CORROBORATION: While the problem of global monetary instability remains, the specific problem of currency wars under a gold-backed fixed exchange rate system is dead. Historians and economists widely corroborate that the system's original function was superseded by new challenges and policy choices, leading to its demise. No external parties attest that the original problem is still live in its Bretton Woods form.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_unchanged).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Bretton Woods system, while initially a Rope, became a Tangled Rope as the US gained disproportionate benefits from its 'exorbitant privilege' (issuing the reserve currency) while other nations bore increasing costs. The extractiveness (0.6) reflects the growing imbalance by 1971, where the US could run deficits without immediate consequence, while other nations accumulated dollars. Suppression (0.7) was high because the system required other nations to maintain fixed parities against the dollar, limiting their monetary policy autonomy. The 'contingent choice' reading argues that this growing imbalance was a choice, not a necessity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of US policymakers, the decision was a necessary act of self-preservation, a 'rope' to cut free from an unsustainable burden. From the perspective of international creditors and developing nations, it was a unilateral act of extraction, a 'snare' that trapped them in a devalued currency regime. This reading emphasizes the latter, highlighting the choice and its extractive consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   US policy makers and the US Treasury were the primary beneficiaries, gaining policy autonomy and financing flexibility. International creditors (e.g., European central banks holding dollars) and developing nations were victims, bearing the costs of dollar devaluation and subsequent instability. The IMF, as an observer, adapted to the new reality but did not directly benefit or pay in the same way.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'contingent choice' reading suggests that the Bretton Woods system's mandate (global monetary stability) was not necessarily dead, but rather overridden by a policy choice prioritizing national interests. The system's coordination function was still viable, but the US chose to extract greater policy autonomy, leading to its collapse. This prevents mislabeling the collapse as purely structural (a Piton or Mountain) when agency was central.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_alternatives,
    'Were there viable policy alternatives for the US in 1971 that would have preserved Bretton Woods or led to a less disruptive transition?',
    'Historical counterfactual analysis by economic historians, examining declassified archives and policy debates from the period to assess the feasibility and likely outcomes of alternative policy paths (e.g., devaluation, capital controls, multilateral negotiation).',
    'If viable alternatives existed, it strengthens the ''contingent choice'' reading, emphasizing agency and the extractive nature of the chosen path. If no viable alternatives existed, it would lend credence to the ''overdetermined collapse'' reading, reducing the perceived extractiveness of the US action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_alternatives, empirical, 'Assessing the feasibility of alternative policy choices for the US in 1971.').

omega_variable(
    causal_weight_of_nixon_decision,
    'What was the precise causal weight of the Nixon Shock relative to underlying structural imbalances (e.g., Triffin Dilemma, US balance of payments deficits) in precipitating the collapse?',
    'Quantitative historical analysis and econometric modeling to disentangle the immediate impact of the policy decision from the long-term effects of structural factors. This would involve comparing the counterfactual of no Nixon Shock against the actual historical trajectory.',
    'A high causal weight for the Nixon decision supports this reading''s emphasis on contingent choice and policy-driven extraction. A low causal weight would shift the classification towards a more structurally determined outcome, potentially reducing the perceived agency and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_weight_of_nixon_decision, empirical, 'Disentangling agency vs. structure in the Bretton Woods collapse.').

omega_variable(
    framing_of_necessity_vs_choice,
    'To what extent was the ''necessity'' framing of the Nixon Shock a genuine belief among policymakers, versus a rhetorical strategy to justify a preferred policy outcome?',
    'Content analysis of internal policy documents, memoirs, and private communications from the period, combined with psychological and political science analysis of decision-making under pressure. This would seek to uncover the true motivations behind the public rhetoric.',
    'If the ''necessity'' framing was primarily rhetorical, it reinforces the ''contingent choice'' reading and highlights the strategic use of claims about inevitability to mask extractive policy decisions. If it was a genuine belief, it complicates the assessment of extractiveness by introducing cognitive constraints on policymakers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_necessity_vs_choice, conceptual, 'Distinguishing genuine belief in necessity from strategic rhetoric in policy decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__contingent_choice_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(tran_tr_t1955, transition_causality__contingent_choice_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__contingent_choice_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.2).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__contingent_choice_reading, base_extractiveness, 1944, 0.2).
narrative_ontology:measurement(tran_be_t1955, transition_causality__contingent_choice_reading, base_extractiveness, 1955, 0.3).
narrative_ontology:measurement(tran_be_t1965, transition_causality__contingent_choice_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(tran_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__contingent_choice_reading, suppression_requirement, 1944, 0.3).
narrative_ontology:measurement(tran_su_t1955, transition_causality__contingent_choice_reading, suppression_requirement, 1955, 0.4).
narrative_ontology:measurement(tran_su_t1965, transition_causality__contingent_choice_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(tran_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, floating_exchange_rate_system).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, dollar_hegemony_post_bretton_woods).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel, focusing on the contingent policy choice. It is linked to other readings (overdetermined_collapse_reading, hybrid_trigger_reading) that offer alternative causal explanations for the Bretton Woods transition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
