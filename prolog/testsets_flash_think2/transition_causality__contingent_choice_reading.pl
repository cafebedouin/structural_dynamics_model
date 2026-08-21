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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Nixon Shock as Contingent Policy Choice
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story analyzes the 1971 Nixon Shock – the unilateral
 *   suspension of the dollar's convertibility to gold – from the 'contingent
 *   choice' reading of the 'transition_causality' kernel. This reading posits
 *   that the transition from the Bretton Woods system to floating exchange
 *   rates was primarily a policy decision by the U.S. government, which could
 *   have been avoided or managed differently, rather than an inevitable
 *   structural collapse. The decision allowed the U.S. to gain significant
 *   policy autonomy at the expense of other nations who lost the stability of
 *   the fixed exchange rate system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.78).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.85).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, snare).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Nixon Shock as Contingent Policy Choice").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, 'e8641e4d-4e02-4fbc-8c10-2903495e37ed').
narrative_ontology:cs_kernel_codification('e8641e4d-4e02-4fbc-8c10-2903495e37ed', formalized).
narrative_ontology:cs_authority_grounding('e8641e4d-4e02-4fbc-8c10-2903495e37ed', extraction).
narrative_ontology:cs_reading_relation('e8641e4d-4e02-4fbc-8c10-2903495e37ed', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('e8641e4d-4e02-4fbc-8c10-2903495e37ed', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('e8641e4d-4e02-4fbc-8c10-2903495e37ed', foundational, policy_choice_primary_causal_node).
narrative_ontology:cs_axiom_status(policy_choice_primary_causal_node, holdable).
narrative_ontology:cs_axiom_grounding('e8641e4d-4e02-4fbc-8c10-2903495e37ed', policy_choice_primary_causal_node, empirically_contingent).
narrative_ontology:cs_axiom('e8641e4d-4e02-4fbc-8c10-2903495e37ed', foundational, counterfactual_viability_high).
narrative_ontology:cs_axiom_status(counterfactual_viability_high, holdable).
narrative_ontology:cs_axiom_grounding('e8641e4d-4e02-4fbc-8c10-2903495e37ed', counterfactual_viability_high, empirically_contingent).
narrative_ontology:cs_reference_frame('e8641e4d-4e02-4fbc-8c10-2903495e37ed', us_policy_autonomy_maximization).
narrative_ontology:cs_drift_state('e8641e4d-4e02-4fbc-8c10-2903495e37ed', post_bretton_woods_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e8641e4d-4e02-4fbc-8c10-2903495e37ed', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, united_states_government).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, other_nations_with_dollar_reserves).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, fixed_exchange_rate_proponents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unilaterally decided to suspend dollar convertibility to gold, gaining significant policy autonomy and removing the constraint of its balance of payments deficit. It actively managed the transition to a floating exchange rate regime.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, united_states_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Held large dollar reserves that were suddenly inconvertible to gold, losing a key mechanism for international monetary stability and facing increased currency volatility. Their options were to accept the new regime or face economic disruption.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, other_nations_with_dollar_reserves, payer,
    institutional, biographical, constrained, global).

% The institution designed to manage the Bretton Woods system, its foundational role was undermined by the unilateral U.S. decision. It had to adapt to a new, less structured international monetary order.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_monetary_fund, excluded,
    institutional, generational, constrained, global).

% Advocated for the stability of the Bretton Woods system and its fixed exchange rates. They bore the cost of the system's collapse, losing their preferred monetary framework and facing new uncertainties.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, fixed_exchange_rate_proponents, payer,
    organized, biographical, constrained, global).

% Analyze the historical causes, counterfactuals, and long-term consequences of the transition from fixed to floating exchange rates, often debating the inevitability versus contingency of the event.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, analytical_economists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, united_states_government).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint (the unilateral suspension of dollar convertibility) effectively ended the Bretton Woods system's coordination of international monetary stability through fixed exchange rates and gold convertibility. It replaced it with a more flexible, but less formally coordinated, floating exchange rate regime.
% TRANSFER_FUNCTION: Transferred the burden of U.S. balance of payments deficits from the U.S. gold reserves to other nations' willingness to hold inconvertible dollars. It also transferred significant policy autonomy to the U.S. government.
% ABSENT_VOICES: Many nations, particularly those with large dollar holdings, were not consulted in the unilateral U.S. decision. They would have argued for a multilateral solution to the Bretton Woods crisis, potentially involving a revaluation of gold or a more orderly transition.
% DISAPPEARANCE_RATIONALE: If the Nixon Shock had not occurred, the global financial system would likely have either found a multilateral solution to preserve a fixed exchange rate system (perhaps with a revalued gold price) or transitioned to floating rates in a more coordinated, less abrupt manner. The current international monetary order is fundamentally shaped by this event.
% FOUNDING_PROBLEM: The U.S. faced a persistent balance of payments deficit, a dwindling gold reserve, and speculative attacks on the dollar, threatening its ability to maintain gold convertibility under the Bretton Woods system.
% FOUNDING_PROBLEM_CORROBORATION: Historians, economic archives (e.g., Federal Reserve, IMF), and contemporary government documents from various nations corroborate the U.S. gold drain and balance of payments issues leading up to 1971. While the specific problem is dead, its consequences persist.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.78) reflects the U.S. gaining policy freedom and offloading its balance of payments problem onto other nations. Suppression (0.85) is high due to the unilateral nature of the decision, which effectively forced other nations to accept the new monetary reality without negotiation. Theater ratio is low (0.10) because the Nixon Shock was a decisive, overt policy shift, not a performative maintenance of a failing system. Accessibility collapse (0.90) was severe for other nations, as the primary alternative (gold convertibility) was removed. Resistance (0.70) was significant internationally, but ultimately ineffective.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. government's perspective, the decision was a necessary, albeit difficult, choice to protect national interests and restore economic stability. From the perspective of other nations, it was a unilateral act of extraction, leveraging U.S. monetary power to externalize its domestic economic problems.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States government is the clear beneficiary, gaining policy autonomy and resolving its gold drain problem. Other nations with dollar reserves and proponents of fixed exchange rates are victims, bearing the costs of currency instability and loss of a predictable monetary anchor. The IMF, while adapting, was structurally excluded from the decision-making process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''contingent_choice_reading'' of the ''transition_causality'' kernel?',
    'Comparison with historical and economic literature that emphasizes policy agency and counterfactual viability in the 1971 decision, distinguishing it from structural inevitability arguments.',
    'If misclassified, the analysis of the transition''s causality would be skewed, potentially understating the role of policy choice and overstating structural forces. This reading''s classification as a Snare would be undermined if the decision was truly unavoidable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensures the story aligns with the specific kernel reading''s premises.').

omega_variable(
    counterfactual_viability_assessment,
    'Were there truly viable alternative policy choices for the U.S. in 1971 that could have avoided the unilateral suspension of gold convertibility?',
    'Detailed historical and economic counterfactual analysis, exploring the feasibility and political costs of alternative multilateral agreements, domestic austerity measures, or a more gradual transition.',
    'If viable alternatives were indeed high, it strengthens the ''contingent choice'' reading and its Snare classification. If alternatives were negligible, it would lend more credence to ''overdetermined_collapse_reading'' or ''hybrid_trigger_reading'', potentially shifting this constraint''s classification towards a more ''Mountain-like'' or ''Tangled Rope'' interpretation from the U.S. perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_assessment, empirical, 'Assesses the empirical basis for the claim of contingent choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1968, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1968, transition_causality__contingent_choice_reading, theater_ratio, 1968, 0.1).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.08).
narrative_ontology:measurement(tran_tr_t1974, transition_causality__contingent_choice_reading, theater_ratio, 1974, 0.1).
narrative_ontology:measurement(tran_tr_t1977, transition_causality__contingent_choice_reading, theater_ratio, 1977, 0.11).
narrative_ontology:measurement(tran_tr_t1980, transition_causality__contingent_choice_reading, theater_ratio, 1980, 0.1).

% Extraction over time
narrative_ontology:measurement(tran_be_t1968, transition_causality__contingent_choice_reading, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(tran_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.75).
narrative_ontology:measurement(tran_be_t1974, transition_causality__contingent_choice_reading, base_extractiveness, 1974, 0.78).
narrative_ontology:measurement(tran_be_t1977, transition_causality__contingent_choice_reading, base_extractiveness, 1977, 0.77).
narrative_ontology:measurement(tran_be_t1980, transition_causality__contingent_choice_reading, base_extractiveness, 1980, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1968, transition_causality__contingent_choice_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(tran_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.85).
narrative_ontology:measurement(tran_su_t1974, transition_causality__contingent_choice_reading, suppression_requirement, 1974, 0.83).
narrative_ontology:measurement(tran_su_t1977, transition_causality__contingent_choice_reading, suppression_requirement, 1977, 0.84).
narrative_ontology:measurement(tran_su_t1980, transition_causality__contingent_choice_reading, suppression_requirement, 1980, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, global_financial_instability).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, petrodollar_system).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel, focusing on the contingent policy choice aspect of the Nixon Shock. It is linked to sibling readings that emphasize structural inevitability or hybrid triggers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
