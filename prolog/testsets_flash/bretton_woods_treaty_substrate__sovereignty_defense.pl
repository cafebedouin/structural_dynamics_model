% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods: Sovereignty Defense Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story analyzes the Bretton Woods system through the
 *   'sovereignty defense' reading, focusing on how the system, while
 *   ostensibly providing monetary stability, imposed external discipline on
 *   non-reserve currency states to preserve national monetary sovereignty for
 *   the United States. The system's design, particularly the dollar's central
 *   role and its convertibility to gold, created an asymmetric structure
 *   where the U.S. benefited from 'exorbitant privilege' while other nations
 *   bore the costs of adjustment. This reading highlights the extractive
 *   nature of the system for many participants, despite its coordination
 *   function.
 *
 * KEY AGENTS:
 *   - united_states_government: Agenda setter (institutional/arbitrage) — benefits from dollar's status
 *   - non_reserve_currency_states: Payer (organized/constrained) — bears costs of external discipline
 *   - developing_economies: Payer (powerless/trapped) — most vulnerable to adjustment costs
 *   - international_monetary_fund: Agenda setter (institutional/constrained) — enforces system rules
 *   - reserve_currency_banks: Beneficiary (institutional/arbitrage) — profits from dollar-centric finance
 *   - gold_market_participants: Excluded (moderate/constrained) — limited role due to fixed gold price
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.65).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.75).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods: Sovereignty Defense Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, 'bbd14433-a9ce-4b62-bdcd-0b78815811b5').
narrative_ontology:cs_kernel_codification('bbd14433-a9ce-4b62-bdcd-0b78815811b5', formalized).
narrative_ontology:cs_authority_grounding('bbd14433-a9ce-4b62-bdcd-0b78815811b5', extraction).
narrative_ontology:cs_interpretation_layer_present('bbd14433-a9ce-4b62-bdcd-0b78815811b5').
narrative_ontology:cs_reading_relation('bbd14433-a9ce-4b62-bdcd-0b78815811b5', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, influences).
narrative_ontology:cs_reading_relation('bbd14433-a9ce-4b62-bdcd-0b78815811b5', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('bbd14433-a9ce-4b62-bdcd-0b78815811b5', foundational, national_monetary_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(national_monetary_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('bbd14433-a9ce-4b62-bdcd-0b78815811b5', national_monetary_sovereignty_is_paramount, conventional).
narrative_ontology:cs_axiom('bbd14433-a9ce-4b62-bdcd-0b78815811b5', foundational, reserve_currency_status_confers_privilege).
narrative_ontology:cs_axiom_status(reserve_currency_status_confers_privilege, holdable).
narrative_ontology:cs_axiom_grounding('bbd14433-a9ce-4b62-bdcd-0b78815811b5', reserve_currency_status_confers_privilege, empirically_contingent).
narrative_ontology:cs_reference_frame('bbd14433-a9ce-4b62-bdcd-0b78815811b5', post_war_monetary_stability_with_us_hegemony).
narrative_ontology:cs_drift_state('bbd14433-a9ce-4b62-bdcd-0b78815811b5', end_of_convertibility_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bbd14433-a9ce-4b62-bdcd-0b78815811b5', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_government).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the system, benefits from the dollar's reserve currency status (exorbitant privilege), and can run persistent balance of payments deficits without immediate consequence. Sets the terms of engagement for other states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Must maintain fixed exchange rates against the dollar, requiring external monetary discipline that often constrains domestic policy choices. They bear the costs of dollar-denominated debt and are vulnerable to U.S. monetary policy shifts.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    organized, biographical, constrained, national).

% Are particularly vulnerable to the system's demands for external discipline, often leading to austerity measures and limited policy space for development. Their access to international finance is tied to compliance with the dollar standard.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_economies, payer,
    powerless, generational, trapped, regional).

% Enforces the rules of the system, providing conditional loans to states facing balance of payments crises, which reinforces the external discipline on non-reserve currency states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the stability and liquidity provided by the dollar-centric system, facilitating international trade and finance. They profit from dollar-denominated transactions and lending.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_banks, beneficiary,
    institutional, generational, arbitrage, global).

% While gold nominally anchors the system, its role is increasingly symbolic for private actors. The fixed price of gold against the dollar is maintained by the U.S., limiting arbitrage opportunities and making gold a less dynamic asset for independent actors.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, gold_market_participants, excluded,
    moderate, immediate, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable international monetary system with fixed exchange rates (against the dollar, convertible to gold) to facilitate trade and prevent competitive devaluations, thereby coordinating global economic interactions.
% TRANSFER_FUNCTION: Transfers the costs of maintaining international monetary stability and external discipline from the United States (as the reserve currency issuer) to non-reserve currency states, who bear the burden of adjustment.
% ABSENT_VOICES: Developing economies and states seeking greater monetary autonomy are structurally marginalized; they would advocate for a more equitable system that does not privilege the dollar and allows for greater domestic policy flexibility, but their influence is limited by their economic and political power within the system.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods system (as interpreted through the sovereignty defense lens) vanished, the international monetary order would immediately destabilize. Non-reserve currency states would gain monetary autonomy but face increased exchange rate volatility, while the U.S. would lose its exorbitant privilege, leading to a fundamental reorganization of global finance and trade.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, trade wars, and financial instability, leading to a collapse of international trade and economic depression. The system aimed to prevent a return to this chaos by establishing a stable, rules-based monetary order.
% FOUNDING_PROBLEM_CORROBORATION: The U.S. government and international financial institutions attest that the problem of monetary instability remains live, justifying the system's continued influence. However, non-reserve currency states and critical economists argue that the original problem has been superseded by new forms of financial instability and dollar hegemony, with corroboration from historical economic data and independent analyses highlighting the asymmetric burdens of the system.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because the system's benefits were unevenly distributed, with the U.S. gaining significant advantages from its reserve currency status, while other states faced constraints on their monetary policy. Suppression (0.75) is high due to the active enforcement mechanisms of the IMF and the lack of viable alternatives for states needing access to international finance. The gold anchor, initially a coordination mechanism, became a snare for non-reserve currency states as the U.S. leveraged its position. Theater ratio (0.20) is low, indicating that the system's stated coordination functions were largely genuine, but increasingly served to mask the underlying extractive dynamics.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the United States, the system was a successful coordination mechanism that provided global stability and legitimate benefits. However, from the perspective of non-reserve currency states, particularly developing economies, the system was a source of significant extraction and suppression, limiting their policy autonomy and imposing disproportionate adjustment costs. The engine's per-seat classification should reflect this divergence, with the U.S. computing as a beneficiary of a Rope-like structure, while non-reserve currency states compute as victims of a Snare-like or Tangled Rope-like structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States government and reserve currency banks are clear beneficiaries (low directionality) due to the 'exorbitant privilege' of issuing the global reserve currency and the stability it provided for international finance. Non-reserve currency states and developing economies are targets (high directionality) as they bore the costs of maintaining fixed exchange rates and adjusting to external shocks. The IMF, while an enforcer, also serves the interests of the dominant powers, placing it closer to the beneficiary side.
 *
 * MANDATROPHY ANALYSIS:
 *   The Bretton Woods system, under this reading, did not suffer from mandatrophy in the sense of its function atrophying. Instead, its original mandate of global monetary stability evolved into a mechanism for asymmetric extraction, where the coordination function became a cover for the U.S.'s 'exorbitant privilege.' The system's persistence was not due to inertia but active enforcement of a structure that disproportionately benefited the U.S. and its financial institutions. This prevents mislabeling it as a Piton; it is a Tangled Rope where the coordination and extraction are deeply intertwined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gold_anchor_function,
    'Was the gold anchor a genuine stabilizer for all participants, or primarily a mechanism to legitimize dollar hegemony?',
    'Historical analysis of gold flows and U.S. monetary policy decisions, particularly during periods of dollar weakness, to determine if the U.S. consistently adhered to convertibility or manipulated the system.',
    'If primarily legitimizing dollar hegemony, the gold anchor''s coordination function is reduced, increasing the system''s effective extractiveness for non-U.S. states. If a genuine stabilizer, the system''s coordination aspect is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gold_anchor_function, empirical, 'Role of gold in the Bretton Woods system.').

omega_variable(
    asymmetric_adjustment_burden,
    'To what extent did the Bretton Woods system impose an asymmetric adjustment burden on non-reserve currency states compared to the United States?',
    'Quantitative economic analysis comparing balance of payments adjustment mechanisms and policy autonomy for the U.S. versus other major economies during the Bretton Woods era.',
    'Higher asymmetry strengthens the ''sovereignty defense'' reading, increasing the system''s computed extractiveness for non-U.S. states. Lower asymmetry would align more with a pure coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_adjustment_burden, empirical, 'Distribution of adjustment costs in the system.').

omega_variable(
    reading_framing_impact,
    'Does framing Bretton Woods as ''sovereignty defense'' obscure genuine coordination benefits for smaller states, or accurately highlight the underlying power dynamics?',
    'Comparative analysis with the ''keynesian_embedded_liberalism'' reading: if the latter''s metrics show significantly lower extraction for smaller states, this reading might overemphasize extraction. If not, this reading''s emphasis is justified.',
    'If this reading overemphasizes extraction, the constraint''s effective extractiveness might be slightly lower than computed. If it accurately highlights power dynamics, the classification as Tangled Rope is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Impact of the ''sovereignty defense'' framing on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(bret_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.2).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1958, 0.55).
narrative_ontology:measurement(bret_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.62).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1958, 0.65).
narrative_ontology:measurement(bret_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, dollar_hegemony_post_bretton_woods).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bretton Woods treaty substrate. It focuses on the asymmetric power dynamics and the defense of U.S. monetary sovereignty. Other readings emphasize different aspects of the system's coordination and extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
