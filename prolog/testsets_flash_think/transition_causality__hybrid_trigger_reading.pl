% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Collapse: Hybrid Trigger Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story describes the collapse of the Bretton Woods
 *   international monetary system, specifically through the 'hybrid trigger'
 *   reading of transition causality. This reading posits that while
 *   structural contradictions (primarily the Triffin Dilemma, where the
 *   dollar's role as both national and reserve currency created an inherent
 *   instability) accumulated over decades, specific contingent events like
 *   the Vietnam War's fiscal shock and French gold demands were necessary
 *   triggers that actualized the system's collapse in 1971. The system
 *   functioned as a Tangled Rope, providing coordination for global trade but
 *   with increasing asymmetric extraction and requiring active enforcement to
 *   maintain its increasingly fragile structure.
 *
 * KEY AGENTS:
 *   - us_government: Primary agenda setter and beneficiary (institutional/arbitrage)
 *   - other_reserve_currency_nations: Primary payers/victims (powerful/constrained)
 *   - international_monetary_fund: Secondary agenda setter/enforcer (institutional/analytical)
 *   - global_trade_and_finance_sectors: Beneficiaries (organized/mobile)
 *   - developing_nations: Secondary payers/victims (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.68).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.75).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Collapse: Hybrid Trigger Reading").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '7bbfa5e5-a9ef-478f-9cb3-26781b328e48').
narrative_ontology:cs_kernel_codification('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', formalized).
narrative_ontology:cs_authority_grounding('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', lineage).
narrative_ontology:cs_interpretation_layer_present('7bbfa5e5-a9ef-478f-9cb3-26781b328e48').
narrative_ontology:cs_reading_relation('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', foundational, structural_contradictions_accumulate).
narrative_ontology:cs_axiom_status(structural_contradictions_accumulate, holdable).
narrative_ontology:cs_axiom_grounding('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', structural_contradictions_accumulate, empirically_contingent).
narrative_ontology:cs_axiom('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', foundational, contingent_triggers_actualize_collapse).
narrative_ontology:cs_axiom_status(contingent_triggers_actualize_collapse, holdable).
narrative_ontology:cs_axiom_grounding('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', contingent_triggers_actualize_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', post_wwii_stability_framework).
narrative_ontology:cs_drift_state('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', post_vietnam_fiscal_shock, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7bbfa5e5-a9ef-478f-9cb3-26781b328e48', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_government).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, global_trade_and_finance_sectors).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, other_reserve_currency_nations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, developing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the world's primary reserve currency, the US government benefited from seigniorage and the ability to finance deficits without immediate market discipline. It actively enforced the fixed exchange rate system but also contributed to its structural strain through fiscal policy. Its exit option was to unilaterally abandon gold convertibility, which it eventually did.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Nations like France and Germany accumulated large dollar reserves, which were theoretically convertible to gold. They bore the cost of US inflation and the Triffin Dilemma, where the dollar's role as reserve currency conflicted with its role as a national currency. Their exit option was to demand gold, risking the collapse of the system they also benefited from.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, other_reserve_currency_nations, payer,
    powerful, biographical, constrained, global).

% The IMF was established to administer the Bretton Woods system, providing short-term liquidity and overseeing exchange rate stability. It acted as an institutional enforcer and arbiter, attempting to manage the growing imbalances and structural contradictions.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).

% These sectors benefited from the stability and predictability of fixed exchange rates, which facilitated international trade and investment. While they eventually adapted to floating rates, the initial system provided a clear framework for their operations.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, global_trade_and_finance_sectors, beneficiary,
    organized, biographical, mobile, global).

% These nations had little influence over the design or operation of the Bretton Woods system. They were often subject to the economic policies of the reserve currency nations and bore the costs of global inflation or instability without the means to influence policy or exit the system effectively.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, developing_nations, payer,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, us_government).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish and maintain a stable international monetary system with fixed exchange rates, preventing competitive devaluations and facilitating global trade after World War II.
% TRANSFER_FUNCTION: Transferred seigniorage benefits and the ability to run persistent balance of payments deficits to the United States, while providing exchange rate stability and global liquidity to other nations, who in turn bore the risk of dollar overvaluation and inflation.
% ABSENT_VOICES: Nations whose economic development was constrained by the fixed exchange rate regime and dollar dominance, particularly those in the Global South, had limited voice in the system's governance or its eventual reform.
% DISAPPEARANCE_RATIONALE: The collapse of Bretton Woods led to a fundamental shift in the international monetary system, moving to floating exchange rates and a more complex, multi-polar financial landscape. Global trade and finance had to adapt to new volatility and risk management strategies.
% FOUNDING_PROBLEM: The post-World War II international economic order was characterized by instability, competitive currency devaluations, and a lack of global liquidity, hindering reconstruction and trade.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians, international relations scholars, and former policymakers (e.g., from the Group of Ten nations) widely corroborate the initial problem and the subsequent emergence of the Triffin Dilemma, supporting the view that the original problem was superseded by new structural issues.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness increased over time as the Triffin Dilemma intensified, forcing other nations to hold more dollars than they desired, effectively financing US deficits. Suppression was high because alternatives to the dollar standard were actively resisted or simply not viable for most nations. Theater ratio rose as the US government and IMF increasingly engaged in performative measures (e.g., gold pool, special drawing rights) to maintain confidence in a system whose underlying contradictions were growing. Resistance from nations like France, demanding gold, highlights the active contestation. Accessibility collapse was high because no ready alternative global monetary system existed.
 *
 * PERSPECTIVAL GAP:
 *   The US government, as the primary beneficiary, viewed the system as a necessary global public good, downplaying its extractive aspects. Other reserve currency nations, particularly those accumulating large dollar reserves, increasingly saw it as an extractive mechanism that forced them to bear the costs of US fiscal policy. The engine's computation of per-seat classification will reflect this divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The Bretton Woods system's original mandate was to provide global monetary stability. However, the Triffin Dilemma meant that fulfilling the global liquidity function (issuing more dollars) undermined the stability function (maintaining dollar-gold convertibility). The system's function drifted from pure coordination to one that increasingly served US seigniorage benefits, leading to a state where its original mandate was compromised by its operational structure. The contingent triggers merely exposed and actualized this accumulated mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_trigger_vs_alternatives,
    'What is the precise weighting of structural contradictions versus contingent triggers in causing the Bretton Woods collapse, compared to readings emphasizing pure policy choice or pure structural inevitability?',
    'Detailed counterfactual historical analysis, economic modeling of alternative policy paths, and comparative studies of other monetary regime transitions.',
    'If structural factors are found to be overwhelmingly dominant, the ''overdetermined_collapse_reading'' gains strength. If specific policy choices could have averted collapse, the ''contingent_choice_reading'' is strengthened. This reading''s validity depends on demonstrating a necessary, but not sufficient, role for contingent triggers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_trigger_vs_alternatives, conceptual, 'Ambiguity in the causal weight of structural vs. contingent factors in system collapse.').

omega_variable(
    counterfactual_trigger_timing,
    'If the specific trigger events (Vietnam War fiscal shock, French gold demands) had not occurred or had been delayed, would the Bretton Woods system have collapsed anyway, and if so, when and how?',
    'Historical simulations and expert consensus on the ''point of no return'' for the system''s structural integrity, independent of specific triggers.',
    'If the system was near collapse regardless of triggers, it strengthens the ''overdetermined_collapse_reading''. If different triggers would have led to a different outcome or a much later collapse, it reinforces the ''hybrid_trigger_reading''s emphasis on contingency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_trigger_timing, empirical, 'Viability of counterfactual scenarios with different trigger timing for system collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1950, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1950, transition_causality__hybrid_trigger_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(tran_tr_t1955, transition_causality__hybrid_trigger_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(tran_tr_t1960, transition_causality__hybrid_trigger_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.4).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.45).

% Extraction over time
narrative_ontology:measurement(tran_be_t1950, transition_causality__hybrid_trigger_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(tran_be_t1955, transition_causality__hybrid_trigger_reading, base_extractiveness, 1955, 0.5).
narrative_ontology:measurement(tran_be_t1960, transition_causality__hybrid_trigger_reading, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.66).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1950, transition_causality__hybrid_trigger_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(tran_su_t1955, transition_causality__hybrid_trigger_reading, suppression_requirement, 1955, 0.65).
narrative_ontology:measurement(tran_su_t1960, transition_causality__hybrid_trigger_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.72).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.74).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel, focusing on the hybrid role of structural contradictions and contingent triggers in the Bretton Woods collapse. Sibling readings explore pure policy choice and pure structural inevitability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
