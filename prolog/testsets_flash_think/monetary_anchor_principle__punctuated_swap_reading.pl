% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Monetary Anchor Principle: Punctuated Swap Reading (Nixon Shock)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story represents the 'punctuated swap' reading of the
 *   monetary anchor principle, focusing on the August 15, 1971, Nixon Shock
 *   as a discrete, unilateral institutional choice. This reading emphasizes
 *   the agency of the U.S. government in transitioning from a fixed exchange
 *   rate system (Bretton Woods) to a floating one, rather than viewing it as
 *   an inevitable outcome of structural forces. The constraint is classified
 *   as a Rope because, despite the unilateral nature of the swap, it aimed to
 *   re-establish a form of international monetary coordination, albeit one
 *   with significant distributional consequences.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.45).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.6).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Monetary Anchor Principle: Punctuated Swap Reading (Nixon Shock)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, 'c607314a-7fe2-428e-a08e-bfbf176a9b16').
narrative_ontology:cs_kernel_codification('c607314a-7fe2-428e-a08e-bfbf176a9b16', formalized).
narrative_ontology:cs_authority_grounding('c607314a-7fe2-428e-a08e-bfbf176a9b16', lineage).
narrative_ontology:cs_interpretation_layer_present('c607314a-7fe2-428e-a08e-bfbf176a9b16').
narrative_ontology:cs_reading_relation('c607314a-7fe2-428e-a08e-bfbf176a9b16', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('c607314a-7fe2-428e-a08e-bfbf176a9b16', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('c607314a-7fe2-428e-a08e-bfbf176a9b16', foundational, monetary_regime_is_institutional_choice).
narrative_ontology:cs_axiom_status(monetary_regime_is_institutional_choice, holdable).
narrative_ontology:cs_axiom_grounding('c607314a-7fe2-428e-a08e-bfbf176a9b16', monetary_regime_is_institutional_choice, conventional).
narrative_ontology:cs_axiom('c607314a-7fe2-428e-a08e-bfbf176a9b16', foundational, sovereign_monetary_autonomy_is_primary).
narrative_ontology:cs_axiom_status(sovereign_monetary_autonomy_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('c607314a-7fe2-428e-a08e-bfbf176a9b16', sovereign_monetary_autonomy_is_primary, instrumental).
narrative_ontology:cs_reference_frame('c607314a-7fe2-428e-a08e-bfbf176a9b16', fixed_exchange_rate_system).
narrative_ontology:cs_drift_state('c607314a-7fe2-428e-a08e-bfbf176a9b16', post_nixon_shock, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c607314a-7fe2-428e-a08e-bfbf176a9b16', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_government).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_taxpayers).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, global_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unilaterally decided to suspend the convertibility of the U.S. dollar to gold, effectively ending the Bretton Woods system. Benefited from increased fiscal and monetary policy autonomy, no longer constrained by gold reserves.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Held significant dollar reserves that were suddenly devalued against gold, representing a loss of wealth and a shift in the international monetary system. Had limited immediate recourse to prevent the change.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks, payer,
    institutional, biographical, constrained, global).

% The institution designed to manage the Bretton Woods system. Its role shifted dramatically from managing fixed exchange rates to overseeing a floating rate system, adapting to the new reality rather than dictating it.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_fund, observer,
    institutional, generational, constrained, global).

% Adapted quickly to the new floating exchange rate regime, finding new opportunities for currency speculation and capital mobility, benefiting from the increased flexibility and reduced constraints on capital flows.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, global_investors, beneficiary,
    powerful, immediate, mobile, global).

% Benefited indirectly from the U.S. government's increased fiscal autonomy, which allowed for greater domestic spending without the constraint of gold outflows. Also experienced some inflationary pressures.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_taxpayers, beneficiary,
    moderate, biographical, constrained, national).

% Individuals and entities outside the U.S. holding dollar assets experienced an immediate loss of purchasing power as the dollar devalued against other currencies and gold. Had no mechanism to prevent or reverse this.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a new international monetary coordination mechanism after the collapse of the Bretton Woods fixed exchange rate system, allowing for flexible exchange rates to absorb economic shocks.
% TRANSFER_FUNCTION: Transferred the burden of U.S. balance of payments deficits from the U.S. gold reserves to foreign dollar holders via devaluation, while transferring greater monetary policy autonomy to the U.S. government.
% ABSENT_VOICES: Developing nations with substantial dollar reserves had little to no voice in the unilateral decision, bearing the costs of devaluation without input into the new regime's design.
% DISAPPEARANCE_RATIONALE: If the principle of a floating, market-determined monetary anchor (or any recognized anchor) vanished, the global financial system would descend into extreme volatility and fragmentation, severely disrupting international trade and investment.
% FOUNDING_PROBLEM: The Bretton Woods system, designed for post-WWII stability, became unsustainable due to the Triffin dilemma and increasing U.S. deficits, leading to a crisis of confidence in the dollar's gold convertibility.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians, international relations scholars, and central bank archives corroborate the pressures on the Bretton Woods system and the U.S. decision, with independent analyses from institutions like the BIS and academic economists.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).
:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is moderate (0.45) because the U.S. gained significant fiscal autonomy, effectively expropriating foreign dollar holders through devaluation, but the new system also offered some benefits of flexibility. Suppression is moderate (0.60) as the U.S. unilaterally imposed the change, leaving other nations with limited immediate alternatives, though they eventually adapted to the new floating regime. Theater ratio is low (0.10) because the action was a direct, functional policy change with immediate, tangible effects, not primarily performative. Accessibility collapse is high (0.70) because the fundamental rules of international monetary exchange were abruptly altered, making the previous system's alternatives (e.g., demanding gold) largely inaccessible. Resistance is moderate (0.50) reflecting international outcry and diplomatic pressure, but ultimately an inability to reverse the U.S. decision.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. government's perspective, this was a necessary and decisive act to restore national economic sovereignty. From the perspective of foreign central banks, it was a unilateral defection that imposed significant costs. The engine's classification will reflect this divergence based on the declared beneficiary/victim structure and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. government and taxpayers are beneficiaries, gaining fiscal autonomy and relief from gold outflow constraints. Foreign central banks and dollar holders are victims, bearing the costs of dollar devaluation. The IMF, while adapting, was structurally constrained by the unilateral action. Global investors, while initially facing uncertainty, quickly adapted to and benefited from the new flexible environment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discrete_choice_vs_inevitability,
    'Was the transition a discrete institutional choice, or an overdetermined outcome of structural pressures?',
    'Counterfactual historical analysis: could alternative policy choices by the U.S. or other nations have sustained the Bretton Woods system longer, or altered the nature of its collapse?',
    'If overdetermined, the ''punctuated_swap_reading'' overstates agency, and the constraint''s extractiveness might be re-evaluated as a consequence of systemic forces rather than a deliberate transfer. If a discrete choice, the U.S. government''s responsibility for the distributional consequences is amplified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discrete_choice_vs_inevitability, conceptual, 'Ambiguity between agency and structural inevitability in the monetary regime shift.').

omega_variable(
    devaluation_as_expropriation_vs_adjustment,
    'Was the devaluation of the dollar against gold an act of expropriation against foreign dollar holders, or a necessary market adjustment to restore equilibrium?',
    'Economic analysis of the U.S. balance of payments and foreign reserve accumulation leading up to 1971, compared with the immediate impact on foreign economies and subsequent policy responses.',
    'If primarily expropriation, the extractiveness metric is robust. If primarily adjustment, the extractiveness might be seen as a ''cost of doing business'' in a global reserve system, potentially lowering the effective extraction for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devaluation_as_expropriation_vs_adjustment, empirical, 'Nature of the wealth transfer from dollar devaluation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1971, 1981).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(mone_tr_t1973, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1973, 0.09).
narrative_ontology:measurement(mone_tr_t1975, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement(mone_tr_t1977, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1977, 0.09).
narrative_ontology:measurement(mone_tr_t1979, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1979, 0.1).
narrative_ontology:measurement(mone_tr_t1981, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1981, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.4).
narrative_ontology:measurement(mone_be_t1973, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1973, 0.45).
narrative_ontology:measurement(mone_be_t1975, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(mone_be_t1977, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1977, 0.46).
narrative_ontology:measurement(mone_be_t1979, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1979, 0.44).
narrative_ontology:measurement(mone_be_t1981, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1981, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.55).
narrative_ontology:measurement(mone_su_t1973, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1973, 0.6).
narrative_ontology:measurement(mone_su_t1975, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(mone_su_t1977, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement(mone_su_t1979, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1979, 0.58).
narrative_ontology:measurement(mone_su_t1981, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1981, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, global_capital_mobility_regime).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, us_dollar_reserve_status).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monetary_anchor_principle' kernel, focusing on the Nixon Shock as a discrete institutional choice. It is linked to the 'overdetermined_composite_reading' and 'triffin_inevitability_reading' which offer alternative explanations for the transition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
