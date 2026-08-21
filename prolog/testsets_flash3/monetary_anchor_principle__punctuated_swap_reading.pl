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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Monetary Anchor Principle: Punctuated Swap Reading (1971 Nixon Shock)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story represents the 'punctuated swap' reading of the
 *   1971 Nixon Shock, where the suspension of dollar-gold convertibility is
 *   viewed as a discrete, unilateral institutional choice rather than an
 *   inevitable outcome of structural forces. It frames the event as a
 *   coordination failure leading to a defection, with clear beneficiaries
 *   (U.S. fiscal autonomy) and victims (foreign dollar holders). The
 *   constraint is claimed as a Rope because, while extractive for some, it
 *   established a new, albeit less formal, coordination mechanism for
 *   international finance.
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
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Monetary Anchor Principle: Punctuated Swap Reading (1971 Nixon Shock)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, 'db1797cc-a6c1-4af2-b6b1-a7b45720c7a0').
narrative_ontology:cs_kernel_codification('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', formalized).
narrative_ontology:cs_authority_grounding('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', lineage).
narrative_ontology:cs_reading_relation('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', foundational, monetary_regime_is_institutional_choice).
narrative_ontology:cs_axiom_status(monetary_regime_is_institutional_choice, holdable).
narrative_ontology:cs_axiom_grounding('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', monetary_regime_is_institutional_choice, conventional).
narrative_ontology:cs_axiom('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', secondary, unilateral_action_can_redefine_global_rules).
narrative_ontology:cs_axiom_status(unilateral_action_can_redefine_global_rules, holdable).
narrative_ontology:cs_axiom_grounding('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', unilateral_action_can_redefine_global_rules, conventional).
narrative_ontology:cs_reference_frame('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', bretton_woods_fixed_exchange_rate_system).
narrative_ontology:cs_drift_state('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', post_nixon_shock, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('db1797cc-a6c1-4af2-b6b1-a7b45720c7a0', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authorities).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, global_trade_partners).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, global_trade_partners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant fiscal autonomy by removing the gold convertibility constraint, allowing for independent monetary policy and deficit financing without gold outflows. They made the unilateral decision to suspend convertibility.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Experienced an effective expropriation via devaluation of their dollar reserves, which were no longer convertible to gold at a fixed rate. Their options were to accept the devalued dollars or retaliate economically, which was costly.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    organized, immediate, constrained, global).

% The institutional body designed to manage the Bretton Woods system. Its role shifted from enforcing fixed exchange rates to managing a floating rate system, observing the unilateral action and its consequences.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Benefited from continued global trade and liquidity, but also faced increased exchange rate volatility and the need to manage their own currency's relationship to the floating dollar. Their trade balances were directly affected by the new regime.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, global_trade_partners, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, global_trade_partners, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a new, albeit less stable, framework for international monetary coordination after the collapse of the fixed exchange rate system, allowing for continued global trade and capital flows.
% TRANSFER_FUNCTION: Transferred the burden of maintaining dollar value from the U.S. gold reserves to foreign central banks and private holders of dollars, effectively devaluing their assets and granting the U.S. greater monetary policy flexibility.
% ABSENT_VOICES: Smaller nations heavily reliant on dollar reserves, who had less leverage to negotiate or retaliate, were effectively forced to accept the new reality. Their concerns about reserve value were not central to the unilateral decision.
% DISAPPEARANCE_RATIONALE: If the 1971 decision to float the dollar had been reversed, the global monetary system would have had to find an alternative anchor, potentially leading to a return to a gold standard or a new, multilateral reserve asset, fundamentally altering international finance.
% FOUNDING_PROBLEM: The U.S. faced a 'Triffin dilemma' where its commitment to gold convertibility at a fixed price was unsustainable due to increasing global demand for dollars (requiring U.S. deficits) and dwindling gold reserves.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians widely corroborate the existence of the Triffin dilemma and the pressure on U.S. gold reserves. However, this reading emphasizes the discrete choice rather than the inevitability, suggesting the problem could have been addressed differently.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.45) reflects the effective devaluation of foreign dollar holdings and the shift of monetary policy flexibility to the U.S. The suppression (0.6) arises from the unilateral nature of the decision, leaving other nations with limited immediate alternatives. Theater ratio is low (0.1) as the action was a direct, functional policy change, not performative. Accessibility collapse is moderate (0.7) as the previous system was effectively gone, but new coordination mechanisms (floating rates) emerged. Resistance (0.5) reflects the initial international outcry and attempts at negotiation.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. perspective, the action was a necessary, albeit difficult, step to restore domestic economic stability and monetary sovereignty. From the perspective of foreign dollar holders, it was an act of expropriation. This reading emphasizes the agency of the U.S. in making a choice, rather than being forced by external conditions.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. fiscal authorities are clear beneficiaries, gaining policy freedom. Foreign dollar holders are victims, bearing the cost of devaluation. Global trade partners are mixed, benefiting from continued trade but facing new risks. The IMF, as an observer, adapted its role to the new regime.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_choice,
    'Was the suspension of gold convertibility a discrete institutional choice, or an inevitable outcome of underlying structural pressures (e.g., Triffin dilemma, Vietnam War deficits)?',
    'Counterfactual historical analysis: detailed modeling of alternative policy paths and their likely outcomes if the U.S. had chosen not to suspend convertibility.',
    'If inevitable, the constraint''s extractiveness might be re-attributed to systemic forces rather than unilateral action, potentially shifting its classification towards a Mountain or a more structurally determined Tangled Rope. If a clear choice, the Rope classification emphasizing coordination failure and defection holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_vs_choice, conceptual, 'Ambiguity between institutional choice and structural inevitability in the 1971 Nixon Shock.').

omega_variable(
    devaluation_as_expropriation,
    'To what extent did the devaluation of the dollar constitute an ''expropriation'' of foreign dollar holdings, versus a necessary market adjustment?',
    'Economic analysis comparing the real value of foreign dollar holdings before and after the suspension, accounting for inflation and alternative investment opportunities.',
    'A strong finding of expropriation would increase the perceived extractiveness and suppression, potentially pushing the constraint towards a Snare. If primarily a market adjustment, the Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devaluation_as_expropriation, empirical, 'The nature of the loss incurred by foreign dollar holders due to the 1971 devaluation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1971, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(mone_tr_t1972, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1972, 0.1).
narrative_ontology:measurement(mone_tr_t1973, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1973, 0.1).
narrative_ontology:measurement(mone_tr_t1974, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1974, 0.1).
narrative_ontology:measurement(mone_tr_t1975, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1975, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.4).
narrative_ontology:measurement(mone_be_t1972, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1972, 0.45).
narrative_ontology:measurement(mone_be_t1973, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1973, 0.48).
narrative_ontology:measurement(mone_be_t1974, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1974, 0.46).
narrative_ontology:measurement(mone_be_t1975, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1975, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.55).
narrative_ontology:measurement(mone_su_t1972, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1972, 0.6).
narrative_ontology:measurement(mone_su_t1973, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1973, 0.62).
narrative_ontology:measurement(mone_su_t1974, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1974, 0.6).
narrative_ontology:measurement(mone_su_t1975, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1975, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, global_reserve_currency_status).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, floating_exchange_rate_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monetary anchor principle' kernel. This 'punctuated swap' reading emphasizes the discrete institutional choice of the 1971 Nixon Shock, contrasting with the 'overdetermined composite' and 'Triffin inevitability' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
