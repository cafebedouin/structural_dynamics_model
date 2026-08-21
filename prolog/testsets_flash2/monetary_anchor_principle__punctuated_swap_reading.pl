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
 *   human_readable: Monetary Anchor Principle: Punctuated Swap Reading (1971 Nixon Shock)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story represents the 'punctuated swap' reading of the
 *   monetary anchor principle, focusing on the August 15, 1971, decision by
 *   the U.S. to unilaterally suspend the dollar's convertibility to gold.
 *   This reading emphasizes the discrete institutional choice that
 *   fundamentally altered the international monetary regime, rather than
 *   viewing it as an inevitable outcome of structural forces. It frames the
 *   event as a coordination failure leading to a unilateral defection,
 *   benefiting U.S. fiscal autonomy at the expense of foreign dollar holders.
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
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '8fa4f403-2f8f-4f21-b700-5d0209cf27d1').
narrative_ontology:cs_kernel_codification('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', formalized).
narrative_ontology:cs_authority_grounding('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', extraction).
narrative_ontology:cs_interpretation_layer_present('8fa4f403-2f8f-4f21-b700-5d0209cf27d1').
narrative_ontology:cs_reading_relation('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', foundational, institutional_agency_primary).
narrative_ontology:cs_axiom_status(institutional_agency_primary, holdable).
narrative_ontology:cs_axiom_grounding('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', institutional_agency_primary, conventional).
narrative_ontology:cs_axiom('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', foundational, monetary_regime_discrete_choice).
narrative_ontology:cs_axiom_status(monetary_regime_discrete_choice, holdable).
narrative_ontology:cs_axiom_grounding('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', monetary_regime_discrete_choice, conventional).
narrative_ontology:cs_reference_frame('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', bretton_woods_fixed_exchange_rate_system).
narrative_ontology:cs_drift_state('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', post_nixon_shock_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8fa4f403-2f8f-4f21-b700-5d0209cf27d1', '').
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

% Benefited from continued global liquidity provided by the dollar, but also faced increased exchange rate volatility and the loss of a stable monetary anchor for international trade and investment.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, global_trade_partners, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, global_trade_partners, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a new, albeit less stable, framework for international monetary coordination after the collapse of the Bretton Woods system, allowing for continued global trade and finance under floating exchange rates.
% TRANSFER_FUNCTION: Transferred the cost of U.S. fiscal expansion and balance of payments deficits from the U.S. gold reserves to foreign dollar holders, effectively devaluing their dollar assets.
% ABSENT_VOICES: Smaller nations heavily reliant on dollar reserves, who had less leverage to negotiate or retaliate against the unilateral U.S. decision, were effectively excluded from the decision-making process.
% DISAPPEARANCE_RATIONALE: The gold standard, as a global monetary anchor, effectively disappeared on August 15, 1971. The world did not revert to a gold-backed system; instead, it adapted to a fiat currency regime with floating exchange rates, indicating a fundamental shift rather than a temporary disruption.
% FOUNDING_PROBLEM: The Bretton Woods system faced increasing pressure from U.S. balance of payments deficits and a dwindling gold supply, making the fixed exchange rate regime unsustainable.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians widely corroborate that the specific problem of gold convertibility under Bretton Woods is dead. While new monetary challenges have emerged, the original problem of maintaining dollar-gold convertibility is no longer relevant.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_unchanged).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) reflects the transfer of wealth from foreign dollar holders due to devaluation, but it's not extremely high because the U.S. did provide a new, albeit less stable, coordination framework. Suppression (0.6) is moderate, as foreign nations had limited immediate options to resist the unilateral U.S. action, but could eventually adjust their policies. Theater ratio is low (0.1) because the action was a direct, functional policy change, not a performative maintenance of an atrophied system. The claimed type is 'rope' because, despite the unilateral defection, the action ultimately led to a new form of international monetary coordination, albeit one with significant extractive elements for some parties.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. perspective, the decision was a necessary, if difficult, step to restore fiscal stability and monetary policy independence. From the perspective of foreign dollar holders, it was an act of expropriation. This divergence is central to the 'punctuated swap' reading, which highlights the agency and choice involved.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. fiscal authorities are clear beneficiaries, gaining policy freedom. Foreign dollar holders are victims, experiencing devaluation. Global trade partners are mixed, benefiting from liquidity but facing volatility. The IMF and smaller nations are observers or excluded, reacting to the shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining a stable international monetary system) was not resolved by the Nixon Shock, but rather fundamentally redefined. The 'punctuated swap' reading prevents mislabeling the event as pure extraction by acknowledging the subsequent, albeit different, coordination function that emerged. However, it highlights the extractive nature of the transition itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_stability,
    'Could the Bretton Woods system have been maintained with different U.S. fiscal or monetary policies, or was its collapse truly inevitable?',
    'Historical counterfactual analysis, comparing outcomes under alternative policy paths. This is largely a conceptual exercise given the historical context.',
    'If the system could have been maintained, the ''punctuated swap'' reading''s emphasis on choice is strengthened, and the extractiveness of the U.S. action is amplified. If collapse was inevitable, the reading shifts towards the ''overdetermined_composite'' or ''Triffin inevitability'' perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_stability, conceptual, 'Assesses the degree of institutional choice versus structural inevitability in the collapse of Bretton Woods.').

omega_variable(
    long_term_coordination_benefits,
    'Did the post-1971 floating exchange rate regime ultimately provide greater or lesser global monetary coordination and stability compared to a reformed gold standard?',
    'Longitudinal economic studies comparing trade volumes, capital flows, and financial crises under both regimes, accounting for other confounding factors.',
    'If the floating regime proved more stable, the ''rope'' classification is reinforced. If less stable, the extractive aspects of the transition are amplified, potentially pushing the classification towards ''tangled_rope'' or ''snare'' for foreign dollar holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_coordination_benefits, empirical, 'Evaluates the long-term coordination efficacy of the post-gold standard monetary system.').


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
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monetary_anchor_principle' kernel. This 'punctuated_swap_reading' emphasizes the discrete institutional choice of the 1971 Nixon Shock, contrasting with readings that highlight structural inevitability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
