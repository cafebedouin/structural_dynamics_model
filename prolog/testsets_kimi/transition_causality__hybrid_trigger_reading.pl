% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Triffin Regime (Hybrid Trigger Reading)
 *   domain: monetary_economics / political_economy / international_finance
 *
 * SUMMARY:
 *   This constraint story models the Bretton Woods gold-exchange standard and
 *   its embedded Triffin Dilemma under the hybrid_trigger_reading of the
 *   transition_causality kernel. The reading holds that structural
 *   contradictions â the incompatibility between global liquidity provision
 *   and gold convertibility â accumulated throughout the 1960s but required
 *   contingent triggers (Vietnam War fiscal expansion, French gold
 *   conversions) to actualize the regime's collapse. The constraint is
 *   claimed as tangled_rope because it combined a genuine global monetary
 *   coordination function (fixed rates, liquidity provision) with asymmetric
 *   extraction (US seigniorage, adjustment burden-shifting) and active
 *   enforcement (capital controls, gold-pool defense, IMF conditionality).
 *
 * KEY AGENTS:
 *   - us_treasury_federal_reserve: Primary agenda-setter and beneficiary (institutional/arbitrage) â administers the dollar-gold peg and collects seigniorage.
 *   - deficit_non_reserve_nations: Primary target (powerless/trapped) â bears asymmetric adjustment and IMF conditionality.
 *   - gold_pool_participating_states: Secondary target (institutional/constrained) â defends the gold peg at direct financial cost.
 *   - international_banks_dollar_market: Financial beneficiary (powerful/mobile) â captures intermediation rents from the dollar-centered liquidity structure.
 *   - academic_analysts_observers: Analytical observer (analytical/analytical) â diagnosed the Triffin contradiction from outside the benefiting coalition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.68).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.72).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Triffin Regime (Hybrid Trigger Reading)").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics / political_economy / international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '60a9d15d-d773-4729-acfd-ffabda4c03e0').
narrative_ontology:cs_kernel_codification('60a9d15d-d773-4729-acfd-ffabda4c03e0', formalized).
narrative_ontology:cs_authority_grounding('60a9d15d-d773-4729-acfd-ffabda4c03e0', lineage).
narrative_ontology:cs_interpretation_layer_present('60a9d15d-d773-4729-acfd-ffabda4c03e0').
narrative_ontology:cs_reading_relation('60a9d15d-d773-4729-acfd-ffabda4c03e0', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('60a9d15d-d773-4729-acfd-ffabda4c03e0', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_axiom('60a9d15d-d773-4729-acfd-ffabda4c03e0', foundational, structural_fragility_requires_trigger).
narrative_ontology:cs_axiom_status(structural_fragility_requires_trigger, holdable).
narrative_ontology:cs_axiom_grounding('60a9d15d-d773-4729-acfd-ffabda4c03e0', structural_fragility_requires_trigger, empirically_contingent).
narrative_ontology:cs_axiom('60a9d15d-d773-4729-acfd-ffabda4c03e0', foundational, counterfactual_openness_of_transition).
narrative_ontology:cs_axiom_status(counterfactual_openness_of_transition, holdable).
narrative_ontology:cs_axiom_grounding('60a9d15d-d773-4729-acfd-ffabda4c03e0', counterfactual_openness_of_transition, empirically_contingent).
narrative_ontology:cs_reference_frame('60a9d15d-d773-4729-acfd-ffabda4c03e0', gold_exchange_equilibrium_under_hegemony).
narrative_ontology:cs_drift_state('60a9d15d-d773-4729-acfd-ffabda4c03e0', post_trigger_collapse_1971, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('60a9d15d-d773-4729-acfd-ffabda4c03e0', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_treasury_federal_reserve).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, international_banks_dollar_market).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, deficit_non_reserve_nations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_pool_participating_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, industrialized_surplus_nations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, industrialized_surplus_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the dollar-gold peg and Bretton Woods rules; collected seigniorage from global reserve demand; bore the Triffin contradiction but retained the unilateral option to suspend convertibility; defended the gold pool until 1968 and closed the gold window in 1971.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_treasury_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, us_treasury_federal_reserve, beneficiary).

% Enforced Article VIII capital controls and par-value obligations; provided conditional liquidity; derived institutional purpose and operating budget from managing the fixed-rate regime.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, imf_bureaucracy, agenda_setter,
    institutional, generational, constrained, global).

% Profited from dollar-denominated intermediation and offshore Eurodollar markets; benefited from the fixed-rate system's liquidity but could arbitrage regulatory gaps across jurisdictions.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_banks_dollar_market, beneficiary,
    powerful, biographical, mobile, global).

% Held dollars as reserves, absorbing US deficits to maintain export competitiveness; faced imported inflation; politically constrained from exiting the fixed-rate system by alliance structures and export dependence.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, industrialized_surplus_nations, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, industrialized_surplus_nations, beneficiary).

% Required dollars for trade and debt service; subject to IMF austerity conditionality when reserves ran low; unable to devalue without stigma or loss of market access; bore asymmetric adjustment burdens.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, deficit_non_reserve_nations, payer,
    powerless, immediate, trapped, national).

% Contributed gold to the London Gold Pool to defend the $35 per ounce peg; bore financial losses as the pool depleted; politically committed to Bretton Woods stability despite rising fiscal costs.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_pool_participating_states, payer,
    institutional, biographical, constrained, national).

% Documented the Triffin Dilemma and predicted the system's unsustainability; provided external corroboration of structural fragility without institutional stake in regime maintenance.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, academic_analysts_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a global nominal anchor (dollar-gold peg) and stable exchange rates, reducing currency risk and transaction costs for post-war trade reconstruction and capital-flow management in the absence of a world central bank.
% TRANSFER_FUNCTION: Moved seigniorage and adjustment asymmetry from non-reserve nations and gold-pool participants to the US Treasury and Federal Reserve, and financial-intermediation rents to dollar-market banks; transferred inflationary pressure outward from the center to the periphery.
% ABSENT_VOICES: Peripheral deficit nations subject to IMF conditionality were formally present but procedurally excluded from agenda-setting; alternative reserve-currency advocates and floating-rate proponents were sidelined in official Bretton Woods fora.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods fixed-rate regime and gold-convertibility constraint vanished overnight, exchange-rate parities would disperse, dollar-reserve holdings would revalue or flee, IMF conditionality would lose its nominal anchor, and the entire architecture of post-war trade and capital control would reorganize â as indeed occurred after 1971.
% FOUNDING_PROBLEM: Post-war monetary chaos and competitive devaluations of the 1930s; lack of a global liquidity provider and nominal anchor after World War II.
% FOUNDING_PROBLEM_CORROBORATION: Keynes and White planning documents from 1944 corroborate the founding problem. Independent macro-historians attest the problem was live in 1945 but had mutated by the 1960s; US Treasury officials and BW architects attest from the beneficiary seat. No corroboration from outside the benefiting parties exists for the claim that the founding problem remained unmutated in 1971.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) reflects the large but partial decoupling of US seigniorage from global coordination costs. Suppression (0.72) is high because the regime's persistence depended on capital controls, legal restrictions on gold markets, and IMF conditionality that blocked alternative monetary arrangements. Theater_ratio (0.45) captures the increasing performative quality of gold-pool interventions in the late 1960s, where public commitments to the $35 peg outran private confidence. Accessibility_collapse (0.58) indicates that alternatives (floating rates, SDR substitution accounts) were technically understood but institutionally suppressed. Resistance (0.60) reflects France's gold raids, recurrent sterling crises, and academic critiques such as Triffin's original exposition.
 *
 * PERSPECTIVAL GAP:
 *   The US Treasury/Federal Reserve seat experiences the constraint as a burdensome but necessary hegemonic responsibility that generates seigniorage; the engine will compute a low directionality and damped extraction. Deficit non-reserve nations experience the same arrangement as a trap where they must absorb dollars and undergo austerity to maintain pegs; the engine computes high directionality and amplified extraction. Gold-pool participants occupy a middle seat: institutional power but constrained exit, yielding intermediate directionality and moderate effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (us_treasury_federal_reserve, international_banks_dollar_market) place those seats at the low-d end of the spectrum. Victim declarations (deficit_non_reserve_nations, gold_pool_participating_states) place those seats at the high-d end. The US Treasury retains arbitrage-grade exit (could close the gold window unilaterally), which drives its derived directionality toward the beneficiary pole. Trapped and constrained exits for victim seats drive directionality toward the target pole. The industrialized_surplus_nations stakeholder is not declared in either array, so it defaults to canonical fallback rather than forced classification, reflecting its genuine ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the Bretton Woods system as either pure coordination (rope) or pure extraction (snare). The genuine coordination function â a global nominal anchor and post-war liquidity provision â is structurally real. However, the asymmetric distribution of seigniorage and adjustment costs, combined with active enforcement against alternatives, means the coordination story is not cover but is coupled with extraction. Mandatrophy (a dead founding problem) is not declared because the founding problem (post-war monetary disorder) was arguably solved and then mutated into a new contradiction; the constraint collapsed rather than persisting as a piton, though some institutional remnants (IMF Article VIII) persist in modified form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_counterfactual_necessity,
    'Were the Vietnam War fiscal shock and French gold runs strictly necessary triggers, or would other contingent events (e.g., a different US recession, a Middle East crisis) have produced the same collapse?',
    'Structured counterfactual analysis of the BW system''s fragility index in 1968-1970; comparison with alternate historical scenarios.',
    'If alternate triggers would have sufficed, the reading edges toward overdetermined; if only these specific triggers breached the threshold, the hybrid reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_counterfactual_necessity, conceptual, 'Whether specific historical triggers were necessary or substitutable.').

omega_variable(
    seigniorage_quantification,
    'What proportion of the measured extraction represents genuine coordination cost (global liquidity provision) versus asymmetric seigniorage rent?',
    'Econometric estimation of the US net seigniorage gain from reserve currency status versus the cost of gold-pool defense and global liquidity provision.',
    'If seigniorage dominates, the constraint is more extractive than coordinated; if liquidity provision costs dominate, it is more coordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_quantification, empirical, 'Quantifying the split between coordination benefit and seigniorage extraction.').

omega_variable(
    kernel_naturalness,
    'Is the Triffin Dilemma a logical necessity of any gold-exchange standard (mountain-like), or a historically contingent property of mid-20th-century geopolitical arrangements?',
    'Comparative analysis of reserve-currency systems under metallic and fiat standards to test for structural recurrence.',
    'If it recurs universally, the constraint approaches mountain status; if it is specific to BW institutions, it is a constructed tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_naturalness, conceptual, 'Whether the Triffin contradiction is a natural law or a constructed institutional feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1960, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_tr_t1960, transition_causality__hybrid_trigger_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_tr_t1963, transition_causality__hybrid_trigger_reading, theater_ratio, 1963, 0.3).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_tr_t1966, transition_causality__hybrid_trigger_reading, theater_ratio, 1966, 0.38).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_tr_t1969, transition_causality__hybrid_trigger_reading, theater_ratio, 1969, 0.48).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.55).

% Extraction over time
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_be_t1960, transition_causality__hybrid_trigger_reading, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_be_t1963, transition_causality__hybrid_trigger_reading, base_extractiveness, 1963, 0.52).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_be_t1966, transition_causality__hybrid_trigger_reading, base_extractiveness, 1966, 0.6).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_be_t1969, transition_causality__hybrid_trigger_reading, base_extractiveness, 1969, 0.66).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_su_t1960, transition_causality__hybrid_trigger_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_su_t1963, transition_causality__hybrid_trigger_reading, suppression_requirement, 1963, 0.6).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_su_t1966, transition_causality__hybrid_trigger_reading, suppression_requirement, 1966, 0.68).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_su_t1969, transition_causality__hybrid_trigger_reading, suppression_requirement, 1969, 0.75).
narrative_ontology:measurement(transition_causality__hybrid_trigger_reading_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, overdetermined_collapse_reading).

% DUAL FORMULATION NOTE:
% The kernel transition_causality (Bretton Woods collapse) decomposes into three structurally distinct readings because the label 'Bretton Woods collapse' conflates competing causal claims with different epsilon profiles and counterfactual commitments. Each reading carries its own constraint_id, stakeholders, and classification. This reading (hybrid_trigger) asserts structural fragility plus necessary contingent triggers; the siblings assert pure contingency or structural inevitability respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
