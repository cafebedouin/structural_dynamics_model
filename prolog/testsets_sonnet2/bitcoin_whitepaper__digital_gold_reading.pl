% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__digital_gold_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Digital Gold: Store-of-Value Reading
 *   domain: cryptocurrency_economics/monetary_systems
 *
 * SUMMARY:
 *   This story instantiates the digital_gold_reading of the contested
 *   bitcoin_whitepaper kernel: Bitcoin as a scarce, appreciating store of
 *   value and inflation hedge, with transaction fees treated as an acceptable
 *   and even reassuring cost of scarcity rather than a defect. This reading
 *   has become the dominant institutional framing since roughly 2017-2020,
 *   displacing the original payment-system emphasis in practice even though
 *   the whitepaper text supports both readings. The coordination function (a
 *   savings vehicle outside discretionary monetary policy) is genuine; the
 *   extraction runs through appreciation-driven wealth transfer to early
 *   holders and fee-driven exclusion of small-value users, both legitimized
 *   by the reading's own framing of volatility and fees as signs of health
 *   rather than cost.
 *
 * KEY AGENTS:
 *   - early_holders: structural beneficiary of appreciation, near-full arbitrage exit
 *   - mining_industrial_operators: beneficiary and partial agenda-setter, favors fee-tolerant framing
 *   - institutional_custodians: beneficiary via allocation product fees, no stake in payment usability
 *   - late_retail_entrants: primary victim of appreciation-cycle timing
 *   - small_value_transactors: primary victim of fee-tolerant congestion policy
 *   - unbanked_target_populations: victim whose original justificatory role has been abandoned by the dominant reading
 *   - p2p_cash_advocates: excluded sibling-reading holders whose governance position lost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.35).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold: Store-of-Value Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '5c2d73e0-250b-4a1d-a01d-3ababb995e9d').
narrative_ontology:cs_kernel_codification('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', fixed_text).
narrative_ontology:cs_authority_grounding('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', practice).
narrative_ontology:cs_interpretation_layer_present('5c2d73e0-250b-4a1d-a01d-3ababb995e9d').
narrative_ontology:cs_reading_relation('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', foundational, scarcity_appreciation_is_primary_virtue).
narrative_ontology:cs_axiom_status(scarcity_appreciation_is_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', scarcity_appreciation_is_primary_virtue, instrumental).
narrative_ontology:cs_axiom('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', secondary, transaction_fee_pressure_signals_healthy_demand).
narrative_ontology:cs_axiom_status(transaction_fee_pressure_signals_healthy_demand, holdable).
narrative_ontology:cs_axiom_grounding('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', transaction_fee_pressure_signals_healthy_demand, empirically_contingent).
narrative_ontology:cs_reference_frame('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', whitepaper_dual_use_ambiguity).
narrative_ontology:cs_drift_state('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', post_2017_institutionalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c2d73e0-250b-4a1d-a01d-3ababb995e9d', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_industrial_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_custodians).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_retail_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, unbanked_target_populations).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, fixed_supply_monetary_soundness_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, digital_scarcity_as_value_store).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired coins when price and mining difficulty were low, and hold large positions relative to circulating supply. Benefit directly and disproportionately from appreciation-driven narrative dominance; can exit into fiat or other assets at will and have every incentive to promote the store-of-value framing since it legitimizes holding rather than spending.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_holders, beneficiary,
    organized, generational, arbitrage, global).

% Run large-scale mining operations funded by capital that prices in continued appreciation and fee revenue. Favor the digital-gold framing because it supports higher transaction fees (miners are paid from fees plus a shrinking block subsidy) and justifies energy-intensive proof-of-work as securing a scarce store of value rather than a payment rail. Can relocate operations across jurisdictions for favorable energy costs and regulation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_industrial_operators, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, mining_industrial_operators, agenda_setter).

% Asset managers, ETF sponsors, and custody banks that earn fees on Bitcoin exposure products. Directly benefit from the digital-gold narrative because it makes Bitcoin legible to conservative allocators (pension funds, sovereign wealth) seeking an inflation hedge rather than a payment technology. Have no stake in low transaction fees or payment usability.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_custodians, beneficiary,
    institutional, generational, arbitrage, global).

% Buy in at or near cycle peaks driven by store-of-value hype, often on borrowed money or with a meaningful share of savings, without early holders' cost basis or capital reserves to weather drawdowns. Bear the brunt of volatility that the appreciation-focused narrative actively encourages; exit typically means realizing a loss.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_retail_entrants, payer,
    powerless, biographical, trapped, national).

% Want to use Bitcoin for everyday payments or remittances. Under the digital-gold reading, the community treats high transaction fees during demand spikes as an acceptable cost of scarcity-driven appreciation rather than a defect to be fixed, and treats layer-1 fee pressure as evidence the asset is 'working' as a store of value. These users are priced out of on-chain settlement precisely when demand (and fees) are highest, and are pushed toward custodial or second-layer alternatives that reintroduce the intermediaries Bitcoin was pitched as removing.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_value_transactors, payer,
    powerless, immediate, constrained, global).

% The original marketing case for Bitcoin's adoption in unbanked or hyperinflationary economies depended on cheap, reliable transactions. Under the digital-gold reading's fee tolerance and volatility-as-feature framing, this population gets an asset too volatile to hold for daily expenses and too costly to move in small amounts during periods of network congestion — the population most cited to justify Bitcoin's social value is least served by the reading that has come to dominate its governance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, unbanked_target_populations, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, unbanked_target_populations, excluded).

% Developers and users who hold the sibling p2p_cash_reading — that Bitcoin's core purpose is censorship-resistant peer-to-peer payment. They object to fee tolerance and low-throughput layer-1 design choices that the digital-gold reading treats as acceptable or even desirable, but their proposals for larger blocks or fee minimization lost the governance contest and they now operate on forked chains or layer-2 systems with far smaller network effects.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, p2p_cash_advocates, excluded,
    moderate, biographical, constrained, global).

% Study Bitcoin's price behavior, correlation with equities and gold, and volatility profile to assess whether the inflation-hedge claim holds empirically. Their findings feed back into institutional allocation decisions but do not set policy for the protocol or the community narrative.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, macro_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, early_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large, dispersed set of holders around a shared belief in fixed, algorithmically enforced scarcity (21 million cap), enabling a common store-of-value asset that no single issuer can dilute — solving the genuine problem of finding a savings vehicle outside state-controlled monetary policy.
% TRANSFER_FUNCTION: Moves purchasing power from later entrants to earlier holders through price appreciation (a structurally zero-sum transfer among holders over any given interval), and moves transaction value from small-value users to miners through fee competition prioritized under the scarcity narrative.
% ABSENT_VOICES: Advocates of the p2p_cash_reading and populations in hyperinflationary or unbanked economies who were the original justificatory case for Bitcoin's payment utility are not represented in the governance processes (mining pools, core developer consensus, exchange listing decisions) that have entrenched the digital-gold framing; they are cited rhetorically but rarely consulted structurally.
% DISAPPEARANCE_RATIONALE: If the digital-gold reading collapsed (e.g., a sustained empirical finding that Bitcoin does not hedge inflation, or a governance shift back toward payment optimization), institutional custody products would lose their allocation thesis, mining economics premised on appreciation would need to re-price around transaction fees alone, and a large class of holders who bought the inflation-hedge narrative would reassess exposure — a substantial reorganization of capital and narrative, not a null event.
% FOUNDING_PROBLEM: The Bitcoin whitepaper (2008) was framed around enabling direct electronic payments without a trusted third party, motivated by mistrust of intermediary-dependent payment systems and, implicitly, of discretionary monetary policy during the financial crisis.
% FOUNDING_PROBLEM_CORROBORATION: Early Bitcoin developers and the original whitepaper text corroborate the payment-system founding problem; macro analysts and financial historians outside the Bitcoin-holding community corroborate that the digital-gold framing emerged later, substantially reshaped by holders and institutional intermediaries whose economic interest is appreciation rather than payment utility — this is a genealogical account with corroboration from outside the beneficiary set, but the community's own current majority position treats the store-of-value framing as the original and correct reading, so status remains contested rather than settled.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__digital_gold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__digital_gold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, not extreme) because the store-of-value function is not fraudulent — Bitcoin's fixed supply is real and the coordination benefit to holders seeking a non-sovereign savings vehicle is genuine. But the reading's fee tolerance and appreciation-first prioritization create a real transfer from late/small-value participants to early/large holders, which the reading's own vocabulary (volatility as maturation, fees as proof of scarcity) obscures rather than resolves. Suppression (0.35) is moderate: no one is coerced into holding Bitcoin, but exit from a sunk position after a price run-up is costly, and the governance process that entrenched this reading over the p2p_cash_reading was itself contested and partly foreclosed alternatives (larger blocks, fee-minimization proposals) through social and technical consensus mechanisms that excluded dissenting developers. Theater ratio rises over the interval (0.05 to 0.28) as institutional marketing and exchange-traded product launches increasingly perform 'digital gold' branding independent of any change in the underlying protocol.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (early holders, miners, custodians) this looks like coordination succeeding exactly as designed — a scarce asset appreciating as adoption grows, fees rising as evidence of genuine demand. From the payer seats (late entrants, small-value users, unbanked populations) the same structure looks like a wealth-transfer mechanism dressed in scarcity language, where the properties celebrated by the dominant reading (volatility, rising fees) are precisely the properties that exclude them. The engine's per-seat computation should reflect this divergence without either seat's account being definitive.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders and industrial miners sit near the full-beneficiary end: they collect appreciation and fee revenue respectively, and both have mobile/arbitrage exit. Institutional custodians benefit from fee income on products built around this reading with no exposure to payment-utility risk. Late retail entrants and small-value transactors sit near the full-target end: trapped or constrained exit, bearing the price-timing and fee costs the reading's own priorities create. Unbanked target populations are especially poorly served: they were the reading's original justificatory case (financial inclusion) but the digital-gold framing that has come to dominate treats their use case as a legacy talking point rather than a design constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (payment without trusted intermediaries) is contested as live or dead depending on whom you ask; the digital-gold reading's own proponents argue the founding problem has evolved rather than died — that monetary soundness was always the deeper problem and payment was an initial use case. The mismatch worth flagging is: founding_problem_status is contested while disappearance_verdict is world_rearranges, which is consistent with a reading that has partially but not fully drifted from its founding justification — not a clean mandatrophy case, but adjacent to one, and the corpus should track whether the gap widens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_dominance_causal_mechanism,
    'Did the digital-gold reading become dominant because it reflects Bitcoin''s actual empirical behavior (genuine store-of-value performance), or because the agents who benefit most from appreciation (early holders, miners, custodians) had disproportionate influence over community narrative, media coverage, and exchange/product design?',
    'Historical analysis of Bitcoin community governance forums, developer mailing lists, and institutional product launch timing relative to price cycles; comparison with the block-size wars where p2p_cash-aligned developers lost governance contests.',
    'If narrative dominance tracks beneficiary influence more than empirical performance, this strengthens the tangled_rope classification (extraction dressed as emergent consensus); if it tracks genuine superior fit to observed use, the coordination function is stronger relative to the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dominance_causal_mechanism, conceptual, 'Whether the digital-gold reading''s dominance is causally driven by beneficiary influence or empirical fit.').

omega_variable(
    inflation_hedge_empirical_status,
    'Does Bitcoin actually function as an inflation hedge (low or negative correlation with fiat purchasing-power loss) or is this a post-hoc narrative unsupported by its realized correlation with risk assets?',
    'Longitudinal correlation analysis between Bitcoin price and CPI/monetary-base measures versus its correlation with equity risk factors, across multiple market cycles including tightening cycles.',
    'If the inflation-hedge property is empirically weak, the digital-gold reading''s core claim is closer to marketing than description, raising the effective extractiveness of the reading (beneficiaries selling a hedge that does not hedge); if empirically robust, the coordination function is more substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_hedge_empirical_status, empirical, 'Whether Bitcoin empirically performs as an inflation hedge, bearing on how substantive the reading''s coordination claim is.').

omega_variable(
    sibling_reading_foreclosure_degree,
    'Has the digital-gold reading''s institutional and social dominance made the p2p_cash_reading practically unrecoverable on the main Bitcoin chain (versus merely disfavored), given layer-1 fee market design and mining incentive structures now optimized around store-of-value use?',
    'Technical analysis of whether protocol-level changes required to re-optimize for low-fee payment (larger blocks, different fee market design) remain technically available versus socially/politically foreclosed by the current governance equilibrium.',
    'If technically foreclosed, the relation to p2p_cash_reading should be reconsidered toward forecloses rather than coexists_with; if merely disfavored and reversible via governance, coexists_with remains accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_degree, conceptual, 'Whether the digital-gold reading has structurally foreclosed the p2p_cash reading on the main chain, or merely disfavors it while both remain live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2008, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(bitc_tr_t2011, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2011, 0.08).
narrative_ontology:measurement(bitc_tr_t2014, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2014, 0.14).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(bitc_tr_t2020, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2008, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement(bitc_be_t2011, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2011, 0.22).
narrative_ontology:measurement(bitc_be_t2014, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2014, 0.32).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2017, 0.46).
narrative_ontology:measurement(bitc_be_t2020, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2020, 0.51).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2024, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(bitcoin_whitepaper__digital_gold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'Bitcoin whitepaper' per the ε-invariance principle: digital_gold_reading (this file, ε=0.58, tangled_rope), p2p_cash_reading (payment-optimized reading, expected lower fee-tolerance and different victim set), and protocol_ossification_reading (stability-as-primary-virtue reading, different beneficiary set centered on node operators/core developers). Each reading is a distinct constraint with its own stable ε; they are linked via affects_constraints because governance and market outcomes in one reading structurally influence resource availability and legitimacy conditions in the others (e.g., digital-gold dominance affects fee-market design decisions that determine whether p2p_cash_reading remains viable on layer 1).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
