% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Bitcoin as Digital Gold — Store-of-Value / Inflation-Hedge Reading
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This story isolates the 'digital gold' reading of the Bitcoin whitepaper
 *   kernel: Bitcoin as a scarce, appreciating store of value and inflation
 *   hedge, distinct from the peer-to-peer cash reading and the
 *   protocol-ossification reading (each its own constraint file). Under this
 *   reading, the fixed 21-million supply cap and predictable issuance
 *   schedule are marketed primarily as scarcity guarantees for holders rather
 *   than as the foundation for a low-fee payment network. Custodial
 *   exchanges, ETF issuers, and large early holders have organized around
 *   this framing because it maximizes asset appreciation and
 *   assets-under-custody; this creates a coordination function (a credible,
 *   verifiable scarce asset around which capital can coordinate) fused with
 *   an extraction dynamic (later entrants buy in at appreciated prices, and
 *   blockspace increasingly prices out small payment use as the network
 *   optimizes for high-value settlement).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.52).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.31).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold — Store-of-Value / Inflation-Hedge Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'f98d09d9-226b-4a21-9ea8-b0332a51d63e').
narrative_ontology:cs_kernel_codification('f98d09d9-226b-4a21-9ea8-b0332a51d63e', fixed_text).
narrative_ontology:cs_authority_grounding('f98d09d9-226b-4a21-9ea8-b0332a51d63e', distributed).
narrative_ontology:cs_reading_relation('f98d09d9-226b-4a21-9ea8-b0332a51d63e', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('f98d09d9-226b-4a21-9ea8-b0332a51d63e', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('f98d09d9-226b-4a21-9ea8-b0332a51d63e', foundational, scarcity_as_primary_value_driver).
narrative_ontology:cs_axiom_status(scarcity_as_primary_value_driver, holdable).
narrative_ontology:cs_axiom_grounding('f98d09d9-226b-4a21-9ea8-b0332a51d63e', scarcity_as_primary_value_driver, instrumental).
narrative_ontology:cs_axiom('f98d09d9-226b-4a21-9ea8-b0332a51d63e', secondary, transaction_fee_cost_is_acceptable_settlement_price).
narrative_ontology:cs_axiom_status(transaction_fee_cost_is_acceptable_settlement_price, holdable).
narrative_ontology:cs_axiom_grounding('f98d09d9-226b-4a21-9ea8-b0332a51d63e', transaction_fee_cost_is_acceptable_settlement_price, conventional).
narrative_ontology:cs_reference_frame('f98d09d9-226b-4a21-9ea8-b0332a51d63e', cypherpunk_electronic_cash_framework).
narrative_ontology:cs_drift_state('f98d09d9-226b-4a21-9ea8-b0332a51d63e', post_institutional_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f98d09d9-226b-4a21-9ea8-b0332a51d63e', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_holders_and_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges_and_etf_issuers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_treasury_allocators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_retail_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, low_value_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, unbanked_populations_seeking_payment_utility).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, fixed_supply_curve_credibility).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, digital_scarcity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired or mined coins when price and difficulty were low, before the store-of-value narrative concentrated demand. Their holdings appreciate as the network's social meaning shifts from payment rail to reserve asset; they can liquidate into deep markets at will and face no fee competition for their existing stack.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_holders_and_miners, beneficiary,
    organized, generational, arbitrage, global).

% Build the on-ramps (spot ETFs, custody products, futures markets) that let the digital-gold narrative attract institutional capital. They actively promote the 'hold, don't spend' framing because it maximizes assets under custody and management fees, and they lobby regulators to formalize this reading over the payment-medium reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges_and_etf_issuers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges_and_etf_issuers, beneficiary).

% Corporate treasuries and funds that allocate to Bitcoin as an inflation hedge alongside gold. They benefit from appreciation driven by scarcity narrative and network effects reinforced by the same custodial infrastructure that markets to them; they can exit into fiat or other assets with comparatively low friction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_treasury_allocators, beneficiary,
    powerful, biographical, arbitrage, global).

% Buy in at appreciated prices driven up partly by the store-of-value narrative itself, often near local price peaks, absorbing volatility risk that early holders already realized as gains. Fee costs for on-chain settlement also compound against them since block space is scarce and priced for asset-settlement-scale transactions rather than small payments.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_retail_entrants, payer,
    moderate, biographical, constrained, global).

% Want to use the network for everyday payments (remittances, small purchases) but face transaction fees that the digital-gold reading treats as an acceptable, even desirable, cost of settlement assurance for large transfers. As blockspace is bid up by asset-settlement demand, small payments become uneconomical, pricing this group out of the base-layer network entirely.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, low_value_transaction_users, payer,
    powerless, immediate, trapped, regional).

% Would benefit most from a low-fee peer-to-peer payment rail but have no voice in the governance conversations where the digital-gold framing is reinforced by exchanges, media, and large holders. Their use case is structurally deprioritized because it does not maximize price appreciation or custodial fee revenue.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, unbanked_populations_seeking_payment_utility, excluded,
    powerless, immediate, trapped, global).

% Maintain the reference implementation and observe how the dominant social narrative (store of value vs. medium of exchange) shapes proposed protocol changes, fee market design, and block size debates without being able to unilaterally settle which reading governs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, protocol_developers, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges_and_etf_issuers).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large, dispersed set of holders around a shared belief in fixed, verifiable digital scarcity, enabling Bitcoin to function as a globally portable, censorship-resistant store of value outside state monetary control.
% TRANSFER_FUNCTION: Moves purchasing power from later entrants (who buy at appreciated prices and pay elevated transaction fees) to earlier holders and custodial intermediaries (who realize gains and collect fees as the store-of-value narrative concentrates demand and crowds out small-payment use of blockspace).
% ABSENT_VOICES: Unbanked and low-income populations who need a cheap payment rail more than an appreciating reserve asset are structurally absent from the exchanges, ETF issuers, and media narratives that reinforce the digital-gold framing; their preferred reading (p2p_cash_reading) competes for the same limited blockspace and loses.
% DISAPPEARANCE_RATIONALE: If the digital-gold framing specifically disappeared (while the network and protocol persisted), custodial products and treasury allocation strategies built on the narrative would likely unwind or reprice sharply, and blockspace demand/fee pressure would shift toward payment use cases — a real rearrangement for the beneficiary seats. But holders committed to the p2p_cash or ossification readings would argue the underlying network is unchanged; only the social narrative and its associated capital flows would move, hence contested rather than a clean world_rearranges.
% FOUNDING_PROBLEM: The whitepaper's founding problem was trusted, direct electronic payments without a financial intermediary. The digital-gold reading repurposes the same fixed-supply mechanism to solve a different problem: a scarce, portable hedge against currency debasement and confiscation.
% FOUNDING_PROBLEM_CORROBORATION: Early Bitcoin mailing-list participants and cypherpunk commentators (outside the current custodial-exchange and treasury-allocator beneficiary set) attest the original founding problem was peer-to-peer electronic cash, not a reserve asset; the digital-gold framing's own promoters (exchanges, ETF issuers, treasury allocators) are the primary source asserting the store-of-value problem is the 'real' or 'more mature' founding purpose, which is a self-interested attestation and is flagged as such.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.22 to 0.52) as the store-of-value narrative institutionalizes through ETFs and corporate treasury adoption, concentrating gains among early holders and intermediaries while later buyers absorb volatility at higher price levels. Theater ratio is moderate and rising (0.28 at endpoint) reflecting increasing promotional and marketing activity (ETF advertising, 'digital gold' branding) relative to genuine protocol-level payment functionality. Suppression is comparatively low (0.31) because no one is legally barred from using Bitcoin as cash — the suppression here is narrative and market-structural (fee markets and social consensus deprioritizing payment use) rather than coercive.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders, miners, custodial exchanges, ETF issuers, and treasury allocators sit near the beneficiary end: they hold appreciating assets or collect fees from managing exposure to those assets, and they have mobile-to-arbitrage exit options. Late retail entrants and low-value transaction users sit near the target end: they buy in at higher prices or are priced out of using the base layer for its original payment purpose, with constrained-to-trapped exit options since alternative low-fee rails require separate infrastructure (Lightning, other chains) that these populations often cannot access.
 *
 * MANDATROPHY ANALYSIS:
 *   The digital-gold reading's coordination function (a shared, verifiable scarcity mechanism enabling trustless value storage) remains genuinely live for many holders — it is not simply theater. But the reading also has crowded out the original payment-medium function for populations who most needed it, which is the asymmetric extraction that makes this tangled_rope rather than a clean rope. Classifying it as tangled_rope rather than snare acknowledges the coordination function is real and voluntary for beneficiaries, not purely coercive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dominance_contest,
    'Is the digital-gold reading the whitepaper''s authentic evolution given real-world adoption patterns, or a beneficiary-driven narrative capture of a payment-medium design?',
    'Track long-run on-chain transaction composition (settlement-scale vs. small-payment volume), Lightning Network adoption rates, and whether layer-2 payment rails successfully decouple payment utility from base-layer fee pressure — if they do, the readings could coexist without victim overlap; if they don''t, the digital-gold reading''s dominance directly forecloses payment utility for excluded populations.',
    'If resolved toward narrative capture, this constraint''s tangled_rope classification strengthens toward snare-like extraction; if resolved toward authentic evolution with successful layer-2 decoupling, the victim set shrinks and the constraint moves toward a cleaner rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance_contest, conceptual, 'Whether the digital-gold reading''s dominance reflects authentic market evolution or beneficiary-driven narrative capture of the original payment design.').

omega_variable(
    beneficiary_concentration_vs_broad_ownership,
    'Is Bitcoin ownership sufficiently distributed that ''early holders'' functions as a broad coordination benefit rather than a concentrated extraction class?',
    'On-chain distribution analysis (Gini coefficient of holdings, concentration among addresses active pre-2017) compared against custodial exchange and ETF assets-under-management concentration.',
    'High concentration among a small early-holder and institutional-custodian class would support treating this as tangled_rope trending toward snare; broad distribution would support a cleaner rope reading where appreciation benefits are widely shared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration_vs_broad_ownership, empirical, 'Whether early-holder and custodial beneficiary concentration is narrow (extraction-like) or broad (coordination-like).').

omega_variable(
    inflation_hedge_efficacy_uncertainty,
    'Does Bitcoin actually function as a reliable inflation hedge, or is the inflation-hedge framing itself a marketing claim not borne out by price-behavior correlation with inflation metrics?',
    'Longitudinal correlation analysis between Bitcoin price movements and CPI/monetary-base expansion across multiple macro cycles, distinguishing genuine hedge behavior from speculative correlation with risk assets.',
    'If Bitcoin does not reliably hedge inflation, the coordination function underlying the digital-gold reading is partly illusory, weakening the case for tangled_rope''s genuine-coordination requirement and pushing the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inflation_hedge_efficacy_uncertainty, empirical, 'Whether the inflation-hedge thesis central to this reading is empirically supported or primarily narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 16, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 9, 0.44).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 16, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 3, 0.18).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.21).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 9, 0.25).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.28).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 16, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the bitcoin_whitepaper kernel. digital_gold_reading and p2p_cash_reading compete for the same scarce blockspace resource, producing directly conflicting fee-market preferences and different victim sets (late entrants/small-payment users here vs. merchants/payment users under p2p_cash). protocol_ossification_reading is influenced by whichever use-case reading currently dominates network effects, since ossification preserves whatever configuration is already in place — currently reinforcing the digital-gold reading's fee-market status quo.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
