% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Bitcoin Digital Gold Reading
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint story captures the digital_gold_reading of the
 *   bitcoin_whitepaper kernel: the interpretation that Bitcoin is primarily a
 *   scarce digital asset optimized for store of value and inflation hedging.
 *   Under this reading, protocol parameters (fixed 21M supply, constrained
 *   block space, fee markets) are defended as features that enforce digital
 *   scarcity. The arrangement coordinates a global holder base around a
 *   non-sovereign store of value while structurally transferring purchasing
 *   power from late entrants and small transactors to early holders and
 *   institutional accumulators. This reading competes with the
 *   p2p_cash_reading (medium-of-exchange priority) and the
 *   protocol_ossification_reading (change illegitimacy); it has achieved
 *   dominance in the BTC main chain through social consensus, narrative
 *   capture, and technical enforcement of scarcity parameters.
 *
 * KEY AGENTS:
 *   - early_holders (powerful/arbitrage) â structural beneficiaries of appreciation
 *   - institutional_accumulators (powerful/arbitrage) â narrative beneficiaries and legitimizers
 *   - mining_incumbents (organized/constrained) â agenda-setters enforcing scarcity and collecting fees
 *   - late_entrants (moderate/constrained) â payers of appreciation premium
 *   - small_transactors (powerless/trapped) â priced out of on-chain access
 *   - p2p_cash_advocates (moderate/constrained) â excluded voices
 *   - macro_analysts (analytical) â external observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.65).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.55).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin Digital Gold Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '262db70b-2d7d-47b8-b621-7fa9cb9087ed').
narrative_ontology:cs_kernel_codification('262db70b-2d7d-47b8-b621-7fa9cb9087ed', fixed_text).
narrative_ontology:cs_authority_grounding('262db70b-2d7d-47b8-b621-7fa9cb9087ed', distributed).
narrative_ontology:cs_reading_relation('262db70b-2d7d-47b8-b621-7fa9cb9087ed', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('262db70b-2d7d-47b8-b621-7fa9cb9087ed', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('262db70b-2d7d-47b8-b621-7fa9cb9087ed', foundational, monetary_scarcity_supreme).
narrative_ontology:cs_axiom_status(monetary_scarcity_supreme, holdable).
narrative_ontology:cs_axiom_grounding('262db70b-2d7d-47b8-b621-7fa9cb9087ed', monetary_scarcity_supreme, instrumental).
narrative_ontology:cs_axiom('262db70b-2d7d-47b8-b621-7fa9cb9087ed', foundational, settlement_over_payments).
narrative_ontology:cs_axiom_status(settlement_over_payments, holdable).
narrative_ontology:cs_axiom_grounding('262db70b-2d7d-47b8-b621-7fa9cb9087ed', settlement_over_payments, conventional).
narrative_ontology:cs_reference_frame('262db70b-2d7d-47b8-b621-7fa9cb9087ed', digital_gold_paradigm).
narrative_ontology:cs_drift_state('262db70b-2d7d-47b8-b621-7fa9cb9087ed', institutional_custody_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('262db70b-2d7d-47b8-b621-7fa9cb9087ed', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_accumulators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_incumbents).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, p2p_cash_advocates).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, stock_to_flow_model).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, hard_money_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulated Bitcoin at low cost basis; their purchasing power increases as new entrants buy into the scarcity narrative at progressively higher prices. Can exit by selling into the market at any time, capturing the appreciation premium.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Corporate treasuries and ETF structures that accumulate Bitcoin as an inflation hedge and treasury reserve asset. They benefit from regulatory and narrative framing that treats Bitcoin as digital gold rather than a payment network.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_accumulators, beneficiary,
    powerful, generational, arbitrage, global).

% Enforce consensus rules including the block size limit and fee market. They validate transactions and collect fees from scarce block space. Their capital is locked into ASIC hardware specific to the current proof-of-work algorithm and fee-market dynamics.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_incumbents, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, mining_incumbents, beneficiary).

% Enter the market at high prices driven by scarcity expectations and institutional marketing. They transfer purchasing power to early holders when they buy. Their exit options are constrained by sunk cost and the absence of comparable non-extractive store-of-value access within the Bitcoin framework.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrants, payer,
    moderate, biographical, constrained, global).

% Need low-cost payment rails but are priced out of on-chain block space by fee competition. They bear the cost of a settlement layer optimized for large-value transfers, and are forced to custodial or off-chain alternatives that reintroduce counterparty risk.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transactors, payer,
    powerless, immediate, trapped, global).

% Argue that Bitcoin should prioritize cheap peer-to-peer payments and larger blocks. Their preferred scaling path was rejected during the block-size wars. They remain socially and technically marginalized within the BTC consensus process, many having exited to forked chains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, p2p_cash_advocates, excluded,
    moderate, biographical, constrained, global).

% Study Bitcoin's monetary role from outside the holder community. They assess whether the digital gold framing accurately describes a non-sovereign store of value or masks a wealth-transfer mechanism from late entrants to early holders.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, macro_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, early_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global consensus around fixed-supply digital scarcity, creating a non-sovereign settlement asset and inflation hedge that does not depend on any central issuer or monetary policy committee.
% TRANSFER_FUNCTION: Moves purchasing power from late entrants and small transactors to early holders and institutional accumulators through enforced scarcity, appreciation pressure, and fee competition for limited block space. Miners extract fees from the same constrained supply.
% ABSENT_VOICES: Small transactors priced out of on-chain usage; unbanked populations who would need affordable payment access; p2p cash advocates who were socially and technically excluded from the BTC main chain governance; late entrants who have no voice in the monetary policy that governs their cost of entry.
% DISAPPEARANCE_RATIONALE: If the digital gold scarcity framework vanished, the 21M cap narrative would lose its social enforcement, block space might expand, fee markets would collapse, early holders would lose the appreciation premium derived from enforced scarcity, capital would rotate toward payment-optimized alternatives or forked chains, and mining economics would restructure around volume rather than fee scarcity.
% FOUNDING_PROBLEM: The absence of a non-sovereign, censorship-resistant store of value independent of fiat monetary inflation, capital controls, and centralized banking systems.
% FOUNDING_PROBLEM_CORROBORATION: Macro economists outside the Bitcoin holder community contest whether fixed supply alone creates stable store-of-value properties, citing volatility and correlation risk. Emerging-market participants attest the inflation problem is live but are divided on whether Bitcoin's fee structure and appreciation dynamics serve them or extract from them.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) reflects the systematic purchasing power transfer from late entrants to early holders through enforced appreciation and fee competition. Suppression (0.55) captures the social and technical marginalization of alternative scaling paths (e.g., big blocks) that would undermine scarcity. Theater ratio (0.30) acknowledges performative elements in 'hodl' culture and institutional marketing while recognizing that the coordination around scarcity is functionally real. Accessibility collapse (0.45) reflects that while altcoins exist, alternatives within the Bitcoin consensus framework itself are suppressed. Resistance (0.50) measures the ongoing opposition from p2p-cash advocates and forked communities. The measurement series share a single time grid to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   Early holders and institutional accumulators experience the constraint as genuine coordination (scarce digital gold preserving wealth against inflation), while late entrants and small transactors experience it as extraction (paying appreciation premiums and fees for access). The agenda-setter seat (mining incumbents) sits between, extracting fees from constrained block space while enforcing the rules that create the scarcity. The engine will compute divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to early_holders (low d, subsidized by appreciation), institutional_accumulators (low d), and mining_incumbents (moderate d, extracts fees but enforces rules). Victim declarations map to late_entrants (high d, pay premium), small_transactors (high d, priced out), and p2p_cash_advocates (high d, excluded from governance). The directionality derivation amplifies effective extraction for trapped and constrained entrants while damping it for mobile early holders.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this as pure extraction (Snare) because the coordination around fixed-supply scarcity is structurally real and globally valuable to holders seeking non-sovereign stores of value; it prevents mislabeling as pure coordination (Rope) because the fixed supply combined with demand generation creates asymmetric wealth transfer from identifiable late entrants. Were the coordination function absent, the constraint would be a pure speculative snare; were the extraction absent, it would be a neutral coordination rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appreciation_vs_extraction_ambiguity,
    'Is the wealth transfer from late entrants to early holders market-based appreciation or structurally enforced extraction by the scarcity constraint?',
    'Cross-asset comparison with supply-elastic commodities and longitudinal analysis of late-entrant risk-adjusted returns versus other stores of value.',
    'If the transfer is extraction, effective chi rises and the constraint leans snare-like; if it is market appreciation, the constraint is closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appreciation_vs_extraction_ambiguity, empirical, 'Whether scarcity-driven appreciation constitutes extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of p2p cash alternatives structural (protocol rules, mining consensus) or internalized (community identity fused with the scarcity narrative)?',
    'Post-fork trajectory analysis: if p2p-cash advocates remain suppressed on chains where protocol rules differ, the suppression is internalized.',
    'Internalized suppression raises effective extraction for identity-locked participants beyond what structural measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment system frame more naturally as distributed authority over a fixed text, or as an expertise-based interpretation of formalized code?',
    'Analysis of upgrade governance: if Core developers function as de facto canonical interpreters, the authority is expertise/lineage hybrid; if node operators independently veto without developer guidance, the authority is distributed.',
    'Changes authority_grounding and whether interpretation_layer_present is valid, altering the CS classification path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framings of Bitcoin authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 9, 0.25).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 15, 0.3).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 9, 0.48).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 3, 0.3).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
