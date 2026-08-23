% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin Digital Gold Reading — Scarcity-Optimized Store of Value
 *   domain: economic/technological/governance
 *
 * SUMMARY:
 *   This story instantiates the digital_gold_reading of the
 *   bitcoin_whitepaper kernel: Bitcoin as a scarce digital asset optimized
 *   for store of value and inflation hedging, where asset appreciation is
 *   prioritized and transaction fees are an acceptable cost of security. The
 *   constraint is the 21M supply cap + halving schedule + fee market that
 *   together create artificial scarcity extracting from late entrants.
 *   Beneficiaries (early adopters, long-term holders, miners, institutions)
 *   capture appreciation; victims (late entrants, high-frequency users,
 *   merchants, developing-nation users) pay the premium. The claimed type is
 *   tangled_rope — genuine coordination (decentralized consensus, credible
 *   scarcity) coexists with asymmetric extraction (early adopters enriched at
 *   late entrants' expense, fee market prices out low-value use). The engine
 *   computes per-seat classifications from this structural data; the authored
 *   claim and metrics are independent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.72).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.48).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin Digital Gold Reading — Scarcity-Optimized Store of Value").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "economic/technological/governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'b6fd9959-917e-4994-a4da-797944d8ec80').
narrative_ontology:cs_kernel_codification('b6fd9959-917e-4994-a4da-797944d8ec80', fixed_text).
narrative_ontology:cs_authority_grounding('b6fd9959-917e-4994-a4da-797944d8ec80', lineage).
narrative_ontology:cs_interpretation_layer_present('b6fd9959-917e-4994-a4da-797944d8ec80').
narrative_ontology:cs_reading_relation('b6fd9959-917e-4994-a4da-797944d8ec80', bitcoin_whitepaper__p2p_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6fd9959-917e-4994-a4da-797944d8ec80', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('b6fd9959-917e-4994-a4da-797944d8ec80', foundational, scarcity_above_throughput).
narrative_ontology:cs_axiom_status(scarcity_above_throughput, holdable).
narrative_ontology:cs_axiom_grounding('b6fd9959-917e-4994-a4da-797944d8ec80', scarcity_above_throughput, deontological).
narrative_ontology:cs_axiom('b6fd9959-917e-4994-a4da-797944d8ec80', secondary, fee_market_as_security_budget).
narrative_ontology:cs_axiom_status(fee_market_as_security_budget, holdable).
narrative_ontology:cs_axiom_grounding('b6fd9959-917e-4994-a4da-797944d8ec80', fee_market_as_security_budget, instrumental).
narrative_ontology:cs_reference_frame('b6fd9959-917e-4994-a4da-797944d8ec80', satoshi_whitepaper_vision).
narrative_ontology:cs_drift_state('b6fd9959-917e-4994-a4da-797944d8ec80', post_institutional_adoption_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6fd9959-917e-4994-a4da-797944d8ec80', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_treasuries).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, high_frequency_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, merchants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, developing_nation_users).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, fixed_supply_monetary_policy).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, decentralized_consensus_security).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, non_sovereign_store_of_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired BTC at negligible cost during 2009-2013; hold concentrated supply. Their wealth grows with every halving and adoption wave. They shape narrative through forums, conferences, and early institutional access. Exit is trivial — they can sell any amount into deep liquidity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopters, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, early_adopters, agenda_setter).

% Accumulated BTC over multiple cycles; treat it as primary savings vehicle. Benefit from appreciation but lack the agenda-setting concentration of early adopters. Can exit to fiat or other assets but face tax events and slippage at size.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Secure the network and enforce protocol rules (including 21M cap). Revenue shifts from block subsidy to transaction fees over time. Their capital is specialized (ASICs, energy contracts) — exit means selling hardware at depreciation or switching chains with different economics.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, miners, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, miners, beneficiary).

% Allocate treasury reserves to BTC as inflation hedge (MicroStrategy model, ETFs). Benefit from appreciation narrative they help legitimize. Exit is liquid but reputational — selling signals loss of conviction in the thesis.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_treasuries, beneficiary,
    institutional, generational, mobile, global).

% Enter at progressively higher price levels; each halving reduces new supply while demand grows. Pay premium to early holders for the same scarcity guarantee. Exit requires finding a later entrant — the Ponzi-adjacent dynamic where late buyers fund early sellers.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrants, payer,
    moderate, biographical, constrained, global).

% Need cheap, reliable on-chain transactions (remittances, daily spending, DeFi). Fee market prices them out during congestion — they pay miner fees that subsidize network security but make small transactions uneconomical. Exit to L2s or altchains involves technical friction and trust trade-offs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, high_frequency_users, payer,
    moderate, immediate, constrained, global).

% Want to accept BTC for goods/services but face volatile fees and settlement uncertainty. The digital gold reading treats merchant adoption as secondary — fees are a feature, not a bug. Many dropped BTC payments after 2017 fee spike; those remaining use custodial processors that reintroduce counterparty risk.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, merchants, payer,
    moderate, biographical, constrained, global).

% In high-inflation economies (Argentina, Turkey, Nigeria, etc.) need censorship-resistant savings AND cheap payments. Digital gold reading prices them out of both — on-chain fees exceed daily wages; L2 access requires smartphones, data, and technical literacy they lack. Their exclusion is structural: the fee market optimizes for high-value settlement, not inclusion.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, developing_nation_users, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, developing_nation_users, excluded).

% Maintain reference implementation, propose BIPs, review code. Their authority derives from technical competence and community trust. They interpret the whitepaper's intent but cannot force upgrades — consensus requires miner/node signaling. Exit means forking (which has failed repeatedly) or walking away.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, protocol_developers, agenda_setter,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, protocol_developers, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, censorship-resistant scarce asset with predictable, algorithmic issuance that no single party can debase — solving the coordination problem of trust-minimized digital scarcity.
% TRANSFER_FUNCTION: Moves purchasing power from late entrants (buying at higher prices) to early holders via appreciation; moves transaction fees from on-chain users to miners as block subsidy declines. The fee market allocates scarce blockspace to highest bidders.
% ABSENT_VOICES: Unbanked populations in high-inflation economies who need both store-of-value AND medium-of-exchange; merchants who need predictable low fees; future generations who inherit a system where 90%+ of supply is already issued. They are absent because the fee market and price appreciation structurally exclude low-value, high-frequency use cases.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the $1T+ institutional store-of-value allocation would seek alternatives (gold, other crypto, TIPS), miner economics would collapse without fee revenue, and the 'digital gold' narrative legitimizing crypto as asset class would evaporate — capital and talent would reorganize around competing scarcity claims.
% FOUNDING_PROBLEM: Need for a decentralized, non-sovereign store of value resistant to monetary debasement by central banks — a digital asset with absolute scarcity enforced by code, not policy.
% FOUNDING_PROBLEM_CORROBORATION: Independent macro analysts (e.g., Lyn Alden, Zoltan Pozsar) document ongoing fiat debasement trends; institutional investors outside early adopter circle (Fidelity, BlackRock ETF filings) cite inflation hedge as primary thesis; central bank researchers (BIS, Fed papers) acknowledge demand for non-sovereign reserves. No corroboration from the beneficiary set alone.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the supply cap creates a zero-sum transfer from buyers to holders — every late entrant pays a scarcity premium that accrues to early holders. Suppression (0.48) is moderate: protocol rules suppress alternative monetary policies (no debasement, no tail emission), but exit to other chains/L2s exists (unlike a true snare). Theater (0.28) is low-moderate: the protocol functions as designed, but 'store of value' narrative increasingly serves to justify fee market that excludes the very users Satoshi's whitepaper centered ('peer-to-peer electronic cash'). Accessibility collapse (0.55) reflects network effects and liquidity moats — alternatives exist but lack Bitcoin's credibility. Resistance (0.45) captures ongoing blocksize wars, Lightning adoption friction, and Ossification debate.
 *
 * PERSPECTIVAL GAP:
 *   From the early-adopter/institutional seat, this is a mountain-like coordination achievement: absolute scarcity enforced by math, not trust. From the developing-nation-user seat, it is a snare: the same scarcity prices them out of the financial inclusion the whitepaper promised. From the merchant/high-frequency seat, it is a piton: the payment function atrophied while the store-of-value narrative persists theatrically. The engine computes this seat divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and institutional treasuries are structural beneficiaries (d ~ 0.1-0.2) — they collect appreciation and/or legitimize the asset. Miners are agenda-setters with partial beneficiary position (d ~ 0.3) — they enforce rules but depend on fee revenue. Late entrants, high-frequency users, and merchants are payers (d ~ 0.7-0.8) — they pay scarcity premium and fees with constrained exit. Developing-nation users are trapped payers (d ~ 0.9) — priced out of both savings and payment functions. Protocol developers sit near analytical (d ~ 0.5) — they maintain the code but cannot unilaterally change economics.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (non-sovereign store of value) remains live — fiat debasement continues globally. However, the arrangement's persistence now depends on extraction from late entrants rather than solving the original coordination problem for all users. The digital gold reading has mandatrophy in the payment function (atrophied to L2s) but not in the store-of-value function (still live). This prevents mislabeling the whole as pure extraction — the coordination function is real but asymmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_scarcity,
    'Is Bitcoin''s 21M cap a discovered natural law of digital scarcity (mountain) or a constructed parameter that benefits identifiable early adopters (false summit)?',
    'Counterfactual analysis: if Satoshi had chosen 210M or 2.1M, would the coordination function hold? If yes, the specific number is constructed; if no, there is something special about 21M. Also: measure whether early-adopter concentration correlates with cap choice across forked chains.',
    'If constructed, FSM signature triggers reclassification to tangled_rope (already claimed) with false_summit_override_target; if natural law, mountain certification path opens but requires accessibility_collapse ~0.9+ and resistance ~0.05 which this constraint does not meet.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_scarcity, conceptual, 'Whether the scarcity parameter is a natural limit or a parameter choice that enriches early participants.').

omega_variable(
    fee_market_coordination_or_extraction,
    'Does the fee market coordinate blockspace allocation efficiently (rope) or extract from low-value users to subsidize miner revenue (snare component)?',
    'Compare Bitcoin''s fee market outcomes to theoretical efficient allocation (Vickrey auction benchmarks) and to alternative designs (EIP-1559, fixed-fee chains). Measure deadweight loss from users priced out vs. security budget funded.',
    'If coordination dominates, the tangled_rope''s coordination function is genuine; if extraction dominates, the snare component is larger than authored and the constraint may compute as snare for payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fee_market_coordination_or_extraction, empirical, 'Whether the fee market is a genuine coordination mechanism or an extractive barrier.').

omega_variable(
    lightning_resolves_victim_set,
    'Does Lightning Network adoption structurally resolve the victim set (late entrants, high-frequency users, merchants, developing-nation users) or merely shift extraction to L2 operators?',
    'Track Lightning adoption metrics: node count, channel capacity, routing success rates, fee levels, custodial vs. non-custodial usage. Measure whether developing-nation users access non-custodial Lightning at scale.',
    'If Lightning resolves victim set at scale, the extraction component shrinks and the constraint may compute as rope for more seats. If custodial capture persists, the victim set remains and the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lightning_resolves_victim_set, empirical, 'Whether L2 scaling genuinely includes excluded users or recreates extraction at another layer.').

omega_variable(
    committer_structure_kernel_reading,
    'How does the digital_gold_reading''s structural commitment to scarcity-above-throughput foreclose, coexist with, or influence the sibling readings of the bitcoin_whitepaper kernel?',
    'Map the logical relations: does scarcity_above_throughput axiom foreclose p2p_cash_reading''s low-fee requirement? Does institutional adoption (digital_gold) create political pressure for ossification (influences protocol_ossification_reading)?',
    'Determines cs_structure.reading_relations and whether axioms are holdable or overridden. If digital_gold_reading forecloses p2p_cash_reading within a single framework, they cannot coexist in one protocol governance — one must exit (BCH fork) or submit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Structural relations between this reading and its kernel siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_dg_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(btc_dg_tr_t3, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(btc_dg_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(btc_dg_tr_t9, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(btc_dg_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(btc_dg_tr_t15, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 15, 0.28).

% Extraction over time
narrative_ontology:measurement(btc_dg_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(btc_dg_be_t3, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(btc_dg_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(btc_dg_be_t9, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 9, 0.63).
narrative_ontology:measurement(btc_dg_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(btc_dg_be_t15, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 15, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(btc_dg_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(btc_dg_su_t3, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 3, 0.3).
narrative_ontology:measurement(btc_dg_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(btc_dg_su_t9, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 9, 0.43).
narrative_ontology:measurement(btc_dg_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(btc_dg_su_t15, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_lightning_network).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_institutional_custody).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_miner_economics).

% DUAL FORMULATION NOTE:
% This constraint (digital_gold_reading) and p2p_cash_reading decompose the 'Bitcoin' label into two ε-distinct constraints: one with high extractiveness (store-of-value scarcity premium) and one with low extractiveness (medium-of-exchange fee market). They share the same kernel (whitepaper + consensus rules) but instantiate different constraints per the ε-invariance principle. The protocol_ossification_reading is a meta-constraint on the kernel's evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__digital_gold_reading, institutional, 0.15).
constraint_indexing:directionality_override(bitcoin_whitepaper__digital_gold_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
