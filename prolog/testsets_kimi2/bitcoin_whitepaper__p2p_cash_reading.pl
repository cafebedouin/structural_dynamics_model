% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__p2p_cash_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: bitcoin_whitepaper__p2p_cash_reading
 *   human_readable: Bitcoin P2P Cash Reading â Fee Market Exclusion
 *   domain: cryptoeconomic/monetary_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the p2p_cash_reading of the
 *   bitcoin_whitepaper kernel. It models the standing Bitcoin protocol
 *   arrangementâfixed block weight limit, competitive fee market,
 *   proof-of-work consensusâas experienced by a reading that holds
 *   Bitcoin's purpose to be censorship-resistant electronic cash for direct
 *   transactions. From this seat, the fee-market dynamic extracts from
 *   populations that cannot pay rising settlement costs, while miners and
 *   custodial infrastructure capture the revenue and secondary benefits. The
 *   constraint retains a genuine global coordination
 *   functionâcensorship-resistant ledger stateâbut under the p2p_cash
 *   reading the same structure asymmetrically excludes the very users the
 *   kernel was built to serve. Sibling readings (digital_gold_reading,
 *   protocol_ossification_reading) coexist in the same ecosystem but assign
 *   different directionality and beneficiary structures to the identical
 *   protocol rules.
 *
 * KEY AGENTS:
 *   - miners: Primary beneficiary/agenda-setter (organized/arbitrage) â captures fee revenue and enforces consensus
 *   - core_developers: Agenda-setter (organized/mobile) â maintains the protocol specification
 *   - unbanked_populations: Primary target (powerless/constrained) â denied base-layer access by rising fees
 *   - small_merchants: Target (moderate/constrained) â priced out of on-chain commerce
 *   - custodial_exchanges: Secondary beneficiary (institutional/arbitrage) â gains custody volume from high fees
 *   - p2p_cash_advocates: Analytical observer (moderate/mobile) â sees drift from cash function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.72).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.68).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin P2P Cash Reading â Fee Market Exclusion").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptoeconomic/monetary_systems").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, 'f6b18249-3630-4a94-8979-e58639e1e24c').
narrative_ontology:cs_kernel_codification('f6b18249-3630-4a94-8979-e58639e1e24c', fixed_text).
narrative_ontology:cs_authority_grounding('f6b18249-3630-4a94-8979-e58639e1e24c', distributed).
narrative_ontology:cs_reading_relation('f6b18249-3630-4a94-8979-e58639e1e24c', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6b18249-3630-4a94-8979-e58639e1e24c', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('f6b18249-3630-4a94-8979-e58639e1e24c', foundational, inclusive_onchain_cash_mandate).
narrative_ontology:cs_axiom_status(inclusive_onchain_cash_mandate, holdable).
narrative_ontology:cs_axiom_grounding('f6b18249-3630-4a94-8979-e58639e1e24c', inclusive_onchain_cash_mandate, instrumental).
narrative_ontology:cs_axiom('f6b18249-3630-4a94-8979-e58639e1e24c', foundational, negligible_fee_scaling_prerogative).
narrative_ontology:cs_axiom_status(negligible_fee_scaling_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('f6b18249-3630-4a94-8979-e58639e1e24c', negligible_fee_scaling_prerogative, empirically_contingent).
narrative_ontology:cs_reference_frame('f6b18249-3630-4a94-8979-e58639e1e24c', peer_to_peer_electronic_cash).
narrative_ontology:cs_drift_state('f6b18249-3630-4a94-8979-e58639e1e24c', contemporary_fee_market_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f6b18249-3630-4a94-8979-e58639e1e24c', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, custodial_exchanges).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, small_merchants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Validate transactions and collect block subsidies plus fees. Under the fee-market arrangement, they capture rising per-transaction revenue as block space scarcity increases. They signal protocol preferences via mining pools and can redirect hash power across competing chains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, miners, agenda_setter,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, miners, beneficiary).

% Maintain the reference client and protocol specification. Their code defines consensus rules including block weight limits and fee estimation. They can propose changes but cannot unilaterally enforce them; social consensus among node operators and miners is required.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_developers, agenda_setter,
    organized, generational, mobile, global).

% Depend on censorship-resistant payment rails for remittances and savings in economies with failing currencies or capital controls. High on-chain fees price them out of base-layer settlement, forcing reliance on custodial intermediaries or exclusion from the network entirely.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_populations, payer,
    powerless, immediate, constrained, global).

% Accept cryptocurrency for goods and services. Volatile and high on-chain fees make small-value payments uneconomical, pushing them toward custodial processors or off-chain layers that reintroduce counterparty risk.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_merchants, payer,
    moderate, biographical, constrained, national).

% Provide on-ramps and custody. Rising base-layer fees drive retail users into custodial wallets and exchange-managed payment rails, concentrating transaction volume and custody fees in their infrastructure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, custodial_exchanges, beneficiary,
    institutional, biographical, arbitrage, global).

% Advocate for base-layer scaling and low-fee inclusion. They operate nodes, publish research, and forked into separate chains when the base protocol maintained the block limit. They observe the constraint from outside the current consensus power structure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, p2p_cash_advocates, observer,
    moderate, generational, mobile, global).

narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global, peer-to-peer value transfer without trusted intermediaries through proof-of-work consensus and shared ledger state.
% TRANSFER_FUNCTION: Moves transaction fee revenue from users to miners, and displaces user custody from self-custody base layer to custodial intermediaries as on-chain fees rise.
% ABSENT_VOICES: Users in developing economies with sub-dollar transaction budgets are priced out of on-chain voice; big-block advocates who forked to separate chains are excluded from the mainchain consensus conversation.
% DISAPPEARANCE_RATIONALE: If the fee-market block-space constraint disappeared (e.g., unlimited cheap block space with equivalent security), miners would lose fee revenue, custodial exchanges would lose forced off-chain volume, and unbanked users would regain direct base-layer access â the Bitcoin network would reorganize around a different security-subsidy and user-access equilibrium.
% FOUNDING_PROBLEM: Double-spending in digital cash without a trusted third party; creation of a censorship-resistant electronic payment system for online commerce.
% FOUNDING_PROBLEM_CORROBORATION: Early cypherpunk literature and the whitepaper itself attest the founding problem was peer-to-peer electronic cash. Current Bitcoin developers and institutional holders attest the problem has evolved into digital gold settlement; unbanked users and forked-chain communities attest the original problem is unsolved. Corroboration from outside the benefiting parties: academic development economists and human-rights organizations document the ongoing need for low-fee censorship-resistant payments.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the fee market prices out low-value transactions, effectively denying the kernel's cash function to the powerless. Suppression is substantial (0.68): the fee-market outcome is enforced by consensus rules, hash-power signaling, and social sanction against protocol changes that would expand block space. Theater ratio (0.48) reflects performative 'decentralization' rhetoric that obscures the custodial re-intermediation high fees force. Accessibility collapse (0.70) is high within the Bitcoin systemâonce a user is committed to Bitcoin, cheap base-layer alternatives are structurally unavailable. Resistance (0.55) is moderate: the big-block faction mounted significant resistance (BCH fork, ongoing advocacy), but the small-block consensus has prevailed in the main chain.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (miners, developers) experience the constraint as a necessary scarcity mechanism that funds security and prevents spam; the target seats experience it as a paywall that strips Bitcoin of its cash utility. The engine computes this divergence from identical protocol rules because the structural dataâwho collects fees, who is excluded, and what exit options each seat possessesâdiffers. Miners can arbitrage hash power across chains; unbanked users cannot arbitrage their need for censorship-resistant cash.
 *
 * DIRECTIONALITY LOGIC:
 *   Miners and custodial exchanges sit near the beneficiary end: they collect fees and custody volume generated by scarce block space. Unbanked populations and small merchants sit near the target end: they bear the cost of exclusion from the base layer. Core developers sit near symmetric: they neither collect the fees nor pay the exclusion cost directly, but their professional identity is bound to the protocol's current parameters. The p2p_cash advocate observer sits outside the directionality derivation, analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was peer-to-peer electronic cash without trusted third parties. Under the p2p_cash reading, the current arrangement has drifted from that mandate: fees now require trusted third parties (custodial exchanges, Lightning service providers) for small transactions, reintroducing the very intermediaries the kernel sought to eliminate. The constraint is therefore not a pure Rope (the coordination is contaminated by extraction), nor a pure Snare (the ledger coordination is real and non-theatrical), but a Tangled Rope where genuine global settlement coordination coexists with asymmetric extraction through fee-market exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'Is this constraint best understood as a reading of the Bitcoin whitepaper kernel, or as an independent description of Bitcoin''s current protocol dynamics?',
    'Textual analysis of the whitepaper and early protocol behavior against contemporary fee-market dynamics and social-contract discourse.',
    'If independent, the kernel framing is ornamental; if kernel-bound, drift_state and reading relations become primary classification drivers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Committer frame ambiguity for Bitcoin whitepaper kernel').

omega_variable(
    fee_extraction_vs_security_budget,
    'Does the fee market extract from users beyond the necessary cost of proof-of-work security, or does fee revenue precisely fund the security threshold?',
    'Empirical estimation of miner cost structures and security budget requirements at various block sizes and hash-rate levels.',
    'If fees exceed security cost, the excess is extractive overhead supporting higher epsilon; if fees approximate cost, the constraint moves toward rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_extraction_vs_security_budget, empirical, 'Fee revenue and security budget alignment').

omega_variable(
    victim_exit_trapped_or_constrained,
    'Are unbanked users structurally trapped in Bitcoin (no alternative censorship-resistant rails), or merely constrained by current fees?',
    'Cross-asset analysis of censorship-resistant payment options and capital-control evasion pathways in jurisdictions with banking exclusion.',
    'If trapped, directionality and effective extraction are amplified; if constrained, exit options moderate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_exit_trapped_or_constrained, empirical, 'Trapped versus constrained exit for victim populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_p2p_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(btc_p2p_tr_t3, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(btc_p2p_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(btc_p2p_tr_t9, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 9, 0.45).
narrative_ontology:measurement(btc_p2p_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(btc_p2p_tr_t15, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(btc_p2p_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(btc_p2p_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.12).
narrative_ontology:measurement(btc_p2p_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(btc_p2p_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(btc_p2p_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(btc_p2p_be_t15, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 15, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(btc_p2p_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(btc_p2p_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.2).
narrative_ontology:measurement(btc_p2p_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(btc_p2p_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(btc_p2p_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(btc_p2p_su_t15, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the bitcoin_whitepaper kernel, instantiating the p2p_cash_reading. Its sibling readings (digital_gold_reading, protocol_ossification_reading) instantiate structurally distinct constraints from the same kernel. The epsilon values differ because the referentâthe standing arrangement under contestâis assessed through different normative premises: cash utility versus store-of-value scarcity versus change illegitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
