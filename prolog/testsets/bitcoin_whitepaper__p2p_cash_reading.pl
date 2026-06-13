% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper__p2p_cash_reading
 *   human_readable: Bitcoin P2P Cash: Censorship-Resistant Transactional Medium
 *   domain: cryptocurrency/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper describes a peer-to-peer electronic cash system, but
 *   the cryptocurrency has become contested between three incompatible
 *   readings of its purpose and design principles. This constraint story
 *   instantiates the P2P CASH READING: Bitcoin is fundamentally a
 *   censorship-resistant transactional medium optimized for direct payments
 *   at low cost, and the protocol should expand capacity (larger blocks,
 *   lower minimums) to keep transactions affordable and preserve this use
 *   case. This reading is in direct conflict with the digital_gold_reading
 *   (which prioritizes scarcity and store-of-value) and under tension with
 *   the protocol_ossification_reading (which treats block size limits as
 *   sacred consensus boundaries). The P2P cash reading produces a
 *   tangled_rope structure: it coordinates censorship-resistant transaction
 *   routing (genuine coordination benefit) while simultaneously imposing a
 *   fee market that excludes economically marginal users (asymmetric
 *   extraction). This exclusion is not a side effect — it is a direct
 *   consequence of the reading's commitment to capacity limits, which drives
 *   up fees during congestion. The constraint is contested and continuously
 *   reinforced through mining-consensus enforcement.
 *
 * KEY AGENTS:
 *   - Censorship-resistant transaction users (beneficiaries): those whose transactions would be frozen or censored by traditional intermediaries; exit options are mobile but switching costs are high due to liquidity.
 *   - Protocol developers maintaining low-fee pathway (beneficiaries/agenda-setters): argue for capacity expansion; exit is available but fragmentary (fork Bitcoin, lose network effects).
 *   - High-frequency retail transactors and remittance senders (victims): bear the cost of the fee market during congestion; exit is trapped or identity-locked depending on dependence.
 *   - Mining pool operators (agenda-setters): enforce consensus rules by mining; incentivized by fees but also by block reward; exit is arbitrage-available (switch to altcoins).
 *   - Protocol stability advocates (agenda-setters/beneficiaries): resist block size increases; benefit from a natural fee market that they frame as discipline rather than censorship.
 *   - Financial surveillance actors (excluded): structurally denied intermediary power; their exclusion is the mechanism's entire point.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.58).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin P2P Cash: Censorship-Resistant Transactional Medium").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9').
narrative_ontology:cs_kernel_codification('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', fixed_text).
narrative_ontology:cs_authority_grounding('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', distributed).
narrative_ontology:cs_reading_relation('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', foundational, on_chain_transactional_affordability_primary).
narrative_ontology:cs_axiom_status(on_chain_transactional_affordability_primary, holdable).
narrative_ontology:cs_axiom_grounding('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', on_chain_transactional_affordability_primary, conventional).
narrative_ontology:cs_axiom('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', foundational, capacity_expansion_legitimate_under_demand).
narrative_ontology:cs_axiom_status(capacity_expansion_legitimate_under_demand, holdable).
narrative_ontology:cs_axiom_grounding('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', capacity_expansion_legitimate_under_demand, instrumental).
narrative_ontology:cs_reference_frame('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', p2p_cash_original_design).
narrative_ontology:cs_drift_state('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', contemporary_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ddf3805-ad1f-4a80-9dd7-b5f61bb28cb9', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, censorship_resistant_transaction_users).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, protocol_developers_maintaining_low_fee_pathway).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, high_frequency_retail_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, remittance_senders_in_fee_spike_periods).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, economically_marginal_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, large_transaction_initiators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, protocol_stability_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use Bitcoin specifically for transactions that would be censored or frozen by traditional financial rails — cross-border payments under sanctions, payments for goods/services blacklisted by intermediaries, transactions in jurisdictions with capital controls. They benefit from the protocol's design commitment to validate transactions without intermediary approval. Exit is theoretically available (switch to alternative cryptographic systems) but Bitcoin's network effect and established hashrate provide liquidity and security advantages.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, censorship_resistant_transaction_users, beneficiary,
    organized, generational, mobile, global).

% Maintain and advocate for protocol rules that prioritize transaction throughput and fee minimization — larger block sizes, lower minimum relay fee, transaction compression techniques. They argue that Bitcoin's legitimacy as peer-to-peer cash depends on keeping on-chain transactions affordable. Exit: they could fork or migrate to alternative implementations, but doing so fragments the network and sacrifices the coordination benefit of the Bitcoin brand and mining consensus.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, protocol_developers_maintaining_low_fee_pathway, beneficiary,
    organized, generational, arbitrage, global).

% Send small-value transactions (under $50 typical value) in normal market conditions and encounter variable, sometimes prohibitive fees. During network congestion, the fee market prioritizes high-value transactions, pricing retail users out of on-chain settlement. They bear the cost of the constraint through either paying fees that exceed transaction value or deferring settlement. Exit is constrained: they could use second-layer solutions (Lightning Network) but adoption is low, or migrate to altcoins with larger blocks, but sacrificing Bitcoin's network liquidity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, high_frequency_retail_transactors, payer,
    moderate, biographical, constrained, global).

% Migrant workers sending money home cross-border face transaction fees that spike during network congestion, consuming 5–15% of remittance value. The fee market allocates block space to the highest bidders, leaving small remittances to wait or pay premium rates. They are trapped: traditional remittance corridors are expensive and slow; Bitcoin is theoretically cheaper but the fee volatility undermines the value proposition. Exit is unavailable — their need for rapid, low-cost cross-border transfer is constant; the constraint forces them to absorb fee variance or use more expensive alternatives.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, remittance_senders_in_fee_spike_periods, payer,
    powerless, biographical, trapped, global).

% Users in high-inflation or capital-control jurisdictions for whom Bitcoin's censorship resistance is precisely the value proposition, but whose transaction frequency and value are low enough that on-chain fees consume a significant fraction. They are identity-locked: Bitcoin as censorship-resistant store-of-value and transactional medium is their exit from their home financial system — switching to altcoins or abandoning Bitcoin means accepting the same financial exclusion they use Bitcoin to escape. The fee market constraint hits this group hardest because they are the least able to absorb fees and the most dependent on affordable transactional access.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, economically_marginal_users, payer,
    powerless, biographical, identity_locked, global).

% Institutions and high-net-worth actors conducting large transfers ($1M+) benefit from the fee market: their transactions get prioritized without friction, and absolute fee amounts remain economically negligible as a percentage of value moved. They also benefit from Bitcoin's role as censorship-resistant settlement layer for large institutional transfers. Exit is available through traditional banking, but Bitcoin's no-intermediary pathway is preferable for certain cross-border scenarios.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, large_transaction_initiators, beneficiary,
    powerful, generational, arbitrage, global).

% Operate the mining infrastructure that validates transactions and secures the network. They enforce the consensus rules (including block size limits, fee minimums) by choosing which blocks to mine. They are structurally incentivized by a fee market that develops as block space becomes scarce — higher fees mean higher transaction throughput rewards. They set and enforce the constraint through deciding what rule versions to adopt. Exit is theoretically available (mine altcoins) but Bitcoin's hashrate, price, and brand make it the most profitable mining target.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, mining_pool_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Advocate for protocol changes only through near-universal consensus (the protocol_ossification_reading). They argue that limiting block size prevents attacks, preserves full-node accessibility, and protects against centralization. They benefit from a fee market that emerges naturally from scarcity, which they frame as disciplining transaction selection rather than censoring. They enforce the constraint by refusing to adopt block-size-increase proposals and signaling opposition to hardforks. Exit is constrained: they could fork Bitcoin into a high-throughput version (as happened with Bitcoin Cash), but the original Bitcoin brand and hashrate majority stay with the stability-first version.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, protocol_stability_advocates, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, protocol_stability_advocates, beneficiary).

% Government financial intelligence agencies and AML/CFT compliance regimes would prefer transactional control and visibility; they are structurally excluded from the Bitcoin system by its design commitment to censorship resistance. They are trapped — they cannot reverse the protocol design without destroying Bitcoin entirely, so they attempt regulatory workarounds and exchange-level monitoring. Their exclusion is the mechanism itself: the constraint's entire function is to deny them the vetoing power they would exercise.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, financial_surveillance_actors, excluded,
    powerful, generational, trapped, global).

% Develop and maintain off-chain scaling solutions (Lightning Network, sidechains, rollups) that exist precisely because on-chain capacity is constrained. They observe and respond to the fee market signal and the P2P cash reading's prioritization of affordability; the protocol_ossification_reading and fee-market outcome drive their development agenda.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, second_layer_protocol_maintainers, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, mining_pool_operators).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enable direct peer-to-peer electronic cash transactions without intermediary approval, settling censorship-resistant value transfer over a decentralized consensus mechanism. This reading prioritizes the coordination problem it solves: transactions that would be blocked or delayed by traditional financial intermediaries are validated and settled in a predetermined time window regardless of institutional preference.
% TRANSFER_FUNCTION: Moves transaction fees from users to miners as compensation for securing the network and validating transactions. Under this reading, fees should remain low enough that transactional use cases (remittances, merchant payments, censorship-escape transfers) remain viable; the architecture should expand capacity to prevent fee escalation from pricing out low-value use cases.
% ABSENT_VOICES: Users in non-English-speaking regions, users in jurisdictions where Bitcoin adoption is nascent, users whose primary need is store-of-value rather than transactional access. The protocol governance forums are dominated by English-language developer and investor communities; the voices of economically marginal users who depend on affordable transactional access have minimal representation in consensus-building discussions.
% DISAPPEARANCE_RATIONALE: If Bitcoin's censorship-resistant transactional guarantees vanished (either through protocol change imposing intermediary-like validation or through regulatory capture making on-chain settlement effectively blocked), remittance flows would revert to traditional corridors, sanctions-circumvention would shift to altcoins or informal networks, and capital-control-circumvention would resume fiat-smuggling routes. The constraint's disappearance would reallocate $billions in annual cross-border settlement value.
% FOUNDING_PROBLEM: Electronic cash transactions were dependent on trusted third parties (banks, payment processors) who could censor, freeze, or reverse payments. During the 2008 financial crisis and thereafter, governments and intermediaries demonstrated both the ability and willingness to selectively block transactions on political, commercial, or surveillance grounds. Bitcoin's founding problem was to enable transactions that could not be unilaterally censored or reversed.
% FOUNDING_PROBLEM_CORROBORATION: The censorship concern is corroborated by documented cases: US sanctions blocking transactions from Iran, Canada freezing accounts during the truckers' protest, payment processors cutting off WikiLeaks funding, exchanges delisting coins for regulatory compliance. However, the severity of the founding problem is contested: traditional financial systems have legal recourse and consumer protection, while Bitcoin offers neither — whether this is a worse or better tradeoff depends on one's position in the financial hierarchy. The founding problem remains live for marginalized users and contested for the wealthy.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).

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
 *   Extractiveness starts low (0.28) when Bitcoin is nascent and block space is abundant (early interval); as adoption grows and blocks fill, the fee market develops, and extractiveness rises to a plateau around 0.62 as the constraint reaches maturity. The trajectory models the period from ~2011 (abundant capacity) through ~2024 (sustained fee market). Theater_ratio rises in parallel because the narrative shifts: early on, block size limits are justified as technical safety measures; by the later interval, they are justified mainly as preserving full-node accessibility and resisting centralization—both legitimate claims, but increasingly detached from the original transactional-cash use case. By time 10+, the metrics stabilize because the constraint has reached a stable state: blocks are consistently full, fee markets are established, the conflict is frozen by consensus stalemate. Suppression rises less steeply than extractiveness because the suppression is not primarily active enforcement (mining is doing its job) but structural: the architecture itself creates scarcity. Unlike a snare (which requires continuous coercion), this constraint's suppression is baked into the protocol design. The measurement grid shares a common time axis (0–14) where every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The protocol_stability_advocates and mining_pool_operators will compute the constraint as a rope or legitimate coordination mechanism because they benefit from the fee market and frame it as market discipline. The censorship_resistant_transaction_users and low-fee advocates will compute it as a rope-boundary case with significant extraction, because they see the scarcity as a constraint on the original use case. The economically_marginal_users will compute it as a snare: the extraction is imposed on the one population for whom exit is identity-locked (they cannot abandon Bitcoin without sacrificing censorship resistance). This divergence is structural and irreducible—it follows from the incompatibility between the P2P cash reading's design commitments (capacity constraints for security) and its expressed goals (affordable transactional access for all). The engine should compute this divergence cleanly from the declared beneficiary/victim structure and exit_options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries benefit from censorship-resistant transactional guarantees and low fees (when blocks are not full). Their exit cost is high because switching to altcoins sacrifices Bitcoin's network liquidity and brand. Derived d values: censorship_resistant_transaction_users d~0.25 (net beneficiary), protocol_developers d~0.30 (net beneficiary but constrained by consensus). Victims bear the cost of the fee market during congestion. High_frequency_retail_transactors have moderate exit options (d~0.70); remittance_senders and economically_marginal_users are trapped or identity-locked (d~0.88–0.95). Large_transaction_initiators benefit from fee markets (d~0.15–0.25). Mining_pool_operators benefit from fees as block reward supplement and have arbitrage exit (d~0.35–0.40). Stability advocates benefit from fee market discipline and governance stability (d~0.35). No overrides are needed; the structural derivation from beneficiary/victim + exit should produce accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The P2P cash reading contains a buried mandate obsolescence: the founding problem (censorship of transactions by intermediaries) is solved by the protocol design, but the mandate to keep transactions affordable has been superseded by the protocol stability reading's mandate to enforce consensus through scarcity. The original founding mandate was to replace intermediaries with p2p routing; the ossified consensus has replaced that mandate with 'protocol stability above all,' which delegitimizes the original use case. This is not a full mandatrophy resolution because the reading maintains a live constituency (censorship-resistant users still benefit, even if transactional affordability erodes). However, the mounting tension between the founding mandate (cheap cash for everyone) and the actual constraint (fee market that prices out low-value transactions) is a key site of conflict. The measurement series shows base_extractiveness rising from 0.28 to 0.62, modeling the accumulation of rent-seeking behavior as the original mandate dissolves. By time 12–14, the constraint has stabilized at high extractiveness despite the reading's claim to be fundamentally about P2P cash—this is the mandatrophy signal: the mandate survives in rhetoric but has been functionally superseded by the protocol_ossification_reading's commitment to stability and scarcity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_mandate_obsolescence,
    'Has the founding problem (censorship of transactions by intermediaries) been sufficiently solved by the protocol design such that the mandate to keep transactions affordable is now secondary to other concerns?',
    'Examine the actual usage distribution: if transaction volume is dominated by store-of-value holding and large institutional transfers rather than retail transactional use, the original mandate has been functionally superseded even if the reading preserves it rhetorically.',
    'If the founding mandate is obsolete, this constraint should be reclassified as a piton (ceremonial maintenance of an original commitment that no longer drives the system) rather than a tangled_rope with live coordination and extraction functions. The measurement series showing stabilizing metrics at high extractiveness supports this reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_mandate_obsolescence, empirical, 'Whether the P2P cash reading''s founding mandate is still live or has been replaced by the protocol_ossification_reading''s mandate.').

omega_variable(
    alternative_capacity_solutions,
    'Are off-chain and second-layer scaling solutions (Lightning Network, sidechains, rollups) adequate substitutes for on-chain transactional capacity, or does the P2P cash reading''s commitment require on-chain affordability?',
    'Observe adoption and usage patterns of second-layer solutions; if remittance and low-value-transaction volume migrates to Lightning while on-chain becomes institution-only, the reading''s mandate has been implicitly redefined to accept bifurcation rather than preserve unified transactional access.',
    'If second-layer solutions are viewed as adequate, the extractive effect of on-chain capacity limits is dampened—some of the victim population can route around the constraint. If they are viewed as insufficient (because they require trust assumptions or liquidity provisioning), the extraction is more severe and the constraint''s type remains tangled_rope. This is a key uncertainty because the reading''s authors are deeply divided on this question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capacity_solutions, conceptual, 'Whether second-layer solutions satisfy or substitute for the P2P cash reading''s mandate for affordable on-chain transactional access.').

omega_variable(
    censorship_resistance_supply_vs_demand,
    'Is censorship resistance a use case with sufficient and stable demand to justify keeping transaction fees low, or has the realized demand been smaller and more episodic than the reading assumes?',
    'Track the percentage of Bitcoin on-chain transaction volume attributable to censorship-escape use cases (sanctions circumvention, political persecution, capital controls) versus store-of-value and speculation; if the proportion is <5% and declining, the reading''s primary beneficiary is smaller than claimed.',
    'If censorship-resistance use cases are a small fraction of volume, the reading overstates the importance of transactional affordability and understates the importance of other design goals (scarcity, security, immutability). This would shift the constraint''s classification toward the digital_gold reading and protocol_ossification reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_resistance_supply_vs_demand, empirical, 'The actual size and persistence of demand for Bitcoin''s censorship-resistance transactional properties.').

omega_variable(
    reading_foreclosure_possibility,
    'Can the P2P cash reading logically coexist with the protocol_ossification reading within a single governance framework, or does the commitment to ''fees should stay low'' fundamentally foreclose the commitment to ''protocol changes require near-universal consensus''?',
    'Examine the governance history: every attempt to expand block size (Bitcoin Classic, Bitcoin Unlimited, Bitcoin Cash fork) has been blocked by the stability-first coalition, suggesting that the two readings are in structural conflict — one reading''s preferred solution (hard fork to expand blocks) is the other reading''s nightmare scenario (contentious protocol change).',
    'If the readings foreclose each other, they cannot coexist within the Bitcoin protocol itself — one reading must eventually win or the network must fork into separate implementations. If they coexist, it is only because the protocol_ossification reading has won consensus governance while the P2P cash reading persists as a minority position with no formal power to enforce its design principles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether the P2P cash and protocol_ossification readings are logically compatible or mutually foreclosing within a single governance framework.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity-locking of economically_marginal_users to Bitcoin structural (they literally cannot access censorship-resistant transactional infrastructure without Bitcoin) or ideological (they have absorbed the Bitcoin narrative and cannot imagine alternatives)?',
    'Post-exit suppression trajectory: if economically marginal users migrate to altcoins with lower fees but higher censorship vulnerability, and their financial condition improves, the identity lock was partly ideological. If they remain trapped because no altcoin offers equivalent censorship resistance, the lock is structural.',
    'If the lock is ideological, the constraint''s effective suppression is higher than the structural measure suggests, because the target population carries the exclusion psychology with them even when alternatives exist. If the lock is structural, the constraint''s extraction is justified by the absence of alternatives and should be evaluated against the benefit of censorship resistance rather than labeled as pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether economically marginal users are trapped by Bitcoin''s lack of alternatives or by ideological commitment to Bitcoin''s brand and narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(bitc_tr_t0, projected).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t2, projected).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(bitc_tr_t10, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t14, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 14, 0.41).
narrative_ontology:measurement_basis(bitc_tr_t14, projected).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(bitc_be_t0, projected).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2, 0.35).
narrative_ontology:measurement_basis(bitc_be_t2, projected).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 4, 0.43).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(bitc_be_t10, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t14, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 14, 0.62).
narrative_ontology:measurement_basis(bitc_be_t14, projected).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(bitc_su_t0, projected).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2, 0.42).
narrative_ontology:measurement_basis(bitc_su_t2, projected).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(bitc_su_t10, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t14, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 14, 0.58).
narrative_ontology:measurement_basis(bitc_su_t14, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__p2p_cash_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper kernel has been decomposed into three constraint stories, each instantiating a different reading of the whitepaper's core claims. This story (P2P CASH READING) models Bitcoin as a censorship-resistant transactional medium, prioritizing affordable on-chain transaction access. The digital_gold_reading models Bitcoin as a scarce store of value, prioritizing protocol immutability and scarcity preservation. The protocol_ossification_reading models Bitcoin as bound to its current protocol specification unless approaching universal consensus change. These three readings have structurally incompatible design goals and victim/beneficiary structures. Each story carries its own ε-invariant metrics, stakeholder set, and mandatrophy state. They are linked via network.affects_constraints because governance decisions in one reading directly constrain the feasibility of the others—a block size increase would satisfy the P2P reading but would trigger the protocol_ossification reading's foreclosure condition and undermine the digital_gold reading's scarcity commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
