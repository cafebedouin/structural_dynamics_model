% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: bitcoin_whitepaper__p2p_cash_reading
 *   human_readable: Bitcoin Blockspace Fee-Market Regime (P2P-Cash Reading)
 *   domain: economic/technological/monetary
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel
 *   'bitcoin_whitepaper': the p2p_cash_reading, which holds that the founding
 *   document's defining promise is censorship-resistant, low-cost, direct
 *   electronic payment, and that arrangements abandoning that promise betray
 *   the kernel. The standing arrangement under contest, and therefore the
 *   referent of every metric here, is the actual operating regime of the
 *   Bitcoin mainnet: a deliberately capped blockspace cleared by fee auction,
 *   governed by a developer-miner-holder consensus that has rejected on-chain
 *   capacity expansion since the 2015-2017 blocksize wars. Assessed by this
 *   reading's own lights, that regime retains a genuine coordination core
 *   (permissionless, intermediary-free settlement that no participant can
 *   unilaterally alter) while rationing transactional access by willingness
 *   to pay: during the congestion episodes of 2013, 2017, 2021, and 2024-25,
 *   fees priced out precisely the users the whitepaper addressed. The sibling
 *   readings, digital_gold_reading (scarcity-first store of value) and
 *   protocol_ossification_reading (stability as the primary virtue), are
 *   separate constraint stories with their own epsilon values and victim
 *   sets; they are linked, not averaged, here. KEY AGENTS (by structural
 *   relationship): - bitcoin_miners: Primary beneficiary and enforcer
 *   (organized/constrained) — collects fees and subsidy, signals on protocol
 *   changes - core_protocol_developers: Agenda-setter
 *   (institutional/identity_locked) — gates what counts as a legitimate
 *   modification - large_holders_and_institutions: Secondary beneficiary
 *   (powerful/arbitrage) — finances advocacy, gains from fee-funded security
 *   - remittance_corridor_users: Primary target (powerless/trapped) — the
 *   whitepaper's paradigm users, priced out at peaks -
 *   small_value_transactors: Target (powerless/constrained) — bids or waits
 *   during congestion - full_node_operators: Dual cost-bearing seat
 *   (organized/identity_locked) — bears expansion costs, collects validation
 *   independence - lightning_routing_operators: Incidental beneficiary
 *   (moderate/mobile) — demand derives from on-chain scarcity -
 *   big_block_fork_community: Excluded dissenter (moderate/mobile) — lost the
 *   consensus war, continues on the fork - cryptoeconomics_researchers:
 *   Analytical observer — sees the full structure, collects nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.66).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin Blockspace Fee-Market Regime (P2P-Cash Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "economic/technological/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, 'c10d5d67-9627-41dc-ae35-63598bd3527f').
narrative_ontology:cs_kernel_codification('c10d5d67-9627-41dc-ae35-63598bd3527f', fixed_text).
narrative_ontology:cs_authority_grounding('c10d5d67-9627-41dc-ae35-63598bd3527f', lineage).
narrative_ontology:cs_interpretation_layer_present('c10d5d67-9627-41dc-ae35-63598bd3527f').
narrative_ontology:cs_reading_relation('c10d5d67-9627-41dc-ae35-63598bd3527f', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('c10d5d67-9627-41dc-ae35-63598bd3527f', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('c10d5d67-9627-41dc-ae35-63598bd3527f', foundational, whitepaper_title_binds_cash_purpose).
narrative_ontology:cs_axiom_status(whitepaper_title_binds_cash_purpose, holdable).
narrative_ontology:cs_axiom_grounding('c10d5d67-9627-41dc-ae35-63598bd3527f', whitepaper_title_binds_cash_purpose, conventional).
narrative_ontology:cs_axiom('c10d5d67-9627-41dc-ae35-63598bd3527f', foundational, fee_markets_must_not_ration_transactional_access).
narrative_ontology:cs_axiom_status(fee_markets_must_not_ration_transactional_access, holdable).
narrative_ontology:cs_axiom_grounding('c10d5d67-9627-41dc-ae35-63598bd3527f', fee_markets_must_not_ration_transactional_access, empirically_contingent).
narrative_ontology:cs_axiom('c10d5d67-9627-41dc-ae35-63598bd3527f', secondary, on_chain_capacity_expansion_is_legitimate).
narrative_ontology:cs_axiom_status(on_chain_capacity_expansion_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c10d5d67-9627-41dc-ae35-63598bd3527f', on_chain_capacity_expansion_is_legitimate, instrumental).
narrative_ontology:cs_reference_frame('c10d5d67-9627-41dc-ae35-63598bd3527f', nakamoto_p2p_cash_mandate).
narrative_ontology:cs_drift_state('c10d5d67-9627-41dc-ae35-63598bd3527f', post_blocksize_war_fee_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c10d5d67-9627-41dc-ae35-63598bd3527f', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, large_holders_and_institutions).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, lightning_routing_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, remittance_corridor_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, small_value_transactors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, full_node_operators).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, fee_market_security_funding_thesis).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, blockspace_scarcity_value_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate specialized hardware producing proof-of-work for the network and are paid the block subsidy plus whatever fees transactors attach. Hardware is single-purpose and capital-intensive, so switching to another chain means writing off rigs. They signal approval or opposition to protocol changes through block version bits and public statements, and fee spikes during congestion periods are direct revenue windfalls.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, beneficiary).

% Maintain the reference software most of the network runs, review and merge proposed changes, and act as the de facto gatekeepers of what counts as a safe modification. Their standing rests on years of accumulated credibility, and many hold significant amounts of the asset itself. Departure to a rival chain is widely read inside the community as betrayal, so leaving carries a reputational price beyond forgone income.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_protocol_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold large positions acquired as long-term savings. They do not operate infrastructure but finance development through grants and foundations, fund advocacy and public-relations efforts, and benefit when ledger security is paid for by transaction fees rather than new issuance, because issuance dilutes their holdings. They can rebalance into other assets at will, giving them more mobility than any operator seat.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, large_holders_and_institutions, beneficiary,
    powerful, generational, arbitrage, global).

% Use the network for everyday-sized payments: purchases, tips, small transfers. When demand for blockspace spikes they must outbid larger transactions, wait hours or days for confirmation, or move off-chain. Alternative cryptocurrencies offer weaker protection against interference and shallower liquidity; bank rails are unavailable or hostile in much of the world they live in.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_value_transactors, payer,
    powerless, immediate, constrained, global).

% Send earnings across borders, the use case the founding document named directly. Their alternatives, money transmitters and correspondent banks, charge high percentages and can freeze accounts, which is precisely what drew them to a permissionless rail. When fees spike past ten dollars per transfer, sending two hundred dollars home stops making sense, and they have nowhere comparable to go.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, remittance_corridor_users, payer,
    powerless, immediate, trapped, regional).

% Run independent copies of the ledger to verify transactions without trusting anyone. Larger blocks raise their storage and bandwidth bills; smaller blocks keep verification cheap but leave less room for transactions. They treat running a node as constitutive of participation, so shutting it down to escape costs would defeat the reason they run it. As a class they demonstrated coordinated power in the 2017 user-activated soft fork episode.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, full_node_operators, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, full_node_operators, beneficiary).

% Operate payment channels and routing capital on the second-layer network built atop the main ledger. Their business exists because on-chain space is scarce and costly; every congestion episode drives users toward their services. They collect small routing fees and manage liquidity, and can redeploy capital elsewhere if the layer loses relevance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, lightning_routing_operators, beneficiary,
    moderate, biographical, mobile, global).

% Argued through 2015-2017 that the block size limit should be raised to preserve cheap on-chain payments, lost the consensus contest, and carried their program to a forked chain. On the main network they retain no voice in governance; their continued existence on the fork demonstrates that exit was possible and costly, since they surrendered the brand, liquidity, and developer gravity of the original network.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, big_block_fork_community, excluded,
    moderate, biographical, mobile, global).

% Publish analyses of fee dynamics, security budgets, and adoption patterns. They neither collect nor pay inside the arrangement; their papers are cited by every faction when convenient and ignored when not.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, cryptoeconomics_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates thousands of mutually distrusting nodes and miners on a single append-only ledger with no trusted intermediary, and rations scarce blockspace among competing transactions by fee auction so that full verification remains affordable for ordinary participants.
% TRANSFER_FUNCTION: Moves transaction fees, plus a declining block subsidy, from transactors to miners; moves timely settlement assurance to whichever transactions outbid the rest; and accrues scarcity value to holders as issuance falls.
% ABSENT_VOICES: Would-be users priced out during congestion, remittance senders and small merchants in fragile-currency economies, hold no seat in protocol governance; the big-block faction speaks only from its fork. They sit outside the developer-miner-holder consensus loop, on rails they cannot influence, which is the condition the founding document was written against.
% DISAPPEARANCE_RATIONALE: Overnight removal of the blockspace cap and fee auction would collapse miner fee revenue toward the subsidy, reopening the security-budget question; holder scarcity narratives and the layer-two industry built on costly on-chain settlement would lose their premises; node storage costs would jump; and the 2017 governance settlement would be void, forcing every faction to renegotiate.
% FOUNDING_PROBLEM: Nakamoto 2008: internet commerce relies on trusted third parties, financial institutions, to mediate electronic payments; the founding problem was enabling direct online payments between parties without going through a financial institution, solving double-spending without trust.
% FOUNDING_PROBLEM_CORROBORATION: Partial and contested. Outside the benefiting parties: academic payment-systems research documents the decline of on-chain retail payment share after each fee crisis; commercial chain-analytics firms publish fee and adoption series no faction controls; international standard-setting bodies and banking regulators attest the censorship and account-freezing pressures that motivate the cash use case. No neutral party attests the full reading, since the store-of-value and ossification factions dispute that the founding problem remains the binding one, and that dispute is itself the signal.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.62: baseline fees are often modest and censorship resistance persists for those who pay, but congestion episodes repeatedly price basic transactions out of the ledger entirely, which from this reading's seat is a real transfer from small users to fee collectors. Suppression 0.66 reflects the enforcement machinery revealed by the blocksize wars: forum moderation, fork stigma, the ossification orthodoxy, and the demonstrated cost of exit (the forked chain lost brand, liquidity, and developers). Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater 0.32: validation and consensus are real work, but a growing share of ecosystem discourse is performative decentralization talk over concentrated mining pools. Accessibility collapse 0.55: alternatives exist (rival chains, layer two, fiat rails) but each collapses substantially on contact — the fork lost network gravity, the layer inherits the base layer's settlement assumptions, fiat rails censor. Resistance 0.72: few monetary arrangements have met five years of open factional warfare plus continuing contention. The measurement series runs on one shared nine-point grid (2009-2025) and shows a cyclical pattern: extraction and enforcement spike with speculative-demand cycles (2013, 2017, 2021, 2024-25 inscription and rune congestion) and relax between peaks. The oscillation is partly an extraction mechanism in itself: each congestion episode normalizes fee-market acceptance ('fees secure the chain'), pushes marginal users off-chain permanently, and thereby shrinks the cash constituency that would resist the next round — intermittent reinforcement, not noise. Endpoint values match the base_properties scalars. The claim (tangled_rope) and the metrics are authored independently; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the miner and core-developer seats the same arrangement computes as functioning coordination they administer and are paid for; from the remittance and small-transactor seats it computes as a toll gate that excludes them exactly when they need passage. Large holders experience it as protective scarcity; node operators experience it as a bargain they struck knowingly. The engine computes these per-seat classifications from power, exit options, and directional position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Miners are declared beneficiaries with constrained exit: the fee stream lands on them, so their derived directionality sits near the beneficiary pole despite their enforcement role. Large holders benefit indirectly (fee-funded security preserves the monetary premium) and hold arbitrage-grade exit, placing them nearest the beneficiary end. Lightning operators benefit incidentally from scarcity-driven demand. Remittance users and small transactors are declared victims with trapped-to-constrained exit, placing them near the full-target pole; trapped remittance corridors sit nearer than constrained domestic transactors. Full node operators are dual-positioned (bear expansion costs, collect validation independence) and are deliberately left out of the beneficiary/victim arrays so their near-symmetric position is not forced to one pole. No directionality override is authored: the override mechanism keys on power atoms, and miners and node operators share the 'organized' level, so any override correcting one would distort the other.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, trust-free electronic payment, is contested rather than dead: the cash constituency argues it is unfulfilled on-chain and merely relocated to layer two; the store-of-value constituency argues the kernel's realized function outgrew it. Authoring the arrangement as tangled_rope prevents two symmetrical errors: reading pure extraction into a fee market that genuinely funds censorship-resistant settlement, and reading pure coordination into a regime that rations access by wealth. The partial displacement of the cash function to layer two is tracked as an omega rather than resolved by assertion, so a future verdict that the mandate has atrophied into performance would have to earn itself from data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'This constraint is one reading of the kernel bitcoin_whitepaper; which reading correctly identifies the kernel''s binding content — the cash promise, the scarcity thesis, or the stability covenant?',
    'Adoption and market evidence over time: sustained retail payment utility favors this reading; persistent accumulation-and-hold behavior with negligible circulation favors digital_gold_reading; continued refusal of all protocol change favors protocol_ossification_reading.',
    'Resolution reallocates the victim set and reclassifies the fee market: under this reading it extracts from priced-out spenders; under digital_gold_reading it is necessary security funding; under protocol_ossification_reading it is the kernel operating as designed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Which sibling reading of the whitepaper kernel binds; committer-frame uncertainty routed here per Rule 2.').

omega_variable(
    layer_two_cash_restoration,
    'Does the second-layer network actually restore the cash function (cheap, censorship-resistant, direct-enough payments), or does it fail on liquidity constraints, routing failures, custodial capture, and on-ramp censorship?',
    'Longitudinal data on channel capacity, routing success rates, fee levels at scale, custody concentration, and entry/exit censorship incidents on the layer-two network.',
    'If the layer restores cash utility, the standing arrangement''s effective burden on small transactors drops and this reading''s victim set shrinks toward historical episodes; if it fails, the victims stand as currently measured and the extraction component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_two_cash_restoration, empirical, 'Whether layer-two substitution mitigates the fee-market exclusion this reading measures.').

omega_variable(
    security_budget_necessity,
    'As the block subsidy declines, is the fee market a necessary coordination cost for funding proof-of-work security, or rent collected under cover of necessity?',
    'Economic modeling of subsidy-decline scenarios against observed fee elasticity, hash-rate response, and comparative evidence from chains with different security-funding designs.',
    'If fees are necessary, part of the measured extraction is the price of the coordination itself and the rope component of the hybrid strengthens; if alternative funding designs suffice, the fee market''s extractive share rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_budget_necessity, empirical, 'Necessity versus rent in the fee-funded security model; the crux of the hybrid classification.').

omega_variable(
    governance_gatekeeping_valence,
    'Does the developer gatekeeping of protocol change protect decentralization (keeping verification cheap for ordinary participants) or entrench incumbent economic interests?',
    'Counterfactual analysis of the 2015-2017 proposals against node-cost data, mining-pool concentration trends, and the distribution of benefits from each rejected and accepted change.',
    'If protective, the measured suppression is largely defense of a coordination condition and the enforcement component reads benign; if entrenching, the suppression is extraction-serving and the arrangement slides toward the pure-extraction end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_gatekeeping_valence, conceptual, 'Valence of the governance veto: protective or extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 2009, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement_basis(bitc_tr_t2009, observed).
narrative_ontology:measurement(bitc_tr_t2011, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2011, 0.09).
narrative_ontology:measurement_basis(bitc_tr_t2011, observed).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2013, 0.16).
narrative_ontology:measurement_basis(bitc_tr_t2013, observed).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement_basis(bitc_tr_t2015, observed).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2017, 0.38).
narrative_ontology:measurement_basis(bitc_tr_t2017, observed).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement_basis(bitc_tr_t2019, observed).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2021, 0.29).
narrative_ontology:measurement_basis(bitc_tr_t2021, observed).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2023, 0.3).
narrative_ontology:measurement_basis(bitc_tr_t2023, observed).
narrative_ontology:measurement(bitc_tr_t2025, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(bitc_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2009, 0.06).
narrative_ontology:measurement_basis(bitc_be_t2009, observed).
narrative_ontology:measurement(bitc_be_t2011, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2011, 0.14).
narrative_ontology:measurement_basis(bitc_be_t2011, observed).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2013, 0.34).
narrative_ontology:measurement_basis(bitc_be_t2013, observed).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement_basis(bitc_be_t2015, observed).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2017, 0.78).
narrative_ontology:measurement_basis(bitc_be_t2017, observed).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement_basis(bitc_be_t2019, observed).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2021, 0.7).
narrative_ontology:measurement_basis(bitc_be_t2021, observed).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2023, 0.48).
narrative_ontology:measurement_basis(bitc_be_t2023, observed).
narrative_ontology:measurement(bitc_be_t2025, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(bitc_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2009, 0.04).
narrative_ontology:measurement_basis(bitc_su_t2009, observed).
narrative_ontology:measurement(bitc_su_t2011, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2011, 0.1).
narrative_ontology:measurement_basis(bitc_su_t2011, observed).
narrative_ontology:measurement(bitc_su_t2013, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2013, 0.26).
narrative_ontology:measurement_basis(bitc_su_t2013, observed).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement_basis(bitc_su_t2015, observed).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2017, 0.82).
narrative_ontology:measurement_basis(bitc_su_t2017, observed).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement_basis(bitc_su_t2019, observed).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement_basis(bitc_su_t2021, observed).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2023, 0.63).
narrative_ontology:measurement_basis(bitc_su_t2023, observed).
narrative_ontology:measurement(bitc_su_t2025, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(bitc_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Bitcoin' conflates at least three structurally distinct claims — a payments promise (this file), a scarcity/asset thesis (digital_gold_reading), and a governance philosophy (protocol_ossification_reading). Each carries its own epsilon, beneficiaries, and victims; forcing one story to span them would make epsilon observer-relative. Edges here link this reading to both siblings. Structurally, the 2017 ossification settlement conditions what this reading can achieve, and this reading's expansion pressure is what catalyzed the ossification doctrine in response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
