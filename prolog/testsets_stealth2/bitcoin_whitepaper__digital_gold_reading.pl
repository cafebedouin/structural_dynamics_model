% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin Digital Gold Reading — Fixed-Supply Store-of-Value Arrangement
 *   domain: economic/technological/monetary
 *
 * SUMMARY:
 *   The whitepaper kernel — 'Bitcoin: A Peer-to-Peer Electronic Cash System'
 *   — is read by this story as a scarcity commitment: the arrangement under
 *   contest is the one in which the 21-million cap, small blocks, and fee
 *   market are maintained because the asset's purpose is store of value and
 *   inflation hedging, with spendability a derivative concern delegated to
 *   second layers. Under this reading the arrangement genuinely solves a
 *   problem no issuer-solved arrangement solved before — credible digital
 *   scarcity without a trusted party — while the same structure moves
 *   purchasing power from late entrants to earlier holders through the
 *   scarcity premium and rations block space by fee competition. This is one
 *   reading of a three-reading kernel: the sibling p2p_cash_reading
 *   instantiates the payments arrangement (different epsilon, different
 *   victim set — censored and unbanked users rather than late entrants), and
 *   protocol_ossification_reading instantiates the governance norm (stability
 *   as the primary virtue) this reading relies on for enforcement. The
 *   epsilon authored here refers only to the store-of-value arrangement as
 *   this reading assesses it: moderate-to-high, because the premium mechanism
 *   and fee market impose real costs on identifiable seats while the scarcity
 *   service itself is delivered. The claim/metric relationship is
 *   deliberately unreconciled: claimed_type is tangled_rope — genuine
 *   coordination plus asymmetric extraction under active enforcement — while
 *   the metrics describe the arrangement's actual operation; the engine
 *   computes per-seat classifications from the structural data, and
 *   divergence between the claim and any computed seat type is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - early_adopter_holders: primary beneficiary (powerful/mobile) — accumulated early, wealth denominated in the asset, collects the appreciation premium
 *   - bitcoin_miners: beneficiary and enforcer (organized/constrained) — collects subsidies and fees, enforces consensus via hashpower, ASIC capital stranded on exit
 *   - bitcoin_core_developers: agenda setter (institutional/identity_locked) — gates protocol change through the reference client, benefits indirectly
 *   - late_entrant_buyers: primary target (moderate/constrained) — buys the premium, bears fee competition and premium-deflation risk
 *   - small_transaction_users: target (powerless/mobile) — priced out of block space, low-cost exit to alternatives
 *   - p2p_cash_advocates: excluded (organized/constrained) — lost the block-size wars, persists on a minority fork
 *   - exchange_custody_intermediaries: secondary beneficiary (institutional/arbitrage) — collects trading and custody fees, asset-agnostic infrastructure
 *   - monetary_policy_researchers: analytical observer (institutional/analytical) — studies the hedge claim from outside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.55).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin Digital Gold Reading — Fixed-Supply Store-of-Value Arrangement").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "economic/technological/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'c8fcee88-5106-4909-b96f-95f912ccdf37').
narrative_ontology:cs_kernel_codification('c8fcee88-5106-4909-b96f-95f912ccdf37', fixed_text).
narrative_ontology:cs_authority_grounding('c8fcee88-5106-4909-b96f-95f912ccdf37', practice).
narrative_ontology:cs_interpretation_layer_present('c8fcee88-5106-4909-b96f-95f912ccdf37').
narrative_ontology:cs_reading_relation('c8fcee88-5106-4909-b96f-95f912ccdf37', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('c8fcee88-5106-4909-b96f-95f912ccdf37', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('c8fcee88-5106-4909-b96f-95f912ccdf37', foundational, scarcity_is_the_value_essence).
narrative_ontology:cs_axiom_status(scarcity_is_the_value_essence, holdable).
narrative_ontology:cs_axiom_grounding('c8fcee88-5106-4909-b96f-95f912ccdf37', scarcity_is_the_value_essence, empirically_contingent).
narrative_ontology:cs_axiom('c8fcee88-5106-4909-b96f-95f912ccdf37', secondary, appreciation_precedes_spendability).
narrative_ontology:cs_axiom_status(appreciation_precedes_spendability, holdable).
narrative_ontology:cs_axiom_grounding('c8fcee88-5106-4909-b96f-95f912ccdf37', appreciation_precedes_spendability, instrumental).
narrative_ontology:cs_reference_frame('c8fcee88-5106-4909-b96f-95f912ccdf37', fixed_supply_scarcity_standard).
narrative_ontology:cs_drift_state('c8fcee88-5106-4909-b96f-95f912ccdf37', post_subsidy_fee_market_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c8fcee88-5106-4909-b96f-95f912ccdf37', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopter_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, exchange_custody_intermediaries).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrant_buyers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transaction_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulated bitcoin between 2009 and 2014 at prices orders of magnitude below current ones. Their wealth is denominated in the asset, so they advocate the scarcity framing, self-custody, and never-selling culture; some sell gradually into deep markets to realize gains. Their benefit depends on the fixed-supply commitment continuing to command a premium; exit means liquidating positions, which large holders can only do slowly without moving the price against themselves.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopter_holders, beneficiary,
    powerful, generational, mobile, global).

% Run the proof-of-work hardware that extends the chain and enforces the consensus rules; collect block subsidies (halving every four years) and transaction fees. Their ASIC fleets have no use outside this protocol, so their capital is committed to the system's continuation; as the subsidy shrinks, their revenue shifts toward the fee stream that block-space scarcity sustains. They signal acceptance or rejection of protocol changes through which rules they extend.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners, agenda_setter).

% Maintain the reference software that full nodes run; a change reaches the network only if they implement it and node operators adopt it. They describe themselves as implementing consensus rather than leading it, but their merge decisions gate what is possible. Their professional standing and social world are inside the ecosystem, and the ossification norm means their day-to-day role is increasingly declining changes rather than making them. Their benefit is indirect: standing and relevance within the ecosystem they steward.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_core_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Bought in after the price had appreciated, at entries set by the scarcity premium that earlier holders' advocacy sustains. They hold the asset through drawdowns, bear the risk that the premium deflates, and pay market fees whenever they move coins on the base layer. Selling is possible at any time but realizes whatever gain or loss the entry timing fixed; their position in the arrangement was set at purchase.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrant_buyers, payer,
    moderate, biographical, constrained, global).

% Want to move modest amounts on the base layer — payments, remittances, small transfers. When block space is contested they are outbid by higher-fee transactions and must wait, batch, or move to second layers or other chains. They have little invested in the asset's appreciation story and can leave for alternatives at low cost, but the arrangement treats their transactions as the marginal demand that fees ration.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transaction_users, payer,
    powerless, immediate, mobile, global).

% Hold the reading that the system should be a cheap, censorship-resistant medium of exchange. They fought the block-size wars of 2015-2017, lost the chain split, and now maintain a minority fork with a small fraction of the hashpower, liquidity, and developer attention. Within the main network their proposals have no path to adoption; their exit was the fork, which left them with the name and not the network effect.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, p2p_cash_advocates, excluded,
    organized, generational, constrained, global).

% Operate the on-ramps, trading venues, and custody services through which most new entrants acquire exposure. They collect trading fees, spreads, and custody charges on flows whose volume the appreciation story drives. Their infrastructure is asset-agnostic — they list whatever assets draw flow and could pivot if this one faded — so their stake is in the trading economy around the asset rather than in the scarcity commitment itself.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, exchange_custody_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Academic and central-bank economists who study whether a fixed-supply digital asset functions as the store of value and inflation hedge its advocates claim. They publish the econometric work both camps cite, testify to legislatures, and design the regulatory categories the asset falls into. They hold no position in the arrangement and can analyze it from outside.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, monetary_policy_researchers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, early_adopter_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces credible digital scarcity: a fixed-supply monetary asset whose cap no issuer, government, or committee can alter, enabling self-custodied long-term savings without trusted intermediaries. The arrangement coordinates millions of mutually distrusting parties on a single monetary standard and a single Schelling point for what the asset is.
% TRANSFER_FUNCTION: Transfers purchasing power from late entrants to earlier holders through the scarcity premium (late entrants buy at prices the premium sets; earlier holders realize gains on exit), and transfers transaction fees from block-space users to miners as the security budget.
% ABSENT_VOICES: The p2p cash faction (forked to a minority chain after losing the block-size wars), low-income users in high-inflation economies who need cheap payments rather than appreciating savings technology, and future entrants not yet in the room whose entry price is being set by current holders' advocacy. None hold seats in the arrangement's governance.
% DISAPPEARANCE_RATIONALE: If the scarcity commitment vanished overnight (cap lifted or credibly contestable), the monetary premium would collapse toward utility value, holders would exit en masse, the fee-plus-subsidy security budget would unravel, and the asset would reprice — a trillion-dollar-class repricing and a reorganization of the entire crypto monetary hierarchy around whatever credibility remained.
% FOUNDING_PROBLEM: The 2008 financial crisis: trust in central banks, fractional-reserve banking, and payment intermediaries collapsed; the whitepaper proposed 'a peer-to-peer version of electronic cash' allowing online payments without a trusted financial institution.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: hard-money economists with no holder position attest the debasement problem is live (post-2020 balance-sheet expansion, the 2021-2023 inflation episode); central-bank research and most academic monetary economists dispute both that framing and bitcoin's hedge efficacy; the p2p cash faction attests the original cash problem was abandoned rather than solved. No single external seat corroborates the digital-gold reading's own account of the founding problem — its status is genuinely disputed across seats.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62 reflects an arrangement that delivers its store-of-value service while transferring real costs: late entrants buy the premium earlier holders' advocacy sustains, and base-layer transactors pay fees that ration block space. Suppression 0.55 is structural rather than theatrical: within the network, alternatives (larger blocks, cap revision) are foreclosed by consensus ossification, and the enforcement requirement peaked during the 2015-2017 block-size wars — node-count campaigns, social pressure on businesses, the New York Agreement's collapse, the chain split — then settled into a self-maintaining norm needing less active force. Theater 0.35: the security function (proof-of-work, full-node validation) is real, while a growing performative layer — hyperbitcoinization narrative, maximalist identity marketing — maintains the premium story. Accessibility collapse 0.52: substitutes exist (other chains, gold, inflation-indexed instruments) but the specific Lindy-plus-liquidity-plus-security bundle is not replicable, so alternatives collapse substantially within the hard-money frame while remaining open outside it. Resistance 0.55: the block-size wars were organized, funded resistance; afterward, resistance persists as fee complaints, block-space contention (ordinal inscriptions), and minority-fork persistence. All three series share one time grid. Extractiveness dips slightly after the 2021 cycle top and recovers with the 2023-2024 fee market — a mild speculative cycle driven by inflow and outflow rather than structural change, not a full oscillation regime.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical nominal community membership. From the early holder's position the arrangement is the sound-money revolution they were promised — coordination they benefit from and defend. From the late entrant's position the same structure operates as a premium-transfer machine they entered near the top of. From the miner's position it is an existential commitment: their capital equipment has no use outside this protocol, so the arrangement's continuation is their balance sheet. From the core developer's position it is guardianship of a thing that must not change. The excluded cash faction computes it as a captured protocol. The engine derives these divergences from power, exit options, and role — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders are declared beneficiaries with mobile exit — low d; the arrangement subsidizes them through the premium, and their exit (gradual liquidation into deep markets) is real. Miners are declared beneficiaries with constrained exit (ASIC capital stranded on exit) — low d, amplified in stake by their trapped position; their secondary agenda-setter role records that they also enforce. Core developers hold no beneficiary declaration: they are agenda setters with identity-locked exit whose benefit is indirect (ecosystem standing, professional relevance), so the canonical fallback places them moderately rather than at the beneficiary end — their situation text records the indirect benefit rather than an override forcing it. Late entrants and small transactors are declared victims — high d; the small users' mobile exit dampens their effective position somewhat, while late entrants' constrained exit keeps them near the full-target end. Exchanges are beneficiaries with arbitrage-grade exit — the lowest d in the story, since their infrastructure is asset-agnostic. Spatial scope is global throughout, which the engine's scope modifier reflects in effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — electronic cash without trusted third parties — was not allowed to atrophy quietly; it was explicitly deprioritized by this reading's victory in the block-size wars, which is mandate transformation rather than mandate death, and the R5 fields accordingly record the founding problem's status as contested rather than dead. The mandatrophy trap this story guards against is the mountain misread: maximalist framing treats the 21-million cap as natural law, but the cap is a socially maintained commitment — no serious revision proposal exists, yet revision remains structurally possible under sufficient consensus, and identifiable seats collect from its maintenance. Claiming tangled_rope keeps both faces visible: the genuine coordination function (credible scarcity no issuer could promise) and the asymmetric extraction (late-entrant premium transfer, fee-rationed block space). The cap_naturalness omega documents the ambiguity directly. If the coordination function ever fails (security-budget collapse), the arrangement degrades toward a premium narrative without substance — the security_budget omega tracks exactly that failure mode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dominance,
    'Is the digital-gold reading''s dominance over the bitcoin_whitepaper kernel a discovery of the kernel''s meaning, or a contingent capture of it by the seats that collect from appreciation?',
    'Cross-reading comparison as new entrant cohorts arrive: if late entrants systematically adopt the cash reading''s usage patterns when fee markets permit, dominance is contingent on fee levels; if they adopt holder behavior regardless, dominance reflects revealed preference for the asset''s premium properties.',
    'If dominance is capture, the p2p_cash_reading becomes the counterfactual arrangement and this story''s victim set (late entrants, fee-rationed transactors) is an artifact of the winning coalition rather than the kernel''s structure; the sibling file would need re-weighting as the primary instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance, conceptual, 'Whether this reading governs the kernel by meaning or by coalition victory.').

omega_variable(
    security_budget_sustainability,
    'Can the fee market alone sustain the proof-of-work security budget as block subsidies continue halving toward zero, without which the scarcity commitment loses its enforcement foundation?',
    'Track fee revenue against miner cost curves across successive halvings; observe whether fee volatility produces hashpower oscillations that threaten reorganization depth.',
    'If the budget fails, enforcement erodes endogenously — the store-of-value premium would reprice toward utility value and the arrangement would degrade from an enforced commitment toward an unenforced narrative; if it holds, the arrangement''s actively-enforced character is confirmed long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_budget_sustainability, empirical, 'Whether fee revenue can replace the block subsidy as the security budget.').

omega_variable(
    inflation_hedge_efficacy,
    'Does bitcoin actually function as an inflation hedge — this reading''s core empirical claim — or does it trade as a risk asset whose correlation with global liquidity conditions dominates any monetary-premium hedge property?',
    'Econometric decomposition across distinct inflation regimes (2021-2023 and successors): regress bitcoin returns against realized inflation, real rates, and liquidity proxies; test hedge behavior specifically in high-inflation, tightening regimes.',
    'If the hedge claim fails, the arrangement''s coordination story weakens toward narrative-sustained premium — the late-entrant cost structure looks more like narrative-driven transfer than sound-money service, pushing effective classification toward the extractive end; if it holds across regimes, the coordination function is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_hedge_efficacy, empirical, 'Whether the inflation-hedge claim survives regime-specific testing.').

omega_variable(
    cap_naturalness,
    'Is the 21-million cap an immutable structural feature of the protocol — a natural law of this system — or a socially maintained commitment that sufficient consensus could revise, with identifiable seats collecting from its maintenance?',
    'Observe governance responses to any serious cap-revision proposal: whether social consensus treats the cap as touchable under any circumstances, and what coalition forms around maintenance versus revision.',
    'If the cap is genuinely immutable in practice, the scarcity component approaches natural-law status and the enforcement requirement falls; if revisable in principle and maintained by coalition, the arrangement is enforced coordination with beneficiaries — the constructed reading this story''s structural data assumes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cap_naturalness, conceptual, 'Whether the supply cap is natural law or maintained commitment.').

omega_variable(
    premium_composition,
    'How much of bitcoin''s price is monetary premium sustained by the scarcity commitment versus utility-floor value from actual payment and settlement demand — is the late entrant''s cost mostly a transfer to earlier holders, or mostly the market price of a delivered service?',
    'Counterfactual utility valuation: estimate payment and settlement demand at observed fee levels and compare to market capitalization; observe price behavior when blockspace utility events (inscription waves, fee spikes) shock the utility channel.',
    'If the premium dominates, the arrangement''s extractive face is structural — late entrants fund earlier holders'' exit — and the victim declarations carry full weight; if utility demand is substantial, part of the measured cost is the price of the service itself and the coordination function is larger than the metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(premium_composition, empirical, 'Monetary premium versus utility floor in the asset''s price.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 2009, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digital_gold_tr_t2009, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2009, 0.04).
narrative_ontology:measurement_basis(digital_gold_tr_t2009, observed).
narrative_ontology:measurement(digital_gold_tr_t2012, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2012, 0.07).
narrative_ontology:measurement_basis(digital_gold_tr_t2012, observed).
narrative_ontology:measurement(digital_gold_tr_t2015, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement_basis(digital_gold_tr_t2015, observed).
narrative_ontology:measurement(digital_gold_tr_t2017, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2017, 0.26).
narrative_ontology:measurement_basis(digital_gold_tr_t2017, observed).
narrative_ontology:measurement(digital_gold_tr_t2019, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement_basis(digital_gold_tr_t2019, observed).
narrative_ontology:measurement(digital_gold_tr_t2021, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2021, 0.36).
narrative_ontology:measurement_basis(digital_gold_tr_t2021, observed).
narrative_ontology:measurement(digital_gold_tr_t2023, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2023, 0.33).
narrative_ontology:measurement_basis(digital_gold_tr_t2023, observed).
narrative_ontology:measurement(digital_gold_tr_t2026, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2026, 0.35).
narrative_ontology:measurement_basis(digital_gold_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(digital_gold_be_t2009, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2009, 0.08).
narrative_ontology:measurement_basis(digital_gold_be_t2009, observed).
narrative_ontology:measurement(digital_gold_be_t2012, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2012, 0.14).
narrative_ontology:measurement_basis(digital_gold_be_t2012, observed).
narrative_ontology:measurement(digital_gold_be_t2015, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement_basis(digital_gold_be_t2015, observed).
narrative_ontology:measurement(digital_gold_be_t2017, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2017, 0.48).
narrative_ontology:measurement_basis(digital_gold_be_t2017, observed).
narrative_ontology:measurement(digital_gold_be_t2019, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement_basis(digital_gold_be_t2019, observed).
narrative_ontology:measurement(digital_gold_be_t2021, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2021, 0.63).
narrative_ontology:measurement_basis(digital_gold_be_t2021, observed).
narrative_ontology:measurement(digital_gold_be_t2023, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2023, 0.6).
narrative_ontology:measurement_basis(digital_gold_be_t2023, observed).
narrative_ontology:measurement(digital_gold_be_t2026, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(digital_gold_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(digital_gold_su_t2009, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2009, 0.05).
narrative_ontology:measurement_basis(digital_gold_su_t2009, observed).
narrative_ontology:measurement(digital_gold_su_t2012, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2012, 0.08).
narrative_ontology:measurement_basis(digital_gold_su_t2012, observed).
narrative_ontology:measurement(digital_gold_su_t2015, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(digital_gold_su_t2015, observed).
narrative_ontology:measurement(digital_gold_su_t2017, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement_basis(digital_gold_su_t2017, observed).
narrative_ontology:measurement(digital_gold_su_t2019, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2019, 0.62).
narrative_ontology:measurement_basis(digital_gold_su_t2019, observed).
narrative_ontology:measurement(digital_gold_su_t2021, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement_basis(digital_gold_su_t2021, observed).
narrative_ontology:measurement(digital_gold_su_t2023, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement_basis(digital_gold_su_t2023, observed).
narrative_ontology:measurement(digital_gold_su_t2026, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(digital_gold_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The whitepaper kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle: this digital-gold arrangement (scarcity commitment; victims are late entrants and fee-rationed transactors; epsilon ~0.62), the p2p-cash payments arrangement (victims are censored and unbanked users; different epsilon), and the ossification governance norm (victims are change advocates). The digital-gold reading is downstream of the whitepaper's credibility and upstream of the cash reading's operating costs: its fee market is the structural pressure the cash reading lives under. Each family file links the others via affects_constraints; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
