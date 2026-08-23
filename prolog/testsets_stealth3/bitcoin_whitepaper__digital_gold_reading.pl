% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Digital Gold: Scarcity-First Store-of-Value Arrangement
 *   domain: economic/technological/monetary
 *
 * SUMMARY:
 *   This story instantiates the digital_gold_reading of the
 *   bitcoin_whitepaper kernel: Bitcoin as a scarce digital asset optimized
 *   for store of value and inflation hedging, in which asset appreciation is
 *   the prioritized output, transaction fees are booked as an acceptable cost
 *   of credible scarcity, and the people bearing the arrangement's costs are
 *   late entrants who buy after appreciation has repriced entry and
 *   transactors who lose fee auctions during congestion. The standing
 *   arrangement under contest — the ε referent — is Bitcoin as actually
 *   operated under scarcity-first governance (fixed 21M cap, deliberately
 *   limited block space, fee-market settlement tiering), assessed by this
 *   reading's own lights; the reading's endorsed alternative plays no part in
 *   ε. CONSTRAINT FAMILY: the colloquial label 'Bitcoin' decomposes per the
 *   ε-invariance principle into three structurally distinct stories sharing
 *   the whitepaper kernel — this digital_gold_reading, the p2p_cash_reading
 *   (medium of exchange; fees are failure, not cost), and the
 *   protocol_ossification_reading (change legitimacy; stability as primary
 *   virtue). Each carries its own ε, victims, and type; they are linked via
 *   network.affects_constraints, not merged. The whitepaper text sits
 *   upstream of all three and is cited by each as evidence for its own
 *   reading. CLAIM/METRIC INDEPENDENCE: claimed_type tangled_rope is authored
 *   from structure — a genuine coordination function (trustless scarcity via
 *   decentralized validation solving the double-spend/issuer-trust problem)
 *   coexisting with asymmetric transfer (appreciation accrues to early
 *   holders against late entrants' entry prices; fee auctions transfer from
 *   transactors to miners) held together by active enforcement (node
 *   validation, miner consensus, socially enforced protocol immutability).
 *   The metrics are authored separately as descriptive facts; the engine
 *   computes per-seat types and any divergence from the claim is the datum.
 *
 * KEY AGENTS:
 *   - - early_adopter_large_holders: Primary beneficiary (powerful/mobile) — accumulated at negligible early prices; each new entrant's purchase marks up their position; exit is gradual distribution into liquidity
 *   - - bitcoin_miners: Agenda-setter and collector (organized/constrained) — enforce consensus by building the valid chain; collect subsidy plus fee-auction revenue; sunk ASIC capital binds them to the arrangement they police
 *   - - core_protocol_developers: Agenda-setter (moderate/identity_locked) — maintain consensus-critical software; collect no direct protocol rent; authority rests on perceived neutrality; professional identity fused with stewardship
 *   - - institutional_fund_managers: Beneficiary (institutional/arbitrage) — package the scarcity narrative into regulated wrappers; collect fees on assets under management regardless of direction
 *   - - late_entrant_retail_holders: Primary payer (moderate/constrained) — bought after repricing; their entries are the counterparty realizing earlier holders' gains; exit means realizing losses against community norms
 *   - - congested_period_transactors: Payer (powerless/constrained) — need on-chain settlement for self-custody and cross-border movement; bid against high-value transactions in fee auctions; small transfers become uneconomic by design tolerance
 *   - - would_be_daily_payment_users: Excluded (powerless/mobile) — merchants, remittance senders, and high-inflation-country earners who wanted electronic cash; priced out of the base layer and absent from governance
 *   - - monetary_policy_regulators: Analytical observer (institutional/analytical) — assess investor protection and systemic risk; can gate the wrappers but not the protocol
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.55).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold: Scarcity-First Store-of-Value Arrangement").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "economic/technological/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopter_large_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_fund_managers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrant_retail_holders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, congested_period_transactors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulated holdings at negligible prices between 2009 and 2013 and still control a large share of outstanding supply. Every new buyer's entry marks up their position; realized gains come from distributing into liquidity raised by successive entrant cohorts. They fund foundations, media, and lobbying that amplify the scarcity narrative, and can time sales across cycles in ways smaller holders cannot.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopter_large_holders, beneficiary,
    powerful, generational, mobile, global).

% Convert energy and specialized hardware into chain security, collecting the block subsidy plus whatever the fee auction clears. They enforce the consensus rules by extending the majority-valid chain and signal acceptance or rejection of proposed changes through hash power. Sunk ASIC capital and facility contracts bind them to the arrangement; redirecting hash power to rival chains means writing off equipment, so they police the rules that generate their revenue.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners, beneficiary).

% Maintain the consensus-critical reference software, review and merge changes, and gate what reaches production. They collect no direct protocol revenue, living on grants, donations, and employer sponsorship. Their standing rests on perceived neutrality and technical authority; after prevailing in the block-size conflict, their reputations and careers are bound to the immutability line they defended, and stepping away would forfeit the identity and community standing their work built.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, core_protocol_developers, agenda_setter,
    moderate, generational, identity_locked, global).

% Package the scarcity asset into regulated wrappers — spot ETFs, custody services, treasury products — and collect basis-point fees on assets under management whether the price rises or falls. Their marketing normalizes the store-of-value framing for mainstream allocators. They bear compliance and regulatory costs rather than protocol costs, and can launch, shrink, or wind down products as the fee opportunity shifts.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_fund_managers, beneficiary,
    institutional, biographical, arbitrage, national).

% Bought after a decade of appreciation had already repriced entry, often at cycle peaks amplified by influencer and institutional marketing. Their purchases are the counterparty side of earlier holders' realized gains. Selling means locking in losses, triggering tax events, and defying community norms that treat selling as weakness; holding means riding volatility they entered too late to be paid for enduring. Fee costs bite hardest on small positions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrant_retail_holders, payer,
    moderate, immediate, constrained, global).

% Need the base layer for things only it provides: self-custody withdrawals, moving holdings off exchanges, cross-border settlement under capital controls. When demand spikes, they bid in the same fee auction as high-value institutional settlements and lose; a small transfer that cost cents at baseline can cost more than the amount moved. They chose this rail for properties rivals do not replicate, so waiting indefinitely or paying up are the realistic options.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, congested_period_transactors, payer,
    powerless, immediate, constrained, global).

% Merchants, remittance senders, and wage earners in high-inflation economies who wanted electronic cash for everyday settlement. Fee levels and volatility pushed them to dollar stablecoins, rival chains, or physical dollars. They hold no seat in protocol governance — no miner hash, no merge access, no wrapper to sponsor — and the objection that the arrangement prices out its founding use case registers nowhere in the store-of-value frame.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, would_be_daily_payment_users, excluded,
    powerless, immediate, mobile, global).

% Assess investor protection, market integrity, and financial-stability exposure. They approve or deny the wrapper products, set custody and disclosure rules, and take testimony from the other seats, but cannot alter the protocol itself; their leverage stops at the perimeter institutions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, monetary_policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, early_adopter_large_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global consensus on a fixed-supply monetary asset without an issuer: decentralized validation solves the double-spend problem and makes absolute digital scarcity credible, while the fee auction allocates scarce block space to the highest-valued settlements.
% TRANSFER_FUNCTION: Moves purchasing power from later buyers to earlier holders through appreciation — each entrant cohort's entry price is the counterparty that realizes prior cohorts' gains; moves fee-auction revenue from transactors to miners during congestion; moves management and custody fees from fund investors to asset managers.
% ABSENT_VOICES: Would-be daily payment users — merchants, remittance senders, high-inflation-country earners — would object that the arrangement prices out the founding use case; they are absent from governance because they hold no hash power, no merge authority, and no wrapper, and have largely exited to stablecoins and rival rails. Small-value transactors caught in fee spikes are present in the system but voiceless in its rule-setting.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, roughly two trillion dollars of stored value would simultaneously seek alternative stores — gold, treasuries, stablecoins, rival assets — repricing all of them; miners' specialized infrastructure would strand; ETF and custody products would unwind; and the cohort of holders who bought above replacement cost would absorb the difference. Settlement patterns for capital-flight and sanctions-evasion uses would rearrange around whatever censorship-resistant rail absorbed the demand.
% FOUNDING_PROBLEM: Enabling direct online payments between parties without a trusted financial institution — solving double-spending with a peer-to-peer timestamp server so two parties could transact electronically without an intermediary.
% FOUNDING_PROBLEM_CORROBORATION: No single attestation settles it. The digital-gold coalition attests the cash problem was superseded because base-layer cash at scale proved impossible; the cash-reading community and the surviving ecosystem of cash-focused forks and layers attest it remains live and unmet. Outside the benefiting parties, independent on-chain analytics firms document the long decline of commercial on-chain payment volume relative to holding volume, academic monetary-economics literature treats the asset's use as overwhelmingly speculative store of value, and central-bank research (BIS and national banks) analyzes it as an investment vehicle rather than a payment rail — external sources that corroborate the migration of function while disputing whether the founding problem is dead or merely displaced to other layers and projects.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness ends at 0.62: the largest flow is the appreciation transfer, in which each entrant cohort's purchases realize prior cohorts' gains — a transfer that is voluntary and disclosed but structurally timed against the entrant — compounded by fee-auction incidence on small transactors during congestion. Suppression is 0.55 and is a raw, unscaled structural property: it operates on protocol modification (the block-size war ended with the large-block position defeated by coordinated node enforcement; changing the cap is effectively impossible without near-universal consensus) and on low-value use cases (priced off the base layer by fee tolerance), not on holding or exiting — alternatives like gold, stablecoins, and rival chains remain open, which caps suppression well below snare levels. Theater_ratio 0.30: scarcity enforcement is functional, but a material share of activity is identity performance — HODL ritual, halving celebration, maximalist messaging — that maintains the narrative the valuation rides on. Accessibility_collapse 0.40: understanding the arrangement does not collapse alternatives; the hard-money frame rhetorically collapses them (uniqueness, Lindy, security-budget arguments) but practical substitutes persist. Resistance 0.45: the block-size wars were genuine organized resistance, absorbed and marginalized; regulatory friction and environmental criticism continue without displacing the arrangement. TEMPORAL/CYCLICAL: the series run on one shared ten-point grid (t0–t16, mapping 2009–2025). Base extractiveness oscillates with the halving-cycle market rhythm — peaks at t8 (2017 mania and the December fee crisis) and t12 (2021 institutional wave), troughs at t6, t9, t14 — and the oscillation is partly the extraction mechanism itself: each euphoria phase recruits a new entrant cohort at cycle peaks who become the trapped counterparty of the next drawdown (intermittent reinforcement), not mere noise. Suppression_requirement traces a distinct, non-cyclical arc: enforcement capacity built sharply through the block-size war (t6–t8), peaked at the UASF victory, then decayed slowly as ossification made heavy enforcement self-reproducing. Theater tracks the cycle with a lag. base_properties values are the interval-end (t16) states, measured in the expansion phase of the current cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical protocol facts. From the early-holder seat the arrangement is a voluntary market rewarding conviction and early risk-taking; from the late-entrant seat the same structure is a timing tax they did not set and cannot renegotiate, with exit priced as loss realization. From the miner seat the fee auction is a fair, transparent priority market they operate; from the congested-transactor seat it is exclusion of small-value settlement from the base layer. From the developer seat immutability is the sacred property that makes scarcity credible; from the excluded payment-user seat it is the wall that locked them out. The engine derives these divergent per-seat classifications from the structural data; this story authors the structure, not the verdicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. early_adopter_large_holders and institutional_fund_managers sit near the beneficiary pole (d low): the arrangement subsidizes their positions and fee streams, and both hold mobile or arbitrage-grade exits. bitcoin_miners derive low-to-moderate d: they bear real costs (capital, energy) but collect the fee-auction revenue the arrangement's scarcity-first design channels to them, and they administer the enforcement that sustains it. core_protocol_developers fall near symmetric via the fallback path: no direct rents collected, real stewardship costs borne, compensated in status and influence. late_entrant_retail_holders and congested_period_transactors sit near the target pole (d high): both bear the transfer, with constrained exits — loss realization and community norms for the former, irreplaceable settlement properties for the latter. would_be_daily_payment_users carry high d as an excluded seat: commentary-grade only under R3, feeding the absent-voices record, never a classification override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — direct online payments without a trusted intermediary — is contested, not dead: the digital-gold coalition holds it was superseded (cash-at-scale proved impossible on the base layer; store of value is the realized purpose), while the cash coalition and outside observers hold it remains unmet. Because the store-of-value function the arrangement currently performs is live and heavily used, mandatrophy_resolved is false: the arrangement has not outlived its functioning mandate so much as migrated it. The classification guards both mislabels: calling this a pure snare erases the genuine coordination achievement (trustless absolute scarcity, solved once, globally, without an issuer); calling it a pure rope erases the constitutive dependence of realized gains on successive entrant cohorts and the designed exclusion of small-value settlement. Piton risk is real but conditional: if the cash function is judged dead AND the store-of-value function decays (security-budget failure), what remains is theatrical maintenance of a scarcity claim — the omega on reflexive inflow dependence and the security-budget omega are the tripwires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_omega,
    'This constraint is ONE reading of the bitcoin_whitepaper kernel (digital_gold_reading). Which reading governs evaluation of the shared protocol arrangement: under the p2p_cash_reading sibling, the same fee levels and block-space limits that this reading books as acceptable cost of scarcity compute instead as the primary obstruction of the founding function?',
    'Comparative classification across the linked sibling stories: classify the identical protocol facts once per reading and compare victim sets, epsilon, and type; the disagreement is located in block-space policy and fee tolerance, not in the protocol facts themselves.',
    'Under the p2p_cash_reading sibling, effective extraction concentrates on excluded small-value users and the arrangement trends toward snare-flavored verdicts; under this reading, extraction concentrates on late entrants and fee payers and the arrangement holds as tangled_rope. Cross-reading comparison, not within-story measurement, resolves it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_omega, conceptual, 'Committer-frame ambiguity: one kernel, three readings, structurally distinct constraints.').

omega_variable(
    reflexive_inflow_dependence,
    'Is the store-of-value function durable (credible scarcity sustaining value independently of new buying) or reflexive (valuation sustained by expected future inflows, making each new entrant cohort structurally necessary rather than incidental)?',
    'Long-horizon flow attribution: correlate drawdown-period valuations with net new-entrant inflow; cohort-level P&L analysis across at least two full cycles; stress-test valuation against flat adoption scenarios.',
    'If reflexive, late-entrant losses are constitutive — the arrangement requires a continuing stream of new buyers to realize prior holders'' gains, pushing classification toward snare; if durable, the transfer to late entrants is transitional price discovery and tangled_rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reflexive_inflow_dependence, empirical, 'Whether appreciation depends constitutively on recruiting successive entrant cohorts.').

omega_variable(
    security_budget_fee_transition,
    'As block subsidies halve toward zero, can transaction fees alone sustain the hash-rate security that backs the scarcity guarantee, and at what fee burden on transactors?',
    'Model hash-rate equilibrium against projected fee revenue at the 2028 and 2032 subsidy epochs; observe fee-market clearing levels during demand spikes as subsidy share declines.',
    'If fees must rise structurally to sustain security, extraction from transactor seats intensifies by design and the fee-tolerance axiom hardens; if security decays instead, the scarcity guarantee itself weakens and the store-of-value premise erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_budget_fee_transition, empirical, 'Post-subsidy sustainability of the security budget and its incidence on transactors.').

omega_variable(
    voluntary_disclosure_extraction_boundary,
    'Does fully voluntary, publicly disclosed purchase place late-entrant losses outside extraction proper (buyer-beware), or does marketing-driven entry under informational and emotional asymmetry keep them inside it?',
    'Framing analysis of the disclosure standard combined with behavioral evidence on cycle-peak entry: survey entry motivations, measure advertising and influencer amplification at cycle tops, and test whether entrants understood the timing risk they bore.',
    'If buyer-beware governs, epsilon drops substantially toward rope and the victim declaration narrows to fee-excluded transactors only; if asymmetric recruitment counts, the tangled_rope classification stands with late entrants as structural payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_disclosure_extraction_boundary, conceptual, 'Whether voluntary entry under hype conditions removes late-entrant losses from the extraction category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 9, 0.34).
narrative_ontology:measurement(bitc_tr_t11, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 11, 0.26).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(bitc_tr_t14, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 16, 0.3).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2, 0.1).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(bitc_be_t11, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 11, 0.42).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(bitc_be_t14, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 14, 0.48).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 16, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2, 0.2).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 9, 0.66).
narrative_ontology:measurement(bitc_su_t11, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 11, 0.6).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(bitc_su_t14, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 14, 0.56).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 16, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the 'Bitcoin' label per the epsilon-invariance principle: one kernel (bitcoin_whitepaper), three structurally distinct constraints. This story (digital_gold_reading) authors epsilon over the store-of-value arrangement with late entrants and fee-paying transactors as victims. The p2p_cash_reading sibling authors epsilon over the same protocol facts as a medium-of-exchange arrangement with excluded small-value users as victims and fees as the extractive object. The protocol_ossification_reading sibling authors epsilon over the change-legitimation arrangement itself. Upstream/downstream: the whitepaper text is upstream of all three and is cited by each as evidence; this reading exerts downstream influence on the ossification reading (immutable scarcity is its premise) and coexists with the cash reading across factions. Values differ across the family because the readings differ, not because the protocol facts differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
