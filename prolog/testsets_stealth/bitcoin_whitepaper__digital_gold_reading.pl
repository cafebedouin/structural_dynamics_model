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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Digital Gold: Scarcity-Optimized Store-of-Value Arrangement
 *   domain: economic/technological/monetary
 *
 * SUMMARY:
 *   This story instantiates the digital-gold reading of the
 *   bitcoin_whitepaper kernel: the operative arrangement is a fixed issuance
 *   schedule plus fee-auctioned block space plus HODL cultural norms, jointly
 *   steering the network toward asset characteristics (scarce, appreciating,
 *   settlement-grade) and away from payment characteristics. The arrangement
 *   has a genuine coordination core — credible scarcity that no issuer can
 *   alter, solving a monetary-expectation problem — and an asymmetric burden
 *   structure: late entrants buy embedded appreciation, fee competition sheds
 *   small transactors, and the ruleset is actively defended (the 2015-2017
 *   capacity wars demonstrated the enforcement machinery at full strength).
 *   Claim and metrics are authored independently: claimed_type states what
 *   this reading's structure is; the metrics describe how the arrangement
 *   actually operates. The sibling readings (p2p_cash_reading,
 *   protocol_ossification_reading) are separate constraint stories with their
 *   own epsilon values and victim sets; this file authors only the
 *   digital-gold instantiation.
 *
 * KEY AGENTS:
 *   - early_adopter_holders: primary beneficiary (powerful/mobile) — collects appreciation funded by later cohorts, exits at will
 *   - bitcoin_miners: enforcement arm and fee collector (institutional/constrained) — upholds the ruleset, collects issuance and fees
 *   - core_protocol_developers: protocol stewards gating change (institutional/identity_locked) — post-war role is deciding what does not change
 *   - full_node_operators: validation veto enforcing the ruleset (organized/identity_locked) — unpaid enforcement backbone, mostly also holders
 *   - custodial_exchanges: flow intermediary (institutional/arbitrage) — collects fees on the narrative's traffic, no identity fusion
 *   - late_entrant_buyers: primary target (powerless/constrained) — buys embedded appreciation, bears full drawdown
 *   - small_transaction_users: shed victims (powerless/mobile) — crowded out by fee competition, exit is the shedding mechanism
 *   - unbanked_remittance_senders: excluded constituency of the rival reading (powerless/mobile) — priced out, voiceless in governance
 *   - monetary_policy_analysts: analytical observer — sees the full structure including reflexivity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.46).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.35).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold: Scarcity-Optimized Store-of-Value Arrangement").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "economic/technological/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '8048f78f-7c7b-4d3c-a867-7846047cfa66').
narrative_ontology:cs_kernel_codification('8048f78f-7c7b-4d3c-a867-7846047cfa66', fixed_text).
narrative_ontology:cs_authority_grounding('8048f78f-7c7b-4d3c-a867-7846047cfa66', lineage).
narrative_ontology:cs_interpretation_layer_present('8048f78f-7c7b-4d3c-a867-7846047cfa66').
narrative_ontology:cs_reading_relation('8048f78f-7c7b-4d3c-a867-7846047cfa66', bitcoin_whitepaper__p2p_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('8048f78f-7c7b-4d3c-a867-7846047cfa66', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('8048f78f-7c7b-4d3c-a867-7846047cfa66', foundational, absolute_supply_cap_is_constitutive).
narrative_ontology:cs_axiom_status(absolute_supply_cap_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('8048f78f-7c7b-4d3c-a867-7846047cfa66', absolute_supply_cap_is_constitutive, conventional).
narrative_ontology:cs_axiom('8048f78f-7c7b-4d3c-a867-7846047cfa66', foundational, scarcity_premium_over_exchange_utility).
narrative_ontology:cs_axiom_status(scarcity_premium_over_exchange_utility, holdable).
narrative_ontology:cs_axiom_grounding('8048f78f-7c7b-4d3c-a867-7846047cfa66', scarcity_premium_over_exchange_utility, instrumental).
narrative_ontology:cs_reference_frame('8048f78f-7c7b-4d3c-a867-7846047cfa66', absolute_scarcity_store_of_value).
narrative_ontology:cs_drift_state('8048f78f-7c7b-4d3c-a867-7846047cfa66', post_block_wars_institutional_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8048f78f-7c7b-4d3c-a867-7846047cfa66', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopter_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrant_buyers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transaction_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, early_adopter_holders).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, algorithmic_scarcity_sound_money_thesis).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, digital_scarcity_premium_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulated large positions when acquisition costs were negligible; their wealth grows as new demand arrives against fixed supply. They articulate and fund the store-of-value narrative and can sell into liquidity provided by later buyers at times of their choosing. Exit is open at any moment; the main costs of leaving are taxes and forgone further appreciation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopter_holders, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, early_adopter_holders, payer).

% Convert electricity and specialized hardware into the right to append blocks, collecting newly issued coins and transaction fees. Hardware and power contracts are sunk and site-specific, so redeployment is costly. They uphold the shared ruleset by extending the longest valid chain and rejecting blocks that break it; revenue rises with coin price and fee levels.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners, beneficiary).

% Maintain the reference software and review proposed changes. After the 2015-2017 capacity dispute, changes effectively require near-unanimous agreement, so their day-to-day role is deciding what does not change. Professional standing and careers are bound up with stewardship of this codebase; stepping away means abandoning the work they are known for.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, core_protocol_developers, agenda_setter,
    institutional, generational, identity_locked, global).

% Run independent copies of the ledger and refuse invalid blocks, which is what makes the ruleset binding without a referee. Most also hold the asset, so the rules they enforce protect their own savings. Operating a node costs money and attention and pays nothing directly; the practice is sustained by conviction and community standing.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, full_node_operators, agenda_setter,
    organized, biographical, identity_locked, global).

% Operate the venues where national currency enters and exits, charging trading fees, spreads, and custody fees on the flow the store-of-value narrative generates. They list many assets and can shift emphasis if better opportunities appear elsewhere; exposure to this particular asset is a business line, not an identity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges, beneficiary,
    institutional, biographical, arbitrage, global).

% Buy after years of appreciation, so each dollar acquires a smaller share of the fixed supply than earlier cohorts' dollars did. They bear full downside volatility and depend on continued new demand for gains. Selling is possible but realizes losses after drawdowns, and community norms treat selling as weakness, so holding through drawdowns is the socially rewarded path.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrant_buyers, payer,
    powerless, biographical, constrained, global).

% Use the network for everyday-sized payments and transfers. When demand for block space rises, fee auctions push their transactions below the confirmation threshold; they either wait indefinitely, pay fees disproportionate to the amount sent, or move to other rails. Leaving for cheaper rails is easy, which is precisely why their needs carry little weight in protocol priorities.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transaction_users, payer,
    powerless, immediate, mobile, global).

% Send cross-border payments and were the constituency the original whitepaper addressed. Under current fee economics and price volatility the base chain serves them poorly; their needs are argued for by others rather than by themselves, since they lack standing in technical forums and their working alternatives lie outside this system.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, unbanked_remittance_senders, excluded,
    powerless, immediate, mobile, continental).

% Study whether the asset delivers the monetary functions claimed: volatility profiles, correlation with risk assets, drawdown behavior, and the fee market's effect on usability. They hold no stake in the arrangement's outcomes and publish assessments that both camps cite selectively.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, monetary_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, early_adopter_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monetary expectations among strangers without an issuer: a publicly verifiable supply schedule that no participant can alter at will, plus a shared validation ruleset, solve the double-spend and credible-scarcity problems that previously required trusting an institution.
% TRANSFER_FUNCTION: Moves purchasing power from each new cohort of buyers to earlier cohorts, via appreciation realized on sale, and to miners, via block subsidy and fees; allocates block space to the highest fee bidders, moving settlement priority away from small-value transactors.
% ABSENT_VOICES: Payment-first users, remittance corridors, and merchants are absent from protocol priority-setting — their constituency lost the capacity dispute and exited to other rails. Future entrants have no seat at all: the terms they will buy under are set before they arrive.
% DISAPPEARANCE_RATIONALE: A mining industry, exchange and custody businesses, corporate treasuries, fund structures, and millions of portfolio positions are organized around the arrangement. Overnight disappearance would strand custody chains, void treasury strategies, and collapse a multi-hundred-billion-dollar asset complex; the world rearranges.
% FOUNDING_PROBLEM: The whitepaper's stated problem: online payments require trusting financial institutions as third parties, with the associated costs and access limits; the founding text is titled a peer-to-peer electronic cash system.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the whitepaper text itself and by independent payment-systems literature documenting trusted-third-party costs in online commerce. Its live status is disputed: holder-side voices attest it is solved or mooted under the asset reading, while payment researchers and the rival cash reading attest it remains live and unaddressed. No arbiter outside the disputing parties exists; the disagreement is the kernel contest itself.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__digital_gold_reading_tests).
:- end_tests(bitcoin_whitepaper__digital_gold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: a real coordination function (credible scarcity) coexists with a real cohort-to-cohort transfer — each entrant's purchase embeds prior cohorts' gains, and fee auctions allocate block space away from small users. Suppression 0.35: alternatives exist outside the system (other assets, other rails), but intra-system dissent carried severe costs during the capacity wars, and exit from the asset itself is penalized by drawdown and community norms; suppression is authored as a raw structural property — the engine scales only extractiveness. Theater_ratio 0.36: self-custody rhetoric coexists with rising custodial concentration (exchanges, funds), and decentralization claims outrun operational reality for the median participant. Accessibility_collapse 0.42: understanding the arrangement does not collapse alternatives — rivals exist — but none carries the credibility premium of the original, so the specific scarcity claim collapses competitors only partially. Resistance 0.58: the arrangement met existential resistance (fork wars, economist critique, environmental campaigns) and survived it. CYCLICAL PATTERN: base_extractiveness oscillates on the roughly four-year issuance-halving cycle — fee spikes and peak-period entry at cycle tops (2017: 0.52; 2021: 0.55), relief in the troughs (2019: 0.44; 2023: 0.47). The eight-point shared grid spans approximately two full cycles. The oscillation is partly the mechanism itself, not noise: peak-period enthusiasm draws in the next cohort, whose purchases supply exit liquidity for earlier ones — intermittent reinforcement timed to the halving clock. End-state scalars reflect the mature post-2024 phase. Receipt surface: gains demonstrably accrue to early_adopter_holders (appreciation realized on their sales), so gain_flow names that seat rather than diffuse; fixing_cost is prohibitive because any repair that touches capacity or issuance dissolves the credibility premium that constitutes the asset's value — the cost of fixing exceeds what any single seat would gain, even though the gains themselves stay concentrated.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the early-holder seat the arrangement is hard-won monetary integrity — a coordination achievement they participate in voluntarily and can leave at will. From the late-entrant seat the same structure operates as paying an entry premium set by others' past accumulation, with exit punished by realized loss and social sanction. From the miner seat fees are earned service revenue and the ruleset is a workplace. From the analyst seat the whole is reflexive: belief in the store-of-value function produces the demand that constitutes the function. The engine derives these divergent classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Early_adopter_holders sit nearest the beneficiary pole: they collect the transfer and hold arbitrage-grade exit. Miners derive low-to-moderate directionality — they collect issuance and fees but bear sunk costs and price exposure. Developers and node operators sit near symmetric: they bear stewardship and operating costs while their holdings benefit; identity lock-in amplifies their stake in the ruleset's stability. Custodial intermediaries derive low directionality through arbitrage positioning on flow. Late_entrant_buyers derive high directionality: constrained exit, full burden of embedded appreciation and drawdown. Small_transaction_users are the instructive case: their exit is mobile, which naively damps their computed burden, but descriptively they are victims — while participating they bear the full fee-market burden, and their departure is the arrangement shedding them rather than them escaping it. Their victimhood is realized as exclusion, which is exactly the cost this reading labels acceptable. Unbanked remittance senders bear the burden through priced-out status with no governance voice at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — peer-to-peer electronic cash — is contested territory. This reading declares mandatrophy_resolved true in a specific sense: the cash mandate was not left to decay but deliberately subordinated, replaced by a store-of-value mandate the community now defends as the arrangement's true purpose. The classification prevents mislabeling in both directions. Reading the arrangement as pure extraction erases the genuine coordination achievement — credible scarcity that no issuer could otherwise commit to, which participants value and defend at real cost. Reading it as pure coordination launders the cohort-to-cohort transfer and the shed population of small users behind the language of price discovery. Tangled rope keeps both faces visible: coordination function and asymmetric burden, actively enforced. On the R5 interview: founding_problem_status contested crossed with disappearance_verdict world_rearranges produces no zombie flag — the arrangement is emphatically not persisting past a dead mandate unnoticed; the dispute over what its mandate IS is live, loud, and constitutive of the kernel contest itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the bitcoin_whitepaper kernel governs this arrangement — scarce digital asset (digital gold), peer-to-peer electronic cash, or ossification doctrine — and whose interests does each reading''s victim set name?',
    'Observe protocol-priority decisions under fee pressure: if capacity expands or fee relief ships, the cash reading governs; if scarcity guarantees are defended at the cost of payment utility, the digital-gold reading governs; if even security-budget adjustments are refused outright, the ossification reading governs.',
    'This story instantiates the digital-gold reading with its victim set (late entrants, fee-shed small users). Under the p2p_cash reading the same arrangement carries a broader victim set (all priced-out transactors) and higher epsilon; under the ossification reading the victim set shifts to would-be upgraders and classification turns on governance rigidity rather than asset economics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level ambiguity: which reading of the whitepaper this arrangement instantiates.').

omega_variable(
    sov_hedge_empirical_status,
    'Does the asset actually deliver the store-of-value and inflation-hedging function this reading claims, given its historical correlation with risk assets and its drawdown depth?',
    'Long-horizon portfolio studies spanning full macro cycles, comparing realized volatility, maximum drawdowns, and correlation structure against gold and inflation-linked instruments.',
    'If the hedge claim fails empirically, appreciation rests on reflexive belief alone, and late entrants are revealed as buying narrative rather than hedging — raising the effective burden on the newest cohorts and strengthening the extraction reading of the transfer function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sov_hedge_empirical_status, empirical, 'Whether the store-of-value premise is empirically delivered or narratively sustained.').

omega_variable(
    reflexive_inflow_dependence,
    'Can the appreciation that defines this reading persist without continuous new-buyer inflow, or does the arrangement require each cohort of entrants to fund the previous cohort''s gains?',
    'On-chain cohort analysis across halving cycles: track realized profit ratios and net capital inflow against price; test whether holder gains ever decouple sustainably from new-inflow volume.',
    'If gains require inflow, the transfer function dominates the coordination function and late entrants are structurally exit liquidity; if gains decouple, the monetary-premium coordination story strengthens and the arrangement reads closer to pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reflexive_inflow_dependence, empirical, 'Whether appreciation is self-sustaining or dependent on successive cohorts of new buyers.').

omega_variable(
    scarcity_social_contingency,
    'Is the twenty-one-million supply cap an effectively immutable feature of the system, or a socially enforced convention that a sufficiently broad consensus could revise?',
    'Governance history and comparative stress tests: examine every serious proposal touching issuance, including tail-emission debates on comparable chains, and the coordination cost observed the last time consensus rules were contested.',
    'If the cap is socially contingent, the scarcity guarantee is an enforced coordination product rather than a background condition, and the arrangement sits firmly at the constructed end of the spectrum; if effectively immutable, part of its operation approaches natural-law status and the coordination achievement reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_social_contingency, conceptual, 'Natural-feature versus enforced-convention status of the supply cap.').

omega_variable(
    fee_market_crowding_boundary,
    'At what fee levels do small-value users exit permanently rather than wait, and how large is the permanently shed population versus the episodic one?',
    'Fee-elasticity studies of on-chain transaction counts by value band across congestion episodes, cross-checked against adoption of off-chain layers and alternative-rail substitutes.',
    'Determines the size of the victim set attributable to fee competition: a large permanent shed population raises the arrangement''s human cost under this reading; a mostly episodic one supports the acceptable-cost framing and lowers effective extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fee_market_crowding_boundary, empirical, 'Boundary of the fee-competition victim set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 2009, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement_basis(bitc_tr_t2009, observed).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2012, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t2012, observed).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t2015, observed).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement_basis(bitc_tr_t2017, observed).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2019, 0.25).
narrative_ontology:measurement_basis(bitc_tr_t2019, observed).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2021, 0.32).
narrative_ontology:measurement_basis(bitc_tr_t2021, observed).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2023, 0.34).
narrative_ontology:measurement_basis(bitc_tr_t2023, observed).
narrative_ontology:measurement(bitc_tr_t2026, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2026, 0.36).
narrative_ontology:measurement_basis(bitc_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2009, 0.15).
narrative_ontology:measurement_basis(bitc_be_t2009, observed).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2012, 0.22).
narrative_ontology:measurement_basis(bitc_be_t2012, observed).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement_basis(bitc_be_t2015, observed).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2017, 0.52).
narrative_ontology:measurement_basis(bitc_be_t2017, observed).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2019, 0.44).
narrative_ontology:measurement_basis(bitc_be_t2019, observed).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement_basis(bitc_be_t2021, observed).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2023, 0.47).
narrative_ontology:measurement_basis(bitc_be_t2023, observed).
narrative_ontology:measurement(bitc_be_t2026, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2026, 0.46).
narrative_ontology:measurement_basis(bitc_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement_basis(bitc_su_t2009, observed).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2012, 0.12).
narrative_ontology:measurement_basis(bitc_su_t2012, observed).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement_basis(bitc_su_t2015, observed).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement_basis(bitc_su_t2017, observed).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement_basis(bitc_su_t2019, observed).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement_basis(bitc_su_t2021, observed).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2023, 0.38).
narrative_ontology:measurement_basis(bitc_su_t2023, observed).
narrative_ontology:measurement(bitc_su_t2026, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2026, 0.35).
narrative_ontology:measurement_basis(bitc_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bitcoin' conflates at least three structurally distinct claims: a scarce-asset thesis, a cash-medium thesis, and a governance-stability doctrine. Per the epsilon-invariance principle these are decomposed into three linked stories sharing the whitepaper kernel. The upstream text anchors all three; this reading's victim set (late entrants priced by appreciation, users shed by fee competition) differs from the p2p_cash reading's (transactors broadly priced out of the medium) and the ossification reading's (would-be upgraders blocked by governance rigidity). This reading INFLUENCES the ossification reading — defending scarcity drives immutability politics — and COEXISTS with the cash reading as live rival positions held by different factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
