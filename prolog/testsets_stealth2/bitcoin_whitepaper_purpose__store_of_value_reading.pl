% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__store_of_value_reading, []).

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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Small-Block Policy (Store-of-Value Reading)
 *   domain: technology/economic/monetary
 *
 * SUMMARY:
 *   The Bitcoin protocol retains a deliberately small on-chain capacity
 *   envelope (the 1MB-era block limit, carried forward through SegWit's
 *   modest effective increase), justified by the doctrine that
 *   decentralization and full-node verifiability are the binding constraints
 *   and that on-chain capacity must be subordinated to them. This story
 *   instantiates the store_of_value_reading of the bitcoin_whitepaper_purpose
 *   kernel: the arrangement is assessed as the protection of a verification
 *   commons that underwrites Bitcoin's monetary premium, with the fee market
 *   as the successor security budget. The same physical arrangement, read
 *   through the sibling electronic_cash_reading, is a different constraint
 *   with a different epsilon — that story is authored separately and linked
 *   here. Family relationship: the whitepaper text is the upstream artifact;
 *   this reading won control of the base layer in the 2015-2017 capacity wars
 *   and now defines the operating environment (fee levels, settlement
 *   guarantees, forced layering) within which the electronic-cash reading
 *   survives on sidechains and sibling chains. Under this reading's own
 *   lights the arrangement carries a genuine coordination function (keeping
 *   validation permissionless by holding hardware requirements at consumer
 *   level) AND asymmetric costs (low-value users priced off the base layer),
 *   which is why the structural claim is hybrid coordination/extraction
 *   rather than either pure pole.
 *
 * KEY AGENTS:
 *   - long_term_holders: primary beneficiary (organized/mobile) — collects the monetary premium the scarcity doctrine underwrites; bears no operational cost
 *   - full_node_operators: primary beneficiary (moderate/identity_locked) — the verification practice the capacity ceiling protects; their hardware floor IS the policy's product
 *   - bitcoin_miners: dual-positioned beneficiary-and-cost-bearer (institutional/constrained) — receives the fee revenue the congestion concentrates, holds sunk SHA-256 capital, lost the capacity war politically
 *   - core_protocol_developers: agenda setter (organized/identity_locked) — stewards the reference client and review process; no formal authority, decisive gatekeeping in practice
 *   - low_value_onchain_users: primary target (powerless/constrained) — remittance senders and small savers for whom congestion-period fees exceed transaction value
 *   - onchain_payment_merchants: secondary target (moderate/constrained) — saw the on-chain payment use case priced out and were pushed onto layered dependencies
 *   - big_block_fork_advocates: excluded voice (powerful/mobile) — moderated out of the main venues during the wars, forked away; their absence is structural
 *   - lightning_routing_operators: secondary beneficiary (moderate/mobile) — earn fees from payment flow the base layer declined; exist because of the policy
 *   - protocol_research_community: analytical observer (analytical/analytical) — models node-cost curves and fee-market equilibria cited by all camps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.52).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.34).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Small-Block Policy (Store-of-Value Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "technology/economic/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, 'a4466ac1-d581-4270-b8eb-9643e48edb20').
narrative_ontology:cs_kernel_codification('a4466ac1-d581-4270-b8eb-9643e48edb20', fixed_text).
narrative_ontology:cs_authority_grounding('a4466ac1-d581-4270-b8eb-9643e48edb20', distributed).
narrative_ontology:cs_reading_relation('a4466ac1-d581-4270-b8eb-9643e48edb20', bitcoin_whitepaper_purpose__electronic_cash_reading, influences).
narrative_ontology:cs_axiom('a4466ac1-d581-4270-b8eb-9643e48edb20', foundational, onchain_capacity_subordinate_to_verifiability).
narrative_ontology:cs_axiom_status(onchain_capacity_subordinate_to_verifiability, holdable).
narrative_ontology:cs_axiom_grounding('a4466ac1-d581-4270-b8eb-9643e48edb20', onchain_capacity_subordinate_to_verifiability, instrumental).
narrative_ontology:cs_axiom('a4466ac1-d581-4270-b8eb-9643e48edb20', secondary, fee_market_sustains_longterm_security).
narrative_ontology:cs_axiom_status(fee_market_sustains_longterm_security, holdable).
narrative_ontology:cs_axiom_grounding('a4466ac1-d581-4270-b8eb-9643e48edb20', fee_market_sustains_longterm_security, empirically_contingent).
narrative_ontology:cs_reference_frame('a4466ac1-d581-4270-b8eb-9643e48edb20', permissionless_validation_baseline).
narrative_ontology:cs_drift_state('a4466ac1-d581-4270-b8eb-9643e48edb20', contemporary_custodial_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a4466ac1-d581-4270-b8eb-9643e48edb20', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_miners).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, onchain_payment_merchants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_routing_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_miners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as long-duration savings across cycles. The small-block policy underwrites the scarcity credibility and settlement-assurance narrative on which their asset's monetary premium rests. They bear no operational cost of the policy — most run no nodes and increasingly hold through custodians and fund wrappers. Liquid global markets mean they can sell at any time, but selling contradicts the thesis they hold, so exit is available and rarely taken.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Run independently verifying nodes on consumer hardware and bandwidth as the system's civic practice. The capacity ceiling is what keeps their hardware and connection requirements within reach; every proposal to raise it threatens the affordability that makes their participation possible. Their involvement fuses with the verify-don't-trust culture — stopping means abandoning the practice that constitutes their role in the system, not merely canceling a subscription.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary,
    moderate, biographical, identity_locked, global).

% Operate proof-of-work infrastructure and collect the fee revenue that congestion concentrates on top of the scheduled subsidy. Congestion raises their per-block fee income, and the security-budget narrative supports their long-term revenue story. Against that, their capital is sunk in SHA-256 hardware usable for nothing else, they bear congestion-driven revenue volatility, and they lost the capacity war politically — the largest mining interests backed bigger blocks and were defeated by the node-and-holder coalition, demonstrating their subordinate position in governance despite industrial scale.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_miners, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_miners, payer).

% Maintain the reference client, review consensus-relevant changes, and steward the small-block policy as maintainers. They hold no formal authority — anyone can fork the code — but their review gatekeeping and accumulated legitimacy decided every capacity proposal to date. They rejected the big-block forks, shepherded SegWit, and treat the capacity ceiling as an invariant. Their professional identities and standing are bound to the project's mission; departure would cost them the community they built.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, core_protocol_developers, agenda_setter,
    organized, generational, identity_locked, global).

% Remittance senders, small savers in high-inflation economies, and new entrants consolidating small balances. During congestion windows, transaction fees ran from dollars to tens of dollars — frequently exceeding the amount being sent. Their alternatives each carry costs they are least equipped to evaluate: custody reintroduces the trusted-intermediary risk they came to escape, altcoins forfeit the liquidity and assurance they sought, and the Lightning path demands channel management and inbound liquidity beyond casual capability. Many simply stopped transacting on the base layer.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users, payer,
    powerless, immediate, constrained, global).

% Businesses that accepted on-chain bitcoin payments faced unpredictable settlement fees and confirmation delays that made point-of-sale economics unreliable. Most migrated to payment processors or Lightning integrations, adding intermediary layers and dependencies between themselves and their customers. The on-chain payment use case — arguably the whitepaper's headline application — became uneconomic for them under the capacity policy, and their compliance with the layered workaround is what remains of it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, onchain_payment_merchants, payer,
    moderate, biographical, constrained, global).

% Miners, exchange operators, and entrepreneurs who argued during 2015-2017 that capacity must scale on-chain. They funded competing clients (XT, Classic, Unlimited), signed the New York Agreement, and were met with forum moderation, denial-of-service attacks on dissenting nodes, and eventual organizational defeat. Those who continued the project forked to a sibling chain. They retain resources and conviction but no longer sit where base-layer decisions are made; their absence from the current conversation is structural, not voluntary silence.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, big_block_fork_advocates, excluded,
    powerful, biographical, mobile, global).

% Operate routing nodes and manage channel liquidity, earning fees from payment flow that the base layer declined to carry. Their business line exists because of the capacity policy — it is the sanctioned absorption path. Large routing hubs concentrate liquidity and introduce new intermediation questions, but operators bear little of the base-layer governance conflict and can redeploy capital elsewhere if the layer's economics deteriorate.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_routing_operators, beneficiary,
    moderate, biographical, mobile, global).

% Academic and independent engineers who model node-cost curves, fee-market equilibria, decentralization metrics, and security-budget projections. Both camps cite their work: capacity proponents lean on studies suggesting larger blocks remain verifiable, small-block proponents on studies of validation-cost growth and node attrition. They hold no stake beyond epistemic standing and publish findings that cut against whichever side's assumptions fail.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, protocol_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_miners).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps full transaction validation affordable at consumer hardware and bandwidth levels by capping blockspace, so that the set of people who can independently verify the ledger stays permissionless and no miner coalition or state actor can push through rules the broad validator population cannot audit. Each user privately prefers more capacity for their own transactions; the cap solves that collective-action problem by binding everyone to the hardware floor that preserves the common verification property.
% TRANSFER_FUNCTION: Moves blockspace allocation from high-volume/low-value transactions to whatever transactions bid highest during scarcity; moves fee revenue from users needing on-chain settlement to miners; moves the payment activity of priced-off users onto Lightning operators' routing tables; and, through the scarcity doctrine the policy embodies, moves monetary premium toward existing holders as new demand bids up a fixed settlement guarantee.
% ABSENT_VOICES: Big-block advocates and the priced-off payment cohort. The advocates were actively removed — forum moderation and infrastructure attacks during the capacity wars — and now deliberate on a sibling chain; the low-value users, disproportionately in developing economies, never had a governance seat at all: protocol deliberation happened on English-language developer channels they neither populated nor shaped. Both would object that the capacity decision traded their use case for a property they were never consulted on.
% DISAPPEARANCE_RATIONALE: If the capacity ceiling vanished overnight, blocks would flood with low-fee and gratuitous traffic, hardware and bandwidth requirements for full validation would climb steadily, node operation would professionalize toward datacenters over a few years, and the independently-verifiable-scarcity property on which the asset's monetary premium rests would erode. Holders would repricing risk, miners would face altered fee dynamics, the layered ecosystem would lose its settlement anchor, and rival chains would inherit part of the premium. Arrangements across the entire ecosystem depend on the ceiling holding.
% FOUNDING_PROBLEM: The limit was installed by the system's creator in 2010 as a temporary anti-spam measure — a crude cap to prevent gratuitous block bloat while the network was young and unmonetized — with the explicit expectation that it would be raised as the system grew.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: archived mailing-list and forum posts by the creator and early developers document the temporary anti-spam intent and the expectation of a later raise; the big-block camp's entire historical argument rests on that record, making them adversarial corroborators of the genealogy; independent systems research on node-cost trajectories addresses the successor question on the merits. The surviving camp disputes the 'temporary' characterization and attests that the decentralization-preservation rationale is live — that dispute is recorded, but the founding-problem-is-dead finding itself rests on sources with no stake in the small-block policy's continuation.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52: congestion-period fees routinely exceeded the value of small transactions, pricing a real cohort off the base layer, yet roughly half the burden is absorbed by the fee market's intended function (post-subsidy security budget) rather than pure overhead. Suppression is authored at 0.34 and is a raw structural property, unscaled: the arrangement does not trap participants — it channels them. Exits exist (other chains, custody, the sanctioned Lightning path) but each carries costs the weakest users cannot fully evaluate, and the historical enforcement episode (forum moderation, node denial-of-service against the XT fork, the New York Agreement's defeat) shows the social machinery that defended the policy at its peak intensity before settling into passive consensus-rule enforcement. Theater_ratio 0.32: the hardware-floor function is real and continuously operative, but a growing share of 'decentralization' activity is rhetorical identity performance conducted by holders who run no nodes and hold through custodians — hence the slow climb from 0.10 to 0.32. Accessibility_collapse 0.35: alternatives demonstrably persist (BCH/BSV exist, other L1s thrive, Lightning operates), so understanding the constraint does not collapse the option space. Resistance 0.6: the block size wars were the most intense governance conflict in the system's history; resistance continues in fee-spike complaints and inscription-controversy episodes. The claim and the metrics are independent authored facts: I claim tangled_rope because both a coordination function and an asymmetric cost structure are structurally present; the metrics describe what the arrangement actually does. All temporal series run on one shared eight-point grid (t0~2010 1MB patch through t14~2025) so every metric is authored at every examined time point; the extractiveness oscillation (peak at t8, the 2017-18 fee crisis, partial relief at t10 as SegWit and early Lightning absorbed flow, renewed pressure at t12-14) tracks adoption and halving cycles rather than an engineered reinforcement loop, and the base_properties scalars reflect the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the long_term_holder seat the arrangement is the guardian of a savings technology: the capacity ceiling is what makes self-verifiable scarcity credible, and the fee burden falls on a use case they have abandoned anyway. From the low_value_user seat the same arrangement operates as a toll gate that charged them out of the system during exactly the period they tried to adopt it. From the miner seat it is double-edged: fee revenue concentrates in congestion, but their capital is chained to the winning policy they did not choose. From the developer seat it is stewardship of an invariant they believe non-negotiable. The engine computes per-seat classifications from the structural data; this story does not adjudicate which seat is 'right'.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (long_term_holders, full_node_operators, bitcoin_miners) derive low directionality — the arrangement subsidizes them: holders via the monetary premium, node operators via a protected hardware floor, miners via concentrated fee revenue. Declared victims (low_value_onchain_users, onchain_payment_merchants) derive high directionality, amplified by their constrained exits: the sanctioned Lightning path imposes liquidity and operational costs that fall hardest on precisely the users the base layer priced out, and the unsanctioned exits (altcoins, custody) forfeit the property they came for. Global spatial scope applies the engine's modest verification-difficulty amplification. Miners are dual-declared (beneficiary with payer secondary role) because they receive the transfer AND bear sunk-capital and political costs; their net position sits beneficiary-side but not at the pure end. Core developers, as agenda setters outside the beneficiary/victim arrays, take the power-atom fallback — their true position is modestly beneficiary-side (mission, standing, and relevance are bound to the policy they maintain) at near-zero direct cost, which the commentary flags for the engine's attention without an override, since no single power-atom-keyed override could distinguish them from the holders who share their atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead and the arrangement persists — this is the honest zombie signature, and the R5 mismatch (dead founding problem x world_rearranges verdict) fires deliberately. The 1MB limit was installed in 2010 as a temporary anti-spam patch while the system was young; that problem vanished years ago. What persists is a successor mandate — protection of the verification commons — which is load-bearing (the world would rearrange without it: blocks would fill with low-value traffic, hardware floors would climb, validation would professionalize) but whose necessity is exactly what the sibling reading contests. The classification discipline prevents the two standard mislabels: the electronic-cash camp reads the arrangement as pure extraction (a snare run for holder rents), which erases the real coordination function; the maximalist camp reads it as natural law (the scalability trilemma as physics), which erases the fact that the limit was a discretionary patch, twice contested, and defeasible by consensus. Hybrid coordination/extraction keeps both faces visible simultaneously. The theater_ratio trajectory matters here: if custodial drift continues, the protected practice shrinks beneath the policy and the arrangement trends toward inertia-sustained vestige — the omega variable custodial_practice_drift_reversibility tracks exactly that failure mode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (store_of_value_reading) of the bitcoin_whitepaper_purpose kernel. What structural facts would change if the sibling electronic_cash_reading were instantiated instead?',
    'Author the sibling as a separate constraint story and compare epsilon, beneficiary/victim sets, and computed types across readings. The disagreement is located in which element of the whitepaper is telos-binding: the title''s ''cash'' purpose versus the decentralization and verification sections.',
    'Under the electronic_cash_reading the same capacity limit computes as high-epsilon extraction from payment users with the holder class as capturer; under this reading it computes as moderate-epsilon hybrid coordination protecting a verification commons. The cross-reading comparison is the measurement; neither value is reconciled into the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one kernel, two readings, two constraints.').

omega_variable(
    verifiability_rhetoric_vs_holder_rent,
    'Is the full-node-verifiability justification a genuine coordination function, or cover for holder-class rents from artificially scarce blockspace?',
    'Independent node-cost curve analysis at candidate block sizes; governance behavior each time capacity increases are proposed; whether verifiability claims track actual node operation and hardware floors or function as identity rhetoric.',
    'If the justification is cover, the arrangement reclassifies toward pure extraction with long_term_holders as the capturing seat; if genuine, the hybrid-coordination reading stands and the fee burden is the price of the commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verifiability_rhetoric_vs_holder_rent, conceptual, 'Whether identity-framed decentralization talk masks scarcity rent.').

omega_variable(
    fee_market_security_sufficiency,
    'Will congestion-generated fee revenue sustain hash-rate security as the block subsidy decays across future halvings?',
    'Track fee-to-subsidy ratios, hash-rate elasticity to fee spikes, and post-halving security budgets through successive subsidy eras.',
    'If insufficient, the capacity policy undermines its own security rationale and the arrangement trends toward theatrical maintenance of a failing good; if sufficient, the security-budget hypothesis the constraint vindicates is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_security_sufficiency, empirical, 'Empirical test of the fee-market security-budget premise.').

omega_variable(
    custodial_practice_drift_reversibility,
    'Does the migration of holdings into custodial and fund structures hollow out the verification practice the capacity policy protects?',
    'Track non-custodial share of supply, reachable-node counts versus economic weight, and self-custody friction trends over time.',
    'If drift continues, performative maintenance grows and the arrangement trends toward inertia-sustained vestige; if self-custody tooling reverses it, the protective function strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_practice_drift_reversibility, empirical, 'Whether the protected practice is shrinking beneath the policy.').

omega_variable(
    lightning_exit_authenticity,
    'Does the Lightning Network actually restore low-fee payment capability for users priced off the base layer, or does it relocate their costs into routing fees, inbound-liquidity requirements, and hub concentration?',
    'Measure off-layer fee incidence for small payments, channel-opening costs, routing reliability, and liquidity concentration among large routing nodes.',
    'If the off-layer path fails the priced-off cohort, their exit option is illusory and effective suppression and extraction rise above the authored values; if it works, the victim class is genuinely served off-layer and the extraction is bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_exit_authenticity, empirical, 'Authenticity of the sanctioned exit path for low-value users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_sov_reading_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(btc_sov_reading_tr_t2, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2, 0.13).
narrative_ontology:measurement(btc_sov_reading_tr_t4, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(btc_sov_reading_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(btc_sov_reading_tr_t8, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(btc_sov_reading_tr_t10, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(btc_sov_reading_tr_t12, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(btc_sov_reading_tr_t14, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 14, 0.32).

% Extraction over time
narrative_ontology:measurement(btc_sov_reading_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(btc_sov_reading_be_t2, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2, 0.26).
narrative_ontology:measurement(btc_sov_reading_be_t4, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(btc_sov_reading_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(btc_sov_reading_be_t8, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(btc_sov_reading_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(btc_sov_reading_be_t12, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(btc_sov_reading_be_t14, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 14, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(btc_sov_reading_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(btc_sov_reading_su_t2, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2, 0.26).
narrative_ontology:measurement(btc_sov_reading_su_t4, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(btc_sov_reading_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(btc_sov_reading_su_t8, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(btc_sov_reading_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(btc_sov_reading_su_t12, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(btc_sov_reading_su_t14, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 14, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_offchain_settlement_layer).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Bitcoin's design purpose' conflates two structurally distinct claims. The store_of_value_reading (this file) authors epsilon ~0.52 for the small-block arrangement assessed as protection of a verification commons with asymmetric costs; the electronic_cash_reading authors a materially higher epsilon for the same physical arrangement assessed as denial of the system's stated cash function. Same referent arrangement, different readings, different epsilons, different victim salience — therefore two files, linked here. Upstream/downstream: this reading controls the base layer and thus influences the sibling's operating environment; the lightning_offchain_settlement_layer entry is downstream, existing as the sanctioned absorption path this policy creates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
