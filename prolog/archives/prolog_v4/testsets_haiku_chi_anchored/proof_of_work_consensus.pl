% ============================================================================
% CONSTRAINT STORY: proof_of_work_consensus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proof_of_work_consensus, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proof_of_work_consensus
 *   human_readable: Hash-Based Proof-of-Work Consensus
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Proof-of-work consensus replaces trusted third parties (payment
 *   processors, central banks) with a distributed computational mechanism:
 *   agents race to solve hash puzzles, and the first to solve one earns the
 *   right to append a block and collect rewards. This constraint exhibits the
 *   full range of DR classifications depending on structural position. From
 *   the environmental cost bearer's view, it is pure extraction (Snare) —
 *   they bear global externalities with no exit. From the mining hardware
 *   manufacturer's view, it is pure coordination (Rope) — the mechanism
 *   creates demand for innovation and manufacturing. From the competitive
 *   miner's view, it is mixed (Tangled Rope) — they participate in security
 *   provisioning while experiencing reward concentration. From the protocol
 *   maintainer's view, it solves a genuine coordination problem (Rope) —
 *   replacing trusted intermediaries with cryptographic certainty. From the
 *   energy-constrained jurisdiction's view, it is mixed (Tangled Rope) —
 *   mining drives local electricity cost and tax revenue simultaneously. The
 *   legacy payment network sees it as degraded theater (Piton) — the
 *   decentralization narrative persists even as technical alternatives
 *   mature. The analytical observer risks seeing it as a cryptographic
 *   natural law (Mountain), but the structural data reveals this as a false
 *   summit: the choice of proof-of-work over proof-of-stake or other
 *   mechanisms is contingent institutional design, not immutable law. The
 *   constraint's extractiveness (0.52) and suppression (0.68) reveal that
 *   proof-of-work functions as tangled coordination/extraction: it genuinely
 *   solves consensus without central authority, but it also creates energy
 *   rent concentration, transaction fee markets with imperfect efficiency,
 *   and environmental externalities that lock out powerless agents.
 *
 * KEY AGENTS:
 *   - Mining Hardware Manufacturers: Primary beneficiary (institutional/arbitrage) — ASIC innovation and manufacturing driven by proof-of-work demand. Can exit by pivoting hardware.
 *   - Protocol Maintainers: Primary beneficiary (institutional/arbitrage) — proof-of-work provides decentralized consensus guarantee. Can arbitrage to alternative mechanisms (PoS).
 *   - Environmental Cost Bearers: Primary victim (powerless/trapped) — global electricity consumption, carbon emissions, e-waste. No exit option.
 *   - Low-Capital Transaction Senders: Secondary victim (powerless/trapped) — trapped in fee market determined by hash power supply and block space scarcity.
 *   - Competitive Miners: Secondary victim/mixed (moderate/constrained) — constrained by ASIC and electricity costs; benefit from block rewards and security provisioning.
 *   - Energy-Constrained Jurisdictions: Tertiary actor (organized/constrained) — mining concentration drives electricity demand and cost; constrained by sovereignty limits over hash flows.
 *   - Legacy Payment Networks: Institutional actor (powerful/mobile) — perceive threat but maintain relevance through regulatory integration and stablecoin alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proof_of_work_consensus, 0.52).
domain_priors:suppression_score(proof_of_work_consensus, 0.68).
domain_priors:theater_ratio(proof_of_work_consensus, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proof_of_work_consensus, extractiveness, 0.52).
narrative_ontology:constraint_metric(proof_of_work_consensus, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(proof_of_work_consensus, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proof_of_work_consensus, tangled_rope).
narrative_ontology:human_readable(proof_of_work_consensus, "Hash-Based Proof-of-Work Consensus").
narrative_ontology:topic_domain(proof_of_work_consensus, "technological/economic").

domain_priors:requires_active_enforcement(proof_of_work_consensus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proof_of_work_consensus, mining_hardware_manufacturers).
narrative_ontology:constraint_beneficiary(proof_of_work_consensus, early_adopters).
narrative_ontology:constraint_beneficiary(proof_of_work_consensus, protocol_maintainers).
narrative_ontology:constraint_victim(proof_of_work_consensus, network_energy_consumption).
narrative_ontology:constraint_victim(proof_of_work_consensus, environmental_externalities).
narrative_ontology:constraint_victim(proof_of_work_consensus, transaction_inclusion_parity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENVIRONMENTAL COST BEARER (SNARE) — Bears full externalized cost of network hash power (global electricity consumption, carbon emissions, e-waste). No exit option; cannot refuse to bear environmental cost of transactions. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(proof_of_work_consensus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-CAPITAL TRANSACTION SENDER (SNARE) — Trapped in fee market determined by hash power supply. Cannot exit network without abandoning transaction. Cannot arbitrage to lower-fee alternative when one miner controls majority hash rate. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.87.
constraint_indexing:constraint_classification(proof_of_work_consensus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPETITIVE MINER (TANGLED ROPE) — Constrained by ASIC capital requirements and electricity costs; also benefits from network security provisioning and block reward structure. Participates in coordination (securing ledger) while experiencing extraction (concentrated reward). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(proof_of_work_consensus, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MINING HARDWARE MANUFACTURER (ROPE) — Primary beneficiary. Experiences constraint as pure coordination mechanism: proof-of-work demand drives ASIC innovation and manufacturing. Can exit by pivoting to other hardware. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(proof_of_work_consensus, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTOCOL MAINTAINER (ROPE) — Benefits from proof-of-work's security properties and decentralized consensus guarantee. Experiences constraint as coordination solution replacing trusted third party. Can arbitrage to alternative consensus mechanisms (PoS, etc.). d≈0.12, f(d)≈-0.02, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(proof_of_work_consensus, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENERGY-CONSTRAINED JURISDICTION (TANGLED ROPE) — Mining concentration in jurisdiction drives electricity demand and cost. Local government is constrained by sovereignty limits over hash power flows (miners can relocate). Also depends on mining tax revenue and network access. d≈0.58, f(d)≈0.73, σ=1.1 → χ≈0.44.
constraint_indexing:constraint_classification(proof_of_work_consensus, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY PAYMENT NETWORK (PITON) — Proof-of-work consensus maintains its perceived necessity through theater (decentralization narrative, censorship resistance discourse) even as institutional alternatives (stablecoins, regulatory payment rails) mature. The constraint persists through market narrative rather than technical necessity. theater_ratio≈0.58; chain performs payment function poorly (slow, expensive) relative to alternatives. d≈0.52, f(d)≈0.65, σ=0.9 → χ≈0.19.
constraint_indexing:constraint_classification(proof_of_work_consensus, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / CRYPTOGRAPHIC NATURAL LAW (MOUNTAIN CANDIDATE) — From civilizational/universal view, proof-of-work appears as immutable cryptographic principle: majority hash power cannot be forged; consensus requires solving discrete problems with no shortcut. ε=0.52, suppression=0.68 contradict mountain gates (ε≤0.25, suppression≤0.05). The 'natural law' framing naturalizes what is actually an institutional choice (proof-of-work vs proof-of-stake vs DAG-based consensus). False summit detection: this is not a cryptographic law but a contingent protocol design.
constraint_indexing:constraint_classification(proof_of_work_consensus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proof_of_work_consensus_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(proof_of_work_consensus, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proof_of_work_consensus, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(proof_of_work_consensus, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(proof_of_work_consensus, TR),
    TR >= 0.70.

:- end_tests(proof_of_work_consensus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The protocol creates genuine value (decentralized consensus) but concentrates economic reward through block subsidies and fees. The extraction is not absolute dominance of powerless by powerful, but rather structural asymmetry: those with capital (hardware, electricity) benefit disproportionately. Over the interval, extractiveness has risen from 0.28 to 0.52 as mining has industrialized (ASIC manufacturers captured efficiency gains, small miners dropped out, hash concentration increased). Suppression (0.68): Moderate-high. Barriers to participation include: (1) capital requirements for competitive ASICs (~$10K-$100K), (2) electricity cost arbitrage (miners must locate near cheap power), (3) knowledge barriers (pool participation lowers barrier but centralizes hash), (4) environmental externalities are suppressed from market signals (energy cost not fully reflected in fee market). These barriers are structural, not artificial — but they are not absolute. Theater ratio (0.58): Moderate-high. The decentralization narrative supports the protocol's legitimacy, but the actual degree of decentralization has declined as pool concentration increased. As of 2025, top 4-5 mining pools control >50% of hash rate. The narrative of 'anyone can mine' persists despite hardware and capital requirements excluding most participants. Theater has risen from 0.35 to 0.58 as institutional alternatives (stablecoins, regulated payment rails) have matured, making the 'trustless' framing increasingly performative for payment use cases.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival disagreement. Environmental cost bearers see pure extraction (Snare, χ≈0.88) — they are trapped and extracting nothing. Mining manufacturers see pure coordination (Rope, χ≈-0.04) — they are beneficiaries and can exit. Competitive miners see tangled coordination/extraction (Tangled Rope, χ≈0.49) — they benefit from rewards but constrained by capital and energy costs. The analytical observer risks seeing natural law (Mountain) — cryptographic proofs cannot be forged — but the structural data reveals contingent design choices. The legacy payment network sees degraded theater (Piton) — the mechanism persists through narrative even as technical and regulatory alternatives mature. This perspectival range is diagnostic: it shows that proof-of-work is neither pure coordination nor pure extraction, but a hybrid that appears differently depending on whether you are (a) bearing environmental costs with no exit, (b) manufacturing hardware with market options, (c) competing for rewards under capital constraints, or (d) offering alternative payment rails.
 *
 * DIRECTIONALITY LOGIC:
 *   Mining hardware manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Strong net beneficiary (negative χ means coordination subsidy). Protocol maintainers: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.02. Net beneficiary. Competitive miners: Victim + constrained → d≈0.65, f(d)≈0.95. Moderate extraction (constrained exit due to sunk capital in hardware and electricity infrastructure). Low-capital transaction senders: Victim + trapped → d≈0.93, f(d)≈1.40. High extraction (cannot exit, cannot arbitrage). Environmental cost bearers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction (abstract collective, no agency). Energy-constrained jurisdictions: Constrained + dependent on mining revenue → d≈0.58, f(d)≈0.73. Moderate extraction (benefit from tax revenue but harmed by electricity costs and relocation risk). The directionality derivation shows that proof-of-work creates multiple victim classes with different exit constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled_rope classification is correct: genuine coordination function (decentralized consensus without central authority) coexists with asymmetric extraction (capital and electricity concentration, environmental externalities, fee market power). The mandatrophy resolution: (1) Proof-of-work is NOT pure extraction (Snare) because the consensus mechanism truly solves a coordination problem — there is real security value. (2) It is NOT pure coordination (Rope) because the extraction mechanisms (mining rewards, fee markets, capital concentration, environmental externalities) are substantial and asymmetric. (3) It exhibits active enforcement (protocol rules enforce puzzle difficulty, block rewards, 10-minute block time). (4) It has beneficiaries (hardware manufacturers, early adopters) and victims (environmental cost bearers, low-capital participants, energy-constrained jurisdictions). (5) The theater ratio (0.58) reflects that the decentralization narrative persists despite declining actual decentralization, and payment use cases are increasingly served by alternatives, making the 'trustless' framing partially theater. The tangled_rope classification is justified: coordination + enforcement + asymmetric extraction + rising theater indicates a hybrid mechanism that genuinely solves consensus but increasingly functions as a wealth concentration device. The false summit (mountain perspective) reveals the analytical risk of naturalizing institutional choices as cryptographic law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hashrate_51_percent_threshold,
    'At what hashrate concentration does proof-of-work security degrade from collective coordination to majoritarian extraction?',
    'Empirical monitoring of Herfindahl-Hirschman Index (HHI) for mining pool concentration; threshold where double-spending attacks become economically rational',
    'If threshold < 40% concentration: modest extraction, rope dominates. If threshold > 65% concentration: snare dominates, network security is myth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hashrate_51_percent_threshold, empirical, 'Hashrate concentration threshold for security degradation').

omega_variable(
    renewable_subsidy_counterfactual,
    'Would proof-of-work mining occur at current scales without renewable energy subsidies and stranded hydro capacity?',
    'Economic modeling of mining profitability under true electricity cost; counterfactual analysis of mining geography in absence of subsidies',
    'If mining requires subsidies: suppression of alternative consensus mechanisms is partly artificial (policy-driven extraction). If mining profitable without subsidies: extraction mechanism is more fundamental (resource scarcity drives coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_subsidy_counterfactual, empirical, 'Whether proof-of-work profitability depends on energy subsidies').

omega_variable(
    transaction_fee_market_efficiency,
    'Does the fee market mechanism reliably allocate block space to highest-value transactions, or does it create deadweight loss through strategic delay and batching?',
    'Analysis of unconfirmed transaction pool; comparison of actual allocation vs economic social welfare maximization; measurement of strategic fee underbidding and mempool volatility',
    'If efficient: fee extraction is fair market coordination (Rope for more perspectives). If inefficient: fee extraction is pure rent-seeking (Snare for more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transaction_fee_market_efficiency, empirical, 'Whether proof-of-work fee market allocates block space efficiently').

omega_variable(
    decentralization_narrative_obsolescence,
    'As institutional stablecoins and regulated payment rails mature, does the decentralization narrative supporting proof-of-work legitimacy become theater?',
    'Survey of actual transaction use case distribution; comparison of proof-of-work adoption in censorship-resistant contexts vs convenience-maximizing contexts',
    'If narrative obsolete: piton classification confirmed, theater_ratio should increase. If narrative still functional: rope classification more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_narrative_obsolescence, conceptual, 'Whether decentralization narrative remains functional vs theatrical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proof_of_work_consensus, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pow_tr_t0, proof_of_work_consensus, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pow_tr_t7, proof_of_work_consensus, theater_ratio, 7, 0.47).
narrative_ontology:measurement(pow_tr_t14, proof_of_work_consensus, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(pow_be_t0, proof_of_work_consensus, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pow_be_t7, proof_of_work_consensus, base_extractiveness, 7, 0.41).
narrative_ontology:measurement(pow_be_t14, proof_of_work_consensus, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proof_of_work_consensus, enforcement_mechanism).
narrative_ontology:affects_constraint(proof_of_work_consensus, bitcoin_money_supply_ceiling).
narrative_ontology:affects_constraint(proof_of_work_consensus, mining_pool_centralization).
narrative_ontology:affects_constraint(proof_of_work_consensus, cryptocurrency_energy_externality).

% DUAL FORMULATION NOTE:
% Proof-of-work consensus is the upstream constraint enabling cryptocurrency networks. Downstream constraints include specific cryptocurrency money supply mechanics (bitcoin_money_supply_ceiling, ε≈0.08, Rope — consensus enforces supply cap), mining pool concentration (mining_pool_centralization, ε≈0.65, Tangled Rope — extraction mechanism for solo miners), and energy externalities (cryptocurrency_energy_externality, ε≈0.71, Snare — pure environmental cost). The constraint family demonstrates ε-invariance principle: the same 'proof-of-work' label covers structurally distinct claims (consensus mechanism, supply enforcement, energy consumption, fee market) with different ε values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(proof_of_work_consensus, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
