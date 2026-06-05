% ============================================================================
% CONSTRAINT STORY: cryptocurrency_mining_centralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cryptocurrency_mining_centralization, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cryptocurrency_mining_centralization
 *   human_readable: Cryptocurrency Mining Centralization
 *   domain: economic/technology/governance
 *
 * SUMMARY:
 *   Cryptocurrency mining centralization is a structural tension between the
 *   protocol's design goal (distributed consensus via decentralized mining)
 *   and the natural equilibrium of proof-of-work systems (centralized pooling
 *   due to variance reduction and economic efficiency). The constraint embeds
 *   genuine coordination (pooled mining solves the variance problem that
 *   makes solo mining economically unviable) alongside extractive asymmetry
 *   (pools capture fees and influence transaction policy; equipment
 *   manufacturers capture technological rents; large farm operators exploit
 *   geographic electricity advantages). The extractiveness has increased over
 *   the measurement interval (0.35 → 0.58) as difficulty scaling and
 *   equipment costs pushed individual participation toward zero, while
 *   theater has remained low (0.30 → 0.48) because the coordination function
 *   is real: pools genuinely do solve variance. This makes the constraint a
 *   clear tangled_rope rather than snare — extraction is embedded in a
 *   functioning coordination mechanism, not pure taking. The constraint's
 *   mandatrophy remains unresolved because alternative consensus mechanisms
 *   (proof-of-stake) promise to eliminate mining centralization entirely, but
 *   the transition is incomplete and partial.
 *
 * KEY AGENTS:
 *   - Individual Miners: Primary victims (powerless/trapped) — unable to solo mine profitably; forced to pool or exit
 *   - Mining Pools: Primary beneficiaries (institutional/arbitrage) — capture coordination rents and transaction policy influence
 *   - Protocol Decentralization: Structural victim (moderate/constrained) — original design principle violated; constrained by the fact that pooling solves a real coordination problem
 *   - ASIC Manufacturers (Bitmain/Antminer): Secondary beneficiaries (institutional/arbitrage) — capture equipment supply rents; effective duopoly
 *   - Mining Farm Operators: Complex institutional position (institutional/constrained) — benefit from scale economies and pooling but constrained by equipment monopolies and electricity costs
 *   - Layer 2 & Alternative Consensus Designers: Organized agents (organized/mobile) — building escape routes via staking, sidechains, and alternative protocols
 *   - Network Security Baseline: Structural victim (powerless/analytical) — concentration creates fork and censorship risk; no agency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cryptocurrency_mining_centralization, 0.58).
domain_priors:suppression_score(cryptocurrency_mining_centralization, 0.62).
domain_priors:theater_ratio(cryptocurrency_mining_centralization, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cryptocurrency_mining_centralization, extractiveness, 0.58).
narrative_ontology:constraint_metric(cryptocurrency_mining_centralization, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cryptocurrency_mining_centralization, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cryptocurrency_mining_centralization, tangled_rope).
narrative_ontology:human_readable(cryptocurrency_mining_centralization, "Cryptocurrency Mining Centralization").
narrative_ontology:topic_domain(cryptocurrency_mining_centralization, "economic/technology/governance").

domain_priors:requires_active_enforcement(cryptocurrency_mining_centralization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cryptocurrency_mining_centralization, large_mining_pools).
narrative_ontology:constraint_beneficiary(cryptocurrency_mining_centralization, asic_manufacturers).
narrative_ontology:constraint_beneficiary(cryptocurrency_mining_centralization, mining_farm_operators).
narrative_ontology:constraint_victim(cryptocurrency_mining_centralization, protocol_decentralization).
narrative_ontology:constraint_victim(cryptocurrency_mining_centralization, small_individual_miners).
narrative_ontology:constraint_victim(cryptocurrency_mining_centralization, network_security_baseline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL MINER (SNARE) — Solo mining is economically unviable; difficulty adjustment and variance guarantee years of zero rewards. Trapped by sunk equipment costs, electricity dependencies, and pool-enforced reward rules. Maximum extraction: the individual can join a pool (abdicate hash power) or remain uncompetitive. No meaningful exit.
constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROTOCOL DECENTRALIZATION (TANGLED ROPE) — Mining centralization violates the original design principle of distribution but provides genuine coordination: pooled mining solves variance, enabling smaller participants to receive predictable rewards. Yet this coordination is embedded in asymmetric extraction — pools take fees (typically 1-3%) and exercise social influence over transaction inclusion policy. The constraint provides coordination with enforced extraction.
constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE MINING POOLS (ROPE) — Benefit from network effects: larger pools have lower variance, attracting hash power, reducing variance further. Pools solve the coordination problem of variance and payment distribution. Beneficiaries experiencing the constraint as pure coordination — they are solving a real problem (variance) while capturing legitimate coordination rents.
constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL DESIGNERS / LAYER 2 (SCAFFOLD) — Organized agents building alternatives: staking systems reduce mining centralization (Ethereum PoS), layer 2 solutions (Lightning, Rollups) reduce on-chain throughput pressure, and alternative consensus mechanisms distribute security differently. These have sunset logic: if staking or sharding mature, mining centralization loses its lock on security. Current suppression moderate because alternatives are partial; exits becoming visible.
constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ASIC MANUFACTURER DUOPOLY (PITON) — Bitmain and Antminer effectively control mining hardware innovation. The constraint between manufacturers and miners is degraded inertia: miners feel locked to whatever chips manufacturers supply because switching costs and obsolescence cycles are predictable, yet the relationship is performative rather than functionally essential. Alternative consensus mechanisms threaten the entire ASIC ecosystem. Theater is maintained through marketing and release cycles.
constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MINING FARM OPERATORS (TANGLED ROPE) — Institutional beneficiaries constrained by electricity costs and geographic advantages. Benefits from pooled mining coordination and network effects. Simultaneously constrained by supply chain monopolies (ASIC manufacturers), difficulty adjustments that force continuous equipment upgrades, and regulatory fragmentation (electricity policy varies by jurisdiction). Active enforcement of extraction through sunk capital requirements.
constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — CONSENSUS SECURITY VIEW (SNARE) — From a civilization-scale view, mining centralization creates consensus vulnerability. A small number of pools (3-5) can collude to fork the chain or censor transactions. The constraint is not immutable law but institutional architecture; however, the analytical observer notes that without mining or mining-like mechanisms, distributed consensus becomes intractable. The snare is structural: mining must exist, and mining naturally centralizes under PoW. This is not a false summit but a genuine dilemma.
constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cryptocurrency_mining_centralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cryptocurrency_mining_centralization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cryptocurrency_mining_centralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cryptocurrency_mining_centralization, TR),
    TR >= 0.70.

:- end_tests(cryptocurrency_mining_centralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated but not extreme. The constraint contains real extraction (pool fees, difficulty-driven obsolescence, equipment monopolies) but is embedded in genuine coordination (pools reduce variance, enabling small participants to earn rewards predictably). The 0.35 → 0.58 trajectory reflects increased consolidation pressure as network difficulty has scaled exponentially, pushing more participants toward zero profitability. The extractiveness would be much higher (0.75+) if the coordination function were absent, but pooled mining actually works — it solves the variance problem. Suppression (0.62): Moderate-high. Significant barriers include equipment costs ($5k-$50k USD), electricity availability (geographic advantage is insurmountable for most), and sunk capital in obsolete ASICs. Solo mining is structurally unviable at current difficulty — the suppression includes both material barriers and technological asymmetry. Theater ratio (0.48): Low-moderate. The constraint is mostly functional rather than performative. Pools actually do distribute variance, calculate shares, and send payments. The theater component emerges from ASIC marketing cycles and the rhetorical commitment to 'decentralization' that the protocol originally promised but structures did not deliver.
 *
 * PERSPECTIVAL GAP:
 *   Mining centralization demonstrates how the same structural constraint can appear as five different types depending on perspective. The individual miner sees snare (trapped, no exit, pure extraction). The pool sees rope (solving coordination, legitimate rent). The protocol designer sees tangled_rope (genuine coordination embedded in asymmetric extraction). The ASIC manufacturer sees piton (maintaining market through hardware cycles, reducing threat via obsolescence). The layer 2 builder sees scaffold (temporary centralization being bypassed by alternatives). The analytical observer risks seeing this as immutable law of PoW (mountain), but the structural data shows it is contingent: it persists because mining exists, pools are efficient, and alternatives are incomplete — not because it is unchangeable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural position relative to extraction flow. Individual miners with trapped exit and victim status derive high d (→ 0.90-0.95), experiencing maximum chi despite the coordination function, because they have no alternatives. Pools with institutional power and arbitrage exit derive low d (→ 0.10-0.15), experiencing near-zero or negative chi because they are beneficiaries with mobility. Protocol decentralization (moderate power, constrained exit, victim status) derives mid-high d (→ 0.65-0.75), experiencing moderate chi because it bears costs but has some agency (layer 2 alternatives are building). ASIC manufacturers (institutional, arbitrage) derive low d (→ 0.05-0.10), similar to pools — they are pure beneficiaries. Mining farms (institutional, constrained, mixed victim/beneficiary) derive moderate d (→ 0.45-0.55) because they benefit from scale economies but are trapped by equipment supply and electricity geography.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED: The constraint's mandatrophy hinges on whether mining centralization is an inherent consequence of PoW (structural necessity) or an institutional arrangement that can be replaced by alternative consensus (contingent). If mining is inherent to Bitcoin's security and cannot be decentralized meaningfully, then the constraint is a snare disguised as rope — pools appear to coordinate but actually extract, with no real alternative. If alternatives (PoS, proof-of-authority, sidechains) genuinely displace mining, then the constraint is scaffold — temporary centralization with a sunset as protocol evolution proceeds. Current evidence is mixed: Ethereum's transition to PoS reduced mining centralization but created validator concentration (did not solve the type). Bitcoin's commitment to PoW means mining centralization persists absent protocol change. The mandatrophy cannot be resolved without determining whether mining-like concentration is an inherent feature of all consensus mechanisms or unique to PoW. Recommend: empirical study of validator concentration in mature PoS systems; economic analysis of whether alternative consensus removes variance-driven pooling; historical simulation of what Bitcoin difficulty and pool concentration would look like under different consensus rules.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mining_pool_extractiveness_boundary,
    'At what fee and behavioral threshold does a mining pool transition from coordination service (legitimate rent) to extractive monopoly (unfair taking)?',
    'Economic analysis of pool fee evolution; correlation between fee level and hash power concentration; comparison of pool fees to infrastructure costs (servers, bandwidth, payment processing); surveys of miner satisfaction and switching behavior.',
    'If boundary < 2%: most current pools are extractive. If boundary > 5%: current centralization is largely justified as coordination rent. Classification of tangled_rope vs snare depends on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mining_pool_extractiveness_boundary, empirical, 'Threshold for mining pool fee extractiveness').

omega_variable(
    asic_manufacturer_moat_sustainability,
    'Is ASIC manufacturing durably concentrated (durable competitive advantage) or contingently so (historical accident + network effects that could be disrupted)?',
    'Technical analysis of ASIC design complexity; patent landscape review; historical analysis of competitive entries (Canaan, Innosilicon, others); correlation between difficulty adjustments and manufacturer market share changes.',
    'If durable: ASIC concentration is a structural feature of PoW, supporting piton/snare classifications. If contingent: the concentration is institutional capture awaiting disruption, supporting scaffold perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asic_manufacturer_moat_sustainability, empirical, 'ASIC manufacturer competitive moat durability').

omega_variable(
    protocol_decentralization_real_or_rhetorical,
    'Is protocol decentralization (node distribution) a functional feature or a rhetorical goal that mining centralization undermines?',
    'Analysis of node count evolution; correlation between mining centralization and full node reduction; empirical study of whether node count affects transaction censorship resistance or chain forking dynamics.',
    'If functional: mining centralization genuinely violates original design, supporting snare/tangled_rope from protocol perspective. If rhetorical: decentralization is marketing, and mining concentration is the actual design equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_decentralization_real_or_rhetorical, empirical, 'Whether protocol decentralization is functionally essential').

omega_variable(
    staking_as_genuine_alternative,
    'Do proof-of-stake systems (Ethereum, future Bitcoin alternatives) actually solve mining centralization or recreate it with different mechanics (validator concentration, stake delegation)?',
    'Comparative analysis of validator concentration in PoS systems; empirical measurement of delegation patterns; assessment of whether PoS eliminates the variance problem that drives PoW pooling.',
    'If genuine solution: scaffold perspective confirmed — alternative consensus provides sunset path. If recreates centralization: mining centralization persists in different form, reducing sunset likelihood.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(staking_as_genuine_alternative, empirical, 'Whether proof-of-stake eliminates mining-style centralization').

omega_variable(
    individual_miner_viability_threshold,
    'At what hardware cost / difficulty level could solo mining become viable for meaningful participant sets (>1% of network)?',
    'Economic modeling: difficulty scaling vs hardware cost evolution; simulation of alternative consensus mechanisms with different variance profiles; historical analysis of periods when solo mining was viable.',
    'If threshold < $10k USD: pool centralization is policy choice, not inevitability (snare classification supported). If threshold > $100k+: mining centralization is inherent to PoW (structural rather than extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_miner_viability_threshold, empirical, 'Economic viability threshold for individual mining').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cryptocurrency_mining_centralization, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crypto_mining_tr_t0, cryptocurrency_mining_centralization, theater_ratio, 0, 0.3).
narrative_ontology:measurement(crypto_mining_tr_t3, cryptocurrency_mining_centralization, theater_ratio, 3, 0.35).
narrative_ontology:measurement(crypto_mining_tr_t6, cryptocurrency_mining_centralization, theater_ratio, 6, 0.42).
narrative_ontology:measurement(crypto_mining_tr_t9, cryptocurrency_mining_centralization, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(crypto_mining_be_t0, cryptocurrency_mining_centralization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crypto_mining_be_t3, cryptocurrency_mining_centralization, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(crypto_mining_be_t6, cryptocurrency_mining_centralization, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(crypto_mining_be_t9, cryptocurrency_mining_centralization, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cryptocurrency_mining_centralization, resource_allocation).
narrative_ontology:affects_constraint(cryptocurrency_mining_centralization, blockchain_consensus_security).
narrative_ontology:affects_constraint(cryptocurrency_mining_centralization, cryptocurrency_transaction_censorship_resistance).
narrative_ontology:affects_constraint(cryptocurrency_mining_centralization, asic_manufacturing_concentration).

% DUAL FORMULATION NOTE:
% Mining centralization is downstream of consensus protocol architecture (PoW vs PoS choice) but represents a distinct structural constraint. The upstream constraint (blockchain_consensus_security) determines whether mining-like mechanisms are necessary; mining_centralization then reflects the institutional equilibrium when mining is required. The downstream constraints (censorship_resistance, transaction policy) depend on mining concentration levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
