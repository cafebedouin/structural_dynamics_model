% ============================================================================
% CONSTRAINT STORY: proof_of_work_consensus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Proof-of-work consensus is a distributed protocol that replaces trusted
 *   third parties with computational proof, enabling Byzantine-fault-tolerant
 *   agreement without a central gatekeeper. The mechanism works by requiring
 *   participants (miners) to invest computational work (hash power) to
 *   validate transactions, with difficulty adjusted so that new blocks appear
 *   at regular intervals regardless of total network hash power. This creates
 *   an economic incentive structure where honest block production is
 *   profitable and attack attempts are costly. However, the constraint
 *   exhibits a structural tension: while the mechanism genuinely solves the
 *   coordination problem of decentralized consensus (no single trusted
 *   intermediary needed), it also creates asymmetric extraction pathways.
 *   Industrial mining operations with economies of scale capture
 *   disproportionate rewards; energy costs are partially externalized to the
 *   electricity ecosystem; and network governance authority concentrates in
 *   major mining pools and core developers despite the peer-to-peer
 *   mythology. The extractiveness has grown over time as mining has
 *   industrialized, difficulty has increased exponentially, and mining pools
 *   have consolidated hash power. Theater ratio has also risen as the
 *   'decentralization' narrative persists despite observable centralization
 *   in practice.
 *
 * KEY AGENTS:
 *   - Mining Pool Operators: Primary beneficiaries (institutional/arbitrage) — capture transaction fees and coordinate smaller miners, with full exit flexibility
 *   - ASIC Manufacturers: Secondary beneficiaries (institutional/arbitrage) — capture value through hardware sales and obsolescence cycles
 *   - Industrial Mining Operations: Tertiary beneficiaries (organized/arbitrage) — benefit from economies of scale and electricity arbitrage across geographies
 *   - Individual Miners: Mixed (moderate/constrained) — benefit from block rewards but face extraction through difficulty spirals and consolidation pressure
 *   - Non-Mining Network Participants: Primary victims (powerless/trapped) — bear security costs via inflation and transaction fees without consensus agency
 *   - Energy Ecosystem: Secondary victim (moderate/constrained) — bears externalized electricity demand and thermal waste; has constrained exit in competitive markets
 *   - Decentralization Narrative: Institutional actor (institutional/constrained) — maintains peer-to-peer mythology despite observable pool centralization; constrained because persistent contradictions erode legitimacy
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — sees both genuine coordination function and emergent extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proof_of_work_consensus, 0.58).
domain_priors:suppression_score(proof_of_work_consensus, 0.62).
domain_priors:theater_ratio(proof_of_work_consensus, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proof_of_work_consensus, extractiveness, 0.58).
narrative_ontology:constraint_metric(proof_of_work_consensus, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(proof_of_work_consensus, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proof_of_work_consensus, tangled_rope).
narrative_ontology:human_readable(proof_of_work_consensus, "Hash-Based Proof-of-Work Consensus").
narrative_ontology:topic_domain(proof_of_work_consensus, "technological/economic").

domain_priors:requires_active_enforcement(proof_of_work_consensus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proof_of_work_consensus, mining_operators).
narrative_ontology:constraint_beneficiary(proof_of_work_consensus, early_adopters).
narrative_ontology:constraint_beneficiary(proof_of_work_consensus, hardware_manufacturers).
narrative_ontology:constraint_victim(proof_of_work_consensus, non_mining_participants).
narrative_ontology:constraint_victim(proof_of_work_consensus, energy_ecosystem).
narrative_ontology:constraint_victim(proof_of_work_consensus, network_decentralization_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-MINING PARTICIPANT (SNARE) — Users holding cryptocurrency cannot exit the consensus mechanism without forfeiting their holdings. They bear the full cost of network security (via inflation/transaction fees) without agency in rule changes. Mining centralization means a tiny cohort of industrial operators controls transaction validation. No alternatives for consensus participation; maximum suppression.
constraint_indexing:constraint_classification(proof_of_work_consensus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL MINER (TANGLED ROPE) — Benefits from the system through block rewards and transaction fees, creating genuine coordination incentive (everyone secures the network via mining). But faces extraction through hardware obsolescence, electricity cost escalation, and solo-mining infeasibility driving consolidation. Constrained exit — can cease mining but forgoes income; cannot escape rising difficulty curves. Both coordination function (securing consensus) and asymmetric extraction (difficulty spiral, consolidation pressure) present.
constraint_indexing:constraint_classification(proof_of_work_consensus, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MINING POOL OPERATOR (ROPE) — Coordinates individual miners into collective hash power; solves the variance problem and captures transaction fees. Arbitrage exit: can migrate to more profitable coins or shift to other consensus mechanisms. Experiences the constraint as pure coordination — the mining pool is a solution to collective action problems created by the base PoW mechanism. Net beneficiary.
constraint_indexing:constraint_classification(proof_of_work_consensus, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ASIC MANUFACTURER (ROPE) — Designs hardware optimized for proof-of-work. Captures value through equipment sales and obsolescence cycles. Arbitrage exit: design for other coins or consensus algorithms. Experiences PoW as pure coordination — it creates reliable demand for specialized hardware. Net beneficiary with full flexibility.
constraint_indexing:constraint_classification(proof_of_work_consensus, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENERGY ECOSYSTEM (SNARE) — Bears the externalized cost of proof-of-work security: electricity consumption, thermal waste, grid strain, stranded electricity infrastructure. Has constrained exit — cannot refuse to provide power to mining operations in competitive markets. No seat at the consensus table despite bearing sustainability costs. Suppression is near-total: energy providers cannot opt out of the transaction cost.
constraint_indexing:constraint_classification(proof_of_work_consensus, snare,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DECENTRALIZATION NARRATIVE (PITON) — The original claim was that PoW replaces trusted intermediaries with algorithmic consensus, enabling peer-to-peer value transfer without centralized gatekeepers. This narrative persists rhetorically despite growing empirical falsification: hash power concentrates in industrial mining pools (Foundry USA, AntPool, Binance Pool control ~50% of Bitcoin hash rate), geographic distribution follows electricity costs (Iceland, El Salvador, China historically, then Texas), and network governance is captured by a small group of core developers and major mining operations. The 'decentralization' function is largely performative; the mechanism enables that performance but requires ongoing theater (peer-to-peer nodes, distributed ledger narratives) to maintain legitimacy despite observable concentration.
constraint_indexing:constraint_classification(proof_of_work_consensus, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational frame, PoW consensus exhibits genuine coordination function (solves Byzantine consensus without trusted third parties) AND asymmetric extraction (energy externalities, mining centralization, consensus capture). The mechanism delivers on its coordination promise (no single gatekeeper needed) but layers extraction mechanisms on top (difficulty spiral favors capital-rich operators, energy costs externalized, network rules captured by developer/miner cartel). Active enforcement required: maintaining the consensus mechanism requires continuous hash power investment and difficulty adjustment. Not a false summit (not a natural law) but a genuine hybrid that both enables decentralized coordination AND enables asymmetric value capture.
constraint_indexing:constraint_classification(proof_of_work_consensus, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.58): Moderate-high and rising. The base PoW mechanism solves genuine coordination problems (Byzantine consensus without trusted authority), but mining centralization, energy externalities, and difficulty-driven consolidation create asymmetric value capture. Initial extractiveness (t=0) was lower (0.15) because early mining was distributed and hardware-cheap; it has grown to 0.58 as industrial operations dominate. The mechanism inherently creates winners and losers: capital-rich operators mine profitably while small operators face marginal costs exceeding rewards. Suppression (0.62): Moderate-high. Non-mining participants cannot exit the consensus mechanism without losing their cryptocurrency holdings. Energy providers cannot refuse service to mining operations. Miners face technological suppression (ASIC specificity, difficulty adjustment, pool-centralized variance reduction). Theater ratio (0.48): Moderate. The 'decentralization' function is substantially performative — the narrative emphasizes peer-to-peer nodes and distributed consensus, but observable reality shows pool concentration, geography following electricity costs, and governance capture by core developers and major mining operations. However, theater ratio is not extreme because the underlying consensus mechanism does genuinely work; the performance gap is between the decentralization claim and centralization reality, not between the claim and complete nonfunctionality.
 *
 * PERSPECTIVAL GAP:
 *   The Rope perspectives (mining pool operators, ASIC manufacturers) classify the constraint as pure coordination because they benefit without bearing costs. The Snare perspective (non-mining participants, energy ecosystem) sees pure extraction because they bear costs without benefits and have no exit. The Tangled Rope perspectives (individual miners, analytical observer) see both coordination and extraction because they occupy positions where both flows are observable. The Piton perspective (decentralization narrative) sees a degraded mechanism maintained through rhetoric. This gap is not observational ambiguity but structural reality: different agents genuinely experience different constraint types because the constraint's structure creates asymmetric benefit and cost flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural relationship: beneficiary vs victim, power level, and exit options. Mining pool operators have low d (0.05–0.15) because they are primary beneficiaries with arbitrage exits; they experience the constraint as beneficial coordination. Non-mining participants have high d (0.90–0.95) because they are trapped victims with no exit; they experience maximum extraction. Individual miners have moderate d (0.55–0.65) because they are both beneficiaries (via block rewards) and victims (via difficulty spirals and hardware costs), and they have constrained but non-zero exits. The energy ecosystem has high d (0.80–0.90) because it bears externalized costs with constrained exit; electricity markets are competitive and cannot refuse service to profitable mining operations. The decentralization narrative has moderate d (0.50–0.60) because it benefits from the mechanism's existence (it provides legitimate-sounding justification) but is victimized by its observable failure to deliver on the promise (concentration is visible and eroding narrative credibility). The analytical observer has moderate d (0.70–0.75) because observation involves seeing both the successful coordination function and the successful extraction mechanisms; neither is hidden from the civilizational perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that Tangled Rope is the structural truth and Rope/Snare are perspectival artifacts of position-dependent experience. If we mistakenly unified the constraint as 'pure Rope' (coordination mechanism), we would naturalize mining centralization and energy externalities as incidental rather than structural. If we unified it as 'pure Snare' (extraction mechanism), we would deny the genuine coordination function that prevents single-point failure in network consensus. Tangled Rope captures that BOTH are real: the mechanism genuinely enables decentralized Byzantine consensus (coordination function, non-negotiable) AND it genuinely concentrates mining hash power and externalizes energy costs (extraction function, emergent from economic incentives). The decentralization narrative is Piton — it performs the coordination function symbolically to maintain legitimacy despite observable centralization. The mandatrophy resolution reveals that PoW is neither inherently decentralized nor inherently centralizing; rather, it solves one coordination problem (Byzantine consensus) while creating new extraction mechanisms (mining consolidation, energy externality). The classification that captures this complexity is Tangled Rope, not a false consensus on pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mining_centralization_threshold,
    'At what point does mining pool concentration constitute consensus capture, and does current concentration exceed this threshold?',
    'Empirical analysis of hash power distribution (Nakamoto coefficient), correlation between pool operator coordination and rule changes, transaction censorship incidence tracking',
    'If threshold < 50% (current level): PoW consensus is captured, reclassifies to pure Snare from analytical perspective. If threshold > 60%: current system may still preserve consensus guarantees, supports Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mining_centralization_threshold, empirical, 'Mining centralization threshold for consensus capture').

omega_variable(
    energy_externalizable_fraction,
    'What fraction of PoW energy consumption is genuine security cost vs. externalizable waste (thermodynamic inefficiency, geographic stranding)?',
    'Thermodynamic analysis of hash rate per joule; comparison of optimal vs actual mining efficiency; correlation between regional electricity surplus and mining facility location',
    'If externalizable fraction > 60%: energy cost is structural extraction mechanism, not coordination cost. Reclassifies extraction from 0.58 to 0.72+. If < 40%: most energy use is security function, validates current Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_externalizable_fraction, empirical, 'Fraction of PoW energy that is externalizable waste').

omega_variable(
    consensus_dependency_on_pool_honesty,
    'How dependent is the consensus mechanism on the assumption that mining pools remain rational and honest? Can a malicious pool operator execute a 51% attack or soft-fork without detection?',
    'Game-theoretic analysis of mining pool incentives; simulation of attack scenarios; historical analysis of actual pool behavior (transaction censorship, mempool manipulation)',
    'If dependency is high: consensus is only as strong as the weakest major pool''s governance. If low: distributed validation and node consensus provide sufficient backstop.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_dependency_on_pool_honesty, empirical, 'Consensus dependency on mining pool honesty').

omega_variable(
    alternative_consensus_availability,
    'Do practical alternatives to PoW (proof-of-stake, proof-of-authority) achieve comparable Byzantine consensus guarantees with lower extraction or externality costs?',
    'Comparative analysis of consensus mechanisms: security guarantees, centralization risk, energy efficiency, complexity, governance capture vectors',
    'If alternatives are strictly superior: PoW classification shifts toward Piton (maintained through vendor lock-in and sunk costs, not functional necessity). If alternatives have trade-offs: PoW remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_consensus_availability, conceptual, 'Whether alternatives to PoW are strictly superior').

omega_variable(
    temporal_sustainability_of_extraction_model,
    'Can the mining extraction model (exponential difficulty growth, hardware obsolescence, energy cost escalation) sustain indefinitely, or does it face fundamental limits?',
    'Long-term modeling of mining profitability under increasing difficulty and energy costs; analysis of hardware refresh cycles and ASIC obsolescence rates; thermodynamic limits on heat dissipation',
    'If unsustainable: PoW transitions toward Piton (theater persists despite declining function). If sustainable: current Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_sustainability_of_extraction_model, empirical, 'Long-term sustainability of the PoW mining extraction model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proof_of_work_consensus, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pow_tr_t0, proof_of_work_consensus, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pow_tr_t5, proof_of_work_consensus, theater_ratio, 5, 0.38).
narrative_ontology:measurement(pow_tr_t10, proof_of_work_consensus, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(pow_be_t0, proof_of_work_consensus, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(pow_be_t5, proof_of_work_consensus, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(pow_be_t10, proof_of_work_consensus, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proof_of_work_consensus, enforcement_mechanism).
narrative_ontology:affects_constraint(proof_of_work_consensus, cryptocurrency_mining_centralization).
narrative_ontology:affects_constraint(proof_of_work_consensus, blockchain_energy_externality).
narrative_ontology:affects_constraint(proof_of_work_consensus, consensus_governance_capture).

% DUAL FORMULATION NOTE:
% PoW consensus decomposes into three structurally distinct constraints: (1) Byzantine consensus problem solved by PoW (ε≈0.05, Mountain from cryptographic perspective), (2) mining centralization as emergent economic phenomenon (ε≈0.65, Snare from miner-victim perspective), (3) energy externalization as systemic cost (ε≈0.70, Snare from energy-ecosystem perspective). These are not observational variants but distinct structural claims linked by causality: the consensus mechanism's design incentives cause mining consolidation, which causes energy cost concentration. This story addresses the unified PoW constraint (ε=0.58); the network edges link to decomposed sub-constraints for deeper analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(proof_of_work_consensus, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
