% ============================================================================
% CONSTRAINT STORY: full_node_resource_burden
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_full_node_resource_burden, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: full_node_resource_burden
 *   human_readable: Full Node Resource Burden in Blockchain Networks
 *   domain: distributed_systems/blockchain_economics
 *
 * SUMMARY:
 *   The full node resource burden represents a structural tension in
 *   blockchain networks between the ideological commitment to
 *   decentralization (requiring distributed verification nodes) and the
 *   practical reality that network state growth and hardware requirements
 *   create barriers to entry that concentrate validation power in organized
 *   operators. The constraint exhibits characteristics of both genuine
 *   coordination (Byzantine fault tolerance requires some verification
 *   redundancy) and extractive layering (economies of scale concentrate
 *   benefits and shift costs to individual operators). The theater ratio
 *   (0.55) reflects the gap between the narrative of 'run a full node to
 *   validate the network' and the structural reality that most nodes perform
 *   mechanical consensus-following rather than independent verification. Over
 *   the measurement interval, extractiveness increased from 0.28 to 0.52,
 *   driven by state growth outpacing hardware affordability improvements,
 *   while theater ratio rose from 0.38 to 0.55 as the functional verification
 *   component degraded relative to performative participation.
 *
 * KEY AGENTS:
 *   - Individual Node Operators: Primary victims (powerless/trapped) — face compounding hardware, bandwidth, and storage costs with no exit path except abandoning participation
 *   - Mining/Staking Pools: Primary beneficiaries and organized coordinators (organized/constrained) — extract from small operators through economies of scale while experiencing the constraint as genuine coordination necessity
 *   - Protocol Maintainers: Secondary beneficiaries (institutional/arbitrage) — benefit from increased dependency on expertise and governance authority; can arbitrage across protocol versions
 *   - Home/Mid-Tier Operators: Secondary victims (moderate/constrained) — experience mixed extraction and coordination; maintain some optionality through light client alternatives but at security/sovereignty cost
 *   - Light Client / Prover Architecture Developers: Organized agents with sunset pathway (organized/mobile) — building alternative participation mechanisms that reduce resource requirements by orders of magnitude
 *   - Blockchain State Growth: Structural driver (non-agent) — historical transactions and smart contract state accumulate; compression is constrained by cryptographic verification requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(full_node_resource_burden, 0.52).
domain_priors:suppression_score(full_node_resource_burden, 0.68).
domain_priors:theater_ratio(full_node_resource_burden, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(full_node_resource_burden, extractiveness, 0.52).
narrative_ontology:constraint_metric(full_node_resource_burden, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(full_node_resource_burden, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(full_node_resource_burden, tangled_rope).
narrative_ontology:human_readable(full_node_resource_burden, "Full Node Resource Burden in Blockchain Networks").
narrative_ontology:topic_domain(full_node_resource_burden, "distributed_systems/blockchain_economics").

domain_priors:requires_active_enforcement(full_node_resource_burden).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(full_node_resource_burden, protocol_maintainers).
narrative_ontology:constraint_beneficiary(full_node_resource_burden, exchange_operators).
narrative_ontology:constraint_beneficiary(full_node_resource_burden, large_stakeholders).
narrative_ontology:constraint_victim(full_node_resource_burden, individual_node_operators).
narrative_ontology:constraint_victim(full_node_resource_burden, network_decentralization).
narrative_ontology:constraint_victim(full_node_resource_burden, accessibility_to_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL NODE OPERATOR (SNARE) — Small operators face escalating hardware, bandwidth, and storage requirements with no exit path short of abandoning network participation entirely. Hardware depreciation, electricity costs, and storage capacity compound. The constraint extracts resource wealth from individuals with no meaningful coordination benefit. Trapped by desire to contribute to network decentralization but structurally unable to afford continued participation as requirements climb.
constraint_indexing:constraint_classification(full_node_resource_burden, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MINING/VALIDATION COALITION (TANGLED ROPE) — Organized operators (mining pools, staking services, institutional validators) experience the constraint as both coordination mechanism and extraction source. The resource burden creates coordination necessity (pooling, delegation, specialization) while simultaneously extracting from smaller operators through economies of scale. These agents benefit from centralization but also depend on protocol legitimacy requiring perceived decentralization.
constraint_indexing:constraint_classification(full_node_resource_burden, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PROTOCOL MAINTAINERS (ROPE) — Benefit from resource burden through increased dependency on their expertise, maintenance services, and protocol governance decisions. Experience the constraint as a coordination mechanism that structures network participation. Can arbitrage between different protocol versions or alternative designs. Net beneficiary from status quo — extraction flows toward this institutional agent.
constraint_indexing:constraint_classification(full_node_resource_burden, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HOME/MID-TIER OPERATOR (TANGLED ROPE) — Faces constrained choices: upgrade hardware at regular intervals (extraction) or lose capacity (exit option costs are high). Also benefits from network security and decentralization benefits (coordination). Some exit optionality exists (switching to light clients, custodial solutions) but at cost of security/sovereignty. Moderate experienced extraction — not powerless, but options are costly.
constraint_indexing:constraint_classification(full_node_resource_burden, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FULL NODE VERIFICATION RITUAL (PITON) — Theatrical compliance: the narrative of 'run a full node to validate the network' persists as institutional performance despite the structural reality that most nodes validate nothing (they follow consensus mechanically). The ritual maintains decentralization theater while actual verification is concentrated in mining/staking pools. Theater ratio high (0.55) because the coordinating function (distributed verification) has largely degraded while the resource burden remains.
constraint_indexing:constraint_classification(full_node_resource_burden, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LIGHT CLIENT/PROVER PATHWAY (SCAFFOLD) — Emerging technologies (light clients, stateless nodes, proof-of-validity systems) offer a sunset mechanism for the full node burden. These architectures reduce resource requirements by 2-3 orders of magnitude while maintaining security guarantees. Organized efforts (Ethereum's Verkle trees, zero-knowledge proofs, data availability layers) are building alternative participation pathways. Suppression will decline as these alternatives mature — estimated 5-10 year sunset for widespread adoption.
constraint_indexing:constraint_classification(full_node_resource_burden, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some verification redundancy is necessary for Byzantine fault tolerance — the Fundamental Trade-off (storage cost vs security against N/2 attacks) appears as a natural law of distributed consensus. This perspective risks naturalizing what is actually a protocol design choice (block size, state pruning, data availability strategy). The engine's false summit detector will identify this as contingent institutional arrangement, not immutable law.
constraint_indexing:constraint_classification(full_node_resource_burden, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(full_node_resource_burden_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(full_node_resource_burden, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(full_node_resource_burden, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(full_node_resource_burden, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(full_node_resource_burden, TR),
    TR >= 0.70.

:- end_tests(full_node_resource_burden_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts resource wealth from individual operators through escalating hardware requirements, creating a time-limited asset (equipment with finite useful life before obsolescence). The extraction is not total because legitimate coordination benefits exist (Byzantine fault tolerance requires some redundancy), but the asymmetric sharing of these benefits (concentrated in pools/institutions) shifts the net balance toward extraction. The trajectory from 0.28 to 0.52 over the measurement interval reflects state growth outpacing hardware affordability, indicating the extraction mechanism is accumulating. Suppression (0.68): High. Barriers to exit are substantial: upgrading hardware is costly, switching to light clients sacrifices security guarantees, and the narrative legitimacy of 'run a full node' creates social pressure. Smaller operators face few middle-ground options. Theater ratio (0.55): Moderate. The functional verification component (checking transactions against consensus rules) is mechanically performed by most nodes without genuine autonomy — they follow the hash chain. But the ritual narration ('you are validating the network') persists, and some nodes do perform independent validation. Theater has increased as the coordination value of individual node participation has declined relative to the resource cost.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates clear polarization: beneficiaries and victims occupy structurally opposite positions. Individual operators see a Snare (extraction with no coordination benefit); organized operators see Tangled Rope (legitimate coordination plus extraction); institutions see Rope (pure coordination). The analytical observer's Mountain is a false summit that naturalizes a policy choice as physical law. The key perspectival gap is between powerless/trapped (Snare) and institutional/arbitrage (Rope) — the same constraint structure produces opposite classifications because exit options and power levels diverge sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position. Individual node operators bear costs (high d: ~0.85-0.95) with no offsetting benefits in their perspective; they experience maximum f(d) → maximum χ. Organized pools benefit from coordination necessity and scale (low d: ~0.20-0.35); they see Rope or low-extraction Tangled Rope. Protocol maintainers benefit institutionally (d: ~0.15); arbitrage exit keeps f(d) low and χ near zero or negative. Mid-tier operators split the difference (d: ~0.60) — they bear some cost but also capture some security benefits. The piton perspective derives from theater gate (high theater_ratio) rather than from experienced extraction magnitude. The scaffold perspective's mobile exit option reduces felt suppression despite the structural barrier remaining.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that the tension between 'this is necessary for decentralization' (supporting Mountain/Rope) and 'this extracts from small operators' (supporting Snare) reflects genuine structural ambiguity, not mislabeling. The Scaffold perspective (light client pathway) dissolves the dilemma: if emergence of security-equivalent alternatives (stateless nodes, zero-knowledge proofs, data availability committees) can reduce requirements below current levels without sacrificing Byzantine fault tolerance, then the current burden is revealed as a contingent institutional arrangement (Tangled Rope + Scaffold sunset), not a natural law or necessary extraction. The mandatrophy is resolved by the omega variables: if light client security parity is achieved (omega 2) and state storage can be compressed (omega 1), the constraint transforms from persistent extraction (Snare) into a temporary coordination failure with a predictable sunset (Scaffold). The false summit (Mountain) is detected by noting that the 'necessary for consensus' framing omits the protocol design choices (block size, state pruning, proof systems) that could shift the burden entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    storage_compression_frontier,
    'Can state storage requirements be compressed below current levels without compromising security guarantees?',
    'Empirical testing of stateless node architectures, Verkle trees, and zero-knowledge proof systems; analysis of security assumptions when full historical state is unavailable',
    'If yes: resource burden is reducible through protocol changes (extraction mechanism strengthens, classification potentially shifts to pure Snare). If no: burden is near-necessary cost (supports Mountain perspective claim). If partially: supports Scaffold sunset thesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(storage_compression_frontier, empirical, 'Feasibility of reducing state storage below current levels').

omega_variable(
    light_client_security_parity,
    'Do light clients with sufficient verification proofs provide cryptographic security parity with full nodes, or do they rely on social consensus (trusting the majority)?',
    'Formal security analysis of light client threat models; comparison of liveness and safety guarantees; empirical measurement of light client vulnerability to consensus forks',
    'If parity: light clients are true alternative (Scaffold sunset is real). If not parity: light clients trade security for convenience (true exit option requires accepting risk), Snare classification strengthened. If unclear: omega remains core uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(light_client_security_parity, empirical, 'Whether light clients achieve security parity with full nodes').

omega_variable(
    decentralization_measurement_validity,
    'Does node count metric capture actual decentralization, or does it measure theater? How many nodes are required to represent genuine distributed consensus vs performative participation?',
    'Network analysis of node autonomy (percentage running outdated code, percentage following hard forks, percentage owned by single entities); correlation between node count and actual consensus diversity',
    'If node count correlates weakly with consensus autonomy: full node resource burden is primarily extractive theater (Snare classification strengthened). If strongly correlated: burden has genuine security function (Mountain or Rope perspectives supported).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_measurement_validity, empirical, 'Whether node count measures decentralization or theater').

omega_variable(
    hardware_cost_trajectory,
    'Will hardware resources (storage, bandwidth, computation) decrease faster than blockchain state growth, or will the burden remain structurally increasing?',
    'Trend analysis of SSD/HDD cost-per-GB, network bandwidth pricing, computational efficiency gains vs actual historical state growth rates across major blockchains',
    'If hardware cheaper faster: burden is temporary (Scaffold sunset is automatic). If state grows faster: burden is structurally increasing (extraction worsens over time, Snare classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_cost_trajectory, empirical, 'Relative trajectory of hardware costs vs state growth').

omega_variable(
    solo_validator_viability_threshold,
    'What is the minimum wealth threshold below which solo staking/validation becomes economically irrational? Is this threshold stable or increasing?',
    'Economic analysis of minimum capital requirements, amortized hardware costs, electricity costs, and forgone yields; historical tracking of minimum efficient scale for solo validators vs pooled validators',
    'If threshold is stable: extraction is bounded (Tangled Rope classification supported). If threshold is increasing: extraction is accumulating over time, classification shifts toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(solo_validator_viability_threshold, empirical, 'Stability of minimum viable solo validator capital').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(full_node_resource_burden, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fnrb_tr_t0, full_node_resource_burden, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fnrb_tr_t4, full_node_resource_burden, theater_ratio, 4, 0.48).
narrative_ontology:measurement(fnrb_tr_t8, full_node_resource_burden, theater_ratio, 8, 0.55).

% Extraction over time
narrative_ontology:measurement(fnrb_be_t0, full_node_resource_burden, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fnrb_be_t4, full_node_resource_burden, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(fnrb_be_t8, full_node_resource_burden, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(full_node_resource_burden, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(full_node_resource_burden, 0.12).
narrative_ontology:affects_constraint(full_node_resource_burden, validator_centralization).
narrative_ontology:affects_constraint(full_node_resource_burden, blockchain_scalability_trilemma).
narrative_ontology:affects_constraint(full_node_resource_burden, mining_pool_consolidation).

% DUAL FORMULATION NOTE:
% The full node resource burden is upstream of validator centralization (resource barriers force delegation to pools) and connected to the scalability trilemma (state growth is a choice variable, not a law). Separate constraint stories model the economic incentives for pool formation and the protocol-level state management choices that drive growth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(full_node_resource_burden, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
