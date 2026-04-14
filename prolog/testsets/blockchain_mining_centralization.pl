% ============================================================================
% CONSTRAINT STORY: blockchain_mining_centralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blockchain_mining_centralization, []).

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
 *   constraint_id: blockchain_mining_centralization
 *   human_readable: Blockchain Mining Centralization
 *   domain: cryptocurrency/distributed_systems/economic_incentives
 *
 * SUMMARY:
 *   Blockchain mining centralization represents a structural tension between
 *   the proof-of-work consensus mechanism's theoretical requirement for
 *   distributed participation and the economic incentives that converge
 *   participation toward large-scale, capital-intensive operations. The
 *   constraint exhibits all six DR types from different perspectives: solo
 *   miners experience pure extraction (Snare), small operations experience
 *   mixed coordination-extraction (Tangled Rope), large pools coordinate
 *   efficiently (Rope), developers coordinate protocol upgrades under
 *   extraction pressure (Tangled Rope), ASIC manufacturers operate with
 *   mobile exit (Tangled Rope), decentralization advocates maintain
 *   performative commitment to distributed ideals (Piton), and analytical
 *   observers risk naturalizing contingent economic structures as immutable
 *   law (Mountain). The extractiveness trajectory (0.28 → 0.58 over 10 years)
 *   reflects accelerating hardware specialization and economies of scale.
 *   Theater ratio (0.32 → 0.48) reflects increasing performative commitment
 *   to decentralization despite measured empirical centralization in hash
 *   distribution. The constraint is fundamentally about who captures the
 *   economic rents from the shared verification work that secures the
 *   protocol.
 *
 * KEY AGENTS:
 *   - Solo Miners: Primary victim (powerless/trapped) — face continuous equipment obsolescence and electricity cost barriers; economic exit is forced consolidation or abandonment
 *   - Small Mining Operations: Secondary victim (moderate/constrained) — achieve some scale but constrained by capital access and electricity procurement; in precarious position between commoditization and consolidation
 *   - Large Mining Pool Operators: Primary beneficiary (institutional/arbitrage) — capture network effects and operate across jurisdictions for electricity arbitrage; coordinate hashing power and distribute variance
 *   - ASIC Manufacturers: Secondary beneficiary (powerful/mobile) — design specialized hardware; capture rents from first-mover advantage and obsolescence cycles; coordinate with pools on firmware optimization
 *   - Protocol Developers: Organized coordinator (organized/constrained) — attempt to maintain decentralization through algorithm and difficulty adjustment changes; constrained by backwards compatibility and consensus requirements; extraction pressure from miners over upgrade signaling
 *   - Electricity Providers in Low-Cost Regions: Incidental beneficiary (institutional/arbitrage) — benefit from mining farm clustering; coordinate infrastructure provisioning
 *   - Decentralization Movement: Organized advocate (organized/constrained) — maintain ideological commitment to distributed mining; constrained by historical narrative; see own framing as degraded (Piton perspective)
 *   - Network Security (Abstract): Primary victim (powerless/trapped) — centralized mining increases 51% attack risk; cannot organize or represent its own interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blockchain_mining_centralization, 0.58).
domain_priors:suppression_score(blockchain_mining_centralization, 0.65).
domain_priors:theater_ratio(blockchain_mining_centralization, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blockchain_mining_centralization, extractiveness, 0.58).
narrative_ontology:constraint_metric(blockchain_mining_centralization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(blockchain_mining_centralization, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blockchain_mining_centralization, tangled_rope).
narrative_ontology:human_readable(blockchain_mining_centralization, "Blockchain Mining Centralization").
narrative_ontology:topic_domain(blockchain_mining_centralization, "cryptocurrency/distributed_systems/economic_incentives").

domain_priors:requires_active_enforcement(blockchain_mining_centralization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blockchain_mining_centralization, large_mining_pools).
narrative_ontology:constraint_beneficiary(blockchain_mining_centralization, asic_manufacturers).
narrative_ontology:constraint_beneficiary(blockchain_mining_centralization, electricity_providers_in_low_cost_regions).
narrative_ontology:constraint_victim(blockchain_mining_centralization, small_miners).
narrative_ontology:constraint_victim(blockchain_mining_centralization, network_decentralization).
narrative_ontology:constraint_victim(blockchain_mining_centralization, energy_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL MINER (SNARE) — Solo miners face escalating hardware costs and electricity requirements that make independent operation economically unviable. Joining mining pools extracts a 1-4% fee while eliminating independence. Exit means abandoning mining entirely. Maximum extraction experienced by individual actors without coordination benefit.
constraint_indexing:constraint_classification(blockchain_mining_centralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL MINING OPERATION (TANGLED ROPE) — Regional operations coordinate with other miners through pools and achieve some economies of scale. Constrained by hardware sourcing bottlenecks and electricity cost variation. Experiences both coordination benefits (shared infrastructure, reduced variance) and extraction (pool fees, difficulty adjustment punishing small operations).
constraint_indexing:constraint_classification(blockchain_mining_centralization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE MINING POOL OPERATOR (ROPE) — Benefits from network effects and arbitrage opportunities. Sees centralization as coordination of hashing power and variance reduction. Operates across jurisdictions to optimize electricity costs. Net beneficiary — extraction flows toward this actor.
constraint_indexing:constraint_classification(blockchain_mining_centralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL DEVELOPMENT COMMUNITY (TANGLED ROPE) — Organized actors (core developers, research communities) coordinate on protocol upgrades to mitigate centralization (difficulty adjustments, mining algorithm changes). Constrained by backwards compatibility and political consensus requirements. Genuine coordination function (maintaining protocol security) alongside extraction pressure (mining pool control of upgrade signaling).
constraint_indexing:constraint_classification(blockchain_mining_centralization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ASIC MANUFACTURER (TANGLED ROPE) — Powerful actor (institutional capability) with mobile exit options (can pivot to other chip markets). Benefits from Moore's law-driven hardware obsolescence cycle. Coordinates with mining pools through hardware specifications and firmware optimization. Experiences mixed extraction and coordination — drives commoditization of mining while accumulating rents from first-mover chip advantages.
constraint_indexing:constraint_classification(blockchain_mining_centralization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DECENTRALIZATION ADVOCATES (PITON) — Organized movement maintaining narrative that mining decentralization is inherent to blockchain design. Theater ratio high: proof-of-work decentralization rhetoric persists despite clear empirical centralization trend. Advocates see their own framing as degraded (performative commitment to decentralization despite accepting centralized pools). Exit constrained by ideological commitment to blockchain's founding narrative.
constraint_indexing:constraint_classification(blockchain_mining_centralization, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a computational complexity perspective, mining centralization may appear as natural law: economies of scale, fixed costs, and hardware optimization create unavoidable convergence toward large-scale operations. This perspective risks false summit: treating contingent economic incentive structures as immutable physical limits.
constraint_indexing:constraint_classification(blockchain_mining_centralization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blockchain_mining_centralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blockchain_mining_centralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blockchain_mining_centralization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blockchain_mining_centralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blockchain_mining_centralization, TR),
    TR >= 0.70.

:- end_tests(blockchain_mining_centralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Solo and small miners face escalating barriers to profitable independent operation. Hardware costs grow exponentially; electricity costs provide a structural moat; pool participation extracts 1-4% fees. The trajectory from 0.28 to 0.58 reflects Moore's law driving ASIC specialization faster than difficulty adjustment can accommodate individual miners. Suppression (0.65): High. Individual miners face multiple suppression mechanisms: hardware obsolescence (replaced within 1-2 years), electricity cost heterogeneity (not addressable by individual operator), pool cartelization risk (1-4% fee lock-in with limited exit), and geographic permissioning (some jurisdictions hostile to mining). However, suppression is not total — solo mining remains technically possible, and some geographic arbitrage exists. Theater ratio (0.48): Moderate. The 'decentralization' narrative for proof-of-work systems persists despite clear empirical centralization trend. The narrative emphasizes 'anyone can mine' while obscuring that profitability has converged to large-scale operations. Theater has increased over the measurement interval as the gap between rhetoric and observed Nakamoto coefficient has widened.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between solo miners and large pools is maximum in this constraint. Solo miners see an insurmountable economic barrier (Snare — pure extraction with no coordination benefit to them). Large pool operators see a coordination solution (Rope — they are solving the variance reduction and capital efficiency problem). The decentralization advocates see performative commitment to an ideal that empirical reality contradicts (Piton — the ritual persists, the function has atrophied). The protocol developers see genuine coordination work under extraction pressure (Tangled Rope — they coordinate security upgrades while mining pools signal veto over protocol changes). The analytical observer risks collapsing this multiplicity by declaring centralization a 'law' of distributed systems (Mountain — false summit), when it is actually a contingent outcome of economic incentive structures that could be redesigned. The perspectival gap reveals that mining centralization is neither immutable nor purely extractive — it is a mixed mechanism that solves some coordination problems while creating new extraction mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position relative to extraction flow. Solo miners (powerless/trapped): d ≈ 0.95 (near-total victims, no exit capacity). Small operations (moderate/constrained): d ≈ 0.60 (significant victims, but some organizational capacity). Large pools (institutional/arbitrage): d ≈ 0.10 (beneficiaries with exit options). Pool operators have arbitrage options (relocate operations, pivot to other verification systems); solo miners face extracted rents with no recovery path. Protocol developers (organized/constrained) experience d ≈ 0.50 (symmetric position — both benefit from mining's security provision and bear cost of centralization pressure on governance). ASIC manufacturers have mobile exit (d ≈ 0.25 as powerful actors with hedge options into other chip markets). The constraint's effective extractiveness χ is scaled by network power asymmetry (f(d)) and global scope (σ(global) = 1.2), amplifying the extraction visible at large spatial scales.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through multi-perspectival decomposition. The mandatrophy question is: 'Is mining centralization a natural law of proof-of-work, or is it an extractive mechanism?' The answer is: neither univocally — it appears as natural law from the analytical context (Mountain), as pure extraction from the powerless solo miner's context (Snare), as coordination solution from the pool operator's context (Rope), and as mixed mechanism from the developer's and moderate operator's contexts (Tangled Rope). No single type is 'correct.' The constraint's actual structure is a presheaf of types over the observation positions. The false summit risk lies in naturalizing what is actually a reversible economic outcome: if electricity cost heterogeneity disappeared (renewable energy ubiquity), if ASIC resistance were maintained (algorithm updates), or if mining rewards shifted toward other verification mechanisms, the centralization pressure would ease and perspectives would shift toward Rope or Scaffold. The mandatrophy resolves by recognizing that mining centralization is structure-dependent, not nature-dependent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pool_cartelization_mechanism,
    'Are large mining pools operating as a cartel restricting hash distribution, or does their centralization reflect genuine economies of scale that no solo operator can overcome?',
    'Analysis of pool fee structures over time; comparison of solo mining profitability curves before and after pool emergence; statistical test for artificial fee maintenance above competitive equilibrium',
    'If cartel: classification shifts toward Snare for powerless agents (extraction mechanism is legal coordination mimicking competition). If genuine economy of scale: classification remains Tangled Rope (extraction is incidental to coordination efficiency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pool_cartelization_mechanism, empirical, 'Whether mining pool centralization is cartelization or genuine economy of scale').

omega_variable(
    algorithm_switching_feasibility,
    'Can proof-of-work algorithms be modified to eliminate ASIC advantages and restore commodity hardware mining, or does ASIC resistance inevitably create new forms of centralization?',
    'Examination of past algorithm change attempts (Monero XMR-V8, etc.); measurement of ASICization timeline post-algorithm change; analysis of capital requirements for GPU vs ASIC mining at different block reward levels',
    'If feasible: decentralization is reversible, constraint is Scaffold with sunset (technical solutions available). If infeasible: centralization is structural, constraint hardens toward Snare (no exit path for solo miners).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_switching_feasibility, empirical, 'Whether ASIC resistance is technically achievable long-term').

omega_variable(
    geographic_electricity_arbitrage_irreversibility,
    'Does the geographic concentration of mining toward low-electricity-cost regions (Iceland, El Salvador, crypto-friendly jurisdictions) represent temporary arbitrage or structural capture of mining by capital flows?',
    'Mapping of mining farm locations and electricity costs over 10-year period; correlation of hash distribution with electricity price maps; analysis of whether electricity-rich but politically unfriendly regions (nuclear-heavy jurisdictions without crypto support) remain unmined',
    'If temporary arbitrage: mining can re-distribute if electricity costs shift. If structural capture: centralization reflects permanent capital lock-in and regulatory permissioning, hardening toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_electricity_arbitrage_irreversibility, empirical, 'Whether geographic mining concentration is reversible arbitrage or structural capital lock-in').

omega_variable(
    protocol_upgrade_veto_power,
    'Do large mining pools exercise de facto veto power over protocol upgrades (via signaling mechanisms), or do developers retain effective control through social consensus?',
    'Historical analysis of protocol upgrades (Bitcoin SegWit2x, block size wars); measurement of correlation between miner signaling and final protocol outcome; identification of cases where miner preference was overridden by community consensus',
    'If miners have veto: extraction includes governance capture, constraint shifts toward Snare (powerless users cannot change rules). If developers retain control: extraction is economic only, constraint remains Tangled Rope (coordination still functions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protocol_upgrade_veto_power, empirical, 'Whether mining pools hold de facto veto power over protocol governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blockchain_mining_centralization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bmc_tr_t0, blockchain_mining_centralization, theater_ratio, 0, 0.32).
narrative_ontology:measurement(bmc_tr_t3, blockchain_mining_centralization, theater_ratio, 3, 0.38).
narrative_ontology:measurement(bmc_tr_t6, blockchain_mining_centralization, theater_ratio, 6, 0.44).
narrative_ontology:measurement(bmc_tr_t10, blockchain_mining_centralization, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(bmc_be_t0, blockchain_mining_centralization, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bmc_be_t3, blockchain_mining_centralization, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(bmc_be_t6, blockchain_mining_centralization, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(bmc_be_t10, blockchain_mining_centralization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blockchain_mining_centralization, resource_allocation).
narrative_ontology:affects_constraint(blockchain_mining_centralization, blockchain_51_percent_attack_risk).
narrative_ontology:affects_constraint(blockchain_mining_centralization, cryptocurrency_energy_consumption).
narrative_ontology:affects_constraint(blockchain_mining_centralization, asic_manufacturing_concentration).
narrative_ontology:affects_constraint(blockchain_mining_centralization, proof_of_work_governance_veto).

% DUAL FORMULATION NOTE:
% Mining centralization decomposes into multiple structurally distinct constraints: (1) individual miner economic viability (ε≈0.72, Snare) — solo mining is unviable; (2) mining pool fee extraction (ε≈0.48, Tangled Rope) — pools coordinate variance but extract rents; (3) ASIC manufacturer rent capture (ε≈0.55, Tangled Rope) — hardware specialization drives obsolescence cycles; (4) protocol governance capture (ε≈0.62, Snare) — large pools can veto protocol upgrades. Each story has its own beneficiary/victim structure and measurement trajectory. This story treats the aggregate constraint at the protocol level; decomposed stories address specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(blockchain_mining_centralization, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
