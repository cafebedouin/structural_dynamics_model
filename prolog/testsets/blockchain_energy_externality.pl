% ============================================================================
% CONSTRAINT STORY: blockchain_energy_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blockchain_energy_externality, []).

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
 *   constraint_id: blockchain_energy_externality
 *   human_readable: Blockchain Energy Externality Constraint
 *   domain: environmental/economic/technology
 *
 * SUMMARY:
 *   Blockchain energy consumption creates a structural constraint where
 *   decentralized consensus mechanisms require energy expenditure that
 *   generates negative externalities (carbon emissions, grid stress,
 *   opportunity cost of renewable energy deployment) borne by agents outside
 *   the blockchain system, while benefits accrue to miners, token holders,
 *   and exchange operators. The constraint exhibits both genuine coordination
 *   function (proof-of-work does solve the Byzantine fault tolerance problem)
 *   and asymmetric extraction (energy costs are socialized while coordination
 *   benefits are privatized). The core tension: whether the energy cost is an
 *   immutable property of distributed consensus (natural law, mountain
 *   perspective) or a contingent engineering choice (proof-of-stake
 *   alternatives exist at orders-of-magnitude lower energy cost, tangled rope
 *   / snare perspective). Theater ratio has increased over the measurement
 *   interval as 'green crypto' narratives become more prevalent despite
 *   fossil fuel mining dominance, indicating growing gap between performative
 *   legitimacy claims and functional reality.
 *
 * KEY AGENTS:
 *   - Blockchain Miners: Primary beneficiaries (institutional/arbitrage) — capture transaction fees and block rewards with low-cost energy arbitrage; highly mobile across jurisdictions
 *   - Cryptocurrency Holders: Secondary beneficiaries (powerful/mobile) — accrue value from network activity; can exit via diversification
 *   - Climate System: Primary victim (powerless/trapped) — cannot exit; bears full cost of carbon emissions; no organizational capacity
 *   - Energy Grid Operators: Secondary victim (moderate/constrained) — face load management challenges, infrastructure strain, cost of peak demand management
 *   - Electricity Consumers: Tertiary victim (moderate/constrained) — face price impacts during high-mining periods; constrained by grid infrastructure dependency
 *   - Renewable Energy Advocates: Organized agents (organized/mobile) — see technology transition (PoS, DAGs) as exit pathway with generational time horizon
 *   - Proof-of-Work Protocol: Institutional constraint carrier (institutional/constrained) — legitimacy narrative maintained through inertia and first-mover advantage despite technical alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blockchain_energy_externality, 0.58).
domain_priors:suppression_score(blockchain_energy_externality, 0.65).
domain_priors:theater_ratio(blockchain_energy_externality, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blockchain_energy_externality, extractiveness, 0.58).
narrative_ontology:constraint_metric(blockchain_energy_externality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(blockchain_energy_externality, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blockchain_energy_externality, tangled_rope).
narrative_ontology:human_readable(blockchain_energy_externality, "Blockchain Energy Externality Constraint").
narrative_ontology:topic_domain(blockchain_energy_externality, "environmental/economic/technology").

domain_priors:requires_active_enforcement(blockchain_energy_externality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blockchain_energy_externality, blockchain_miners).
narrative_ontology:constraint_beneficiary(blockchain_energy_externality, cryptocurrency_holders).
narrative_ontology:constraint_beneficiary(blockchain_energy_externality, exchange_operators).
narrative_ontology:constraint_victim(blockchain_energy_externality, climate_system).
narrative_ontology:constraint_victim(blockchain_energy_externality, electricity_consumers).
narrative_ontology:constraint_victim(blockchain_energy_externality, energy_grid_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE AND ENERGY INFRASTRUCTURE (SNARE) — Cannot exit the system; bears the full cost of blockchain's energy demand. Climate operates at planetary scale with no alternatives; energy grid operators face load management costs from volatile mining demand spikes. No exit option, high suppression, maximum experienced extraction.
constraint_indexing:constraint_classification(blockchain_energy_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ELECTRICITY CONSUMERS (TANGLED ROPE) — Constrained by infrastructure dependency but benefit from broader electricity markets and distributed ledger benefits (if any). Face higher electricity costs during high-mining periods, cannot easily switch suppliers in many jurisdictions. Some coordination benefit from decentralized infrastructure, but extraction through price increases is significant.
constraint_indexing:constraint_classification(blockchain_energy_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MINERS AND OPERATORS (ROPE) — Primary beneficiaries with arbitrage options (can relocate to lower-cost energy jurisdictions, switch currencies, adapt hardware). Experience the constraint as pure coordination: validating transactions enables the system to function. Net beneficiaries — extraction runs toward this agent.
constraint_indexing:constraint_classification(blockchain_energy_externality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RENEWABLE ENERGY ADVOCATES (SCAFFOLD) — Organized agents (grid operators, renewable energy providers, policy advocates) view blockchain energy demand as a temporary coordination failure with sunset potential: renewable energy scaling and proof-of-stake protocols reduce mining energy intensity. Exit path exists through technology transition. Theater ratio moderate — the 'green crypto' narrative is partly performative, but genuine technical pathways (PoS migration, renewable mining) exist.
constraint_indexing:constraint_classification(blockchain_energy_externality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POW LEGITIMACY MAINTENANCE (PITON) — The proof-of-work consensus mechanism persists despite technical alternatives (proof-of-stake, directed acyclic graphs) that achieve similar coordination with orders-of-magnitude lower energy cost. The maintenance of PoW rhetoric ('immutability requires energy sacrifice,' 'energy expenditure is feature not bug') is substantially performative — institutional inertia and first-mover advantage preserve PoW networks despite known technical degradation. Theater ratio high as legitimacy narrative outpaces functional justification.
constraint_indexing:constraint_classification(blockchain_energy_externality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / NATURAL LAW (MOUNTAIN) — From a thermodynamic universalist view, distributed consensus always requires energy expenditure to prevent Sybil attacks; the energy cost is an immutable property of decentralized validation. However, structural data reveals this as naturalization of an engineering choice (PoW mechanism design) rather than a law of physics. Proof-of-stake achieves similar security with proportional energy, not exponential. The false summit detector will flag this perspective.
constraint_indexing:constraint_classification(blockchain_energy_externality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blockchain_energy_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blockchain_energy_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blockchain_energy_externality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blockchain_energy_externality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blockchain_energy_externality, TR),
    TR >= 0.70.

:- end_tests(blockchain_energy_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The blockchain system captures significant energy resources (~120-140 TWh annually for PoW networks as of 2024) that could be deployed for grid services or other productive uses. However, extractiveness is not maximal (0.70+) because: (1) some genuine coordination benefit exists (decentralized consensus does solve real problems), (2) mining does produce work (validating transactions), and (3) alternative pathways exist (PoS, renewable scaling). Suppression (0.65): High. Energy externalities are difficult to price at point-of-use; miners have jurisdictional arbitrage options to evade carbon pricing; climate victims have no mechanisms to resist or exit. Barriers to alternative consensus mechanisms are high due to network effects and sunk infrastructure. Theater ratio (0.55): Moderate. The 'green crypto' narrative is increasing (environmental claims, renewable mining partnerships) but represents only ~15-20% of actual mining activity. The gap between narrative and reality is growing, hence the rising theater_ratio over the interval. Claimed type Tangled Rope justified by: beneficiaries clearly declared (miners, holders, operators); victims clearly declared (climate, grid operators, electricity consumers); genuine coordination function exists (PoW does solve Byzantine fault tolerance); asymmetric extraction exists (energy costs externalized); active enforcement required (mining difficulty adjustment, network governance). Extractiveness > 0.46 triggers measurement and omega requirements, both satisfied.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Miners see Rope (pure coordination — the system works, they are solving the validation problem). Climate and grid victims see Snare (pure extraction — they bear costs with no benefit). Renewable energy advocates see Scaffold (temporary problem with sunset — PoS migration and renewable scaling create exit path). PoW legitimacy maintenance sees Piton (ritual persistence through inertia, theater > function). The analytical observer risks seeing Mountain (thermodynamics requires energy) but structural data reveals this as false summit. The divergence reflects fundamental disagreement about whether energy expenditure is immutable property of consensus or contingent design choice. The engine's task: measure the gap and identify which perspectives are naturalizing versus which are revealing structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Miners (beneficiaries, institutional power, arbitrage exit) get low d → low or negative chi. Climate system (victims, powerless, trapped exit) gets high d → high chi. Electricity consumers (ambiguous — benefit from electricity access, harmed by mining demand) get moderate d based on constrained exit. The resulting chi values scale effective extraction by scope (global → σ=1.2 amplification) and by the sigmoid transformation f(d). At global scope with high d values from victim perspectives, effective extraction chi becomes substantial even before applying f(d). Canonical fallback values are overridden by explicit structural data (beneficiary/victim declarations), preventing mis-classification from pure power atom defaults.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the declaration of both beneficiaries (miners, operators, token holders) and victims (climate, grid operators, consumers), paired with the genuine coordination function (PoW does solve Byzantine fault tolerance, mining does validate transactions). Without both elements, the constraint would collapse to either Rope (if only coordination visible) or Snare (if only extraction visible). The Tangled Rope classification holds if and only if: (1) coordination function is non-trivial (verified: PoW is a working consensus mechanism), (2) extraction is asymmetric (verified: energy costs are socialized, benefits privatized), and (3) active enforcement is required (verified: network protocol actively maintains PoW mechanism). The theatrical elevation (0.55) suggests that the coordination narrative is being overstated relative to function, but does not eliminate the genuine coordination problem. Proof-of-stake alternatives reduce but do not eliminate the mandatrophy — they demonstrate that the coordination function could be achieved at lower extraction cost, but do not prove that current PoW systems are purely extractive. The resolving insight: PoW is a valid but inefficient solution to a real problem; the constraint's extractiveness reflects inefficiency, not illegitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_of_stake_effectiveness,
    'Does proof-of-stake achieve cryptographic security equivalent to proof-of-work, or do PoW''s energy costs provide non-substitutable security properties?',
    'Empirical comparison of attack cost vectors, censorship resistance, and finality guarantees across PoW vs PoS implementations; longitudinal security audit data from Ethereum 2.0 and other PoS networks',
    'If PoS equally secure: PoW is purely extractive waste — classification upgrades to Snare. If PoW provides non-substitutable security: classification confirmed as Tangled Rope with genuine coordination benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_of_stake_effectiveness, empirical, 'Whether proof-of-stake provides equivalent security to proof-of-work').

omega_variable(
    renewable_mining_feasibility,
    'Can blockchain mining scale on renewable energy without distorting renewable deployment incentives or destabilizing grids through load inflexibility?',
    'Grid simulation data; historical analysis of mining migration to renewable-rich regions; measurement of grid stability impact during demand volatility',
    'If feasible: Scaffold sunset is real and extractiveness could decline significantly. If infeasible: mining will remain tied to baseload fossil fuels, converting into pure extraction (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_mining_feasibility, empirical, 'Whether mining can scale on renewable energy without grid destabilization').

omega_variable(
    externality_pricing_mechanism,
    'Can carbon pricing (tax, cap-and-trade, grid demand pricing) effectively internalize blockchain''s energy externality, or do game-theoretic dynamics (mining jurisdiction arbitrage, protocol design against pricing) prevent implementation?',
    'Policy implementation tracking across jurisdictions with carbon tax; measurement of mining relocation patterns in response to pricing; protocol-level analysis of potential mechanisms to enforce carbon cost internalization',
    'If priceable: Tangled Rope confirmed with partial remedy path. If unpriceable: extraction persists even with policy intent, confirming Snare classification for climate victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_pricing_mechanism, conceptual, 'Whether blockchain energy externality can be priced and internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blockchain_energy_externality, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bce_tr_t0, blockchain_energy_externality, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bce_tr_t3, blockchain_energy_externality, theater_ratio, 3, 0.48).
narrative_ontology:measurement(bce_tr_t6, blockchain_energy_externality, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(bce_be_t0, blockchain_energy_externality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bce_be_t3, blockchain_energy_externality, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(bce_be_t6, blockchain_energy_externality, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blockchain_energy_externality, enforcement_mechanism).
narrative_ontology:affects_constraint(blockchain_energy_externality, carbon_credit_accounting_protocol).
narrative_ontology:affects_constraint(blockchain_energy_externality, renewable_energy_infrastructure_competition).

% DUAL FORMULATION NOTE:
% Blockchain energy externality decomposes into two structurally distinct constraints: (1) mining_consensus_energy_requirement (ε~0.20, the computational cost of PoW itself — near-immutable given PoW mechanism choice), and (2) blockchain_energy_externality (ε~0.58, the socialization of that cost via lack of carbon pricing and jurisdictional arbitrage — contingent institutional arrangement). This story focuses on the second constraint. The first would be classified as Tangled Rope (genuine computation + externality) if decomposed separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
