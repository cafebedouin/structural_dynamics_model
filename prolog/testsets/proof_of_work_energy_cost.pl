% ============================================================================
% CONSTRAINT STORY: proof_of_work_energy_cost
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proof_of_work_energy_cost, []).

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
 *   constraint_id: proof_of_work_energy_cost
 *   human_readable: Proof of Work Energy Cost Constraint
 *   domain: cryptography/blockchain/environmental
 *
 * SUMMARY:
 *   Proof of Work (PoW) energy consumption creates a structural constraint
 *   that bridges cryptographic necessity and environmental externality.
 *   Bitcoin and residual PoW chains require continuous energy expenditure to
 *   secure distributed consensus — this energy cost is simultaneously the
 *   mechanism by which mining operations prove work, the beneficiary of block
 *   rewards, and a socialized cost borne by electricity grid operators and
 *   carbon-budget-bearing populations. The constraint exhibits high
 *   tangled-rope characteristics: genuine coordination function (trustless
 *   consensus without central authority) coexists with asymmetric extraction
 *   (mining economics externalize grid strain and environmental costs onto
 *   non-participants). The extractiveness has increased over the measurement
 *   interval (0.35 → 0.62) as mining scale has grown and energy cost per
 *   transaction has risen. Theater ratio remains relatively low (0.35),
 *   indicating the constraint is functionally coherent rather than
 *   performative — energy expenditure genuinely secures the network, not
 *   merely signals commitment. However, the emergence of Proof of Stake
 *   (Ethereum, modern chains) and the intellectual credibility of PoS
 *   consensus protocols have created a competing pathway, introducing piton
 *   dynamics: PoW persists in Bitcoin through consensus lock-in and network
 *   effects despite known alternatives of lower energy intensity.
 *
 * KEY AGENTS:
 *   - Energy-Bearing Populations: Primary victim (powerless/trapped) — bear socialized costs of grid strain, carbon emissions, and opportunity cost of electricity diverted from other uses. No exit mechanism or compensation.
 *   - Grid Operators: Secondary victim (moderate/constrained) — manage peak-load coordination; subordinated to mining demand inflexibility. Constrained by physics of grid management and mining capital mobility.
 *   - Mining Operations: Primary beneficiary (institutional/arbitrage) — capture block rewards; experience energy cost as coordination mechanism. High exit options (relocate, switch chains, retire).
 *   - Hardware Manufacturers: Secondary beneficiary (institutional/arbitrage) — drive ASIC efficiency innovation; profit from mining R&D without bearing energy externalities.
 *   - Blockchain Participants: Mixed (organized/constrained) — benefit from network security; partly insulated from energy cost externalities through participation benefit.
 *   - PoW Protocol (Bitcoin): Institutional actor (institutional/constrained) — locked into PoW through consensus; cannot unilaterally exit despite alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proof_of_work_energy_cost, 0.58).
domain_priors:suppression_score(proof_of_work_energy_cost, 0.62).
domain_priors:theater_ratio(proof_of_work_energy_cost, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proof_of_work_energy_cost, extractiveness, 0.58).
narrative_ontology:constraint_metric(proof_of_work_energy_cost, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(proof_of_work_energy_cost, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proof_of_work_energy_cost, tangled_rope).
narrative_ontology:human_readable(proof_of_work_energy_cost, "Proof of Work Energy Cost Constraint").
narrative_ontology:topic_domain(proof_of_work_energy_cost, "cryptography/blockchain/environmental").

domain_priors:requires_active_enforcement(proof_of_work_energy_cost).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proof_of_work_energy_cost, mining_hardware_manufacturers).
narrative_ontology:constraint_beneficiary(proof_of_work_energy_cost, miners_with_cheap_energy_access).
narrative_ontology:constraint_beneficiary(proof_of_work_energy_cost, blockchain_security_beneficiaries).
narrative_ontology:constraint_victim(proof_of_work_energy_cost, energy_grid_operators).
narrative_ontology:constraint_victim(proof_of_work_energy_cost, carbon_budget_bearing_populations).
narrative_ontology:constraint_victim(proof_of_work_energy_cost, competing_electricity_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY-BEARING POPULATIONS (SNARE) — Climate and grid capacity costs are socialized; mining operations externalize environmental damage. Trapped populations bear the full cost of grid strain and carbon emissions without exit or compensation mechanism. Maximum extraction with minimal coordination benefit — the security of cryptocurrency networks accrues to participants; the energy burden accrues to non-participants.
constraint_indexing:constraint_classification(proof_of_work_energy_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GRID OPERATORS (TANGLED ROPE) — Coordinate electricity distribution and peak-load management (genuine coordination function). But PoW mining creates inflexible demand spikes that subordinate grid operators to mining profitability. Constrained by physics (cannot simply deny power without system instability) and by mining capital mobility. Extraction and coordination coexist: grid operators solve real coordination problems but mining extraction undermines their resource management.
constraint_indexing:constraint_classification(proof_of_work_energy_cost, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MINING OPERATIONS (ROPE) — Experience PoW energy cost as a coordination mechanism: energy expenditure certifies transaction validation, enabling trustless network operation. Beneficiary perspective — energy cost is the lever by which mining operations claim block rewards. Exit options are high (arbitrage to alternative chains, hardware, locations) — this is a participation choice, not an imposed constraint.
constraint_indexing:constraint_classification(proof_of_work_energy_cost, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HARDWARE MANUFACTURERS (ROPE) — Pure beneficiary. Mining hardware R&D is driven entirely by PoW energy cost — more efficient ASIC chips capture arbitrage. No exit cost: manufacturing follows demand. Coordination benefit is genuine (industry standards, energy efficiency innovation) but comes downstream of extraction from other agents.
constraint_indexing:constraint_classification(proof_of_work_energy_cost, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BLOCKCHAIN SECURITY BENEFICIARIES (TANGLED ROPE) — Depend on PoW energy expenditure for network security (genuine coordination benefit). But organized agents (node operators, exchange protocols, institutional holders) are partly insulated from energy cost externalities. Constrained exit: switching to PoS requires protocol consensus, not unilateral action. Asymmetric: security benefits accrue to participants; energy costs accrue to populations outside the blockchain.
constraint_indexing:constraint_classification(proof_of_work_energy_cost, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE POW SECURITY PARADIGM (PITON) — PoW as security mechanism is being replaced by PoS and alternative consensus (Ethereum transition, emerging chains). The PoW paradigm persists in Bitcoin and residual chains through inertia and consensus lock-in. Theater ratio reflects: energy expenditure is *performative* as security after PoS viability is proven — it continues because the network cannot easily exit, not because it is functionally superior. Piton: the mechanism is degraded relative to known alternatives but maintained through institutional lock-in.
constraint_indexing:constraint_classification(proof_of_work_energy_cost, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a universal perspective, trustless consensus over a distributed network necessarily requires some energy expenditure as a scarce resource that cannot be spoofed. Energy cost is inherent to solving the Byzantine generals problem without central authority. However, the structural data contradicts this classification — PoS demonstrates that equivalent security can be achieved with dramatically lower energy expenditure. The 'mountain' framing naturalizes PoW's specific high-energy implementation as if it were the only possible solution, masking that the constraint is technological choice, not thermodynamic law.
constraint_indexing:constraint_classification(proof_of_work_energy_cost, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proof_of_work_energy_cost_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(proof_of_work_energy_cost, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proof_of_work_energy_cost, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(proof_of_work_energy_cost, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(proof_of_work_energy_cost, TR),
    TR >= 0.70.

:- end_tests(proof_of_work_energy_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine coordination function (trustless consensus requires energy scaricity that cannot be spoofed) alongside significant extraction (environmental and grid costs externalized to non-participants). The value reflects that PoW solves a real problem but at higher energy cost than proven alternatives. The trajectory from 0.35 to 0.62 reflects mining scale growth without corresponding efficiency gains — block reward levels stabilize, but network hash rate increases, driving total energy expenditure upward. Suppression (0.62): Moderate-high. Barriers to exit from PoW networks include: (1) consensus lock-in — Bitcoin holders cannot unilaterally switch protocols; (2) network effects — PoW chain value depends on hash rate and perceived immutability; (3) mining capital sunk costs — ASIC hardware is chain-specific; (4) political economy — miners have veto power over protocol change. These barriers are structural, not universal — holders can sell and move capital, miners can relocate, but the PoW commitment is highly path-dependent. Theater ratio (0.35): Low, with increasing trend. PoW energy expenditure is functionally coherent as a security mechanism, not primarily performative. The rising trend reflects that as PoS alternatives mature and prove viable, the continued energy expenditure in Bitcoin becomes increasingly performative — it continues not because it is the most efficient solution but because protocol exit is collectively difficult. The theater increase signals piton dynamics emerging.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap opposes beneficiaries and victims. Mining operations see PoW as a legitimate market mechanism (rope) — participants choose to mine or hold cryptocurrency, accepting energy costs as the price of participation and security. Energy-bearing populations see PoW as extraction (snare) — they bear costs of grid strain and carbon emissions without choice, benefit, or compensation. Grid operators see mixed coordination and extraction (tangled rope) — they solve genuine load-balancing problems but are subordinated to mining demand. This gap reflects structural power: beneficiaries control exit and profit; victims are locked into electricity consumption and climate bearing. The false summit appears when observers naturalize PoW's high energy as thermodynamic necessity rather than recognizing it as a design choice with known lower-energy alternatives (PoS).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Mining operations are beneficiaries with arbitrage-level exit options (can relocate, exit chain, retire) — they derive d from structured data as approximately 0.15-0.25 (low extraction experienced). Energy-bearing populations are victims with trapped exit options — d ≈ 0.95 (high extraction experienced). Grid operators are moderate victims with constrained exit — d ≈ 0.65. The asymmetry between mining operations' d (beneficiary, arbitrage) and populations' d (victim, trapped) generates the tangled-rope classification: coordinating at low cost for beneficiaries, extracting at high cost from victims, in a single constraint. Scope modifier σ(global) = 1.2 scales extractiveness upward, reflecting that energy cost externalities affect planetary climate and grid infrastructure globally.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through structural decomposition. The mandatrophy asks: is PoW energy cost a fundamental coordination problem (mountain) that blockchains must solve, or an extractive mechanism (snare) that externalizes costs onto non-participants? The resolution: it is BOTH, structured differently across perspectives. From a physics/cryptography view (mountain perspective), trustless consensus requires some energy-based scaricity to prevent Sybil attacks — this is a genuine coordination requirement. From an implementation view (tangled rope perspective), PoW achieves this coordination at far higher energy cost than necessary because of Bitcoin's specific consensus rules and economic incentives. From a climate/grid view (snare perspective), the energy costs are externalized to non-participants and socialized as environmental damage. The mandatrophy is resolved by recognizing that PoW is a valid but expensive solution to a real problem. The existence of PoS as a lower-cost alternative proves that the current energy expenditure in Bitcoin is not minimally necessary — it reflects path-dependence and institutional lock-in rather than thermodynamic law. The theater ratio's increase (0.20 → 0.35) confirms piton emergence: as alternatives prove viable, the continued energy expenditure becomes increasingly performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pow_vs_pos_security_equivalence,
    'Do PoS and PoW provide genuinely equivalent security guarantees, or does PoW''s energy expenditure provide asymptotic security advantages that PoS cannot achieve?',
    'Comparative analysis of consensus attack costs, nothing-at-stake problem resolution, economic finality guarantees across PoW and PoS implementations; empirical security incident data for PoS chains vs PoW chains across equivalent time horizons',
    'If equivalent: PoW energy cost is pure waste (snare reclassification across more perspectives, mountain reclassified as false summit). If PoW-asymptotic: constraint becomes mountain from analytical perspective; PoS architectures are not true alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pow_vs_pos_security_equivalence, empirical, 'Whether PoW and PoS provide equivalent security').

omega_variable(
    externality_internalization_path,
    'Can energy cost externalities be internalized through carbon pricing, grid fees, or energy-backed currencies without fundamentally altering mining economics?',
    'Economic modeling of mining profitability under varying carbon price scenarios; empirical observation of mining relocation patterns when electricity costs change; comparison of mining concentration in carbon-heavy vs renewable energy regions',
    'If internalization is possible: constraint becomes rope (coordination with distributed cost). If not: extraction is structural, snare reclassification. If mining becomes unprofitable: PoW security collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_internalization_path, empirical, 'Whether externalities can be internalized').

omega_variable(
    consensus_protocol_path_lock_in,
    'Is Bitcoin''s commitment to PoW a genuine technological lock-in (requires 51% consensus to change), or is the energy expenditure level contingent on economic incentives that could be altered without protocol change?',
    'Analysis of mining participation in hypothetical reduced-reward scenarios; game theory modeling of mining economics under carbon tax; observation of whether miners can credibly shift to PoS-equivalent energy consumption without protocol fork',
    'If genuine lock-in: PoW persists as piton indefinitely. If economic contingency: constraint could shift to rope if mining incentives change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_protocol_path_lock_in, conceptual, 'Whether PoW consensus is locked in or contingent on economics').

omega_variable(
    energy_source_carbon_intensity_variability,
    'Does mining concentration in renewable-energy-rich regions (Iceland, Norway, stranded hydroelectric) actually avoid carbon emissions, or does it displace emissions by preempting renewable energy that would otherwise replace fossil baseload?',
    'Marginal carbon accounting: grid carbon intensity at the margin of mining electricity addition; analysis of renewable energy development patterns in mining-intensive regions; counterfactual grid composition if mining demand were removed',
    'If truly low-carbon: snare classification weakens (less genuine victimhood of carbon-bearing populations). If displacement: extraction from non-mining populations remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_source_carbon_intensity_variability, empirical, 'Whether mining in renewables actually avoids carbon emissions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proof_of_work_energy_cost, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pow_energy_tr_t0, proof_of_work_energy_cost, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pow_energy_tr_t3, proof_of_work_energy_cost, theater_ratio, 3, 0.28).
narrative_ontology:measurement(pow_energy_tr_t6, proof_of_work_energy_cost, theater_ratio, 6, 0.35).
narrative_ontology:measurement(pow_energy_tr_t9, proof_of_work_energy_cost, theater_ratio, 9, 0.42).

% Extraction over time
narrative_ontology:measurement(pow_energy_be_t0, proof_of_work_energy_cost, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pow_energy_be_t3, proof_of_work_energy_cost, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(pow_energy_be_t6, proof_of_work_energy_cost, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(pow_energy_be_t9, proof_of_work_energy_cost, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proof_of_work_energy_cost, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(proof_of_work_energy_cost, 0.25).
narrative_ontology:affects_constraint(proof_of_work_energy_cost, blockchain_consensus_mechanism_choice).
narrative_ontology:affects_constraint(proof_of_work_energy_cost, cryptocurrency_volatility_external_costs).
narrative_ontology:affects_constraint(proof_of_work_energy_cost, renewable_energy_grid_displacement).

% DUAL FORMULATION NOTE:
% PoW energy cost is downstream of the consensus mechanism choice (PoW vs PoS). Separate constraint story: blockchain_consensus_mechanism_choice (ε=0.72, Tangled Rope) describes the institutional lock-in and path-dependence that maintains PoW in Bitcoin despite PoS viability. PoW energy cost (ε=0.58) is the downstream extractive consequence of that lock-in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(proof_of_work_energy_cost, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
