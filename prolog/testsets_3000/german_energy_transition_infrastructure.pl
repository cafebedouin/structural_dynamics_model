% ============================================================================
% CONSTRAINT STORY: german_energy_transition_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_energy_transition_infrastructure, []).

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
 *   constraint_id: german_energy_transition_infrastructure
 *   human_readable: German Energy Transition Infrastructure Coordination and Extraction
 *   domain: energy_policy/infrastructure
 *
 * SUMMARY:
 *   The German Energiewende (energy transition) creates a structural tension
 *   between the coordination requirements for integrating variable renewable
 *   energy sources into a stable grid and the extractive mechanisms embedded
 *   in monopoly grid operations, subsidy structures, and incumbent utility
 *   market dominance. The constraint exhibits characteristics of all major
 *   types but clusters around Tangled Rope: it performs genuine coordination
 *   (grid balancing, renewable integration, decarbonization) while
 *   simultaneously extracting from vulnerable agents (distributed renewable
 *   producers locked into feed-in tariff systems, consumers bearing grid
 *   modernization costs, coal-dependent regions facing stranded assets). The
 *   extractiveness value (0.52) reflects that while extraction is
 *   significant, the underlying coordination problem is real — integrating
 *   50%+ wind and solar capacity on a grid designed for central generation
 *   requires active system management. Suppression (0.65) reflects structural
 *   barriers: monopoly grid operators control connection and dispatch,
 *   regional systems lack competition, coal regions have limited economic
 *   alternatives, and consumer bill transparency obscures cost allocation.
 *   Theater ratio (0.58) indicates moderate performative content: regulatory
 *   frameworks (capacity auctions, connection procedures) involve extensive
 *   ritual despite much actual balancing occurring through markets and
 *   automated systems.
 *
 * KEY AGENTS:
 *   - Incumbent Utilities (E.ON, RWE, Vattenfall): Primary beneficiaries (institutional/arbitrage) — extract through subsidy capture, grid priority for large plants, industrial electricity contracts, and cross-border trading. Can exit to European markets.
 *   - Distributed Renewable Producers (solar homeowners, wind cooperatives): Primary victims (powerless/trapped) — locked into feed-in tariff systems, face high grid connection costs, margin extraction through system fees. Cannot exit without abandoning capital.
 *   - Grid Operators (Tennet, Amprion, TransnetBW, 50Hertz): Institutional coordinators (institutional/arbitrage) — manage variable supply and demand, benefit from ecosystem complexity requiring their technical services. See constraint as coordination problem.
 *   - Residential Consumers: Secondary victims (moderate/constrained) — benefit from decarbonized electricity but bear grid modernization costs through rising bills. Can switch providers but not exit system fees.
 *   - Coal-Dependent Regions (Ruhr, Lausitz, Cologne): Regional economy victims (moderate/trapped) — face extraction through stranded assets, workforce transition costs, and infrastructure write-downs. Geographic mobility constrained.
 *   - Environmental NGOs and Renewable Advocates: Organized agents (organized/constrained) — see constraint as temporary coordination failure with sunset logic driven by battery cost curves and regulatory reform.
 *   - Federal Grid Agency (Bundesnetzagentur): Regulatory infrastructure (institutional/arbitrage) — maintains elaborate governance theater while actual system management increasingly automated and market-driven.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional extraction as physical necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_energy_transition_infrastructure, 0.52).
domain_priors:suppression_score(german_energy_transition_infrastructure, 0.65).
domain_priors:theater_ratio(german_energy_transition_infrastructure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_energy_transition_infrastructure, extractiveness, 0.52).
narrative_ontology:constraint_metric(german_energy_transition_infrastructure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(german_energy_transition_infrastructure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_energy_transition_infrastructure, tangled_rope).
narrative_ontology:human_readable(german_energy_transition_infrastructure, "German Energy Transition Infrastructure Coordination and Extraction").
narrative_ontology:topic_domain(german_energy_transition_infrastructure, "energy_policy/infrastructure").

domain_priors:requires_active_enforcement(german_energy_transition_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_energy_transition_infrastructure, incumbent_utilities).
narrative_ontology:constraint_beneficiary(german_energy_transition_infrastructure, manufacturing_sector).
narrative_ontology:constraint_beneficiary(german_energy_transition_infrastructure, grid_operators).
narrative_ontology:constraint_victim(german_energy_transition_infrastructure, distributed_renewable_producers).
narrative_ontology:constraint_victim(german_energy_transition_infrastructure, consumers).
narrative_ontology:constraint_victim(german_energy_transition_infrastructure, regional_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL DISTRIBUTED RENEWABLE PRODUCERS (SNARE) — Feed-in tariffs and grid connection rules create high barriers to market entry and extraction of margins through grid fees. Producers are locked into the system by subsidy structure and cannot exit without abandoning investments. Trapped at biographical scale with no alternative market access.
constraint_indexing:constraint_classification(german_energy_transition_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESIDENTIAL CONSUMERS AND COOPERATIVES (TANGLED ROPE) — Benefit from renewable grid access and reduced carbon electricity; face extraction through grid modernization costs and system balancing fees. High suppression due to monopoly grid operators and limited exit options. Can switch providers but not escape system costs. Mixed coordination (grid stability) and extraction (cost asymmetry).
constraint_indexing:constraint_classification(german_energy_transition_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GRID OPERATORS AND SYSTEM INTEGRATORS (ROPE) — Experience the constraint as pure coordination problem: managing variable renewable supply, balancing regional capacity, integrating prosumers. Benefit from subsidized renewable growth creating ecosystem complexity requiring their services. High arbitrage options through European market access and technical service exports.
constraint_indexing:constraint_classification(german_energy_transition_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT UTILITIES AND MANUFACTURING (ROPE) — Primary beneficiaries extracting from the transition through subsidy capture, grid priority, and industrial electricity contracts. Experience the constraint as coordinating their transition while maintaining market dominance. Arbitrage options through European energy markets and cross-sector mobility.
constraint_indexing:constraint_classification(german_energy_transition_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENVIRONMENTAL NGOS AND RENEWABLE ADVOCATES (SCAFFOLD) — See the constraint as a temporary coordination failure with sunset logic. EU decarbonization targets and green technology cost curves are making fossil-based extraction mechanisms unsustainable. Exit path visible: regulatory shift toward full-cost pricing and grid parity eliminate the extraction mechanism's foundation. Sunset estimated 10-15 years as battery storage and demand management mature.
constraint_indexing:constraint_classification(german_energy_transition_infrastructure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL GRID AGENCY AND REGULATORY INFRASTRUCTURE (PITON) — Maintains elaborate regulatory theater around grid stability, capacity planning, and fairness mechanisms. The actual function (managing variable renewables) is increasingly handled by market mechanisms and automated control. Regulatory rituals persist through institutional inertia: grid connection hearings, capacity auctions, regional balancing markets that duplicate each other. Theater ratio high — performative coordination masking that real power flows through technical systems and financial derivatives.
constraint_indexing:constraint_classification(german_energy_transition_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICS VIEW (MOUNTAIN) — From civilizational scope, some structural constraints are immutable: physics of grid stability (voltage, frequency, reserve capacity), thermodynamic limits on conversion efficiency, geographic distribution of renewable resources. These appear as natural laws. However, the base properties reveal this as false summit — most extraction derives from institutional arrangement (subsidy rules, monopoly grid access) not from physics. Engine flags as naturalization.
constraint_indexing:constraint_classification(german_energy_transition_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_energy_transition_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_energy_transition_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_energy_transition_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(german_energy_transition_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(german_energy_transition_infrastructure, TR),
    TR >= 0.70.

:- end_tests(german_energy_transition_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Elevated and rising over time (0.28→0.52 across 15-year interval). Initial period (2010-2015) saw moderate extraction as feed-in tariffs primarily subsidized producers; middle period (2015-2020) saw extraction rise as grid operators began charging variable fees and system balancing costs concentrated on distributed sources; current period (2020-2025) shows sustained extraction as incumbent utilities maintain margin capture despite regulatory pressure. The trajectory reflects accumulation of rent-seeking layers atop genuine coordination costs rather than exponential growth. Suppression (0.65): High and stable. Structural barriers include: (1) monopoly grid operator control over connection and dispatch (no alternative grid), (2) subsidy lock-in making exit costly for distributed producers, (3) geographic immobility of coal regions, (4) consumer information asymmetry about bill composition. These are enforcement mechanisms — agents understand they face high costs to exit and lack alternatives. Theater ratio (0.58): Moderate and rising. Early period had genuine technical necessity for coordination (grid stability was non-trivial challenge). Current period increasingly relies on market mechanisms (balancing power markets, automated frequency response) and financial derivatives (wind-down contracts), yet regulatory theater persists (connection hearings, capacity auctions, regional balancing committees) as legitimation. The theater increased because actual coordination mechanisms are now sufficiently automated that human-facing regulatory processes are more ritual than functional.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why single-perspective analysis fails for infrastructure transitions. The beneficiary institutional actor (utilities) will report that the constraint is pure coordination (Rope) with necessary margins for managing complexity. This is analytically correct from their position but masks the extraction borne by powerless agents (distributed producers) and moderate agents (consumers, coal regions). The organized renewable advocacy coalition will report that the constraint is temporary (Scaffold) with visible sunset pathways (battery cost curves, regulatory reform), which is also correct but only visible from a power position with exit options and generational time horizons. The trapped distributed producer sees permanent extraction (Snare), which is correct from their biographical perspective but may be temporally limited (battery storage may eventually enable prosumer markets). The piton observation (regulatory theater) is correct but can obscure that some theater reflects genuine coordination challenges early in the transition — the theater is rising over time as actual technical complexity declines relative to institutional elaboration. No single perspective captures the full structure; the presheaf over all perspectives (including the false-summit analytical view) is required.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from beneficiary/victim declarations and exit options. Incumbent utilities benefit from the constraint and have arbitrage options (exit to European markets, diversification into gas/nuclear), yielding low d (~0.15). Distributed renewable producers are victims locked into tariff systems with trapped/constrained exit options, yielding high d (0.85-0.95). Consumers are victims with constrained exit (can switch providers but not escape system fees), yielding moderate-high d (0.65-0.75). Grid operators benefit through ecosystem complexity requiring their services and have arbitrage options, yielding low-moderate d (0.30-0.40). Coal regions are victims with trapped exit (geographic immobility), yielding high d (0.90). Environmental NGOs are organized actors with constrained exit and mixed beneficiary/victim status (benefit from decarbonization, victimized by extraction), yielding moderate d (0.50-0.60). The resulting chi values reflect these divergent experiences: beneficiaries experience low/negative chi (constraint feels like profitable coordination); victims experience high chi (constraint feels like extraction); organized agents see moderate chi with sunset logic.
 *
 * MANDATROPHY ANALYSIS:
 *   EXEMPLAR OF CONSTRAINT FAMILY DECOMPOSITION: The German energy transition is not a single constraint but a family of 3-4 linked constraints with distinct ε values. The structure should be decomposed as: (1) german_grid_integration_coordination (ε≈0.15, Rope) — the genuine physical/technical problem of matching variable supply to demand, centrally located infrastructure, requires sophisticated balancing; (2) incumbent_utility_subsidy_capture (ε≈0.65, Snare) — extraction of rents through preferential grid access and industrial electricity contracts, locked victim structure; (3) distributed_renewable_lock_in (ε≈0.58, Snare/Tangled Rope) — feed-in tariff system that subsidizes entry but then extracts through variable fees and capacity charges; (4) coal_regional_stranded_assets (ε≈0.72, Snare) — geographic victims locked into stranded coal infrastructure with no market exit. The current single-constraint modeling conflates genuine coordination (0.15) with institutional extraction (0.65) yielding an intermediate value (0.52). The decomposition would enable precision targeting: coordination costs are unavoidable and legitimate (grid balancing, reserve capacity); extraction mechanisms can be reformed (competitive grid operation, transparent cost allocation, regional economic transition support). The mandatrophy resolves through network decomposition — each constraint in the family has its own perspectives, beneficiary/victim declarations, and lifecycle trajectory. The family structure reveals that 'the Energiewende' is not a single phenomenon but an overlapping set of coordination problems and extraction mechanisms whose futures diverge: grid coordination is solvable (Rope→Scaffold→cheaper grid); incumbent utility extraction is resolvable (Snare→Rope through market reform); distributed producer lock-in is resolvable (Snare→Rope/Scaffold through battery storage); coal region stranded assets require targeted transition support (Snare with external aid).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grid_stability_vs_monopoly_extraction,
    'What portion of measured extraction (0.52) reflects legitimate grid stability costs vs monopoly pricing by grid operators?',
    'Comparative analysis of grid balancing costs in competitive markets (Texas ERCOT, Australian NEM) vs monopoly-operated systems (German TSO). Cross-national cost transparency.',
    'If legitimate costs > 80%: reclassify as Rope (pure coordination). If extraction > 60%: confirm Tangled Rope or escalate to Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_stability_vs_monopoly_extraction, empirical, 'Grid stability cost vs monopoly extraction attribution').

omega_variable(
    feed_in_tariff_subsidy_incidence,
    'Do feed-in tariff subsidies primarily flow to distributed producers or get captured by equipment manufacturers and utilities?',
    'Decomposition of subsidy flows from consumer bills through tariff structure; tracking of equipment cost reductions vs tariff reductions over time; analysis of who retains rents.',
    'If distributed producers capture >60% of net subsidy: snare classification may overstate extraction. If utilities/manufacturers capture >60%: snare is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feed_in_tariff_subsidy_incidence, empirical, 'Feed-in tariff subsidy incidence and distribution').

omega_variable(
    battery_storage_sunset_credibility,
    'Are cost trajectories and deployment rates for battery storage sufficient to actually break the grid operator extraction mechanism within 10-15 years?',
    'Projection of battery cost curves, installation rates, and energy density improvements; modeling of grid balancing requirements with high-penetration storage; regulatory readiness assessment.',
    'If feasible: scaffold classification confirmed — exit path is real and constrained. If infeasible: reclassify organizational actors to trapped/constrained rather than having genuine sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(battery_storage_sunset_credibility, empirical, 'Battery storage cost curve trajectory and grid adequacy timing').

omega_variable(
    regional_coal_phase_out_stranded_assets,
    'To what degree does regional economy extraction derive from stranded coal assets and workforce transition costs vs genuine grid coordination requirements?',
    'Geographic decomposition of extraction costs by region; correlation between coal-dependent regions and measured suppression/extraction; comparison to non-coal transition regions.',
    'If stranded assets explain >50% of extraction: regional economy victims may be transitional (Scaffold) rather than structural (Snare). Affects whether constraint is truly national or decomposes into regional stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_coal_phase_out_stranded_assets, empirical, 'Stranded coal asset cost attribution in regional extraction').

omega_variable(
    european_integration_arbitrage_scope,
    'How much does incumbent utility arbitrage (institutional/arbitrage exit option) derive from European grid integration rather than German domestic extraction?',
    'Tracking of incumbent utility revenue sources: domestic market share vs cross-border trading, export of balancing services, arbitrage across price zones. If cross-border >40%, directionality may reflect European dynamics more than German extraction.',
    'If high arbitrage: beneficiary power is more mobile than modeling assumes. May require separate institutional perspectives for utilities as German domestic actors vs European market participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_integration_arbitrage_scope, empirical, 'Incumbent utility arbitrage scope and geographic distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_energy_transition_infrastructure, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(getrans_tr_t0, german_energy_transition_infrastructure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(getrans_tr_t5, german_energy_transition_infrastructure, theater_ratio, 5, 0.5).
narrative_ontology:measurement(getrans_tr_t10, german_energy_transition_infrastructure, theater_ratio, 10, 0.56).
narrative_ontology:measurement(getrans_tr_t15, german_energy_transition_infrastructure, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(getrans_be_t0, german_energy_transition_infrastructure, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(getrans_be_t5, german_energy_transition_infrastructure, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(getrans_be_t10, german_energy_transition_infrastructure, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(getrans_be_t15, german_energy_transition_infrastructure, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_energy_transition_infrastructure, global_infrastructure).
narrative_ontology:affects_constraint(german_energy_transition_infrastructure, german_coal_phase_out).
narrative_ontology:affects_constraint(german_energy_transition_infrastructure, eu_carbon_pricing_mechanism).
narrative_ontology:affects_constraint(german_energy_transition_infrastructure, renewable_energy_grid_parity).

% DUAL FORMULATION NOTE:
% The German energy transition constraint decomposes into structurally distinct claims: grid integration coordination (ε≈0.15, Rope), incumbent utility capture (ε≈0.65, Snare), distributed renewable lock-in (ε≈0.58, Tangled Rope), and coal regional stranded assets (ε≈0.72, Snare). The current story treats these as one constraint (ε=0.52); precision analysis requires family decomposition into four linked stories with different beneficiary/victim structures and different sunset horizons. See german_grid_integration_coordination, german_utility_subsidy_capture, german_distributed_renewable_lock_in, german_coal_stranded_assets for decomposed analyses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(german_energy_transition_infrastructure, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
