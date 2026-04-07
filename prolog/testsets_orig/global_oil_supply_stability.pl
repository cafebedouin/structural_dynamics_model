% ============================================================================
% CONSTRAINT STORY: global_oil_supply_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_oil_supply_stability, []).

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
 *   constraint_id: global_oil_supply_stability
 *   human_readable: Global Oil Supply Stability as Coordination and Extraction
 *   domain: geopolitical_economy/energy_infrastructure
 *
 * SUMMARY:
 *   Global oil supply stability functions as both a coordination mechanism
 *   solving the collective action problem of energy infrastructure investment
 *   and an extraction mechanism concentrating rents to oil-producing states
 *   and integrated energy majors at the cost of import-dependent economies
 *   and renewable transition actors. The constraint exhibits the full
 *   spectrum of DR classification: from snare (oil-importing nations without
 *   alternatives) to rope (diversified energy companies) to scaffold
 *   (renewable transition pathways with temporal sunset) to piton (Strategic
 *   Petroleum Reserve ritual). The extractiveness (0.58) reflects moderate
 *   asymmetric advantage to producers over consumers; suppression (0.68)
 *   reflects the high barriers to exit through renewable transition or energy
 *   efficiency. Theater ratio (0.55) reflects that approximately half of the
 *   observable supply management activity is genuine coordination (preventing
 *   wasteful price crashes) and half is performative (ritual reserve
 *   management, public announcements without material production changes).
 *
 * KEY AGENTS:
 *   - Oil-Producing Petrostates (Saudi Arabia, Russia, Iran): Primary beneficiaries (institutional/arbitrage) — capture rents through production discipline and cartel coordination
 *   - Oil-Importing Nations Without Reserves (India, Turkey, most developing nations): Primary victims (powerless/trapped) — dependent on imports with no exit capacity; bear full cost of supply disruptions and price volatility
 *   - Integrated Energy Majors (Shell, Chevron, TotalEnergies, Equinor): Secondary beneficiaries (institutional/arbitrage) — benefit from stable supply and infrastructure; have diversification options
 *   - Renewable Energy Transition Actors (solar, wind, battery, grid-tech): Secondary victims (moderate/constrained) — constrained by capital competition and regulatory capture despite enabling conditions from energy crisis
 *   - Climate-Committed Nations (Paris Agreement signatories): Organized actors (organized/constrained) — see energy transition as temporary constraint with sunset; constrained by energy poverty risk during transition
 *   - Strategic Petroleum Reserve System: Institutional actor (institutional/arbitrage) — maintains performative ritual; primary function increasingly decorative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_oil_supply_stability, 0.58).
domain_priors:suppression_score(global_oil_supply_stability, 0.68).
domain_priors:theater_ratio(global_oil_supply_stability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_oil_supply_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_oil_supply_stability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(global_oil_supply_stability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_oil_supply_stability, tangled_rope).
narrative_ontology:human_readable(global_oil_supply_stability, "Global Oil Supply Stability as Coordination and Extraction").
narrative_ontology:topic_domain(global_oil_supply_stability, "geopolitical_economy/energy_infrastructure").

domain_priors:requires_active_enforcement(global_oil_supply_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_oil_supply_stability, oil_producing_nations).
narrative_ontology:constraint_beneficiary(global_oil_supply_stability, integrated_energy_companies).
narrative_ontology:constraint_beneficiary(global_oil_supply_stability, consumer_nations_with_reserves).
narrative_ontology:constraint_victim(global_oil_supply_stability, oil_importing_nations_without_reserves).
narrative_ontology:constraint_victim(global_oil_supply_stability, renewable_transition_actors).
narrative_ontology:constraint_victim(global_oil_supply_stability, carbon_constrained_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OIL-IMPORT-DEPENDENT NATION (SNARE) — A nation dependent on imported oil with no strategic reserves faces maximum extraction. Supply disruptions trigger immediate economic crisis; the nation cannot exit the constraint without multi-decade infrastructure transformation. Exit cost is civilizational. Suppression is total: military dependence on oil-producing allies, currency reserves consumed by price volatility, renewable transition indefinitely delayed by capital scarcity. This agent experiences pure extraction with no coordinating function.
constraint_indexing:constraint_classification(global_oil_supply_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OIL-PRODUCING PETROSTATE (TANGLED ROPE) — Coordinates global energy supply (genuine coordination function) while extracting rents through artificial scarcity and cartel discipline. OPEC coordination solves a real collective action problem: without production discipline, oil prices would collapse and all producers would suffer. But this coordination mechanism is asymmetrically structured — it extracts from import-dependent nations and constrains renewable alternatives. Exit options are constrained by economic lock-in: diversification is capital-intensive and often politically blocked by resource nationalism. Both beneficiary and victim of their own constraint.
constraint_indexing:constraint_classification(global_oil_supply_stability, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTEGRATED ENERGY MAJOR (ROPE) — Multinational energy corporations with diversified portfolios experience the constraint as pure coordination. Oil supply stability enables long-term investment planning, futures markets, and refinery operations. The firm benefits from predictable pricing and established infrastructure. These firms have arbitrage options: they can shift investment into renewables, gas, or infrastructure. For them, oil stability is a solved coordination problem, not an extraction mechanism. Net experience: rope.
constraint_indexing:constraint_classification(global_oil_supply_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RENEWABLE ENERGY TRANSITION ACTOR (TANGLED ROPE) — Wind, solar, battery, and grid-modernization industries benefit from price volatility and oil-supply disruptions, which create urgency for transition investment. However, they are constrained by capital competition with established fossil fuel infrastructure, regulatory capture by oil interests, and the long payback cycles for renewable infrastructure. The constraint both enables (crisis creates mandate) and constrains (capital scarcity) their growth. High extraction from coordination benefit mismatch.
constraint_indexing:constraint_classification(global_oil_supply_stability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC PETROLEUM RESERVE SYSTEM (PITON) — The SPR (US, IEA, others) was created as a coordination mechanism to buffer supply shocks and prevent economic collapse from embargo. It has become substantially performative: reserve drawdowns are politically timed rather than reserve-scarcity driven; auction prices rarely reflect true emergency value; the system's primary function (stabilizing energy markets during genuine crisis) is opaque and rarely invoked. The SPR persists through institutional inertia, not active function. Theater ratio is high: the ritual of reserve management persists despite low actual protective capacity relative to total consumption.
constraint_indexing:constraint_classification(global_oil_supply_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE COMMITMENT COALITION (SCAFFOLD) — Nations and cities with net-zero commitments (Paris signatories, C40, subnational actors) experience oil supply stability as a temporary constraint with a sunset. The constraint declines as renewable capacity increases and demand shifts. However, enforcement is weak (many signatories miss targets), exit cost is high (energy poverty during transition), and the sunset timeline is contested. This is a genuine scaffold: coordination on transition pathway with explicit planned exit, but sunset is conditional on technology deployment and political will.
constraint_indexing:constraint_classification(global_oil_supply_stability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From the longest civilizational timescale, oil supply stability appears as an immutable law of geopolitical physics: energy density is finite, geological reserves are finite, production capacity is finite. Demand always exceeds supply at some price point. The structure of energy markets appears as a natural law constraint analogous to thermodynamic limits. However, the structural data contradicts this: the constraint is socially constructed (OPEC cartel behavior, renewable suppression, capital allocation decisions). This perspective risks false naturalization of extractive institutions as inherent limits.
constraint_indexing:constraint_classification(global_oil_supply_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_oil_supply_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_oil_supply_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_oil_supply_stability, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_oil_supply_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_oil_supply_stability, TR),
    TR >= 0.70.

:- end_tests(global_oil_supply_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts significant wealth from oil-importing to oil-producing nations through rent capture. OPEC+ production discipline maintains prices 20-40% above competitive marginal cost. However, extractiveness is not higher (e.g., 0.70+) because: (1) renewable alternatives are technically feasible and cost-competitive, preventing total lock-in; (2) integrated majors distribute rents to importing nations through refining operations and dividend flows; (3) supply shocks are genuine coordination problems whose solution has real value. Suppression (0.68): High but not total. Oil-importing nations face significant barriers to exit (infrastructure lock-in, capital scarcity, geopolitical dependence) but not insurmountable ones — renewable transition is technologically feasible within 20-30 years at scale. Strategic options exist (energy efficiency, electrification, hydrogen) but require sustained capital and political commitment. Theater ratio (0.55): OPEC+ announcements, reserve management, and price-target rhetoric are partially performative — they coordinate genuine supply decisions but also create false scarcity narratives. SPR management adds theatrical component (announced drawdowns that occur at small scale relative to consumption).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (oil supply coordination with asymmetric rents) produces divergent classifications from different structural positions. The mapping is not arbitrary: it follows from the indexical tuple. A powerless agent with trapped exit options experiences maximum extraction (snare). An institutional beneficiary with arbitrage options experiences coordination (rope). An organized actor with sunset options sees temporary constraint (scaffold). The perspectival divergence is diagnostic: it reveals that the constraint's 'stability' is distributed unevenly. Stability for whom? For the producer, it is. For the import-dependent powerless nation, it is destabilizing instability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Oil-producing states as institutional beneficiaries with arbitrage options (can shift to downstream, gas, sovereign wealth) get low d (0.15-0.25), resulting in negative effective extraction for them—they experience the constraint as beneficial coordination. Oil-importing nations as powerless victims with trapped exit options get high d (0.90-0.95), resulting in maximum experienced extraction. Integrated majors as institutional beneficiaries with arbitrage get low d. Renewable actors as moderate victims with constrained exit get moderate-high d (0.70-0.80). Climate-committed nations as organized actors with constrained exit get moderate d (0.50-0.65). The SPR as institutional arbitrage gets low d but high theater ratio prevents rope classification (piton instead). The analytical observer at civilizational scale risks deriving d from false naturalization—treating scarcity as fixed rather than socially constructed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through careful decomposition of perspectives. The claimed type (tangled_rope) reflects the composite: genuine coordination function (supply stabilization, infrastructure investment) + asymmetric extraction (rent capture, transition blocking). The beneficiary perspective (rope) and victim perspective (snare) are both empirically justified—they are not mandatrophic confusions but accurate readings from different structural positions. The mandate (that oil supply be stable and coordination function be real) is compatible with the observation (that distribution is asymmetric and extraction occurs). The risk of mandatrophy arises if the constraint claims to be pure rope (coordination without extraction) or pure snare (extraction without coordination). The data rejects both claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_transition_cost_timing,
    'What is the true cost and timeline for global renewable energy transition at current technology deployment rates?',
    'Life-cycle assessment of renewable infrastructure; cost-of-capital comparison with fossil fuel replacement; empirical transition timelines from early-adopter nations (Denmark, Costa Rica, Paraguay)',
    'If transition cost < 10% of GDP over 30 years: scaffold sunset is achievable and the constraint is fundamentally temporary. If > 40% GDP: sunset timeline extends to civilizational scale and scaffold classification fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_transition_cost_timing, empirical, 'Cost and timeline for global renewable transition').

omega_variable(
    cartel_stability_mechanism,
    'What mechanism maintains OPEC+ cartel discipline despite price volatility, differing national interests, and production cost heterogeneity?',
    'Historical analysis of cartel defection rates, pricing variance, and punishment mechanisms; comparison with other commodity cartels (copper, aluminum); geopolitical modeling of military alliance structures within OPEC+',
    'If cartel is inherently unstable and discipline decays: oil supply becomes more volatile and extractiveness increases. If cartel discipline is self-enforcing: extraction persists at current levels indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_stability_mechanism, empirical, 'Mechanism sustaining OPEC+ production discipline').

omega_variable(
    oil_demand_elasticity_threshold,
    'At what price point does oil demand collapse due to fuel-switching, behavioral change, or economic recession?',
    'Empirical elasticity estimates from historical price shocks (1973, 1979, 2008, 2022); modeling of substitution rates for transportation (electric vehicles), heating (heat pumps), electricity (renewables); consumption pattern shifts in high-price regimes',
    'If threshold is low (demand collapses at $80-100/barrel): constraint becomes self-limiting and extraction cannot persist indefinitely. If threshold is high (demand persists to $150+/barrel): extraction capacity is much larger and coordinating function is marginal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oil_demand_elasticity_threshold, empirical, 'Price elasticity threshold for oil demand destruction').

omega_variable(
    geopolitical_constraint_heterogeneity,
    'How much of the constraint''s extraction is due to genuine scarcity vs. intentional supply restriction by geopolitically motivated actors?',
    'Comparison of actual production capacity (estimated reserves, extraction technology) with actual production volumes; analysis of production cuts during non-crisis periods (OPEC+ quota management); modeling of counterfactual supply under perfect competition',
    'If restriction accounts for > 60% of extraction: constraint is primarily political (tangled rope / snare) and could be rapidly destabilized by political realignment. If restriction < 20%: constraint is primarily physical scarcity and is more structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_constraint_heterogeneity, empirical, 'Relative contribution of scarcity vs. intentional restriction').

omega_variable(
    carbon_constraint_internalization,
    'Will carbon pricing, regulation, or climate-motivated capital reallocation materially constrain oil demand within the constraint''s biographical timescale (20-50 years)?',
    'Empirical tracking of carbon tax implementation and pricing levels; monitoring of fossil fuel divestment and capital flight; historical comparison with other phase-out constraints (leaded gasoline, CFCs, asbestos)',
    'If internalized rapidly: scaffold sunset accelerates and snare classification for locked-in nations becomes time-limited. If internalized slowly: constraint persists at current extractiveness for 50+ years.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_constraint_internalization, preference, 'Timeline for carbon constraints to materially affect oil demand').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_oil_supply_stability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glob_tr_t0, global_oil_supply_stability, theater_ratio, 0, 0.38).
narrative_ontology:measurement(glob_tr_t10, global_oil_supply_stability, theater_ratio, 10, 0.48).
narrative_ontology:measurement(glob_tr_t20, global_oil_supply_stability, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(glob_be_t0, global_oil_supply_stability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(glob_be_t10, global_oil_supply_stability, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(glob_be_t20, global_oil_supply_stability, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_oil_supply_stability, resource_allocation).
narrative_ontology:affects_constraint(global_oil_supply_stability, renewable_energy_transition_capital_bottleneck).
narrative_ontology:affects_constraint(global_oil_supply_stability, geopolitical_energy_dependence).
narrative_ontology:affects_constraint(global_oil_supply_stability, global_carbon_emissions_lock_in).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_oil_supply_stability, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
