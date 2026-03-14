% ============================================================================
% CONSTRAINT STORY: regional_airline_consolidation_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_airline_consolidation_pressure, []).

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
 *   constraint_id: regional_airline_consolidation_pressure
 *   human_readable: Regional Airline Consolidation Pressure
 *   domain: economic/aviation/competition
 *
 * SUMMARY:
 *   Regional airline consolidation pressure arises from a structural
 *   asymmetry in the US aviation system: major carriers control hub airports
 *   and international routes through legacy slot allocations and network
 *   advantages, while regional carriers depend entirely on feed contracts
 *   with these majors for route access and passenger supply. This dependency
 *   creates a extraction mechanism where legacy carriers progressively
 *   pressure regional operators to accept lower margins, accept capacity
 *   commitments that exceed profitable demand, absorb fuel cost volatility,
 *   and ultimately either accept acquisition or cease operations. The
 *   constraint is 'tangled' because genuine coordination function exists
 *   (regional aircraft efficiently serve secondary markets and feed hub
 *   networks) alongside asymmetric extraction (legacy carriers capture the
 *   coordination surplus). Base extractiveness has grown from 0.35 (2015) to
 *   0.58 (2025) as fuel costs, labor shortages, and aircraft financing have
 *   compressed regional margins while legacy carriers have consolidated
 *   further. The scaffold perspective from low-cost carriers represents a
 *   real sunset mechanism: point-to-point networks, secondary airport
 *   development, and all-electric regional aircraft (in development) could
 *   dissolve the hub-dependency structure within 10-20 years.
 *
 * KEY AGENTS:
 *   - Regional Airline Operators: Primary victims (powerless/trapped) — financially dependent on feed contracts; face margin compression, capacity mandates, and acquisition pressure
 *   - Secondary Market Communities: Secondary victims (moderate/constrained) — depend on regional connectivity; face route abandonment risk and price increases as regional carriers consolidate
 *   - Major Legacy Carriers (United, Delta, American): Primary beneficiaries (institutional/arbitrage) — extract capacity coordination surplus; can shift between owned-operated and partner-operated aircraft; control hub slot allocation
 *   - Airport Hub Operators: Secondary beneficiary (institutional/arbitrage) — benefit from major carrier network concentration; maintain slot allocation authority
 *   - Low-Cost Carriers (Southwest, Allegiant, Frontier): Organized challengers (organized/mobile) — represent alternative business model that bypasses regional consolidation constraint through secondary airports and point-to-point routing
 *   - Aircraft Manufacturers (Boeing/Airbus): Mixed institutional actor (powerful/mobile) — benefit from consolidation-driven regional aircraft orders but face margin pressure on regional specs
 *   - DOT Capacity/Slot System: Institutional degraded actor (institutional/arbitrage) — slot allocation nominally public but functionally captured by legacy carriers; maintenance through performative regulation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy and network arrangements as inevitable physics of aviation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_airline_consolidation_pressure, 0.58).
domain_priors:suppression_score(regional_airline_consolidation_pressure, 0.65).
domain_priors:theater_ratio(regional_airline_consolidation_pressure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_airline_consolidation_pressure, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_airline_consolidation_pressure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regional_airline_consolidation_pressure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_airline_consolidation_pressure, tangled_rope).
narrative_ontology:human_readable(regional_airline_consolidation_pressure, "Regional Airline Consolidation Pressure").
narrative_ontology:topic_domain(regional_airline_consolidation_pressure, "economic/aviation/competition").

domain_priors:requires_active_enforcement(regional_airline_consolidation_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_airline_consolidation_pressure, major_legacy_carriers).
narrative_ontology:constraint_beneficiary(regional_airline_consolidation_pressure, airport_hub_operators).
narrative_ontology:constraint_victim(regional_airline_consolidation_pressure, regional_airline_operators).
narrative_ontology:constraint_victim(regional_airline_consolidation_pressure, secondary_market_communities).
narrative_ontology:constraint_victim(regional_airline_consolidation_pressure, regional_route_connectivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A regional carrier cannot exit without ceasing operations. Trapped by structural dependency on major carriers' feed networks, fuel cost volatility, and CRJ-900 aircraft financing. Bears full extraction through capacity requirements contracts, fuel surcharges, and slot restrictions. No alternative route to market access.
constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Communities served by regional carriers experience both coordination benefit (connectivity to hub networks) and extraction (price increases, route abandonment risk, reduced schedule frequency). High switching costs and geographic constraints limit exit but not immutably.
constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Legacy carriers benefit from regional feed networks with minimal coordination cost from their perspective. Can arbitrage regional capacity by shifting between owned-operated and partner-operated aircraft. Experience the constraint as pure coordination mechanism for hub-and-spoke leverage.
constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% Southwest, Allegiant, and low-cost carriers experience consolidation pressure as a temporary barrier that is being dissolved by alternative business models: point-to-point routing, secondary airports, all-Boeing fleets. Sunset mechanism: as fuel efficiency improves and secondary airport infrastructure develops, the regional consolidation constraint loses force.
constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% Runway capacity controls at major hubs (ORD, DCA, ATL, LGA) are theoretically allocated by DOT to prevent congestion. In practice, these slots are captured and defended by major carriers; the allocation mechanism persists as a performative gate while the real allocation is done through legacy carrier market power and regional feed contracts. Theater ratio reflects that slots are nominally public but functionally privatized.
constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Aircraft manufacturers benefit from consolidation through increased CRJ and narrow-body aircraft orders from expanding legacy carriers, but also face extraction through fixed regional specs, warranty obligations, and aftermarket service requirements. Experience both coordination (standardized fleets) and asymmetric extraction (margin pressure on regional-sized jets).
constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% From a civilizational perspective, airline consolidation is framed as an inevitable response to fuel costs, labor availability, and network economics — a natural law of aviation. However, this naturalizes policy choices (slot allocation, foreign ownership restrictions, labor regulation) that could be restructured. The mountain classification is a false summit indicating naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_airline_consolidation_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_airline_consolidation_pressure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_airline_consolidation_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_airline_consolidation_pressure, TR),
    TR >= 0.70.

:- end_tests(regional_airline_consolidation_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. Regional carriers face margin compression from fuel costs, labor competition, and aircraft financing, but the primary extraction mechanism is the feed contract structure itself — legacy carriers can demand capacity commitments, frequency requirements, and yield management terms that lock regional partners into low-margin operations. The measurement trajectory (0.35 → 0.58) reflects both genuine structural cost increases and progressive intensification of extraction through contract renegotiation. Theater ratio (0.48): Moderate. DOT slot allocation and foreign ownership restrictions create procedural theater (appear to govern carrier relationships when they actually reflect legacy carrier market power), but the primary coordination function (feed networks, capacity efficiency) is functionally real, not purely theatrical. Theater ratio increases slightly (0.38 → 0.48) as regulatory workarounds and contractual complexity increase without resolving the underlying asymmetry. Suppression (0.65): High. Regional operators face structural barriers to exit: aircraft financing requires major carrier contracts for revenue certainty; slot access at major hubs is controlled by legacy carriers; international routes are closed to regional operators. However, suppression is not absolute — some independent regional operators exist (Horizon, SkyWest); Southwest demonstrates that point-to-point routing is an alternative business model. The suppression reflects concentrated power, not physical impossibility.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. The legacy carrier sees a rope (efficient network coordination with minimal overhead). The regional operator sees a snare (trapped in low-margin feed dependency with no exit). The low-cost carrier sees a scaffold (temporary barrier being dissolved by alternative models). The secondary market community sees tangled rope (benefits from connectivity but extracted from by margin pressure). The DOT slot system sees a piton (allocated by regulation but functionally captured, maintained through policy inertia). The analytical observer risks seeing a mountain (inevitable consolidation) when the evidence points to a tangled rope with a sunset mechanism. The perspectival gap reveals that the constraint is not a single natural phenomenon but a set of institutional arrangements (slot allocation, feed contracting, foreign ownership rules, labor regulation) that could be restructured.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Legacy carriers as beneficiaries with arbitrage options (can exit through acquisition or rebalancing) experience low d → negative effective extraction χ. Regional operators as victims with trapped exit (cannot exit without ceasing to exist) experience high d → high f(d) → high χ. Low-cost carriers as organized agents with mobile exit (can operate through alternative business models) experience moderate d. Secondary market communities as constrained victims experience moderate-to-high d depending on alternate transportation options. The directionality derivation correctly predicts that legacy carriers perceive coordination (rope) while regional operators perceive extraction (snare) — the same structural arrangement produces opposite perceptual classifications based on exit capacity and beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three tangled rope gates: (1) genuine coordination function exists (regional aircraft efficiently serve secondary markets and feed hub networks — this is real network efficiency, not pure theater); (2) asymmetric extraction exists (regional operators are locked into below-cost margin contracts while majors extract the coordination surplus); (3) active enforcement is required (legacy carriers must continuously negotiate and renegotiate feed contracts to maintain the arrangement). The mandatrophy is not 'is this coordination or extraction?' but 'what portion of the arrangement is real coordination versus extractive leverage?' The measurement trajectory shows increasing extractiveness (0.35 → 0.58) while theater ratio stays moderate (0.38 → 0.48), indicating that the coordination function remains real (low theater) but extraction has intensified due to structural cost pressures. The scaffold perspective from low-cost carriers confirms the sunset mechanism — alternative business models (point-to-point, secondary airports, all-electric aircraft) can replicate the coordination function without the asymmetric extraction. The constraint is not permanently tangled but tangled under current institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regional_profitability_threshold,
    'What combination of fuel costs, labor wages, and aircraft financing makes a regional route economically unsustainable?',
    'Time-series analysis of regional airline profitability against cost inputs; cross-carrier operational metrics; synthetic cost modeling with varied policy parameters',
    'If threshold is primarily structural (fuel/physics): consolidation pressure is partially inevitable. If threshold is primarily policy-driven (labor rules, slot allocation): consolidation pressure could be reversed through regulatory restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_profitability_threshold, empirical, 'Economic viability threshold for regional airline operations').

omega_variable(
    hub_network_necessity,
    'Is the hub-and-spoke model necessary for efficient route coverage, or is it primarily a legacy carrier strategy to capture secondary market connectivity?',
    'Comparative analysis of point-to-point networks (Southwest, Frontier) vs hub-dependent networks (United, Delta); cost-structure modeling; community connectivity outcomes under different architectures',
    'If necessary: regional consolidation reflects real network efficiency. If strategic: consolidation is extractive leveraging of legacy network advantages. Classification shifts from Tangled Rope (necessary coordination) to Snare (extractive gatekeeping) from regional carrier perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hub_network_necessity, empirical, 'Whether hub-and-spoke architecture is technically necessary or strategically chosen').

omega_variable(
    secondary_airport_viability,
    'Can secondary airports (Midway, Oakland, Love Field) develop sufficient connecting flight volume to serve as genuine alternatives to major hubs?',
    'Operational data on secondary airport cargo/passenger volumes; infrastructure investment trends; route profitability at secondary vs primary airports',
    'If secondary airports can scale: scaffold perspective confirmed — low-cost carrier sunset mechanism is real. If not: scaffold is aspirational and consolidation pressure persists longer than 20-year horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_airport_viability, empirical, 'Scalability of secondary airports as consolidation alternatives').

omega_variable(
    foreign_ownership_constraint_binding,
    'How much of the regional consolidation pressure derives from the foreign ownership restrictions (49% non-US) that prevent global carriers from directly acquiring regional operators?',
    'Counterfactual analysis: modeling regional carrier valuations under liberalized foreign ownership vs current restrictions; historical comparison to countries with different ownership rules',
    'If binding: removing ownership restrictions would enable alternative consolidation paths (global carrier acquisition vs legacy carrier absorption). This shifts the constraint from purely structural to policy-dependent, making it a Scaffold with explicit policy sunset rather than market-driven sunset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreign_ownership_constraint_binding, conceptual, 'Binding effect of foreign airline ownership restrictions on consolidation dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_airline_consolidation_pressure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rac_tr_t0, regional_airline_consolidation_pressure, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rac_tr_t5, regional_airline_consolidation_pressure, theater_ratio, 5, 0.43).
narrative_ontology:measurement(rac_tr_t10, regional_airline_consolidation_pressure, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(rac_be_t0, regional_airline_consolidation_pressure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rac_be_t5, regional_airline_consolidation_pressure, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rac_be_t10, regional_airline_consolidation_pressure, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_airline_consolidation_pressure, resource_allocation).
narrative_ontology:affects_constraint(regional_airline_consolidation_pressure, hub_airport_slot_allocation).
narrative_ontology:affects_constraint(regional_airline_consolidation_pressure, international_route_carrier_licensing).
narrative_ontology:affects_constraint(regional_airline_consolidation_pressure, regional_aircraft_manufacturing_margins).

% DUAL FORMULATION NOTE:
% Regional airline consolidation pressure is downstream of structural choices in hub airport slot allocation and foreign ownership restrictions. The consolidation pressure could be decomposed into three distinct constraints: (1) slot allocation asymmetry (how hubs are governed), (2) feed contract extraction (how regional operators are leveraged), and (3) aircraft financing dependency (how regional equipment is financed). This story treats consolidation as a unified phenomenon but acknowledges the causal upstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_airline_consolidation_pressure, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
