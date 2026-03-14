% ============================================================================
% CONSTRAINT STORY: airline_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_airline_market_concentration, []).

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
 *   constraint_id: airline_market_concentration
 *   human_readable: Airline Market Concentration and Competitive Extraction
 *   domain: economic/regulatory/transportation
 *
 * SUMMARY:
 *   U.S. airline market concentration has increased dramatically over the
 *   past two decades through mergers (Delta-Northwest, United-Continental,
 *   American-US Airways), reduced low-cost carrier entry, and strategic
 *   dominance of major carriers at key hubs. This constraint exhibits the
 *   signature of a tangled rope with strong snare characteristics for
 *   passengers and competitors: genuine coordination benefits (network
 *   density, schedule reliability, cost efficiency) coexist with significant
 *   extractive mechanisms (price premiums, reduced route competition,
 *   predatory pricing on competitive routes, barriers to entry through slot
 *   control and network effects). The theatrical regulatory apparatus (DOT
 *   scrutiny of mergers, FAA slot allocation oversight) maintains the
 *   appearance of competitive oversight while actual enforcement is degraded.
 *   The constraint's extractiveness has risen over 20 years through
 *   sequential mergers and slot consolidation; theater_ratio remains moderate
 *   because the extraction mechanism (pricing power, service reduction) is
 *   economically visible, not purely procedural. However, the regulatory
 *   theater has intensified relative to actual competitive pressure.
 *
 * KEY AGENTS:
 *   - Passengers on concentrated routes (powerless/trapped) — dependent on air travel with limited alternatives, facing price premiums and reduced service options
 *   - Regional/low-cost carriers (powerless/trapped) — face barriers to entry and predatory competition from incumbents; once committed, cannot profitably exit or compete
 *   - Business travelers (moderate/constrained) — benefit from network connectivity and frequent flyer programs while paying premium fares and accepting pricing discipline
 *   - Major carriers (institutional/arbitrage) — benefit from concentration, control slot allocation, capture merger approvals, arbitrage across routes and international partnerships
 *   - Regional communities and small airports (organized/constrained) — depend on major carrier agreements for essential air service while having limited bargaining power
 *   - Department of Transportation and FAA (institutional/arbitrage) — maintain regulatory theater; capture by major carriers reduces enforcement effectiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(airline_market_concentration, 0.58).
domain_priors:suppression_score(airline_market_concentration, 0.65).
domain_priors:theater_ratio(airline_market_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(airline_market_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(airline_market_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(airline_market_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(airline_market_concentration, tangled_rope).
narrative_ontology:human_readable(airline_market_concentration, "Airline Market Concentration and Competitive Extraction").
narrative_ontology:topic_domain(airline_market_concentration, "economic/regulatory/transportation").

domain_priors:requires_active_enforcement(airline_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(airline_market_concentration, major_carriers).
narrative_ontology:constraint_beneficiary(airline_market_concentration, airport_slot_holders).
narrative_ontology:constraint_victim(airline_market_concentration, passengers).
narrative_ontology:constraint_victim(airline_market_concentration, regional_airlines).
narrative_ontology:constraint_victim(airline_market_concentration, competitors_seeking_entry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPTIVE PASSENGER (SNARE) — Passengers dependent on air travel face severely restricted choice in concentrated markets. High suppression through lack of alternatives (driving/rail uneconomical for long distances), high extractiveness through price premiums and fee structures. No meaningful exit except for essential travel choices. Maximum experienced extraction.
constraint_indexing:constraint_classification(airline_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL AIRLINE COMPETITOR (SNARE) — New entrants face barriers to entry (slot availability, fuel cost leverage, network effects of frequent flyer programs). Once committed to entry, stuck in asymmetric competition. Major carriers can absorb losses on competitive routes to drive out competitors. Trapped in predatory dynamics with no profitable exit.
constraint_indexing:constraint_classification(airline_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUSINESS TRAVELER (TANGLED ROPE) — Benefits from network connectivity and frequent flyer coordination across major carriers; constrained by the need for reliable scheduling and the ecosystem of airline partnerships. Experiences mixed extraction (higher fares) and coordination benefits (frequent flyer miles, schedule density, alliance perks). Can partially exit via shifting to video conferencing but faces career/business costs.
constraint_indexing:constraint_classification(airline_market_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR CARRIER (ROPE) — Experiences concentration as coordination mechanism: airline partnerships, codeshare agreements, and slot control enable efficient network planning and revenue management. Benefits from barriers to entry that protect market position. Can arbitrage between routes and across alliances. Sees concentration as solving a coordination problem — how to match supply and demand across complex networks while maintaining profitability.
constraint_indexing:constraint_classification(airline_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL COMMUNITIES (TANGLED ROPE) — Small airports depend on major carrier agreements for essential air service (subsidized routes). Receive coordination benefit (air connectivity) alongside extraction (control over service quality and route planning). Cannot freely exit dependence on major carriers; organized resistance possible through political pressure but constrained by economic realities. Genuine coordination function (routes would not exist without carrier commitment) coexists with asymmetric extraction (service is conditional on carrier's profitability calculations).
constraint_indexing:constraint_classification(airline_market_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — The antitrust enforcement and slot allocation system is largely performative. Mergers are reviewed but major consolidations are approved despite concentration concerns. Slot allocation at congested airports (LAX, JFK, ORD) occurs through grandfather clauses and long-term holdings rather than competitive auctions. The regulatory apparatus maintains the theater of competitive oversight while the actual enforcement is degraded — regulatory capture and bureaucratic inertia preserve the status quo. Theater ratio reflects the disconnect between stated competitive goals and actual market outcomes.
constraint_indexing:constraint_classification(airline_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NETWORK EFFECTS VIEW (MOUNTAIN) — From a universal/civilizational perspective, some degree of concentration may be inherent to airline economics: network effects (hubs benefit from denser connections), capital intensity (economies of scale in fleet and maintenance), and coordination complexity (scheduling across hundreds of daily flights) create natural barriers to entry. This perspective sees concentration as a feature of the industry's fundamental physics rather than a contingent institutional arrangement. However, the structural data contradicts this — regulatory decisions (merger approvals, slot allocation mechanisms) are contingent choices, not immutable laws. The mountain classification is a false summit that naturalizes policy artifacts as natural limits.
constraint_indexing:constraint_classification(airline_market_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(airline_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(airline_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(airline_market_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(airline_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(airline_market_concentration, TR),
    TR >= 0.70.

:- end_tests(airline_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Airlines extract through price premiums on concentrated routes (estimated 20-30% above competitive levels in some markets), ancillary fees, capacity discipline (reducing seats to maintain load factors), and reduced service on low-margin routes. The extraction is substantial but not maximal because genuine coordination benefits exist (hub efficiency, schedule density) and some competitive pressure persists (low-cost carriers, international competitors). Suppression (0.65): High. Passengers have severely restricted alternatives: long-distance driving is uneconomical for most routes, rail is underdeveloped in the U.S., video conferencing doesn't eliminate business travel necessity. Barriers to entry for competitors include slot scarcity, network effects of frequent flyer programs, fuel cost leverage by major carriers, and strategic predatory pricing. Theater ratio (0.48): Moderate. The regulatory theater is visible but not dominant because the extraction mechanism is economically transparent — passengers experience price premiums and fee structures directly. The theater manifests in merger reviews that appear rigorous but result in approvals, and slot allocation that claims competitive principles but actually operates through grandfather clauses.
 *
 * PERSPECTIVAL GAP:
 *   The constraint reveals a fundamental perspectival divide between beneficiaries and victims. Passengers and regional competitors see snare (maximum extraction, no coordination benefit perceived). Business travelers and small airports see tangled rope (mixed extraction and genuine coordination). Major carriers see rope (pure coordination mechanism that solves the problem of matching supply to demand in complex networks). Regulators see piton (their enforcement role has atrophied while the apparatus persists through theater). The analytical observer risks seeing mountain (network effects and economies of scale are inherent to airline economics), but the structural data contradicts this — regulatory decisions (merger approvals, slot allocation mechanisms) are contingent choices, not immutable laws of physics or economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Passengers experience high d (0.85-0.95) because they are trapped victims with no exit option. Competitors experience high d (0.80-0.90) because they are victims of predatory competition despite nominally being able to choose not to enter (the choice is structurally unavailable once they assess the barriers). Business travelers experience moderate d (0.55-0.65) because they benefit from network coordination but pay extraction premiums and face suppression of alternative services. Major carriers experience low d (0.10-0.25) because they are beneficiaries of concentration, extract more than they pay in coordination costs, and have arbitrage options. Regulatory agencies would experience low/negative d in an ideal competitive regime, but actual regulatory capture produces mixed effects — they benefit from industry relationships and political support while nominally bearing responsibility for enforcement. The analytical observer at civilizational scope is at risk of d ≈ 0.72 (analytical canonical) which maps to the false summit — naturalizing what is actually a contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by exposing that the debate between 'is airline concentration a natural feature of the industry?' and 'is it extractive market failure?' is not a classification question but a question about which institutions and mechanisms are changeable. The mountain perspective (concentration is inherent to airline economics) is empirically questionable — regulatory reforms (slot auctions, merger blocking, capacity discipline agreements) could reduce concentration. The snare perspective (passengers are simply extracted from) misses the genuine coordination benefits of hub-and-spoke networks. The rope perspective (from carriers' view) is real for the specific coordination problem of network planning but obscures the power asymmetry that extracts from captive passengers. The tangled rope classification is most durable because it acknowledges both the genuine coordination function (networks do require density and planning) and the genuine asymmetric extraction (passengers and competitors bear costs disproportionate to their negotiating power). The mandatrophy is resolved by recognizing that concentration solves a coordination problem (hubs) through mechanisms that also enable extraction (pricing power, entry barriers). The policy lever is whether the coordination benefits require the extractive mechanisms, or whether alternative coordination structures (auction-based slot allocation, regulated pricing on concentrated routes, capacity caps) could preserve coordination while reducing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_necessity,
    'Are airline network effects genuinely necessary (economies of scale, scheduling complexity) or are they artificially reinforced by regulatory and business model choices?',
    'Comparative analysis of airline markets with different regulatory regimes (EU slot allocation reforms, budget carrier network models, historical deregulation outcomes); econometric isolation of true economies of scale from lock-in effects',
    'If genuinely necessary: concentration is partially justified by coordination demands (tangled_rope classification more durable). If reinforced artifacts: concentration is primarily extractive (snare classification strengthens for passengers and competitors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_necessity, empirical, 'Whether network effects are inherent or artificially reinforced').

omega_variable(
    slot_allocation_alternative_feasibility,
    'Would price-based slot allocation (auctions) at congested airports enable new entrants, or would major carriers'' ability to cross-subsidize routes prevent meaningful competition?',
    'Policy simulation studies; case analysis of EU/UK slot auctions post-Brexit; historical analysis of slot allocation mechanisms and entry outcomes',
    'If auctions enable entry: primary barrier is regulatory choice (extractiveness remains but becomes politically addressable). If cross-subsidization prevents entry regardless: structural economics limit competition more than policy choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slot_allocation_alternative_feasibility, empirical, 'Feasibility of slot allocation reform to enable competition').

omega_variable(
    suppression_mechanism_structural_vs_economic,
    'Is high suppression of passenger choice due to structural lack of alternatives (no competing surface transport) or economic mechanisms (airlines artificially inflate margins to price out competition)?',
    'Comparison of markets with different surface transport availability (transcontinental US vs European short-haul where rail competes); price elasticity analysis pre/post competitive entry; international comparison of fares in concentrated vs competitive markets',
    'If structural: suppression is unavoidable coordination cost. If economic: suppression is extractive mechanism that policy could address.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_economic, empirical, 'Whether suppression is structural or economically manipulated').

omega_variable(
    merger_approval_logic,
    'Do airline merger approvals reflect genuine coordination benefits (complementary networks, cost synergies) or regulatory capture and abandonment of competitive standards?',
    'Analysis of post-merger outcomes: competition intensity, capacity discipline, route rationalization; comparison of predicted vs actual synergies; historical pattern of merger approval and subsequent market outcomes',
    'If coordination benefits real: mergers may justify tangled_rope classification despite high concentration. If capture: mergers are extractive power consolidation (snare classification for competitors/passengers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merger_approval_logic, empirical, 'Whether merger approvals reflect genuine coordination or regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(airline_market_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airline_tr_t0, airline_market_concentration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(airline_tr_t10, airline_market_concentration, theater_ratio, 10, 0.41).
narrative_ontology:measurement(airline_tr_t20, airline_market_concentration, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(airline_be_t0, airline_market_concentration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(airline_be_t10, airline_market_concentration, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(airline_be_t20, airline_market_concentration, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(airline_market_concentration, resource_allocation).
narrative_ontology:affects_constraint(airline_market_concentration, aircraft_emissions_regulation).
narrative_ontology:affects_constraint(airline_market_concentration, labor_power_imbalance_airlines).
narrative_ontology:affects_constraint(airline_market_concentration, hub_airport_capacity_limits).

% DUAL FORMULATION NOTE:
% Airline market concentration decomposes into three structurally distinct constraint stories with different epsilon values: (1) pricing power and route entry barriers (this story, ε=0.58, tangled_rope), (2) fleet coordination and schedule complexity (ε≈0.30, rope — genuine coordination with minimal extraction), (3) airport capacity bottleneck (ε≈0.70, snare — passengers and airlines trapped by scarcity). This story focuses on market concentration as an extraction mechanism. The fleet coordination constraint exists independently; the capacity bottleneck would persist even with market fragmentation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(airline_market_concentration, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
