% ============================================================================
% CONSTRAINT STORY: airport_capacity_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_airport_capacity_constraint, []).

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
 *   constraint_id: airport_capacity_constraint
 *   human_readable: Airport Capacity Constraint and Slot Allocation
 *   domain: transportation/infrastructure/economic
 *
 * SUMMARY:
 *   Airport capacity constraints at major hub airports create a structural
 *   tension between physical scarcity (runways can only handle so many
 *   operations per hour) and regulatory allocation (slot grandfathering
 *   systems that privilege incumbent carriers over new entrants and
 *   underserved routes). The constraint exhibits characteristics of multiple
 *   DR types depending on observer position: a snare for new airlines and
 *   travelers denied choice, a tangled rope mixing genuine coordination
 *   (preventing runway gridlock) with extraction (incumbent protection), a
 *   rope for legacy carriers experiencing it as pure coordination, a scaffold
 *   for liberalization movements seeking to replace grandfathering with
 *   dynamic pricing, a piton reflecting institutional inertia in slot
 *   allocation rules, and a potential false summit from the natural scarcity
 *   perspective. The actual enforcement mechanism (FAA slot rules, EU slot
 *   regulation, bilateral air service agreements) layered atop physical
 *   bottlenecks creates measurable extraction. Base extractiveness has risen
 *   from 0.38 (when hub consolidation was less severe) to 0.52 (current
 *   state) over 30 years as network effects concentrate traffic at major hubs
 *   and incumbent market share compounds through slot accumulation. Theater
 *   ratio remains moderate (0.38) because slot allocation serves genuine
 *   safety/coordination functions, but the ratio has increased slightly as
 *   the performative aspects of grandfathering (justifying inherited rights
 *   through operational necessity claims) have become more prominent relative
 *   to actual safety coordination.
 *
 * KEY AGENTS:
 *   - Legacy Carriers (institutional/arbitrage): Primary beneficiaries — own grandfathered slots, use them as competitive moat, extract rent through secondary market sales to new entrants
 *   - New Entrant Airlines (powerless/trapped): Primary victims — face multi-year slot acquisition timelines, pay premium prices in secondary markets, effectively excluded from major hubs
 *   - Airport Authorities (institutional/constrained): Enforcer role — coordinate actual ground operations, absorb pressure from all sides, have nominal authority over slot allocation but limited real power against FAA mandates and bilateral agreements
 *   - Air Travelers in Underserved Markets (powerless/trapped): Victims of indirect extraction — limited route options, higher fares, poor connectivity because resources concentrated on profitable hub-to-hub routes served by incumbents
 *   - Regional Carriers (moderate/constrained): Mixed position — some benefit from feed agreements with legacy carriers, some trapped by inability to compete for hub slots
 *   - EU/ICAO Liberalization Movement (organized/constrained): Working to sunset the constraint through open skies protocols, dynamic capacity management, and pricing mechanisms
 *   - Regulatory Bodies (institutional/arbitrage): FAA, EU Commission, bilateral aviation negotiators — maintain the system despite internal recognition of its extractive properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(airport_capacity_constraint, 0.52).
domain_priors:suppression_score(airport_capacity_constraint, 0.65).
domain_priors:theater_ratio(airport_capacity_constraint, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(airport_capacity_constraint, extractiveness, 0.52).
narrative_ontology:constraint_metric(airport_capacity_constraint, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(airport_capacity_constraint, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(airport_capacity_constraint, tangled_rope).
narrative_ontology:human_readable(airport_capacity_constraint, "Airport Capacity Constraint and Slot Allocation").
narrative_ontology:topic_domain(airport_capacity_constraint, "transportation/infrastructure/economic").

domain_priors:requires_active_enforcement(airport_capacity_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(airport_capacity_constraint, legacy_carriers).
narrative_ontology:constraint_beneficiary(airport_capacity_constraint, airport_operators).
narrative_ontology:constraint_beneficiary(airport_capacity_constraint, incumbent_airlines).
narrative_ontology:constraint_victim(airport_capacity_constraint, new_entrant_airlines).
narrative_ontology:constraint_victim(airport_capacity_constraint, air_travelers_denied_choice).
narrative_ontology:constraint_victim(airport_capacity_constraint, underserved_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERSERVED AIR TRAVELER (SNARE) — Cannot exit the constraint. Limited routing options, high prices, no alternative transportation for long distances. Slot scarcity concentrated at hub airports means routes to secondary cities are subsidized only when convenient for legacy carriers. Experiences maximum extraction with minimal alternative.
constraint_indexing:constraint_classification(airport_capacity_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEW ENTRANT AIRLINE (SNARE) — Structurally trapped by slot allocation rules that grandfather incumbents. Cannot acquire meaningful capacity at congested hub airports without multi-year waitlists or purchasing from existing carriers at inflated secondary market prices. Exit would require abandonment of market; presence requires accepting subordinate position indefinitely.
constraint_indexing:constraint_classification(airport_capacity_constraint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL AIRLINE OPERATOR (TANGLED ROPE) — Experiences mixed constraint. Genuine coordination function: slot allocation system prevents total chaos and runway collisions; enables route planning and scheduling. Simultaneously extracted from through priority schemes favoring majors and maintenance fees. High cost to exit (abandonment of developed routes) but some agency through feed partnerships and regional exemptions.
constraint_indexing:constraint_classification(airport_capacity_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LEGACY CARRIER (ROPE) — Primary beneficiary experiencing the constraint as pure coordination mechanism. Slot grandfathering (historical priority rights) provides stable, predictable capacity allocation. Coordinates network planning with minimal competition for premium slots. Experiences constraint as enabling efficient operations, not as extraction.
constraint_indexing:constraint_classification(airport_capacity_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AIRPORT AUTHORITY (TANGLED ROPE) — Genuine coordination benefit: slot allocation prevents gridlock and unsafe overcrowding. Simultaneously extracted as enforcer — FAA mandates compliance, airlines game system through false declarations of operational requirements, airport absorbs coordination costs. Has agency through slot redesign but faces political pressure from legacy carrier coalition.
constraint_indexing:constraint_classification(airport_capacity_constraint, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL LIBERALIZATION COALITION (SCAFFOLD) — EU sky liberalization and ICAO open skies protocols represent sunset mechanisms: capacity constraints are framed as temporary coordination problems to be solved through infrastructure investment, dynamic pricing, and demand management rather than permanent slot grandfathering. Sunset logic: as capacity grows and pricing mechanisms mature, slot scarcity extraction declines. Current suppression high but declining as alternatives mature.
constraint_indexing:constraint_classification(airport_capacity_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: SLOT GRANDFATHER RIGHTS (PITON) — Theater ratio 0.38 reflects that grandfathered slot rights are largely performative allocation mechanism where the real function (preventing runway collisions) could be served by more efficient methods (dynamic pricing, market-clearing auctions, real-time traffic management). The ritual persists through institutional path dependency — legacy carriers fought deregulation and won regulatory entrenchment. Slot allocation ceremony maintains political legitimacy while actual allocation increasingly reflects informal power, not rules.
constraint_indexing:constraint_classification(airport_capacity_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL SCARCITY VIEW (MOUNTAIN) — From civilizational scale, runway capacity is a genuine physical limit: only so many aircraft can use a runway safely per hour. Queuing theory shows bottleneck formation is inevitable when demand exceeds capacity. Constraint appears immutable by nature. Engine will identify this as potential false summit — the constraint's extractive properties derive from allocation rules, not physical law. Scarcity is real; extraction through grandfathering is contingent.
constraint_indexing:constraint_classification(airport_capacity_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(airport_capacity_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(airport_capacity_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(airport_capacity_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(airport_capacity_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(airport_capacity_constraint, TR),
    TR >= 0.70.

:- end_tests(airport_capacity_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts measurable benefits to incumbents through slot value (estimated at $2M-$5M per daily slot pair at major US hubs). New entrants pay this premium; travelers bear it through higher fares. The extraction is sustained through regulatory enforcement (FAA mandatory slot compliance) and through secondary market mechanisms that commodify grandfathered rights. Extractiveness is NOT at snare levels (0.66+) because: (a) some coordination value is genuine (runway safety, schedule predictability), and (b) infrastructure alternatives exist in principle (though politically blocked). Suppression (0.65): High. Barriers to entry are substantial: slot acquisition costs measured in hundreds of millions, regulatory timelines of 2-5 years per slot pair, network effects that make single-route entry unviable. However, suppression is not absolute — new entrants do enter (Southwest, regional carriers), but only through feed partnerships or secondary market purchases. Barriers have INCREASED over the measurement interval as hub consolidation has concentrated slot value. Theater ratio (0.38): Moderate. The constraint performs genuine coordination function (runway utilization optimization, collision prevention through coordinated scheduling). But performance is increasingly performative: actual runway capacity utilization at major US hubs is 70-85%, suggesting slack exists; sophisticated air traffic management could support dynamic allocation. The grandfathering ritual persists partly through political inertia rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Legacy carriers (Rope) see pure coordination and stability. New entrants (Snare) see pure extraction and imprisonment. Airport operators (Tangled Rope) see mixed function — coordination burden they must enforce alongside extraction occurring through their system. Liberalization movements (Scaffold) see temporary problem with sunset mechanism (open skies adoption). The slot grandfathering institution itself (Piton) sees its own process as degraded — slot allocation ceremonies (IATA conferences, FAA meetings) have increasingly performative character. The natural scarcity framing (Mountain) risks naturalizing what is actually a political-regulatory choice: US hubs could expand capacity, EU hubs deploy dynamic allocation, but incumbent lobbying blocks both. The perspectival gap reveals the structural question: how much of experienced scarcity is physical (runway throughput floor) vs. regulatory (slot grandfathering choice) vs. political-economic (incumbent collective action blocking expansion)?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each actor's structural position. Legacy carriers with arbitrage exit options (can reallocate slots to more profitable routes, sell excess capacity) experience low d (d~0.15) → low χ. New entrants with trapped exit (must acquire slots or exit market entirely) experience high d (d~0.85) → high χ. The airport authority with constrained exit (must comply with FAA mandates, cannot simply opt out) experiences moderate d (d~0.60). Regional carriers with constrained exit but some partnership options experience moderate d (d~0.55). The new liberalization coalition with constrained exit (organizational but not individual agent level) experiences moderate d (d~0.58). Directionality shifts over the measurement interval: as hub consolidation deepens, incumbent exit options improve (flexibility to shift slots, market depth to sell), while new entrant exit options worsen (fewer alternative hubs with available capacity). This creates increasing d-spread and rising χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that all classifications describe real structural features from their respective observer positions. The mandatrophy question is NOT 'which type is right?' but 'whose constraint is it?' The answer differs by position: it is a Snare for trapped agents, a Rope for beneficiaries, a Tangled Rope for enforcers, a Scaffold for liberalization movements, and a false summit from the natural scarcity view. The analytical edge case is whether the system should be re-engineered (open skies sunset) or maintained (natural scarcity acceptance). Current regulatory framework chooses maintenance despite rising social costs. The false summit detector will flag the Mountain perspective as naturalization of a contingent choice: runway capacity is physical, but the extraction mechanism is regulatory. Resolving mandatrophy requires distinguishing between: (1) Real coordination value (preventing gridlock): ~25% of suppression justified, (2) Legitimate temporary scarcity pending expansion: ~15% justified, (3) Political incumbency protection via grandfathering: ~60% extractive, not justified by coordination or natural scarcity. The high-extractiveness measurement (0.52) reflects that category 3 dominates — the constraint's primary function has shifted from coordination to incumbent rent protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_regulatory_scarcity,
    'Is the constraint-experienced scarcity fundamentally physical (runway capacity is an immutable floor) or primarily regulatory (slot grandfathering artificially constrains available capacity)?',
    'Empirical comparison of actual runway utilization at constrained airports vs theoretical maximum utilization under dynamic pricing or market-clearing allocation. Analysis of capacity expansion history and slot creation feasibility.',
    'If physical: Mountain classification justified. If regulatory: Snare/Tangled Rope extraction mechanism is engineered choice, not natural limit. Could reduce extractiveness 0.52 → 0.28 through reallocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_regulatory_scarcity, empirical, 'Whether scarcity is physical or regulatory').

omega_variable(
    secondary_slot_market_efficiency,
    'Does the secondary slot market (leasing, sales among carriers) function as an efficient price discovery mechanism or as a rent-extraction vehicle for incumbents?',
    'Analysis of secondary market pricing vs marginal cost of operations, correlation between slot prices and route profitability, impact on new entrant participation rates.',
    'If efficient: secondary market legitimizes grandfathering as price-discovery mechanism. If rent-extractive: secondary market amplifies extraction by commodifying access, raising barrier for new entrants from grandfathered rights to grandfathered rights + market premium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_slot_market_efficiency, empirical, 'Efficiency of secondary slot market').

omega_variable(
    infrastructure_investment_counterfactual,
    'Would additional runway/terminal capacity be economically justified if the benefits of expanded choice and new-entrant competition were internalized?',
    'Cost-benefit analysis of expansion: compare capital cost against benefits to new entrants, reduced fares from competition, improved connectivity to underserved markets, aviation demand elasticity.',
    'If expansion justified: current allocation of finite capacity reflects political choices favoring incumbents over infrastructure investment. Constraint is then partially artificial. If expansion not justified: physical scarcity is binding and grandfathering is second-best allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_investment_counterfactual, empirical, 'Economic justification for infrastructure expansion').

omega_variable(
    international_liberalization_sunset_pace,
    'Will EU-style open skies and dynamic slot allocation expand globally fast enough to obsolete grandfathering before incumbent lock-in becomes permanently embedded?',
    'Tracking of regulatory reforms: ICAO recommendations adoption rates, bilateral air service agreements, domestic deregulation timelines in major aviation markets, technology deployment (dynamic air traffic management) readiness.',
    'If fast (5-10 year horizon): Scaffold perspective is descriptively accurate, extractiveness will decline. If slow (20+ years): extractiveness persists, scaffold is aspirational, constraint mutates toward Piton rather than Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_liberalization_sunset_pace, empirical, 'Pace of international open skies liberalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(airport_capacity_constraint, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airpt_tr_t0, airport_capacity_constraint, theater_ratio, 0, 0.32).
narrative_ontology:measurement(airpt_tr_t10, airport_capacity_constraint, theater_ratio, 10, 0.35).
narrative_ontology:measurement(airpt_tr_t20, airport_capacity_constraint, theater_ratio, 20, 0.38).
narrative_ontology:measurement(airpt_tr_t30, airport_capacity_constraint, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(airpt_be_t0, airport_capacity_constraint, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(airpt_be_t10, airport_capacity_constraint, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(airpt_be_t20, airport_capacity_constraint, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(airpt_be_t30, airport_capacity_constraint, base_extractiveness, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(airport_capacity_constraint, resource_allocation).
narrative_ontology:boltzmann_floor_override(airport_capacity_constraint, 0.2).
narrative_ontology:affects_constraint(airport_capacity_constraint, airline_consolidation_lock_in).
narrative_ontology:affects_constraint(airport_capacity_constraint, hub_and_spoke_network_dependency).

% DUAL FORMULATION NOTE:
% Airport capacity constraint is upstream of airline market consolidation — grandfathered slots enable hub control, which enables consolidation, which reinforces need for hub capacity. Decomposed into separate stories for physical scarcity (Mountain), regulatory allocation (Tangled Rope), and secondary market dynamics (Snare for new entrants). See network linkage documentation for interaction effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(airport_capacity_constraint, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
