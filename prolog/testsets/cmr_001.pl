% ============================================================================
% CONSTRAINT STORY: cmr_001
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cmr_001, []).

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
 *   constraint_id: cmr_001
 *   human_readable: Critical Minerals Reserve
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The US Critical Minerals Reserve represents a geopolitical response to
 *   perceived supply chain vulnerability in strategic minerals (rare earths,
 *   cobalt, lithium, nickel). Established as a $12 billion program to
 *   stockpile and develop domestic production capacity, the reserve embodies
 *   a tension between genuine coordination (securing supply for national
 *   defense) and extraction (redistributing costs to downstream manufacturers
 *   and global commodity markets). The constraint exhibits six distinct
 *   classifications depending on the observer's structural position: defense
 *   contractors experience coordination (rope), domestic miners experience
 *   temporary support with eventual integration (scaffold), global supply
 *   chains experience pure extraction (snare), downstream manufacturers
 *   experience mixed constraints (tangled rope), the strategic resource
 *   bureaucracy experiences its own degraded ritual (piton), and the
 *   analytical observer risks naturalizing geopolitical concentration as
 *   immutable scarcity (false mountain). The theater ratio reflects that much
 *   reserve activity is performative announcement and stockpile rotation
 *   rather than actual supply diversification or technological substitution.
 *
 * KEY AGENTS:
 *   - Defense Contractors and Strategic Industries: Primary beneficiaries (institutional/arbitrage) — receive guaranteed supply access, price locks, and procurement priority
 *   - Global Supply Chains: Primary victims (powerless/trapped) — face rising mineral prices, allocation uncertainty, and cannot exit supply constraints
 *   - Domestic Mining and Processing Sectors: Secondary beneficiary (organized/constrained) — receive preferential purchasing and development support; constrained by technological and capital scaling
 *   - Downstream Manufacturers (US-based): Secondary victim (moderate/constrained) — benefit from domestic supply security but face higher material costs and competitive disadvantage
 *   - Strategic Resource Bureaucracy: Institutional actor (institutional/arbitrage) — maintains Cold War-era supply security doctrine; maintains reserve through performative metrics
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent geopolitical concentration as immutable resource scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cmr_001, 0.52).
domain_priors:suppression_score(cmr_001, 0.65).
domain_priors:theater_ratio(cmr_001, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cmr_001, extractiveness, 0.52).
narrative_ontology:constraint_metric(cmr_001, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cmr_001, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cmr_001, tangled_rope).
narrative_ontology:human_readable(cmr_001, "Critical Minerals Reserve").
narrative_ontology:topic_domain(cmr_001, "economic/geopolitical").

domain_priors:requires_active_enforcement(cmr_001).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cmr_001, domestic_defense_contractors).
narrative_ontology:constraint_beneficiary(cmr_001, strategic_manufacturing_sectors).
narrative_ontology:constraint_beneficiary(cmr_001, domestic_mining_operators).
narrative_ontology:constraint_victim(cmr_001, supply_chain_flexibility).
narrative_ontology:constraint_victim(cmr_001, global_commodity_markets).
narrative_ontology:constraint_victim(cmr_001, downstream_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SUPPLY CHAIN (SNARE) — Downstream manufacturers dependent on stable, low-cost mineral access face rising prices and allocation uncertainty. Trapped by supply constraints they cannot control. The reserve mechanism creates artificial scarcity in global markets, forcing price adjustments that redistribute wealth upward. No exit option: manufacturers cannot switch to alternative supply without massive retooling.
constraint_indexing:constraint_classification(cmr_001, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC DOWNSTREAM MANUFACTURERS (TANGLED ROPE) — Benefit from domestic supply security (coordination function) but face higher material costs and allocation restrictions (extraction). Can shift suppliers or materials over time, but face switching costs and competitive disadvantage vs international producers with cheaper minerals. Constrained exit: dependency on domestic supply framework.
constraint_indexing:constraint_classification(cmr_001, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE CONTRACTORS & STRATEGIC INDUSTRIES (ROPE) — Primary beneficiaries. Receive guaranteed domestic supply, price-locked access, and strategic priority. Experience the reserve as pure coordination mechanism: solving collective action problem of secure supply chains. Can arbitrage between domestic reserve pricing and international markets. Net beneficiary position.
constraint_indexing:constraint_classification(cmr_001, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOMESTIC MINING & PROCESSING (SCAFFOLD) — See the reserve as temporary support structure enabling transition from import-dependent to domestic-competitive production. Extraction exists (preferential purchasing, subsidized access) but sunset logic applies: as domestic production capacity matures over 15-20 years, the reserve's protective function declines and competitive pressures return. High suppression during transition (competitors cannot access cheap international alternatives), but suppression declines as domestic supply matures.
constraint_indexing:constraint_classification(cmr_001, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STRATEGIC RESOURCE DOCTRINE (PITON) — From the civilizational view, the reserve embodies a Cold War framing of resource security as a strategic competition problem. The doctrine persists through institutional inertia (defense procurement regulations, national security exemptions to trade law) despite changing geopolitical reality. Theater ratio reflects that much reserve activity is performative: announcing reserve capacity, rotating stockpiles, reporting readiness metrics — activities that signal strategic intent more than they enhance actual supply resilience. Degraded function (reserve cannot substitute for true supply chain resilience) maintained by bureaucratic theater.
constraint_indexing:constraint_classification(cmr_001, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From a universal perspective, some concentration of mineral resources by geography is a natural fact: rare earth elements cluster in specific geological formations; processing capacity has structural economies of scale. This perspective risks seeing supply-chain concentration as immutable natural law rather than as a contingent outcome of historical trade patterns and investment choices. Engine's false summit detector should flag this naturalization.
constraint_indexing:constraint_classification(cmr_001, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cmr_001_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cmr_001, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cmr_001, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cmr_001, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cmr_001, TR),
    TR >= 0.70.

:- end_tests(cmr_001_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reserve creates artificial scarcity in global mineral markets, raising prices for all consumers except domestic preferred sectors. Defense contractors and strategic industries benefit through preferential access and price locks; downstream manufacturers and global supply chains bear costs. The extraction is substantial but not maximal because: (1) the reserve volume is limited relative to global consumption, (2) technological substitution and recycling may reduce dependency, (3) market mechanisms can partially deflect the scarcity mechanism. Suppression (0.65): Moderate-high. Trade restrictions on competing suppliers, preferential procurement rules, and strategic minerals designation create significant barriers to alternative suppliers. Downstream manufacturers face real switching costs and competitive disadvantage. Global markets cannot freely access minerals at production cost. However, suppression is not absolute: black markets, recycling, substitution, and diplomatic pressure can work around reserve constraints. Theater ratio (0.58): Moderate. Reserve activities include substantial performative elements — announcing reserve purchases, rotating stockpiles for readiness reports, celebrating capacity milestones — that signal strategic intent more than enhance actual supply resilience. Yet there is real functional content: physical inventory does exist, strategic pricing mechanisms do operate, and some genuine supply chain strengthening occurs. The ratio reflects mixed purpose: half-real coordination function, half-institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reveals how a single institutional mechanism distributes benefits and costs asymmetrically across structural positions. Defense contractors (institutional beneficiaries with arbitrage exit) see pure coordination — the reserve solves their supply problem. Global supply chains (powerless victims with trapped exit) see pure extraction — mineral costs rise, supply uncertainty increases, no alternative exists. Domestic miners (organized actors with constrained exit) see temporary support enabling transition — they benefit from preferential purchasing but face declining margins as capacity matures. Downstream manufacturers (moderate victims with constrained exit) experience tangled rope — they benefit from domestic supply security (coordination) but pay higher costs (extraction) and cannot easily switch to international suppliers. The strategic resource bureaucracy (institutional actors with arbitrage) maintains the reserve through Cold War doctrine, seeing it as degraded but necessary institutional ritual (piton). The analytical observer risks naturalizing the entire structure as immutable scarcity (false mountain), failing to distinguish between geological facts (mineral concentration) and contingent choices (investment patterns, trade policy, industrial structure).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's relationship to the extraction flow and their structural capacity to exit. Defense contractors benefit directly (d ≈ 0.05, low extraction experienced). Global supply chains bear costs with no exit option (d ≈ 0.95, maximum extraction). Domestic miners benefit from preferential access but face capacity constraints (d ≈ 0.40, moderate extraction). Downstream manufacturers are mixed — they benefit from security but pay higher costs and have limited switching options (d ≈ 0.60, moderate extraction). The strategic bureaucracy experiences the reserve as performative maintenance of institutional structure (d ≈ 0.25, low extraction but high theater). The analytical observer's directionality depends on whether they distinguish contingent policy choices from immutable natural constraints (naturalization error produces false mountain).
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint does not yet resolve whether the reserve is primarily a coordination mechanism (rope/scaffold) responding to genuine supply vulnerability or an extraction mechanism (snare/tangled rope) distributing costs to global and domestic consumers. The mandatrophy depends on three empirical omega variables: (1) whether domestic capacity can realistically achieve self-sufficiency within the claimed timeline, (2) whether substitution and recycling can reduce mineral dependency before extraction persists indefinitely, (3) whether the reserve actually contains sufficient inventory to function as claimed or is mostly theater. If empirical evidence shows that domestic capacity cannot scale sufficiently and substitution is slow, the scaffold perspective (temporary support) is invalidated, and the constraint becomes a permanent snare — strategic minerals become a mechanism for extracting rents from global consumers and domestic manufacturers. The high theater ratio (0.58) indicates that institutional theater is already masking uncertainty about underlying function. The mandatrophy is not resolvable without data on actual reserve adequacy, domestic production scaling timelines, and substitution technology trajectories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_capacity_sufficiency,
    'Can domestic mining and processing capacity realistically reach supply self-sufficiency within 20 years, or does the reserve merely delay structural dependency?',
    'Geological surveys of domestic rare earth deposits; engineering studies of processing scaling; capital cost projections for new facilities; historical comparison with other countries'' domestic capacity targets',
    'If realistic: scaffold perspective valid, sunset logic works, extraction declines as capacity matures. If infeasible: reserve becomes permanent extraction mechanism (snare persists), and the proclaimed ''transition'' is theater masking indefinite redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_capacity_sufficiency, empirical, 'Whether domestic mining capacity can achieve supply self-sufficiency').

omega_variable(
    international_retaliation_probability,
    'What is the probability that nations holding mineral reserves (China, Congo, Indonesia) will restrict exports in response to US reserve creation, escalating the scarcity cycle?',
    'Trade pattern analysis pre/post reserve announcement; diplomatic communications and statements from producing nations; modeling of tariff/export restriction scenarios',
    'If high: reserve triggers tit-for-tat escalation, increasing suppression globally and making snare classification dominant. If low: reserve operates as intended coordination mechanism, validating rope/scaffold perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_retaliation_probability, empirical, 'Probability of retaliatory mineral export restrictions').

omega_variable(
    substitution_technology_trajectory,
    'Will technological substitution for scarce minerals (recycling, alternative materials, synthetic substitutes) reduce dependency before domestic capacity matures?',
    'R&D progress in battery chemistry, semiconductor manufacturing with abundant materials, circular economy recycling infrastructure; patent trends; commercial deployment timelines',
    'If fast: reserve''s extraction mechanism weakens as substitutes emerge; snare and tangled_rope classifications degrade toward rope as the scarcity constraint itself becomes obsolete. If slow: substitution lags reserve timeline, and extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_technology_trajectory, empirical, 'Rate of substitution technology deployment').

omega_variable(
    reserve_actually_available,
    'Does the $12 billion reserve actually contain mineral quantities sufficient to buffer supply disruptions, or is the declared reserve capacity largely theater?',
    'Audit of physical inventory; comparison of reserve tonnage vs annual consumption; stress-test against realistic disruption scenarios; transparency of stockpile location and purity data',
    'If well-stocked and verified: reserve has real coordination function (rope/scaffold). If mostly theater: piton classification dominates, and the reserve is institutional inertia masquerading as strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reserve_actually_available, empirical, 'Actual mineral inventory sufficiency of the reserve').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cmr_001, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmr_tr_t0, cmr_001, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cmr_tr_t5, cmr_001, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cmr_tr_t10, cmr_001, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cmr_be_t0, cmr_001, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cmr_be_t5, cmr_001, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cmr_be_t10, cmr_001, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cmr_001, resource_allocation).
narrative_ontology:affects_constraint(cmr_001, semiconductor_supply_chain_bottleneck).
narrative_ontology:affects_constraint(cmr_001, battery_supply_security).
narrative_ontology:affects_constraint(cmr_001, geopolitical_mineral_dependency).

% DUAL FORMULATION NOTE:
% The critical minerals reserve can be decomposed into two structurally distinct constraints: (1) the supply security problem (whether domestic or allied nations can reliably access minerals needed for defense and strategic manufacturing) and (2) the cost redistribution mechanism (how the reserve's preferential purchasing and allocation rules transfer wealth from global consumers to domestic preferred sectors). The reserve as announced conflates these two problems, but they have different ε values and different extraction mechanisms. The supply security problem has lower ε (genuine coordination function); the cost redistribution has higher ε (extraction). A complete analysis would separate these into distinct constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cmr_001, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
