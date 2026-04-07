% ============================================================================
% CONSTRAINT STORY: global_maritime_logistics_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_maritime_logistics_dependency, []).

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
 *   constraint_id: global_maritime_logistics_dependency
 *   human_readable: Global Maritime Logistics Dependency
 *   domain: economic/infrastructure/geopolitical
 *
 * SUMMARY:
 *   Global maritime logistics comprises 90% of international trade by volume
 *   and approximately 80% by value. The constraint operates at multiple
 *   levels: (1) physical-geographic (island and landlocked nations have no
 *   choice), (2) technological (20/40-foot container standard creates
 *   lock-in), (3) institutional (flag-of-convenience registry enables
 *   regulatory arbitrage), (4) economic (shipping oligopoly and port
 *   consolidation create pricing power). The constraint exhibits genuine
 *   coordination function (enables global supply chains) simultaneously with
 *   extraction (geographic dependency enables rent-seeking by shippers and
 *   port operators). The 2021-2023 shipping crisis revealed the extraction
 *   mechanism: when capacity tightens, shippers impose surcharges of
 *   300-400%, manufacturing absorbs unilaterally, and developing exporters
 *   lose market share. The constraint is stabilizing at higher extractiveness
 *   (0.35 in 2005 → 0.58 in 2025) as consolidation increases and climate
 *   disruption reduces route flexibility.
 *
 * KEY AGENTS:
 *   - Island Nations (powerless/trapped): Samoa, Mauritius, Caribbean states — export-dependent, zero alternatives to shipping, experience maximum extraction (shipping costs 20-40% of export value)
 *   - Landlocked Developing States (moderate/constrained): Chad, Rwanda, Zambia — constrained by infrastructure, partial alternatives (rail to neighboring ports), constrained extraction
 *   - Maritime Shipping Oligopoly (institutional/arbitrage): Maersk, MSC, CMA CGM — operate system, control capacity, capture pricing power, net beneficiaries
 *   - Container Port Operators (institutional/arbitrage): DP World, PSA, Hutchison Ports — control gateway chokepoints, capture surcharges during congestion, net beneficiaries
 *   - Manufacturing Supply Chains (organized/constrained): Automotive, electronics, apparel companies — coordinate global production, absorb shipping cost surges, experience mixed coordination-extraction
 *   - Flag-of-Convenience States (institutional/arbitrage): Panama, Liberia, Marshall Islands — provide vessel registration services, enable regulatory arbitrage, maintain through inertia
 *   - Analytical Observer (analytical/analytical): Evaluates constraint as hybrid coordination-extraction system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_maritime_logistics_dependency, 0.58).
domain_priors:suppression_score(global_maritime_logistics_dependency, 0.72).
domain_priors:theater_ratio(global_maritime_logistics_dependency, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_maritime_logistics_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_maritime_logistics_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_maritime_logistics_dependency, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_maritime_logistics_dependency, tangled_rope).
narrative_ontology:human_readable(global_maritime_logistics_dependency, "Global Maritime Logistics Dependency").
narrative_ontology:topic_domain(global_maritime_logistics_dependency, "economic/infrastructure/geopolitical").

domain_priors:requires_active_enforcement(global_maritime_logistics_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_maritime_logistics_dependency, maritime_shipping_oligopoly).
narrative_ontology:constraint_beneficiary(global_maritime_logistics_dependency, container_port_operators).
narrative_ontology:constraint_beneficiary(global_maritime_logistics_dependency, flag_of_convenience_states).
narrative_ontology:constraint_victim(global_maritime_logistics_dependency, island_nations).
narrative_ontology:constraint_victim(global_maritime_logistics_dependency, landlocked_developing_states).
narrative_ontology:constraint_victim(global_maritime_logistics_dependency, agricultural_exporters).
narrative_ontology:constraint_victim(global_maritime_logistics_dependency, manufacturing_supply_chains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISLAND NATION (SNARE) — No alternative to maritime shipping for import/export. Physical geography creates absolute dependency. Cannot exit without abandoning trade entirely. Experiences maximum extraction: shipping costs consume 20-40% of export value. No negotiating power.
constraint_indexing:constraint_classification(global_maritime_logistics_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LANDLOCKED DEVELOPING STATE (TANGLED ROPE) — Constrained by geography and infrastructure costs but not completely trapped. Partial alternatives exist (rail to neighboring ports, regional shipping). Extraction is significant but negotiable. Genuine coordination function: maritime system enables cross-border trade, but terms are asymmetric.
constraint_indexing:constraint_classification(global_maritime_logistics_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MARITIME SHIPPING OLIGOPOLY (ROPE) — Maersk, MSC, CMA CGM collectively operate the system. Experience shipping as coordination: connecting suppliers to markets. Net beneficiary with exit option (can redeploy capacity, optimize routes). Low experienced extraction because they are the system operator.
constraint_indexing:constraint_classification(global_maritime_logistics_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MANUFACTURING SUPPLY CHAINS (TANGLED ROPE) — Organized actors (automotive, electronics, apparel) coordinate global production across maritime routes. Genuine coordination function (production is impossible without global shipping), but terms create extraction: supply shocks and cost surges are passed through. 2021-2023 shipping crisis showed mechanism: when capacity tightens, shippers raise rates 300-400%, manufacturing bears cost.
constraint_indexing:constraint_classification(global_maritime_logistics_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FLAG-OF-CONVENIENCE STATES (PITON) — Panama, Liberia, Marshall Islands register 80% of global merchant fleet. Nominal sovereignty, zero enforcement capacity. Exist to enable regulatory arbitrage (low labor standards, minimal safety oversight). Theater ratio high (maritime law enforcement is largely performative); actual extraction derives from shipping operators, not from flag states. This tier of the system persists through inertia.
constraint_indexing:constraint_classification(global_maritime_logistics_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Maritime logistics is simultaneously a genuine coordination mechanism (enables global production and consumption) and a vehicle for extraction (geographic dependency enables rent-seeking by shippers and port operators). The constraint is hybrid: 40% functional coordination, 60% extractive surplus capture. Mandatrophy resolved by recognizing both components.
constraint_indexing:constraint_classification(global_maritime_logistics_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_maritime_logistics_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_maritime_logistics_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_maritime_logistics_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_maritime_logistics_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_maritime_logistics_dependency, TR),
    TR >= 0.70.

:- end_tests(global_maritime_logistics_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system extracts through geographic dependency, capacity control, and pricing power. Island and landlocked nations face absolute dependency (0.85+ experienced extraction from their perspective). Manufacturing supply chains face conditional extraction (rate surges during tightness, 0.40-0.60 experienced extraction depending on market conditions). The 0.58 value represents the system-average, weighted toward trapped agents with high exposure. Suppression (0.72): High. Multiple barriers to exit: (a) geographic (no physical alternative to shipping for island states), (b) infrastructural (alternative routes require massive investment — Arctic routes, trans-Asia rail, air freight all have 10-20 year development timelines and cost >$100B), (c) institutional (flag-of-convenience system creates regulatory arbitrage that is difficult to reverse), (d) technical (container standard lock-in binds infrastructure). Theater ratio (0.45): Moderate. The maritime system has genuine functional content (it actually moves goods efficiently) but includes performative elements: flag-of-convenience enforcement is largely theatrical, port security theater, environmental compliance auditing with low verification capacity. Theater has increased from 0.32 (early 2000s, pre-9/11 security theater) to 0.45 (2025, environmental reporting requirements with minimal enforcement).
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) — maritime shippers genuinely solve the logistics problem of connecting global producers to consumers. This perspective is correct for the beneficiaries: the system is functional, and they experience it as enabling their business. The trapped perspective (island nations) sees pure extraction (Snare) — they have no choice, pay high costs, and experience no coordination benefit (the coordination happens at the supply-chain level, not at the island's level). The constrained perspective (manufacturing supply chains) sees hybrid extraction-coordination (Tangled Rope) — the system coordinates their global production (genuine benefit) but extracts through cost surges (asymmetric extraction). The analytical observer sees the same hybrid structure (Tangled Rope): the system is 40-50% coordination function, 50-60% extraction mechanism. The perspectival gap reveals that 'coordination' and 'extraction' are not properties of the system itself but relationships between the system and specific agents. For shippers, it is Rope; for trapped agents, it is Snare; for constrained agents, it is Tangled Rope. The analytical position reconciles by acknowledging all three as valid perspectival readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those who capture pricing power: maritime shippers (Maersk et al.) benefit from capacity control and geographic dependency; port operators (DP World et al.) benefit from congestion surcharges; flag-of-convenience states benefit from registration fees and regulatory arbitrage. These agents have arbitrage exit options: they can redeploy capacity, choose alternative ports, or adjust flag registrations. Their derived d-values are low (0.05-0.20), producing negative or near-zero chi — they experience the constraint as enabling, not extractive. Victims are those trapped by geography or supply-chain dependency: island nations, landlocked states, and manufacturing exporters have high d-values (0.75-0.95 for trapped agents, 0.55-0.70 for constrained agents). Their chi values are high (1.1-1.4 for powerless/trapped, 0.7-0.9 for moderate/constrained). The extraction flow is unidirectional: from those who cannot exit toward those who control capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The system is genuine Tangled Rope, not falsely classified as pure coordination or pure extraction. Resolution evidence: (1) Beneficiary group exists and substantive: maritime shippers, port operators, and flag-of-convenience states derive genuine benefit from the system's coordination function. Removing them would disrupt the system. (2) Victim group exists and substantive: island nations, landlocked states, and manufacturing exporters bear asymmetric costs that exceed their coordination benefits. (3) Active enforcement exists: flag-of-convenience regulations, port security protocols, IMO maritime law (though enforcement is partial and performative). (4) Extractiveness is not dominant (0.58 < 0.66): the system is not a pure snare because it actually does coordinate global logistics. (5) Suppression is high (0.72): barriers to exit prevent victims from opt-out, which is required for Tangled Rope. The mandatrophy is fully resolved by the hybrid classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    port_congestion_vs_shipping_shortage,
    'Are supply shocks (2021-2023 shipping costs x3-x4) driven by structural shipping capacity shortage or by port congestion coordination failures?',
    'Decomposition of cost increases by component: (a) vessel utilization rates, (b) port dwell time increases, (c) fuel costs, (d) capacity hoarding by shippers. Historical comparison to 2008-2009 crisis.',
    'If capacity shortage: extraction is structural (Snare dominates). If coordination failure: constraint is more Rope-like with temporary Snare-like phases. Changes classification for moderate agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(port_congestion_vs_shipping_shortage, empirical, 'Whether supply shocks reflect capacity shortage or coordination failure').

omega_variable(
    alternative_route_feasibility,
    'Can Arctic shipping routes, land-based corridors (BELT AND ROAD), or air freight fundamentally substitute for Suez-Panama maritime corridors?',
    'Cost analysis of alternative routes under climate stabilization; infrastructure development timelines for trans-Asia rail; environmental impact of air freight scaling. Scenario modeling for 2040-2050.',
    'If alternatives viable: island and landlocked states have exit options (reclassify to constrained/mobile). If not: geographic trap persists (mountain-like immutability from trapped perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_route_feasibility, empirical, 'Whether alternative transport routes can substitute for maritime logistics').

omega_variable(
    regulatory_capture_mechanism,
    'Is the flag-of-convenience system maintained by genuine coordination necessity (vessel registration efficiency) or by shipping industry regulatory capture?',
    'Comparative analysis of high-flag-use countries vs high-compliance-cost countries; political economy of IMO rule-setting; quantification of regulatory arbitrage value to shipping operators.',
    'If coordination necessity: piton classification is accurate. If capture: the constraint has an artificial enforcement component that could be reversed (makes it more Snare-like at the implementation layer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, conceptual, 'Whether flag-of-convenience system reflects coordination necessity or regulatory capture').

omega_variable(
    container_ship_standardization_lock_in,
    'Is the 20/40-foot container standard an inherent coordination equilibrium or a lock-in that prevents container design alternatives?',
    'Historical analysis of container standard adoption (1960s-1970s); technical feasibility analysis of alternative modular standards; cost-benefit of converting port infrastructure.',
    'If lock-in: port operators and shipping lines have captured a standard that extracts from users who cannot deploy incompatible equipment. Reclassifies from Rope (coordination standard) to Snare (captured standard).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(container_ship_standardization_lock_in, empirical, 'Whether container standardization is coordination equilibrium or lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_maritime_logistics_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gmlde_tr_t0, global_maritime_logistics_dependency, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gmlde_tr_t10, global_maritime_logistics_dependency, theater_ratio, 10, 0.4).
narrative_ontology:measurement(gmlde_tr_t20, global_maritime_logistics_dependency, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(gmlde_be_t0, global_maritime_logistics_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gmlde_be_t10, global_maritime_logistics_dependency, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gmlde_be_t20, global_maritime_logistics_dependency, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_maritime_logistics_dependency, global_infrastructure).
narrative_ontology:affects_constraint(global_maritime_logistics_dependency, suez_canal_geopolitical_chokepoint).
narrative_ontology:affects_constraint(global_maritime_logistics_dependency, panama_canal_capacity_dependency).
narrative_ontology:affects_constraint(global_maritime_logistics_dependency, container_port_consolidation).
narrative_ontology:affects_constraint(global_maritime_logistics_dependency, shipping_fuel_cost_pass_through).

% DUAL FORMULATION NOTE:
% Global maritime logistics dependency decomposes into (a) geographic chokepoint constraints (Suez, Panama) with higher extractiveness (0.70+, Snare-like), (b) port operator consolidation (DP World dominance) with moderate extractiveness (0.55-0.65, Tangled Rope), and (c) shipper oligopoly (Maersk, MSC, CMA CGM) with lower extractiveness (0.40-0.55, Rope-like from shipper perspective, Tangled Rope from manufacturing perspective). This story aggregates the system-level constraint; disaggregated stories would show different ε values and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_maritime_logistics_dependency, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
