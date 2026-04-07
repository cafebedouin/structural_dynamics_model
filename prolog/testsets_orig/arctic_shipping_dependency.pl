% ============================================================================
% CONSTRAINT STORY: arctic_shipping_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_shipping_dependency, []).

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
 *   constraint_id: arctic_shipping_dependency
 *   human_readable: Arctic Shipping Route Dependency and Climate-Institutional Lock-in
 *   domain: geopolitical/economic/environmental
 *
 * SUMMARY:
 *   Arctic shipping dependency represents a structural lock-in of global
 *   supply chains to a newly-viable transportation corridor that externalizes
 *   climate and indigenous costs onto powerless agents while concentrating
 *   economic benefits in Arctic transit states and shipping capital. Climate
 *   change has opened previously inaccessible polar routes, creating a
 *   genuine coordination opportunity (faster global shipping) layered with
 *   asymmetric extraction (unsustainability risks, indigenous displacement,
 *   feedback destabilization). The constraint exhibits mandatrophy: the
 *   coordination function (global efficiency) is real but not unique to
 *   Arctic routing; the extraction is contingent on path-dependent capital
 *   investment, state territorial claims, and suppression of indigenous
 *   governance. The extractiveness has risen from 0.32 to 0.58 over 15 years
 *   as Arctic routes have transitioned from experimental to operational, as
 *   capital and resource industries have invested in Arctic-dependent supply
 *   chains, and as indigenous communities have faced escalating impacts.
 *   Theater ratio remains moderate (0.48) because some genuine coordination
 *   cost reduction exists, but theater has risen as environmental impact
 *   assessments and safety protocols have become performative cover for
 *   continued expansion.
 *
 * KEY AGENTS:
 *   - Arctic Transit States (Russia, Canada, Denmark): Primary beneficiaries (institutional/arbitrage) — control territorial passages, capture transit fees and resource development rights, accumulate geopolitical leverage
 *   - Arctic Indigenous Communities: Primary victims (powerless/trapped) — face ecosystem disruption, marine pollution, cultural erosion, climate destabilization; geographically immobile; no exit alternatives
 *   - Non-Arctic Maritime States: Secondary victims/beneficiaries (moderate/constrained) — gain short-term shipping efficiency but constrained by climate risk exposure and dependence on Arctic governance they cannot control
 *   - Shipping Capital and Resource Industries: Primary beneficiaries (institutional/constrained) — capture cost reduction and resource access opportunity but increasingly locked into supply chains dependent on Arctic viability as climate destabilizes
 *   - International Climate/Environmental Coalitions: Organized agents (organized/constrained) — advocating for alternative routes, carbon pricing, and route restrictions; building technological and policy exit pathways
 *   - Traditional Canal Authorities (Suez, Panama): Secondary actors (institutional/arbitrage) — institutional infrastructure increasingly theatrical as Arctic routes emerge; maintaining relevance through regulatory expansion
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies both genuine coordination (global supply chain efficiency) and irreducible extraction (climate/indigenous externalization)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_shipping_dependency, 0.58).
domain_priors:suppression_score(arctic_shipping_dependency, 0.65).
domain_priors:theater_ratio(arctic_shipping_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_shipping_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(arctic_shipping_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(arctic_shipping_dependency, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_shipping_dependency, tangled_rope).
narrative_ontology:human_readable(arctic_shipping_dependency, "Arctic Shipping Route Dependency and Climate-Institutional Lock-in").
narrative_ontology:topic_domain(arctic_shipping_dependency, "geopolitical/economic/environmental").

domain_priors:requires_active_enforcement(arctic_shipping_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_shipping_dependency, arctic_transit_states).
narrative_ontology:constraint_beneficiary(arctic_shipping_dependency, shipping_capital).
narrative_ontology:constraint_beneficiary(arctic_shipping_dependency, resource_extraction_industries).
narrative_ontology:constraint_victim(arctic_shipping_dependency, arctic_indigenous_communities).
narrative_ontology:constraint_victim(arctic_shipping_dependency, global_climate_stability).
narrative_ontology:constraint_victim(arctic_shipping_dependency, non_arctic_maritime_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARCTIC INDIGENOUS COMMUNITIES (SNARE) — Trapped by geographic immobility and dependence on ecosystems destabilized by shipping activity. Cannot exit the region; bear costs of pollution, marine noise, disrupted animal migration patterns, and cultural erosion. No alternative livelihood options. Maximum experienced extraction with no agency.
constraint_indexing:constraint_classification(arctic_shipping_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-ARCTIC MARITIME STATES (TANGLED ROPE) — Benefit from reduced shipping costs and faster transit times via Arctic routes, but constrained by dependence on climate stability and international governance frameworks they cannot fully control. Experience mixed coordination (efficiency gains) and extraction (climate risk and loss of alternative route leverage). Significant agency but not free exit.
constraint_indexing:constraint_classification(arctic_shipping_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ARCTIC TRANSIT STATES (ROPE) — Primary beneficiaries with arbitrage options. Control passage through claimed Arctic waters; capture transit fees, port development, strategic positioning. Experience the constraint as legitimate coordination: regulating shipping enables ecosystem management (claimed) while generating revenue. Net beneficiary position with institutional power and exit optionality.
constraint_indexing:constraint_classification(arctic_shipping_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL CLIMATE/ENVIRONMENTAL COALITIONS (SCAFFOLD) — Organized agents (IPCC, Arctic Council observer states, indigenous advocacy networks) see Arctic shipping dependency as a temporary coordination failure with a generational sunset: alternative fuel ships, sub-Arctic routing protocols, and ice-free corridor diversification are building exit pathways. Low effective extraction because the coalition has agency and sees policy intervention points (carbon pricing, mandated environmental impact assessment, route restrictions). Theater ratio low — functional coordination can replace current path-dependent extraction.
constraint_indexing:constraint_classification(arctic_shipping_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SHIPPING CAPITAL AND RESOURCE INDUSTRIES (TANGLED ROPE) — Benefit enormously from Arctic routes (cost reduction, climate opportunity for oil/gas extraction). Constrained by climate instability feedback loops: Arctic warming that opened routes destabilizes global supply chains. Genuine coordination function (efficient resource allocation) layered with asymmetric extraction (externalizing climate costs onto non-Arctic populations and indigenous groups). Active enforcement required — requires state protection, international treaty navigation, and suppression of alternative routes.
constraint_indexing:constraint_classification(arctic_shipping_dependency, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL SUEZ/PANAMA ROUTE GOVERNANCE (PITON) — Institutional infrastructure (canal authorities, maritime law, port systems) built for 20th-century trade geography is increasingly theatrical. The constraint persists through legacy institutional inertia even as Arctic routes emerge as alternatives. These institutions see their own role as degraded — maintaining relevance through rate hikes and regulatory expansion rather than functional necessity. Theater ratio high (performative maintenance of canal-dependent supply chains).
constraint_indexing:constraint_classification(arctic_shipping_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, Arctic shipping dependency exhibits both genuine coordination (global supply chain efficiency) and irreducible extraction (externalizing climate and indigenous costs). The constraint is neither immutable law nor pure extraction — it is an institutional arrangement with path-dependent benefits and externalized costs. Mandatrophy resolution: the coordination function (efficient global shipping) is real but not unique to Arctic dependency; the extraction is contingent on the distribution of climate costs.
constraint_indexing:constraint_classification(arctic_shipping_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_shipping_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_shipping_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_shipping_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_shipping_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arctic_shipping_dependency, TR),
    TR >= 0.70.

:- end_tests(arctic_shipping_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Arctic shipping dependency creates genuine efficiency gains (3-21 days faster between Asia and Europe depending on route) worth 10-15% fuel savings for some trade lanes. But these benefits are concentrated in shipping capital and Arctic states while costs are diffuse (climate destabilization, indigenous impacts, global supply chain volatility). The measured extractiveness at 0.58 reflects that the coordination function is real and not trivial, but the distribution is severely asymmetric. Suppression (0.65): High. Multiple binding mechanisms suppress alternatives: (1) Path-dependent capital investment in Arctic infrastructure (icebreaker fleets, port development, supply chains) makes exit expensive; (2) Arctic territorial claims and state enforcement create legal barriers to alternative routes; (3) Indigenous communities cannot exit geographic location; (4) Climate feedback locks non-Arctic states into Arctic dependency (destabilization increases shipping unreliability, making predictable Arctic routes more valuable despite long-term unsustainability); (5) Fossil fuel industries suppress alternative route investment through capital allocation and narrative control. Theater ratio (0.48): Moderate. Environmental impact assessment procedures exist and are not purely theatrical — genuine ecosystem monitoring occurs. But theater is rising as climate impact assessments become performative cover for continued expansion, and as safety protocols adapt to Arctic conditions rather than restricting expansion. The measurement trajectory shows theater rising as extractiveness rises, suggesting Goodhart drift (environmental procedure as performance metric rather than functional constraint).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the beneficiary perspective (Rope) and the victim perspective (Snare) is maximal: 2-3 classification levels apart. This is diagnostic of a high-extraction, asymmetric constraint. The organized coalition perspective (Scaffold) represents an intermediate position — they acknowledge the extraction but see an exit path. The piton perspective reveals that traditional canal infrastructure is degrading as Arctic routes replace it, but institutional inertia keeps the theater running. The mandatrophy is resolved by recognizing that all perspectives are internally consistent but represent different structural positions: beneficiaries genuinely experience coordination benefits; victims genuinely experience pure extraction; organized coalitions genuinely see an exit pathway; and the analytical observer genuinely sees both functions coexisting asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values encode each agent's structural position relative to the extraction flow. Arctic transit states (beneficiaries + institutional + arbitrage) have d ≈ 0.10-0.20 — low directionality, they experience negative effective extraction (net benefit). Shipping capital (beneficiaries + institutional + constrained, due to capital lock-in) has d ≈ 0.35-0.45 — moderate directionality, benefits exceed costs but exit is increasingly expensive. Non-Arctic maritime states (mixed beneficiaries and victims + moderate + constrained) have d ≈ 0.55-0.65 — high-moderate directionality, they bear significant climate risk costs relative to shipping efficiency gains. Indigenous communities (victims + powerless + trapped) have d ≈ 0.95 — maximum directionality, maximum experienced extraction. The effective extractiveness χ = ε × f(d) × σ(S) scales by scope (global σ = 1.2) and power-dependent directionality function. This produces high χ for powerless indigenous agents (d ≈ 0.95, f(d) ≈ 1.42, global scope 1.2) and low or negative χ for Arctic states (d ≈ 0.15, f(d) ≈ -0.01, global scope 1.2).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids the mandatrophy trap by decomposing perspectives by structural position. The question 'Is Arctic shipping dependency coordination or extraction?' has no universal answer — it depends on which agent and which time horizon. For Arctic transit states (institutional/arbitrage), it is coordination. For indigenous communities (powerless/trapped), it is extraction. For environmental coalitions (organized/constrained) over a generational horizon, it is a temporary Scaffold with a visible sunset. The analytical observer sees Tangled Rope — a genuine coordination function (global supply chain efficiency) inseparably entangled with asymmetric extraction (climate and indigenous costs). The mandatrophy is not 'which type is correct?' but 'how do these coexisting functions and distributions persist?' The answer is: suppression. High suppression (0.65) of indigenous veto power, of alternative routes, of climate cost internalization, and of non-Arctic maritime state governance participation locks the configuration in place despite its instability. As suppression weakens (through climate impacts, indigenous political organizing, or technological alternatives), the structure will decompose toward Scaffold (if organized exit pathways work) or toward pure Snare + Mountain (climate tipping points make Arctic routes unviable despite being locked in).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_feedback_collapse_threshold,
    'At what Arctic ice loss threshold does the shipping efficiency gain collapse due to climate destabilization feedback (supply chain disruption, extreme weather, infrastructure failure)?',
    'Integration of Arctic sea ice dynamics models with global supply chain disruption risk; historical analysis of shipping losses and delays during Arctic storm events; comparison of fuel savings against climate-risk externalities',
    'If threshold is near (< 5°C warming equivalent): Arctic shipping dependency is self-defeating extraction masquerading as coordination — the efficiency gains are temporary and precede cascading climate failures. If threshold is distant (> 8°C equivalent): the constraint has genuine mid-century coordination value alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_feedback_collapse_threshold, empirical, 'Climate tipping point threshold for Arctic shipping viability').

omega_variable(
    indigenous_consent_enforceability,
    'Can indigenous communities exercise meaningful veto power over Arctic shipping under existing international frameworks (UNCLOS, Arctic Council, domestic claims), or is ''consultation'' performative?',
    'Case study analysis of shipping route disputes (Inuit Tapiriit Kanatami vs Canadian Arctic Gateway, Sami rights in Nordic waters); tracking of rejected environmental impact assessments and policy reversals driven by indigenous advocacy; documentation of enforcement mechanisms for indigenous consent within UNCLOS and Arctic governance structures',
    'If consent is enforceable: indigenous communities move from trapped to constrained; classification shifts from snare to tangled rope. If performative: suppression metric increases above 0.65 — extraction is stronger than current assessment. Theater ratio may rise if environmental impact assessment procedures lack real veto power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_consent_enforceability, empirical, 'Whether indigenous communities have enforceable consent power over Arctic shipping').

omega_variable(
    alternative_route_viability,
    'Can sub-Arctic routing (longer but avoiding ice, permafrost, and indigenous territorial overlap) achieve cost parity with Arctic routes within 10 years? What is the technology and infrastructure investment required?',
    'Techno-economic analysis of alternative shipping corridors (enhanced icebreaker fleets for non-Arctic Arctic routes, northern Pacific alternatives, enhanced intermodal infrastructure); comparison of total cost of ownership: Arctic direct vs sub-Arctic safe routes including fuel, insurance, speed, climate risk premiums',
    'If viable and cheaper: Arctic shipping dependency dissolves — non-Arctic states gain exit optionality; classification shifts from snare/tangled rope to scaffold (sunset becomes observable). If sub-Arctic is more expensive: Arctic dependency persists through economic lock-in despite climate risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_route_viability, empirical, 'Economic viability of sub-Arctic routing alternatives to Arctic shipping').

omega_variable(
    state_enforcement_capacity_divergence,
    'Can individual Arctic transit states enforce consistent environmental and safety standards on shipping in claimed waters, or does state capacity divergence (Russia vs Scandinavian coast guards) create enforcement vacuums that reduce effective suppression?',
    'Comparative analysis of coast guard capacity, search-and-rescue response times, environmental enforcement budgets, and frequency of maritime incidents in Russian vs Canadian vs Scandinavian Arctic waters; documentation of flag-state evasion and environmental violations',
    'If capacity is uniform: suppression remains high (0.65+) — enforcement prevents exit. If capacity diverges sharply: effective suppression may be regional (high in Scandinavian waters, low in Russian waters), creating heterogeneous constraint landscape. This would require separate constraint stories per region or a network decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_capacity_divergence, empirical, 'Whether state enforcement capacity is uniform across Arctic transit states').

omega_variable(
    fossil_fuel_industry_alternative_exit_costs,
    'What is the true cost to shipping capital and resource extraction industries of abandoning Arctic routes and returning to traditional Suez/Panama + longer Southern routes? What is the profitability impact?',
    'Comparative shipping cost analysis controlling for fuel prices, insurance, transit time, port fees, and climate risk; calculation of NPV difference between Arctic and traditional routes for representative supply chains (oil/LNG, minerals, containerized trade); tracking of corporate transition costs if Arctic routes were closed',
    'If exit costs are low (< 5% margin reduction): beneficiaries are more constrained than institutional; they have real alternatives. Classification shifts from institutional arbitrage to constrained. If exit costs are catastrophic (> 20% margin compression): beneficiaries are locked in despite climate risk — this increases suppression and extraction simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fossil_fuel_industry_alternative_exit_costs, empirical, 'Exit cost magnitude for shipping capital and resource industries from Arctic dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_shipping_dependency, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arcs_tr_t0, arctic_shipping_dependency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arcs_tr_t5, arctic_shipping_dependency, theater_ratio, 5, 0.41).
narrative_ontology:measurement(arcs_tr_t10, arctic_shipping_dependency, theater_ratio, 10, 0.48).
narrative_ontology:measurement(arcs_tr_t15, arctic_shipping_dependency, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(arcs_be_t0, arctic_shipping_dependency, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(arcs_be_t5, arctic_shipping_dependency, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(arcs_be_t10, arctic_shipping_dependency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(arcs_be_t15, arctic_shipping_dependency, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_shipping_dependency, resource_allocation).
narrative_ontology:boltzmann_floor_override(arctic_shipping_dependency, 0.18).
narrative_ontology:affects_constraint(arctic_shipping_dependency, indigenous_territorial_sovereignty).
narrative_ontology:affects_constraint(arctic_shipping_dependency, global_supply_chain_climate_risk).
narrative_ontology:affects_constraint(arctic_shipping_dependency, fossil_fuel_extraction_lock_in).
narrative_ontology:affects_constraint(arctic_shipping_dependency, international_maritime_law_fragmentation).

% DUAL FORMULATION NOTE:
% Arctic shipping dependency is a constraint family requiring decomposition by observational method. The constraint discussed here (geopolitical/economic lock-in perspective) has ε = 0.58, Tangled Rope at analytical scale. A separate story would address the physical climate constraint (Arctic viability threshold vs global climate destabilization), which would have different ε and different perspectives. The two stories are linked via network.affects_constraints: if the climate story reaches its tipping point, the geopolitical-economic story's extractiveness may rise (forced choice between Arctic and collapse) or collapse (routes become unviable). Decomposition recommended for corpus rigor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arctic_shipping_dependency, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
