% ============================================================================
% CONSTRAINT STORY: electrification_scale_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electrification_scale_2026, []).

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
 *   constraint_id: electrification_scale_2026
 *   human_readable: Industrial Scale Electrification via Gigafactory Integration
 *   domain: technological/economic/environmental
 *
 * SUMMARY:
 *   Industrial-scale electrification, catalyzed by Tesla's gigafactory model
 *   and subsequent global rollout, represents a fundamental shift in energy
 *   infrastructure. The constraint is not the physical transition itself
 *   (electricity is a known technology) but the institutional arrangement
 *   that accelerates electrification through vertical integration, economies
 *   of scale, regulatory mandates, and supply-chain consolidation. This
 *   arrangement creates a tangled coordination-extraction hybrid: it
 *   genuinely solves the coordination problem of scaling battery production
 *   cost-effectively, but simultaneously extracts from legacy industries,
 *   resource-extraction communities, and developing nations locked into
 *   mineral supply chains. The theater ratio has declined over the interval
 *   (from 0.55 in 2010 to 0.35 by 2026) because early electrification claims
 *   were more aspirational (Tesla's 'accelerating sustainable energy') and
 *   less functionally proven; by 2026, battery cost curves and grid
 *   integration are empirically demonstrable, reducing performative content.
 *   However, extractiveness has risen (0.28 to 0.38) as the supply-chain
 *   gatekeeping effects and mineral-extraction burdens have become clearer,
 *   and as electrification has locked developing economies into dependency on
 *   lithium and cobalt supplies from politically fragile regions.
 *
 * KEY AGENTS:
 *   - Vertically Integrated Battery Manufacturers (Tesla, CATL, BYD): Institutional/arbitrage beneficiaries — capture value through scale economies and supply-chain control; experience constraint as pure coordination benefit
 *   - Coal Mining Communities: Powerless/trapped victims — no geographic, skill, or capital mobility; extraction is total substitution of livelihood
 *   - Rare Earth Mining Workers: Powerless/trapped victims — substituted extraction, not liberation; locked into low-wage, high-risk mineral extraction
 *   - Legacy Automotive Suppliers: Moderate/constrained actors — face mixed coordination (enabling EV supply chains) and extraction (price-cost squeeze)
 *   - Electricity Grid Operators: Organized/constrained actors — coordinate renewable integration but extract via mandated upgrades and demand-response enforcement
 *   - Climate Policy Coalition: Organized/mobile agents — driving scaffold mechanism through subsidies and emission regulations with claimed sunset
 *   - Incumbent Energy Infrastructure: Institutional/arbitrage — maintains extraction through sunk capital and regulatory protection (piton mechanism)
 *   - Developed Nation EV Consumers: Powerful/mobile actors — face upfront infrastructure costs but have arbitrage capacity through relocation and subsidy access
 *   - Developing Nation Energy Communities: Powerless/trapped — extracted via commodity dependence and supply-chain consolidation
 *   - Analytical Observer: Civilizational perspective — risks naturalizing institutional arrangements (subsidy structures, regulatory lock-in) as immutable thermodynamic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electrification_scale_2026, 0.38).
domain_priors:suppression_score(electrification_scale_2026, 0.42).
domain_priors:theater_ratio(electrification_scale_2026, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electrification_scale_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(electrification_scale_2026, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(electrification_scale_2026, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electrification_scale_2026, tangled_rope).
narrative_ontology:human_readable(electrification_scale_2026, "Industrial Scale Electrification via Gigafactory Integration").
narrative_ontology:topic_domain(electrification_scale_2026, "technological/economic/environmental").

domain_priors:requires_active_enforcement(electrification_scale_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electrification_scale_2026, vertically_integrated_battery_manufacturers).
narrative_ontology:constraint_beneficiary(electrification_scale_2026, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(electrification_scale_2026, early_adopter_ev_consumers).
narrative_ontology:constraint_beneficiary(electrification_scale_2026, electricity_grid_operators).
narrative_ontology:constraint_victim(electrification_scale_2026, legacy_automotive_suppliers).
narrative_ontology:constraint_victim(electrification_scale_2026, fossil_fuel_industries).
narrative_ontology:constraint_victim(electrification_scale_2026, resource_extraction_communities).
narrative_ontology:constraint_victim(electrification_scale_2026, grid_stability_advocates).
narrative_ontology:constraint_victim(electrification_scale_2026, rare_earth_mining_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL MINING COMMUNITY (SNARE) — Trapped by geographic, skill, and capital constraints. Electrification extracts their livelihoods without viable alternatives in resource-extraction regions. No arbitrage, no mobility. d≈0.93, f(d)≈1.38, σ=0.9 → χ≈0.55.
constraint_indexing:constraint_classification(electrification_scale_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RARE EARTH MINING WORKERS (SNARE) — Substituted extraction: shift from fossil fuels to minerals, not liberation from extractive labor. Trapped by skill specialization, geographic immobility, and supply-chain consolidation around low-cost producers. d≈0.92, f(d)≈1.35, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(electrification_scale_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LEGACY AUTOMOTIVE SUPPLIERS (TANGLED ROPE) — Coordinating the supply chain transition (rope element: enabling battery assembly, thermal management, integration). But also extracted from: smaller suppliers forced into price-cost squeeze as OEMs demand EV-compatible components at fossil-era margins. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(electrification_scale_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: VERTICALLY INTEGRATED BATTERY MANUFACTURERS (ROPE) — Net beneficiaries. Gigafactory model creates genuine coordination: standardized production, predictable supply, economies of scale benefit the entire EV ecosystem AND battery makers themselves. Net positive value capture through arbitrage (licensing, manufacturing, supply contracts). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Negative effective extraction = net coordination benefit.
constraint_indexing:constraint_classification(electrification_scale_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ELECTRICITY GRID OPERATORS (TANGLED ROPE) — Coordinating intermittent renewable integration (rope element). But extraction occurs via enforcement: mandates for rapid grid upgrades, balancing requirements, and peak-demand liability when EVs charge unpredictably. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.31.
constraint_indexing:constraint_classification(electrification_scale_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE POLICY COALITION (SCAFFOLD) — Temporary coordination mechanism with explicit sunset: electrification subsidies, emission regulations, and manufacturing incentives are designed to bridge the cost gap until battery costs reach grid parity (estimated 2025-2028 achieved for most geographies). Low extraction because the coalition has agency and the suppression mechanism (subsidy withdrawal) is conditional on cost milestones. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.16. has_sunset_clause_rationale: Subsidies and emission mandates explicitly phase out as battery cost targets hit and grid parity is achieved. Estimated sunset: 2028-2032 for most developed economies.
constraint_indexing:constraint_classification(electrification_scale_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INCUMBENT ENERGY INFRASTRUCTURE (PITON) — Persists through institutional inertia (natural gas plants, oil refineries, coal plants). Primary function (energy provision) is atrophying but constraint (capital lock-in, stranded assets, regulatory moats) remains enforced by sunk capital and political support. Theater_ratio=0.45 is borderline; the performative element is regulatory justification of continued operation ('firm capacity' arguments). Technically theater ≤ 0.70, so this classifies as Rope with inertia rather than strict Piton, but the directional analysis (institutional, arbitrage, globe scope) shows extraction pathways persist via regulatory protection.
constraint_indexing:constraint_classification(electrification_scale_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: DEVELOPED NATION ENERGY CONSUMERS (TANGLED ROPE) — Coordinate through price signals and grid decarbonization (rope element). But extraction: upfront costs of EV and home charging infrastructure, volatile electricity prices during transition, grid congestion charges. Powerful + mobile means they can arbitrage (buy EVs in subsidized markets, relocate to lower-cost regions), so extraction is moderate. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.16.
constraint_indexing:constraint_classification(electrification_scale_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: DEVELOPING NATION ENERGY-DEPENDENT COMMUNITIES (SNARE) — Global electrification logic benefits wealthy nations with subsidy capacity and grid infrastructure; trapped in lower-cost supply chains (mineral extraction, manufacturing) with minimal bargaining power. Electrification as global regime extracts via commodity dependence: rare earths, lithium, cobalt sourced from politically fragile regions. d≈0.94, f(d)≈1.40, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(electrification_scale_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, some efficiency loss in energy conversion (coal → electricity → stored energy → motion) is irreducible thermodynamic fact. Battery chemistry has hard physical limits on energy density. Grid transmission losses are physics, not policy. However, the structural data (ε=0.38, suppression=0.42, theater=0.35) contradicts a strong mountain classification. The engine will identify this as a false summit: most of the 'immutable' constraints are actually institutional (supply-chain consolidation, subsidy structures, regulatory lock-in), not thermodynamic.
constraint_indexing:constraint_classification(electrification_scale_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electrification_scale_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electrification_scale_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electrification_scale_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(electrification_scale_2026, TR),
    TR >= 0.70.

:- end_tests(electrification_scale_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, rising. Base extraction reflects supply-chain gatekeeping effects (vertically integrated manufacturers control battery technology, pricing power). The rise over the interval (0.28 → 0.38) reflects growing awareness of mineral supply-chain concentration and developing-nation commodity dependence. Suppression (0.42): Moderate. Barriers exist (capital requirements for gigafactories, regulatory complexity, incumbent industry lobbying) but are not total — open-source battery initiatives, competing manufacturers, and policy support reduce suppression. Theater ratio (0.35): Low-moderate, declining. Early electrification (2010s) was heavily performative (sustainability claims, futurism, 'accelerating the inevitable'). By 2026, the functional reality has matured: gigafactory cost curves are empirically proven, grid integration is operationally real, not aspirational. Theater persists (some greenwashing, some subsidy justification rhetoric) but at reduced levels. Claimed type (tangled_rope): The constraint simultaneously enables genuine coordination (scaling battery production, integrating supplies) AND extracts asymmetrically (gatekeeping, mineral dependencies, legacy industry displacement). Both elements are structurally necessary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence across power and exit dimensions. Vertically integrated manufacturers see rope (pure coordination benefit). Coal communities see snare (total extraction, no exit). Grid operators see tangled_rope (coordination responsibility plus extraction liability). Climate coalition sees scaffold (temporary mechanism approaching sunset). Incumbent energy sees piton (degraded role persisting through inertia). Developed consumers see tangled_rope (mixed benefit and cost). Developing mineral communities see snare (trapped extraction). The perspectival gap reflects genuine structural asymmetry: the same electrification logic that coordinates Tesla's supply chain extracts from coal communities and mineral-dependent nations. This is not a measurement perspective problem — it is a real structural difference in how the constraint operates across power levels and exit capacities.
 *
 * DIRECTIONALITY LOGIC:
 *   Vertically integrated manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net coordination. Coal mining communities: Victim + trapped → d≈0.93, f(d)≈1.38. Maximum extraction. Rare earth workers: Victim + trapped → d≈0.92, f(d)≈1.35. Maximum extraction. Legacy suppliers: Victim + constrained → d≈0.68, f(d)≈1.05. High extraction, some coordinating benefit. Grid operators: Victim + constrained → d≈0.55, f(d)≈0.75. Mixed. Climate coalition: Beneficiary/organizer + mobile → d≈0.35, f(d)≈0.30. Low extraction, agency present. Incumbent energy: Beneficiary (of lock-in) + arbitrage → d≈0.08, f(d)≈-0.08. Piton classification from theater gate, not from directionality. Developed consumers: Moderate cost/benefit + mobile → d≈0.42, f(d)≈0.42. Moderate extraction. Developing communities: Victim + trapped → d≈0.94, f(d)≈1.40. Maximum extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint resolves mandatrophy by demonstrating that electrification is genuinely both coordination and extraction. The coordination element (scaling battery production, integrating supplies, solving the cost-curve problem) is real and essential. The extraction element (mineral supply-chain gatekeeping, developing-nation commodity lock-in, legacy industry displacement) is equally real and not incidental. The mandatrophy is resolved by refusing to collapse the hybrid into a single type. The constraint IS a rope from the battery manufacturer perspective AND a snare from the coal community perspective. Both are true. The system-level classification (claimed_type: tangled_rope) reflects that the mechanism simultaneously solves collective-action problems (battery scale) and creates asymmetric burdens (supply-chain power). If the analysis tried to reduce it to pure rope ('electrification is coordination'), it would naturalize the extraction effect as incidental ('job transitions happen'). If it reduced to pure snare ('electrification is gatekeeping'), it would dismiss the genuine technological breakthrough. The tangled_rope framework holds both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_chain_concentration_vs_coordination,
    'Is gigafactory consolidation primarily a coordination mechanism enabling scale efficiencies, or an extraction mechanism via supply-chain gatekeeping?',
    'Historical analysis of battery cost curves with/without vertical integration; comparison of open-source battery designs vs proprietary designs; market structure evolution (number of entrants, price dispersion, switching costs)',
    'If primarily coordination: rope classification dominates, extraction is 0.20-0.30. If primarily gatekeeping: tangled_rope or snare classification dominates, extraction rises to 0.50+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_concentration_vs_coordination, empirical, 'Whether gigafactories enable coordination or enforce extraction via gatekeeping').

omega_variable(
    mineral_extraction_substitution,
    'Does electrification reduce total extractive harm globally, or substitute fossil fuel extraction with mineral extraction at comparable or greater intensity?',
    'Lifecycle analysis comparing coal mining externalities (health, land, water) to lithium/cobalt mining externalities; labor condition comparative studies; supply-chain transparency and enforcement effectiveness',
    'If net reduction: electrification is genuine constraint relief. If net substitution: the power transfer is from coal communities to mineral-extraction communities, replicating snare structures at different scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mineral_extraction_substitution, empirical, 'Whether electrification reduces or substitutes extractive harm').

omega_variable(
    grid_stability_phase_transition,
    'At what penetration of renewable and EV load does the grid transition from a coordination problem (balancing diverse loads) to an extraction problem (peak-demand pricing, mandatory demand-response)?',
    'Simulation modeling of grid stability under varying solar+wind+EV penetration; empirical study of demand-response costs in high-penetration grids (Denmark, Germany); measurement of grid operator enforcement actions vs voluntary coordination',
    'If grid remains coordinative up to 60%+ penetration: tangled_rope persists. If extraction mechanisms activate early (30-40% penetration): snare classification rises for grid consumers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_phase_transition, empirical, 'Penetration threshold where grid transitions from coordination to extraction').

omega_variable(
    subsidy_dependency_and_sunset_credibility,
    'Can electrification subsidies and emission mandates actually be withdrawn once achieved, or do they persist as permanent extraction mechanisms disguised as temporary support?',
    'Historical analysis of energy subsidies (ethanol, nuclear, natural gas) that claimed sunset but persisted; modeling of political economy incentives for subsidy lock-in; early-stage observation of subsidy withdrawal attempts (EU, US regional policy 2025-2026)',
    'If sunset is credible: scaffold classification holds, extraction is temporary. If sunset is politically impossible: scaffold is aspirational (piton classification), extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_dependency_and_sunset_credibility, preference, 'Whether electrification subsidies can be genuinely withdrawn').

omega_variable(
    early_adopter_cost_incidence,
    'Are EV early-adopter costs (premium purchase price, charging infrastructure) primarily borne by wealthy consumers (arbitrage), or are they socialized through grid upgrades and cross-subsidization?',
    'Cost-benefit analysis of EV vs ICE at purchase; grid upgrade cost allocation mechanisms; electricity rate structure analysis (fixed vs variable, time-of-use pricing, EV surcharges); income distribution of tax credits and subsidies',
    'If borne privately by wealthy: efficient market coordination (rope). If socialized: extractive transfer from non-EV users to EV users (tangled_rope or snare depending on distribution).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(early_adopter_cost_incidence, empirical, 'Whether EV early-adopter costs are private or socialized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electrification_scale_2026, 2010, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electrification_scale_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(elec_tr_t8, electrification_scale_2026, theater_ratio, 8, 0.4).
narrative_ontology:measurement(elec_tr_t16, electrification_scale_2026, theater_ratio, 16, 0.35).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electrification_scale_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(elec_be_t8, electrification_scale_2026, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(elec_be_t16, electrification_scale_2026, base_extractiveness, 16, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electrification_scale_2026, resource_allocation).
narrative_ontology:affects_constraint(electrification_scale_2026, fossil_fuel_stranded_assets).
narrative_ontology:affects_constraint(electrification_scale_2026, mineral_supply_chain_concentration).
narrative_ontology:affects_constraint(electrification_scale_2026, grid_peak_demand_pricing).
narrative_ontology:affects_constraint(electrification_scale_2026, rare_earth_geopolitics).

% DUAL FORMULATION NOTE:
% Electrification can be decomposed into multiple constraint stories: (1) Battery cost-curve breakthrough (ε≈0.08, Mountain from technical perspective) — thermodynamic limits on energy density are immutable. (2) Supply-chain scaling mechanism (ε≈0.25, Rope from coordination perspective) — genuine coordination benefit, low extraction. (3) Mineral gatekeeping and developing-nation commodity lock-in (ε≈0.55, Snare from extraction perspective) — asymmetric power over supply, high extraction. The present story (ε=0.38, Tangled Rope) integrates all three at the level of industrial organization and policy regime. Upstream decomposition would separate technical breakthrough from institutional implementation; this story treats them as inseparable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electrification_scale_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
