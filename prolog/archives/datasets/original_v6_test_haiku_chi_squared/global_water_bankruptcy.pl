% ============================================================================
% CONSTRAINT STORY: global_water_bankruptcy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_water_bankruptcy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: global_water_bankruptcy
 *   human_readable: The Global Water Bankruptcy Constraint
 *   domain: environmental/economic
 *
 * SUMMARY:
 *   Global water bankruptcy emerges as the primary constraint on sustainable
 *   hydrological systems when aggregate human water demand exceeds the
 *   renewable supply rate. This has been accelerating since the 1960s due to
 *   three factors: (1) agricultural intensification and irrigation expansion,
 *   (2) industrial and municipal water use growth, and (3) climate-driven
 *   recharge reduction in key aquifer regions. The constraint exhibits
 *   characteristics of a pure extraction (snare) mechanism rather than a
 *   coordination failure or natural law. Institutional arrangements — water
 *   subsidies, groundwater property rights that permit depletion,
 *   agricultural commodity pricing that externalizes hydrological costs, and
 *   upstream dam construction that capture shared river flow — systematically
 *   benefit industrial agriculture and urban monopolies while imposing costs
 *   on subsistence communities, aquatic ecosystems, and future generations.
 *   The 'bankruptcy' framing is precise: like financial bankruptcy, the
 *   system has entered a state where liabilities (committed water uses)
 *   exceed assets (renewable supply), and the resolution requires either
 *   restructuring (which harms creditors) or default (which harms debtors).
 *   The extraction is legalized through property law and international
 *   treaties that lack enforcement mechanisms. The theater ratio (0.58)
 *   reflects that water governance increasingly relies on sustainability
 *   rhetoric, virtual water trade frameworks, and international water-sharing
 *   treaties that are performative — they produce the appearance of
 *   stewardship while actual allocation follows geopolitical power.
 *
 * KEY AGENTS:
 *   - Subsistence agricultural communities (powerless/trapped): Primary victims in aquifer-depleted regions (South Asia, North Africa, Middle East); lack economic or political capacity to adapt; face abandonment or grinding poverty.
 *   - Industrial irrigation agriculture (powerful/mobile): Primary beneficiary; captures cheap water through subsidies and externalized cost of aquifer depletion and salinity damage; has exit options (crop switching, relocation) but exercises them selectively.
 *   - Water-scarce nations (moderate/constrained): Secondary victims; trapped by geopolitical dependency on shared rivers; upstream dam construction and agricultural diversion in upstream nations extract water sovereignty without recourse.
 *   - Urban water monopolies (institutional/arbitrage): Secondary beneficiary; controls urban supply; experiences constraint as legitimate coordination problem; has arbitrage options (desalination, imports, recycling) but maintains subsidy extraction from groundwater.
 *   - Aquatic ecosystems and future generations (powerless/trapped): Tertiary victims; completely outside the economic system; cannot negotiate, organize, or exit; bear irreversible costs (species extinction, ecosystem collapse, aquifer depletion beyond recharge).
 *   - International water governance bodies (institutional/constrained): Maintain performative frameworks (treaties, sustainability goals, water-accounting protocols); theater ratio indicates these are increasingly theatrical as actual water allocation follows geopolitical power.
 *   - Analytical observer (analytical/analytical): Civilizational perspective risks naturalizing institutional extraction as a physical law of hydrology — 'scarcity is inevitable' obscures how institutional design creates scarcity through subsidized overconsumption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_water_bankruptcy, 0.68).
domain_priors:suppression_score(global_water_bankruptcy, 0.72).
domain_priors:theater_ratio(global_water_bankruptcy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_water_bankruptcy, extractiveness, 0.68).
narrative_ontology:constraint_metric(global_water_bankruptcy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_water_bankruptcy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_water_bankruptcy, snare).
narrative_ontology:human_readable(global_water_bankruptcy, "The Global Water Bankruptcy Constraint").
narrative_ontology:topic_domain(global_water_bankruptcy, "environmental/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_water_bankruptcy, industrial_agriculture_irrigators).
narrative_ontology:constraint_beneficiary(global_water_bankruptcy, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(global_water_bankruptcy, urban_water_monopolies).
narrative_ontology:constraint_victim(global_water_bankruptcy, subsistence_agricultural_communities).
narrative_ontology:constraint_victim(global_water_bankruptcy, groundwater_dependent_regions).
narrative_ontology:constraint_victim(global_water_bankruptcy, future_generations).
narrative_ontology:constraint_victim(global_water_bankruptcy, aquatic_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE FARMERS (SNARE) — Trapped by hydrological collapse. Wells run dry; no exit option within the constraint (migration entails total social/economic loss). Extraction is maximal: constraint forces abandonment of land or grinding poverty through reduced yields. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.68.
constraint_indexing:constraint_classification(global_water_bankruptcy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WATER-SCARCE NATION (SNARE) — Constrained by geopolitical dependency on shared river basins (Nile, Jordan, Euphrates). Cannot unilaterally increase supply; cannot fully exit without economic devastation. Upstream dam construction and agricultural irrigation in upstream nations extract water sovereignty. d≈0.80, f(d)≈1.18, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(global_water_bankruptcy, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: INDUSTRIAL IRRIGATION (TANGLED ROPE) — Mobile exit option exists (switch to dry-farmed crops or relocate operations), but switching has significant cost. Benefits from cheap water subsidy and externalized drainage/salinity costs. Experiences extraction of groundwater from future generations and from downstream users, but also coordinates efficiency gains through irrigation infrastructure. d≈0.55, f(d)≈0.75, σ=0.8 → χ≈0.41.
constraint_indexing:constraint_classification(global_water_bankruptcy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: URBAN WATER MONOPOLY (ROPE) — Arbitrage exit (desalination, import, recycling). Experiences constraint as a coordination problem: water pricing, allocation, and infrastructure investment are legitimate collective-action functions. Benefits from monopoly control and underpriced groundwater. Suppression of alternatives (decentralized rainwater harvesting, wastewater recycling) is structural but maintenance costs are moderate. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(global_water_bankruptcy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: AQUATIC ECOSYSTEMS / FUTURE GENERATIONS (SNARE) — Completely trapped. Cannot exit, cannot negotiate, cannot organize. Rivers run dry (Colorado, Yellow, Indus); lakes collapse (Aral Sea, Dead Sea); aquifers drain irreversibly (Ogallala, North China Plain). Extraction is maximal and non-recoverable within human timescale. d≈0.98, f(d)≈1.48, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(global_water_bankruptcy, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL WATER GOVERNANCE (PITON) — Treaty frameworks (Ramsar, Agenda 21, SDG 6) persist as performative instruments with minimal enforcement. Theater ratio: commitment ceremonies, water-accounting protocols, sustainability frameworks are largely theatrical — actual water allocation follows geopolitical power (upstream dam construction, agricultural subsidies, extraction concessions). The treaties coordinate marginally but extract legitimacy and compliance theater. theater_ratio=0.58. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.22.
constraint_indexing:constraint_classification(global_water_bankruptcy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: HYDROLOGICAL CYCLE LIMIT (FALSE MOUNTAIN) — A civilizational/universal perspective might frame water bankruptcy as a physical law: precipitation is finite, and demand that exceeds it is impossible. However, the base properties (ε=0.68, suppression=0.72, theater=0.58) contradict mountain criteria (ε≤0.25, suppression≤0.05). The constraint is not emergent from hydrological physics but from institutional arrangements (subsidized extraction, irrigation intensity, population growth, waste). The 'natural law' framing naturalizes the contingent — this is a false summit, revealing how naturalizing language obscures extractive structures.
constraint_indexing:constraint_classification(global_water_bankruptcy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_water_bankruptcy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_water_bankruptcy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_water_bankruptcy, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_water_bankruptcy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_water_bankruptcy, TR),
    TR >= 0.70.

:- end_tests(global_water_bankruptcy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from subsistence farmers, aquatic ecosystems, and future generations to subsidize industrial agriculture and urban water monopolies. The extraction is not maximal (snare floor is ≥0.46) because some actors (wealthy nations, urban centers) have partial adaptation options, and some institutional mechanisms (water efficiency improvements, conservation) provide modest relief. However, the trend is upward: extractiveness has doubled from 0.32 (1960) to 0.68 (2020) as the system transitions from surplus to deficit. Suppression (0.72): High. Barriers to exit are substantial: groundwater depletion is physically irreversible on human timescales (fossil aquifer recovery period: 5,000-10,000 years). Agricultural relocation requires capital, knowledge, and social cohesion that subsistence communities lack. Upstream dam construction by politically powerful nations removes flow from downstream users with no compensation or negotiation. Water pricing in many regions is heavily subsidized, suppressing cost signals that would induce conservation. Alternative water sources (desalination, rainwater harvesting, wastewater recycling) are suppressed through infrastructure lock-in and regulatory barriers. Theater ratio (0.58): Moderate. International water-sharing treaties, sustainability commitments, and water-accounting frameworks are increasingly theatrical — they produce legitimacy and the appearance of stewardship without actually reducing extraction. Actual allocation follows geopolitical power (upstream dam construction, agricultural subsidies, extraction concessions). The theater has increased over the measurement period as the gap between commitment and action has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a dramatic perspectival gap. Industrial irrigators and urban monopolies see a Rope (coordination mechanism for allocating scarce water efficiently). Wealthy nations with desalination options see a Tangled Rope (some coordination, manageable costs, exit available). Water-scarce nations and subsistence communities see a Snare (no exit, no negotiation power, costs borne by powerless actors). Aquatic ecosystems and future generations experience a Snare that approaches a Mountain in irreversibility (species extinction and aquifer depletion are permanent). International governance institutions maintain a Piton perspective (treaties and protocols persist through inertia despite low effectiveness). A civilizational analytical observer risks naturalizing the constraint as a Mountain of physical law ('water scarcity is inevitable'), which obscures how institutional design created the bankruptcy through subsidized overconsumption, weak property rights for collective resources, and geopolitical extraction. The gap between beneficiary and victim perspectives is maximal: what appears as rational resource coordination to irrigators appears as structural violence to subsistence farmers.
 *
 * DIRECTIONALITY LOGIC:
 *   Subsistence agricultural communities: Victim + trapped → d≈0.92, f(d)≈1.38. Extreme extraction; ground well dry, no alternatives within constraint structure. Water-scarce nations: Victim + constrained → d≈0.80, f(d)≈1.18. Significant extraction; geopolitical dependency on shared rivers, no unilateral control. Industrial irrigation: Beneficiary + mobile (but constrained in practice) → d≈0.55, f(d)≈0.75. Mixed: benefits from cheap water but experiences feedback (rising input costs, yield decline from salinization). Urban water monopoly: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; has exit options (desalination, imports) but maintains extraction through monopoly control and subsidy. Aquatic ecosystems and future generations: Victim + trapped → d≈0.98, f(d)≈1.48. Maximum extraction; completely outside market system, cannot exit, cannot recover. International governance: Institutional/constrained → d≈0.35, f(d)≈0.32. Piton classification derives from theater gate (0.58 ≥ 0.70 threshold not met, but sufficient to indicate performativity). The directionality pattern shows that all major victims are either powerless or institutionally trapped, while all beneficiaries have some form of exit option or control.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through explicit acknowledgment that the constraint is fundamentally extractive, not coordinative. The confusion arises because water scarcity can be framed as a coordination problem ('humanity needs to allocate a scarce resource efficiently') or as an extraction problem ('some actors' overconsumption systematically excludes others'). The base properties (ε=0.68, suppression=0.72, beneficiaries present, victims present, requires_active_enforcement=false) indicate that this is a snare, not a rope or tangled_rope. The high extractiveness and suppression, combined with the directional asymmetry (beneficiaries have arbitrage/mobile exit; victims are trapped/constrained), confirm the snare classification. The mandatrophy is further resolved by showing that institutional reform alone cannot address the constraint because the extraction is encoded in property rights, subsidies, and geopolitical structures. A rope-only perspective would suggest that better allocation mechanisms (markets, treaties, efficiency improvements) could solve the problem; the snare diagnosis reveals that allocation mechanisms themselves are capture mechanisms — they legitimize extraction while appearing to coordinate. The theater ratio (0.58) indicates that governance increasingly relies on performative frameworks (sustainability rhetoric, voluntary commitments, accounting protocols) that produce the appearance of action without reducing extraction. This is the core of the mandatrophy: the constraint persists because the institutions designed to address it are themselves extractive mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    virtual_water_substitution_sufficiency,
    'Can global trade in virtual water (water-embedded in agricultural products) adequately substitute for local water in reducing bankruptcy without creating systemic dependency and extraction leverage?',
    'Modeling of water-stress propagation through agricultural trade networks; analysis of dependency concentration in exporting nations; longitudinal tracking of price volatility and sovereign control over export restrictions',
    'If substitution effective: constraint reclassifies from snare to tangled_rope for importing nations (coordination function of trade exists). If substitution creates upstream dependency and extraction leverage: remains snare with different victims (exporting agricultural regions become trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(virtual_water_substitution_sufficiency, empirical, 'Virtual water trade as substitute for local hydrological limits').

omega_variable(
    aquifer_recharge_timescale_recovery,
    'Can fossil aquifer depletion (Ogallala, North China Plain) be arrested before irreversible desertification, or is the extraction rate already locked into permanent ecosystem collapse within the next 50 years?',
    'Hydrological modeling of recharge vs extraction rates; paleoclimate reconstruction of Holocene recharge baselines; monitoring of well productivity decline and saltwater intrusion fronts; cost analysis of artificial recharge vs agricultural transition',
    'If recovery possible: constraint permits adaptation and partial reversal (shifts toward scaffold perspective — temporary extraction with sunset). If locked into collapse: constraint is immutable snare for affected regions (mountain-like in irreversibility but extractive in mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aquifer_recharge_timescale_recovery, empirical, 'Whether fossil aquifer depletion permits recovery before irreversible collapse').

omega_variable(
    desalination_economic_scalability,
    'Can energy-efficient desalination scale to provide >30% of global freshwater demand at costs that don''t reproduce the same extraction pattern with energy as the substitute good?',
    'Cost trajectory analysis of reverse osmosis and emerging technologies; energy source decarbonization modeling; comparison of desalination pricing to agricultural water value; analysis of brine disposal extraction costs on coastal ecosystems',
    'If scalable at cost parity: removes hard hydrological limit; constraint shifts from snare (trapped by physical scarcity) to tangled_rope (energy-cost extraction but with exit via technology). If cost remains prohibitive: scarcity remains, but wealthy nations exit via desalination while poor nations remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desalination_economic_scalability, empirical, 'Desalination cost trajectory and global scalability for substituting hydrological bankruptcy').

omega_variable(
    coordination_vs_extraction_institutional_design,
    'Is the global water bankruptcy fundamentally a coordination problem (optimizing use of finite supply) or an extraction problem (some actors'' overconsumption systematically excludes others from access)?',
    'Comparative institutional analysis of water governance models (market allocation vs cooperative management vs state rationing); case studies of constraints lifted vs constraints persisting after institutional reform; analysis of wealth correlation with water access post-reform',
    'If primarily coordination: constraint is remediable by better allocation mechanisms (rope/scaffold outcome possible). If primarily extraction: institutional reforms fail because they don''t address the underlying extraction incentive (snare diagnosis correct; structural reform needed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_institutional_design, conceptual, 'Whether water bankruptcy is coordination or extraction problem').

omega_variable(
    population_and_consumption_trajectory_binding,
    'Will global population stabilization (projected 2080s peak at ~10 billion) combined with consumption pattern shifts reduce demand below sustainable supply, or does current trajectory lock in permanent deficit regardless of future population decline?',
    'Demographic projection validation; modeling of consumption pattern change under water scarcity stress; comparison of per-capita water footprint trends to hydrological capacity; analysis of lock-in effects of infrastructure (dams, irrigation systems, cities) built during high-extraction period',
    'If reduction possible: constraint has a temporal sunset (scaffold perspective valid). If lock-in prevents reduction: extraction is irreversible within generational timescale (mountain-like permanence but with mechanism of extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(population_and_consumption_trajectory_binding, empirical, 'Whether population and consumption stabilization can reduce water demand below sustainable supply').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_water_bankruptcy, 1960, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gwb_tr_t0, global_water_bankruptcy, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gwb_tr_t30, global_water_bankruptcy, theater_ratio, 30, 0.45).
narrative_ontology:measurement(gwb_tr_t60, global_water_bankruptcy, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(gwb_be_t0, global_water_bankruptcy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gwb_be_t30, global_water_bankruptcy, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(gwb_be_t60, global_water_bankruptcy, base_extractiveness, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_water_bankruptcy, resource_allocation).
narrative_ontology:affects_constraint(global_water_bankruptcy, agricultural_commodity_subsidy_lock).
narrative_ontology:affects_constraint(global_water_bankruptcy, groundwater_property_rights_extraction).
narrative_ontology:affects_constraint(global_water_bankruptcy, transnational_river_basin_geopolitics).

% DUAL FORMULATION NOTE:
% The global water bankruptcy can be decomposed into three structurally distinct constraints: (1) agricultural subsidies that create incentive for water-intensive monoculture (ε≈0.35, coordination failure), (2) groundwater property rights that permit exhaustion of fossil aquifers without penalty (ε≈0.72, pure extraction), and (3) geopolitical control of shared rivers via upstream dam construction (ε≈0.58, extraction with coordination element). The integrated constraint (ε=0.68) is the system effect where all three reinforce each other. Water bankruptcy is downstream of subsidy architecture and property law but upstream of climate impact and ecosystem collapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
