% ============================================================================
% CONSTRAINT STORY: rare_earth_cluster_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_cluster_coupling, []).

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
 *   constraint_id: rare_earth_cluster_coupling
 *   human_readable: Rare Earth Cluster Coupling in Global Supply Chain
 *   domain: materials_science/geopolitics/economics
 *
 * SUMMARY:
 *   The rare earth cluster coupling constraint operates at the intersection
 *   of geology, industrial organization, and geopolitics. Seventeen rare
 *   earth elements (lanthanides plus yttrium and scandium) have overlapping
 *   extraction conditions, inseparable chemical properties at the ore
 *   concentration stage, and overlapping refining processes. This creates a
 *   clustering effect: extracting one rare earth efficiently typically
 *   requires processing multiple elements jointly. The strategic lever
 *   emerges not from the geological clustering itself but from institutional
 *   choices: where processing infrastructure is built, who controls
 *   technology for separation chemistry, how supply chains are organized, and
 *   which actors can arbitrage supply constraints. China controls ~70% of
 *   global rare earth refining capacity (compared to ~12% of primary
 *   extraction), a dominance achieved through deliberate investment in
 *   processing technology and infrastructure starting in the 1990s. This
 *   creates a two-level constraint: geological clustering is real (natural
 *   law component), but institutional concentration amplifies it into a
 *   strategic extraction mechanism. Dependent nations experience snare
 *   dynamics (trapped by supply control); emerging market producers
 *   experience tangled rope (coordinated supply chains with embedded
 *   extraction); dominant producers experience rope (coordination function
 *   with arbitrage benefits). The scaffold perspective shows real but
 *   constrained exit paths: recycling, substitution materials, and processing
 *   diversification are technically feasible but capital-intensive and
 *   subject to regulatory barriers. Theater ratio is moderate and stable
 *   (0.48), reflecting that the constraint operates through market
 *   concentration and infrastructure control rather than overt coercion — the
 *   performative element is lower than traditional geopolitical supply
 *   restrictions.
 *
 * KEY AGENTS:
 *   - Dominant Rare Earth Producers (China): Primary beneficiary (institutional/arbitrage) — controls refining, benefits from supply concentration, can set prices within ecosystem
 *   - Dependent Technology Nations (US, EU, Japan): Primary victim (powerless/trapped) — device manufacturers dependent on stable supply, face strategic restriction risk, cannot easily substitute or relocate
 *   - Emerging Market Producers (Vietnam, Indonesia, Myanmar): Secondary victims (organized/constrained) — extract primary ores but lack processing capacity, face technology subordination, constrained by capital and processing bottlenecks
 *   - Technology Manufacturers: Moderate victim (institutional/constrained) — benefit from stable supply chains but face price volatility and strategic risk
 *   - Strategic Diversification Coalition: Organized agents with exit paths (powerful/mobile) — governments and firms pursuing recycling, substitution, and processing reshoring
 *   - Analytical Observer: Risks naturalizing institutional choices (concentration, technology access restrictions) as geological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_cluster_coupling, 0.38).
domain_priors:suppression_score(rare_earth_cluster_coupling, 0.52).
domain_priors:theater_ratio(rare_earth_cluster_coupling, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_cluster_coupling, extractiveness, 0.38).
narrative_ontology:constraint_metric(rare_earth_cluster_coupling, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rare_earth_cluster_coupling, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_cluster_coupling, tangled_rope).
narrative_ontology:human_readable(rare_earth_cluster_coupling, "Rare Earth Cluster Coupling in Global Supply Chain").
narrative_ontology:topic_domain(rare_earth_cluster_coupling, "materials_science/geopolitics/economics").

domain_priors:requires_active_enforcement(rare_earth_cluster_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_cluster_coupling, dominant_rare_earth_producers).
narrative_ontology:constraint_beneficiary(rare_earth_cluster_coupling, technology_manufacturers).
narrative_ontology:constraint_victim(rare_earth_cluster_coupling, emerging_market_suppliers).
narrative_ontology:constraint_victim(rare_earth_cluster_coupling, strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT TECHNOLOGY SECTOR (SNARE) — Countries and firms without domestic rare earth capacity face structural entrapment. Extraction devices (supply concentration, pricing power, strategic restriction) operate continuously. Exit options are materially blocked by geology and capital requirements. Maximally experienced extraction.
constraint_indexing:constraint_classification(rare_earth_cluster_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING MARKET PRODUCERS (TANGLED ROPE) — Vietnam, Indonesia, Myanmar producers have genuine coordination function (enabling stable supply chains, investment in processing infrastructure) alongside asymmetric extraction (technology access constraints, pricing subordination to dominant producers). Constrained exit — can build capacity but face capital barriers and processing bottlenecks.
constraint_indexing:constraint_classification(rare_earth_cluster_coupling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: DOMINANT PRODUCERS (ROPE) — China's integrated rare earth production (mining + refining + magnet manufacturing) benefits from the constraint as coordination: stable supply chains, predictable pricing within ecosystem, processed material advantage. Net beneficiary with arbitrage options (can shift investment between rare earth elements based on global demand).
constraint_indexing:constraint_classification(rare_earth_cluster_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STRATEGIC DIVERSIFICATION COALITION (SCAFFOLD) — US, EU, Japan governments + firms pursuing rare earth sovereignty have sunset logic: recycling, substitution materials (permanent magnets without heavy rare earths), supply chain reshoring. Theater ratio decreases as technical alternatives mature. Exit path visible but constrained by capital requirements and technology maturation timelines.
constraint_indexing:constraint_classification(rare_earth_cluster_coupling, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNOLOGY MANUFACTURING — DEPENDENT NATIONS (TANGLED ROPE) — Device manufacturers in South Korea, Taiwan, Japan benefit from stable rare earth supply chains (coordination function) but face price volatility, supply restrictions, and technology subordination to China. Constrained exit — can pursue recycling and substitution but cannot easily relocate production or source independently.
constraint_indexing:constraint_classification(rare_earth_cluster_coupling, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (PITON) — From a geologically deterministic perspective, rare earth clustering (17 elements with overlapping extraction conditions, inseparable chemical properties) appears as an immutable natural constraint: the earth simply concentrated these elements in specific locations; physics and chemistry dictate joint production. However, this naturalizes what is structurally a contingent institutional choice: processing concentration in China, regulatory barriers to alternative processing, lack of investment in recycling infrastructure. The constraint appears immutable but is partially constructed.
constraint_indexing:constraint_classification(rare_earth_cluster_coupling, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: GEOLOGICAL IMMUTABILITY (MOUNTAIN) — From a deep geological timescale, rare earth element clustering is a consequence of planetary differentiation and mineralization processes that operated 4+ billion years ago. The lanthanides cluster together in the periodic table (chemical similarity), in mineralogy (co-precipitation), and geographically (few economically viable deposits). This appears to be an irreducible natural law. However, the strategic leverage derives not from geology but from institutional choices about processing infrastructure and technology access.
constraint_indexing:constraint_classification(rare_earth_cluster_coupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_cluster_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_cluster_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_cluster_coupling, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_cluster_coupling, TR),
    TR >= 0.70.

:- end_tests(rare_earth_cluster_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts through pricing power, supply concentration, and technology access subordination. Not maximal (0.72+) because substitution pathways exist and are technically improving. Emerging market producers capture some value through primary ore extraction, though this value is small compared to processing margins. The extraction is real but not as severe as a monopoly on non-substitutable goods. Theater ratio (0.48): Low-moderate. The constraint operates through market concentration and infrastructure control rather than overt coercion or performative ritual. Supply restrictions when they occur (e.g., 2010 export quota) are explicit. Suppression (0.52): Moderate-high. Barriers include geological rarity (few economically viable deposits), processing capital requirements ($500M+ for modern refinery), technology barriers (separation chemistry is complex), and environmental regulation (rare earth processing is environmentally intensive). Barriers are real but not insurmountable — multiple countries have attempted processing diversification; most faced cost and regulatory challenges rather than impossibility. The claimed type (tangled_rope) reflects that the constraint combines genuine coordination function (stable supply chains enable manufacturing ecosystem) with asymmetric extraction (pricing power, technology subordination, supply control).
 *
 * PERSPECTIVAL GAP:
 *   The rare earth constraint demonstrates how a single structural phenomenon (rare earth element clustering and processing concentration) generates incompatible classifications from different positions. The beneficiary's Rope perspective is empirically correct from their structural position — the constraint does coordinate supply chains and provide arbitrage benefits. The victim's Snare perspective is equally correct from their position — they are trapped by supply concentration and face extraction through pricing power. The gap is not a measurement error; it is the constraint's actual structural nature: Tangled Rope with asymmetric extraction. The geological/natural law perspective risks misclassifying the institution (processing concentration) as geology (element clustering). The false summit test: if rare earth clustering were truly immutable (mountain), then processing concentration would be geologically inevitable. Instead, multiple countries have attempted processing diversification; most faced economic and regulatory barriers, not physical impossibility. This proves that the institutional concentration is contingent, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the agent's structural relationship to the extraction flow and their exit options. Dominant producers have arbitrage exit (can shift investment between rare earth elements and applications) and benefit from the constraint → low d → negative χ → experienced as Rope. Dependent technology nations are trapped by geology and infrastructure concentration; no exit options; bear full cost → high d → high χ → experienced as Snare. Emerging market producers are organized (can coordinate extraction improvements) and constrained (face processing bottlenecks); both benefit from supply agreements and suffer from value subordination → moderate d → moderate χ → experienced as Tangled Rope. The strategic coalition is powerful and mobile (has capital for diversification, can pursue alternative materials) but still constrained by maturation timelines; has visible exit paths → lower-moderate d → experienced as Scaffold. The piton perspective (geological inevitability) has low theater (the constraint operates through market concentration, not performative mechanisms) and represents a risk of naturalizing institutional choices.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is genuinely hybrid — it contains both coordination function and asymmetric extraction. The coordination function is real: stable supply chains, integrated supply-demand forecasting, long-term investment in processing infrastructure. The asymmetric extraction is equally real: China's processing dominance, pricing power over dependent manufacturers, technology access restrictions, strategic supply control. The mandatrophy is resolved by recognizing that both components are structural, not by claiming one is primary. The constraint must be classified as Tangled Rope (not pure extraction/Snare, not pure coordination/Rope) because removing either component changes the entire structure. If the coordination function disappeared (supply chains fragmented into spot markets), the constraint would become pure Snare with higher perceived extraction. If the asymmetric extraction disappeared (true competitive processing market), the constraint would become pure Rope. Both are real; both are essential to the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    processing_concentration_necessity,
    'Is rare earth processing concentration in China a geological necessity or an institutional/economic choice?',
    'Historical analysis of processing investments, environmental regulation comparisons, recycling technology maturation rates, and rare earth processing attempted elsewhere (US Molycorp, EU efforts)',
    'If geological necessity: constraint is closer to mountain; institutional alternatives are severely limited. If institutional choice: constraint is primarily tangled_rope; diversification is economically feasible but capital-intensive and faces regulatory barriers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(processing_concentration_necessity, empirical, 'Whether rare earth processing concentration is geologically necessary or institutionally determined').

omega_variable(
    substitution_technology_ceiling,
    'Can permanent magnet technology substitution (e.g., iron-cobalt, soft magnets) eliminate dependence on heavy rare earths within 10-30 years?',
    'Engineering feasibility studies, manufacturing cost comparisons, performance gap analysis, R&D investment trajectory for alternative magnet materials',
    'If substitution succeeds: scaffold sunset is real; constraint transitions from tangled_rope to rope within generational timescale. If substitution fails: heavy rare earth dependence persists; constraint remains snare for dependent nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_technology_ceiling, empirical, 'Technical feasibility of permanent magnet material substitution').

omega_variable(
    recycling_economic_viability,
    'Can recycled rare earths provide meaningful supply diversification at economically competitive prices within manufacturing cost structures?',
    'Life-cycle cost analysis of recycling pathways, scale-up trajectories for industrial recycling, price parity timelines with primary production, technology maturation data from pilot programs',
    'If economically viable: recycling creates genuine exit path for dependent nations; constraint becomes constrained rather than trapped. If economically marginal: recycling remains niche; dependence persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_economic_viability, empirical, 'Economic viability of rare earth recycling as supply alternative').

omega_variable(
    strategic_mineral_coupling,
    'Are rare earth constraints structurally linked to other strategic minerals (cobalt, lithium, semiconductor manufacturing inputs) in ways that prevent diversification?',
    'Supply chain dependency graph analysis, multi-mineral extraction location mapping, processing step overlap analysis, substitution feasibility across entire technology stack',
    'If highly coupled: rare earth constraint is exemplar of broader strategic mineral architecture; solving rare earths doesn''t solve strategic autonomy. If decoupled: rare earth problem is more tractable independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_mineral_coupling, empirical, 'Degree of structural coupling between rare earth and other strategic mineral constraints').

omega_variable(
    geopolitical_enforcement_visibility,
    'How overtly does China enforce rare earth supply restrictions, versus how much enforcement is implicit in pricing power and processing control?',
    'Historical analysis of supply restrictions, export quotas, pricing dynamics, and comparison with other concentrated commodity markets (oil, agricultural products)',
    'If explicit enforcement: constraint is snare (coercive mechanism visible). If implicit through market power: constraint appears as rope (coordination) from beneficiary perspective but snare from victim perspective; perspectival gap is maximal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_enforcement_visibility, empirical, 'Visibility and explicitness of rare earth supply enforcement mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_cluster_coupling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(recc_tr_t0, rare_earth_cluster_coupling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(recc_tr_t10, rare_earth_cluster_coupling, theater_ratio, 10, 0.45).
narrative_ontology:measurement(recc_tr_t20, rare_earth_cluster_coupling, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(recc_be_t0, rare_earth_cluster_coupling, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(recc_be_t10, rare_earth_cluster_coupling, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(recc_be_t20, rare_earth_cluster_coupling, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_cluster_coupling, resource_allocation).
narrative_ontology:boltzmann_floor_override(rare_earth_cluster_coupling, 0.12).
narrative_ontology:affects_constraint(rare_earth_cluster_coupling, semiconductor_supply_chain).
narrative_ontology:affects_constraint(rare_earth_cluster_coupling, battery_technology_dependency).
narrative_ontology:affects_constraint(rare_earth_cluster_coupling, permanent_magnet_monopoly).

% DUAL FORMULATION NOTE:
% Rare earth cluster coupling is downstream of geological distribution constraints but structurally independent. The rare earth element clustering (chemical and mineralogical co-precipitation) is a distinct constraint from the geopolitical supply concentration (institutional choice about processing infrastructure). These could be decomposed into separate stories: rare_earth_geological_clustering (ε≈0.08, Mountain) and rare_earth_processing_concentration (ε≈0.38, Tangled Rope). The current story focuses on the processing concentration and its geopolitical consequences; the geological clustering is treated as background context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_cluster_coupling, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
