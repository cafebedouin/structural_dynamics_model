% ============================================================================
% CONSTRAINT STORY: chaco_road_network_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chaco_road_network_hierarchy, []).

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
 *   constraint_id: chaco_road_network_hierarchy
 *   human_readable: Chaco Road Network Hierarchy and Regional Control
 *   domain: archaeology/political_economy
 *
 * SUMMARY:
 *   The Chaco Road Network (circa 900–1150 CE) represents one of the most
 *   extensive pre-Hispanic engineering projects in North America:
 *   approximately 400 miles of formally constructed roads radiating from
 *   Chaco Canyon in the San Juan Basin (New Mexico/Colorado/Utah). The roads
 *   are typically 30 feet wide, straightened across terrain, and built with
 *   labor-intensive construction. The network structurally connected Chaco
 *   Canyon (a central ceremonial and administrative hub) to peripheral
 *   settlements and outlying communities. This constraint story examines the
 *   road network as a mechanism of hierarchical control and regional
 *   coordination. The roads enabled genuine coordination functions
 *   (communication, trade, ceremonial aggregation) while simultaneously
 *   extracting labor from peripheral communities and subordinating autonomous
 *   settlement networks to central authority. The classification varies
 *   dramatically across perspectives: peripheral communities experience the
 *   network as a Snare (extraction with minimal reciprocal benefit), the
 *   central authority experiences it as Rope (coordination mechanism),
 *   regional elites experience it as Tangled Rope (mixed benefit and
 *   obligation), and late-period expansion suggests the network became
 *   increasingly theatrical (Piton) as functional utility declined.
 *
 * KEY AGENTS:
 *   - Chaco Canyon Central Authority: Primary beneficiary (institutional/arbitrage) — controls redistribution, claims prestige from construction and maintenance, commands labor through authority structure
 *   - Peripheral Communities: Primary victim (powerless/trapped) — provide labor for construction and maintenance, supply goods to central redistribution, structurally dependent on access to the road network for trade and resources
 *   - Autonomous Settlement Networks: Secondary victim (moderate/constrained) — initially independent, gradually incorporated into the network through economic incentives and coercive pressure; lose autonomy over trade routes and settlement decisions
 *   - Regional Elite Lineages: Secondary beneficiary (powerful/mobile) — participate in road construction and maintenance; benefit from preferential redistribution and status recognition; tied to central authority through prestige competition
 *   - Mid-Level Administrative Layer: Organized intermediary (organized/constrained) — delegates labor mobilization and local coordination; receives authority from central hub; constrained by delegated mandate
 *   - Archaeological Record: Analytical observer (analytical/analytical) — observes network as material infrastructure; risks naturalizing contingent hierarchy as inevitable response to geography
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chaco_road_network_hierarchy, 0.58).
domain_priors:suppression_score(chaco_road_network_hierarchy, 0.72).
domain_priors:theater_ratio(chaco_road_network_hierarchy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chaco_road_network_hierarchy, extractiveness, 0.58).
narrative_ontology:constraint_metric(chaco_road_network_hierarchy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(chaco_road_network_hierarchy, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chaco_road_network_hierarchy, tangled_rope).
narrative_ontology:human_readable(chaco_road_network_hierarchy, "Chaco Road Network Hierarchy and Regional Control").
narrative_ontology:topic_domain(chaco_road_network_hierarchy, "archaeology/political_economy").

domain_priors:requires_active_enforcement(chaco_road_network_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chaco_road_network_hierarchy, chaco_canyon_central_authority).
narrative_ontology:constraint_beneficiary(chaco_road_network_hierarchy, regional_elite_lineages).
narrative_ontology:constraint_victim(chaco_road_network_hierarchy, peripheral_communities).
narrative_ontology:constraint_victim(chaco_road_network_hierarchy, autonomous_settlement_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL COMMUNITIES (SNARE) — Structurally trapped within the road network system. Communities must participate in road maintenance labor (corvée obligations), supply goods to centers, and route trade through mandated pathways. Exit is constrained by geographic location, resource dependency, and the absence of alternative infrastructure. The roads extract labor and goods with minimal reciprocal benefit to peripheral settlements. Maximum suppression due to the interdependency of survival resources and the physical impossibility of bypassing the network.
constraint_indexing:constraint_classification(chaco_road_network_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CHACO CANYON CENTRAL AUTHORITY (ROPE) — Experiences the road network as a coordination mechanism that solves legitimate collective action problems: aggregating labor for infrastructure, standardizing trade routes, enabling rapid communication across the region. The authority benefits from the network (arbitrage position: can extract value via redistribution control), but the network itself performs genuine coordination functions. The roads coordinate regional exchange and ceremonial gatherings. From this perspective, the constraint is primarily cooperative, not extractive.
constraint_indexing:constraint_classification(chaco_road_network_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL ELITE LINEAGES (TANGLED ROPE) — Mobile yet engaged. Regional elites benefit from the hierarchy (preferential access to redistribution, status recognition) and participate in road construction and maintenance. They experience genuine coordination benefits (the roads enable their own exchange networks and alliance marriages) alongside asymmetric extraction (they control redistribution and claim prestige from construction projects). They are not trapped, but they are embedded in the system such that autonomy is constrained by prestige competition — defecting would cost status and alliance ties.
constraint_indexing:constraint_classification(chaco_road_network_hierarchy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: AUTONOMOUS SETTLEMENT NETWORKS (TANGLED ROPE) — Communities that initially resisted integration or attempted to maintain independent exchange networks. Constrained by the growing hegemony of the road system: their own local exchange becomes increasingly costly when bypassed by the official network, their young adults are drawn to central ceremonies via roads, and their autonomy erodes through selective economic incentives. They are not powerless (they have resources, local authority structures, some mobility), but their capacity to opt out diminishes over time as the roads become the sole viable infrastructure. They both benefit from reduced trade costs and suffer extraction via subordination.
constraint_indexing:constraint_classification(chaco_road_network_hierarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: MID-LEVEL ADMINISTRATIVE LAYER (SCAFFOLD) — Intermediate authority structures (local leaders, regional coordinators) experience the road network as a temporary support structure for a delegated administrative function. From their perspective, the roads coordinate labor mobilization and ceremonial access during a period of expanding central authority. The scaffold logic emerges from their generational time horizon: the roads may not persist indefinitely if central authority weakens or alternative systems emerge. Their suppression is moderate because they have some autonomy in local implementation, though constrained by the delegated mandate.
constraint_indexing:constraint_classification(chaco_road_network_hierarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: LATE-PERIOD ROAD MAINTENANCE RITUAL (PITON) — Archaeological evidence suggests that in the late Chaco period (1100–1150 CE), road construction and maintenance became increasingly performative. Roads were maintained and expanded even as the central redistribution system degraded, as if the performance of hierarchy (building and maintaining the physical roads) could maintain the social hierarchy itself. Theater ratio is high because the roads' functional coordination role declined while their symbolic assertion of central authority intensified. The constraint persists through institutional inertia — the roads continued to extract labor even as their coordination benefits diminished.
constraint_indexing:constraint_classification(chaco_road_network_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational perspective, the Chaco road network is sometimes characterized as a natural consequence of geography and population density: any regional system at scale requires infrastructure, and the semi-arid Southwest necessarily concentrates settlement. This perspective risks naturalizing the political choice of hierarchy as an inevitable response to geography. However, the structural data reveals this as a false summit: other settlements in the Southwest maintained autonomous networks with minimal central hierarchy, demonstrating that the road hierarchy is a contingent political arrangement, not a law of ecology.
constraint_indexing:constraint_classification(chaco_road_network_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chaco_road_network_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chaco_road_network_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chaco_road_network_hierarchy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chaco_road_network_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chaco_road_network_hierarchy, TR),
    TR >= 0.70.

:- end_tests(chaco_road_network_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The road network extracted significant labor from peripheral communities for construction and maintenance. Labor demands increased over the network's operational period (measurements show rise from 0.35 to 0.58). However, extractiveness is not maximal (not 0.70+) because peripheral communities did receive some benefits: reduced trade costs, access to central redistribution ceremonies, and reduced vulnerability to resource scarcity through participation in a larger system. The extraction is real but not entirely one-directional. Suppression (0.72): High. Peripheral communities faced significant barriers to exit: geographic isolation, resource dependency on central redistribution, absence of alternative infrastructure, and social/kinship ties to the system. The roads themselves became the infrastructure that subordinated autonomy — once constructed, the network eliminated alternative pathways and concentrated power at Chaco. Theater ratio (0.45): Moderate. The roads had genuine functional value for most of the network's operational period, but evidence suggests increasing performativity in the late Chaco period (1100–1150 CE) — roads were maintained and expanded even as central redistribution declined, suggesting the performance of hierarchy through infrastructure became as important as the infrastructure's actual function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the primary diagnostic feature of this constraint. Chaco roads are simultaneously a coordination mechanism (Rope from central perspective), an extraction system (Snare from peripheral perspective), a mixed system of benefit and obligation (Tangled Rope from elite and moderate perspectives), a temporary solution (Scaffold from administrative perspective), a degraded ritual (Piton from late-period perspective), and a naturalized necessity (false Mountain from analytical perspective). No single type is 'correct' — all are structurally accurate readings from different positions. The constraint's coherence as a system depends on suppressing the peripheral perspective — if peripheral communities could collectively exit or organize resistance, the network's extraction mechanism fails. The road network's persistence depends on maintaining power asymmetries that keep peripheral agents powerless and trapped. The system collapses around 1150 CE, potentially because this suppression eventually failed — communities either stopped contributing labor or were simply abandoned for more sustainable settlement patterns.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by their structural relationship to the road network hierarchy. Peripheral communities are full targets (d ≈ 0.95): they provide labor and resources with minimal exit options and minimal reciprocal benefit. The central authority is a beneficiary (d ≈ 0.05): they control the system and extract redistributive value. Regional elites are partial beneficiaries (d ≈ 0.30): they benefit from status and selective redistribution but are constrained by prestige competition. Autonomous settlement networks shift from mobile (d ≈ 0.50) to increasingly constrained targets (d ≈ 0.75) as the roads become mandatory infrastructure. The mid-level administrative layer occupies an intermediate position (d ≈ 0.45): they exercise delegated authority but remain dependent on central mandate. These directionality values feed the sigmoid f(d) to produce experienced extractiveness (χ) — peripheral agents experience high χ, the central authority experiences low or negative χ. The network's structure thus produces radically different classifications from the same base properties depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the Mandatrophy (false natural law detection) by demonstrating that the Chaco road hierarchy is a political choice, not an inevitable response to geography or population density. The false summit risk is high — the roads are massive, visually impressive, and encompass a region with genuine scarcity constraints (semi-arid Southwest). It is easy to naturalize them as 'what any society would build given these conditions.' However, other Southwest societies in the same geographic and temporal context (e.g., the Mogollon, other Rio Grande pueblos) maintained autonomous settlement networks and less centralized road systems. The roads are not a law of ecology — they are a particular political arrangement that other communities chose not to adopt. The structural data confirms this: the roads exhibit high suppression (not natural constraint but social constraint) and active enforcement (not spontaneous coordination but mandated hierarchy). The late-period increase in theater ratio (0.25 → 0.45) is diagnostic: if the roads were a natural response to conditions, theater ratio should decline over time as the system optimizes. Instead, it rises, suggesting the performance of hierarchy became decoupled from functional necessity — a signature of institutional inertia and false naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_symbolic_roads,
    'To what degree did Chaco roads function as economic-redistribution infrastructure versus symbolic assertion of central authority?',
    'Archaeological evidence: road wear patterns, traffic volume reconstruction via artifact scatters, presence of road-side shrines vs storage facilities; ethnohistoric analogues of Pueblo road systems and their economic role',
    'If primarily functional: constraint classifies as Rope from more perspectives (genuine coordination). If primarily symbolic: constraint classifies as Snare/Piton from more perspectives (extraction/theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_symbolic_roads, empirical, 'Functional coordination versus symbolic authority assertion').

omega_variable(
    peripheral_agency_and_resistance,
    'How much agency did peripheral communities retain in accepting or negotiating the terms of road-network integration? Did some settlements resist or defect?',
    'Settlement survey data showing communities outside the road network; abandonment timing correlated with road expansion; ethnohistoric evidence of community resistance to central authority; local variation in road quality and maintenance',
    'If communities had significant agency: exit_options for periphery upgrade from trapped to constrained; classification shifts from Snare to Tangled Rope. If communities were coerced into the network: classification remains Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peripheral_agency_and_resistance, empirical, 'Agency and resistance options for peripheral communities').

omega_variable(
    redistribution_veracity,
    'Did the central authority actually redistribute goods equitably in exchange for road labor, or was redistribution primarily symbolic/insufficient?',
    'Stable isotope analysis of faunal remains and maize in peripheral vs central settlements; storage facility capacity reconstruction; evidence of feasting scale and frequency in peripheral settlements; archaeological signatures of nutritional stress',
    'If redistribution was substantial and equitable: the rope (coordination) component is real, strengthening Tangled Rope classification. If redistribution was minimal or elite-captured: the extraction component dominates, suggesting stronger Snare classification for periphery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_veracity, empirical, 'Substance and equity of central redistribution to road labor contributors').

omega_variable(
    road_network_collapse_causality,
    'Did the road network collapse because central authority weakened, or did the road network''s extraction weaken central authority by unsustainable labor demands?',
    'Chronological correlation: does road abandonment precede or follow central administrative collapse? Dendrochronology of structures; timing of regional settlement reorganization; evidence of resource depletion or labor strikes',
    'If roads collapsed because authority weakened: constraint is classified as dependent on central power (Piton or degraded Rope). If roads extraction caused collapse: constraint contributed to system failure (early warning signal of unsustainable Snare dynamics).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(road_network_collapse_causality, empirical, 'Causality between road network collapse and central authority decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chaco_road_network_hierarchy, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chaco_tr_t0, chaco_road_network_hierarchy, theater_ratio, 0, 0.25).
narrative_ontology:measurement(chaco_tr_t100, chaco_road_network_hierarchy, theater_ratio, 100, 0.35).
narrative_ontology:measurement(chaco_tr_t200, chaco_road_network_hierarchy, theater_ratio, 200, 0.45).

% Extraction over time
narrative_ontology:measurement(chaco_be_t0, chaco_road_network_hierarchy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chaco_be_t100, chaco_road_network_hierarchy, base_extractiveness, 100, 0.52).
narrative_ontology:measurement(chaco_be_t200, chaco_road_network_hierarchy, base_extractiveness, 200, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chaco_road_network_hierarchy, resource_allocation).
narrative_ontology:affects_constraint(chaco_road_network_hierarchy, chaco_canyon_redistribution_system).
narrative_ontology:affects_constraint(chaco_road_network_hierarchy, pueblo_ceramic_exchange_networks).
narrative_ontology:affects_constraint(chaco_road_network_hierarchy, anasazi_regional_settlement_hierarchy).

% DUAL FORMULATION NOTE:
% The Chaco road network is analyzed as a single constraint with multiple perspectives rather than as a decomposed family, because the ε-invariance principle suggests that changing the observable (e.g., measuring roads as infrastructure vs. roads as authority symbols) does not substantially change the base extractiveness value — it changes the theater ratio and suppression interpretation, but the core extraction of labor from peripheral communities remains constant at ε ≈ 0.58. However, the road network's dependent constraints (redistribution system, ceremonial aggregation, regional settlement hierarchies) have their own ε values and should be examined as separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chaco_road_network_hierarchy, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
