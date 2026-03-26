% ============================================================================
% CONSTRAINT STORY: snowpack_decline_mountain_hydrology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_snowpack_decline_mountain_hydrology, []).

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
 *   constraint_id: snowpack_decline_mountain_hydrology
 *   human_readable: Snowpack Decline and Mountain Hydrology Constraint
 *   domain: environmental/hydrology/climate
 *
 * SUMMARY:
 *   Snowpack decline in mountain regions constrains downstream water
 *   availability through a mechanism combining forced climate change with
 *   locked-in institutional infrastructure. As atmospheric warming shifts
 *   precipitation from snow to rain and accelerates snowmelt timing,
 *   downstream agricultural communities, urban water systems, and alpine
 *   ecosystems face declining water supply during the critical growing and
 *   dry seasons. The constraint operates as a pure extraction mechanism
 *   (snare) because: (1) the physical mechanism (thermodynamically-forced
 *   hydrological shift) cannot be negotiated; (2) downstream users have no
 *   viable exit options — agriculture cannot relocate, urban systems cannot
 *   substitute fundamentally different water sources at scale, ecosystems
 *   have no geographic escape; (3) suppression is high — water rights are
 *   locked into historical allocations based on historical snowpack,
 *   infrastructure was engineered for conditions that no longer exist, and
 *   transitioning to alternative sources or demand reduction faces political
 *   barriers. The extractiveness value (0.68) reflects the gap between
 *   historical water availability and projected supply under continued
 *   climate forcing, measured as a fraction of demand that cannot be met. The
 *   theater ratio (0.38) is low, indicating that the constraint is primarily
 *   functional (physical) rather than performative, though some institutional
 *   theater exists around water allocation rituals and conservation theater
 *   that distracts from the underlying supply crisis.
 *
 * KEY AGENTS:
 *   - Downstream Agricultural Communities: Primary victim (powerless/trapped, regional scope) — depend on summer baseflow for irrigation; no geographic exit; water rights are senior but insufficient
 *   - Urban Water Systems: Secondary victim (moderate/constrained, national scope) — face growing demand and declining supply; some exit options (desalination, inter-basin transfer) but at high cost
 *   - Alpine Ecosystems: Primary victim (powerless/trapped, regional scope) — abstract collective victim with no self-advocacy; experience ecosystem stress and phenological mismatch
 *   - Summer Baseflow Infrastructure: Tertiary victim (powerless/trapped, national scope) — hydropower, fish hatcheries, industrial cooling all dependent on summer flow; locked into historical hydrological patterns
 *   - Climate System: Non-agent driver — atmospheric warming forcing the constraint through physical mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(snowpack_decline_mountain_hydrology, 0.68).
domain_priors:suppression_score(snowpack_decline_mountain_hydrology, 0.72).
domain_priors:theater_ratio(snowpack_decline_mountain_hydrology, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(snowpack_decline_mountain_hydrology, extractiveness, 0.68).
narrative_ontology:constraint_metric(snowpack_decline_mountain_hydrology, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(snowpack_decline_mountain_hydrology, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(snowpack_decline_mountain_hydrology, snare).
narrative_ontology:human_readable(snowpack_decline_mountain_hydrology, "Snowpack Decline and Mountain Hydrology Constraint").
narrative_ontology:topic_domain(snowpack_decline_mountain_hydrology, "environmental/hydrology/climate").

domain_priors:requires_active_enforcement(snowpack_decline_mountain_hydrology).
% --- Structural relationships ---
narrative_ontology:constraint_victim(snowpack_decline_mountain_hydrology, downstream_agricultural_communities).
narrative_ontology:constraint_victim(snowpack_decline_mountain_hydrology, urban_water_systems).
narrative_ontology:constraint_victim(snowpack_decline_mountain_hydrology, alpine_ecosystems).
narrative_ontology:constraint_victim(snowpack_decline_mountain_hydrology, summer_baseflow_dependent_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM AGRICULTURAL COMMUNITIES (SNARE) — Face declining summer water availability with no viable exit. Trapped by geographic dependence on snowpack-fed baseflow; irrigation requirements cannot be met as spring snowmelt shifts earlier and volume declines. High suppression: limited water rights transfers, entrenched agricultural leases, capital investment in irrigation infrastructure. Extraction mechanism operates through temporal mismatch — traditional irrigation schedules misalign with compressed snowmelt window.
constraint_indexing:constraint_classification(snowpack_decline_mountain_hydrology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: URBAN WATER SYSTEMS (SNARE) — Municipal utilities dependent on snowpack for summer baseflow experience declining water availability. Constrained by existing infrastructure (reservoirs built for historical snowpack levels), population growth projections, and political barriers to conservation mandates. Some exit options available (groundwater mining, desalination, inter-basin transfers) but at high capital and political cost. Suppression manifests through locked-in infrastructure and growth expectations.
constraint_indexing:constraint_classification(snowpack_decline_mountain_hydrology, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALPINE ECOSYSTEMS (SNARE) — Have no exit options and bear maximum extraction. Compressed snowmelt window reduces habitat suitability for snow-adapted species; earlier spring flow disrupts reproductive cycles of aquatic organisms; late-summer drought stress increases. No self-advocacy mechanism; ecosystem services (water storage, carbon sequestration, biodiversity) are externalities to the extraction logic. Powerless victim with zero degrees of freedom.
constraint_indexing:constraint_classification(snowpack_decline_mountain_hydrology, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: SUMMER BASEFLOW-DEPENDENT INFRASTRUCTURE (SNARE) — Hydropower facilities, fish hatcheries, minimum-flow requirements for ecosystem health, and industrial cooling systems all depend on reliable summer baseflow. The constraint extracts from these systems through temporal scarcity — the infrastructure was engineered for historical snowpack patterns and cannot flex as those patterns shift. Suppression: locked-in physical infrastructure, regulatory minimum flows that become unachievable.
constraint_indexing:constraint_classification(snowpack_decline_mountain_hydrology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, snowpack decline follows from the laws of thermodynamics and atmospheric physics: warming atmospheric temperatures shift precipitation phases from snow to rain and accelerate snowmelt timing. This is a natural consequence of climate forcing, not an institutional arrangement. However, the EXTRACTION MECHANISM — who bears costs and who benefits — is not natural. The misalignment between historical infrastructure and shifting hydrology is a contingent institutional fact.
constraint_indexing:constraint_classification(snowpack_decline_mountain_hydrology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(snowpack_decline_mountain_hydrology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(snowpack_decline_mountain_hydrology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(snowpack_decline_mountain_hydrology, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(snowpack_decline_mountain_hydrology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(snowpack_decline_mountain_hydrology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts through a widening gap between historical water availability (on which all downstream institutions are optimized) and projected supply under warming. The measurement trajectory shows extractiveness increasing from 0.32 (1970s, early climate signal not yet clear) through 0.48 (1990s-2000s, observational confirmation) to 0.68 (2010s-2020, accelerating decline). The increase is monotonic because the physical forcing (greenhouse gas accumulation) is monotonic and because infrastructure adaptation lags behind hydrological change. Suppression (0.72): High. Multiple binding mechanisms prevent exit: (1) Water law is based on historical flow — riparian rights and prior appropriation doctrines allocate water based on 20th-century snowpack norms; (2) Infrastructure investment is massive and sunk — dams, irrigation systems, and urban water networks are built for historical hydrology; (3) Agricultural communities lack substitutes — transitioning to rain-fed crops or relocating farms is economically ruinous; (4) Urban growth is politically locked — regions have invested in growth expecting water availability; (5) Ecosystem redistribution would require managed migration across vast distances, which is institutionally infeasible. Theater ratio (0.38): Low-moderate. The constraint is substantially functional (water is genuinely scarce) but some institutional theater exists: water allocation negotiations, conservation campaigns that focus on individual behavior rather than supply mismatch, and projections based on outdated hydroclimatic assumptions.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer at civilizational scale risks classifying snowpack decline as a mountain (immutable natural law). The atmospheric physics are indeed immutable — warming drives the hydrological shift. But the EXTRACTION MECHANISM is contingent: it arises because infrastructure was built for different conditions, because water law encodes historical flows, because demand growth is locked in, and because ecosystem constraints are not binding in current allocation rules. The powerless victim (agriculture, ecosystems) experiences the constraint as snare — no exit, high suppression, high extraction. The moderate agent (urban systems) experiences snare with slightly more agency (constrained exit through desalination or inter-basin transfer, though expensive). If the analytical observer conflates physical law with institutional constraint, they will miss that mitigation pathways exist: demand reduction (politically hard), infrastructure redesign (expensive but feasible), water rights reform (politically locked but structurally possible), and ecosystem prioritization (requires valuation change).
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries exist for this constraint. All identified agents are victims. The directionality values (d) derive from the structural constraint of being dependent on snowpack in a warming climate: (1) Agricultural communities: trapped exit (d ≈ 0.95) — cannot leave without economic collapse; (2) Urban systems: constrained exit (d ≈ 0.75) — can substitute but at high cost; (3) Alpine ecosystems: trapped exit (d = 1.0) — zero degrees of freedom; (4) Infrastructure: trapped exit (d = 0.90) — physical capital cannot be relocated. The high d values across all victims produce high f(d) values, scaling extractiveness upward. The absence of beneficiaries is diagnostic: this is pure extraction, not a mixed coordination-extraction hybrid. The climate system that forces the constraint is not an agent with interests — it is a mechanism producing the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY STRUCTURAL CLARITY: Snowpack decline is unambiguously a snare (not a rope or tangled rope) because: (1) it has no coordination function — the extraction is a side effect of climate physics applied to locked-in institutions, not a mechanism coordinating multiple agents' interests; (2) it has asymmetric extraction — victims have no corresponding beneficiaries; (3) it has high suppression — exit is blocked by both physical law (climate forcing) and institutional law (water rights, sunk infrastructure). The mandatrophy is resolved by observing that the constraint's primary function is not coordination but constraint on water supply. The mountain perspective is a false summit — yes, atmospheric physics is immutable, but the extraction mechanism (the mismatch between infrastructure and hydrology) is not natural law, it is institutional lag. The snare classification stands across all perspectives that account for the structural relationships of dependence, not just the physical forcing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_anthropogenic_vs_natural,
    'What proportion of observed snowpack decline is attributable to anthropogenic climate forcing versus natural climate variability?',
    'Climate attribution studies isolating forced signal from natural modes (PDO, ENSO); comparison of observed trends to climate model ensembles with and without anthropogenic forcing',
    'If >80% anthropogenic: the constraint is a forced extraction mechanism with minimal agency for adaptation. If <50% anthropogenic: natural variability dominates, and the constraint appears more like a natural disaster than a snare. The ε value depends on the agency question — institutional arrangements can mitigate natural variability but not override forced change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attribution_anthropogenic_vs_natural, empirical, 'Proportion of snowpack decline attributable to anthropogenic forcing').

omega_variable(
    adaptive_capacity_reservoir_storage,
    'Can expanded reservoir storage or inter-basin water transfers effectively decouple downstream demand from snowpack timing and volume?',
    'Engineering feasibility analysis; cost-benefit assessment of storage expansion; environmental impact evaluation of new infrastructure; political economy of interstate water transfers',
    'If yes: the constraint is solvable at high cost, reducing ε from 0.68 to ~0.35 (tangled rope with coordination function). If no: the constraint is irreversible within current institutional frameworks, confirming snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity_reservoir_storage, empirical, 'Whether storage expansion can decouple demand from snowpack timing').

omega_variable(
    demand_reduction_political_feasibility,
    'Can downstream water demand be reduced to match declining snowpack supply through conservation, agricultural transition, or population management?',
    'Historical analysis of water conservation adoption rates; cost of agricultural transition to less water-intensive crops; political economy of regional growth constraints',
    'If politically feasible: demand-side adaptation is available as exit option, moving constrained agents toward mobile. If infeasible: trapped agents remain trapped; suppression persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demand_reduction_political_feasibility, preference, 'Political feasibility of demand reduction matching supply decline').

omega_variable(
    temporal_compression_ecosystem_threshold,
    'Is there a critical threshold of snowmelt window compression below which alpine ecosystems experience irreversible state shifts?',
    'Ecological time-series data; species-specific phenological analysis; threshold crossing detection in ecosystem productivity and diversity metrics',
    'If threshold exists and is being approached: alpine ecosystems face extinction risk, confirming powerless victim status. If thresholds are adaptive: some ecosystem transitions are reversible, reducing extraction severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_compression_ecosystem_threshold, empirical, 'Whether ecosystem collapse threshold exists for snowmelt compression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(snowpack_decline_mountain_hydrology, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(snowpack_tr_t0, snowpack_decline_mountain_hydrology, theater_ratio, 0, 0.15).
narrative_ontology:measurement(snowpack_tr_t25, snowpack_decline_mountain_hydrology, theater_ratio, 25, 0.28).
narrative_ontology:measurement(snowpack_tr_t50, snowpack_decline_mountain_hydrology, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(snowpack_be_t0, snowpack_decline_mountain_hydrology, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(snowpack_be_t25, snowpack_decline_mountain_hydrology, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(snowpack_be_t50, snowpack_decline_mountain_hydrology, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(snowpack_decline_mountain_hydrology, resource_allocation).
narrative_ontology:affects_constraint(snowpack_decline_mountain_hydrology, western_water_law_prior_appropriation).
narrative_ontology:affects_constraint(snowpack_decline_mountain_hydrology, agricultural_irrigation_demand_lock).
narrative_ontology:affects_constraint(snowpack_decline_mountain_hydrology, alpine_ecosystem_phenological_mismatch).
narrative_ontology:affects_constraint(snowpack_decline_mountain_hydrology, reservoir_capacity_hydrological_obsolescence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
