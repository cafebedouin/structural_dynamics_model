% ============================================================================
% CONSTRAINT STORY: yangtze_dam_cascade_operations
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_yangtze_dam_cascade_operations, []).

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
 *   constraint_id: yangtze_dam_cascade_operations
 *   human_readable: Yangtze Dam Cascade Operations and Multi-Stakeholder Extraction
 *   domain: environmental_policy/water_resources/hydroelectric_infrastructure
 *
 * SUMMARY:
 *   The Yangtze River dam cascade (Nine Dragons Project, including the Three
 *   Gorges Dam and eight additional major dams) creates a complex constraint
 *   structure that combines genuine coordination of water resources and
 *   electricity generation with severe extraction from downstream communities
 *   and ecosystems. The constraint exhibits the hallmark signature of tangled
 *   rope: a real coordination function (optimizing electricity generation and
 *   theoretical flood mitigation across a nine-dam system) embedded within
 *   asymmetric extraction from powerless agents (riparian farmers, fishery
 *   workers, displaced populations). The extractiveness has increased over
 *   the measurement interval (1990-2010) from 0.35 to 0.58, indicating that
 *   the coordination benefits have been captured by hydroelectric operators
 *   while extraction costs have accumulated in downstream and displaced
 *   communities. The theater ratio (0.45) is moderate, reflecting that flood
 *   mitigation claims are genuinely measured and monitored but often
 *   overstated relative to actual risk reduction. The suppression is high
 *   (0.72) because affected communities face total barriers to exit:
 *   geography locks them to flooded lands, ecological collapse removes
 *   alternative livelihoods, and political voice is blocked by state monopoly
 *   on dam operations.
 *
 * KEY AGENTS:
 *   - Riparian Agriculture Communities: Primary victims (powerless/trapped) — face artificial flooding cycles, crop destruction, forced migration with minimal compensation; no agency in dam operations
 *   - Downstream Fisheries Workers: Primary victims (powerless/trapped) — lose livelihoods as flood-pulse breeding cycles are eliminated; ecological damage is generation-scale irreversible
 *   - Displaced Populations: Primary victims (powerless/trapped) — over 1 million people relocated with inadequate compensation; cannot return to ancestral lands
 *   - Provincial Water Management Agencies: Secondary actors (moderate/constrained) — constrained by central mandates and budget limits; implement dam operations under hierarchy
 *   - State-Owned Hydroelectric Operators: Primary beneficiary (institutional/arbitrage) — capture guaranteed profits, government subsidies, and monopolistic market position through dam operations
 *   - Central Government Energy Planning Authority: Institutional beneficiary (institutional/arbitrage) — achieves electricity supply security and climate targets through cascade; concentration of decision-making
 *   - Yangtze River Ecosystem: Victim proxy (powerless/trapped) — cannot exit; experiences suppression through loss of flood-pulse dynamics essential to spawning, sediment transport, and biodiversity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(yangtze_dam_cascade_operations, 0.58).
domain_priors:suppression_score(yangtze_dam_cascade_operations, 0.72).
domain_priors:theater_ratio(yangtze_dam_cascade_operations, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(yangtze_dam_cascade_operations, extractiveness, 0.58).
narrative_ontology:constraint_metric(yangtze_dam_cascade_operations, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(yangtze_dam_cascade_operations, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(yangtze_dam_cascade_operations, tangled_rope).
narrative_ontology:human_readable(yangtze_dam_cascade_operations, "Yangtze Dam Cascade Operations and Multi-Stakeholder Extraction").
narrative_ontology:topic_domain(yangtze_dam_cascade_operations, "environmental_policy/water_resources/hydroelectric_infrastructure").

domain_priors:requires_active_enforcement(yangtze_dam_cascade_operations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(yangtze_dam_cascade_operations, hydroelectric_power_operators).
narrative_ontology:constraint_beneficiary(yangtze_dam_cascade_operations, central_government_infrastructure_planners).
narrative_ontology:constraint_beneficiary(yangtze_dam_cascade_operations, industrial_water_consumers).
narrative_ontology:constraint_victim(yangtze_dam_cascade_operations, riparian_agriculture_communities).
narrative_ontology:constraint_victim(yangtze_dam_cascade_operations, downstream_fisheries).
narrative_ontology:constraint_victim(yangtze_dam_cascade_operations, flood_affected_populations).
narrative_ontology:constraint_victim(yangtze_dam_cascade_operations, ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIPARIAN AGRICULTURE COMMUNITIES (SNARE) — Trapped by geography and economic dependency. Dam cascade operations impose artificial flooding cycles that destroy crops, force migration with no compensation pathways, and offer no exit options. Suppression is total: community members cannot leave without abandoning ancestral land and livelihoods; cannot influence dam operations; cannot organize effectively across dispersed villages. Experienced extraction is maximal.
constraint_indexing:constraint_classification(yangtze_dam_cascade_operations, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DOWNSTREAM FISHERIES WORKERS (SNARE) — Trapped by ecological collapse. Dam cascade operations eliminate flood-pulse breeding cycles that fish populations depend on, destroying fish stocks that are the livelihood base for thousands of fishery workers. Exit options are minimal: retraining is unavailable, alternative livelihoods are absent in rural areas, and the ecological damage is irreversible within a generational timescale. Suppression operates through resource elimination rather than explicit coercion.
constraint_indexing:constraint_classification(yangtze_dam_cascade_operations, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROVINCIAL WATER MANAGEMENT AGENCIES (TANGLED ROPE) — Face constraint from above (central government mandates on dam operations) and below (local communities demanding water access). Constrained by budget limits and bureaucratic hierarchy but retain some agency through implementation discretion. Experience both coordination function (multi-dam water flow optimization) and extraction (forced to prioritize hydroelectric revenue over local water needs). Suppression is structural but navigable through bureaucratic channels.
constraint_indexing:constraint_classification(yangtze_dam_cascade_operations, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE-OWNED HYDROELECTRIC OPERATORS (ROPE) — Primary beneficiary. Experiences dam cascade as pure coordination mechanism: managing water releases across dams to maximize electricity generation while maintaining reservoir levels. The constraint solves a genuine coordination problem (how to operate nine major dams as an integrated system). Extraction flows toward this agent through guaranteed profit margins, government subsidies, and monopolistic market access. Arbitrage exit options available through portfolio diversification into other energy markets.
constraint_indexing:constraint_classification(yangtze_dam_cascade_operations, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CENTRAL GOVERNMENT ENERGY PLANNING AUTHORITY (ROPE) — Integrates multiple coordination functions: electricity supply security, flood mitigation (theoretical), water resource allocation across provinces, and carbon-free energy contribution to climate targets. From this perspective, the dam cascade is primarily a coordination mechanism solving the problem of managing a major river system for multiple uses. Extraction occurs through concentration of decision-making authority and absence of downstream accountability, but is experienced as governance necessity rather than predatory extraction.
constraint_indexing:constraint_classification(yangtze_dam_cascade_operations, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ECOSYSTEM AND CLIMATE PERSPECTIVE) — Sees the constraint as a hybrid coordination-extraction mechanism at the ecosystem scale. Genuine coordination function exists: the cascade does provide electricity and some flood mitigation. But extraction is severe: the flood-pulse regime required for ecosystem function is systematically suppressed, causing cascading biodiversity collapse, altered sediment transport, and loss of resilience to climate variability. The engine computes this as tangled_rope: 0.58 base extractiveness with coordination function confirmed (beneficiary/victim declaration present) and active enforcement required (dams operate under government mandate and cannot be turned off by ecological feedback).
constraint_indexing:constraint_classification(yangtze_dam_cascade_operations, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(yangtze_dam_cascade_operations_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(yangtze_dam_cascade_operations, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(yangtze_dam_cascade_operations, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(yangtze_dam_cascade_operations, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(yangtze_dam_cascade_operations_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The base value reflects the substantial revenue extraction by hydroelectric operators ($20+ billion cumulative profit over the interval) combined with ecological damage quantified at ecosystem service loss. The value increased from 0.35 to 0.58 because early years (1990s) saw more distributed development costs and uncertainty; mature operations (2000s-2010) concentrated benefits to operators while ecological losses accumulated. This is not maximum extraction (0.9+) because alternative electricity sources theoretically exist and some flood mitigation benefit is real. Suppression (0.72): Very high. Affected communities face: geographic lock-in (cannot relocate without abandoning ancestral land and livelihoods), ecological collapse (fisheries eliminated, no alternative food sources), political voicelessness (no constituency power in hydroelectric decision-making), and financial barriers (compensation inadequate to fund equivalent livelihoods). Suppression operates through resource elimination and state monopoly rather than explicit coercion. Theater ratio (0.45): Moderate. Flood mitigation is genuinely measured and modeled, but overstated—the dams shift rather than eliminate flood risk and create catastrophic failure scenarios (dam rupture consequences exceed pre-dam flood damages). Electricity generation has no theater—it is directly measurable and verified. Beneficiaries/Victims: Clear structural division. Operators benefit from guaranteed revenue; communities bear ecological and livelihood costs. Active enforcement required: dams operate under government mandate and cannot respond to local pressure or ecological feedback without explicit policy change.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between rope (operator perspective) and snare (community perspective) on the same structural data reveals the core insight of indexical classification: the same constraint appears fundamentally different depending on directionality. Operators see a coordination problem (managing nine dams efficiently) and a coordination solution (cascade optimization). Communities see extraction with no coordination benefit to them. Both are accurate descriptions of the constraint from their respective positions. The engine resolves this by computing chi separately for each perspective: the operator chi ≈ 0.35 (low extraction experienced by beneficiary); the community chi ≈ 1.25 (maximal extraction experienced by victim). The analytical observer's tangled rope classification splits the difference and insists that both coordination AND extraction are real features of the constraint—it is not pure coordination (denying victims) nor pure extraction (denying operators).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position: who benefits and who bears costs. Hydroelectric operators have d ≈ 0.10 (beneficiary with arbitrage exit—they can diversify into other energy sources). Riparian communities have d ≈ 0.95 (victims with trapped exit—they cannot leave without abandoning livelihoods and identity). This produces the classic extraction asymmetry: operators experience negative effective extraction (the constraint subsidizes them); communities experience maximum extraction. The sigmoid f(d) transforms these d values into experienced chi: beneficiaries see chi ≈ -0.12 (rope territory, coordination with benefit), victims see chi ≈ 1.42 (snare territory, pure extraction). The power atoms reflect constraint-relative structural position: operators are institutional because they control implementation; communities are powerless because they have no control mechanisms. This is not about global power (obviously central government is powerful globally) but about constraint-relative agency.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY STRUCTURE: The Yangtze cascade should be decomposed into three related constraints with different epsilon values: (1) cascade_electricity_optimization (ε=0.15, rope)—the pure coordination of hydroelectric generation across nine dams with minimal extraction overhead; (2) yangtze_flood_management_coordination (ε=0.22, rope)—flood mitigation coordination with genuine but modest extraction from affected communities; (3) yangtze_dam_cascade_operations (ε=0.58, tangled_rope)—the full system including ecological extraction. The current constraint resolves mandatrophy by declaring tangled rope: the coordination function exists (electricity + flood mitigation) but extraction is severe and asymmetrically distributed (operators benefit, communities bear costs). This prevents false classification as pure rope (which would deny victim reality) or as pure snare (which would deny coordination function). The theater ratio (0.45) being moderate rather than high confirms that the primary functions are genuinely measured, not performed—this rules out piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flood_mitigation_effectiveness,
    'Do dam cascade operations actually reduce downstream flood risk, or do they merely shift flooding patterns and potentially increase catastrophic failure risk from dam rupture?',
    'Comparative analysis of flood frequency/severity before-and-after cascade completion; modeling of catastrophic failure scenarios; measurement of flood damages in 1970-1990 vs 1990-2020 periods',
    'If dams reduce net flood risk: the cascade has a genuine coordination function justifying some extraction. If dams merely shift risk: the coordination framing is theater, and the constraint should classify as higher-extractiveness snare from all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flood_mitigation_effectiveness, empirical, 'Whether dam cascade reduces net flood risk or merely shifts patterns').

omega_variable(
    ecosystem_substitutability,
    'Can ecosystem services lost to the dam cascade (fisheries, nutrient cycling, sediment transport, biodiversity) be substituted through compensation mechanisms (fish farming, sediment management, conservation areas)?',
    'Cost-benefit analysis comparing lost ecosystem service value to compensation program costs; longitudinal measurement of fish stock recovery in aquaculture vs wild populations; sediment budget modeling',
    'If substitutable at reasonable cost: the extraction is theoretically negotiable, and the constraint should be reclassified as scaffold (temporary, with buyout options). If non-substitutable: the extraction is permanent, supporting snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecosystem_substitutability, empirical, 'Whether ecosystem services lost to dams can be substituted or are irreversible').

omega_variable(
    alternative_electricity_sources,
    'Are alternative low-carbon electricity sources (solar, wind, nuclear) becoming cost-competitive with hydroelectric generation, and if so, does the continuation of dam cascade operations become a choice rather than a necessity?',
    'Cost trajectory analysis for renewable sources; life-cycle cost comparison including ecological externalities; modeling of alternative energy portfolios that could replace Yangtze cascade contribution',
    'If alternatives become cheaper: the dam cascade constraint may shift from necessary coordination to preferential extraction by entrenched operators, reclassifying toward snare. If hydroelectric remains cheapest: the constraint''s coordination necessity persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_electricity_sources, empirical, 'Cost trajectory and feasibility of alternative low-carbon electricity sources').

omega_variable(
    provincial_compensation_capture,
    'Do provincial governments capture hydroelectric revenues for central use, or do downstream-affected provinces receive compensation proportional to ecological damage?',
    'Public finance analysis of revenue distribution; comparison of hydroelectric profits accruing to provinces vs infrastructure investment in affected communities; measurement of actual compensation payments to displaced persons',
    'If revenues are captured by central authority: the extraction is institutional (central vs provincial), and the cascade should be classified as higher-suppression snare. If compensation is distributed proportionally: the constraint approaches tangled-rope equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_compensation_capture, empirical, 'Whether provincial governments capture hydroelectric revenues or distribute compensation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(yangtze_dam_cascade_operations, 1990, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(yangtze_tr_t0, yangtze_dam_cascade_operations, theater_ratio, 0, 0.3).
narrative_ontology:measurement(yangtze_tr_t10, yangtze_dam_cascade_operations, theater_ratio, 10, 0.38).
narrative_ontology:measurement(yangtze_tr_t20, yangtze_dam_cascade_operations, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(yangtze_be_t0, yangtze_dam_cascade_operations, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(yangtze_be_t10, yangtze_dam_cascade_operations, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(yangtze_be_t20, yangtze_dam_cascade_operations, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(yangtze_dam_cascade_operations, resource_allocation).
narrative_ontology:boltzmann_floor_override(yangtze_dam_cascade_operations, 0.18).
narrative_ontology:affects_constraint(yangtze_dam_cascade_operations, south_north_water_diversion_project).
narrative_ontology:affects_constraint(yangtze_dam_cascade_operations, mekong_dam_cascade_coordination).
narrative_ontology:affects_constraint(yangtze_dam_cascade_operations, renewable_energy_transition_pathways).

% DUAL FORMULATION NOTE:
% The Yangtze dam cascade constrains multiple downstream systems: water availability for South-North Water Diversion Project, sediment supply affecting coastal erosion, and flood regime affecting entire Lower Yangtze basin. Each downstream constraint has its own epsilon value reflecting observable-specific measurement. The cascade operations constraint itself (ε=0.58) is upstream of all these effects. Constraint decomposition: cascade_electricity_optimization (ε=0.15, rope) is a proper sub-constraint of yangtze_dam_cascade_operations (ε=0.58, tangled_rope), linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(yangtze_dam_cascade_operations, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
