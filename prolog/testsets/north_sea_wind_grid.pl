% ============================================================================
% CONSTRAINT STORY: north_sea_wind_grid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_north_sea_wind_grid, []).

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
 *   constraint_id: north_sea_wind_grid
 *   human_readable: The North Sea 100GW Multinational Wind Power Grid Initiative
 *   domain: geopolitical/energy/infrastructure
 *
 * SUMMARY:
 *   The North Sea 100GW wind grid initiative is a geopolitical-economic
 *   constraint system involving ten sovereign states (Germany, Denmark,
 *   Netherlands, Belgium, France, UK, Norway, Sweden, Poland, and Lithuania)
 *   committing to shared offshore infrastructure. The constraint exhibits
 *   tension between genuine coordination gains (load balancing across 500+
 *   km, risk pooling, renewable integration efficiency) and asymmetric
 *   extraction (unequal financing burden, dispatch authority concentration in
 *   major states, externalized fossil transition costs). The same structural
 *   phenomenon appears variously as immutable physics (Mountain), pure
 *   coordination (Rope), mixed coordination-extraction (Tangled Rope),
 *   temporary solution with sunset (Scaffold), degraded regulatory ritual
 *   (Piton), and pure extraction (Snare), depending on the observer's
 *   structural position. The constraint's evolution shows increasing
 *   extractiveness (0.28 → 0.52) as operational complexity reveals
 *   asymmetries in dispatch authority and cost allocation, while theater
 *   ratio remains moderate (0.42 → 0.55) reflecting both substantial
 *   technical requirements and genuine coordination function rather than pure
 *   performance.
 *
 * KEY AGENTS:
 *   - Integrated Grid Operators: Primary beneficiary (institutional/arbitrage) — capture coordination value and operational efficiency gains; can redirect capital if terms worsen
 *   - Major Participating Nations (Germany, Netherlands, Denmark): Primary beneficiary (organized/constrained) — gain energy security and climate credentials; face regulatory constraints but have exit options
 *   - Low-Capacity States (Poland, Lithuania, Belgium): Secondary victim (organized/constrained) — dependent on technology access and financing; locked into interconnection standards favoring high-capacity states
 *   - Coastal Communities: Tertiary victim (powerless/trapped) — bear environmental costs (noise, habitat disruption, fishing exclusion) with minimal negotiation power
 *   - Fossil Fuel Producers (Norway oil industry, coal regions): Mixed victim-beneficiary (moderate/constrained) — face market displacement but gain grid export value and geopolitical influence
 *   - National Energy Regulators: Institutional intermediary (institutional/arbitrage) — maintain performative approval/oversight role despite limited functional control
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent infrastructure choices as immutable physics of renewable integration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_sea_wind_grid, 0.52).
domain_priors:suppression_score(north_sea_wind_grid, 0.48).
domain_priors:theater_ratio(north_sea_wind_grid, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_sea_wind_grid, extractiveness, 0.52).
narrative_ontology:constraint_metric(north_sea_wind_grid, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(north_sea_wind_grid, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(north_sea_wind_grid, tangled_rope).
narrative_ontology:human_readable(north_sea_wind_grid, "The North Sea 100GW Multinational Wind Power Grid Initiative").
narrative_ontology:topic_domain(north_sea_wind_grid, "geopolitical/energy/infrastructure").

domain_priors:requires_active_enforcement(north_sea_wind_grid).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(north_sea_wind_grid, participating_nations).
narrative_ontology:constraint_beneficiary(north_sea_wind_grid, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(north_sea_wind_grid, integrated_grid_operators).
narrative_ontology:constraint_victim(north_sea_wind_grid, fossil_fuel_producers).
narrative_ontology:constraint_victim(north_sea_wind_grid, low_capacity_states).
narrative_ontology:constraint_victim(north_sea_wind_grid, coastal_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL COMMUNITIES (SNARE) — Cannot exit marine spatial planning decisions; bear full cost of offshore wind infrastructure (noise, habitat disruption, fishing exclusion zones) with minimal negotiation power. No mechanism for independent verification of environmental claims or compensation adequacy. Maximum extraction experienced by structurally trapped agents.
constraint_indexing:constraint_classification(north_sea_wind_grid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOW-CAPACITY STATES (TANGLED ROPE) — Constrained by technology access, financing requirements, and regulatory asymmetry. Grid participation requires infrastructure investment these nations cannot fund independently, creating dependency on major participants. Yet participation also provides clean energy access and climate commitment credentials. Active enforcement mechanisms (interconnection standards, grid dispatch rules) create asymmetric extraction favoring high-capacity states, while coordination benefits are genuine but unevenly distributed.
constraint_indexing:constraint_classification(north_sea_wind_grid, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTEGRATED GRID OPERATORS (ROPE) — Experiences grid as pure coordination mechanism: shared infrastructure enables real-time dispatch optimization, load balancing across borders, and risk pooling. Net beneficiary with immediate exit options (can shift capital to other markets). Extraction runs toward these actors. The constraint solves their collective action problem of managing variable renewable output across jurisdictions. Low coercion experienced — participation is voluntary exit.
constraint_indexing:constraint_classification(north_sea_wind_grid, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MAJOR PARTICIPATING NATIONS (SCAFFOLD) — Organized state actors with high capacity see this as temporary coordination framework with built-in sunset: grid is designed for 30-40 year operational life, with explicit decommissioning clauses and technology transition triggers. High suppression (regulatory requirements, interconnection standards, dispute resolution procedures) is tolerated because it declines as decentralized generation and storage technologies mature. EU Green Deal and net-zero commitments provide exit rationale.
constraint_indexing:constraint_classification(north_sea_wind_grid, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: NATIONAL ENERGY REGULATORS (PITON) — Regulatory approval and oversight bodies have largely performative function: they certify environmental reviews and grid safety but lack real enforcement power over multinational infrastructure. Theater ratio (0.55) reflects extensive permitting theater while core technical decisions are made by grid operators and major state investors. The regulatory architecture persists through institutional inertia despite limited functional control — alternative governance models (private operator consortia) could deliver grid coordination with less regulatory overhead.
constraint_indexing:constraint_classification(north_sea_wind_grid, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FOSSIL FUEL PRODUCERS (TANGLED ROPE) — Face extraction via grid integration that undermines coal and natural gas markets, yet also benefit from export infrastructure (Norway as electricity exporter, Denmark's grid role enhancing technical credibility). Constrained exit: cannot fully exit North Sea governance without losing geopolitical influence. Active enforcement mechanisms lock in fossil displacement trajectory. Mixed experience: both victim (declining fuel demand) and beneficiary (grid export value).
constraint_indexing:constraint_classification(north_sea_wind_grid, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a systems physics perspective, integrating high-proportion variable renewables across load centers separated by 500+ km requires real-time demand-response coordination that appears immutable: the constraint is the speed-of-light limit on information propagation, battery discharge rates, and electromechanical generator dynamics. However, this naturalization masks contingent choices: storage deployment, demand management, and interconnection investment are policy variables, not laws of physics. The engine's false summit detector should flag this perspective as naturalization.
constraint_indexing:constraint_classification(north_sea_wind_grid, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_sea_wind_grid_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(north_sea_wind_grid, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_sea_wind_grid, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(north_sea_wind_grid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(north_sea_wind_grid, TR),
    TR >= 0.70.

:- end_tests(north_sea_wind_grid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately-high. The grid initiative requires €100B+ investment, with unequal burden distribution across participating nations. Major states (Germany, Netherlands) shoulder greater financial burden but receive greater dispatch authority and grid operator benefits. Low-capacity states depend on imported technology and financing, creating leverage asymmetry. However, this is not pure extraction: genuine coordination gains exist (15-25% efficiency improvement from load pooling, risk reduction from geographic diversity). The intermediate extractiveness reflects this hybrid: extraction component exists but coordination component is substantial. Theater ratio (0.55): Moderate. Extensive environmental assessment, regulatory certification, and multinational coordination procedures reflect genuine complexity and legitimate governance requirements, not pure theatrical compliance. However, core technical decisions (grid architecture, dispatch protocols) are made by grid operators and major-state representatives with limited participatory input from smaller states or affected communities. Suppression (0.48): Moderate. Regulatory requirements, interconnection standards, and dispute resolution procedures create barriers to exit or modification, but these are not absolute: states can theoretically exit framework, albeit at significant cost. The constraint requires active enforcement through multinational agreements and regulatory harmonization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how infrastructure projects create perspectival chasms across power asymmetries. The same 100GW grid is a rational coordination solution (major-state perspective: Rope/Scaffold), a financing trap (low-capacity-state perspective: Tangled Rope/Snare), a displacement mechanism (fossil-producer perspective: Tangled Rope), an externality absorption site (coastal-community perspective: Snare), and naturalized physics (civilizational-analytical perspective: Mountain). No single type is 'correct' — each is the genuine structural reality of a different agent class. The mandatrophy resolves by showing that the presheaf of classifications IS the constraint's true structure: the coordination gains are real, AND the extraction is real, AND the cost externalization is real. They are not contradictory but simultaneous.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality values are determined by structural position and exit capacity. Grid operators and major institutional participants (Germany, Netherlands) are beneficiaries with arbitrage exit → low d → low/negative χ. Low-capacity states (Poland, Lithuania) are forced participants with constrained exit → mid-to-high d → moderate χ. Coastal communities and fishing interests are trapped victims → maximum d → maximum χ. Fossil producers are complex: simultaneous victims (fuel displacement) and beneficiaries (grid export opportunity) → mid d → mid χ. These derivations flow from the beneficiary/victim declarations and exit_options assigned to each perspective. The engine computes d from these structural parameters and applies the sigmoid f(d) to produce experienced extractiveness. No override is needed — the base structural data (who benefits, who exits, what power level) produces accurate directionality automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC FRAMEWORK: The North Sea wind grid demonstrates mandatrophy resolution through perspectival decomposition. The question 'Is this coordination or extraction?' has no single answer — it is both, differentially distributed across agents. Major states legitimately solve a coordination problem (load balancing, renewable integration, climate commitments). Low-capacity states experience extraction (financing burden asymmetry, dispatch authority concentration). Coastal communities bear externalized costs. Fossil regions face displacement without transition support. The constraint is simultaneously Rope (for operators), Tangled Rope (for low-capacity states and fossil regions), Snare (for coastal communities), and Scaffold (for major states viewing it as temporary). The presheaf over the observation space is the true structure. Any attempt to force a single classification either naturalizes the coordination gains (falsely calling it Mountain or pure Rope) or oversimplifies the coordination function (falsely calling it pure Snare). The framework's power is to hold all perspectives simultaneously: the constraint is internally contradictory from a monolithic viewpoint, but coherent from a perspectival one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    financing_burden_asymmetry,
    'Does the €100B+ total project cost create extraction through unequal financing burden, or does it represent fair risk-sharing for legitimate coordination gains?',
    'Comparative analysis of financing contribution vs energy received vs long-term cost recovery; audits of subsidy distribution across participating nations; modeling of capacity utilization patterns by country over 30-year operational life',
    'If burden asymmetric: constraint shifts from Tangled Rope toward Snare for low-capacity states. If fairly distributed: Tangled Rope classification holds; extraction component is legitimate coordination tax.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financing_burden_asymmetry, empirical, 'Whether project financing burden creates asymmetric extraction').

omega_variable(
    grid_dispatch_control_locus,
    'Does grid dispatch authority genuinely belong to multinational operator consensus, or do major states retain veto power that converts coordination into extraction?',
    'Analysis of grid dispatch protocols and voting structures; case studies of load-sharing conflicts; review of dispute resolution outcomes favoring/disfavoring low-capacity states',
    'If consensus-driven: Rope classification is valid. If major states retain veto: constraint is Tangled Rope or Snare from low-capacity perspective. Critical for assessing suppression component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_dispatch_control_locus, empirical, 'Locus of grid dispatch authority and decision-making power').

omega_variable(
    stranded_fossil_asset_externality,
    'Are grid integration benefits fairly distributed, or does rapid fossil displacement externalize closure costs onto coal-dependent regions without compensation or transition support?',
    'Tracking of regional unemployment, infrastructure investment, and just-transition spending in coal-dependent areas; comparison of energy prices pre/post grid integration by region; equity analysis of grid revenue distribution',
    'If transition externalized: constraint is Snare for affected regions despite global climate benefit. If transition supported: constraint is Scaffold with genuine sunset clauses and compensation mechanisms. Affects classification from regional vs global perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_fossil_asset_externality, empirical, 'Whether fossil phase-out costs are externalized or managed through transition support').

omega_variable(
    technical_lock_in_duration,
    'Is the 30-40 year operational horizon a genuine sunset clause enabling technology transition, or does grid infrastructure create path dependency that extends extraction beyond the stated decommissioning timeline?',
    'Technology roadmap analysis for grid replacement; modeling of economics for alternative architectures (distributed storage, local generation) post-2055; analysis of asset retirement practices in similar multinational infrastructure projects',
    'If genuine sunset: Scaffold classification holds. If lock-in extends beyond timeline: constraint degrades from Scaffold to Piton (performative transition rhetoric with actual inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_lock_in_duration, empirical, 'Whether grid infrastructure creates genuine technology transition opportunity or path lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(north_sea_wind_grid, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nswind_tr_t0, north_sea_wind_grid, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nswind_tr_t10, north_sea_wind_grid, theater_ratio, 10, 0.52).
narrative_ontology:measurement(nswind_tr_t20, north_sea_wind_grid, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(nswind_be_t0, north_sea_wind_grid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nswind_be_t10, north_sea_wind_grid, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(nswind_be_t20, north_sea_wind_grid, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(north_sea_wind_grid, global_infrastructure).
narrative_ontology:affects_constraint(north_sea_wind_grid, eu_energy_security_transition).
narrative_ontology:affects_constraint(north_sea_wind_grid, north_sea_maritime_sovereignty).
narrative_ontology:affects_constraint(north_sea_wind_grid, fossil_fuel_economic_transition).

% DUAL FORMULATION NOTE:
% The North Sea wind grid can be decomposed into three structurally distinct constraints: (1) the renewable integration coordination problem (ε≈0.25, pure Rope), (2) the financing and dispatch authority asymmetry (ε≈0.52, Tangled Rope), and (3) the environmental externality burden (ε≈0.65, Snare). This story treats the grid as a unified constraint with ε=0.52 capturing the hybrid nature. Alternative decomposition: separate stories for grid-as-coordination vs grid-as-extraction-mechanism, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(north_sea_wind_grid, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
