% ============================================================================
% CONSTRAINT STORY: photovoltaic_deployment_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_photovoltaic_deployment_ceiling, []).

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
 *   constraint_id: photovoltaic_deployment_ceiling
 *   human_readable: Photovoltaic Deployment Ceiling: Grid Integration and Market Extraction
 *   domain: energy_infrastructure/renewable_transition
 *
 * SUMMARY:
 *   The photovoltaic deployment ceiling emerges at the intersection of
 *   technical grid constraints and institutional incentive structures that
 *   favor incumbent utilities and utility-scale developers over distributed
 *   generation. This constraint exhibits a mixed institutional structure:
 *   genuine coordination problems (integrating variable renewable sources)
 *   are entangled with extraction mechanisms (interconnection queue delays,
 *   cost allocation rules, studyupgrade requirements that disproportionately
 *   burden small developers). The constraint's theater_ratio (0.64) reflects
 *   that interconnection study procedures perform legitimizing
 *   rituals—detailed technical analysis—while delivering administrative
 *   delays that serve market protection rather than grid safety. The
 *   extractiveness has risen from 0.28 to 0.52 over 15 years as distributed
 *   solar deployment accelerated past the administrative process capacity
 *   designed for sporadic generation additions. The suppression value (0.58)
 *   indicates substantial barriers: $5k-$50k upfront study costs, 5-10 year
 *   queue delays, arbitrary technical requirement escalation, and distributed
 *   generation's structural disadvantage in rate structures designed for
 *   centralized supply. This constraint is at a critical juncture: grid
 *   modernization technologies (battery storage, smart inverters,
 *   synchronverters) are eroding the technical legitimacy of the ceiling,
 *   while incumbent utility cost-recovery mechanisms are evolving to capture
 *   emerging revenue streams from distributed resources. The scaffold
 *   perspective identifies a genuine sunset: as distributed intelligence and
 *   storage commoditize, the current queuing and study protocols will become
 *   visibly theatrical and will be displaced by standards-based
 *   interconnection (IEEE 1547 revision, UL 4703). The question is whether
 *   the sunset occurs through technical displacement or through political
 *   reform.
 *
 * KEY AGENTS:
 *   - Distributed Solar Developers: Primary victim (powerless/trapped) — small residential/commercial installers bearing full cost of queue delays and arbitrary requirements; cannot exit without stranded investment
 *   - Incumbent Utilities: Primary beneficiary (institutional/arbitrage) — capture extended cost recovery, avoid distributed competition during queue delays, control rate structures that disadvantage distributed generation
 *   - Grid Operators: Secondary beneficiary (institutional/arbitrage) — manage coordination role while using queue as control mechanism; benefit from reduced variable source penetration during cost-recovery periods
 *   - Large-Scale Solar Developers: Secondary beneficiary (powerful/arbitrage) — have resources to navigate queues, benefit from barrier to distributed competition, operate in utility-scale market protected from distributed encroachment
 *   - Grid Modernization Coalition: Organized agents (organized/constrained) — battery vendors, smart-inverter manufacturers, distribution software firms seeing sunset in 10-15 years as technical standards replace queue theater
 *   - Interconnection Study Practitioners: Institutional actors (institutional/arbitrage) — consulting firms and utility engineers who perform the studies; benefit from continued queue demands and study complexity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent AC synchronous grid design as immutable physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(photovoltaic_deployment_ceiling, 0.52).
domain_priors:suppression_score(photovoltaic_deployment_ceiling, 0.58).
domain_priors:theater_ratio(photovoltaic_deployment_ceiling, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(photovoltaic_deployment_ceiling, extractiveness, 0.52).
narrative_ontology:constraint_metric(photovoltaic_deployment_ceiling, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(photovoltaic_deployment_ceiling, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(photovoltaic_deployment_ceiling, tangled_rope).
narrative_ontology:human_readable(photovoltaic_deployment_ceiling, "Photovoltaic Deployment Ceiling: Grid Integration and Market Extraction").
narrative_ontology:topic_domain(photovoltaic_deployment_ceiling, "energy_infrastructure/renewable_transition").

domain_priors:requires_active_enforcement(photovoltaic_deployment_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(photovoltaic_deployment_ceiling, incumbent_utilities).
narrative_ontology:constraint_beneficiary(photovoltaic_deployment_ceiling, grid_operators).
narrative_ontology:constraint_beneficiary(photovoltaic_deployment_ceiling, interconnection_gatekeepers).
narrative_ontology:constraint_victim(photovoltaic_deployment_ceiling, distributed_solar_developers).
narrative_ontology:constraint_victim(photovoltaic_deployment_ceiling, grid_resilience).
narrative_ontology:constraint_victim(photovoltaic_deployment_ceiling, decentralization_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISTRIBUTED SOLAR DEVELOPER (SNARE) — Faces interconnection queues lasting 5-10 years, upfront study costs ($5k-$50k), and arbitrary technical requirement escalation. Cannot exit: investment is stranded without grid connection. Maximum extraction from powerless position with no alternatives.
constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL GRID OPERATOR (TANGLED ROPE) — Faces genuine coordination problem (managing variable renewable inflows) and also benefits from bottleneck enforcement (extended power purchase agreements, ratepayer cost recovery). Constrained by technical feasibility and regulatory mandates; some agency but significant extraction asymmetry.
constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT UTILITY (ROPE) — Experiences the constraint as coordination of their own essential function: maintaining grid stability. Captures extraction through extended cost-recovery periods and avoided stranded asset write-downs. Net beneficiary with full arbitrage options (can lobby, restructure rates, acquire distributed capacity).
constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GRID MODERNIZATION COALITION (SCAFFOLD) — Organized agents (battery manufacturers, smart-inverter vendors, distribution companies) see the bottleneck as a temporary coordination failure solvable via technical standards (IEEE 1547, UL 4703) and distributed intelligence. Sunset visible in 10-15 years as hardware and software commoditize; extraction is tolerated because the constraint is perceived as diminishing.
constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LARGE SOLAR DEVELOPER (TANGLED ROPE) — Powerful actors with resources for extended queue navigation and transmission-level interconnection. Still faces extraction (higher upfront costs, delayed revenue) but also benefits from barrier to distributed competition. Mixed experience: extraction is present but navigable for well-capitalized actors.
constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERCONNECTION STUDY PROCESS (PITON) — The formal process (impact studies, upgrade requirements, cluster studies) performs a legitimizing ritual: appears to ensure grid safety but has degraded in function as solar deployment accelerated beyond the process's designed capacity. Theater ratio high (0.64) because most queue delays are administrative batching and cost allocation theater rather than actual technical analysis. Maintained through institutional inertia despite low correlation between study rigor and actual interconnection success.
constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL LIMITS VIEW (MOUNTAIN) — From a long-horizon universal perspective, inertial grids have fundamental physical limits on non-synchronous power penetration: dynamic stability, harmonic distortion, and frequency response cannot be overcome by administrative process. This view naturalizes the constraint as immutable physics. However, structural data reveals this as a false summit: the 'physical limit' is actually a contingent design choice (AC synchronous grids). Alternative grid architectures (DC microgrids, hybrid AC/DC, synchronverters) exist but are excluded by incumbent technology lock-in, not by physics.
constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(photovoltaic_deployment_ceiling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(photovoltaic_deployment_ceiling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(photovoltaic_deployment_ceiling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(photovoltaic_deployment_ceiling, TR),
    TR >= 0.70.

:- end_tests(photovoltaic_deployment_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over interval. The constraint extracts significant value from distributed developers through queue delays (5-10 years = delayed revenue), study costs ($5k-$50k upfront), and administrative burden. However, extractiveness is not maximal (0.70+) because some distributed projects do eventually interconnect and the extraction is partial rather than complete (developers retain option to self-consume or pursue other markets). The 15-year trajectory shows rising extractiveness: as deployment accelerated from 2010-2025, the queue bottleneck became more severe (administrative theater intensified), and extraction mechanisms became more visible (cost-allocation changes, study requirement escalation). Suppression (0.58): Moderate-high. Barriers to exit include upfront costs that cannot be recovered if project is abandoned, sunk costs in site assessment and permitting, and lack of alternative grid-connection pathways for distributed generation (some jurisdictions have microgrids or standalone battery options, but most have none). Suppression is not total (0.70+) because distributed developers do eventually succeed in significant numbers (U.S. installed ~40 GW cumulative distributed solar by 2025). Theater ratio (0.64): High, and rising. The interconnection study process performs legitimacy theater—detailed technical analysis that gives the appearance of rigorous grid safety assessment—but increasingly disconnected from actual grid safety outcomes. Most queue delays are administrative batching and cost-allocation negotiation, not technical analysis. As distributed solar deployment has accelerated, the gap between actual technical requirements (which studies increasingly show can accommodate much higher penetration) and actual upgrade requirements (which spike unpredictably based on queue composition) has widened. Theater ratio rose from 0.35 (when queues were short and studies could plausibly be technical) to 0.64 (when queues are long and studies are obviously batching mechanisms).
 *
 * PERSPECTIVAL GAP:
 *   The dramatic gap between snare (distributed developer) and rope (incumbent utility) for nearly identical base properties reveals asymmetric extraction: the constraint coordinates grid integration (genuine function, supporting rope classification) while simultaneously using interconnection processes to extract value from distributed developers and protect utility-scale market share (supporting snare classification for targets). This is the canonical tangled_rope structure: both beneficiary and victim functions are real and structural, not artifacts of perspective. The scaffold perspective's projected sunset is empirically grounded in battery cost curves and smart-inverter standards trajectories; the piton perspective's degraded theater is observable in rising gap between study recommendations and actual requirements.
 *
 * DIRECTIONALITY LOGIC:
 *   Utilities derive d from beneficiary + arbitrage = 0.12-0.18 (negative or near-zero effective extraction). Distributed developers derive d from victim + trapped = 0.92-0.95 (maximum effective extraction). Grid operators derive d from both roles + constrained = 0.50-0.55 (symmetric). Large-scale developers derive d from secondary beneficiary + arbitrage = 0.28-0.32. The dispatcher overrides this with directionality_overrides for specific agent classes if field evidence shows the derivation misses structural nuance (e.g., if some utilities are captured by distributed solar industry associations despite institutional status, lowering their d further).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy through structural decomposition. The 'deployment ceiling' is not a single constraint but the intersection of two: (1) grid_technical_integration_coordination (genuine; ε ≈ 0.15-0.20, rope from all perspectives, no victims) and (2) utility_market_protection_mechanism (extractive; ε ≈ 0.60-0.70, snare from distributed perspective, rope from utility perspective). The first is pure coordination; the second is pure extraction disguised as coordination. The tangled_rope classification at the aggregate level reflects that they are empirically entangled—same institutional actors, same regulatory framework, same queue process. But the mandatrophy resolution is to recognize that the ceiling's severity (extracted value of $billions in delayed distributed solar deployment) comes overwhelmingly from the second constraint, not from genuine grid technical limits. Grid modernization (batteries, synchronverters) is making the technical coordination easier, but cost-recovery mechanisms and rate structures (which enforce the extraction component) are evolving to maintain the extraction as technical justification erodes. The scaffold sunset depends on whether technical standards (IEEE 1547, UL 4703) are adopted with sufficient bandwidth to bypass queue procedures entirely. If adopted: sunset in 10-15 years. If regulatory pressure maintains queue theater: extraction persists and constraint becomes piton (theater-driven inertia).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_vs_regulatory_bottleneck,
    'Is the deployment ceiling driven by genuine technical grid stability limits or by regulatory/administrative barriers masquerading as technical requirements?',
    'Comparative analysis: interconnection success rates in deregulated vs regulated markets; grid simulation studies showing actual stability limits vs actual utility upgrade requirements; pilot programs with streamlined interconnection procedures',
    'If technical: mountain classification is correct and ceiling reflects unavoidable physics. If regulatory: constraint is tangled_rope with artificially suppressed extraction (utilities maintain overhead as policy choice). Current evidence suggests 60-70% regulatory, 30-40% technical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_vs_regulatory_bottleneck, empirical, 'Technical vs regulatory bottleneck composition').

omega_variable(
    alternative_grid_architecture_viability,
    'Are synchronous AC grid requirements (driving the current deployment ceiling) fundamentally necessary or a technology lock-in choice?',
    'Technical analysis of DC microgrids, hybrid AC/DC architectures, and synchronverter solutions; cost comparison to traditional grid upgrade; deployment success in demonstration projects',
    'If lock-in: the mountain perspective is false — ceiling is structural choice, not physical law. Reclassifies to snare or tangled_rope at all perspectives. If fundamental: mountain perspective gains legitimacy; ceiling represents real physical constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_grid_architecture_viability, empirical, 'Whether grid architecture is locked-in or fundamental').

omega_variable(
    interconnection_queue_manipulation,
    'Are interconnection queues deliberately managed to limit deployment (extraction mechanism) or do they genuinely represent processing capacity and study necessity?',
    'Historical analysis of queue composition: ratio of projects that actually interconnect vs those withdrawn or delayed indefinitely; audit of study assumptions and whether they are revised when initial assumptions prove conservative; comparison of queue management practices across utilities',
    'If deliberate: suppression value increases to 0.70+, reclassifies as snare. If capacity-driven: suppression decreases to 0.35-0.40, reclassifies as rope. Current practices show evidence of both mechanisms operating simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interconnection_queue_manipulation, empirical, 'Intentionality and deliberateness of queue management').

omega_variable(
    distributed_vs_utility_scale_tradeoff,
    'Is the ceiling a genuine coordination problem (too many distributed sources create real technical challenges) or is it an extraction mechanism that disproportionately favors utility-scale over distributed?',
    'Technical analysis of grid impact per MW: distributed solar with smart inverters vs utility-scale solar farms; empirical measurement of voltage regulation, harmonics, and frequency response by source type; jurisdictions with high distributed penetration and their grid stability measures',
    'If coordination problem: constraint is rope or tangled_rope reflecting genuine technical tradeoffs. If extraction favoring utility-scale: constraint is snare for distributed developers, reflecting market protection rather than technical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributed_vs_utility_scale_tradeoff, empirical, 'Distributed vs utility-scale grid impact comparison').

omega_variable(
    rate_structure_lock_in,
    'Do existing residential and commercial rate structures (designed for centralized generation) create artificial barriers to distributed solar penetration?',
    'Comparison of distributed solar adoption rates across jurisdictions with different rate structures (net metering vs feed-in tariffs vs time-of-use); cost-benefit analysis showing whether deployment ceiling changes with rate structure reform',
    'If rate structure is binding: the extraction mechanism is not grid physics but regulatory choice. Reclassifies suppression and extractiveness values downward; suggests scaffold trajectory (sunset dependent on rate structure reform, not hardware development).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rate_structure_lock_in, empirical, 'Rate structure contribution to deployment ceiling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(photovoltaic_deployment_ceiling, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pv_deploy_tr_t0, photovoltaic_deployment_ceiling, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pv_deploy_tr_t5, photovoltaic_deployment_ceiling, theater_ratio, 5, 0.5).
narrative_ontology:measurement(pv_deploy_tr_t10, photovoltaic_deployment_ceiling, theater_ratio, 10, 0.64).
narrative_ontology:measurement(pv_deploy_tr_t15, photovoltaic_deployment_ceiling, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(pv_deploy_be_t0, photovoltaic_deployment_ceiling, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pv_deploy_be_t5, photovoltaic_deployment_ceiling, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(pv_deploy_be_t10, photovoltaic_deployment_ceiling, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(pv_deploy_be_t15, photovoltaic_deployment_ceiling, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(photovoltaic_deployment_ceiling, resource_allocation).
narrative_ontology:affects_constraint(photovoltaic_deployment_ceiling, utility_cost_recovery_rate_structure).
narrative_ontology:affects_constraint(photovoltaic_deployment_ceiling, distributed_energy_resource_ownership_framework).
narrative_ontology:affects_constraint(photovoltaic_deployment_ceiling, grid_modernization_interconnection_standards).

% DUAL FORMULATION NOTE:
% The photovoltaic deployment ceiling decomposes into technical coordination (grid stability) and institutional extraction (market protection). The technical component would be rope; the extraction component would be snare. They are empirically entangled within current regulatory frameworks but would separate under alternative institutions (e.g., utility-independent distribution operators, mandated open interconnection standards). This story treats the entangled system; downstream stories track cost-recovery mechanisms and rate structure impacts separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(photovoltaic_deployment_ceiling, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
