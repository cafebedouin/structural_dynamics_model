% ============================================================================
% CONSTRAINT STORY: container_capacity_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_container_capacity_mismatch, []).

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
 *   constraint_id: container_capacity_mismatch
 *   human_readable: The Volume-Infrastructure Paradox: Container Capacity Mismatch
 *   domain: logistics/physical_infrastructure/economic
 *
 * SUMMARY:
 *   The volume-infrastructure paradox arises when efficient source production
 *   vastly outpaces the physical or logical capacity of distribution
 *   channels. This creates a structural tension between the producers'
 *   incentive to maximize output and infrastructure operators' incentive to
 *   maintain scarcity rents. The constraint exhibits all six DR types from
 *   different perspectives: pure extraction (Snare) for trapped receivers,
 *   mixed coordination-extraction (Tangled Rope) for logistics providers and
 *   infrastructure authorities, net benefit (Rope) for source producers,
 *   theatrical compliance (Piton) for regulatory systems, temporary solution
 *   (Scaffold) for resilience coalitions, and false naturalization (Mountain)
 *   for analysts who treat capacity limits as immutable. The theater ratio
 *   reflects that 'just-in-time' doctrine and 'optimization' metrics mask the
 *   fundamental brittleness of running at capacity ceiling. Infrastructure
 *   operators maintain congestion partly through deliberate under-investment
 *   in expansion, partly through regulatory capture that prevents competing
 *   infrastructure, and partly through genuine technical constraints. The
 *   constraint's extractiveness has risen from 0.28 to 0.52 over twenty years
 *   as source production has accelerated and infrastructure investment has
 *   stagnated. The theater ratio has similarly risen from 0.35 to 0.58,
 *   indicating that the system increasingly relies on performative efficiency
 *   metrics rather than actual capacity resilience.
 *
 * KEY AGENTS:
 *   - Source Producers (Institutional/Arbitrage): Primary beneficiary—gain scarcity rents, can arbitrage to alternative markets or hold inventory, experience the constraint as a market-clearing mechanism
 *   - Infrastructure Operators (Organized/Constrained): Mixed beneficiary-victim—extract congestion rents and priority pricing, but constrained by physical assets and regulatory limits on expansion; maintain chronic under-investment
 *   - Downstream Receivers (Powerless/Trapped): Primary victims—bear full cost of spoilage, inventory backlog, supply uncertainty, and price volatility; cannot exit regional markets
 *   - Logistics Providers (Moderate/Constrained): Secondary beneficiary-victim—provide transportation coordination but also extract through congestion surcharges and storage fees
 *   - Regulatory/Policy Bodies (Institutional/Arbitrage): Maintain piton theater—enforce 'efficient supply chains' via just-in-time doctrine and cost-per-unit metrics, avoiding acknowledgment of fragility
 *   - Resilience Coalition (Organized/Mobile): Build alternative pathways—buffer stock programs, supply diversification, distributed production infrastructure with sunset logic
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing contingent infrastructure choices as physical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(container_capacity_mismatch, 0.52).
domain_priors:suppression_score(container_capacity_mismatch, 0.68).
domain_priors:theater_ratio(container_capacity_mismatch, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(container_capacity_mismatch, extractiveness, 0.52).
narrative_ontology:constraint_metric(container_capacity_mismatch, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(container_capacity_mismatch, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(container_capacity_mismatch, tangled_rope).
narrative_ontology:human_readable(container_capacity_mismatch, "The Volume-Infrastructure Paradox: Container Capacity Mismatch").
narrative_ontology:topic_domain(container_capacity_mismatch, "logistics/physical_infrastructure/economic").

domain_priors:requires_active_enforcement(container_capacity_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(container_capacity_mismatch, source_producers).
narrative_ontology:constraint_beneficiary(container_capacity_mismatch, infrastructure_operators).
narrative_ontology:constraint_victim(container_capacity_mismatch, downstream_receivers).
narrative_ontology:constraint_victim(container_capacity_mismatch, market_efficiency).
narrative_ontology:constraint_victim(container_capacity_mismatch, supply_chain_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM RECEIVER (SNARE) — Cannot exit the supply chain; bears full cost of congestion, spoilage, and inventory backlog. Trapped in regional market with limited alternative sources. d≈0.92, f(d)≈1.39, σ=0.9 → χ≈0.65. Pure extraction through forced waiting and quality degradation.
constraint_indexing:constraint_classification(container_capacity_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOGISTICS PROVIDER (TANGLED ROPE) — Provides coordination function (movement of goods), but also extracts rents through congestion premiums, storage fees, and priority pricing. Constrained by infrastructure assets they cannot quickly scale. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.37. Hybrid coordination-extraction.
constraint_indexing:constraint_classification(container_capacity_mismatch, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOURCE PRODUCER (ROPE) — Benefits from efficient production and bottleneck creation. Can arbitrage away to alternative markets or hold inventory to manage demand. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary; sees constraint as coordination mechanism (market clearing through scarcity value).
constraint_indexing:constraint_classification(container_capacity_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INFRASTRUCTURE AUTHORITY (TANGLED ROPE) — Operates the capacity bottleneck (ports, highways, rail, data centers). Coordination function: moves goods and data at scale. Extraction function: controls access, sets tolls/rates, resists capacity expansion (CapEx avoidance). d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Moderate effective extraction; beneficiary from status quo capacity shortage.
constraint_indexing:constraint_classification(container_capacity_mismatch, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY FICTION (PITON) — 'Just-in-time' supply chain doctrine treats the mismatch as optimized efficiency, not as a chronic structural crisis. Ritual compliance with efficiency metrics (cost-per-unit, inventory turns) masks that the system runs at the edge of collapse. theater_ratio=0.58 reflects performative metrics; actual reliability is degraded. The fiction persists through inertia — no institutional actor has incentive to acknowledge that the emperor has no clothes.
constraint_indexing:constraint_classification(container_capacity_mismatch, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RESILIENCE COALITION (SCAFFOLD) — Organized response (buffer stock programs, supply chain diversification, distributed production) to build redundancy and reduce bottleneck dependence. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.29. Lower effective extraction because coalition has agency and sees an exit path (decentralization). Has implicit sunset: as distributed systems mature, centralized choke points lose power.
constraint_indexing:constraint_classification(container_capacity_mismatch, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL LAW (MOUNTAIN) — Treats capacity constraints as immutable physical limits: ports have fixed throughput, rail corridors have fixed capacity, data pipes have fixed bandwidth. From this view, the mismatch is an inherent property of large-scale logistics. However, structural data (ε=0.52, suppression=0.68) contradicts mountain classification — this is a false summit. Capacity is not physically immutable; it is a choice of investment and governance, not a law of nature.
constraint_indexing:constraint_classification(container_capacity_mismatch, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(container_capacity_mismatch_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(container_capacity_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(container_capacity_mismatch, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(container_capacity_mismatch, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(container_capacity_mismatch, TR),
    TR >= 0.70.

:- end_tests(container_capacity_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from receivers through forced waiting, spoilage losses, inventory carrying costs, and price volatility. However, extraction is not maximal (≥0.66 for snare) because the system maintains a veneer of coordination: goods do eventually move, supply chains do function. The extractiveness has grown from 0.28 to 0.52 over the interval as the production-capacity gap has widened and infrastructure operators have deliberately under-invested. Suppression (0.68): High. Receivers face significant barriers to exit: capacity is controlled by infrastructure monopolies or oligopolies, alternative distribution channels are suppressed through regulatory/financial barriers, and switching costs are prohibitive. Geographic/contractual lock-in is common. Suppression reflects both structural barriers and deliberate maintenance of scarcity. Theater ratio (0.58): Moderate-high. The system justifies itself through 'just-in-time' efficiency doctrine, cost-per-unit optimization, and 'lean supply chain' metrics. These are performative—the system measures what it optimizes for (inventory turns, cost per unit) while ignoring what it does not (resilience, reliability, buffer capacity). As the theater has risen from 0.35 to 0.58, the gap between official metrics and actual fragility has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The source producer sees pure coordination (Rope)—they are solving the efficient-production problem. Infrastructure operators see temporary profit optimization (Tangled Rope with implicit rent extraction). Logistics providers see mixed coordination-extraction (Tangled Rope)—they move goods but also extract through surcharges. Downstream receivers see pure extraction (Snare)—they have no escape and bear all costs. The resilience coalition sees a solvable temporary problem with a sunset (Scaffold)—distributed alternatives can emerge. The regulatory system sees a naturalized efficiency requirement (Piton)—the theater of 'lean' supply chains masks the brittleness. The civilizational analytical observer risks seeing immutable physical limits (Mountain)—'ports have fixed throughput'—but the structural data reveals this as a false summit: capacity is a choice of investment and governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Source producers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; arbitrage options give high exit value. Downstream receivers: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction; regional lock-in removes exit options. Logistics providers: Mixed + constrained → d≈0.58, f(d)≈0.72. Moderate-high extraction; extraction via surcharges and priority pricing offset by coordination function. Infrastructure operators: Beneficiary (from rents) + constrained → d≈0.35, f(d)≈0.35. Low-moderate effective extraction because they extract rents, but constrained by regulatory/political limits and sunk asset bases prevent rapid exit. Resilience coalition: Organized + mobile → d≈0.45, f(d)≈0.50. Lower effective extraction; coalition has agency (can build alternatives) and mobile exit (distributed systems). Regulatory system: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate (≥0.70 theaters), not from high chi. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification is false summit—naturalizes contingent governance choices.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION STRATEGY: This constraint resolves mandatrophy by separating the legitimate coordination function (moving goods at scale) from the extraction mechanism (scarcity rents maintained through deliberate under-investment). The coordination function is real—large-scale supply chains are genuinely difficult to manage. The extraction mechanism is also real—infrastructure operators benefit from the mismatch and have little incentive to expand capacity. The mandatrophy is resolved by recognizing that this is a Tangled Rope, not a pure Rope or pure Snare. (1) Coordination function: The constraint solves the problem of moving commodity volumes. (2) Asymmetric extraction: Infrastructure operators and source producers extract rents from receivers through congestion. (3) Active enforcement: The extraction requires deliberate under-investment decisions and regulatory capture to suppress competing infrastructure. The theater ratio (0.58) reflects performative 'efficiency' metrics that hide the extraction. The scaffold perspective (resilience coalition with sunset) is the key structural feature: if distributed alternatives mature, the central bottleneck loses extractive power. The piton perspective (regulatory theater) is real but not permanent—policy can change. The false mountain is important: the constraint is NOT an immutable physical law, though it is often framed that way. The analytical observer who naturalizes the bottleneck misses the political economy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_expansion_economics,
    'Why do infrastructure operators chronically under-invest in capacity expansion despite high rents from scarcity premiums?',
    'Financial analysis of infrastructure operator capital budgets, ROI calculations, and political economy of public/private infrastructure investment; comparison with historical expansion rates during different regulatory regimes',
    'If CapEx barriers are primarily financial: problem is Rope (coordination of investment). If barriers are political/regulatory: problem is Snare (operators use regulation to maintain extraction rents). If barriers are technical/physical: constraint approaches Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_expansion_economics, empirical, 'Root cause of chronic capacity under-investment').

omega_variable(
    production_efficiency_source,
    'Does the source producer''s efficiency (ability to produce far beyond container capacity) reflect genuine technological advantage or artificial suppression of alternative distribution channels?',
    'Comparative analysis: efficiency metrics when alternative distribution exists vs when bottleneck is sole outlet; investigation of infrastructure operator investment in competing channels; study of producer pricing behavior when capacity is abundant vs constrained',
    'If genuine efficiency: mismatch is coordination problem (Rope → Tangled Rope). If artificial suppression: mismatch is extraction mechanism (Snare/Piton with theatrical efficiency metrics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(production_efficiency_source, empirical, 'Whether production efficiency is genuine or artificially created by suppressing alternatives').

omega_variable(
    supply_chain_fragility_threshold,
    'At what point does the bottleneck shift from a managed constraint (Tangled Rope) to a systemic fragility (Snare) with catastrophic breakdown risk?',
    'Stress testing: simulation of demand shocks, capacity disruptions, and cascading failures; historical analysis of supply chain collapses; measurement of actual vs theoretical reserve capacity margins',
    'If threshold is high and distant: current state is manageable Tangled Rope. If threshold is low and imminent: system is brittle Snare masked by piton theater. Determines whether scaffold sunset is sufficient or catastrophic derisking is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_fragility_threshold, empirical, 'Fragility threshold for supply chain collapse').

omega_variable(
    decentralization_viability,
    'Can distributed production and supply chains reduce dependence on centralized infrastructure, or are scale/efficiency losses prohibitive?',
    'Pilot projects with distributed production (3D printing, regional manufacturing, local storage hubs); cost-benefit analysis comparing centralized bottleneck rents vs distributed redundancy overhead; market adoption rates when alternatives become available',
    'If decentralization is viable: scaffold sunset is real and imminent (10-20 years). If costs are prohibitive: coalition is aspirational; constraint persists indefinitely as structural feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralization_viability, empirical, 'Technical and economic viability of decentralized alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(container_capacity_mismatch, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccm_tr_t0, container_capacity_mismatch, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ccm_tr_t10, container_capacity_mismatch, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ccm_tr_t20, container_capacity_mismatch, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(ccm_be_t0, container_capacity_mismatch, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ccm_be_t10, container_capacity_mismatch, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(ccm_be_t20, container_capacity_mismatch, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(container_capacity_mismatch, resource_allocation).
narrative_ontology:affects_constraint(container_capacity_mismatch, supply_chain_concentration).
narrative_ontology:affects_constraint(container_capacity_mismatch, geographic_rent_extraction).
narrative_ontology:affects_constraint(container_capacity_mismatch, just_in_time_fragility).

% DUAL FORMULATION NOTE:
% The volume-infrastructure paradox decomposes into three structurally distinct constraints: (1) supply_chain_concentration (ε≈0.35, Rope/Tangled Rope) — the efficiency logic of centralized hubs; (2) geographic_rent_extraction (ε≈0.55, Tangled Rope/Snare) — the political economy of infrastructure monopoly; (3) just_in_time_fragility (ε≈0.62, Snare) — the systemic risk from eliminating buffer capacity. This story aggregates the perspectives across all three. Upstream: supply_chain_concentration drives the efficiency gain that creates the volume mismatch. Downstream: geographic_rent_extraction and just_in_time_fragility inherit the bottleneck structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(container_capacity_mismatch, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
