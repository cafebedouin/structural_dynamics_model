% ============================================================================
% CONSTRAINT STORY: nsw_transmission_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsw_transmission_bottleneck, []).

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
 *   constraint_id: nsw_transmission_bottleneck
 *   human_readable: NSW Regional Transmission Congestion
 *   domain: technological/political
 *
 * SUMMARY:
 *   The NSW-South Australia transmission bottleneck represents a structural
 *   constraint on inter-regional electricity trade created by the physical
 *   carrying capacity of high-voltage interconnects. South Australia has
 *   developed substantial renewable generation capacity (wind, solar) with
 *   lower marginal costs than incumbent fossil-fuel generators in NSW. The
 *   interconnect's capacity limit (approximately 2,000 MW) means that during
 *   high-demand periods, SA renewable output cannot fully export, causing
 *   energy prices to reflect NSW generation mix rather than the lowest-cost
 *   SA supply. This creates a multi-perspectival constraint: from the SA
 *   renewable exporter view it is a snare (trapped by geography); from the
 *   incumbent NSW generator view it is rope (protects market position); from
 *   the grid decarbonization view it is tangled rope (mixes real coordination
 *   needs with incumbent extraction); from the regulatory view it is piton
 *   (technically managed through pricing but functionally degraded). The
 *   constraint's extractiveness has increased from 0.35 to 0.52 over the
 *   interval as renewable capacity in SA expanded and incumbent thermal
 *   capacity in NSW contracted, increasing the scarcity premium captured by
 *   remaining fossil generators. Theater ratio has increased from 0.42 to
 *   0.58 as the National Electricity Market regulatory framework increasingly
 *   relies on congestion pricing to allocate scarce transmission while
 *   deferring physical expansion.
 *
 * KEY AGENTS:
 *   - South Australian Renewable Exporters: Primary victim (powerless/trapped) — own zero-marginal-cost capacity that cannot export due to physical constraint; bear full opportunity cost of transmission limit
 *   - NSW Consumers: Secondary victim (moderate/constrained) — pay scarcity rents on electricity during congested periods; constrained exit through capex-heavy alternatives (rooftop solar, batteries, or interstate relocation)
 *   - Incumbent NSW Thermal Generators: Primary beneficiary (institutional/arbitrage) — receive scarcity-based pricing during periods when SA exports are constrained; have arbitrage exit via financial instruments, fuel contracts, and political influence
 *   - Transmission Network Operator (AEMO/TNO): Secondary beneficiary (institutional/arbitrage) — congestion justifies transmission investment programs and validates operational complexity; exit through regulatory investment approval
 *   - Grid Decarbonization Imperative: Organized victim (organized/constrained) — climate policy and renewable transition experience constraint as hybrid extraction-coordination problem; constrained exit because energy transition cannot avoid grid management during transition period
 *   - Regulatory Framework (NEM): Institutional actor (institutional/arbitrage) — encodes congestion management through pricing; maintains performative solution; inertia makes large-scale expansion slow (10-15 years vs 3-5 for comparable European projects)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice (transmission investment timing, market design, retirement schedules) as immutable physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsw_transmission_bottleneck, 0.52).
domain_priors:suppression_score(nsw_transmission_bottleneck, 0.65).
domain_priors:theater_ratio(nsw_transmission_bottleneck, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsw_transmission_bottleneck, extractiveness, 0.52).
narrative_ontology:constraint_metric(nsw_transmission_bottleneck, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nsw_transmission_bottleneck, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsw_transmission_bottleneck, tangled_rope).
narrative_ontology:human_readable(nsw_transmission_bottleneck, "NSW Regional Transmission Congestion").
narrative_ontology:topic_domain(nsw_transmission_bottleneck, "technological/political").

domain_priors:requires_active_enforcement(nsw_transmission_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsw_transmission_bottleneck, incumbent_nsw_generators).
narrative_ontology:constraint_beneficiary(nsw_transmission_bottleneck, transmission_network_operators).
narrative_ontology:constraint_victim(nsw_transmission_bottleneck, sa_renewable_exporters).
narrative_ontology:constraint_victim(nsw_transmission_bottleneck, nsw_consumers).
narrative_ontology:constraint_victim(nsw_transmission_bottleneck, grid_decarbonization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SA RENEWABLE EXPORTERS (SNARE) — Trapped by physical transmission constraint; cannot export lowest-cost zero-carbon electricity to NSW market despite economic advantage. No exit option from geography or grid topology. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.68. Pure extraction: constraint exists, alternative suppliers benefit, exporters bear full cost.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NSW CONSUMERS (SNARE) — Constrained by transmission limit; pay higher prices than would obtain if SA renewable capacity could export freely. Exit requires grid-scale battery storage or local renewable generation (high capex, slow deployment). d≈0.80, f(d)≈1.15, σ=0.9 → χ≈0.55. Victims of price-support mechanism for incumbent generators.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT NSW THERMAL GENERATORS (ROPE) — Beneficiary (institutional/arbitrage). Congestion creates a captive market: their output commands scarcity rents during high-demand periods. The constraint solves a coordination problem for them — prevents price collapse from SA renewables. Exit option via financial hedging, fuel contracts. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary; constraint coordinates their market protection.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: TRANSMISSION NETWORK OPERATOR (ROPE) — Secondary beneficiary (institutional/arbitrage). Congestion justifies infrastructure investment programs, regulatory cost recovery, and system management complexity that validates their role. Coordinates the engineering problem (voltage stability, frequency control) of managing interconnect. Exit via arbitrage through investment approval processes. d≈0.12, f(d)≈0.02, σ=0.9 → χ≈0.01. Low effective extraction; primarily coordination.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: GRID DECARBONIZATION IMPERATIVE (TANGLED ROPE) — Organized agents (climate policy, renewable sector, demand-side response) experience the constraint as a hybrid: coordination function (requires grid management during transition) + asymmetric extraction (transmission constraint suppresses zero-carbon supply, forcing reliance on retained fossil capacity). Constrained exit: energy transition cannot avoid grid management but is slowed by incumbent protection. d≈0.58, f(d)≈0.68, σ=1.0 → χ≈0.35. Mixed: real coordination need (grid stability during transition) meets rent extraction (congestion creates fossil fuel market power).
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — The National Electricity Market rules treat transmission congestion as a technical engineering problem managed through locational marginal pricing (LMP). The rules encode a performative solution: price signals that reflect congestion but do not eliminate it. theater_ratio=0.58. The regulatory framework sees congestion as legitimate market signal rather than as a protection mechanism for incumbents. Inertia: rules persist because infrastructure rebuild takes 10-15 years; cheaper to manage congestion via pricing than to expand transmission, though long-term analysis contradicts this. d≈0.15, f(d)≈0.08, σ=1.0 → χ≈0.05. Piton because theater >0.5 and the primary function (market clearing via congestion pricing) does not fully address the underlying problem.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW VIEW (ATTEMPTED MOUNTAIN) — From a civilizational/universal view, the interconnect has a physical carrying capacity determined by thermal limits, voltage stability, and protection scheme settings. No agent can change the laws of physics. However, this classification is a FALSE SUMMIT: the structural data (ε=0.52, suppression=0.65) reveals that the constraint's extractiveness is not immutable. Regulatory choices (network investment timing, connection approval), technological choices (HVDC vs AC, compensation equipment), and market design choices (inter-regional trade rules) all affect effective capacity. The 'natural law' framing naturalizes policy choices.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsw_transmission_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsw_transmission_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsw_transmission_bottleneck, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsw_transmission_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nsw_transmission_bottleneck, TR),
    TR >= 0.70.

:- end_tests(nsw_transmission_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint creates a measurable rent stream for incumbent thermal generators during congested periods. This is neither pure coordination (rope) nor immutable law (mountain). The mechanism is clear: SA renewable capacity is prevented from exporting, raising the marginal supply cost for NSW, allowing thermal generators to capture the price difference. Extractiveness has grown as renewable penetration increased (from 0.35 to 0.52 over the interval) because the gap between SA marginal cost and NSW thermal cost widened. However, extractiveness is not maximal (0.70+) because the constraint does serve genuine coordination functions during grid transition — rapid retirement of all thermal capacity would create stability risks that require transmission management. Suppression (0.65): High. Barriers to exit are significant: SA generators cannot move their physical location; NSW consumers face high capex for distributed generation; the interconnect expansion requires 10-15 year regulatory and construction timelines. However, suppression is not total (0.85+) because some exit options exist: battery storage is declining in cost; demand response programs are emerging; distributed solar is growing. Theater ratio (0.58): Moderate-high. The National Electricity Market's congestion pricing mechanism is partly functional (allocates scarce capacity to highest-value uses) and partly performative (signals scarcity without resolving it). The theater has increased as regulatory solutions (LMP, constraint equations, financial transmission rights) have grown more sophisticated while physical expansion has stalled. The increasing theater reflects growing Goodhart drift: the pricing mechanism becomes the goal rather than a means to allocate scarce transmission.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces divergent classifications across the observation site. SA renewable exporters see a snare (trapped, no exit). NSW thermal generators see a rope (solved their coordination problem of avoiding price collapse). Grid decarbonization sees tangled rope (real coordination need during transition, but overlaid with incumbent protection). The regulatory framework sees a piton (pricing mechanism is performative, deferring the hard choice of expansion vs alternatives). The analytical observer risks seeing a mountain (it's the laws of physics) but the structural data reveals false summitery: the effective constraint is regulatory and financial, not physical. The bottleneck could be expanded, managed via storage and demand-side solutions, or eliminated through inter-regional tariff design — these are choices, not laws. This perspectival gap is the diagnostic signature of tangled rope: mixed coordination function (grid stability during transition) with asymmetric extraction (incumbent protection), not a single type.
 *
 * DIRECTIONALITY LOGIC:
 *   SA renewable exporters: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction — geography and grid topology eliminate exit option. NSW consumers: Victim + constrained → d≈0.80, f(d)≈1.15. High extraction — must pay scarcity premium but can exit through capex (rooftop solar, batteries). Incumbent NSW thermal generators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary — constraint protects market; exit through financial instruments, fuel contracts, political influence. Transmission network operator: Beneficiary + arbitrage → d≈0.12, f(d)≈0.02. Low effective extraction — primarily coordinating; exit via regulatory processes. Grid decarbonization: Victim + constrained → d≈0.58, f(d)≈0.68. Mixed — constrained by grid management needs during transition but experiencing extracted rents. Regulatory framework: Institutional + arbitrage → d≈0.15, f(d)≈0.08. Low effective extraction from piton perspective; primarily managing technical problem. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risk of false summit — naturalizing policy choice as physical law.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by recognizing that the transmission bottleneck is GENUINELY BOTH coordination and extraction, not one disguised as the other. The tangled rope classification is structurally correct: (1) Coordination function: Grid stability during transition from coal to renewables requires managed interconnect capacity and frequency support. Rapid unmanaged retirement of thermal plants creates blackout risk. The constraint serves the real function of maintaining grid reliability while sources transition. (2) Asymmetric extraction: The constraint creates scarcity rents for incumbent thermal generators and justifies slow transmission expansion that protects their market power. These benefits are NOT necessary for grid stability and represent rent extraction overlaid on top of coordination. The critical test: Could grid stability be achieved with faster transmission expansion or alternative technologies (batteries, demand response)? If yes, the extraction is unnecessary. Preliminary evidence suggests alternatives exist and are cost-competitive within 5-7 years, indicating that suppression of these alternatives is the extractive component. The decarbonization timeline (coal retirement window, renewable deployment pace) is the natural outer bound of the constraint's legitimate coordination function. Beyond that timeline, the constraint becomes pure extraction (snare). The tangled rope classification correctly identifies both components and marks the frontier where coordination necessity ends and incumbent protection begins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_expansion_timing,
    'Is the slow pace of transmission expansion a technical/economic necessity or a regulatory/incumbent-protection choice?',
    'Cost-benefit analysis of accelerated transmission vs alternative technologies (battery storage, demand flexibility, distributed generation); comparison with international fast-track interconnect builds (Denmark-Germany HVDC: 3 years; Australia average: 10-15 years)',
    'If technical: constraint is closer to natural law. If choice: constraint is revealed as institutional extraction mechanism. Affects classification floor from rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_expansion_timing, empirical, 'Whether transmission expansion pace is technical or policy-driven').

omega_variable(
    sa_export_capacity_market_value,
    'What is the actual economic rent (per MWh) captured by incumbent NSW generators due to transmission constraint?',
    'Empirical analysis of price spreads between uncongested and congested periods; counterfactual modeling of SA renewable export at full capacity; comparison with wholesale electricity prices in congestion-free periods',
    'If rent >$20/MWh: constraint is clearly extractive (snare from consumer perspective). If <$5/MWh: price effect is marginal, constraint is primarily coordination problem. Affects tangled_rope vs rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sa_export_capacity_market_value, empirical, 'Magnitude of rent extraction via transmission constraint').

omega_variable(
    alternative_capacity_deployment,
    'Could battery storage, demand response, or distributed solar achieve equivalent grid stability at lower cost than transmission expansion?',
    'Cost comparison: $/MWh-hr of transmission expansion vs grid-scale battery + distribution investment; modeling of alternative dispatch scenarios; pilot data from distributed flexibility programs',
    'If alternatives are cheaper: current constraint is rent protection (snare confirmed). If transmission is cheaper: constraint reflects genuine economic coordination. Changes beneficiary legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capacity_deployment, empirical, 'Whether alternatives to transmission expansion are cost-competitive').

omega_variable(
    incumbent_fossil_phase_out_timeline,
    'What is the binding constraint on coal generator retirement: economics, policy, or grid stability risk?',
    'Historical analysis of retirement decisions vs coal plant economics; modeling grid stability impacts of accelerated retirements; comparison with international retirement schedules (UK, Germany)',
    'If economics dominates: generators would exit anyway; constraint temporarily protects transitioning assets. If policy dominates: incumbent protection may extend constraint unnecessarily beyond grid stability window. Affects piton vs snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_fossil_phase_out_timeline, empirical, 'Binding constraint on fossil generator retirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsw_transmission_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nswt_tr_t0, nsw_transmission_bottleneck, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nswt_tr_t5, nsw_transmission_bottleneck, theater_ratio, 5, 0.5).
narrative_ontology:measurement(nswt_tr_t10, nsw_transmission_bottleneck, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(nswt_be_t0, nsw_transmission_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nswt_be_t5, nsw_transmission_bottleneck, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(nswt_be_t10, nsw_transmission_bottleneck, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsw_transmission_bottleneck, global_infrastructure).
narrative_ontology:affects_constraint(nsw_transmission_bottleneck, australian_fossil_fuel_subsidy).
narrative_ontology:affects_constraint(nsw_transmission_bottleneck, renewable_energy_integration_lag).
narrative_ontology:affects_constraint(nsw_transmission_bottleneck, grid_decarbonization_speed).

% DUAL FORMULATION NOTE:
% The transmission bottleneck is downstream of generation capacity distribution (SA has renewable resources, NSW has incumbent thermal) but is a structurally distinct constraint. The upstream generation constraints have their own ε values reflecting resource endowments; this constraint has ε=0.52 reflecting the policy/regulatory choice to manage congestion via pricing rather than expansion. The bottleneck affects decarbonization speed because it creates incentives to retain fossil capacity (protection during congestion) and disincentives to retire early.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsw_transmission_bottleneck, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
