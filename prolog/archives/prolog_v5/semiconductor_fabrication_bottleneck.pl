% ============================================================================
% CONSTRAINT STORY: semiconductor_fabrication_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_fabrication_bottleneck, []).

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
 *   constraint_id: semiconductor_fabrication_bottleneck
 *   human_readable: Semiconductor Fabrication Bottleneck
 *   domain: industrial/economic/technical
 *
 * SUMMARY:
 *   The semiconductor fabrication bottleneck represents a structural
 *   constraint on global chip supply where access to advanced manufacturing
 *   nodes (sub-7nm) is concentrated in a handful of facilities (primarily
 *   TSMC in Taiwan, Samsung in South Korea, Intel globally, with SMIC in
 *   mainland China). This concentration creates extraction mechanisms across
 *   multiple dimensions: pricing power during shortage cycles, arbitrary
 *   priority allocation, forced acceptance of unfavorable contract terms, and
 *   sunk costs that lock customers into incumbent relationships. The
 *   constraint exhibits genuine coordination functions (process development,
 *   yield optimization, capacity planning) alongside asymmetric extraction,
 *   making it a diagnostic tangled rope from multiple perspectives. The
 *   bottleneck emerged sharply during 2020-2022 due to pandemic supply shocks
 *   but persists through underlying structural factors: capital intensity of
 *   fab construction ($10-20B per facility), long time-to-profitability (5-10
 *   years), specialized knowledge and expertise, and geopolitical
 *   concentration of both supply and demand.
 *
 * KEY AGENTS:
 *   - Chip Designers: Primary victims (powerless/trapped) — cannot exit the queue, face arbitrary delays and yield variability during shortage cycles
 *   - Device Manufacturers: Secondary victims (moderate/constrained) — face high switching costs, forced into long-term contracts at unfavorable terms during shortage cycles
 *   - Incumbent Foundries (TSMC, Samsung, Intel): Primary beneficiaries (institutional/arbitrage) — capture pricing power, priority allocation, and rents during shortage cycles
 *   - Equipment Suppliers (ASML, Applied Materials, Lam Research): Secondary beneficiaries (organized/mobile) — benefit from sustained capital equipment demand and capacity expansion investments
 *   - Geopolitical Diversification Coalition: Organized agents (organized/constrained) — governments and consortia funding new fabs (CHIPS Act, EU Chips Act, Samsung GIDC) to distribute production and reduce Taiwan concentration
 *   - Supply Chain Resilience: Victim aggregate (powerless/trapped) — global economic exposure to Taiwan-concentrated production; systemic fragility from single-point failure risk
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional constraints as physics limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_fabrication_bottleneck, 0.58).
domain_priors:suppression_score(semiconductor_fabrication_bottleneck, 0.65).
domain_priors:theater_ratio(semiconductor_fabrication_bottleneck, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_fabrication_bottleneck, extractiveness, 0.58).
narrative_ontology:constraint_metric(semiconductor_fabrication_bottleneck, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(semiconductor_fabrication_bottleneck, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_fabrication_bottleneck, tangled_rope).
narrative_ontology:human_readable(semiconductor_fabrication_bottleneck, "Semiconductor Fabrication Bottleneck").
narrative_ontology:topic_domain(semiconductor_fabrication_bottleneck, "industrial/economic/technical").

domain_priors:requires_active_enforcement(semiconductor_fabrication_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_fabrication_bottleneck, incumbent_foundries).
narrative_ontology:constraint_beneficiary(semiconductor_fabrication_bottleneck, fab_owners).
narrative_ontology:constraint_beneficiary(semiconductor_fabrication_bottleneck, equipment_suppliers).
narrative_ontology:constraint_victim(semiconductor_fabrication_bottleneck, chip_designers).
narrative_ontology:constraint_victim(semiconductor_fabrication_bottleneck, device_manufacturers).
narrative_ontology:constraint_victim(semiconductor_fabrication_bottleneck, emerging_nations).
narrative_ontology:constraint_victim(semiconductor_fabrication_bottleneck, supply_chain_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHIP DESIGNER (SNARE) — Cannot exit the fabrication queue. Design capability is decoupled from production access. Faces arbitrary allocation delays, yield variability used as extraction mechanism, and forced acceptance of unfavorable terms during shortage cycles. Maximum experienced extraction — no alternatives for advanced node access.
constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVICE MANUFACTURER (TANGLED ROPE) — Constrained by technical switching costs and multi-year qualification cycles, but also benefits from fab ecosystem (yield improvements, process nodes, collaborative development). Significant extraction (priority allocation, price premiums) but genuine coordination function exists (joint process development, quality standards). Constrained exit — cannot freely switch fabs without massive sunk cost.
constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FOUNDRY (ROPE) — Primary beneficiary. Experiences constraint as coordination: managing scarce capacity, setting priority rules, and extracting rents during shortage. Net beneficiary with maximum flexibility — can arbitrage between customers, redirect capacity to highest-margin designs, and maintain pricing power. Extraction flows toward this agent.
constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GEOPOLITICAL DIVERSIFICATION COALITION (SCAFFOLD) — Organized agents (governments, major OEMs, consortia) funding new fabs in Taiwan, South Korea, US, EU to distribute production. Sees the bottleneck as temporary coordination failure with sunset clause: CHIPS Act funding, Intel foundry expansion, Samsung GIDC, and TSMC capacity additions are building redundancy. Low effective extraction because organized coalition has agency and clear exit pathway. Theater is moderate — actual fab capacity is being built, not merely promised.
constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTI-PATTERNING RITUAL (PITON) — Sub-7nm manufacturing relies on extreme ultraviolet (EUV) and multi-patterning techniques whose primary function (enabling advanced nodes) has been partially achieved, but the extraction mechanism persists through technological lock-in. Fab qualification, equipment specifications, and process IP create switching barriers that persist even when alternatives theoretically exist. The ritual is maintained by institutional inertia: massive sunk costs in tool development, operator expertise, and customer relationships. Theater ratio reflects the gap between claimed process capability and actual utilization efficiency.
constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EQUIPMENT SUPPLIER (TANGLED ROPE) — Benefits from bottleneck through sustained capital equipment demand (fabs must continuously upgrade tools to maintain competitiveness). Also genuinely coordinates production expansion (ASML, Applied Materials, Lam Research directly enable fab capacity scaling). Exit options are mobile — can pivot tool designs to different nodes/markets — but switching away from advanced nodes to mature node suppliers would mean abandoning highest-margin market segments. Experienced extraction is moderate: high revenue but also genuine coordination value.
constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICS LIMIT VIEW (MOUNTAIN) — From a civilizational view, quantum tunneling effects and manufacturing precision limits create inherent physical constraints on sub-7nm scaling. Some experts argue the bottleneck is ultimately a law of physics, not policy — that we are approaching fundamental limits of silicon-based lithography. However, the structural data contradicts this: the extraction is socially organized (fab access, price setting, priority allocation), suppression is institutional (switching costs, qualification barriers), and theater is moderate (actual productive activity occurs). The engine will identify this as a false summit — the 'physics limit' framing naturalizes what is actually a contingent industrial arrangement.
constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_fabrication_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_fabrication_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_fabrication_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_fabrication_bottleneck, TR),
    TR >= 0.70.

:- end_tests(semiconductor_fabrication_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Incumbent foundries extract significant rents during shortage cycles through price premiums, priority allocation, and forced long-term contracts. However, the extraction is not total (0.70+) because genuine coordination functions exist: yield optimization, process development, and capacity scaling require real collaboration. The value reflects that extraction is substantive but not the constraint's only function. Trajectory shows increasing extractiveness (0.42→0.58 over interval) as geopolitical tensions tighten Taiwan supply concentration and new fabs have not yet matured to full capacity. Suppression (0.65): High. Significant barriers to exit include fab qualification cycles (18-36 months), process IP lock-in, switching costs from established supply relationships, and limited alternative nodes in the short term (5-year horizon for new fabs to reach full production). Theater ratio (0.48): Moderate. The constraint involves genuine productive activity — actual wafer manufacturing, process development, equipment engineering — but also performance theater: promised node roadmaps, capability claims, and capacity announcements often exceed near-term delivery. Theater increased from 0.32 to 0.48 as geopolitical diversification funding accelerated far ahead of actual facility construction completion (2020s announcements vs 2025+ production targets). The partial theater reflects gap between announced capacity additions and actual production timelines.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap opens between the beneficiary's Rope classification and the trapped agent's Snare classification. From the incumbent foundry perspective, the bottleneck is a coordination mechanism — managing scarce capacity, setting allocation rules, and extracting legitimate value from scarcity. From the chip designer perspective, the same mechanism is pure extraction — they cannot exit the queue, have no coordination role in foundry decisions, and experience only the extraction side (delays, yield risk, price premiums). The tangled_rope classification at moderate/constrained represents the middle agent experiencing both coordination benefit (access to cutting-edge process) and extraction cost (high switching costs, forced contracts). The scaffold perspective resolves the gap by declaring the bottleneck temporary — new fab facilities in multiple geographies will mature, reducing concentration and extraction by 2027-2030. The analytical observer's mountain classification is revealed as false summit by structural data: the constraint is socially organized (beneficiary/victim structure), not inherent to physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) value flows from their structural relationship: beneficiary status, exit cost, and power to negotiate terms. Beneficiaries (incumbent foundries) derive d from arbitrage exit capability and net benefit position → d~0.05 → f(d) negative → extraction runs toward them, subsidizing their position. Trapped agents (chip designers) derive d from trapped exit and victim status → d~0.95 → f(d) maximum → they experience maximum effective extraction. Constrained agents (device manufacturers) derive d from high but surmountable switching costs + victim status → d~0.70 → f(d) moderate-high. The scope modifier amplifies this at global scale: a local fab bottleneck affects only regional designers, but Taiwan-concentrated production affects global designer population. Scope (global) σ=1.2 scales all χ values upward, making the bottleneck's effective extraction more severe as its geographic reach widens.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint classifies as tangled rope because it simultaneously exhibits (a) genuine coordination function — yield optimization, process development, capacity planning require real technical collaboration between foundry and customer, and (b) asymmetric extraction — foundry captures pricing power, priority allocation, and rents disproportionate to customer benefit. Both functions are structural, not optional. The beneficiary (incumbent foundry) genuinely needs customers for volume and feedback; the victim (chip designer) genuinely needs the foundry's process capability. But extraction mechanism (priority allocation, pricing power during shortage) persists alongside coordination. Snare classification from the powerless perspective is consistent with tangled rope classification at the analytical level: the Snare represents the victim's experienced reality (pure extraction from their viewpoint), while Tangled Rope represents the full structural picture (extraction + coordination). The mandatrophy is resolved by showing why both are correct at their respective positions: the trapped agent experiences snare because they cannot see or benefit from the coordination function; the analytical observer sees tangled rope because it documents both functions in structural detail.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fab_capacity_shortage_causality,
    'Is the shortage structural (genuine capacity limit relative to demand) or artificial (capacity withheld through monopolistic pricing/allocation)?',
    'Inventory tracking of unsold fab capacity; comparison of actual wafer production rates vs nameplate capacity; analysis of fab utilization rates and idle capacity duration',
    'If structural: constraint is primarily coordination failure (Rope/Scaffold). If artificial: constraint is primarily extraction mechanism (Snare/Tangled Rope). If mixed: current tangled_rope classification is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fab_capacity_shortage_causality, empirical, 'Whether fab shortage is structural capacity limit or artificial withholding').

omega_variable(
    node_transition_necessity,
    'Is continuous advancement to smaller nodes economically necessary or driven by investor expectations and vendor marketing?',
    'Cost-benefit analysis of sub-7nm adoption; correlation between actual performance gains and application requirements; longitudinal adoption rates at each node; market segment analysis showing where sub-7nm is mandatory vs optional',
    'If necessary: bottleneck drives genuine coordination (higher Rope/Scaffold classification). If unnecessary: bottleneck is artificial rent extraction (higher Snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(node_transition_necessity, conceptual, 'Whether continuous node shrinkage is economically necessary').

omega_variable(
    geopolitical_diversification_feasibility,
    'Can new fab facilities in non-Taiwan locations achieve equivalent performance and cost at scale, or is Taiwan''s geopolitical concentration a structural necessity given knowledge lock-in?',
    'Performance benchmarking of Intel, Samsung, SMIC fabs against TSMC equivalents; analysis of yield rates, quality metrics, time-to-production for equivalent nodes; identification of tacit knowledge/expertise barriers vs pure capital requirements',
    'If feasible: scaffold sunset is real — geopolitical diversification will reduce bottleneck. If locked-in: bottleneck persists despite diversification attempts, suggesting deeper structural constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_diversification_feasibility, empirical, 'Whether geopolitical fab diversification can achieve parity with Taiwan').

omega_variable(
    euvmask_complexity_saturation,
    'Has EUV lithography reached saturation complexity, making further nodes increasingly difficult despite nominal capability?',
    'Analysis of EUV tool throughput trends; mask cost escalation data; defect rate trajectories; correlation between process complexity and yield losses in actual production; comparison of design rules vs physical capability',
    'If saturated: bottleneck is partially a natural physics/engineering limit (higher Mountain classification). If unsaturated: bottleneck is primarily organizational/economic (lower Snare/Tangled Rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(euvmask_complexity_saturation, empirical, 'Whether EUV lithography has reached functional saturation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_fabrication_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semfab_tr_t0, semiconductor_fabrication_bottleneck, theater_ratio, 0, 0.32).
narrative_ontology:measurement(semfab_tr_t2, semiconductor_fabrication_bottleneck, theater_ratio, 2, 0.4).
narrative_ontology:measurement(semfab_tr_t5, semiconductor_fabrication_bottleneck, theater_ratio, 5, 0.48).
narrative_ontology:measurement(semfab_tr_t10, semiconductor_fabrication_bottleneck, theater_ratio, 10, 0.44).

% Extraction over time
narrative_ontology:measurement(semfab_be_t0, semiconductor_fabrication_bottleneck, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(semfab_be_t2, semiconductor_fabrication_bottleneck, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(semfab_be_t5, semiconductor_fabrication_bottleneck, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(semfab_be_t10, semiconductor_fabrication_bottleneck, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_fabrication_bottleneck, resource_allocation).
narrative_ontology:affects_constraint(semiconductor_fabrication_bottleneck, chip_design_cycle_delays).
narrative_ontology:affects_constraint(semiconductor_fabrication_bottleneck, device_manufacturer_supply_chain_fragility).
narrative_ontology:affects_constraint(semiconductor_fabrication_bottleneck, geopolitical_semiconductor_dependency).

% DUAL FORMULATION NOTE:
% The fabrication bottleneck is upstream of multiple dependent constraints in semiconductor supply chain. Chip design cycle delays are downstream consequences of fab allocation failures. Device manufacturer supply fragility derives from single-point failure risk in Taiwan-concentrated production. Geopolitical semiconductor dependency reflects the bottleneck's concentration in allied nations' territories. All three downstream constraints share the same root structure: uneven access to advanced manufacturing capacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_fabrication_bottleneck, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
