% ============================================================================
% CONSTRAINT STORY: semiconductor_supply_chain_resilience
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_supply_chain_resilience, []).

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
 *   constraint_id: semiconductor_supply_chain_resilience
 *   human_readable: Semiconductor Supply Chain Resilience Constraint
 *   domain: industrial/geopolitical/economic
 *
 * SUMMARY:
 *   The semiconductor supply chain represents a hybrid constraint oscillating
 *   between coordination necessity and extractive monopoly. The extreme
 *   capital requirements (>$10B per advanced fab), technology concentration
 *   (TSMC dominates leading-edge logic; ASML dominates EUV lithography), and
 *   geopolitical fragmentation (Taiwan, South Korea, US, EU competing for
 *   supply security) create a structured tension between legitimate
 *   efficiency coordination and asymmetric extraction. Taiwan's concentration
 *   of advanced logic production (90%+ of leading-edge) creates a critical
 *   vulnerability that drives the entire constraint structure. Downstream
 *   manufacturers face trapped dependence; advanced foundries and integrated
 *   device manufacturers enjoy monopoly rents; states pursue expensive
 *   industrial policy responses; and equipment suppliers maintain gatekeeping
 *   power through IP, export controls, and natural complexity barriers. The
 *   constraint exhibits all six types depending on structural position. The
 *   2020-2023 supply crisis temporarily elevated extractiveness (0.42 → 0.58)
 *   and theater (performative production targets, emergency allocations).
 *   Recent measurements suggest stabilization rather than sunset — industrial
 *   policy subsidies have not yet generated genuine distributed capacity with
 *   cost parity, maintaining extraction mechanisms despite resilience
 *   investments.
 *
 * KEY AGENTS:
 *   - TSMC: Primary beneficiary (institutional/arbitrage) — 90%+ advanced node supply control, commanding premium pricing and customer lock-in
 *   - Samsung: Secondary beneficiary (institutional/arbitrage) — 2nd-tier advanced node supplier with similar monopoly dynamics but smaller share
 *   - ASML: Tertiary beneficiary (institutional/arbitrage) — near-monopoly on EUV lithography with sustained pricing power
 *   - Consumer Electronics OEMs (Apple, Intel, AMD, Qualcomm): Primary victims (powerless/trapped) — dependent on foundry access with 18-36 month qualification locks
 *   - Developing Nation Chip Producers: Primary victims (powerless/trapped) — structurally blocked from advanced tiers by capital requirements and export controls
 *   - Global Supply Stability: Secondary victim (powerless/trapped) — abstract collective good bearing cost of concentration risk with no advocacy
 *   - Secondary Chipmakers: Mixed victim (moderate/constrained) — face switching costs but not completely immobilized
 *   - Industrial Policy States (US, EU, South Korea, Taiwan): Organized enforcer (organized/constrained) — pursuing resilience through subsidy and coordination
 *   - Distributed Resilience Initiative: Organized reformer (organized/constrained) — building alternative pathways with 10-15 year sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_supply_chain_resilience, 0.58).
domain_priors:suppression_score(semiconductor_supply_chain_resilience, 0.68).
domain_priors:theater_ratio(semiconductor_supply_chain_resilience, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_supply_chain_resilience, extractiveness, 0.58).
narrative_ontology:constraint_metric(semiconductor_supply_chain_resilience, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(semiconductor_supply_chain_resilience, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_supply_chain_resilience, tangled_rope).
narrative_ontology:human_readable(semiconductor_supply_chain_resilience, "Semiconductor Supply Chain Resilience Constraint").
narrative_ontology:topic_domain(semiconductor_supply_chain_resilience, "industrial/geopolitical/economic").

domain_priors:requires_active_enforcement(semiconductor_supply_chain_resilience).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_resilience, vertically_integrated_manufacturers).
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_resilience, advanced_process_technology_leaders).
narrative_ontology:constraint_victim(semiconductor_supply_chain_resilience, downstream_electronics_manufacturers).
narrative_ontology:constraint_victim(semiconductor_supply_chain_resilience, developing_nation_chip_producers).
narrative_ontology:constraint_victim(semiconductor_supply_chain_resilience, global_supply_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER ELECTRONICS MANUFACTURER (SNARE) — Trapped in dependence on foundry access. No capacity to self-manufacture advanced chips. Cannot shift to alternative suppliers due to qualification delays (18-36 months) and locked supplier contracts. Bears full cost of supply disruptions (production halts, margin compression) with no exit capacity. Maximum experienced extraction.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION CHIP INDUSTRY (SNARE) — Structurally trapped in low-value assembly and testing tiers. Capital requirements for advanced fabs (>$10B) are insurmountable without state subsidies. Technology access restricted by export controls and IP licensing. Cannot exit toward higher value-add tiers. Bears cost of volatile supply prices and geopolitical fragmentation.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SECONDARY CHIPMAKER (TANGLED ROPE) — Constrained but not trapped. Faces high switching costs (process requalification, equipment procurement) and geopolitical risk (Taiwan concentration). But possesses capacity to develop alternative supply relationships, backward integrate selected processes, or shift to older-node production. Benefits from coordination (shared standards, industry consortia) while bearing extraction through supply monopoly pricing.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ADVANCED FOUNDRY (ROPE) — Net beneficiary. Controls 90%+ of leading-edge capacity. Experiences supply chain as coordination mechanism: achieving standardized interfaces, managing complex production scheduling, maintaining customer relationships. High margin extraction possible due to monopoly position. Has exit flexibility (can shift to contract manufacturing, licensing, or design). Perceived constraint is coordination of demand and capacity.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTEGRATED DEVICE MANUFACTURER / VERTICALLY INTEGRATED (ROPE) — Net beneficiary through control of design-to-fab integration. Samsung, Intel, Broadcom derive competitive advantage from internal supply security. Perceive constraint as pure coordination: internal transfer pricing, fab-to-design scheduling, capacity allocation. Low suppression on beneficiaries — can exit or shift capacity freely.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INDUSTRIAL POLICY COALITION (STATES) — Organized but constrained. US, EU, South Korea, Taiwan face supply chain vulnerability at geopolitical scale. Active enforcement through subsidy programs (CHIPS Act, EU Chips Act), export controls (EUV lithography restrictions), and incentive structures to build resilience. Constraints: cost of subsidies ($100B+), risk of stranded assets if demand shifts, coordination challenges (protecting IP while building distributed capacity). Benefits from resilience building; bears cost of market distortion.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LITHOGRAPHY EQUIPMENT SUPPLIER (PITON) — ASML's near-monopoly on EUV lithography is a degraded constraint maintained through inertia and export controls. Theater ratio (0.55) reflects that the monopoly's 'natural' position (only company with EUV capability) is sustained through government export restrictions and IP moats, not through inherent technical superiority. Alternative suppliers are intentionally suppressed. The constraint persists through orchestrated enforcement rather than genuine functional necessity.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: DISTRIBUTED RESILIENCE INITIATIVE (SCAFFOLD) — Organized multinational effort (SEMATECH, industry consortia, government partnerships) to build redundancy and regional capacity. Has explicit sunset logic: as regional capacity matures (10-15 year horizon), dependence on single-source fabs decreases, and extraction mechanisms lose force. Benefits from temporary coordination; bearable temporary constraints due to visible exit strategy.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From civilizational scale, some supply chain concentration is inherent to semiconductor physics: extreme capital requirements, process complexity, economy of scale requirements, and precision tolerances create natural monopoly conditions. This perspective sees the constraint as an immutable physical/economic law. Engine will flag as false summit: the concentration is partly contingent institutional arrangement (IP monopoly, export controls, subsidy targeting) not purely thermodynamic.
constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_supply_chain_resilience_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_supply_chain_resilience, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_supply_chain_resilience, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_supply_chain_resilience, TR),
    TR >= 0.70.

:- end_tests(semiconductor_supply_chain_resilience_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. TSMC's 90%+ share of advanced nodes allows sustained premium pricing (15-20% above foundry cost) and volume penalties for customer diversity. ASML's EUV near-monopoly captures similar rents. But extraction is not total because: (1) some customers have partial backward-integration options (IDMs, some large fabless firms), (2) older nodes offer imperfect substitutes, (3) geopolitical fragmentation creates alternative supply sources (Samsung, future EU/US capacity). If TSMC had 95%+ and no geographic alternatives, extractiveness would exceed 0.70. Current 0.58 reflects mixed coordination and extraction. Suppression (0.68): High. Significant barriers include: (1) capital requirements block entry ($10B+ fab), (2) technology lock-in (switching costs 18-36 months), (3) export controls (EUV access, China restrictions), (4) IP licensing (process technology licensed to few partners), (5) demand volatility creates forced inventory holdings. But suppression is not maximum because: (1) some customers self-manufacture at older nodes, (2) regional alternatives emerging (Samsung, Intel Foundry Services), (3) state subsidies reducing effective barriers for supported players. Theater ratio (0.55): Moderate. Substantial performative activity: (1) production announcements far exceed actual capacity ramp (Intel, Samsung), (2) geopolitical commitments to build distributed capacity mostly unfunded, (3) supply allocation committees create appearance of coordination without actual sharing mechanisms, (4) strategic stockpiles serve political more than operational purposes. But theater is not dominant because genuine supply constraints are real — the 2021-2023 shortage was not purely theatrical, production ramps face real technical barriers.
 *
 * PERSPECTIVAL GAP:
 *   The gap between TSMC's Rope perspective and consumer OEM's Snare perspective is the diagnostic signal. TSMC genuinely solves coordination problems (scheduling, yield, quality). But the same institutional arrangement extracts from downstream manufacturers through monopoly pricing. No observer position is 'wrong' — each captures a real structural phenomenon. The perspective distribution (Rope × 2, Tangled Rope × 2, Snare × 2, Scaffold × 1, Piton × 1, Mountain × 1) shows that coordination and extraction are genuinely mixed at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   TSMC benefits from the constraint (low d) because their power and arbitrage options mean the supply bottleneck they control generates rents. Consumer OEM victims have high d because they are trapped paying those rents with no escape. The institutional actors (TSMC, ASML, Samsung) have d-values around 0.10-0.20 (beneficiary side of sigmoid). The powerless actors (OEM manufacturers, developing nation producers) have d-values around 0.85-0.95 (target side). Organized actors (states, coalitions) have intermediate d ≈ 0.50-0.65 because they bear costs of subsidies and disruption but exercise agency through policy. The wide spread in d values explains why this is neither pure rope nor pure snare — it's genuinely tangled across the institutional landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through constraint family decomposition. This constraint oscillates between two structural mechanisms: (1) genuine coordination bottleneck (semiconductor fabrication requires massive capital and process complexity — real coordination problem), and (2) extractive monopoly rent-taking (TSMC, ASML using technological dominance to extract from downstream). These are NOT two interpretations of the same constraint — they are structurally distinct. The resolution declares the claimed_type as tangled_rope because both mechanisms are present simultaneously and interdependent. TSMC cannot extract absent genuine production bottleneck; the bottleneck cannot be solved absent TSMC's capacity. The mandatrophy is false mislabeling only if the observer claims pure coordination (Rope, ignoring extraction) or pure extraction (Snare, ignoring coordination). The indexed perspectival set correctly shows both. Extractiveness (0.58) is high enough to avoid misclassification as pure Rope; suppression (0.68) and active enforcement (true) confirm it's not a Snare; theater (0.55) and the visible industrial policy response suggest neither pure natural law nor pure performative degradation. The constraint's real nature is captured by the multi-perspective set: it IS tangled, and that's the analytically correct classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_fragmentation_threshold,
    'At what point does geographic distribution of capacity (China, Taiwan, US, EU) cross a threshold from redundancy to inefficiency?',
    'Cost-benefit analysis of distributed vs centralized production; measurement of yield loss, capex duplication, and logistics overhead; empirical comparison of resilience gains vs efficiency costs',
    'If threshold < 3 regional leaders: current fragmentation is extractive (concentrated actors benefit, distributed capacity bears costs). If threshold > 5 leaders: distributed resilience is net beneficial coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_fragmentation_threshold, empirical, 'Optimal degree of geographic distribution for supply chain resilience').

omega_variable(
    export_control_necessity,
    'Do export controls on advanced semiconductors and lithography equipment serve genuine national security or are they primarily rent-extraction mechanisms protecting incumbent suppliers?',
    'Strategic analysis of actual military/dual-use requirements vs restrictions imposed; comparison of allowed technology nodes vs security threat landscape; measurement of market access restrictions unrelated to dual-use risk',
    'If genuine security necessity: export controls are legitimate coordination/enforcement. If primarily rent-protection: controls are snare-class extraction with geopolitical cover story. Classification shifts from tangled_rope to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(export_control_necessity, conceptual, 'Whether export controls serve security or protect incumbent rents').

omega_variable(
    subsidy_sustainability,
    'Can state-subsidized distributed fab capacity (CHIPS Act, EU Chips Act) achieve economic viability without permanent subsidy, or do these create permanently stranded assets?',
    '5-10 year longitudinal tracking of fab profitability without subsidy; analysis of capacity utilization, cost curves, and competitive positioning vs pure-play foundries; measurement of subsidy withdrawal effects',
    'If viable: scaffold perspective confirmed (sunset is real, temporary coordination). If permanently dependent on subsidy: constraint becomes a snare in which states bear extraction to support politically-favored manufacturers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidy_sustainability, empirical, 'Long-term viability of subsidized distributed fab capacity').

omega_variable(
    alternative_lithography_feasibility,
    'Could competing lithography approaches (nanoimprint, electron beam, alternative plasma sources) viably challenge EUV monopoly if IP and export restrictions were removed?',
    'Technical feasibility assessment of alternative approaches; cost analysis of scaling alternative methods; market simulation of competitive pricing if barriers removed; historical precedent analysis (Intel''s process leadership, Samsung''s advances)',
    'If viable alternatives exist: ASML monopoly is artificially sustained extraction (snare). If true technical monopoly: constraint approaches mountain (natural dominance). If limited viability: tangled coordination with some genuine technical barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_lithography_feasibility, empirical, 'Viability of alternative lithography technologies to challenge EUV dominance').

omega_variable(
    industrial_policy_coordination_success,
    'Do subsidies and coordinated industrial policy for semiconductor resilience generate genuine supply chain improvements or merely redistribute rents to incumbent manufacturers and equipment suppliers?',
    'Measurement of cost reduction vs target; comparison of subsidized capacity innovation vs private R&D pace; analysis of rent extraction to equipment suppliers and design firms; measurement of actual supply security improvements',
    'If effective coordination: policy creates rope-class coordination benefits. If pure rent redistribution: policy sustains and expands extraction mechanisms, pushing constraint toward pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(industrial_policy_coordination_success, empirical, 'Whether industrial policy subsidies generate coordination benefits or rent redistribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_supply_chain_resilience, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semisupply_tr_t0, semiconductor_supply_chain_resilience, theater_ratio, 0, 0.42).
narrative_ontology:measurement(semisupply_tr_t3, semiconductor_supply_chain_resilience, theater_ratio, 3, 0.48).
narrative_ontology:measurement(semisupply_tr_t6, semiconductor_supply_chain_resilience, theater_ratio, 6, 0.55).
narrative_ontology:measurement(semisupply_tr_t10, semiconductor_supply_chain_resilience, theater_ratio, 10, 0.55).
narrative_ontology:measurement(semisupply_tr_t15, semiconductor_supply_chain_resilience, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(semisupply_be_t0, semiconductor_supply_chain_resilience, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(semisupply_be_t3, semiconductor_supply_chain_resilience, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(semisupply_be_t6, semiconductor_supply_chain_resilience, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(semisupply_be_t10, semiconductor_supply_chain_resilience, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(semisupply_be_t15, semiconductor_supply_chain_resilience, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_supply_chain_resilience, resource_allocation).
narrative_ontology:affects_constraint(semiconductor_supply_chain_resilience, electronics_supply_chain_fragility).
narrative_ontology:affects_constraint(semiconductor_supply_chain_resilience, rare_earth_supply_concentration).
narrative_ontology:affects_constraint(semiconductor_supply_chain_resilience, geopolitical_technology_control).
narrative_ontology:affects_constraint(semiconductor_supply_chain_resilience, lithography_equipment_monopoly).

% DUAL FORMULATION NOTE:
% The semiconductor supply chain decomposes into multiple structurally distinct constraints: (1) fabrication capacity concentration (this story, ε=0.58), (2) equipment supplier monopoly (lithography, ε=0.65), (3) rare materials dependency (tantalum, cobalt, ε=0.52), (4) geopolitical fragmentation (trade controls, ε=0.48). Each has distinct ε value, beneficiary/victim structure, and temporal dynamics. This story covers the foundry capacity layer. Upstream equipment and materials constraints feed into it via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_supply_chain_resilience, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
