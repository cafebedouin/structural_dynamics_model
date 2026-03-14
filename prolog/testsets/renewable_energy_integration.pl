% ============================================================================
% CONSTRAINT STORY: renewable_energy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_renewable_energy_integration, []).

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
 *   constraint_id: renewable_energy_integration
 *   human_readable: Renewable Energy Integration into Existing Grid Infrastructure
 *   domain: energy_infrastructure/regulatory_economics
 *
 * SUMMARY:
 *   The renewable energy integration constraint manifests as a structural
 *   tension between incumbent utility business models (guaranteed cost
 *   recovery) and distributed generation economics. The constraint exhibits
 *   mixed coordination and extraction: grid operators genuinely solve the
 *   technical problem of demand-supply matching across diverse renewable
 *   sources, while incumbent utilities use interconnection queuing,
 *   cost-allocation rules, and reliability rhetoric to protect thermal
 *   generation revenues. The extractiveness trajectory (0.38→0.52) reflects
 *   increasing rent-seeking behavior as renewable penetration threatens
 *   incumbent margins. Theater ratio increase (0.42→0.58) indicates growing
 *   performative justification — the 'reliability requirement' framing
 *   intensifies as the technical case for incumbent dominance weakens
 *   (demonstrated by Denmark, Germany success at high penetration). FERC
 *   Order 2023-B and state-level queue reforms represent genuine sunset
 *   mechanisms: three-year timelines for standardized interconnection
 *   procedures, cost allocation reforms, and distributed generation
 *   integration. However, the constraint remains tangled because incumbent
 *   utilities retain coordination function (transmission operation, frequency
 *   regulation, voltage control) alongside their extractive rent-seeking. The
 *   constraint cannot be classified as pure Rope (coordination exists) or
 *   pure Snare (extraction is real but not total). The scaffold perspective
 *   captures the reform trajectory, but this is not yet the dominant
 *   experience for small distributed solar developers, who remain trapped in
 *   multi-year queues.
 *
 * KEY AGENTS:
 *   - Incumbent Fossil-Fuel Utility: Primary beneficiary (institutional/constrained) — extracts margin protection through interconnection barriers; also provides genuine coordination function
 *   - Distributed Solar Developer: Primary victim (powerless/trapped) — faces 3-7 year interconnection queues and unilateral cost allocation
 *   - Grid Operator / ISO: Institutional beneficiary (institutional/arbitrage) — genuinely solves coordination problem, recovers costs through mandated fees
 *   - Utility-Scale Renewable Developer: Secondary victim (moderate/constrained) — can appeal and litigate but faces regional transmission bottlenecks
 *   - Public Utility Commission: Regulatory institution (institutional/constrained) — theoretically independent but subject to incumbent influence and capture risk
 *   - RTO Reform Coalition: Organized reformers (organized/mobile) — state regulators, renewable industry, environmental groups pushing FERC Order 2023-B implementation with sunset timeline
 *   - Grid Reliability Justification: Rhetorical actor (institutional/arbitrage) — maintains performative cover for incumbent protection despite contradictory international evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(renewable_energy_integration, 0.52).
domain_priors:suppression_score(renewable_energy_integration, 0.48).
domain_priors:theater_ratio(renewable_energy_integration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(renewable_energy_integration, extractiveness, 0.52).
narrative_ontology:constraint_metric(renewable_energy_integration, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(renewable_energy_integration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(renewable_energy_integration, tangled_rope).
narrative_ontology:human_readable(renewable_energy_integration, "Renewable Energy Integration into Existing Grid Infrastructure").
narrative_ontology:topic_domain(renewable_energy_integration, "energy_infrastructure/regulatory_economics").

domain_priors:requires_active_enforcement(renewable_energy_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(renewable_energy_integration, incumbent_fossil_fuel_operators).
narrative_ontology:constraint_beneficiary(renewable_energy_integration, grid_operators_with_coordination_function).
narrative_ontology:constraint_victim(renewable_energy_integration, renewable_developers).
narrative_ontology:constraint_victim(renewable_energy_integration, grid_stability_as_public_good).
narrative_ontology:constraint_victim(renewable_energy_integration, ratepayers_with_distributed_solar).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISTRIBUTED SOLAR DEVELOPER (SNARE) — Small solar installers and homeowners face interconnection queues spanning 3-7 years, technical studies mandated by incumbent utilities, and unilateral cost allocation (standby charges, distribution upgrades borne by developers). No meaningful exit: cannot bypass grid, cannot appeal technical requirements, cannot recover study costs if project is rejected. Maximum suppression via procedural barriers.
constraint_indexing:constraint_classification(renewable_energy_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UTILITY-SCALE RENEWABLE DEVELOPER (TANGLED ROPE) — Can appeal regulatory decisions, has resources for legal challenge, benefits from capacity auctions and power purchase agreements (coordination function), but faces transmission bottlenecks, interconnection cost barriers, and curtailment requirements during grid stress. Constrained exit — can build elsewhere but faces similar barriers region-to-region. Mixed extraction and coordination.
constraint_indexing:constraint_classification(renewable_energy_integration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GRID OPERATOR / ISO (ROPE) — Genuinely solves collective action problem: manages real-time demand-supply matching, frequency regulation, voltage stability. Experiences renewable integration as a coordination challenge requiring new mechanisms (faster ramping, demand response, storage coordination). Net beneficiary through mandated cost recovery plus new coordination services revenue. Sees extraction as technical fee, not coercive overhead.
constraint_indexing:constraint_classification(renewable_energy_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT FOSSIL-FUEL UTILITY (TANGLED ROPE) — Benefits from regulatory asset base guarantees and cost-of-service recovery (coordination with ratepayers on rate structure). Simultaneously faces stranded asset risk and declining thermal generation revenues. Experiences renewable integration as extraction (lost margin) plus coordination obligation (must maintain reliability). Constrained exit — cannot abandon franchise territory. Asymmetric extraction targeting renewable developers.
constraint_indexing:constraint_classification(renewable_energy_integration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RTO REFORM COALITION (SCAFFOLD) — Organized stakeholders (renewable trade associations, state regulators, environmental groups) see interconnection bottleneck as a temporary coordination failure with a sunset clause: Order 2023-B (FERC) mandates three-year timeline for interconnection queue reforms, cost allocation reforms, and interconnection service agreements standardization. High agency, clear exit path. Theater remains high during reform transition (still performative queue processing) but declining as reforms take effect. Sunset: 5-10 years as reforms mature and distributed generation integration mechanisms standardize.
constraint_indexing:constraint_classification(renewable_energy_integration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: GRID RELIABILITY JUSTIFICATION (PITON) — 'Grid reliability requires incumbent dominance' framing persists through institutional inertia despite contradictory evidence: Denmark and Germany achieve 60%+ renewable penetration without sacrificing reliability. The reliability argument is mostly performative cover for incumbent protection. Theater ratio high (0.68) — the argument is ritualized and repeated but functionally decoupled from actual reliability mechanisms. Piton classification reflects degraded justification, not high extraction magnitude.
constraint_indexing:constraint_classification(renewable_energy_integration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL LIMITS (MOUNTAIN) — From a civilizational/universal perspective, some grid coordination overhead is immutable: frequency regulation, voltage stability, and demand-supply matching are physics-based constraints regardless of generation source. This perspective risks naturalizing contingent institutional arrangements (interconnection queues, cost allocation rules, regional balkanization) as inherent physical necessity. Engine false-summit detection should flag this as naturalization.
constraint_indexing:constraint_classification(renewable_energy_integration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(renewable_energy_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(renewable_energy_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(renewable_energy_integration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(renewable_energy_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(renewable_energy_integration, TR),
    TR >= 0.70.

:- end_tests(renewable_energy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, and rising over the interval. The constraint extracts margin protection from distributed generators via interconnection gatekeeping (distributed solar faces 3-7 year queues; utility-scale faces transmission bottlenecks). The rising trajectory reflects increasing incumbent rent-seeking as renewable penetration threatens thermal generation revenues. However, extractiveness is not at snare levels (0.66+) because: (1) grid operators provide genuine coordination value, (2) utility-scale developers have negotiating capacity and PPA access, (3) international evidence (Denmark 80% renewables, Germany 60%+) proves reliability is achievable at high penetration, (4) reform mechanisms (FERC 2023-B) are creating structured exit paths. Suppression (0.48): Moderate. Interconnection queue delays, technical study mandates, and cost-allocation rules create real barriers to entry, but they are not total: some projects succeed, appeals exist, and regulatory forums can override utility decisions. Theater ratio (0.58): Moderate-high. The 'grid reliability requires incumbent dominance' argument is increasingly performative: actual reliability outcomes in high-penetration jurisdictions contradict the claim. Theater has grown from 0.42 to 0.58 as contradictions accumulate and the rhetorical burden increases. This rising theater ratio combined with moderating extractiveness is diagnostic of a constraint in transition: reform mechanisms are reducing actual extraction (developers finding alternative paths) while incumbent rhetoric intensifies to justify the remaining extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a clean perspectival stratification by power level and exit capacity. Powerless/trapped agents (distributed solar) see pure extraction (Snare) — procedural barriers are insurmountable. Moderate/constrained agents (utility-scale developers) see mixed coordination and extraction (Tangled Rope) — they have resources to negotiate but face real limitations. Institutional agents with arbitrage (grid operators, utilities benefiting from new coordination) see pure coordination (Rope). Organized/mobile agents (reform coalition) see a temporary problem with a real sunset (Scaffold) — FERC timelines and state regulatory windows provide structured exit paths. Piton classification (institutional/arbitrary perspective) reflects that the 'grid reliability' justification persists through inertia despite international evidence that high renewable penetration is compatible with reliability. The mountain classification (analytical/universal) represents the false summit risk: naturalizing incumbent-specific coordination requirements (frequency regulation, voltage control) as physics-based barriers immutable to any organization structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position: who benefits, who bears costs, and at what exit capacity. Incumbent utilities are beneficiaries with constrained exit (cannot abandon franchise) — derived d ≈ 0.35-0.45 (beneficiary status lowers d; constrained exit moderates this). Distributed solar developers are victims with trapped exit — derived d ≈ 0.92 (victim + trapped → maximum d → maximum f(d) ≈ 1.42). Grid operators are beneficiaries with arbitrage (can shift to different coordination mechanisms or privatize) — derived d ≈ 0.10-0.15 (beneficiary + arbitrage → very low d). Utility-scale developers are victims with constrained exit — derived d ≈ 0.75 (victim + constrained). The reform coalition is organized with mobile exit (can push regulation, can invest in alternative structures) — derived d ≈ 0.40-0.50 (organized + mobile moderates both victim and beneficiary signals). The directionality signature reveals the asymmetry: victims are either fully trapped (distributed) or significantly constrained (utility-scale), while beneficiaries have exit options. This asymmetry is the mechanism driving extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that all six types are legitimate perspectival readings but the scaffold and piton perspectives capture the structural transformation. The snare perspective (powerless/trapped distributed solar) is accurate from that agent's position but not representative of the whole constraint. The rope perspective (grid coordination) is accurate but ignores the extraction layered on top. The tangled rope classification (claimed type) is the appropriate analytical reading because it acknowledges both genuine coordination function (grid operators solving real technical problems) and asymmetric extraction (interconnection barriers protecting incumbent revenues). The scaffold perspective validates that reform mechanisms (FERC 2023-B, state queue reforms) are real and structurally consequential — this is not aspirational; it is a structured sunset with regulatory teeth. The piton perspective (grid reliability rhetoric) is functionally true — the 'reliability requires incumbents' framing is increasingly disconnected from operational reality as high-penetration jurisdictions prove reliability is independent of generation source mix. The mountain perspective (analytical) represents a real risk: the constraint can appear as an immutable physics problem (frequency regulation, voltage stability) when it is actually a contingent institutional arrangement. The mandatrophy is resolved by recognizing that the constraint is transitioning from snare/tangled-rope (incumbent extraction with coordination overlay) toward scaffold (temporary coordination problem being solved). This transition is driven by: (1) FERC regulatory action, (2) evidence from high-penetration regions, (3) distributed storage and demand-response providing alternative coordination mechanisms, and (4) incumbent utilities themselves investing in renewables. The analytical observer's task is to measure whether the transition is real or performative — are interconnection queues actually shortening, or is theater increasing to cover stagnation?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_extraction_vs_coordination_cost,
    'How much of the utility''s cost-recovery claim reflects genuine grid coordination burden vs. extraction rent protection?',
    'Comparative cost analysis: transmission/distribution costs in high-renewables regions (Denmark, Australia) vs. incumbent-dominated regions; correlation between renewable penetration and claimed reliability costs',
    'If genuine coordination cost > 0.30: extractiveness drops to 0.35, classification shifts toward rope. If <0.15: extractiveness rises to 0.65, classification shifts toward snare from incumbent perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_extraction_vs_coordination_cost, empirical, 'Decomposing utility cost recovery into coordination vs. extraction').

omega_variable(
    interconnection_queue_bottleneck_intentionality,
    'Are interconnection queue delays structural (caused by technical complexity and limited study resources) or intentional (used as gatekeeping mechanism)?',
    'Comparative study: queue delays in jurisdictions with FERC Order 2023-B reforms vs. pre-reform baselines; correlation between queue length and incumbent market power; cost-benefit analysis of accelerated study timelines',
    'If structural: suppression reduces to 0.30, classification shifts toward rope/scaffold for all developers. If intentional: suppression increases to 0.65, classification shifts toward snare for distributed solar.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interconnection_queue_bottleneck_intentionality, empirical, 'Whether interconnection delays are structural constraint or gatekeeping mechanism').

omega_variable(
    storage_and_demand_response_substitutability,
    'Do battery storage and demand-response mechanisms provide genuine alternatives to incumbent-controlled frequency regulation, or are they complementary rather than substitutes?',
    'Technical analysis: comparing ramping rates and frequency response capabilities of distributed storage+demand response vs. conventional generation; field trials in high-renewables regions',
    'If substitutes: grid operator''s monopoly on coordination weakens, extractiveness drops to 0.35, opens Rope classification. If complementary: incumbent retains coordination bottleneck, extractiveness remains 0.50+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_and_demand_response_substitutability, empirical, 'Whether distributed storage/demand-response substitute for incumbent frequency services').

omega_variable(
    regulatory_capture_depth_in_puc,
    'To what extent do state Public Utility Commissions make interconnection and cost-allocation decisions independent of incumbent utility preferences?',
    'Historical analysis: PUC decisions on contested interconnection cases; cross-state variance in approval rates and timelines; funding/revolving-door analysis of commissioners',
    'If independent: extractiveness drops to 0.38, incumbent power downgraded. If captured: extractiveness rises to 0.62, institutional actors experience identity_locked constraint rather than constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth_in_puc, empirical, 'Degree of regulatory capture in PUC interconnection decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(renewable_energy_integration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(renew_tr_t0, renewable_energy_integration, theater_ratio, 0, 0.42).
narrative_ontology:measurement(renew_tr_t5, renewable_energy_integration, theater_ratio, 5, 0.55).
narrative_ontology:measurement(renew_tr_t10, renewable_energy_integration, theater_ratio, 10, 0.58).
narrative_ontology:measurement(renew_tr_t15, renewable_energy_integration, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(renew_be_t0, renewable_energy_integration, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(renew_be_t5, renewable_energy_integration, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(renew_be_t10, renewable_energy_integration, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(renew_be_t15, renewable_energy_integration, base_extractiveness, 15, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(renewable_energy_integration, global_infrastructure).
narrative_ontology:boltzmann_floor_override(renewable_energy_integration, 0.18).
narrative_ontology:affects_constraint(renewable_energy_integration, grid_frequency_regulation_monopoly).
narrative_ontology:affects_constraint(renewable_energy_integration, transmission_congestion_pricing).
narrative_ontology:affects_constraint(renewable_energy_integration, stranded_asset_protection_regulatory_capture).

% DUAL FORMULATION NOTE:
% Renewable integration decomposes into three structurally distinct constraints: (1) GRID FREQUENCY REGULATION — technical coordination problem (low ε, rope); (2) INTERCONNECTION QUEUE GATEKEEPING — incumbent protection mechanism (high ε, snare from distributed perspective); (3) STRANDED ASSET RECOVERY — regulatory capture of cost-allocation rules (high ε, tangled rope). This story addresses the aggregate constraint; downstream stories model the specific mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(renewable_energy_integration, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
