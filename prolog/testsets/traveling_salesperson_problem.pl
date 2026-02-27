% ============================================================================
% CONSTRAINT STORY: traveling_salesperson_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_traveling_salesperson_problem, []).

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
 *   constraint_id: traveling_salesperson_problem
 *   human_readable: Applied Logistics Optimization (based on TSP)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Applied TSP optimization in logistics creates a hybrid constraint
 *   combining genuine coordination value (solving a computationally hard
 *   problem) with asymmetric extraction (vendor lock-in, data asymmetry,
 *   worker schedule coercion). The coordination problem is real: routing
 *   large delivery networks to minimize distance/time is NP-complete, and
 *   heuristic solutions provide substantial cost reductions. But the
 *   extraction mechanism is equally real: proprietary algorithms become black
 *   boxes, vehicle and demand data concentrate in vendor platforms, and
 *   algorithmic enforcement imposes schedule rigidity on workers without
 *   corresponding wage compensation. This constraint exhibits six distinct
 *   classification perspectives, revealing how the same technical
 *   optimization problem appears as pure extraction to independent workers,
 *   mixed coordination-extraction to small operators, pure coordination to
 *   large operators and vendors, a temporary problem being solved by
 *   open-source alternatives, degraded academic research, and potentially a
 *   natural law of computation. The theater_ratio has risen from 0.35 to 0.58
 *   over the measurement interval, indicating that performative elements
 *   (vendor optimization benchmarking, claims of 'AI-driven efficiency,' SaaS
 *   dashboard complexity) are increasingly substituting for actual
 *   transparency about how optimization value is distributed.
 *
 * KEY AGENTS:
 *   - Logistics Optimization Vendors: Primary beneficiary (institutional/arbitrage) — extract value through algorithmic propriety, data asymmetry, and vendor lock-in; can serve multiple industries and geographies
 *   - Large Fleet Operators: Secondary beneficiary (organized/mobile) — capture optimization benefits; have resources and leverage to develop alternatives or negotiate favorable terms
 *   - Small Parcel Services: Primary victim (moderate/constrained) — forced to adopt systems under competitive necessity; lack leverage to negotiate or develop alternatives; bear vendor lock-in costs
 *   - Independent Couriers: Primary victim (powerless/trapped) — subject to algorithmic schedule enforcement with no autonomy or negotiating power; cannot exit without losing market access
 *   - Route Worker Autonomy: Secondary victim (powerless/trapped) — schedule rigidity increases without wage compensation; drivers bear coercive enforcement of optimization without benefit distribution
 *   - Open-Source Optimization Movement: Organized advocates (organized/constrained) — building alternative pathways (OSRM, VROOM); see vendor extraction as temporary problem with sunset
 *   - Academic TSP Research: Institutional actor (institutional/arbitrage) — maintains performative research apparatus on canonical hard problem increasingly disconnected from real logistics constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(traveling_salesperson_problem, 0.38).
domain_priors:suppression_score(traveling_salesperson_problem, 0.42).
domain_priors:theater_ratio(traveling_salesperson_problem, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(traveling_salesperson_problem, extractiveness, 0.38).
narrative_ontology:constraint_metric(traveling_salesperson_problem, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(traveling_salesperson_problem, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(traveling_salesperson_problem, tangled_rope).
narrative_ontology:human_readable(traveling_salesperson_problem, "Applied Logistics Optimization (based on TSP)").
narrative_ontology:topic_domain(traveling_salesperson_problem, "economic/technological").

domain_priors:requires_active_enforcement(traveling_salesperson_problem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(traveling_salesperson_problem, logistics_optimization_vendors).
narrative_ontology:constraint_beneficiary(traveling_salesperson_problem, large_fleet_operators).
narrative_ontology:constraint_victim(traveling_salesperson_problem, independent_couriers).
narrative_ontology:constraint_victim(traveling_salesperson_problem, small_parcel_services).
narrative_ontology:constraint_victim(traveling_salesperson_problem, route_worker_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT COURIER (SNARE) — Trapped within algorithmic routing systems that extract delivery efficiency gains while imposing zero schedule autonomy. Cannot exit without losing market access. Experiences maximum extraction through algorithmic enforcement of optimization solutions that benefit logistics platforms.
constraint_indexing:constraint_classification(traveling_salesperson_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL PARCEL SERVICE (TANGLED ROPE) — Constrained by adoption costs and competitive necessity to implement TSP-based routing. Experiences both coordination benefit (access to optimization) and extraction (vendor lock-in, proprietary algorithms, data asymmetry). Limited exit due to market consolidation but some agency through vendor selection.
constraint_indexing:constraint_classification(traveling_salesperson_problem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LOGISTICS OPTIMIZATION VENDOR (ROPE) — Benefits from coordination problem (TSP is genuinely hard; vendors provide real solution). Experiences the constraint as a standard: communication of routing requirements enables delivery network optimization. Primary beneficiary with arbitrage options (can serve multiple industries, geographies).
constraint_indexing:constraint_classification(traveling_salesperson_problem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE FLEET OPERATOR (ROPE) — Organized institutional actor with resources to develop proprietary optimization or negotiate favorable vendor terms. Experiences TSP solution as enabling infrastructure. Mobile exit options: can build in-house systems, switch vendors, or negotiate volume discounts.
constraint_indexing:constraint_classification(traveling_salesperson_problem, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE OPTIMIZATION MOVEMENT (SCAFFOLD) — Organized advocates (OSRM, VROOM, open routing libraries) are building alternative verification pathways for TSP solutions. Sees vendor lock-in as a temporary coordination failure with a sunset: distributed, auditable, non-proprietary optimization infrastructure is developing. Estimated sunset: 5-10 years for open-source routing to achieve parity with commercial systems for mid-market operators.
constraint_indexing:constraint_classification(traveling_salesperson_problem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC TSP RESEARCH APPARATUS (PITON) — Decades of TSP scholarship (NP-completeness proofs, approximation algorithms, heuristic benchmarks) persist in academic curricula and journals despite diminishing relevance to actual logistics applications. The real-world problem has shifted from 'find the optimal route' to 'optimize under constraints: delivery windows, vehicle capacity, driver labor law, demand variability.' Academic TSP is largely performative: a canonical hard problem that demonstrates algorithmic sophistication but whose optimization results rarely influence deployed systems. The apparatus persists through institutional inertia.
constraint_indexing:constraint_classification(traveling_salesperson_problem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Traveling Salesperson Problem is a fundamental computational constraint: the combinatorial explosion of possible routes creates an inherent verification gap between claimed optimality and actual system performance. No logistics system can compute the true optimum for large networks. This perspective risks naturalizing a contingent institutional choice (proprietary heuristics + vendor lock-in) as a law of computation. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(traveling_salesperson_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(traveling_salesperson_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(traveling_salesperson_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(traveling_salesperson_problem, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(traveling_salesperson_problem, TR),
    TR >= 0.70.

:- end_tests(traveling_salesperson_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The constraint generates genuine coordination value (TSP solutions reduce delivery costs measurably), but extraction mechanisms include vendor lock-in, proprietary algorithms, data concentration, and worker schedule coercion. The score reflects that coordination and extraction are genuinely mixed — this is not pure extraction (which would be 0.66+), but it's also not low extraction (0.05). The rise from 0.22 to 0.38 over the interval reflects increasing vendor market power and consolidation. Suppression (0.42): Moderate. Significant barriers include switching costs, integration lock-in, lack of algorithmic transparency for proprietary systems, and workers' inability to organize against algorithmic control. But suppression is not total — open-source alternatives exist, some operators have built in-house systems, and labor regulations provide some floor (though weak). Theater ratio (0.58): Moderate-high. Vendors increasingly market 'AI-driven optimization' and 'intelligent routing' as opaque black-box value propositions. Academic conferences celebrate TSP algorithms despite their limited relevance to constrained real-world problems. Dashboard complexity and optimization benchmarking serve performative rather than transparency functions. Open-source alternatives use simpler, auditable heuristics with lower theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits profound perspectival gaps. The vendor sees a pure coordination mechanism (Rope) — they solve a hard problem and enable network optimization. The large operator sees the same thing at their scale (Rope). The open-source movement sees a temporary coordination failure being solved (Scaffold) — distributed, auditable optimization is maturing. But the small operator sees mixed extraction and coordination (Tangled Rope) — real benefit but also lock-in. The independent courier sees pure extraction (Snare) — algorithmic control with no benefit. The academic establishment sees a canonical hard problem (Mountain) — NP-completeness as a law of computation. The analytical observer recognizes this mountain as a false summit: the 'hardness' of TSP is real (NP-complete), but the extraction lies not in computational complexity but in institutional choices (proprietary algorithms, data asymmetry, worker control).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) depends on their structural position relative to the constraint. Large operators and vendors have low d values (beneficiaries with exit options) — they experience low or negative χ; extraction runs toward them. Independent couriers have high d values (trapped victims) — they experience maximum χ. Small operators have medium-high d values (constrained victims) — they experience significant but not maximal χ. The open-source movement has lower d than vendors (organized with mobile exit options) — they experience lower χ and see the constraint as temporary. The academic apparatus occupies a peculiar position: institutional beneficiary (gets research funding, publications from TSP work) with arbitrage options but increasingly disconnected from real extraction flows. The directionality derives from who benefits from the optimization gap and who bears the costs of proprietary solutions.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID CLASSIFICATION: This constraint resolves mandatrophy by showing that the same technical optimization problem contains both genuine coordination (TSP is hard; heuristic solutions are valuable) and asymmetric extraction (vendor lock-in, data concentration, worker coercion). The tangled_rope classification captures this duality: the constraint provides real coordination value (lower beneficiaries without this would have dramatically worse logistics costs) while simultaneously enabling asymmetric extraction (vendors and large operators capture optimization gains; workers bear schedule rigidity). The vendor's Rope perspective and the worker's Snare perspective are not measuring different constraints — they're measuring the same constraint from positions with radically different d values. The scaffold perspective (open-source movement) shows a real structural feature: distributed alternative optimization is maturing and will eventually reduce extraction. The piton perspective reveals that academic TSP research has become performative — the canonical hard problem persists in curricula and conferences not because it drives logistics innovation but because it's institutionally entrenched. The mountain perspective is exposed as a false summit: it naturalizes the institutional choice (proprietary black-box optimization) as a law of computation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_sufficiency,
    'Does transparency in routing algorithms (open-source code, auditable heuristics) substantively reduce extraction, or does data asymmetry (proprietary demand forecasts, vehicle fleet data) remain the primary extraction mechanism?',
    'Comparative case study: open-source OSRM vs proprietary systems across equivalent logistics networks; measurement of route efficiency gains retained by vendors vs passed to operators; analysis of data ownership and access restrictions',
    'If transparency sufficient: scaffold sunset is real and extraction ceiling is lower. If data asymmetry primary: open-source fails to disrupt vendor extraction and extraction remains high despite algorithmic transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether algorithmic transparency or data asymmetry drives extraction').

omega_variable(
    worker_schedule_autonomy_cost,
    'What portion of logistics cost reduction comes from genuinely optimal routing versus coercive schedule rigidity that extracts worker flexibility without corresponding wage increases?',
    'Labor economics analysis: comparison of worker compensation, schedule autonomy indices, and injury rates pre- and post-TSP implementation; cross-sector comparison with less algorithmically managed delivery services; survey data on worker experience of schedule discretion',
    'If significant portion from schedule coercion: snare classification for workers is confirmed and suppression score is underestimated. If primarily from routing optimization: tangled_rope classification is more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_schedule_autonomy_cost, empirical, 'Portion of cost reduction from worker schedule rigidity vs routing optimization').

omega_variable(
    small_operator_exit_viability,
    'Can small parcel services realistically exit from proprietary TSP systems and adopt open-source alternatives, or do network effects and integration lock-in make exit impossible regardless of technical availability?',
    'Market analysis: cost and time required for small operators to migrate from proprietary to open-source systems; measurement of network effects (integrations with customer platforms, payment systems, vehicle tracking); case studies of attempted migrations and their success rates',
    'If exit viable: constrained exit_options is accurate and tangled_rope classification holds. If exit blocked: exit_options should be trapped and classification should shift toward snare for small operators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_operator_exit_viability, empirical, 'Whether small operators can realistically exit from proprietary vendor systems').

omega_variable(
    optimization_plateau_point,
    'Beyond what problem size or complexity does algorithmic routing optimization yield marginal improvement, and at what cost does further optimization extraction exceed coordination benefit?',
    'Logistics engineering analysis: cost-benefit curves for heuristic routing at different network scales; measurement of vendor optimization value delivered at different operator sizes; identification of break-even points where open-source solutions achieve equivalent performance',
    'If plateau occurs at small network scales: many small operators are in pure extraction regime (snare). If plateau is high: tangled_rope classification broadly holds and coordination benefit is substantial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimization_plateau_point, empirical, 'Problem size at which routing optimization yields marginal returns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(traveling_salesperson_problem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsp_tr_t0, traveling_salesperson_problem, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tsp_tr_t5, traveling_salesperson_problem, theater_ratio, 5, 0.48).
narrative_ontology:measurement(tsp_tr_t10, traveling_salesperson_problem, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(tsp_be_t0, traveling_salesperson_problem, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(tsp_be_t5, traveling_salesperson_problem, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(tsp_be_t10, traveling_salesperson_problem, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(traveling_salesperson_problem, resource_allocation).
narrative_ontology:affects_constraint(traveling_salesperson_problem, algorithmic_labor_control).
narrative_ontology:affects_constraint(traveling_salesperson_problem, supply_chain_data_asymmetry).
narrative_ontology:affects_constraint(traveling_salesperson_problem, platform_operator_lock_in).

% DUAL FORMULATION NOTE:
% Applied TSP optimization in logistics decomposes into three structurally distinct constraints: (1) the computational hardness of finding optimal routes (mountain-like, ε~0.08), (2) the economic extraction through vendor lock-in and data concentration (snare-like, ε~0.55 for small operators), and (3) the coercive enforcement of worker schedules (snare-like, ε~0.48 for couriers). This story models the tangled integration of all three. Upstream: computational complexity (inherent, immutable). Downstream: labor control systems and supply chain financialization (contingent, extractive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(traveling_salesperson_problem, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
