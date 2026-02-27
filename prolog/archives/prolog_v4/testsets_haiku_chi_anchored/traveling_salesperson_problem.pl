% ============================================================================
% CONSTRAINT STORY: traveling_salesperson_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The Traveling Salesperson Problem, a canonical NP-hard optimization
 *   problem in computer science, has been industrialized into logistics
 *   platforms (Uber Freight, Amazon Logistics, DHL Route Optimizer) that use
 *   TSP-derived algorithms to route delivery vehicles. This constraint
 *   examines the structural relationship between the computational
 *   optimization mechanism and the distribution of costs/benefits across
 *   delivery drivers, independent carriers, platform operators, and shippers.
 *   The application exhibits the full range of DR classification from
 *   different perspectives: pure coordination (platform operator view), pure
 *   extraction (delivery driver view), mixed coordination-extraction (small
 *   shipper view), temporary with a sunset pathway (regulatory reform view),
 *   and potentially immutable natural law (computational theory view). The
 *   constraint's extraction increases over time as platforms refine
 *   algorithmic efficiency, capture greater market share, and pass cost
 *   reductions selectively rather than uniformly across stakeholders.
 *
 * KEY AGENTS:
 *   - Delivery Driver: Primary victim (powerless/trapped) — locked into algorithmically optimized routes, unable to negotiate efficiency-safety trade-offs
 *   - Independent Carrier: Secondary victim (moderate/constrained) — small operators forced to adopt proprietary algorithms or exit market
 *   - Small Shipper: Mixed beneficiary-victim (moderate/constrained) — benefits from optimization but has no control over routing criteria or pricing
 *   - Logistics Platform Operator: Primary beneficiary (institutional/arbitrage) — captures network effects and scale benefits from TSP-derived routing
 *   - Algorithm Licensing Firm: Secondary beneficiary (organized/arbitrage) — monetizes TSP solutions across client base
 *   - Workforce Coalition: Organized victim (organized/constrained) — building alternative frameworks for transparent algorithm auditing
 *   - Regulatory Reform Movement: Organized agent (organized/constrained) — pursuing algorithm transparency mandates and open-source alternatives
 *   - Academic Research Establishment: Institutional observer (institutional/arbitrage) — maintains TSP as canonical problem despite secondary relevance to practical deployment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as computational necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(traveling_salesperson_problem, 0.38).
domain_priors:suppression_score(traveling_salesperson_problem, 0.42).
domain_priors:theater_ratio(traveling_salesperson_problem, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(traveling_salesperson_problem, extractiveness, 0.38).
narrative_ontology:constraint_metric(traveling_salesperson_problem, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(traveling_salesperson_problem, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(traveling_salesperson_problem, tangled_rope).
narrative_ontology:human_readable(traveling_salesperson_problem, "Applied Logistics Optimization (based on TSP)").
narrative_ontology:topic_domain(traveling_salesperson_problem, "economic/technological").

domain_priors:requires_active_enforcement(traveling_salesperson_problem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(traveling_salesperson_problem, logistics_platform_operators).
narrative_ontology:constraint_beneficiary(traveling_salesperson_problem, algorithm_licensing_firms).
narrative_ontology:constraint_victim(traveling_salesperson_problem, delivery_workforce).
narrative_ontology:constraint_victim(traveling_salesperson_problem, small_carrier_operators).
narrative_ontology:constraint_victim(traveling_salesperson_problem, route_optimization_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DELIVERY DRIVER (SNARE) — Locked into algorithmically optimized routes they cannot modify or fully understand. Exit options (employment) are tied to platform compliance. High suppression: no transparent cost allocation, no ability to negotiate route efficiency or safety trade-offs. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(traveling_salesperson_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT CARRIER (SNARE) — Small logistics operators cannot compete with platform-optimized networks. Forced to either adopt proprietary algorithms (paying licensing fees) or exit the market. Suppression is high: switching costs are large, alternatives are limited. d≈0.80, f(d)≈1.20, σ=0.9 → χ≈0.44.
constraint_indexing:constraint_classification(traveling_salesperson_problem, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SMALL SHIPPER (TANGLED ROPE) — Benefits from optimized routing (lower shipping costs) and reaches organized logistics networks; simultaneously constrained by platform-set pricing and no voice in optimization criteria. Mixed experience: genuine coordination benefit (access to efficiency) + asymmetric extraction (pricing power, black-box algorithms). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(traveling_salesperson_problem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LOGISTICS PLATFORM OPERATOR (ROPE) — Experiences TSP optimization as pure coordination: solving the vehicle routing problem enables scale, network effects, and customer satisfaction. Benefits from algorithmic efficiency without bearing extraction costs (passed to workforce and independent operators). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(traveling_salesperson_problem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALGORITHM LICENSING FIRM (ROPE) — Sees TSP solutions as enabling standardized, scalable coordination across clients. Captures licensing revenue; experiences the constraint as a coordination infrastructure that generates mutual benefit (logistics firms get efficient routing, licensing firm gets revenue). d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(traveling_salesperson_problem, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: WORKFORCE COALITION (TANGLED ROPE) — Organized labor sees TSP optimization as providing genuine coordination benefits (reduced accident rates from AI-optimized routes, lower fuel costs enabling higher wages) alongside systematic extraction (algorithmic speed-up, precision route timing, reduced worker autonomy). Coalition is building alternative frameworks (transparent algorithm auditing, worker-input optimization). d≈0.65, f(d)≈0.98, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(traveling_salesperson_problem, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATORY REFORM MOVEMENT (SCAFFOLD) — Sees current TSP application as a temporary institutional arrangement with a clear sunset. Algorithm transparency mandates, worker algorithm audits, and open-source routing solutions are building alternative pathways. The constraint is structured as coordination with extraction overlay; reform aims to remove extraction while retaining coordination. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(traveling_salesperson_problem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ACADEMIC RESEARCH ESTABLISHMENT (PITON) — TSP is intellectually iconic in computer science education and optimization research. University courses, textbooks, and research programs are built around TSP solutions. The actual logistics application (what this constraint describes) is secondary to the cultural/institutional investment in TSP as canonical problem. Theater ratio ≈ 0.60: significant effort spent on pedagogical and theoretical TSP work while practical deployment challenges remain unaddressed. d≈0.12, f(d)≈-0.06, σ=1.2 → χ≈-0.02.
constraint_indexing:constraint_classification(traveling_salesperson_problem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a computational theory standpoint, TSP is NP-hard: finding optimal solutions is computationally intractable for large instances. From this framing, the constraint appears as an immutable property of the computational landscape. However, the structural data (ε=0.38, suppression=0.42, theater=0.35) contradicts a mountain classification. The 'intractability' is a mathematical property of the decision problem; the extractiveness arises from how industrial applications of TSP solutions distribute costs and benefits—a contingent institutional arrangement, not a natural law.
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
 *   Extractiveness (0.38): Moderate. TSP optimization in logistics does produce genuine efficiency gains (lower fuel consumption, faster delivery, fewer vehicles needed), which are real coordination benefits. However, platforms capture disproportionate share of gains through opacity in route design, speed-up expectations, and pricing power. The extraction is not maximal (0.46+) because some benefits genuinely flow to shippers and users. But it is substantial because drivers bear costs (algorithmic intensity, route micro-management, safety trade-offs) without transparent benefit sharing. Suppression (0.42): Moderate-high. Workers are suppressed by: algorithmic opacity (they cannot understand or challenge routing logic), employment lock-in (refusal to follow routes means termination), and information asymmetry (platforms have complete routing data, workers have none). However, suppression is not absolute—open-source alternatives exist, workforce organizing is active, and regulatory pressure is mounting. Theater ratio (0.35): Low-moderate. TSP application is relatively functional—the algorithms actually optimize routes and produce measurable efficiency gains. However, some theater emerges in: academic framing of TSP as the 'real problem' when actual logistics deployment challenges are secondary; marketing narratives about 'algorithmic fairness' that obscure actual power asymmetries; and regulatory theater ('algorithm auditing' that doesn't actually affect routing decisions).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same computational optimization mechanism appears as pure coordination from the platform's perspective (Rope: solving a genuine technical problem), as pure extraction from the driver's perspective (Snare: locked routes, algorithmic intensification), and as mixed coordination-extraction from multiple intermediate perspectives. The independent carrier sees Snare (forced into adopting proprietary algorithms). The small shipper sees Tangled Rope (genuine access benefits + pricing oppression). The workforce coalition sees Tangled Rope (efficiency gains + autonomy loss, with potential for organized negotiation). The regulatory movement sees Scaffold (temporary institutional arrangement with a viable sunset through transparency mandates and open-source alternatives). The academic establishment maintains Piton (TSP remains intellectually central to computer science, but practical logistics challenges are treated as applications of a solved problem). The analytical observer risks Mountain classification (TSP is NP-hard, so optimization under constraints is 'natural'), but this naturalizes what is actually a contingent choice: whether platforms share optimization gains and whether workers participate in routing decisions.
 *
 * DIRECTIONALITY LOGIC:
 *   Delivery driver: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit employment or route decisions. Independent carrier: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction; significant exit barriers (switching costs, training, market concentration). Small shipper: Mixed (benefits from efficiency + victimized by pricing) + constrained → d≈0.68, f(d)≈1.05. Moderate extraction; genuinely benefits from optimization but has no agency in pricing/routing. Platform operator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Negative effective extraction; net beneficiary. Algorithm licensing firm: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Negative effective extraction; monetizes coordination. Workforce coalition: Mixed (beneficiary from route safety improvements + victim of speed-up) + constrained → d≈0.65, f(d)≈0.98. Moderate extraction but with organized agency to negotiate. Regulatory movement: Organized + constrained → d≈0.45, f(d)≈0.48. Low effective extraction; coalition has agency and sees viable path forward (transparency mandates, open-source adoption). Academic establishment: Institutional + arbitrage → d≈0.12, f(d)≈-0.06. Piton classification comes from theater_ratio ≥ 0.70 and inertial maintenance, not from high chi. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (observer naturalizes constraint); the engine's false summit detector catches this.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint satisfies all three tangled rope gates: (1) requires_active_enforcement=true — platforms actively enforce route compliance, algorithmic opacity, and pricing constraints; (2) beneficiaries present — logistics platforms, shippers benefit from optimization; (3) victims present — delivery drivers, independent carriers bear costs of optimization and extraction. The mandatrophy resolves because the constraint exhibits GENUINE COORDINATION FUNCTION (TSP actually does reduce logistics costs and improve service) alongside ASYMMETRIC EXTRACTION (platforms capture disproportionate gains, workers bear intensity costs without transparent benefit sharing). This is not a Snare masquerading as coordination or a Rope masquerading as extraction. Both functions are real. The solution is not to eliminate TSP application (which would eliminate coordination benefits) but to redistribute extraction through transparency mandates, algorithmic auditing, and worker participation in route design. The workforce coalition and regulatory movement perspectives show that the constraint's extraction mechanism is contingent—not inherent to the optimization problem itself, but to how platforms choose to implement and monetize it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_sufficiency,
    'Does algorithmic transparency (opening optimization criteria, cost functions, trade-off parameters) fundamentally change the extraction mechanism, or do drivers/workers remain trapped by economic necessity despite understanding the algorithm?',
    'Pilot programs in unionized logistics; measurement of worker autonomy and exit rates post-transparency; comparison of working conditions under transparent vs opaque algorithms',
    'If transparency enables exit/negotiation: constraint downgrades from Snare to Tangled Rope. If transparency is performative: constraint remains Snare (workers understand their trap but cannot escape it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether algorithmic transparency reduces extraction or remains performative').

omega_variable(
    open_source_routing_viability,
    'Can open-source TSP solvers (e.g., VROOM, OR-Tools) actually compete with proprietary platform algorithms in real-world logistics, or do network effects and training data advantages lock in proprietary solutions?',
    'Comparative deployment analysis: open-source routing quality vs proprietary; cost of adoption and maintenance; market share trends',
    'If open-source is viable: scaffold perspective confirmed—sunset pathway exists. If proprietary lock-in persists: scaffold is aspirational, constraint persists as Tangled Rope or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_routing_viability, empirical, 'Whether open-source routing solutions can break proprietary lock-in').

omega_variable(
    extraction_vs_efficiency_decomposition,
    'How much of the measurable wage suppression / driver strain in optimized logistics is inherent to the efficiency gains (unavoidable cost of better routing) vs how much is rent extraction by platforms (avoidable, contingent institutional choice)?',
    'Comparison of wage/working-condition trends in unionized vs non-unionized logistics; analysis of platform profit margins; counterfactual modeling of wage-sharing scenarios with same algorithmic efficiency',
    'If decomposition shows 70%+ efficiency / 30% extraction: constraint is closer to Rope (mixed benefit). If 30% efficiency / 70% extraction: constraint is closer to Snare (pure predation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_efficiency_decomposition, empirical, 'Decomposition of wage suppression into efficiency-driven vs rent-extraction components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(traveling_salesperson_problem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsp_tr_t0, traveling_salesperson_problem, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tsp_tr_t5, traveling_salesperson_problem, theater_ratio, 5, 0.27).
narrative_ontology:measurement(tsp_tr_t10, traveling_salesperson_problem, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(tsp_be_t0, traveling_salesperson_problem, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(tsp_be_t5, traveling_salesperson_problem, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(tsp_be_t10, traveling_salesperson_problem, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(traveling_salesperson_problem, resource_allocation).
narrative_ontology:affects_constraint(traveling_salesperson_problem, last_mile_delivery_labor).
narrative_ontology:affects_constraint(traveling_salesperson_problem, algorithmic_wage_setting).
narrative_ontology:affects_constraint(traveling_salesperson_problem, platform_information_asymmetry).

% DUAL FORMULATION NOTE:
% The TSP application constraint (this story) describes the structural effects of deploying TSP-derived algorithms in real logistics. It is upstream of platform-specific constraints (last_mile_delivery_labor, algorithmic_wage_setting) that model sector-specific labor extraction. The TSP constraint's ε=0.38 reflects the mixed coordination-extraction at the algorithmic level; downstream constraints model how specific industries (gig delivery, parcel logistics) apply these algorithms with higher extraction (ε≥0.50).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(traveling_salesperson_problem, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
