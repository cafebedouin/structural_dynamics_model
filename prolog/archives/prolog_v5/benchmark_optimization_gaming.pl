% ============================================================================
% CONSTRAINT STORY: benchmark_optimization_gaming
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_benchmark_optimization_gaming, []).

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
 *   constraint_id: benchmark_optimization_gaming
 *   human_readable: Benchmark Optimization Gaming in Performance Evaluation Systems
 *   domain: organizational_systems/performance_measurement
 *
 * SUMMARY:
 *   Benchmark optimization gaming describes the structural dynamic in which
 *   performance evaluation systems become targets for manipulation rather
 *   than measures of genuine capability. When organizations, individuals, or
 *   systems face evaluation against numerical benchmarks, incentive
 *   structures often reward metric improvement over the underlying
 *   performance the metric was designed to capture. This constraint exhibits
 *   a tangled-rope structure: the benchmark system provides genuine
 *   coordination value (clarity, comparability, measurable goals) while
 *   simultaneously enabling extraction (selective optimization, metric
 *   arbitrage, gaming that divorces reported performance from actual
 *   capability). The extractiveness trajectory shows accumulation over time
 *   (0.35 → 0.62): as benchmarks become more consequential (higher stakes),
 *   gaming incentives intensify and the theater ratio rises (0.42 → 0.75),
 *   indicating increasing divergence between the performative act of
 *   benchmark reporting and the functional capability the benchmark claims to
 *   measure. This is not a pathology of measurement systems specifically but
 *   a general property of incentivized evaluation: when observable metrics
 *   become targets, strategic actors optimize for metrics rather than
 *   underlying goals. Goodhart's Law formalizes the principle: 'Any metric
 *   sufficiently optimized ceases to measure what it was intended to
 *   measure.' The constraint emerges across institutional contexts: academic
 *   rankings and grade inflation, medical quality metrics and risk selection,
 *   economic growth measurements and externality accounting, software
 *   benchmarks and compiler-specific optimization, standardized testing and
 *   teaching-to-the-test. The structure is invariant: (1) genuine
 *   coordination problem requiring measurable evaluation, (2) metric
 *   selection that captures part but not all of genuine performance, (3)
 *   misaligned incentives creating pressure to optimize metrics, (4)
 *   strategic actor response that improves metric without corresponding
 *   capability improvement, (5) degradation of both the metric and the
 *   underlying performance as resources shift toward gaming.
 *
 * KEY AGENTS:
 *   - System Integrity (Powerless/Trapped): Abstract collective good — genuine performance, unmeasured capabilities, resource efficiency. Bears cost of metric corruption with no agency to exit or organize.
 *   - Resource-Constrained Actors (Moderate/Constrained): Organizations, individuals, or teams facing benchmark pressure with limited resources. High barriers to exit; feel forced to game metrics or fall behind. Also benefit from coordinating function of benchmarks.
 *   - Benchmark-Setting Authority (Institutional/Arbitrage): Metric designers, ranking organizations, regulatory bodies. Benefit from coordination function; can modify benchmarks, shift to alternatives, or withdraw without personal cost. Net beneficiaries.
 *   - Well-Resourced Optimizers (Powerful/Mobile): Actors with capacity to hire optimization specialists, acquire gaming tools, engineer metric performance. Mobile exit options but strong incentive to stay (competitive advantage from superior optimization). Experience extraction but also benefit.
 *   - Legacy Measurement Institution (Institutional/Constrained): Academic ranking systems, standardized testing bodies, performance review departments. Maintain benchmarks through inertia despite known Goodhart drift. Constrained by institutional commitment to the regime.
 *   - Measurement Reform Movement (Organized/Constrained): Coalitions advocating alternative metrics (multi-metric systems, causal inference, behavioral measurement, continuous feedback). Building parallel pathways with sunset logic: expect traditional benchmarks to be replaced as alternatives mature.
 *   - Analytical Observer (Analytical/Analytical): Civilizational perspective that risks naturalizing benchmark gaming as an immutable law of measurement rather than a contingent institutional arrangement enabled by specific design choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(benchmark_optimization_gaming, 0.58).
domain_priors:suppression_score(benchmark_optimization_gaming, 0.62).
domain_priors:theater_ratio(benchmark_optimization_gaming, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(benchmark_optimization_gaming, extractiveness, 0.58).
narrative_ontology:constraint_metric(benchmark_optimization_gaming, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(benchmark_optimization_gaming, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(benchmark_optimization_gaming, tangled_rope).
narrative_ontology:human_readable(benchmark_optimization_gaming, "Benchmark Optimization Gaming in Performance Evaluation Systems").
narrative_ontology:topic_domain(benchmark_optimization_gaming, "organizational_systems/performance_measurement").

domain_priors:requires_active_enforcement(benchmark_optimization_gaming).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(benchmark_optimization_gaming, evaluated_actor).
narrative_ontology:constraint_beneficiary(benchmark_optimization_gaming, benchmark_setter).
narrative_ontology:constraint_victim(benchmark_optimization_gaming, system_integrity).
narrative_ontology:constraint_victim(benchmark_optimization_gaming, genuine_performance).
narrative_ontology:constraint_victim(benchmark_optimization_gaming, resource_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM INTEGRITY (SNARE) — Abstract collective good cannot organize or advocate. Bears full cost of metric corruption and optimized-away capabilities. No exit option from degraded measurement systems. Maximum experienced extraction with zero agency.
constraint_indexing:constraint_classification(benchmark_optimization_gaming, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESOURCE-CONSTRAINED ACTORS (TANGLED ROPE) — High barriers to exit the benchmark game: career advancement, funding, or employment contingent on benchmark scores. Face pressure to optimize metrics rather than genuine performance. But also benefit from visible performance signaling and potential coordination gains if benchmarks genuinely measured capability. Extraction is real but bounded by partial coordination value.
constraint_indexing:constraint_classification(benchmark_optimization_gaming, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BENCHMARK SETTER (ROPE) — Benefits from coordination function: clear metrics enable comparison, resource allocation, and performance communication. Experiences the constraint as pure coordination with minimal extraction costs. Authority can modify benchmarks, exit the regime, or shift to alternative metrics without cost. Net beneficiary.
constraint_indexing:constraint_classification(benchmark_optimization_gaming, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WELL-RESOURCED OPTIMIZERS (TANGLED ROPE) — Powerful actors with resources to hire specialists, acquire tools, and engineer benchmark performance. Can also invest in genuine improvement. Experience extraction (pressure to optimize) but also benefit from benchmark setting mechanism. Have mobile exit options but strong incentives to stay (competitive advantage from optimization capability). Moderate extraction, high agency.
constraint_indexing:constraint_classification(benchmark_optimization_gaming, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY MEASUREMENT INSTITUTION (PITON) — Institutional systems (standardized testing, academic rankings, performance reviews) maintain benchmark rituals through inertia. Theater ratio high: measurement systems persist despite known Goodhart drift and metric corruption. Institutions see their own measurement regime as degraded but continue enforcement. Theater-driven classification reflects the shift from genuine coordination to performative compliance.
constraint_indexing:constraint_classification(benchmark_optimization_gaming, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MEASUREMENT REFORM MOVEMENT (SCAFFOLD) — Organized efforts (multi-metric systems, outcome tracking, capability assessment) create temporary parallel measurement pathways. See benchmark optimization as a solvable coordination failure with a sunset: better metrics (causal inference, behavioral measurement, continuous feedback) will replace single-number benchmarks. Organized agents have constrained but non-zero exit options through alternative measurement schemes. Scaffold classification reflects temporary nature of traditional benchmark regime.
constraint_indexing:constraint_classification(benchmark_optimization_gaming, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Goodhart's Law appears as an immutable principle: 'Any metric sufficiently optimized ceases to measure what it was intended to measure.' This suggests benchmark gaming is not a contingent institutional failure but a structural feature of measurement systems. However, this naturalization masks the contingent institutional choices (single-metric emphasis, misaligned incentives, suppressed alternative data) that enable the gaming. False summit indicator: Goodhart's Law describes a real mechanism but doesn't necessitate the suppression of alternatives or the institutional commitment to compromised metrics.
constraint_indexing:constraint_classification(benchmark_optimization_gaming, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(benchmark_optimization_gaming_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(benchmark_optimization_gaming, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(benchmark_optimization_gaming, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(benchmark_optimization_gaming, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(benchmark_optimization_gaming, TR),
    TR >= 0.70.

:- end_tests(benchmark_optimization_gaming_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and accumulating. The baseline extractiveness (0.35 at t=0) reflects that benchmarks do provide genuine coordination value — clear metrics, comparable performance, visible targets. As benchmarks become more consequential and stakes increase, extractive pressure intensifies. By t=20-30, extractiveness reaches 0.58-0.62, reflecting that strategic optimization has become the dominant driver of benchmark improvement, with actual underlying performance improvement lagging. The accumulation trajectory is diagnostic: if benchmarks genuinely measured performance, extractiveness should be stable; the increasing extractiveness over time indicates metric corruption and resource diversion. Suppression (0.62): Moderate-high. Barriers to exiting benchmark optimization include career consequences (grades, rankings, employment decisions contingent on metrics), competitive disadvantage (peers who game metrics advance), and institutional dependence (resource allocation based on benchmark scores). But suppression is not total — some actors maintain genuine performance investment, and alternative measurement systems exist. Theater ratio (0.68): High and increasing. The trajectory shows how measurement theater accumulates: as gaming becomes more sophisticated, the performative gap widens (0.42 → 0.68). Actors spend increasing effort on metric optimization (visible, rewards) vs genuine capability improvement (invisible, no immediate reward). The theater itself becomes a form of extraction: resources that could improve actual performance are redirected to optimizing reported performance.
 *
 * PERSPECTIVAL GAP:
 *   The original beneficiary (benchmark setter) sees pure coordination: metrics enable evaluation, comparison, and performance improvement. This is true — benchmarks do solve a genuine coordination problem (how to measure performance comparably). The moderate actor (resource-constrained) sees tangled rope: the benchmark system coordinates but also compels gaming, creating extraction pressure alongside coordination value. This is also true — benchmarks enable and incentivize strategic metric optimization. The powerless agent (system integrity, genuine performance) sees snare: benchmarks exist but are systematically degraded by gaming, providing no coordination value for this agent and imposing costs (misdirected resources, obscured real performance). This is structurally true — abstract collective goods cannot exit or organize, and gaming directly impairs them. The analytical observer at civilizational scope risks seeing mountain (Goodhart's Law as immutable): measurement gaming appears as an inevitable law of any incentivized system. But this naturalizes what are contingent institutional choices: (1) relying on single metrics rather than multidimensional assessment, (2) misaligning incentives so metrics and actual performance diverge, (3) suppressing alternative measurement systems, (4) enforcing high-stakes consequences for metrics. Each choice is changeable; the convergence of all four creates the snare structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reveals why gaming persists despite universal recognition of its costs. Beneficiaries (benchmark setters) experience the system as rope, seeing no extraction cost for them — they capture coordination value (clarity, comparison) without bearing optimization burden. The constraint's enforcement benefits them directly and costs them nothing, so they have no incentive to reform. Moderate actors (resource-constrained) experience tangled rope: they benefit from benchmark clarity but also bear gaming pressure. But because moderate actors are individually weak, they cannot coordinate to demand metric reform. Powerful optimizers experience tangled rope with benefits: they can exploit gaming mechanics for competitive advantage, making them unlikely to support alternatives. The powerless collective (genuine performance) cannot organize or exit, so their opposition never becomes externally visible. This is a classic Olsonian collective action problem: benefits of gaming are concentrated (individual optimizer, benchmark authority), costs are diffuse (systemic integrity, resource efficiency). The organized reform movement sees the problem clearly but lacks the institutional power to shift the regime without external crisis (widespread metric failure, regulatory intervention, or alternative technology maturation).
 *
 * MANDATROPHY ANALYSIS:
 *   Benchmark optimization gaming illustrates mandatrophy resolution through perspectival diversity. The claim 'benchmark gaming is inevitable' (natural law) is rejected by comparing the powerless/trapped perspective (snare: maximum extraction, no agency) with the institutional/arbitrage perspective (rope: pure coordination, full agency). If the same constraint classified as both mountain and rope from different perspectives, the system is not immutable — the institutional perspective sees mutability because they have agency; the powerless perspective sees fixity because they have none. The mandatrophy is resolved by recognizing that 'inevitability' is observer-relative: inevitable for powerless agents, contingent for institutional actors. The Tangled Rope classification at moderate/constrained power captures the core hybrid: benchmarks genuinely coordinate (rope component) while simultaneously enabling extraction (snare component). This hybrid is not a contradiction but a structural fact — the same metric can coordinate performance comparison while enabling gaming. The scaffold perspective (reform movement, generational time, global scope) provides the exit path: alternative measurement systems (multi-metric assessment, continuous behavioral feedback, causal inference-based measurement) can replace benchmark consolidation if designed to lower Goodhart drift. The sunset is not automatic but structural: as alternatives mature, the gaming mechanism loses force because optimizing a single metric no longer provides competitive advantage when performance is measured multidimensionally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gaming_vs_goodhart_distinction,
    'Is benchmark gaming an intentional manipulation strategy or an inevitable consequence of metric selection itself (Goodhart''s Law)?',
    'Evidence analysis: actor intent (explicit optimization for metrics), comparative system design (systems with low Goodhart drift vs high), and timeline of metric divergence',
    'If intentional gaming: snare classification strengthened; extraction is deliberate and suppressible. If Goodhart inevitability: mountain partially justified; but contingent institutional choices (metric selection, incentive alignment) remain changeable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gaming_vs_goodhart_distinction, conceptual, 'Whether gaming is intentional manipulation or inevitable metric property').

omega_variable(
    alternative_metric_feasibility,
    'Are alternative measurement systems (multi-metric, continuous feedback, capability assessment) genuinely feasible at scale, or does benchmark consolidation re-emerge under different labels?',
    'Longitudinal case studies of organizations that shifted to alternative measurement systems; tracking whether new metrics show similar Goodhart drift over time',
    'If feasible: scaffold sunset is real, measurement reform has genuine agency. If metrics converge: the gaming mechanism is structural, suppression mechanisms are stronger than alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_metric_feasibility, empirical, 'Whether alternative measurement systems can sustain without Goodhart drift').

omega_variable(
    suppression_mechanism_origin,
    'Is suppression of alternative metrics driven by institutional inertia (easy defaults), incentive misalignment (gatekeepers benefit from single-metric clarity), or genuine coordination problems (multiple metrics more complex)?',
    'Institutional analysis of metric adoption decisions; tracking who benefits from single-metric regimes; historical comparison of transition costs',
    'If inertia: suppression is reducing and alternatives will emerge. If incentive misalignment: suppression is actively maintained and will persist absent external pressure. If coordination: suppression reflects genuine coordination cost that alternatives must overcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_origin, empirical, 'Whether suppression is inertia, incentive-driven, or coordination-based').

omega_variable(
    extractive_intent_detectability,
    'Can the analyst distinguish between benign metric optimization (genuine performance improvement correlated with metric) and extractive gaming (metric improvement without underlying capability improvement)?',
    'Behavioral outcomes tracking: do benchmark improvements predict downstream capability gains? Time-series correlation between metric optimization and real-world performance',
    'If distinguishable: gaming is observable and suppressible; snare classification holds. If indistinguishable: the constraint becomes a mountain (unknowable extraction) or rope (coordination mixed with inevitable measurement noise).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractive_intent_detectability, empirical, 'Distinguishability of genuine improvement from extractive gaming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(benchmark_optimization_gaming, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(benchopt_tr_t0, benchmark_optimization_gaming, theater_ratio, 0, 0.42).
narrative_ontology:measurement(benchopt_tr_t10, benchmark_optimization_gaming, theater_ratio, 10, 0.55).
narrative_ontology:measurement(benchopt_tr_t20, benchmark_optimization_gaming, theater_ratio, 20, 0.68).
narrative_ontology:measurement(benchopt_tr_t30, benchmark_optimization_gaming, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(benchopt_be_t0, benchmark_optimization_gaming, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(benchopt_be_t10, benchmark_optimization_gaming, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(benchopt_be_t20, benchmark_optimization_gaming, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(benchopt_be_t30, benchmark_optimization_gaming, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(benchmark_optimization_gaming, information_standard).
narrative_ontology:boltzmann_floor_override(benchmark_optimization_gaming, 0.08).
narrative_ontology:affects_constraint(benchmark_optimization_gaming, goodharts_law_metric_corruption).
narrative_ontology:affects_constraint(benchmark_optimization_gaming, academic_grade_inflation).
narrative_ontology:affects_constraint(benchmark_optimization_gaming, medical_risk_selection_gaming).
narrative_ontology:affects_constraint(benchmark_optimization_gaming, gdp_externality_gaming).

% DUAL FORMULATION NOTE:
% Benchmark optimization gaming is upstream of specific institutional manifestations (grade inflation, risk selection, GDP accounting) that each have their own extractiveness values reflecting domain-specific constraints. This story captures the general structural mechanism; downstream stories model domain-specific instantiations of the gaming mechanism with different beneficiaries, victims, and institutional configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(benchmark_optimization_gaming, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
