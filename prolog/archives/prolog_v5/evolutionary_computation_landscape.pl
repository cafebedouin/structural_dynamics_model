% ============================================================================
% CONSTRAINT STORY: evolutionary_computation_landscape
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolutionary_computation_landscape, []).

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
 *   constraint_id: evolutionary_computation_landscape
 *   human_readable: Evolutionary Computation Landscape Constraint
 *   domain: computational_theory/optimization/artificial_intelligence
 *
 * SUMMARY:
 *   The evolutionary computation landscape constraint encodes a structural
 *   tension between the need for standardized evaluation (enabling
 *   reproducibility and cumulative comparison) and the pressure toward
 *   innovation lock-in created by benchmark-centric research incentives.
 *   Canonical test problems (CEC benchmarks, BBOB functions) serve legitimate
 *   coordination functions but simultaneously suppress alternative
 *   algorithmic approaches by making published comparison difficult for
 *   researchers outside dominant paradigms. Early evolutionary algorithms
 *   (genetic algorithms, evolution strategies) benefited from benchmark
 *   standardization as validation infrastructure. Over time, as research
 *   matured, the benchmarks shifted from enabling-infrastructure to
 *   constraining-lock-in: publications require performance on canonical
 *   problems; funding agencies request improvements on standard metrics; new
 *   algorithm proposals must prove superiority on established test suites.
 *   This creates a ratchet mechanism: each new dominant algorithm becomes
 *   embedded in benchmarks, making it harder for alternative approaches to
 *   gain visibility. The constraint exhibits genuine coordination function
 *   (benchmarks do enable comparison) layered with asymmetric extraction (the
 *   choice of which benchmarks to canonicalize concentrates research
 *   attention on certain problem classes and away from others). Theater ratio
 *   (0.65) reflects that much published evolutionary computation work
 *   optimizes for benchmark performance rather than addressing real problem
 *   structure; researchers invest effort in micro-optimizations on canonical
 *   problems with known solutions rather than exploring genuinely novel
 *   algorithmic principles or applying algorithms to diverse real-world
 *   problems.
 *
 * KEY AGENTS:
 *   - Dominant Algorithm Developers: Primary beneficiary (institutional/arbitrage) — canonical benchmarks enable citation accumulation and paradigm gravity. Easy exit: if new paradigm emerges, can switch or incorporate into dominant framework.
 *   - Alternative Algorithm Researchers: Primary victim (powerless/trapped) — cannot publish results without canonical benchmarks; cannot access benchmarks without conforming to dominant evaluation frame. Trapped: lack institutional affiliation, funding, computational resources.
 *   - Benchmark Publishing Institutions: Secondary beneficiary (institutional/arbitrage) — CEC competitions, BBOB standardization committees control which problems are canonical. Authority and resource concentration.
 *   - Field Exploration Capacity: Primary victim (analytical/trapped) — abstract collective good. The fraction of computational resources devoted to exploring novel algorithmic space diminishes as benchmark optimization consumes research time and funding.
 *   - Incremental Improvers: Moderate agent (moderate/constrained) — can work within dominant frameworks, benefit from infrastructure and citations, but constrained to incremental innovation within established paradigms.
 *   - Open Algorithm Initiative: Organized agents (organized/constrained) — AutoML and algorithm portfolio systems create alternative evaluation pathways (problem-adaptive selection) that bypass benchmark-centric evaluation. Have exit path via problem-aware optimization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolutionary_computation_landscape, 0.38).
domain_priors:suppression_score(evolutionary_computation_landscape, 0.48).
domain_priors:theater_ratio(evolutionary_computation_landscape, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolutionary_computation_landscape, extractiveness, 0.38).
narrative_ontology:constraint_metric(evolutionary_computation_landscape, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(evolutionary_computation_landscape, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolutionary_computation_landscape, tangled_rope).
narrative_ontology:human_readable(evolutionary_computation_landscape, "Evolutionary Computation Landscape Constraint").
narrative_ontology:topic_domain(evolutionary_computation_landscape, "computational_theory/optimization/artificial_intelligence").

domain_priors:requires_active_enforcement(evolutionary_computation_landscape).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolutionary_computation_landscape, dominant_algorithm_developers).
narrative_ontology:constraint_beneficiary(evolutionary_computation_landscape, benchmark_publishing_institutions).
narrative_ontology:constraint_victim(evolutionary_computation_landscape, alternative_algorithm_researchers).
narrative_ontology:constraint_victim(evolutionary_computation_landscape, field_exploration_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE ALGORITHM RESEARCHER (SNARE) — Early-career researchers proposing novel evolutionary approaches face publication barriers, benchmark discrimination, and funding scarcity. Trapped within the dominant paradigm's infrastructure; cannot access computational resources or benchmark datasets without conforming to established metrics. Maximum extraction: their ideas are filtered by gatekeepers controlling evaluation standards.
constraint_indexing:constraint_classification(evolutionary_computation_landscape, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCREMENTAL IMPROVISER (TANGLED ROPE) — Mid-career researchers working within dominant frameworks (genetic algorithms, particle swarm optimization) benefit from established benchmarks, citation networks, and computational infrastructure. Also constrained by the same benchmarks that define success; must optimize for measurable performance on canonical problems rather than exploring genuinely novel design spaces. Mixed experience: genuine coordination benefits (shared evaluation frameworks) layered with asymmetric extraction (innovation channeled toward dominant paradigms).
constraint_indexing:constraint_classification(evolutionary_computation_landscape, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT FRAMEWORK DEVELOPER (ROPE) — Institutions and research groups that pioneered mainstream evolutionary algorithms (genetic algorithms, differential evolution, particle swarm optimization) benefit from the canonical benchmark ecosystem. Experiences the constraint as pure coordination: standardized benchmarks enable reproducibility and cumulative research. Net beneficiary — the framework accumulates citations and research gravity around its design choices. Low extraction experienced because exit is frictionless; they can shift to new paradigms if advantageous.
constraint_indexing:constraint_classification(evolutionary_computation_landscape, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CANONICAL BENCHMARK SUITE (PITON) — Standard test problems (CEC competition benchmarks, BBOB functions, traveling salesman problem variants) persist despite acknowledged limitations. Researchers continue evaluating against them not because they represent important real-world problems but because universality of comparison requires universality of targets. Theater ratio high: much research energy goes into micro-optimizations on arbitrary canonical problems rather than addressing problem structure diversity. The benchmark suite has atrophied from its original function (measuring genuine algorithmic progress) into a maintenance ritual.
constraint_indexing:constraint_classification(evolutionary_computation_landscape, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN ALGORITHM INITIATIVE (SCAFFOLD) — Organized efforts (AutoML frameworks, algorithm portfolios, Bayesian optimization services) create alternative evaluation pathways by decoupling algorithm selection from human-specified benchmarks. These systems automatically match algorithms to problem structure, reducing dependence on canonical test suites. Sunset logic: as problem-aware algorithm selection matures, the need to defend performance on fixed benchmarks diminishes. Low extraction because these organized agents have agency and a clear exit path (problem-adaptive evaluation replacing benchmark-centric evaluation).
constraint_indexing:constraint_classification(evolutionary_computation_landscape, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NO FREE LUNCH VIEW (MOUNTAIN) — From the civilizational analytical perspective, some canonical test suite is mathematically inevitable: without fixed evaluation criteria, comparison is meaningless. The No Free Lunch theorem (all algorithms equivalent averaged over all problems) creates an apparent logical necessity for standardized benchmarks. However, the structural data reveals this as false summit: the constraint is not the mathematical necessity of comparison but the institutional lock-in to specific canonical problems with specific metrics. Problem-adaptive evaluation is theoretically coherent and practically viable.
constraint_indexing:constraint_classification(evolutionary_computation_landscape, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_computation_landscape_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolutionary_computation_landscape, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_computation_landscape, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolutionary_computation_landscape, TR),
    TR >= 0.70.

:- end_tests(evolutionary_computation_landscape_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does extract from alternative researchers and from the field's exploration capacity, but the extraction is not severe as snare-level constraints because: (1) alternative algorithms can still be published in specialized venues, (2) some researchers do pursue novel approaches despite benchmark bias, (3) the coordination function of benchmarks is genuine and valuable. The value reflects a real but limited extraction. Suppression (0.48): Moderate-high. Significant barriers include: publication bias toward canonical benchmarks (most journals require standard comparisons), funding scarcity for alternative approaches, computational resource concentration, and institutional prestige tied to canonical benchmark leaderboards. However, suppression is not total — open-source implementations, pre-prints, and specialist conferences provide partial alternatives. Theater ratio (0.65): Moderate-high. A significant fraction of published work involves minor variations on canonical benchmark optimization rather than addressing problem-class diversity or real-world application. Researchers invest effort in tuning parameters for CEC competition functions that have known solutions and known problem structure, rather than exploring why certain algorithm classes work on certain problem structures or applying algorithms to diverse industrial/scientific problems with unknown structure. The theater has increased over time as benchmarks have become more established and expectations for canonical comparison more standardized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates six distinct classifications from a single structural reality. The alternative researcher sees a snare (trapped, no exit, high extraction). The incremental improviser sees tangled rope (genuine coordination function of benchmarks, but also genuine constraint on innovation direction). The dominant developer sees rope (benchmarks enable reproducibility and cumulative work; low experienced extraction). The benchmark suite itself appears as piton from civilizational perspective (performative function, atrophied from original enabling role into maintenance ritual). The open algorithm initiative sees scaffold (alternative evaluation pathways creating a sunset trajectory for benchmark-centric research). The analytical observer risks false summit (mathematical necessity of comparison) but the structural data reveals contingency (problem-adaptive evaluation is theoretically coherent). The perspectival gap exposes how 'standard' infrastructure can function as both enabling (for agents aligned with the standard) and constraining (for agents proposing alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (dominant algorithm developers, benchmark institutions) have arbitrage-level exit — they can abandon the constraint if it becomes disadvantageous and retain institutional standing. Their directionality d is low (0.05-0.15), yielding negative f(d) and low or negative experienced extraction. Victims (alternative researchers) have trapped exit — no computational resources, no publication venues outside canonicalized benchmarks, institutional barriers to accessing competition infrastructure. Their directionality d is high (0.90+), yielding f(d) ≈ 1.3-1.4 and maximum experienced extraction. The institutional victim (field exploration capacity) is collective and powerless, d ≈ 0.95. Incremental improvers have constrained exit — can publish, can access resources, but at cost of constraining innovation to within-paradigm space. Their d is moderate (0.50-0.60). Open algorithm initiatives have organized status and constrained exit but also agency and exit paths; their d is moderate (0.40-0.55) but reflects that they are building alternatives rather than trapped within the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that benchmark infrastructure is simultaneously coordination mechanism and extraction mechanism, and that which aspects dominate depends entirely on the agent's structural position. Dominant developers genuinely experience benchmarks as enabling coordination (Rope). Alternative researchers genuinely experience benchmarks as extraction mechanism (Snare). Both experiences are structural, not misperceptions. The constraint is not 'really' one type viewed from multiple angles — it is genuinely different constraint-experiences for different agents because the mechanisms benefiting one class (benchmark standardization for validation) are the same mechanisms trapping another class (benchmark standardization prevents alternative approaches from being comparable). The analytical observer's false summit (No Free Lunch mathematical necessity) reveals the naturalization move: claiming that benchmarks are inevitable because comparison requires criteria obscures the institutional choice of which criteria to canonicalize. Problem-adaptive evaluation is theoretically viable and practically emerging; the 'inevitability' of current benchmarks is institutional, not mathematical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    benchmark_diversity_threshold,
    'How much benchmark diversity is sufficient to prevent benchmark-driven design bias without losing comparability?',
    'Meta-analysis of algorithm performance correlation across diverse benchmark families; measurement of generalization from canonical to real-world problems',
    'If threshold is low (few diverse benchmarks): modest extraction, most agents can work around canonical bias. If threshold is high (many diverse benchmarks): severe extraction, resources exhausted on benchmark compliance without real innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_diversity_threshold, empirical, 'Sufficient diversity in benchmarks to prevent design bias').

omega_variable(
    problem_structure_representation,
    'Do canonical benchmark suites represent the distribution of real-world optimization problems?',
    'Comparative analysis of problem characteristics (modality, separability, dimensionality distribution) in canonical suites vs industrial/scientific application portfolios',
    'If canonical suites are representative: benchmark constraint is coordination mechanism (more Rope-like across perspectives). If canonical suites are skewed: benchmark constraint is extraction mechanism concentrating innovation on artificial problems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(problem_structure_representation, empirical, 'Whether benchmarks match real-world problem distribution').

omega_variable(
    algorithm_landscape_exploration_cost,
    'What fraction of evolutionary computation research effort goes toward exploring fundamentally novel algorithmic principles vs optimizing within established paradigms?',
    'Citation analysis and research direction classification; survey of researchers on constraint perceived from their work',
    'If exploration fraction is low (<20%): constraint is high-extraction Snare for alternative researchers. If exploration fraction is high (>40%): constraint is lower-extraction Tangled Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_landscape_exploration_cost, empirical, 'Proportion of research directed toward algorithmic exploration').

omega_variable(
    computational_resource_concentration,
    'Are computational resources for evolutionary algorithm research concentrated in dominant-paradigm research groups?',
    'Analysis of funding distribution, GPU/cluster allocation to research groups by algorithmic focus; measurement of computation costs for canonical benchmark runs',
    'If concentration is high: suppression mechanism is resource scarcity (trapped exit for alternative researchers). If distributed: suppression mechanism is primarily institutional bias rather than resource constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_resource_concentration, empirical, 'Resource concentration in dominant algorithm research groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolutionary_computation_landscape, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evolc_tr_t0, evolutionary_computation_landscape, theater_ratio, 0, 0.42).
narrative_ontology:measurement(evolc_tr_t5, evolutionary_computation_landscape, theater_ratio, 5, 0.55).
narrative_ontology:measurement(evolc_tr_t10, evolutionary_computation_landscape, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(evolc_be_t0, evolutionary_computation_landscape, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(evolc_be_t5, evolutionary_computation_landscape, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(evolc_be_t10, evolutionary_computation_landscape, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolutionary_computation_landscape, information_standard).
narrative_ontology:affects_constraint(evolutionary_computation_landscape, hyperparameter_optimization_lock).
narrative_ontology:affects_constraint(evolutionary_computation_landscape, algorithmic_monoculture_risk).
narrative_ontology:affects_constraint(evolutionary_computation_landscape, applied_problem_algorithm_mismatch).

% DUAL FORMULATION NOTE:
% The evolutionary computation landscape constraint decomposes into separable concerns: (1) benchmark selection bias (which problems are canonical) constrains exploration of alternative algorithms; (2) performance metrics (which objective functions matter) favor certain algorithm classes over others; (3) computational resource allocation concentrates on dominant paradigms. Each has different ε and different extraction mechanisms. The current story focuses on benchmark-driven constraint; see companion stories for metric bias and resource concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolutionary_computation_landscape, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
