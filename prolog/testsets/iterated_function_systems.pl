% ============================================================================
% CONSTRAINT STORY: iterated_function_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iterated_function_systems, []).

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
 *   constraint_id: iterated_function_systems
 *   human_readable: IFS Convergence and Computational Resources
 *   domain: technological/computational_mathematics
 *
 * SUMMARY:
 *   Iterated Function Systems (IFS) provide a mathematically elegant method
 *   for generating fractals by repeatedly applying a set of affine
 *   transformations. The constraint examined here is not mathematical (IFS
 *   theory is stable) but structural: the computational resources required to
 *   achieve convergence for meaningful visualization and analysis have
 *   created a resource allocation asymmetry that affects different scientific
 *   communities differently. High-performance computing centers and
 *   specialized hardware manufacturers benefit from the resource
 *   concentration, while small research groups and educational institutions
 *   bear the extraction costs of limited access and extended computation
 *   times. The constraint exhibits both coordination functions (standardized
 *   benchmarks, algorithm publication, resource-sharing protocols) and
 *   genuine asymmetric extraction (differential access to computational
 *   capacity). The theater ratio has risen from pedagogical focus on IFS in
 *   1990s computer science to broader marginalisation in production graphics,
 *   yet the algorithm persists in curricula through institutional inertia.
 *   Open-source alternatives and GPU accessibility are creating scaffold
 *   pathways that may diminish extraction asymmetry over a generational
 *   timescale.
 *
 * KEY AGENTS:
 *   - High-Performance Computing Centers: Primary beneficiary (institutional/arbitrage) — consolidate computational resources, capture value from standardization, control resource allocation through queuing and pricing
 *   - Specialized Hardware Manufacturers: Primary beneficiary (institutional/arbitrage) — extract through vendor lock-in for optimized IFS convergence on proprietary platforms
 *   - Small Research Groups: Primary victim (moderate/constrained) — bear extraction through limited access, longer computation times, inability to leverage parallelization effectively
 *   - Undergraduate Researchers: Severe victim (powerless/trapped) — trapped within educational resource constraints, cannot access tools needed for meaningful experimentation; no exit options within institutional context
 *   - Open Source Community: Organized agent (organized/constrained) — building alternative pathways through FLAM, FractalExplorer, open-source GPU libraries; sees convergence bottleneck as temporary problem with clear sunset
 *   - Academic Computational Graphics: Institutional observer (institutional/arbitrage) — maintains IFS pedagogy through curriculum inertia despite reduced practical relevance; theater ratio high (0.68)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iterated_function_systems, 0.38).
domain_priors:suppression_score(iterated_function_systems, 0.48).
domain_priors:theater_ratio(iterated_function_systems, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iterated_function_systems, extractiveness, 0.38).
narrative_ontology:constraint_metric(iterated_function_systems, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(iterated_function_systems, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iterated_function_systems, tangled_rope).
narrative_ontology:human_readable(iterated_function_systems, "IFS Convergence and Computational Resources").
narrative_ontology:topic_domain(iterated_function_systems, "technological/computational_mathematics").

domain_priors:requires_active_enforcement(iterated_function_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iterated_function_systems, specialized_hardware_manufacturers).
narrative_ontology:constraint_beneficiary(iterated_function_systems, high_performance_computing_centers).
narrative_ontology:constraint_beneficiary(iterated_function_systems, applied_visualization_vendors).
narrative_ontology:constraint_victim(iterated_function_systems, small_research_groups).
narrative_ontology:constraint_victim(iterated_function_systems, undergraduate_education).
narrative_ontology:constraint_victim(iterated_function_systems, open_source_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERGRADUATE RESEARCHER (SNARE) — Trapped by institutional resource constraints and curriculum lock-in. Cannot access computational resources required for meaningful IFS convergence experiments. Bears the extraction of limited access to tools while watching commercial vendors dominate the space. No exit options within the educational timeline.
constraint_indexing:constraint_classification(iterated_function_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL RESEARCH GROUP (TANGLED ROPE) — Moderate power with constrained exit. Benefits from IFS convergence for visualization and modeling work, but extraction occurs through resource allocation asymmetry. Must accept longer convergence timelines and cannot leverage parallelization. Constrained by equipment depreciation cycles and grant funding volatility. Coordination function exists (shared algorithms, published benchmarks) but asymmetric cost distribution.
constraint_indexing:constraint_classification(iterated_function_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-PERFORMANCE COMPUTING CENTER (ROPE) — Institutional beneficiary with arbitrage options. Experiences IFS convergence as a pure coordination problem: resource allocation via queuing systems, standardized interfaces, documented convergence criteria. Can exit to alternative computation problems or service models. Benefits from computational resource consolidation and vendor contracts.
constraint_indexing:constraint_classification(iterated_function_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COMMUNITY (SCAFFOLD) — Organized agents building alternative convergence pathways (FLAM, FractalExplorer, Fractint) that reduce dependency on commercial hardware. Constrained by volunteer effort and fragmented resources, but sees clear sunset logic: GPU accessibility and algorithmic improvements are democratizing IFS convergence. Exit path exists through community software development and distributed computing models. Suppression declining as technology diffuses.
constraint_indexing:constraint_classification(iterated_function_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC COMPUTATIONAL GRAPHICS (PITON) — Institutional perspective on legacy IFS pedagogy in computer science curricula. Theater ratio high (0.68): teaching fractal generation persists in textbooks and courses as cultural artifact of 1990s computational aesthetics, but IFS has been largely replaced by procedural texture generation and procedural modeling in production graphics. Algorithms taught, tools rarely used in practice. Maintained through institutional inertia and canonical textbook presence, not functional dominance.
constraint_indexing:constraint_classification(iterated_function_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a computational complexity perspective, IFS convergence bottlenecks reflect fundamental limits: fractal dimension computation requires iterative refinement to arbitrary precision, creating inherent computational cost that cannot be eliminated by organizational changes. Convergence rate depends on contraction ratio of transformations — a mathematical property, not a resource allocation choice. This perspective sees the constraint as immutable natural law. Engine flagged for false summit detection.
constraint_indexing:constraint_classification(iterated_function_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iterated_function_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iterated_function_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iterated_function_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(iterated_function_systems, TR),
    TR >= 0.70.

:- end_tests(iterated_function_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The constraint exhibits moderate base extraction. The computational requirements for IFS convergence create genuine asymmetries in access and capability, but recent GPU acceleration has reduced the severity of the bottleneck. The declining extraction trajectory (0.52 → 0.38 over the interval) reflects algorithmic improvements and hardware democratization. Suppression (0.48): Moderate. Barriers to entry include capital equipment costs, technical knowledge requirements, and institutional access controls. However, suppression is not total — open-source tools exist, and cloud computing is reducing hardware barriers. Smaller groups can achieve meaningful results with extended computation times. Theater ratio (0.55): The educational presentation of IFS has become increasingly theatrical. While IFS exemplifies key computational concepts (iteration, self-similarity, dimension), it is rarely the primary tool in production graphics or applied research. Its persistence in curricula reflects pedagogical tradition more than operational utility.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence: beneficiaries see pure coordination (rope) through resource-sharing and standardized interfaces. Victims see extraction (snare for trapped undergraduates, tangled rope for constrained small groups). The open-source coalition sees a temporary problem with a sunset (scaffold) — GPU accessibility and algorithmic improvements are democratizing convergence. The academic graphics establishment sees a degraded ritual (piton) — IFS persists in teaching despite marginal production relevance. The civilizational analytical observer risks naturalizing a contingent institutional arrangement (resource concentration) as an immutable mathematical law (mountain). This perspectival range indicates a genuine hybrid constraint: both coordination functions and extraction mechanisms are operative.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality derives from institutional consolidation: HPC centers and hardware vendors benefit from resource concentration and can shift to alternative problems if IFS becomes less relevant (arbitrage exit). Their d values are low, producing negative χ (they experience the constraint as enabling rather than extractive). Victim directionality reflects trapped or constrained exit options: small research groups cannot easily migrate to alternative tools without significant rework (constrained exit, moderate d → moderate f(d) → moderate χ). Undergraduate researchers have no exit option within their institutional timeline (trapped exit, high d → high f(d) → high χ for the snare perspective). Open-source community agents have constrained exit (they cannot immediately replace institutional resources) but organized power and clear sunset logic, producing moderate d and reduced χ compared to isolated small groups.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID COORDINATION-EXTRACTION: The constraint resolves mandatrophy by identifying both genuine coordination (standardized algorithms, published benchmarks enabling resource-sharing) and genuine asymmetric extraction (hardware access concentration, vendor lock-in effects). Neither dominates. The beneficiaries experience coordination through resource consolidation; the victims experience extraction through access barriers. The open-science coalition's scaffold perspective confirms that the extraction mechanism is not structural (mathematical) but institutional (resource allocation) — as technology diffuses and algorithms improve, the asymmetry declines. The declining extractiveness trajectory (0.52 → 0.38) supports this interpretation: the constraint is eroding as alternatives mature, characteristic of tangled rope degrading toward rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convergence_sufficiency_threshold,
    'What convergence threshold (iteration count, Hausdorff distance, visual fidelity) constitutes ''sufficient'' IFS rendering for different application domains?',
    'Empirical comparison across domains: medical imaging requirements vs. entertainment visualization vs. mathematical research. Analysis of diminishing returns in iteration count for visual quality.',
    'If threshold is low: perceived convergence bottleneck is artificial, extraction is behavioral (agents choosing over-convergence). If threshold is domain-specific and high: bottleneck is structural, extraction is genuine resource asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_sufficiency_threshold, empirical, 'Sufficiency threshold for IFS convergence across application domains').

omega_variable(
    algorithmic_acceleration_ceiling,
    'Are recent acceleration techniques (adaptive iteration, early termination, GPU vectorization) genuinely novel or incremental refinements of known optimizations?',
    'Systematic literature review of convergence acceleration methods from 1990-present. Classification by novelty vs. re-implementation. Measurement of practical speedup achieved.',
    'If genuinely novel: suppression is declining through innovation, scaffold perspective is valid. If incremental: suppression is structural (fundamental iteration requirements), extraction mechanism is stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_acceleration_ceiling, empirical, 'Whether algorithmic acceleration represents fundamental improvement or incremental optimization').

omega_variable(
    hardware_commodity_transition,
    'Will GPU/ASIC commoditization eventually eliminate the resource concentration that creates extraction asymmetry?',
    'Historical price trends for GPU compute per unit. Projection of cost curves vs. algorithmic efficiency gains. Timeline analysis of when household hardware reaches current HPC capability.',
    'If transition is likely (10-15 year horizon): scaffold sunset is real, constraint converts to rope. If commoditization stalls: extraction persists structurally because hardware cost is irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_commodity_transition, empirical, 'Timeline for hardware commoditization eliminating resource concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iterated_function_systems, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ifs_tr_t0, iterated_function_systems, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ifs_tr_t5, iterated_function_systems, theater_ratio, 5, 0.48).
narrative_ontology:measurement(ifs_tr_t10, iterated_function_systems, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(ifs_be_t0, iterated_function_systems, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ifs_be_t5, iterated_function_systems, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ifs_be_t10, iterated_function_systems, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iterated_function_systems, resource_allocation).
narrative_ontology:affects_constraint(iterated_function_systems, fractal_dimension_computation).
narrative_ontology:affects_constraint(iterated_function_systems, procedural_texture_approximation).
narrative_ontology:affects_constraint(iterated_function_systems, gpu_algorithm_optimization).

% DUAL FORMULATION NOTE:
% IFS convergence can be framed two ways: (1) As a mathematical constraint on computational complexity (fundamental iteration requirements for arbitrary precision), or (2) As an institutional constraint on resource access (concentration of computational capacity). These generate different ε values (0.08 for pure mathematical bottleneck vs. 0.38 for resource allocation asymmetry). This story addresses the institutional constraint; the mathematical bottleneck is decomposed separately as a distinct mountain-type constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iterated_function_systems, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
