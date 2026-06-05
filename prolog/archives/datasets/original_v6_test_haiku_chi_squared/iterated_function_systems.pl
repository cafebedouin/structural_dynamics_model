% ============================================================================
% CONSTRAINT STORY: iterated_function_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   Iterated Function Systems (IFS) generate fractals through repeated
 *   application of affine transformations, producing visually complex
 *   structures from simple recursive rules. The constraint arises at the
 *   intersection of mathematical elegance and computational expense:
 *   generating high-resolution fractal images requires exponentially
 *   increasing iteration counts, which demand access to computational
 *   resources (GPU time, memory bandwidth, specialized hardware). This
 *   creates a structural tension between the algorithm's mathematical
 *   simplicity and its practical computational cost. Commercial graphics
 *   firms control optimized IFS implementations and GPU infrastructure,
 *   creating an extraction mechanism where researchers and educators must
 *   either pay for computational access or operate with degraded educational
 *   implementations. Open-source alternatives exist but lack the optimization
 *   and hardware acceleration of commercial solutions, creating a
 *   performative theater where community effort maintains interfaces without
 *   solving the underlying computational bottleneck. The constraint exhibits
 *   properties of both coordination (IFS does solve a legitimate problem:
 *   generating complex fractal imagery from simple rules) and extraction
 *   (computational resource gatekeeping creates artificial scarcity). The
 *   theater ratio (0.62) reflects that much IFS documentation and open-source
 *   effort is performative: tutorials and algorithm descriptions abound, but
 *   they cannot overcome the fundamental computational cost — users must
 *   still either pay or wait.
 *
 * KEY AGENTS:
 *   - Underfunded Researchers: Primary victim (powerless/trapped) — cannot exit IFS research without abandoning computational access; depend on institutional GPU allocation or cloud services
 *   - Educational Institutions: Secondary victim (moderate/constrained) — limited budgets for hardware and licensing; benefit from IFS curriculum integration but constrained by resource costs
 *   - Commercial Graphics Rendering Firms: Primary beneficiary (institutional/arbitrage) — control optimized IFS implementations, GPU acceleration libraries, and licensing; benefit from computational monopoly
 *   - Open Source IFS Community: Organized but degraded (organized/constrained) — maintain fractint, Fragmentarium, apophysis with volunteer effort; sustain interface but cannot overcome computational bottleneck
 *   - GPU Computational Access Providers: Institutional gatekeeper (institutional/arbitrage) — cloud GPU services (AWS, Lambda Labs) control computational resource allocation; extract via per-hour billing
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing computational necessity as inherent mathematical limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iterated_function_systems, 0.35).
domain_priors:suppression_score(iterated_function_systems, 0.48).
domain_priors:theater_ratio(iterated_function_systems, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iterated_function_systems, extractiveness, 0.35).
narrative_ontology:constraint_metric(iterated_function_systems, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(iterated_function_systems, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(iterated_function_systems, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(iterated_function_systems, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iterated_function_systems, tangled_rope).
narrative_ontology:human_readable(iterated_function_systems, "IFS Convergence and Computational Resources").
narrative_ontology:topic_domain(iterated_function_systems, "technological/computational_mathematics").

domain_priors:requires_active_enforcement(iterated_function_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iterated_function_systems, commercial_graphics_rendering_firms).
narrative_ontology:constraint_beneficiary(iterated_function_systems, compression_algorithm_developers).
narrative_ontology:constraint_victim(iterated_function_systems, educational_institutions).
narrative_ontology:constraint_victim(iterated_function_systems, open_source_researchers).
narrative_ontology:constraint_victim(iterated_function_systems, gpu_computational_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERFUNDED RESEARCHER (SNARE) — Trapped by computational resource barriers. Cannot exit without abandoning IFS research. Limited GPU access, high per-iteration cost. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.58. Full extraction: computational necessity creates path dependency.
constraint_indexing:constraint_classification(iterated_function_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTION (TANGLED ROPE) — Constrained by budget limits but benefits from IFS curriculum integration and student skill development. Faces licensing and hardware costs. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.37. Mixed: coordination function (teaching) + extraction (resource requirement).
constraint_indexing:constraint_classification(iterated_function_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMERCIAL GRAPHICS FIRM (ROPE) — Benefits from IFS compression efficiency. Owns computational infrastructure; can arbitrage between proprietary and open implementations. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Net beneficiary: controls resource access and algorithm licensing.
constraint_indexing:constraint_classification(iterated_function_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE IFS COMMUNITY (PITON) — Organized effort to maintain IFS libraries (fractint, apophysis) with minimal computational overhead. Theater ratio=0.62 reflects performative documentation and tutorial maintenance without underlying computational efficiency gains. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.28. Degraded: community labor sustains interface; core computational bottleneck persists.
constraint_indexing:constraint_classification(iterated_function_systems, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GPU-ACCELERATED IFS INITIATIVE (SCAFFOLD) — Temporary coordination mechanism with sunset. CUDA/OpenCL implementations are creating alternative computational pathways with explicit lifecycle end (as quantum computing and neuromorphic hardware mature). d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.13. Low extraction; organized agents have agency and see a path beyond the constraint.
constraint_indexing:constraint_classification(iterated_function_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational perspective, iteration count grows exponentially with desired detail; computational cost is inherent to fractal generation. Any finite resolution requires at least N iterations where N scales with precision. This perspective risks naturalizing what is contingent resource allocation. d≈0.70, f(d)≈1.12, σ=1.0 → χ≈0.39. False summit: structural data contradicts mountain classification.
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
 *   Extractiveness (0.35): Moderate. IFS convergence has a real computational cost — iteration count grows with desired detail (approximately logarithmic in precision). However, this cost is not purely extractive: much of it reflects genuine algorithmic requirement. The extraction component (0.35 rather than 0.15) comes from resource gatekeeping and artificial scarcity, not from the math itself. GPU licensing, cloud service markups, and proprietary optimization create extraction above the mathematical baseline. Suppression (0.48): Moderate. Significant barriers include GPU cost, memory bandwidth limitations, and vendor lock-in (CUDA vs OpenCL vs Metal). But suppression is not total — open-source alternatives exist, alternative algorithms (escape-time, hybrid methods) provide some bypass options, and quantum/neuromorphic hardware represents emerging alternatives. Theater ratio (0.62): Moderate-high. IFS documentation (countless tutorials, algorithm descriptions) is abundant but performative. The theater is that education and open-source effort create the appearance of accessibility while the core computational bottleneck remains. Users can understand the algorithm perfectly but still cannot render high-resolution fractals without computational resources. The theater has increased over the measurement interval as GPU specialization has deepened while open-source alternatives have stagnated in optimization.
 *
 * PERSPECTIVAL GAP:
 *   The commercial graphics firm sees coordination (Rope) — IFS solves a real computational problem elegantly. The underfunded researcher sees pure extraction (Snare) — the same IFS method that is elegant to describe is computationally expensive to execute and gated by resource access. The educational institution sees a mixed arrangement (Tangled Rope) — IFS enables powerful curriculum content but with resource costs. The open-source community sees a degraded ritual (Piton) — maintaining fractint and fractal libraries is substantial community work that provides interface access without solving the computational limitation. The GPU-acceleration initiative sees a temporary problem with a sunset (Scaffold) — current GPU-based IFS implementations are an intermediate technology with explicit obsolescence as quantum and neuromorphic hardware mature. The civilizational observer risks seeing natural law (Mountain) — iteration count scaling appears inherent to fractal generation — but the constraint is actually a contingent arrangement of resource allocation and licensing.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial graphics firm: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary: controls resource access and optimization. Underfunded researcher: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction: no exit option, complete dependence on resource access. Educational institution: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but with partial benefit (curriculum integration). Open source community: Organized + constrained → d≈0.55, f(d)≈0.75. Moderate extraction; community labor sustains interface without solving bottleneck. GPU-acceleration initiative: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction; organized agents have agency and see alternative pathways. Analytical observer: analytical → d≈0.70, f(d)≈1.12. Mountain perspective naturalized constraint; engine detects false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   IFS constraint resolves mandatrophy by decomposing into two structurally distinct claims: (1) Mathematical: Fractal generation requires iteration; this is inherent. (2) Institutional: Computational resources are gated by licensing and hardware access; this is contingent. The false summit (Mountain from analytical perspective) confuses these two. The actual constraint is Tangled Rope: genuine coordination (elegant algorithm solving genuine compression/generation problem) plus genuine extraction (resource gatekeeping). The theater ratio (0.62) reflects educational effort that creates appearance of accessibility without solving computational cost. Resolution: The constraint is NOT a natural law; it is an institutional arrangement that can be partially dissolved through algorithmic innovation (better convergence criteria, hybrid methods) or hardware access democratization (cloud services, quantum computing). The open-source community's piton perspective is structural reality — community labor maintains the interface while the underlying bottleneck persists. The scaffold perspective (GPU acceleration as temporary) is the most hopeful: current GPU-based IFS will be obsolete within 15-20 years as tensor cores and quantum algorithms mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iteration_convergence_criterion,
    'What precision threshold distinguishes convergent IFS approximation from extractive over-iteration?',
    'Empirical analysis of visual discernibility vs iteration count; comparison of perceptual convergence (human eye detection limit) vs mathematical convergence (machine epsilon)',
    'If perceptual convergence << mathematical convergence: significant computational extraction occurs in imperceptible iterations. If they align: most iteration costs are justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iteration_convergence_criterion, empirical, 'Convergence criterion distinguishing necessary vs extractive iteration').

omega_variable(
    memory_bottleneck_vs_arithmetic,
    'Is the primary computational bottleneck arithmetic operations or memory bandwidth? Does this shift with hardware architecture?',
    'Profiling studies across CPU, GPU, and tensor hardware; measurement of FLOPs achieved vs theoretical peak; memory access patterns in different IFS algorithms',
    'If arithmetic-bound: parallelization is effective, suppression decreases. If memory-bound: architectural constraints dominate, suppression increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(memory_bottleneck_vs_arithmetic, empirical, 'Whether computational bottleneck is arithmetic or memory').

omega_variable(
    open_source_cuda_feasibility,
    'Can open-source CUDA implementations achieve feature parity with proprietary commercial IFS renderers without licensing barriers?',
    'Benchmarking of open-source (cuIFS, Fragmentarium) vs commercial (Ultra Fractal, Apophysis Pro) implementations; measurement of feature lag and performance ratios',
    'If feasible: scaffold sunset is real, suppression will decline. If not: open source remains degraded (piton), suppression persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_cuda_feasibility, empirical, 'Whether open-source can achieve feature parity with commercial renderers').

omega_variable(
    algorithmic_improvement_ceiling,
    'Does the fundamental algorithm class (affine transformation iteration) have algorithmic complexity room for improvement, or are current methods near-optimal?',
    'Theoretical complexity analysis of alternative IFS algorithms (e.g., escape-time variants, hybrid methods); benchmarking against theoretical lower bounds',
    'If room for improvement: suppression and extractiveness can decrease via innovation. If near-optimal: the constraint is closer to a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_improvement_ceiling, empirical, 'Whether current IFS algorithms have room for improvement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iterated_function_systems, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ifs_tr_t0, iterated_function_systems, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ifs_tr_t5, iterated_function_systems, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ifs_tr_t10, iterated_function_systems, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(ifs_be_t0, iterated_function_systems, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ifs_be_t5, iterated_function_systems, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(ifs_be_t10, iterated_function_systems, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iterated_function_systems, resource_allocation).
narrative_ontology:affects_constraint(iterated_function_systems, fractal_compression_efficiency).
narrative_ontology:affects_constraint(iterated_function_systems, gpu_memory_bandwidth_limit).
narrative_ontology:affects_constraint(iterated_function_systems, quantum_algorithm_simulation).

% DUAL FORMULATION NOTE:
% IFS convergence decomposes into two constraints: (1) Mathematical convergence (near-universal, low ε, mountain-like) and (2) Computational resource gatekeeping (contingent, moderate ε, tangled_rope). This story addresses the institutional arrangement (constraint 2). The upstream mathematical constraint has independent epistemic status and should be analyzed separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iterated_function_systems, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
