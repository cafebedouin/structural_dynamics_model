% ============================================================================
% CONSTRAINT STORY: nvidia_cuda_ecosystem_lockin
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nvidia_cuda_ecosystem_lockin, []).

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
 *   constraint_id: nvidia_cuda_ecosystem_lockin
 *   human_readable: Nvidia CUDA Ecosystem Lock-in
 *   domain: technological/software_infrastructure
 *
 * SUMMARY:
 *   Nvidia's CUDA ecosystem represents an integrated hardware-software
 *   constraint that exhibits tangled rope characteristics at multiple scales.
 *   The constraint combines genuine coordination function (CUDA solves the
 *   heterogeneous accelerator abstraction problem elegantly) with asymmetric
 *   extraction (Nvidia captures disproportionate value; competing platforms
 *   are marginalized; users face high switching costs). The base
 *   extractiveness (0.62) reflects that CUDA's dominance is not purely
 *   passive network effect — it is actively maintained through continuous
 *   optimization, developer incentive alignment, and architectural coupling
 *   that makes alternatives persistently inferior. The suppression (0.68)
 *   reflects high barriers to exit: individual researchers and organizations
 *   have invested substantially in CUDA knowledge and tooling; retraining
 *   costs are significant; alternative ecosystems (OpenCL, HIP, oneAPI)
 *   remain immature or performance-inferior; TPU and other proprietary
 *   alternatives offer no freedom from vendor lock-in, merely different
 *   vendors. Theater ratio (0.35) is relatively low, indicating the
 *   constraint is primarily functional rather than performative — CUDA
 *   genuinely provides superior developer experience and performance
 *   optimization, not merely theatrical appeals to standardization.
 *
 * KEY AGENTS:
 *   - Nvidia Corporation: Primary beneficiary (institutional/arbitrage) — captures value from ecosystem network effects and hardware-software co-optimization
 *   - Locked-In AI Researchers: Primary victim (powerless/trapped) — bear full switching cost of ecosystem dependency; no realistic exit path
 *   - Enterprise AI Organizations: Secondary victim (organized/constrained) — benefit from CUDA maturity but face vendor risk and architectural constraints
 *   - Competing GPU Manufacturers: Secondary victim (institutional/arbitrage) — maintain degraded alternative ecosystems (AMD ROCm, Intel oneAPI) with persistent performance disadvantage
 *   - Open-Source Framework Projects: Organized agents (organized/constrained) — PyTorch, JAX building substrate-agnostic abstractions to reduce CUDA moat; see sunset pathway
 *   - Alternative Accelerator Platforms: Organized agents (organized/constrained) — TPU, Cerebras, Graphcore attempting to break CUDA lock-in but starting from zero developer base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nvidia_cuda_ecosystem_lockin, 0.62).
domain_priors:suppression_score(nvidia_cuda_ecosystem_lockin, 0.68).
domain_priors:theater_ratio(nvidia_cuda_ecosystem_lockin, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, extractiveness, 0.62).
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nvidia_cuda_ecosystem_lockin, tangled_rope).
narrative_ontology:human_readable(nvidia_cuda_ecosystem_lockin, "Nvidia CUDA Ecosystem Lock-in").
narrative_ontology:topic_domain(nvidia_cuda_ecosystem_lockin, "technological/software_infrastructure").

domain_priors:requires_active_enforcement(nvidia_cuda_ecosystem_lockin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nvidia_cuda_ecosystem_lockin, nvidia_corporation).
narrative_ontology:constraint_beneficiary(nvidia_cuda_ecosystem_lockin, cuda_ecosystem_developers).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, competing_gpu_manufacturers).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, ai_researchers_without_cuda_access).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, alternative_accelerator_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN AI RESEARCHER (SNARE) — Individual researchers and smaller organizations cannot easily migrate from CUDA due to training investment, library dependencies, and performance requirements. Switching costs are high; alternatives (OpenCL, HIP, oneAPI) offer inferior performance or immature ecosystems. No realistic exit path.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENTERPRISE AI ORGANIZATION (TANGLED ROPE) — Large organizations benefit from CUDA's maturity, performance optimization, and developer talent concentration. However, they also bear costs: license dependency, vendor risk, and architectural constraints. Some exit capacity through multi-platform strategies, but constrained by switching friction and ecosystem lock-in at scale.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NVIDIA CORPORATION (ROPE) — Primary beneficiary. CUDA ecosystem functions as a coordination mechanism: it standardizes hardware-software integration, enabling rapid AI development and deployment. Nvidia experiences this as pure coordination — the ecosystem solves the collective problem of heterogeneous accelerator abstraction. Arbitrage exit: Nvidia can shift to other markets if GPU demand changes.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE ALTERNATIVE PROJECTS (SCAFFOLD) — PyTorch, JAX, and others are building substrate-agnostic abstractions that reduce CUDA dependency. These projects see the lock-in as a temporary coordination failure with a sunset: as backend abstraction layers mature and alternative accelerators (TPU, Cerebras, Graphcore) improve, the CUDA moat weakens. Theater is low (actual functional decoupling); sunset estimated at 5-10 years for multi-backend maturity in production AI systems.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPETING GPU MANUFACTURERS (PITON) — AMD (ROCm), Intel (oneAPI), and others maintain alternative ecosystems that are structurally functional but perform significantly worse than CUDA in practice. These alternatives persist through institutional inertia and vendor funding, not through competitive advantage. Theater ratio is high: marketing claims of platform-agnostic development are undermined by persistent CUDA advantage. No exit: competitors are locked into their own infrastructure investments.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a first-principles perspective, GPU compute standardization around a single proven ecosystem is a natural consequence of network effects and switching cost economics. First-mover advantage + developer concentration + hardware-software co-optimization produce a stable equilibrium that appears immutable. However, this risks naturalizing what is actually an enforceable institutional lock-in. The engine's false summit detector will identify this as naturalization of contingent market structure.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nvidia_cuda_ecosystem_lockin_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nvidia_cuda_ecosystem_lockin, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nvidia_cuda_ecosystem_lockin, TR),
    TR >= 0.70.

:- end_tests(nvidia_cuda_ecosystem_lockin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. CUDA's dominance combines genuine technical superiority (coordination function) with enforced network effects (extraction mechanism). Nvidia captures value through: (1) hardware-software co-optimization advantage, (2) developer talent concentration, (3) continuous optimization investment competitors cannot match, (4) architectural decisions that create switching friction. The value is not extracted through coercive mechanisms but through compounding technical advantage. The measurement tracks a rising trajectory (0.35→0.62 over interval) reflecting that as AI adoption accelerated, CUDA's advantage compound-networked: more developers attracted → more libraries built → higher switching cost for organizations → stronger lock-in → more vendor rent-capture possible. Suppression (0.68): High. Barriers to exit include: (1) educational investment in CUDA-specific knowledge, (2) library ecosystem dependency (cuBLAS, cuDNN, NCCL all CUDA-native), (3) performance expectations created by CUDA optimization, (4) organizational inertia (retraining large ML teams is costly), (5) alternative platforms explicitly offer no proprietary freedom (TPU, Cerebras are also vendor-locked), (6) publication bias toward CUDA-optimized research. Theater (0.35): Low. The constraint is substantially functional — CUDA genuinely delivers better performance and developer experience. Theater increases only at the edges: marketing of 'platform-agnostic' alternatives that remain CUDA-dependent; competitor claims of parity that benchmarks contradict.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximum perspectival divergence from base properties. Nvidia sees pure coordination (Rope) — CUDA solves a real collective problem of accelerator abstraction. Locked-in researchers see pure extraction (Snare) — they bear all costs with no exit. Enterprise organizations see mixed function and extraction (Tangled Rope) — they benefit from maturity but also bear vendor risk. Open-source projects see a temporary problem (Scaffold) — they are building abstraction layers with an estimated 5-10 year sunset. Competitors see their own degraded systems (Piton) — ROCm and oneAPI persist through inertia, not competitive advantage. The analytical observer risks a false mountain — treating first-mover network effects as immutable law rather than contingent equilibrium. The perspectival gap reveals that CUDA lock-in is not a natural law of GPU computing but a specific institutional arrangement that different agents experience as coordination, extraction, temporality, degradation, or naturalization depending on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each agent are derived from their structural position: (1) Nvidia is a beneficiary with arbitrage options (can shift markets if GPU demand changes) — derives low d → negative effective extraction χ → agent experiences rope. (2) Individual researchers are victims trapped in CUDA (no arbitrage, high switching cost) — derives high d → high f(d) → high χ → agent experiences snare. (3) Enterprise organizations are victims with constrained options (can develop multi-platform strategies but at cost) — derives moderate d → moderate χ → tangled rope. (4) Open-source projects are organized with exit capacity (they are building alternatives) — derives low-moderate d → low χ → scaffold. (5) Competing manufacturers are institutional with arbitrage capacity but no market advantage (locked into their own infrastructure) — derives moderate d, but piton classification overrides based on theater gate. (6) Analytical observer sees the structure from civilizational distance — derives moderate d, but false summit detector flags the mountain classification as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT CLASSIFICATION VERIFICATION: The tangled_rope classification is structurally justified and avoids the mandatrophy trap. The constraint exhibits both genuine coordination function (CUDA does solve the heterogeneous accelerator abstraction problem at scale) AND asymmetric extraction (Nvidia captures disproportionate value; competitors and users face persistent disadvantage). It is not a pure rope (which would require minimal extraction) nor a pure snare (which would require zero coordination benefit). The perspectives confirm: Rope (Nvidia), Tangled Rope (Enterprise), Snare (Individual), Scaffold (Open Source), Piton (Competitors), and a false Mountain (Analytical). The perspectival spread from Snare to Rope validates that the base metrics (ε=0.62, suppression=0.68, χ modulated across agent positions) are capturing real structural heterogeneity, not ambiguity about a single type. The mandatory tangled_rope gates are satisfied: (1) requires_active_enforcement=true: CUDA dominance is sustained through continuous optimization and ecosystem investment, (2) beneficiaries declared: nvidia_corporation, cuda_ecosystem_developers, (3) victims declared: competing_gpu_manufacturers, ai_researchers_without_cuda_access, alternative_accelerator_platforms. No mandatrophy resolution needed — the classification is robust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_accelerator_parity,
    'Will open alternative accelerators (TPU, Cerebras, Graphcore, Trainium) achieve performance parity with CUDA-optimized workflows within 5-10 years?',
    'Benchmark comparison of production AI training/inference across CUDA vs alternatives; adoption rate tracking by industry segment; performance-per-watt measurements',
    'If parity achieved: scaffold perspective confirmed, CUDA lock-in weakens to rope. If parity fails: CUDA dominance persists, snare perspective remains structural for non-Nvidia agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_accelerator_parity, empirical, 'Whether alternative accelerators achieve competitive performance').

omega_variable(
    developer_ecosystem_fluidity,
    'Can PyTorch/JAX/MLIR abstraction layers achieve true substrate-agnostic compilation efficiency without persistent CUDA-specific optimizations?',
    'Analysis of compilation overhead for non-CUDA backends; measurement of performance degradation when forced to use CUDA-free code paths; tracking of developer satisfaction with non-CUDA workflows',
    'If achievable: architectural lock-in is weaker than enforcement lock-in, tangled rope rather than snare. If not achievable: abstraction layers are performative, piton-level alternative platforms remain dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developer_ecosystem_fluidity, empirical, 'Whether abstraction layers can provide true substrate-agnostic performance').

omega_variable(
    enforcement_mechanism_clarity,
    'Is CUDA ecosystem dominance enforced actively by Nvidia (pricing, licensing restrictions, deliberate API incompatibility) or passively through network effects and developer preference?',
    'Analysis of Nvidia licensing terms, API stability guarantees, interoperability intentions; comparison with historical enforced lock-in cases (e.g., Windows Office, Intel x86); survey of developer migration barriers due to policy vs technical factors',
    'If active enforcement: pure snare classification. If passive network effects: tangled rope or rope classification. If mixed: classification depends on agent power level and exit capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_clarity, conceptual, 'Whether CUDA dominance is actively enforced or passively maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nvidia_cuda_ecosystem_lockin, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cuda_tr_t0, nvidia_cuda_ecosystem_lockin, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cuda_tr_t5, nvidia_cuda_ecosystem_lockin, theater_ratio, 5, 0.31).
narrative_ontology:measurement(cuda_tr_t10, nvidia_cuda_ecosystem_lockin, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(cuda_be_t0, nvidia_cuda_ecosystem_lockin, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cuda_be_t5, nvidia_cuda_ecosystem_lockin, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cuda_be_t10, nvidia_cuda_ecosystem_lockin, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nvidia_cuda_ecosystem_lockin, information_standard).
narrative_ontology:affects_constraint(nvidia_cuda_ecosystem_lockin, gpu_compute_standardization).
narrative_ontology:affects_constraint(nvidia_cuda_ecosystem_lockin, ai_infrastructure_moat).
narrative_ontology:affects_constraint(nvidia_cuda_ecosystem_lockin, developer_training_capital).

% DUAL FORMULATION NOTE:
% CUDA lock-in is downstream of GPU compute standardization (the market settled on CUDA as the dominant platform) and upstream of specific AI infrastructure investments that depend on CUDA. The upstream constraint reflects empirical dominance; this story reflects the institutional enforcement and switching cost mechanisms that maintain dominance. If alternative accelerators achieve parity, this constraint weakens to rope or scaffold; the upstream standardization constraint remains but with different effectiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nvidia_cuda_ecosystem_lockin, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
