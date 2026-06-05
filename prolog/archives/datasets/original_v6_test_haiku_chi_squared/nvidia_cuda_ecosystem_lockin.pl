% ============================================================================
% CONSTRAINT STORY: nvidia_cuda_ecosystem_lockin
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   Nvidia's CUDA ecosystem represents one of the most consequential platform
 *   lock-ins in contemporary technology. CUDA solved a genuine technical
 *   problem: GPUs have heterogeneous architectures, and writing
 *   high-performance code for them required either vendor-specific low-level
 *   programming or complex compiler infrastructure. CUDA provided a unified,
 *   relatively portable abstraction layer that enabled the explosion of
 *   GPU-accelerated machine learning. However, over the past 15 years, this
 *   coordination mechanism has calcified into an extractive lock-in.
 *   Developers have invested millions of person-hours in CUDA code. Research
 *   institutions have built their entire ML stacks around CUDA libraries
 *   (cuDNN, TensorRT, NCCL). Cloud providers have optimized their
 *   infrastructure for CUDA workloads. The switching costs are now so high
 *   that even when technically superior or more economical alternatives
 *   emerge (AMD ROCm, Intel oneAPI), adoption remains marginal. Nvidia
 *   maintains its dominance not primarily through technical superiority —
 *   other architectures can match CUDA's performance on many workloads — but
 *   through ecosystem lock-in: the gravitational pull of the existing
 *   developer base, libraries, and best practices. This constraint exhibits a
 *   stable tangled_rope character: it provides genuine coordination value
 *   (unified GPU programming) while simultaneously enabling asymmetric
 *   extraction (Nvidia captures monopoly rents from a locked-in base that has
 *   no practical exit). The theater_ratio is relatively low (0.35) because
 *   the coordination function is real and the lock-in mechanism is structural
 *   rather than performative. Unlike theatrical pitons that persist through
 *   ritual maintenance, CUDA's lock-in is maintained through genuine economic
 *   barriers to switching.
 *
 * KEY AGENTS:
 *   - Nvidia Corporation: Primary beneficiary (institutional/arbitrage) — captures dominant market position, software licensing leverage, and ecosystem network effects. Controls both hardware and software layers, enabling vertical lock-in.
 *   - AI Researchers and ML Engineers: Primary victims (powerless/trapped at individual level, moderate/constrained at enterprise level) — deeply invested in CUDA training and code; switching costs are prohibitive. Career advancement path locked to CUDA expertise.
 *   - Alternative GPU Vendors (AMD, Intel, others): Secondary victims (powerful/mobile theoretically, but piton in practice) — have technically viable architectures and interoperable software, but cannot achieve critical mass adoption due to network effects and institutional switching costs. Stuck in inertial equilibrium.
 *   - Cloud Infrastructure Providers: Mixed beneficiary/victim (institutional/constrained) — benefit from CUDA ecosystem maturity and customer demand for CUDA-compatible services. Locked in: investing in alternative backends risks fragmentation and customer alienation.
 *   - Downstream AI Researchers: Victims (moderate/constrained) — depend on CUDA-optimized libraries and hardware access; innovation constrained by Nvidia's release cycles and pricing.
 *   - Hardware Portability Principle: Abstract victim (powerless/trapped) — the technical goal of write-once-run-anywhere is degraded by CUDA lock-in; hardware independence is sacrificed for lock-in.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both the genuine coordination problem (GPU heterogeneity) and the extractive lock-in mechanism (ecosystem network effects); emerging alternatives (JAX multi-backend, PyTorch abstraction layers) represent genuine escapes from CUDA dependency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nvidia_cuda_ecosystem_lockin, 0.52).
domain_priors:suppression_score(nvidia_cuda_ecosystem_lockin, 0.68).
domain_priors:theater_ratio(nvidia_cuda_ecosystem_lockin, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, extractiveness, 0.52).
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nvidia_cuda_ecosystem_lockin, tangled_rope).
narrative_ontology:human_readable(nvidia_cuda_ecosystem_lockin, "Nvidia CUDA Ecosystem Lock-in").
narrative_ontology:topic_domain(nvidia_cuda_ecosystem_lockin, "technological/software_infrastructure").

domain_priors:requires_active_enforcement(nvidia_cuda_ecosystem_lockin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nvidia_cuda_ecosystem_lockin, nvidia_corporation).
narrative_ontology:constraint_beneficiary(nvidia_cuda_ecosystem_lockin, cuda_developer_base).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, alternative_gpu_vendors).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, downstream_ai_researchers).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, hardware_portability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AI RESEARCHER (SNARE) — Once deeply invested in CUDA-optimized code, switching architectures means rewriting entire ML pipelines. Career advancement depends on access to CUDA-capable hardware. No practical exit. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENTERPRISE DEPLOYMENT (SNARE) — Organizational investment in CUDA training, library selection, and hardware procurement creates switching costs. Exit is theoretically possible but economically punitive. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE MODEL LAB (TANGLED_ROPE) — Benefits from CUDA performance leadership and ecosystem maturity (coordination function). Also locked in: benefits depend on continuous Nvidia innovation and lack of viable alternatives. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NVIDIA (ROPE) — Experiences the constraint as pure coordination: CUDA unifies software development across heterogeneous GPU generations, enabling ecosystem network effects. Benefits massively from lock-in but frames it as solving a real technical problem. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE VENDOR (PITON) — AMD (ROCm), Intel (oneAPI), and others have functionally equivalent architectures and interoperable software stacks, but cannot compete because the CUDA ecosystem's lock-in has become self-reinforcing through network effects and institutional inertia. The 'alternative' is technically viable but institutionally extinct. theater_ratio=0.62 (vendor messaging about openness and compatibility is largely performative; actual adoption remains marginal).
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED_ROPE) — From civilizational timescale, the constraint exhibits both coordination (CUDA genuinely solved a heterogeneous compiler problem) and extraction (lock-in is maintained through ecosystem advantages, not technical necessity). Emerging standards (SYCL, OpenCL, JAX backends, PyTorch multi-backend) are alternative coordination mechanisms that could decouple software from hardware. d≈0.65, f(d)≈0.95, σ=1.2 → χ≈0.59.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nvidia_cuda_ecosystem_lockin_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, TypeOther, context(agent_power(powerful), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. Nvidia captures substantial monopoly rents from locked-in developers and enterprises. The effective extraction is not maximal because alternatives do exist (switching is economically painful, not technically impossible) and the ecosystem continues to deliver genuine technical value. However, the extractiveness has grown over time (0.22 → 0.52 over the measurement interval) as lock-in has deepened and Nvidia's market power has consolidated. Suppression (0.68): High. Multiple barriers prevent exit: (1) switching costs are enormous (rewriting ML pipelines, retraining teams); (2) the alternative ecosystem (ROCm, oneAPI) remains immature relative to CUDA; (3) network effects (developer base, library maturity, cloud provider support) create a positive feedback loop favoring CUDA; (4) Nvidia's first-mover advantage means the company controls the highest-performance implementation. Theater ratio (0.35): Low-moderate. The coordination function is largely real — CUDA genuinely solved the GPU compiler heterogeneity problem. The lock-in is not maintained through theatrical performance but through structural economic barriers (switching costs, network effects). This distinguishes CUDA from pitons, which persist through ritual maintenance rather than real function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates a substantial perspectival gap between the beneficiary and victims. Nvidia sees pure coordination (Rope) — they experience CUDA as solving a real technical problem and enabling ecosystem growth. Large model labs see mixed benefits (Tangled Rope) — they benefit from CUDA's maturity but are locked in to Nvidia's release cycles. Researchers see extraction (Snare) — their code investments and career paths are locked in with no practical exit. Alternative vendors see an inert degraded competitor (Piton) — they have technically viable alternatives but cannot break the lock-in cycle. The analytical observer sees the constraint as a tangled rope that is transitioning toward piton status: as emerging alternatives (JAX multi-backend, PyTorch abstraction) mature, CUDA's dominance becomes increasingly dependent on institutional inertia rather than genuine technical necessity. This perspectival diversity is a signature of tangled ropes: the constraint is real (not a false natural law) and it provides genuine benefits, but the benefits are systematically asymmetric and enable extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Nvidia: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Strong net beneficiary. AI researchers: Victim + trapped (individually) → d≈0.92, f(d)≈1.38; Victim + constrained (enterprise) → d≈0.75, f(d)≈1.10. High extraction. Large model labs: Mixed (beneficiary at high-tech frontier, victim from lock-in) + arbitrage → d≈0.45, f(d)≈0.50. Moderate extraction with benefits. Alternative vendors: Potential beneficiary (if they captured market) but actually victims of network effects + mobile → d≈0.55, f(d)≈0.75. Piton classification comes from high theater_ratio if we measured from the vendor's performative messaging angle; measured structurally (market position), it's a victim of lock-in. Analytical observer: analytical → d≈0.65, f(d)≈0.95. Sees the constraint from outside, recognizing both coordination and extraction mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint's tangled_rope classification resolves the mandatrophy by showing that lock-in is a hybrid of coordination and extraction. The coordination function is real: CUDA genuinely unified GPU programming across heterogeneous architectures. The extraction is also real: Nvidia captures monopoly rents from locked-in users who cannot practically exit. Neither reading (pure coordination, pure extraction) is wrong; they describe different aspects of the same structural reality. The constraint appears as tangled_rope from the analytical perspective because: (1) beneficiaries (Nvidia, CUDA dev base) and victims (researchers, alternative vendors) are clearly identified; (2) both coordination and extraction functions are active (not just one); (3) enforcement is ongoing (Nvidia invests in ecosystem maturity to maintain lock-in). The mandatrophy is resolved by measuring the perspectival gap: beneficiaries experience Rope (low d, high network benefits); victims experience Snare (high d, constrained exit). The constraint is neither purely coordination nor purely extraction — it is a hybrid that can only be understood through multiple indexed perspectives. Emerging alternatives (PyTorch multi-backend, JAX XLA compilation across different backends, Modular Mojo as a hardware-agnostic language) may transition this toward Scaffold (temporary coordination with a sunset as portability improves), but as of 2026, the lock-in remains structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cuda_necessity_vs_contingency,
    'Is CUDA''s dominance a necessary consequence of technical superiority or a contingent outcome of ecosystem network effects and switching costs?',
    'Comparative performance analysis of ROCm, oneAPI, and CUDA on identical workloads controlling for library maturity; survey of adoption decisions to isolate technical vs institutional factors; measurement of switching costs across different AI workload categories',
    'If necessary: constraint is closer to Mountain (immutable technical limit). If contingent: constraint is firmly Snare/Tangled Rope (extractive lock-in maintaining artificial dominance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cuda_necessity_vs_contingency, empirical, 'Whether CUDA dominance reflects technical necessity or contingent network effects').

omega_variable(
    alternative_ecosystem_tipping_point,
    'What adoption threshold for alternative backends (PyTorch multi-backend, JAX XLA, Modular Mojo) would trigger irreversible migration away from CUDA lock-in?',
    'Historical precedent analysis (GPU vs TPU, CUDA vs HIP timing); measurement of threshold effects in developer tool adoption; modeling of network effect decay rates as alternatives mature',
    'If threshold is low (< 10% market share): ecosystem is fragile and lock-in could break quickly. If threshold is high (> 40%): Nvidia can maintain dominance even with viable alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_ecosystem_tipping_point, empirical, 'Adoption threshold for triggering irreversible migration from CUDA').

omega_variable(
    government_intervention_feasibility,
    'Could antitrust enforcement or government-mandated interoperability standards effectively reduce CUDA lock-in, or does the technical complexity of compiler optimization make such mandates impractical?',
    'Legal analysis of precedent (Microsoft/IE antitrust, Apple/App Store), technical analysis of interop standards feasibility (comparing SYCL vs CUDA API burden), policy scenario modeling',
    'If feasible: lock-in becomes regulatory risk, shortening effective extraction window. If infeasible: lock-in is structurally protected and extraction can continue indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_intervention_feasibility, conceptual, 'Feasibility of antitrust or interoperability-based intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nvidia_cuda_ecosystem_lockin, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cuda_tr_t0, nvidia_cuda_ecosystem_lockin, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cuda_tr_t5, nvidia_cuda_ecosystem_lockin, theater_ratio, 5, 0.3).
narrative_ontology:measurement(cuda_tr_t10, nvidia_cuda_ecosystem_lockin, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(cuda_be_t0, nvidia_cuda_ecosystem_lockin, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cuda_be_t5, nvidia_cuda_ecosystem_lockin, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(cuda_be_t10, nvidia_cuda_ecosystem_lockin, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nvidia_cuda_ecosystem_lockin, global_infrastructure).
narrative_ontology:affects_constraint(nvidia_cuda_ecosystem_lockin, ai_model_training_infrastructure).
narrative_ontology:affects_constraint(nvidia_cuda_ecosystem_lockin, gpu_supply_chain_concentration).
narrative_ontology:affects_constraint(nvidia_cuda_ecosystem_lockin, ml_framework_standardization).

% DUAL FORMULATION NOTE:
% The CUDA lock-in constraint is upstream of multiple downstream constraints: it creates dependencies in AI training infrastructure (models optimized for CUDA), GPU supply chain decisions (cloud providers locked into Nvidia), and ML framework design (libraries optimized for CUDA performance). The constraint family should include separate analyses of the GPU supply concentration (hardware) and ML framework lock-in (software) as distinct but coupled constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
