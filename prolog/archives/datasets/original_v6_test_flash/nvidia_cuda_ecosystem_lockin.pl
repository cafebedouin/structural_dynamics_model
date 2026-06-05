% ============================================================================
% CONSTRAINT STORY: nvidia_cuda_ecosystem_lockin
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
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
 *   domain: technological
 *
 * SUMMARY:
 *   Nvidia's CUDA ecosystem provides a high-performance platform for AI
 *   development, but also creates a lock-in effect that can be detrimental to
 *   developers and competing hardware vendors. This lock-in arises from the
 *   integrated hardware and software environment, as well as the large CUDA
 *   developer community and extensive software libraries. The CUDA ecosystem
 *   offers significant performance benefits, but it also limits developer
 *   choice and creates a dependency on Nvidia's products and services.
 *
 * KEY AGENTS:
 *   - Nvidia: Primary beneficiary (institutional/arbitrage) - benefits from increased market share and rent extraction.
 *   - AI Developers: Primary victim (powerless/trapped) - face high switching costs and dependence on CUDA.
 *   - Competing Hardware Vendors: Secondary victim (powerful/constrained) - struggle to compete due to CUDA's dominance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nvidia_cuda_ecosystem_lockin, 0.6).
domain_priors:suppression_score(nvidia_cuda_ecosystem_lockin, 0.7).
domain_priors:theater_ratio(nvidia_cuda_ecosystem_lockin, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, extractiveness, 0.6).
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nvidia_cuda_ecosystem_lockin, tangled_rope).
narrative_ontology:human_readable(nvidia_cuda_ecosystem_lockin, "Nvidia CUDA Ecosystem Lock-in").
narrative_ontology:topic_domain(nvidia_cuda_ecosystem_lockin, "technological").

domain_priors:requires_active_enforcement(nvidia_cuda_ecosystem_lockin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nvidia_cuda_ecosystem_lockin, nvidia).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, ai_developers).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, competing_hardware_vendors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% AI developers heavily invested in CUDA-based code face significant costs and disruption to migrate to alternative platforms. They are largely trapped by prior investment and the performance benefits of CUDA.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% AI startups may choose CUDA for its performance, but also face a risk of lock-in. They have some ability to choose other platforms, but face performance or compatibility penalties.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Nvidia benefits from the lock-in, as it creates a strong competitive advantage and allows them to extract rents from the AI ecosystem.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Competing hardware vendors struggle to gain market share due to CUDA's dominance, limiting developer choice.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer sees the CUDA ecosystem as a tangled rope, providing high performance and a large software base, but also creating lock-in and limiting competition.
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
    constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nvidia_cuda_ecosystem_lockin, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nvidia_cuda_ecosystem_lockin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate-high. Nvidia extracts rents from the AI ecosystem through its proprietary platform. Suppression (0.70): High. Significant barriers to switching platforms due to code investment and network effects. Theater ratio (0.30): Low. The CUDA ecosystem provides genuine performance benefits, reducing the theater.
 *
 * PERSPECTIVAL GAP:
 *   AI developers with legacy CUDA code experience the ecosystem as a snare, due to high switching costs. AI startups see it as a tangled rope, balancing performance against the risk of lock-in. Nvidia benefits from the lock-in effect as a rope, using it to strengthen its competitive advantage and extract rents. Competing hardware vendors experience the CUDA lock-in as a snare, finding it difficult to compete. The analytical observer views the ecosystem as a tangled rope, acknowledging both the benefits and drawbacks of CUDA's dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   Nvidia benefits (low d) while AI developers and competing vendors bear costs (high d). The lock-in is enforced by network effects and the performance advantages of CUDA.
 *
 * MANDATROPHY ANALYSIS:
 *   The CUDA ecosystem's classification is complex. It cannot be simply labeled as a Snare because it offers tangible performance benefits and a large software base. Conversely, labeling it as Rope would ignore the restrictive effects of lock-in. The Tangled Rope classification captures this duality, highlighting both the coordination and extraction aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_standard_adoption,
    'Will open standards like SYCL gain sufficient traction to break CUDA''s lock-in?',
    'Track adoption rates of SYCL and other open standards; monitor performance benchmarks across different platforms.',
    'If open standards succeed, CUDA lock-in becomes less of a constraint (classification shifts towards tangled_rope or rope). If they fail, the lock-in intensifies (classification shifts towards snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_standard_adoption, empirical, 'The extent to which open standards gain adoption.').

omega_variable(
    hardware_software_integration_value,
    'Is the performance advantage of Nvidia''s integrated hardware and software sustainable in the long run?',
    'Compare performance of Nvidia GPUs with other hardware platforms using standardized AI benchmarks.',
    'If Nvidia maintains a significant performance lead, lock-in is justified. If other platforms close the gap, the lock-in becomes more of a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_software_integration_value, empirical, 'The sustainability of Nvidia''s performance advantage.').

omega_variable(
    network_effects_persistence,
    'How long will CUDA''s network effects persist, given the rapid pace of innovation in AI hardware and software?',
    'Monitor the growth of the CUDA developer community and the availability of CUDA-compatible software libraries.',
    'If CUDA''s network effects remain strong, the lock-in persists. If they weaken, developers have more flexibility to switch platforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_persistence, empirical, 'The strength and persistence of CUDA''s network effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nvidia_cuda_ecosystem_lockin, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nvid_tr_t0, nvidia_cuda_ecosystem_lockin, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nvid_tr_t5, nvidia_cuda_ecosystem_lockin, theater_ratio, 5, 0.3).
narrative_ontology:measurement(nvid_tr_t10, nvidia_cuda_ecosystem_lockin, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(nvid_be_t0, nvidia_cuda_ecosystem_lockin, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nvid_be_t5, nvidia_cuda_ecosystem_lockin, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(nvid_be_t10, nvidia_cuda_ecosystem_lockin, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nvidia_cuda_ecosystem_lockin, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
