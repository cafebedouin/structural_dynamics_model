% ============================================================================
% CONSTRAINT STORY: tsp_computational_complexity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_tsp_computational_complexity, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tsp_computational_complexity
 *   human_readable: Computational Complexity of the Traveling Salesman Problem
 *   domain: technological
 *
 * SUMMARY:
 *   The Traveling Salesman Problem (TSP) is a classic problem in combinatorial optimization,
 *   where the goal is to find the shortest possible route that visits each city exactly once
 *   and returns to the origin city. The inherent computational complexity (NP-hardness)
 *   of finding the optimal solution for large instances poses a significant constraint,
 *   creating a market for approximation algorithms and a competitive moat for actors
 *   with sufficient resources to tackle it at scale.
 *
 * KEY AGENTS (by structural relationship):
 *   - Small businesses & gig economy drivers: Primary target (powerless/trapped) — bears extraction from inefficient routing or high software costs.
 *   - Large logistics corporations & tech giants: Primary beneficiary (institutional/arbitrage) — benefits from the complexity as a barrier to entry.
 *   - Algorithm developers: Secondary beneficiary (analytical/mobile) — benefits from the demand for heuristic solutions.
 *   - Analytical observer: Sees the full structure of extraction and coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsp_computational_complexity, 0.47).
domain_priors:suppression_score(tsp_computational_complexity, 0.60).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(tsp_computational_complexity, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsp_computational_complexity, extractiveness, 0.47).
narrative_ontology:constraint_metric(tsp_computational_complexity, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(tsp_computational_complexity, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(tsp_computational_complexity, tangled_rope).
narrative_ontology:human_readable(tsp_computational_complexity, "Computational Complexity of the Traveling Salesman Problem").
narrative_ontology:topic_domain(tsp_computational_complexity, "technological").

% --- Binary flags ---
domain_priors:requires_active_enforcement(tsp_computational_complexity). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(tsp_computational_complexity, large_logistics_corporations).
narrative_ontology:constraint_beneficiary(tsp_computational_complexity, algorithm_developers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(tsp_computational_complexity, small_scale_logistics_operators).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. For a small operator, the problem is
% an intractable trap, forcing reliance on costly or inefficient solutions.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.47 * 1.42 * 1.0 = 0.6674. With ε>=0.46 and suppression>=0.60, this is a Snare.
constraint_indexing:constraint_classification(tsp_computational_complexity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. For a large corporation, the problem's difficulty
% is a competitive moat that they have the resources to manage effectively.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% χ = 0.47 * -0.12 * 1.2 = -0.067. This is a clear Rope.
constraint_indexing:constraint_classification(tsp_computational_complexity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the extraction and the coordination
% function (the market for heuristics).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.47 * 1.15 * 1.2 = 0.6486. This is < 0.66, so it is not a snare.
% It meets all criteria for a Tangled Rope.
constraint_indexing:constraint_classification(tsp_computational_complexity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsp_computational_complexity_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(tsp_computational_complexity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tsp_computational_complexity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget = snare,
    TypeBeneficiary = rope.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsp_computational_complexity, ExtMetricName, E),
    (E =< 0.25 -> false ; E >= 0.46). % Must be high-extraction.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(tsp_computational_complexity, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(tsp_computational_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.47) is set just above the Snare threshold to capture the
 *   significant cost imposed by computational intractability. Suppression (0.60) reflects
 *   the high difficulty of finding optimal solutions, forcing reliance on a limited set
 *   of heuristic alternatives. The constraint is "enforced" by the mathematical reality
 *   of NP-hardness, which is a persistent, non-human force, justifying the
 *   `requires_active_enforcement` flag for the Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a powerless actor (e.g., a small delivery company), the problem
 *   is a Snare: they are trapped by high computational costs or forced into sub-optimal,
 *   costly manual routing. For an institutional actor (e.g., FedEx), the problem's
 *   difficulty is a feature, not a bug. It creates a competitive moat, making their
 *   massive investment in operations research a source of market power. They see it as a
 *   Rope that coordinates market structure to their benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: Large corporations and tech giants leverage their capital and technical
 *     prowess to solve TSP at a scale smaller competitors cannot, creating a barrier to entry.
 *     Algorithm developers also benefit from the persistent demand for better heuristics.
 *   - Victims: Small-scale operators who lack the resources to license high-end optimization
 *     software or develop in-house solutions bear the direct costs of inefficiency.
 *
 * MANDATROPHY ANALYSIS:
 *   The analytical classification as a Tangled Rope correctly identifies that this is not
 *   a pure, unchangeable Mountain of physics nor a simple Snare. The existence of a vibrant
 *   market for approximation algorithms is a genuine coordination function that provides
 *   workable, if imperfect, solutions. The framework captures both this coordination
 *   and the asymmetric extraction imposed on those who cannot access its best solutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_tsp_computational_complexity,
    'Will quantum computing fundamentally alter the complexity class of NP-hard problems like TSP, specifically whether P=NP?',
    'A formal proof of P vs NP, or the development of a quantum algorithm that solves TSP in polynomial time.',
    'If P=NP is proven, this constraint collapses from a Tangled Rope/Snare into a simple Rope. If P!=NP is proven, it solidifies into a Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(tsp_computational_complexity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As ε > 0.46, this is required.
% The extractiveness has slowly increased as the scale of global logistics
% has made optimal routing more critical and valuable.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(tsp_computational_complexity_tr_t0, tsp_computational_complexity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsp_computational_complexity_tr_t5, tsp_computational_complexity, theater_ratio, 5, 0.10).
narrative_ontology:measurement(tsp_computational_complexity_tr_t10, tsp_computational_complexity, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(tsp_computational_complexity_ex_t0, tsp_computational_complexity, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(tsp_computational_complexity_ex_t5, tsp_computational_complexity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(tsp_computational_complexity_ex_t10, tsp_computational_complexity, base_extractiveness, 10, 0.47).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The development of heuristics and solvers acts as an information standard.
narrative_ontology:coordination_type(tsp_computational_complexity, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately models the directionality for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */