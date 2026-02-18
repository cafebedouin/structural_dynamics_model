% ============================================================================
% CONSTRAINT STORY: gradient_descent_optimization
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_gradient_descent_optimization, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gradient_descent_optimization
 *   human_readable: Gradient Descent Iterative Optimization
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   Gradient Descent is a first-order iterative optimization algorithm for finding
 *   a local minimum of a differentiable function. It functions as a fundamental
 *   tool in machine learning, but its behavior is highly dependent on the
 *   topology of the "loss landscape" it traverses. The constraint models the
 *   rules of this traversal.
 *
 * KEY AGENTS (by structural relationship):
 *   - Parameter Vector (theta): Primary target (powerless/trapped) — its state is dictated by the update rule.
 *   - ML Practitioner: Primary beneficiary (institutional/mobile) — uses the algorithm as a tool to achieve a goal.
 *   - Hardware & Energy Budgets: Secondary victims (powerless/trapped) — bear the computational cost.
 *   - Analytical Observer: Analytical observer — sees the full mathematical structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: It extracts "computational labor" (FLOPs). While fundamental,
% the cost of convergence is a direct extraction from the system's energy.
domain_priors:base_extractiveness(gradient_descent_optimization, 0.30).

% Rationale: Low. While GD is the "default," second-order methods (Newton)
% or evolutionary strategies are highly visible alternatives.
domain_priors:suppression_score(gradient_descent_optimization, 0.20).

% Rationale: The algorithm is almost entirely functional, with little to no
% performative aspect. It is a pure coordination mechanism for finding a minimum.
domain_priors:theater_ratio(gradient_descent_optimization, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gradient_descent_optimization, extractiveness, 0.30).
narrative_ontology:constraint_metric(gradient_descent_optimization, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(gradient_descent_optimization, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These are included because one perspective classifies as Mountain. They model
% the unchangeable nature of the mathematical laws for the parameter vector.
narrative_ontology:constraint_metric(gradient_descent_optimization, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(gradient_descent_optimization, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(gradient_descent_optimization, rope).
narrative_ontology:human_readable(gradient_descent_optimization, "Gradient Descent Iterative Optimization").
narrative_ontology:topic_domain(gradient_descent_optimization, "technological/mathematical").

% --- Emergence flag (required for mountain constraints) ---
% The update rule emerges from the laws of multivariate calculus.
domain_priors:emerges_naturally(gradient_descent_optimization).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(gradient_descent_optimization, ml_practitioners).
narrative_ontology:constraint_beneficiary(gradient_descent_optimization, automation_systems).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(gradient_descent_optimization, hardware_longevity).
narrative_ontology:constraint_victim(gradient_descent_optimization, energy_budgets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PARAMETER VECTOR (theta)
% For the parameters being optimized, the update rule is a natural law.
% The iterate cannot "choose" to go uphill or sidestep a saddle point.
% The landscape is its absolute, unchangeable reality.
% NOTE: The Mountain classification reflects the agent's perspective of
% unchangeability. The engine will likely reject this classification due to
% ε=0.30 > 0.25, producing a false_natural_law signature, which is the
% intended diagnostic result here.
constraint_indexing:constraint_classification(gradient_descent_optimization, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ML PRACTITIONER
% For the engineer, GD is a "Rope"—a tool for functional coordination.
% By adjusting the "tension" (learning rate), they guide the model toward
% the desired state (low error). They have high agency and exit options.
constraint_indexing:constraint_classification(gradient_descent_optimization, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view sees a functional coordination tool with moderate,
% non-coercive extraction (computational cost), classifying it as a Rope.
% This matches the constraint_claim.
constraint_indexing:constraint_classification(gradient_descent_optimization, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: VANISHING GRADIENTS / FLAT PLATEAUS
% In a flat plateau, the algorithm extracts massive computational power for
% no progress. The iterate is trapped in a loop that leads nowhere.
% This represents a failure mode where the tool becomes purely extractive.
constraint_indexing:constraint_classification(gradient_descent_optimization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gradient_descent_optimization_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (powerless) and beneficiary (institutional).
    constraint_indexing:constraint_classification(gradient_descent_optimization, TypeTarget, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(gradient_descent_optimization, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    % Verify that the base metrics are within a plausible, non-mountain range.
    narrative_ontology:constraint_metric(gradient_descent_optimization, extractiveness, E),
    narrative_ontology:constraint_metric(gradient_descent_optimization, suppression_requirement, S),
    E > 0.25,
    S > 0.05.

test(emergence) :-
    domain_priors:emerges_naturally(gradient_descent_optimization).

:- end_tests(gradient_descent_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.30): Represents the non-trivial computational cost (energy, hardware cycles) required to use the algorithm. It's not free, but it's not designed to be punitive.
 *   - Suppression (0.20): Low, as many alternative optimizers exist (Adam, RMSProp, second-order methods), though GD remains a common baseline.
 *   - Mountain Perspective: The core tension in this story is the dual nature of the constraint. For the parameter vector being updated, the laws of calculus are absolute and unchangeable (a Mountain). However, the algorithm itself, as a system, has an extractive cost (ε=0.30) that is too high for a true Mountain. This file models the *perceived* unchangeability for the powerless agent, while allowing the system's `false_natural_law` detector to correctly flag the metric inconsistency.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The ML practitioner sees a flexible Rope they can tune (learning rate) or replace. The parameter vector sees an immutable Mountain whose topology dictates its every move. In failure modes (vanishing gradients), this same immutable law becomes a Snare, extracting resources for no benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `ml_practitioners` directly benefit by using this tool to solve optimization problems.
 *   - Victims: `hardware_longevity` and `energy_budgets` bear the direct physical cost of the computation, representing the extractive component of the algorithm.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies GD as primarily a coordination tool (Rope) from the user and analytical perspectives. It avoids mislabeling the computational cost as a purely extractive Snare (in its normal function) while still capturing the Snare-like failure mode in specific topological contexts (flat plateaus). The Mountain perspective highlights how a natural law can be the basis for a tool with non-zero extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_gradient_descent_1,
    'Is the loss landscape convex (reliable convergence) or non-convex (many local minima)?',
    'Formal verification of the loss function''s second derivative matrix (Hessian).',
    'If Convex: GD is a reliable Rope. If Non-Convex: GD risks becoming a Snare of local minima.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_gradient_descent_1, empirical, 'Whether the specific loss landscape is convex, determining if local minima are a trap.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gradient_descent_optimization, 1847, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.30) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No specific Boltzmann or Network data declared for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% groups is sufficient to model the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */