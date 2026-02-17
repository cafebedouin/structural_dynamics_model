% ============================================================================
% CONSTRAINT STORY: three_body_unpredictability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_three_body_unpredictability, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: three_body_unpredictability
 *   human_readable: The Three-Body Problem (Computational Irreducibility)
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   The Three-Body Problem describes the motion of three celestial bodies
 *   interacting via gravity. Unlike the stable two-body system, the general
 *   three-body system lacks a closed-form solution and exhibits chaotic
 *   dynamics (extreme sensitivity to initial conditions). This renders
 *   long-term trajectories fundamentally uncomputable, establishing a hard
 *   limit on celestial prediction. This constraint story models this
 *   computational irreducibility as a natural law, or Mountain.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Celestial Body (Subject): A powerless agent whose path is dictated by
 *     the unyielding laws of gravity.
 *   - The Orbital Engineer (Observer): An agent who must work within the
 *     confines of this unchangeable physical law, for example by exploiting
 *     rare stable configurations (Lagrange points).
 *   - The Long-Term Navigator (Observer): An agent for whom the chaotic
 *     divergence is a fixed, unavoidable feature of the physical landscape.
 *   - The Analytical Observer: Sees the full structure as a fundamental limit
 *     of dynamical systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The problem is a fundamental limit of physics and computation, not
% a socio-economic system that extracts value. The low scores reflect its
% status as a natural law (Mountain). It doesn't suppress alternatives; there
% are no alternatives to gravity.
domain_priors:base_extractiveness(three_body_unpredictability, 0.05).
domain_priors:suppression_score(three_body_unpredictability, 0.02).
domain_priors:theater_ratio(three_body_unpredictability, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(three_body_unpredictability, extractiveness, 0.05).
narrative_ontology:constraint_metric(three_body_unpredictability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(three_body_unpredictability, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
% Accessibility Collapse: Alternatives to gravity are inconceivable.
narrative_ontology:constraint_metric(three_body_unpredictability, accessibility_collapse, 1.0).
% Resistance: Meaningful resistance to a law of physics is incoherent.
narrative_ontology:constraint_metric(three_body_unpredictability, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(three_body_unpredictability, mountain).
narrative_ontology:human_readable(three_body_unpredictability, "The Three-Body Problem (Computational Irreducibility)").

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges from the natural laws of physics without human design.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(three_body_unpredictability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain (natural law), this constraint has no inherent beneficiaries
% or victims in a structural sense. Agents are affected by it, but do not
% stand in a beneficiary/victim relationship TO it. No enrichment needed.

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

% PERSPECTIVE 1: THE PLANET IN A TRIPLE-STAR SYSTEM
% For the planet, the gravitational pull is an absolute and unchangeable
% physical law. Its chaotic trajectory is a fixed feature of its existence.
constraint_indexing:constraint_classification(three_body_unpredictability, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SATELLITE ENGINEER
% For the engineer, the laws governing the system are still a Mountain. They
% cannot change the laws of gravity. Instead, they map the "terrain" of this
% Mountain to find rare points of stability (Lagrange points). These stable
% points are features OF the Mountain, not a different constraint type.
constraint_indexing:constraint_classification(three_body_unpredictability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile), % Mobile in choosing where to place assets
            spatial_scope(national))).

% PERSPECTIVE 3: THE LONG-TERM NAVIGATOR
% For a navigator planning a multi-century journey, the chaotic nature of
% n-body interactions is an unchangeable feature of the universe. It's a
% fundamental hazard of the environment, like radiation or the speed of light.
% It is a Mountain that must be navigated around, not a Snare that can be
% dismantled.
constraint_indexing:constraint_classification(three_body_unpredictability, mountain,
    context(agent_power(moderate),
            time_horizon(historical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical view sees the problem as a canonical example of deterministic
% chaos in dynamical systems—a fundamental feature of mathematics and physics.
constraint_indexing:constraint_classification(three_body_unpredictability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(three_body_unpredictability_tests).

test(perspectival_invariance) :-
    % Verify that as a Mountain, the classification is invariant across perspectives.
    constraint_indexing:constraint_classification(three_body_unpredictability, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(three_body_unpredictability, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(three_body_unpredictability, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type1 == Type2,
    Type2 == Type3.

test(threshold_validation) :-
    % Verify that the base metrics adhere to the Mountain classification thresholds.
    narrative_ontology:constraint_metric(three_body_unpredictability, extractiveness, E),
    narrative_ontology:constraint_metric(three_body_unpredictability, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(three_body_unpredictability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores (ε=0.05, suppression=0.02) are consistent with a natural law.
 *   The three-body problem does not "extract" value in a socio-economic sense;
 *   it is a fundamental, computationally irreducible feature of the universe.
 *   Its effects (e.g., computational cost for navigation) are consequences of
 *   the law, not properties of the law itself. The added NL Profile metrics
 *   (accessibility_collapse=1.0, resistance=0.0) and the `emerges_naturally`
 *   flag formally certify this as a Mountain for the classification engine.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a Mountain (natural law), the
 *   classification is invariant across all possible indices. An engineer
 *   exploiting Lagrange points and a navigator thwarted by chaos are both
 *   interacting with the same unchangeable physical reality. Their goals
 *   differ, but their classification of the underlying law does not.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint lacks inherent directionality. It has no
 *   beneficiaries or victims. Therefore, no `constraint_beneficiary` or
 *   `constraint_victim` facts are declared.
 *
 * MANDATROPHY ANALYSIS:
 *   This story exemplifies the "false natural law" detection capability. If
 *   the metrics were higher, the engine would classify this as a Tangled Rope
 *   or Snare from some perspectives, creating a coupling violation with the
 *   `mountain` claim. By setting metrics that align with physical reality and
 *   providing the full NL profile, the story correctly models a fundamental
 *   limit, preventing the mislabeling of physics as a socio-economic
 *   extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_three_body_unpredictability,
    "Are there significant, undiscovered 'islands of stability' (e.g., families of periodic orbits) within the general three-body problem?",
    "Large-scale numerical searches for periodic solutions and quasi-periodic regions in phase space.",
    "If Yes: The 'Mountain' has more predictable regions, enabling new stable orbits (potential for new Rope constraints). If No: The chaotic nature is confirmed to be near-total.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(three_body_unpredictability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a Mountain representing a timeless physical law, this constraint does not
% exhibit lifecycle drift. Its core metrics are constant. Temporal measurements
% are not required for low-extraction constraints (base_extractiveness <= 0.46).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% --- Network Decomposition (Constraint Families) ---
% The colloquial label "The Three-Body Problem" can be decomposed into
% multiple, structurally distinct constraints per the ε-invariance principle.
% This story models the core computational irreducibility (a Mountain).
% Other related constraints could be modeled separately.
%
% DUAL FORMULATION NOTE:
% This constraint is one of several potential stories decomposed from "The Three-Body Problem".
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories could include:
%   - lagrange_point_stability (ε≈0.10, Rope) - modeling the coordination function of stable points.
%   - interstellar_navigation_cost (ε≈0.45, Tangled Rope) - modeling the fuel/compute cost.
%
% narrative_ontology:affects_constraint(three_body_unpredictability, lagrange_point_stability).
% narrative_ontology:affects_constraint(three_body_unpredictability, interstellar_navigation_cost).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a Mountain, this constraint has no inherent
% directionality, and no beneficiary/victim groups are declared.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */