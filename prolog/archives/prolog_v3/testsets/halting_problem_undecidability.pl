% ============================================================================
% CONSTRAINT STORY: halting_problem_undecidability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_halting_problem_undecidability, []).

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
 *   constraint_id: halting_problem_undecidability
 *   human_readable: The Undecidability of the Halting Problem
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   The Halting Problem is the mathematical proof that no general algorithm
 *   can exist that decides, for all possible inputs, whether an arbitrary
 *   program will finish running or continue to run forever. It represents a
 *   fundamental, unchangeable limit on what is knowable via computation,
 *   establishing an irreducible horizon for automated reasoning and formal
 *   verification. It is a canonical example of a Mountain constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - Software Engineer (moderate/constrained): Experiences the constraint as a
 *     practical barrier to perfect automated verification, but is not a
 *     structural victim of extraction.
 *   - Theoretical Computer Scientist (analytical/analytical): Perceives the
 *     constraint as a foundational law defining the limits of computability.
 *   - Executing Program (powerless/trapped): Subject to the law without any
 *     capacity to observe or alter it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(halting_problem_undecidability, 0.05).
domain_priors:suppression_score(halting_problem_undecidability, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(halting_problem_undecidability, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(halting_problem_undecidability, extractiveness, 0.05).
narrative_ontology:constraint_metric(halting_problem_undecidability, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(halting_problem_undecidability, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(halting_problem_undecidability, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(halting_problem_undecidability, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(halting_problem_undecidability, mountain).
narrative_ontology:human_readable(halting_problem_undecidability, "The Undecidability of the Halting Problem").
narrative_ontology:topic_domain(halting_problem_undecidability, "mathematical/technological").

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the logic of self-reference in
% any Turing-complete system. It requires no human design or enforcement.
domain_priors:emerges_naturally(halting_problem_undecidability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain constraint (a feature of mathematical
% reality), the Halting Problem does not have structural beneficiaries or
% victims in the sense of asymmetric extraction. Its effects are universal
% within its domain.

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

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because it is a fundamental logical limit.
% We include multiple perspectives to demonstrate this invariance.

% PERSPECTIVE 1: THE EXECUTING PROGRAM
% For an arbitrary program, its halt status being undecidable by a general
% algorithm is an unchangeable feature of its computational universe.
constraint_indexing:constraint_classification(halting_problem_undecidability, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SOFTWARE ENGINEER
% For an engineer building high-assurance systems, the inability to create a
% perfect, general-purpose static analyzer is a fixed boundary condition.
constraint_indexing:constraint_classification(halting_problem_undecidability, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE THEORETICAL COMPUTER SCIENTIST (ANALYTICAL OBSERVER)
% For a theorist, the undecidability is a foundational law of computation,
% a fixed point from which other proofs of undecidability are derived.
constraint_indexing:constraint_classification(halting_problem_undecidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(halting_problem_undecidability_tests).

test(perspectival_invariance) :-
    % Verify that the classification is Mountain from different perspectives.
    constraint_indexing:constraint_classification(halting_problem_undecidability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(halting_problem_undecidability, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeAnalytical == mountain.

test(mountain_thresholds_adherence) :-
    % Verify that the base metrics are within the required range for a Mountain.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    config:param(mountain_extractiveness_max, ExtMax),
    config:param(mountain_suppression_ceiling, SuppMax),
    narrative_ontology:constraint_metric(halting_problem_undecidability, ExtMetricName, E),
    narrative_ontology:constraint_metric(halting_problem_undecidability, SuppMetricName, S),
    E =< ExtMax,
    S =< SuppMax.

test(natural_law_profile_present) :-
    % Verify the constraint has the required metrics for NL certification.
    narrative_ontology:constraint_metric(halting_problem_undecidability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(halting_problem_undecidability, resistance, R),
    domain_priors:emerges_naturally(halting_problem_undecidability),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(halting_problem_undecidability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Halting Problem is a canonical example of a Mountain constraint. Its
 *   base extractiveness (0.05) is minimal, reflecting that it is a feature of
 *   reality, not a system designed for extraction. The "cost" it imposes on
 *   engineers is a consequence of its existence, not its function. The
 *   suppression score (0.01) is near zero because it does not coercively
 *   suppress alternatives; it logically proves them to be impossible within
 *   its axiomatic system. The Natural Law profile metrics are set to their
 *   extremes (accessibility_collapse=1.0, resistance=0.0) because no
 *   alternative is conceivable and no meaningful resistance is possible.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap; the classification is Mountain from all
 *   viewpoints. While a software engineer might *experience* the constraint
 *   as a frustrating barrier (metaphorically, a "snare" on their ambition for
 *   perfect verification), its *structure* remains that of an unchangeable
 *   natural law. The framework correctly distinguishes between subjective
 *   experience and objective structure, classifying it as a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint has no structural beneficiaries or victims.
 *   Directionality is not a relevant concept for a fundamental mathematical
 *   truth. The effects are symmetric and universal for all agents operating
 *   within Turing-complete systems.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain prevents misinterpretation. Labeling it
 *   a Snare because it frustrates engineers would be a category error,
 *   conflating a natural limit with a coercive, man-made extractive system.
 *   The framework's strict metric requirements for Mountain classification
 *   ensure that only true, unchangeable background conditions receive this label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_halting_problem_undecidability,
    'Is the undecidability of the Halting Problem an absolute feature of physical reality, or only of the abstract Turing Machine model?',
    'Empirical investigation into physical "hypercomputation" theories or evidence that the universe itself is not Turing-computable.',
    'If hypercomputation is physically possible, the constraint might be a Scaffold of our current technological paradigm. If not, it remains a permanent Mountain.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_halting_problem_undecidability, empirical, 'Whether physical reality permits computational models that transcend Turing limits (hypercomputation).').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(halting_problem_undecidability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for this constraint as base_extractiveness (0.05) is below the
% 0.46 threshold for mandatory lifecycle drift tracking. The metrics of a
% mathematical theorem are time-invariant.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is a foundational limit and does not have a coordination
% function or direct structural influence on other policy-based constraints
% in the corpus in the same way a regulation might.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a Mountain, directionality is not applicable.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */