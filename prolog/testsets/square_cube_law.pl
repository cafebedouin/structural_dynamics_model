% ============================================================================
% CONSTRAINT STORY: square_cube_law
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-15
% ============================================================================

:- module(constraint_square_cube_law, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: square_cube_law
 *   human_readable: The Square-Cube Law
 *   domain: technological/biological
 *
 * SUMMARY:
 *   The Square-Cube Law, a principle of geometry, states that as an object
 *   grows in size, its surface area increases by the square of the multiplier,
 *   while its volume (and mass) increases by the cube. This represents a
 *   fundamental scaling limit where properties dependent on surface area
 *   (like structural strength or heat dissipation) fail to keep pace with
 *   properties dependent on volume (like weight or heat generation). It is an
 *   immutable feature of three-dimensional space.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mega-Fauna / Giant: Primary target (powerless/trapped) — physical integrity is bound by the law.
 *   - Aircraft Designer / Engineer: Institutional actor (institutional/mobile) — must operate within the fixed boundaries of the law.
 *   - Evolutionary Biologist: Analytical observer (analytical/analytical) — maps the physical limits on animal size.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a fundamental law of geometry, the square-cube law has near-zero
% extractiveness and suppression. It doesn't "extract" value in a socio-economic
% sense; it defines the fixed, neutral landscape of physical possibility.
domain_priors:base_extractiveness(square_cube_law, 0.01).
domain_priors:suppression_score(square_cube_law, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(square_cube_law, 0.0).       % Piton detection (>= 0.70). A physical law has no performative aspect.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(square_cube_law, extractiveness, 0.01).
narrative_ontology:constraint_metric(square_cube_law, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(square_cube_law, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(square_cube_law, accessibility_collapse, 1.0). % No alternative is conceivable.
narrative_ontology:constraint_metric(square_cube_law, resistance, 0.0). % Active resistance is incoherent.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(square_cube_law, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the geometry of 3D space without
% human design or enforcement. Required for the mountain metric gate.
domain_priors:emerges_naturally(square_cube_law).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Not applicable. As a Mountain constraint representing a physical law, there
% are no socio-economic beneficiaries or victims. No enrichment needed.

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

% UNIFORM-TYPE CONSTRAINT: As a natural law, this constraint is a Mountain
% from all perspectives. The classification is invariant. We include multiple
% perspectives to demonstrate this invariance.

% PERSPECTIVE 1: THE POWERLESS SUBJECT (GIANT)
% Experiences the law as an absolute physical limit on their existence.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL ACTOR (ENGINEER)
% Views the law as a fixed boundary condition for design and construction.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile), % Can choose different materials, but not a different law.
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (BIOLOGIST)
% Views the law as an unchangeable feature of the universe's geometric "hardware."
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(square_cube_law_tests).

test(perspectival_invariance) :-
    % Verify that the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(square_cube_law, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(square_cube_law, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(square_cube_law, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation_mountain) :-
    % Verify the metrics are within the canonical range for a Mountain.
    narrative_ontology:constraint_metric(square_cube_law, extractiveness, E),
    narrative_ontology:constraint_metric(square_cube_law, suppression_requirement, S),
    narrative_ontology:constraint_metric(square_cube_law, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(square_cube_law, resistance, R),
    E =< 0.25,
    S =< 0.05,
    AC >= 0.85,
    R =< 0.15.

:- end_tests(square_cube_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This file models a canonical Mountain: a physical/mathematical law. The
 *   scores are near-zero (ε=0.01, suppression=0.01) because a physical law is
 *   not extractive; it is a neutral, fixed feature of reality. The consequences
 *   of the law (e.g., a giant's bones breaking) are distinct from the nature
 *   of the law itself.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The law is a Mountain for the powerless
 *   giant, the institutional engineer, and the analytical biologist alike.
 *   Their *strategies* for dealing with the law differ, but their
 *   classification of the law itself is uniform. This demonstrates the
 *   "uniform-type" exception for natural laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. As a Mountain constraint, there are no beneficiaries or
 *   victims in a socio-economic sense, so directionality (d) is not derived
 *   from structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not applicable here. As a pure Mountain with near-zero
 *   extraction, there is no coordination function to degrade or asymmetric
 *   extraction to disguise. The constraint's claim of being a `mountain` is
 *   verifiably true, posing no risk of misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_square_cube_law,
    'Is the law a feature of spacetime geometry itself, or an emergent property of particle interactions within that geometry?',
    'A unified theory of quantum gravity or experimental evidence of non-Euclidean geometry at micro-scales.',
    'If geometry itself, the Mountain is absolute. If emergent, there may be exotic physics (e.g., inside a neutron star) where the law is bypassed, changing its scope from universal to conditional.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(square_cube_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.01) is below the 0.46 threshold for
% mandatory temporal tracking. As a physical law, its properties do not drift.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No coordination function is present, so coordination_type is not applicable.
% No known structural dependencies on other constraints in the corpus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */