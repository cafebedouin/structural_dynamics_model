% ============================================================================
% CONSTRAINT STORY: pythagorean_geometric_constancy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_pythagorean_geometric_constancy, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pythagorean_geometric_constancy
 *   human_readable: The Pythagorean Theorem
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   The Pythagorean Theorem states that in a right-angled triangle within a
 *   Euclidean (flat) space, the square of the hypotenuse is equal to the
 *   sum of the squares of the other two sides (a^2 + b^2 = c^2). This
 *   represents an immutable geometric constraint on the relationship between
 *   linear distance and orthogonal orientation. It is a foundational,
 *   unchangeable property of its axiomatic system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Student/Builder (powerless/moderate): Experiences the theorem as an
 *     unyielding natural law that must be obeyed to achieve stable outcomes.
 *   - Engineer/Architect (institutional): Uses the theorem as a perfect
 *     coordination device, but is still absolutely bound by its rules.
 *   - Analytical Observer (analytical): Perceives the theorem as an axiomatic
 *     consequence of Euclidean geometry, a pure Mountain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The theorem extracts nothing; it is a pure description of a
% geometric relationship. Any "extraction" comes from misapplying it to
% non-Euclidean spaces, which is a property of a different constraint.
domain_priors:base_extractiveness(pythagorean_geometric_constancy, 0.02).
% Rationale: Within Euclidean space, the theorem suppresses no alternatives;
% it is the only possibility. Its existence does not prevent the development
% of other geometries.
domain_priors:suppression_score(pythagorean_geometric_constancy, 0.01).
% Rationale: The theorem is pure function with zero performative theater.
domain_priors:theater_ratio(pythagorean_geometric_constancy, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pythagorean_geometric_constancy, extractiveness, 0.02).
narrative_ontology:constraint_metric(pythagorean_geometric_constancy, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(pythagorean_geometric_constancy, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Accessibility Collapse: Within Euclidean geometry, alternatives are not just
% inaccessible, they are axiomatically impossible.
narrative_ontology:constraint_metric(pythagorean_geometric_constancy, accessibility_collapse, 1.0).
% Resistance: Meaningful resistance is incoherent. One cannot "oppose" the
% theorem within its domain; one can only be incorrect.
narrative_ontology:constraint_metric(pythagorean_geometric_constancy, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pythagorean_geometric_constancy, mountain).
narrative_ontology:human_readable(pythagorean_geometric_constancy, "The Pythagorean Theorem").

% --- Emergence flag (required for mountain constraints) ---
% The theorem emerges naturally from the axioms of Euclidean geometry without
% human design or enforcement.
domain_priors:emerges_naturally(pythagorean_geometric_constancy).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% This is a uniform-type Mountain constraint. No enrichment needed.
% It is a property of the space itself, not a system imposed by one group
% on another.

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
% invariant across all perspectives because it is a natural law of its domain.

% PERSPECTIVE 1: THE STUDENT (MOUNTAIN)
% A powerless agent for whom the theorem is an absolute, unchangeable fact
% that must be learned and applied.
constraint_indexing:constraint_classification(pythagorean_geometric_constancy, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE ENGINEER (MOUNTAIN)
% An institutional agent who uses the theorem as a perfect coordination tool,
% but is still absolutely bound by its immutable logic.
constraint_indexing:constraint_classification(pythagorean_geometric_constancy, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical perspective recognizes the theorem as a fundamental,
% non-extractive, non-suppressive property of Euclidean space.
constraint_indexing:constraint_classification(pythagorean_geometric_constancy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pythagorean_geometric_constancy_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from multiple perspectives,
    % demonstrating its status as a uniform-type natural law.
    constraint_indexing:constraint_classification(pythagorean_geometric_constancy, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pythagorean_geometric_constancy, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(pythagorean_geometric_constancy, mountain, context(agent_power(analytical), _, _, _)).

test(natural_law_profile_compliance) :-
    % Verify the metrics required for the natural_law_signature certification.
    narrative_ontology:constraint_metric(pythagorean_geometric_constancy, accessibility_collapse, AC),
    AC >= 0.85,
    narrative_ontology:constraint_metric(pythagorean_geometric_constancy, resistance, R),
    R =< 0.15,
    domain_priors:emerges_naturally(pythagorean_geometric_constancy).

test(mountain_metric_thresholds) :-
    % Verify the core metrics are within the Mountain classification range.
    domain_priors:base_extractiveness(pythagorean_geometric_constancy, E),
    E =< 0.25,
    domain_priors:suppression_score(pythagorean_geometric_constancy, S),
    S =< 0.05.

:- end_tests(pythagorean_geometric_constancy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Pythagorean Theorem is a quintessential Mountain. Its extractiveness
 *   and suppression scores are near zero because, within its axiomatic domain
 *   (Euclidean space), it neither extracts value nor suppresses valid
 *   alternatives. It simply *is*. The classification is invariant across all
 *   perspectives, from a student to an engineer, because its truth is not
 *   contingent on power, time, or scope. The required Natural Law profile
 *   metrics (accessibility_collapse=1.0, resistance=0.0) and the
 *   `emerges_naturally` flag are included to ensure it passes the engine's
 *   most stringent certification for a natural law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a uniform-type constraint,
 *   classifying as Mountain from all valid indices. This invariance is the
 *   hallmark of a true natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint describing a property of space, there are no
 *   differentiated beneficiaries or victims. The concept is not applicable.
 *   The constraint is symmetric to all observers within its domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The original file conflated the theorem itself with the consequences of
 *   its misapplication (e.g., in spherical geometry), leading to an erroneous
 *   'Snare' classification. This version corrects that by adhering to the
 *   ε-invariance principle. The theorem is a Mountain. The error arising from
 *   applying it to a curved surface is a separate constraint, linked via
 *   `affects_constraint`. This prevents mislabeling a fundamental law as
 *   extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_pythagorean_geometric_constancy,
    'Is Euclidean geometry, and thus the Pythagorean theorem, a fundamental property of physical reality at all scales, or an emergent property of spacetime at low energy densities?',
    'Empirical tests of geometric relations at the Planck scale or within extreme gravitational fields.',
    'If fundamental, it is a true universal Mountain. If emergent, it is a local Mountain that is a component of a larger, more complex geometric structure (e.g., General Relativity).',
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_pythagorean_geometric_constancy, conceptual, 'Is Euclidean geometry a fundamental or emergent property of physical reality?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pythagorean_geometric_constancy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a Mountain constraint with base_extractiveness < 0.46.
% Temporal measurements are not required as there is no extractive lifecycle
% to track for drift.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% --- Network Decomposition (Constraint Families) ---
% The misapplication of this theorem in non-Euclidean contexts creates errors
% that can be modeled as a separate constraint.
%
% DUAL FORMULATION NOTE:
% This constraint is the foundational Mountain. The "Snare" experienced by
% surveyors on a curved Earth is a separate constraint representing the error
% from misapplication.
% Related stories:
%   - spherical_geometry_error (ε≈0.50, Snare)
%
narrative_ontology:affects_constraint(pythagorean_geometric_constancy, spherical_geometry_error).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a uniform-type Mountain, directionality is
% not a relevant concept.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */