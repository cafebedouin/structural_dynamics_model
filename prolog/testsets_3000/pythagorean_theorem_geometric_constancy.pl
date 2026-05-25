% ============================================================================
% CONSTRAINT STORY: pythagorean_theorem_geometric_constancy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pythagorean_theorem_geometric_constancy, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pythagorean_theorem_geometric_constancy
 *   human_readable: The Pythagorean Theorem — Euclidean Geometric Constancy
 *   domain: mathematical/geometric
 *
 * SUMMARY:
 *   The Pythagorean Theorem represents a mathematical constraint of maximal
 *   invariance: a^2 + b^2 = c^2 holds identically across all right triangles
 *   in Euclidean space without exception, variation, or negotiation. This
 *   constraint exemplifies the Mountain classification because it emerges
 *   necessarily from the axiomatic structure of Euclidean geometry, is
 *   accessible to independent verification by any agent capable of
 *   construction or proof, and exhibits zero degrees of freedom for all
 *   structural positions. No agent — regardless of power, time horizon, exit
 *   options, or spatial scope — can alter the theorem's truth within
 *   Euclidean geometry. There are no beneficiaries or victims; no extraction
 *   occurs; no suppression is required because denial is structurally
 *   impossible. The constraint's theater_ratio (0.15) reflects minimal
 *   performative content: a right triangle either satisfies the equation or
 *   it does not. Verification is direct and replicable. The constraint has
 *   persisted unchanged for over 2,500 years across all cultures and
 *   mathematical traditions that adopt Euclidean axioms, supporting its
 *   classification as a natural law of geometry.
 *
 * KEY AGENTS:
 *   - Practical Surveyor: Individual/powerless (trapped, universal scope) — experiences the constraint as immutable law of spatial construction with zero exit options
 *   - Applied Mathematician: Powerful/specialized (constrained, global scope) — uses the theorem as a foundation for computational models; cannot build Euclidean systems where it fails
 *   - Analytical Observer: Analytical intelligence (analytical, universal scope) — perceives the theorem as a logical necessity emerging from Euclidean axioms
 *   - Geometry Curriculum Designer: Organized institution (mobile, continental scope) — must include the theorem in any curriculum claiming to teach Euclidean geometry; cannot exit without teaching a different geometry
 *   - Student Learner: Moderate individual (mobile, local scope) — can verify the theorem through proof and construction; accessibility is maximal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pythagorean_theorem_geometric_constancy, 0.08).
domain_priors:suppression_score(pythagorean_theorem_geometric_constancy, 0.02).
domain_priors:theater_ratio(pythagorean_theorem_geometric_constancy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pythagorean_theorem_geometric_constancy, extractiveness, 0.08).
narrative_ontology:constraint_metric(pythagorean_theorem_geometric_constancy, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(pythagorean_theorem_geometric_constancy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pythagorean_theorem_geometric_constancy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(pythagorean_theorem_geometric_constancy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pythagorean_theorem_geometric_constancy, mountain).
narrative_ontology:human_readable(pythagorean_theorem_geometric_constancy, "The Pythagorean Theorem — Euclidean Geometric Constancy").
narrative_ontology:topic_domain(pythagorean_theorem_geometric_constancy, "mathematical/geometric").

domain_priors:emerges_naturally(pythagorean_theorem_geometric_constancy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICAL SURVEYOR (MOUNTAIN) — A builder, engineer, or surveyor attempting to construct a right angle or verify perpendicularity in the physical world has no exit from the constraint. The geometric relationship holds with zero degrees of freedom. Any attempt to construct right triangles in Euclidean space will satisfy a^2 + b^2 = c^2 without exception or negotiation. The constraint is experienced as an immutable law of spatial construction.
constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED MATHEMATICIAN (MOUNTAIN) — A physicist, engineer, or computational modeler using Euclidean geometry as a modeling framework experiences the theorem as a non-negotiable constraint on their calculations. Even with significant resources and mathematical sophistication, one cannot build a Euclidean geometry where the theorem fails. The constraint persists across all standard mathematical treatments and implementations.
constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational and universal scope, the Pythagorean Theorem is a logical consequence of Euclidean geometry's axioms. It is not a law that could be violated or reformed through different coordination arrangements, institutional structures, or social choices. The theorem holds with necessity across all contexts where the underlying geometric axioms are assumed. Zero degrees of freedom; universal accessibility to the truth; no suppression mechanism required because denial is structurally impossible.
constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: CURRICULUM DESIGNER (MOUNTAIN) — An educational institution designing geometry curricula faces a constraint: any geometry course that claims to teach Euclidean geometry must include the Pythagorean Theorem as a verifiable fact. One cannot 'teach Euclidean geometry' while denying the theorem — that would be teaching a different geometry (hyperbolic, spherical, or non-Euclidean). The constraint is immutable within the chosen geometric framework.
constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: STUDENT LEARNER (MOUNTAIN) — A student learning Euclidean geometry can verify the theorem through construction, proof, and calculation. The constraint is not suppressible through institutional pressure, economic incentive, or political mandate. A student can exit ignorance through engagement with the proof, but cannot exit the mathematical truth itself. The accessibility of the constraint (verifiable through multiple independent methods) and resistance to denial (zero degrees of freedom) are maximal.
constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pythagorean_theorem_geometric_constancy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pythagorean_theorem_geometric_constancy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(pythagorean_theorem_geometric_constancy, ExtMetricName, E),
    domain_priors:suppression_score(pythagorean_theorem_geometric_constancy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(pythagorean_theorem_geometric_constancy),
    narrative_ontology:constraint_metric(pythagorean_theorem_geometric_constancy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(pythagorean_theorem_geometric_constancy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(pythagorean_theorem_geometric_constancy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The Pythagorean Theorem does not extract value from any agent in the service of another. It does not concentrate resources, create asymmetric power, or require coercion. Its truth is equally accessible to all agents. The small non-zero value reflects the minimal resource cost of learning and verifying the theorem, not extraction. Suppression (0.02): Negligible. The theorem cannot be suppressed because its truth is verifiable through multiple independent methods (geometric construction, algebraic proof, coordinate geometry, trigonometric derivation). Active denial is structurally implausible. Theater ratio (0.15): Low. Verification of the Pythagorean Theorem is direct and replicable. Any agent can construct a right triangle and measure side lengths; the constraint either holds or it does not. No performative activity is required. The small value reflects the minimal pedagogical theater in teaching the theorem, not the theorem itself.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All five perspectives classify the constraint identically as Mountain. The Practical Surveyor, Applied Mathematician, Analytical Observer, Curriculum Designer, and Student Learner all experience zero degrees of freedom. This invariance across all structural positions is the defining signature of a natural law. The constraint does not depend on the observer's power level, exit options, or spatial scope — it persists as a mathematical necessity. This uniformity confirms the Mountain classification and demonstrates that the constraint is not contingent on institutional arrangements, coordination mechanisms, or social choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to this constraint because there are no beneficiaries or victims. The Pythagorean Theorem does not extract from any agent for the benefit of another. All agents experience the same immutable mathematical relationship. The theorem's d value (directionality) is structurally undefined — there is no 'target' or 'beneficiary' in the constraint's operation. This absence of directionality is a critical signature of the Mountain classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    euclidean_axiom_selection,
    'Does the Pythagorean Theorem emerge as a consequence of Euclidean geometric axioms, or is the theorem itself a foundational axiom that constitutes what ''Euclidean geometry'' means?',
    'Axiomatic analysis of Euclidean geometry (Hilbert''s axioms, Euclid''s postulates, coordinate geometry definitions) to establish the logical dependency chain',
    'If theorem is a consequence: the constraint''s necessity flows from the parallel postulate. If theorem is foundational: the constraint''s necessity is conventional (we define Euclidean geometry by this property). Both outcomes preserve the Mountain classification but with different philosophical grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(euclidean_axiom_selection, conceptual, 'Whether the theorem is a consequence or foundation of Euclidean geometry').

omega_variable(
    physical_space_euclidicity,
    'Is physical space (spacetime, the universe at human scales, or cosmological scales) actually Euclidean, or is the theorem merely an accurate approximation in some regimes?',
    'Physical measurements of geometric properties at cosmological, planetary, and laboratory scales; comparison of observed geometric relationships against Pythagorean predictions; analysis of spacetime curvature in relativity',
    'If physical space is Euclidean: the theorem describes actual physical law and the Mountain classification includes physical necessity. If space is non-Euclidean: the theorem is a mathematical law (consequence of chosen axioms) but not a universal physical law — still Mountain in mathematics, but analytically distinct from physical law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_space_euclidicity, empirical, 'Whether physical space satisfies Euclidean geometry').

omega_variable(
    alternative_geometry_access,
    'For practical surveying, construction, and engineering at human scales, is the accessibility and verifiability of hyperbolic or spherical geometry comparable to Euclidean geometry?',
    'Comparative analysis of measurement protocols, calculation complexity, and empirical verification effort across Euclidean, hyperbolic, and spherical models for human-scale applications',
    'If alternatives are equally accessible: the Pythagorean Theorem''s dominance is path-dependent (historical convention, ease of computation), not inherent necessity. If Euclidean is uniquely accessible at human scales: the Mountain classification gains empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_geometry_access, empirical, 'Comparative accessibility of alternative geometric models at human scales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pythagorean_theorem_geometric_constancy, 0, 4000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pyth_tr_t0, pythagorean_theorem_geometric_constancy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pyth_tr_t2000, pythagorean_theorem_geometric_constancy, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(pyth_tr_t4000, pythagorean_theorem_geometric_constancy, theater_ratio, 4000, 0.15).

% Extraction over time
narrative_ontology:measurement(pyth_be_t0, pythagorean_theorem_geometric_constancy, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(pyth_be_t2000, pythagorean_theorem_geometric_constancy, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(pyth_be_t4000, pythagorean_theorem_geometric_constancy, base_extractiveness, 4000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pythagorean_theorem_geometric_constancy, information_standard).
narrative_ontology:affects_constraint(pythagorean_theorem_geometric_constancy, euclidean_geometry_completeness).
narrative_ontology:affects_constraint(pythagorean_theorem_geometric_constancy, right_angle_construction_constraint).

% DUAL FORMULATION NOTE:
% The Pythagorean Theorem is part of a larger constraint family in Euclidean geometry. The upstream constraint is the axiomatic structure of Euclidean geometry (Hilbert's axioms, the parallel postulate); the downstream constraints are specific applications (construction techniques, distance metrics, coordinate transformations). This story focuses on the theorem as a geometric fact independent of its proof methods or applications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
