% ============================================================================
% CONSTRAINT STORY: pythagorean_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pythagorean_theorem, []).

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
 *   constraint_id: pythagorean_theorem
 *   human_readable: Pythagorean Theorem
 *   domain: mathematics/euclidean_geometry
 *
 * SUMMARY:
 *   The Pythagorean theorem is a foundational constraint of Euclidean
 *   geometry: for any right triangle in Euclidean space, the square of the
 *   hypotenuse equals the sum of the squares of the other two sides (a² + b²
 *   = c²). This constraint is invariant across all observers, power
 *   structures, temporal horizons, and spatial scopes. It classifies as a
 *   mountain from every perspective because it emerges naturally from the
 *   axioms of Euclidean geometry and cannot be negotiated, suspended, or
 *   escaped by any agent. The theorem exhibits zero extractive content: no
 *   individual or institution benefits from its existence at the cost of
 *   others. No agent is suppressed or coerced by the relationship between
 *   triangle sides. The theorem is prior to human institutional action and
 *   independent of political or economic structure. It has been known,
 *   proven, and applied identically across cultures and centuries,
 *   demonstrating its status as a natural law of mathematical structure.
 *
 * KEY AGENTS:
 *   - Student: Encounters the theorem as an invariant constraint; no power to alter it
 *   - Analytical Observer: Derives the theorem from Cartesian metric structure; recognizes its necessity
 *   - Mathematical Institution: Maintains the theorem as a foundational invariant across educational systems
 *   - Engineering Discipline: Relies on the theorem as an absolute computational guarantee
 *   - Non-Euclidean Geometry: Boundary case showing the theorem is domain-specific to Euclidean space
 *   - Formal Axiomatic System: The theorem exists as a logical consequence of axioms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pythagorean_theorem, 0.08).
domain_priors:suppression_score(pythagorean_theorem, 0.02).
domain_priors:theater_ratio(pythagorean_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pythagorean_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(pythagorean_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(pythagorean_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pythagorean_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(pythagorean_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pythagorean_theorem, mountain).
narrative_ontology:human_readable(pythagorean_theorem, "Pythagorean Theorem").
narrative_ontology:topic_domain(pythagorean_theorem, "mathematics/euclidean_geometry").

domain_priors:emerges_naturally(pythagorean_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT (MOUNTAIN) — Encounters the Pythagorean theorem as an invariant structural property of right triangles in Euclidean space. The relationship a² + b² = c² holds regardless of the student's power, resources, or preferences. No escape, no negotiation, no contextual override. The constraint is the geometry itself.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of Cartesian coordinate geometry, the Pythagorean theorem emerges as a necessary consequence of the metric structure of Euclidean space. Distance d between points (x₁, y₁) and (x₂, y₂) is defined d = √((x₂-x₁)² + (y₂-y₁)²). For a right triangle with legs aligned to axes, this definition directly produces a² + b² = c². The constraint emerges from the foundation of the geometric system, not from enforcement or design.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL INSTITUTION (MOUNTAIN) — Mathematicians, educators, and scientific institutions treat the Pythagorean theorem as a foundational invariant. It cannot be negotiated, reformed, or temporarily suspended. Its universality across cultures and time periods (known to Babylonian mathematicians c. 1800 BCE, independently derived in multiple civilizations) demonstrates that the constraint is not contingent on institutional power or preference. The theorem is prior to institutional authority.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ENGINEERING (MOUNTAIN) — Engineering communities rely on the Pythagorean theorem as a computational guarantee across generational timescales. Structural engineers, surveyors, architects, and construction teams use the theorem to verify measurements, design load-bearing elements, and ensure spatial accuracy. No agent (engineer, firm, nation) can escape or extract from this constraint. The theorem's reliability is absolute — it is the foundation on which practical spatial disciplines rest.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: NON-EUCLIDEAN GEOMETRY (MOUNTAIN BOUNDARY) — In hyperbolic and spherical geometries, the Pythagorean theorem does not hold in its standard form. On a sphere's surface, geodesic triangles satisfy a² + b² > c² (spherical excess). This is NOT a violation of the mountain classification — it confirms it. The Pythagorean theorem is a mountain of EUCLIDEAN geometry, not of geometry in general. The constraint remains immutable within its proper domain. Transitioning to non-Euclidean space does not 'escape' the theorem; it changes which constraint becomes the relevant mountain (spherical law of cosines, hyperbolic metric). This perspective demonstrates the ε-invariance principle: the constraint's domain is part of its identity.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: FORMAL AXIOMS (MOUNTAIN) — Within the axioms of Euclidean geometry (Euclid's postulates or the Hilbert axiomatization), the Pythagorean theorem is a logical consequence, not a contingent fact. Given the parallel postulate and the definitions of right angle, congruence, and area, the theorem is derivable with zero degrees of freedom. No alternative formulation is possible without changing the foundational axioms. The constraint is axiomatic.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pythagorean_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(pythagorean_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pythagorean_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(pythagorean_theorem, ExtMetricName, E),
    domain_priors:suppression_score(pythagorean_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(pythagorean_theorem),
    narrative_ontology:constraint_metric(pythagorean_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(pythagorean_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(pythagorean_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal and declining. The Pythagorean theorem has no extractive function — no agent derives benefit at another's cost. The slight nonzero value (0.08 rather than 0.0) reflects the epistemological reality that proof and pedagogical presentation involve some informational overhead and institutional gatekeeping around mathematical knowledge historically, but this is incidental to the theorem itself, not inherent to the constraint. Modern open-access mathematics has reduced even this incidental overhead toward 0. Suppression (0.02): Negligible. The theorem cannot be suppressed — it is logically true regardless of institutional barriers to knowledge. Historical suppression of mathematical knowledge (medieval library restrictions, asymmetric access to geometry education) did not alter the theorem's validity, only delayed its discovery by some observers. Resistance (0.05): Extremely low. The theorem exhibits near-perfect accessibility once Euclidean geometry is understood. No counter-evidence or competing theory challenges it within its proper domain. Theater ratio (0.15): Very low. The theorem's practical applications and pedagogical presentation involve some ritual (formal proofs, standard problem sets, certification exams), but the underlying constraint has minimal performative content. A right triangle simply satisfies the relationship; no theater is required to demonstrate this.
 *
 * PERSPECTIVAL GAP:
 *   All six perspectives classify the Pythagorean theorem identically as a mountain. This is NOT unusual for natural law constraints — mountains are expected to be invariant across observers. The perspectival gap is NOT expressed in different classifications (all Mountain) but in EMPHASIS: the student emphasizes inescapability; the analytical observer emphasizes logical necessity; the institution emphasizes universality and stability; engineering emphasizes practical reliability; non-Euclidean geometry emphasizes domain specificity; formal axiomatics emphasizes deductive foundation. These are different entry points to the same conclusion: the constraint is immutable, universal within its domain, and prior to institutional authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality analysis does not apply to mountains. The theorem has no beneficiaries or victims; no agent experiences extraction or benefits asymmetrically. All observers occupy the same structural position relative to the constraint: they are all subject to it equally. The d value (directionality) is undefined because the constraint operates symmetrically across all agents. The chi formula χ = ε × f(d) × σ(S) produces χ ≈ 0.08 × 0.65 × 1.0 ≈ 0.05 for canonical parameters, reflecting that effective extraction (chi) is negligible. No agent experiences the theorem as extractive regardless of power level or exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The Pythagorean theorem resolves the mandatrophy trivially: it is a mountain. There is no possibility of mislabeling it as pure extraction (snare) or mixed extraction-coordination (tangled rope) because the theorem exhibits no extractive mechanism. All six perspectives confirm the same classification with the same base metrics (ε ≤ 0.25, suppression ≤ 0.05). The engine's mandatrophy resolution framework is not needed here — the constraint is a canonical natural law. The theorem serves as a reference case for validating that the classification system correctly identifies non-extractive constraints and does not falsely label mathematical and physical laws as institutional mechanisms. The invariance across all (P, T, E, S) tuples is the defining property of a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_euclidean,
    'Is the Pythagorean theorem a mountain of mathematics itself, or only a mountain of Euclidean geometry?',
    'Formal analysis of the theorem''s dependence on the parallel postulate and the universality of non-Euclidean geometries',
    'If only Euclidean: the constraint is domain-specific (a mountain within a specific axiomatic system). If universal: the constraint transcends geometric frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_boundary_euclidean, conceptual, 'Whether the theorem is universal or domain-specific to Euclidean geometry').

omega_variable(
    physical_space_curvature,
    'Is Euclidean geometry (and thus the Pythagorean theorem) empirically accurate to physical space, or is it an approximation?',
    'Empirical tests of spacetime geometry at various scales (local, astronomical, quantum); comparison of predicted vs measured distances in curved spacetime (general relativity)',
    'If empirically accurate: the theorem describes physical reality. If approximation: the theorem is a valid model within a limited regime, and general relativity''s geodesic constraint is the true mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_space_curvature, empirical, 'Whether Euclidean geometry is empirically accurate to physical space').

omega_variable(
    measurement_definition_circularity,
    'Does the Pythagorean theorem emerge from the definition of Euclidean distance, or is it an independent geometric fact?',
    'Axiomatic reconstruction of Euclidean geometry starting from alternative postulates (e.g., Birkhoff''s metric axioms vs Hilbert''s axioms); comparison of what must be assumed vs what can be derived',
    'If emerges from definition: the theorem is a tautology relative to the metric. If independent: the theorem is a substantive geometric law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_definition_circularity, conceptual, 'Whether the theorem is definitional or substantive relative to Euclidean axioms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pythagorean_theorem, 0, 4000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pyth_tr_t0, pythagorean_theorem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pyth_tr_t2000, pythagorean_theorem, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(pyth_tr_t4000, pythagorean_theorem, theater_ratio, 4000, 0.15).

% Extraction over time
narrative_ontology:measurement(pyth_be_t0, pythagorean_theorem, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(pyth_be_t2000, pythagorean_theorem, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(pyth_be_t4000, pythagorean_theorem, base_extractiveness, 4000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pythagorean_theorem, information_standard).
narrative_ontology:affects_constraint(pythagorean_theorem, euclidean_geometry_axioms).
narrative_ontology:affects_constraint(pythagorean_theorem, spherical_excess_constraint).
narrative_ontology:affects_constraint(pythagorean_theorem, hyperbolic_geometry_metric).

% DUAL FORMULATION NOTE:
% The Pythagorean theorem is a member of a constraint family spanning Euclidean, spherical, and hyperbolic geometries. Each geometry has a corresponding metric constraint. The Pythagorean theorem (ε=0.08, Mountain) is upstream of spherical excess constraint (ε=0.08, Mountain) and hyperbolic geodesic law (ε=0.08, Mountain) in the logical dependency graph. All three are natural laws of their respective geometric domains. The family relationship reflects ε-invariance: all members have identical base extraction because they are all laws of mathematics, differing only in domain. This is a constraint family decomposed by mathematical domain, not by empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
