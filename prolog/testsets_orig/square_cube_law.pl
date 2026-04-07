% ============================================================================
% CONSTRAINT STORY: square_cube_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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
 *   The Square-Cube Law is a geometric principle stating that as linear
 *   dimensions increase by a factor k, surface area increases by k^2 and
 *   volume increases by k^3. This creates a fundamental constraint on all
 *   scaling processes in biology and engineering. The constraint is not
 *   imposed by any agent or institution; it emerges directly from the
 *   mathematics of three-dimensional space. No organism, engineer, or society
 *   can negotiate with or suppress this relationship. It is a natural law in
 *   the strict Deferential Realism sense: zero degrees of freedom, absolute
 *   accessibility collapse, irreducible across all observables.
 *
 * KEY AGENTS:
 *   - Biological Organisms: All living systems subject to scaling laws (powerless/trapped) — cannot alter geometric relationships
 *   - Engineering Systems: Structures, machines, vehicles designed by engineers (institutional/analytical) — must optimize within geometric constraints
 *   - Analytical Observer: Mathematics perspective (analytical/analytical) — sees pure dimensional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(square_cube_law, 0.08).
domain_priors:suppression_score(square_cube_law, 0.02).
domain_priors:theater_ratio(square_cube_law, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(square_cube_law, extractiveness, 0.08).
narrative_ontology:constraint_metric(square_cube_law, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(square_cube_law, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(square_cube_law, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(square_cube_law, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(square_cube_law, mountain).
narrative_ontology:human_readable(square_cube_law, "The Square-Cube Law").
narrative_ontology:topic_domain(square_cube_law, "technological/biological").

domain_priors:emerges_naturally(square_cube_law).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% An organism scaling up cannot escape the law. As it grows, surface area increases as L^2 while volume increases as L^3. Heat dissipation, nutrient absorption, and structural support all degrade relative to mass. No organism can negotiate with geometry — the constraint is absolute.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Engineers cannot escape the law, only optimize within it. Structural requirements scale as L^3; surface area available for bracing, heat dissipation, or material efficiency scales as L^2. Larger structures require disproportionate material investment. No institutional arrangement, technology, or market mechanism can repeal this geometry.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the analytical perspective, the square-cube law is a consequence of dimensional analysis alone. It holds for all objects in three-dimensional space regardless of material, composition, or context. It is not a contingent property of physics but a logical consequence of geometry itself.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(square_cube_law_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(square_cube_law, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(square_cube_law, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(square_cube_law, ExtMetricName, E),
    domain_priors:suppression_score(square_cube_law, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(square_cube_law),
    narrative_ontology:constraint_metric(square_cube_law, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(square_cube_law, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(square_cube_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. The square-cube law does not extract value from anyone; it is not a constraint imposed by agents but a pure geometric fact. No extraction occurs because there is no asymmetry in who bears costs — all agents bear the same geometric constraint equally. Suppression (0.02): Near-zero. The constraint cannot be suppressed or negotiated because it is not enforced by coercion but follows from first principles of geometry. No alternative exists to be suppressed. Theater ratio (0.15): Minimal. The constraint has no performative component — it operates identically whether observed or not. Engineering calculations and biological physiology implement the law directly, not through ritual or theater. Accessibility collapse (0.92): Very high. The square-cube law cannot be made accessible to agents who want to violate it. It is fundamentally irreducible — no clever design, material science, or institutional arrangement can make a large organism's surface-area-to-volume ratio scale linearly instead of as L^-1. Resistance (0.08): Very low. The law meets zero resistance because no one resists a pure mathematical fact. Engineers and biologists do not resist the square-cube law; they acknowledge it and design within its constraints.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All observers — organisms, engineers, mathematicians — classify the square-cube law as a mountain from their distinct contexts. The powerless organism experiences it as an absolute limit on scaling. The engineer experiences it as a binding constraint on design. The mathematician sees it as a logical necessity. All three perspectives produce mountain classification because the underlying structure is invariant across all observables and all agent contexts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there are no beneficiaries or victims in the structural sense. The square-cube law does not create an extraction flow from one agent to another. All agents experience the same geometric limit symmetrically. The constraint is not a power relationship; it is a natural fact. The absence of beneficiary/victim structure and directionality values is a defining feature of true mountains.
 *
 * MANDATROPHY ANALYSIS:
 *   The square-cube law resolves the mandatrophy by being non-contestable. There is no risk of mislabeling coordination as extraction because no coordination function exists — the constraint is purely geometric. There is no risk of mislabeling extraction as coordination because no asymmetric power relationship exists. The law is invariant across all observables: whether measured in biological scaling, engineering load calculations, physics simulations, or pure mathematics, the L^2/L^3 ratio remains identical. No measurement basis or context changes the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(square_cube_law, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(square_cube_law, allometric_scaling_law).
narrative_ontology:affects_constraint(square_cube_law, structural_material_limit).
narrative_ontology:affects_constraint(square_cube_law, metabolic_scaling_exponent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
