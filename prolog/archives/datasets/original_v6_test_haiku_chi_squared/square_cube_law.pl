% ============================================================================
% CONSTRAINT STORY: square_cube_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: square_cube_law
 *   human_readable: The Square-Cube Law
 *   domain: technological/biological
 *
 * SUMMARY:
 *   The square-cube law is a pure mathematical/physical constraint: as any
 *   object scales uniformly by factor k, its surface area increases by k²
 *   while its volume and mass increase by k³. This geometric relationship is
 *   invariant across all domains—engineering, biology, materials science,
 *   planetary physics. It imposes no beneficiary or victim; it is a boundary
 *   condition on all physical reality. The constraint manifests identically
 *   whether an agent is powerful or powerless, institutional or isolated,
 *   because the constraint is not about extraction or coordination. It is
 *   about the inescapable structure of three-dimensional space. This
 *   constraint is the gold standard natural law exemplar: it emerges from
 *   axioms of geometry, it has zero degrees of freedom for all indices, and
 *   it holds across all observable methodologies.
 *
 * KEY AGENTS:
 *   - Engineers and architects: Powerful/analytical — must design within constraints but experience only geometric limits, not extraction
 *   - Biological organisms: Powerless/trapped — cannot exceed size thresholds set by diffusion and weight-bearing, but this is law not victimization
 *   - Materials scientists: Powerful/analytical — research structural improvements but work within the geometric framework
 *   - Regulators and standardizers: Institutional/analytical — codify unavoidable consequences, do not negotiate with geometry
 *   - The Mathematical Universe: Analytical/analytical — the constraint holder; no agency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(square_cube_law, 0.12).
domain_priors:suppression_score(square_cube_law, 0.02).
domain_priors:theater_ratio(square_cube_law, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(square_cube_law, extractiveness, 0.12).
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

% PERSPECTIVE 1: MATHEMATICAL INVARIANT (MOUNTAIN) — The square-cube relationship is a pure consequence of geometric scaling. For any object scaled by factor k, surface area ∝ k², volume ∝ k³. This holds universally across all materials, architectures, and biological systems. No agent can negotiate with geometry. ε=0.12, suppression=0.02, accessibility_collapse=0.92 → Mountain certified.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEER'S CONSTRAINT (MOUNTAIN) — Structural engineers, architects, and aerospace designers experience the square-cube law as an irreducible limit on scaling. Doubling the size of a bridge requires eight times the material mass but only four times the deck strength — this tradeoff cannot be designed away. High power + analytical exit → d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.17. The constraint remains mountain because no engineering solution eliminates the geometric fact.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANISM'S BODY PLAN (MOUNTAIN) — Biological organisms cannot exceed certain size thresholds due to surface-area-limited diffusion and weight-bearing limits. A land animal cannot grow to 1000 meters in length — the square-cube law makes the body plan mechanically impossible and metabolically unviable. The organism is trapped by geometry. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.16. Mountain persists even for powerless agents because the constraint is not extractive—it is simply physical law.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: REGULATORY STANDARD (MOUNTAIN) — Building codes, safety standards, and material certifications all embed the square-cube law constraints (weight limits, stress factors, cooling requirements scale with cube). Regulators do not enforce the law—they codify the unavoidable consequence. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Even institutional beneficiaries of standardization experience the square-cube law as mountain, not rope.
constraint_indexing:constraint_classification(square_cube_law, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: BIOPHYSICIST'S UNIVERSE (MOUNTAIN) — Across all scales—from bacteria to whales to planets—the square-cube law determines the relationship between structure and function. Heat dissipation, nutrient transport, structural support, and locomotion all scale predictably with the geometric ratio. This is not extractive; it is descriptive of reality. ε=0.12, suppression=0.02 → Mountain threshold satisfied universally.
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
 *   Extractiveness (0.12): Very low. The law does not extract value; it constrains available designs. Some inefficiency emerges—engineers must use more material to achieve desired strength as size increases—but this is not extraction in the sense of asymmetric transfer. It is waste inherent to the geometry. Suppression (0.02): Minimal. No agent suppresses alternatives to geometry; the constraint is freely observable and theoretically understood. Theater ratio (0.15): Very low. The law is not performed or maintained—it is simply true. References to the square-cube law in design and biology are descriptive, not performative.
 *
 * PERSPECTIVAL GAP:
 *   Unusually, there is NO perspectival gap. All five perspectives classify as Mountain because the constraint is a property of space itself, not of power relations. Even the powerless organism's perspective yields Mountain—the constraint does not extract from the organism; it simply defines feasible body plans. This uniformity across all perspectives is the defining signature of a true natural law constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because none exist. The square-cube law does not benefit any agent relative to others—it constrains all equally. The engineer, the organism, the regulator, and the mathematician all experience the same geometric fact. There is no asymmetry to model. The directionality derivation defaults to analytical (observer perspective) across all perspectives because the constraint has no structural relationship—it has a relationship only to geometry itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_innovation_workaround,
    'Can new materials with orders-of-magnitude higher strength-to-weight ratios effectively nullify the square-cube law constraint for engineered structures?',
    'Empirical testing of graphene composites, metamaterials, and biomimetic structures at increasing scale; comparison of predicted vs actual weight-to-strength ratios',
    'If successful: constraint becomes local to traditional materials (iron, concrete, aluminum), not universal. Mountain classification degrades to Rope (material selection as coordination). If unsuccessful: the geometric law is truly inescapable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(material_innovation_workaround, empirical, 'Whether advanced materials can overcome square-cube law constraints').

omega_variable(
    biological_scaling_universality,
    'Do all biological organisms exhibit the same surface-area-to-volume scaling relationships, or have evolutionary and developmental mechanisms produced systematic deviations from the geometric prediction?',
    'Allometric analysis across taxonomic groups; measurement of actual vs predicted metabolic rates, heat dissipation, and structural limits as function of size',
    'If universal: mountain classification holds for biology. If deviations are systematic and evolved: the law is a constraint that organisms adapt to, suggesting partial escape via co-evolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_scaling_universality, empirical, 'Whether biological scaling follows geometric law universally').

omega_variable(
    size_limit_enforcement_mechanism,
    'Is the size limit on land animals imposed by the square-cube law itself, or by downstream factors (energy availability, atmospheric composition, soil bearing capacity) that happen to correlate with geometric scaling?',
    'Paleontological analysis of extinct mega-fauna; biomechanical modeling of locomotion, respiration, and thermoregulation as functions of size; assessment of whether these constraints would relax with different environmental parameters',
    'If enforcement is geometric: mountain is fundamental. If enforcement is environmental/ecological: the geometric law is descriptive but not causally restrictive—organisms scale with available resources, and the law is incidental to that process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(size_limit_enforcement_mechanism, empirical, 'Whether square-cube law directly enforces biological size limits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(square_cube_law, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sqcube_tr_t0, square_cube_law, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sqcube_tr_t50, square_cube_law, theater_ratio, 50, 0.15).
narrative_ontology:measurement(sqcube_tr_t100, square_cube_law, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(sqcube_be_t0, square_cube_law, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sqcube_be_t50, square_cube_law, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(sqcube_be_t100, square_cube_law, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(square_cube_law, global_infrastructure).
narrative_ontology:affects_constraint(square_cube_law, heat_dissipation_scaling).
narrative_ontology:affects_constraint(square_cube_law, structural_scaling_limits).
narrative_ontology:affects_constraint(square_cube_law, metabolic_rate_scaling).
narrative_ontology:affects_constraint(square_cube_law, transport_diffusion_limits).

% DUAL FORMULATION NOTE:
% The square-cube law is upstream of multiple domain-specific constraints. Heat dissipation, structural strength, and metabolic rate all scale with the geometric ratios defined by this law. However, this constraint itself is not decomposable—it has a single, universal ε value and a single classification across all perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
