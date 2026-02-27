% ============================================================================
% CONSTRAINT STORY: four_color_theorem_topological_bound
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_four_color_theorem_topological_bound, []).

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
 *   constraint_id: four_color_theorem_topological_bound
 *   human_readable: The Four Color Theorem
 *   domain: mathematical/topological
 *
 * SUMMARY:
 *   The Four Color Theorem is a mathematical constraint that bounds the
 *   chromatic number of planar graphs to exactly four colors. Originally
 *   conjectured in 1852 by Francis Guthrie, it remained unproven for 124
 *   years until Appel and Haken's 1976 computer-assisted proof. The theorem
 *   stands as a canonical example of a Mountain constraint in mathematical
 *   topology: the bound appears as an immutable consequence of the plane's
 *   topological structure, not negotiable by institutional arrangement, proof
 *   methodology variation, or observational perspective. The constraint is
 *   invariant across all formulations (cartographic coloring, graph chromatic
 *   number, embedding theory) and applies universally to all planar graphs.
 *   Unlike many mathematical conjectures that can be sidestepped by reframing
 *   assumptions, the four-color bound admits no exit options — any planar
 *   graph either requires ≤4 colors (true) or admits a counterexample (false,
 *   and the theorem asserts false). The proof's computer-assisted nature has
 *   introduced some theater (0.15 theater_ratio, reflecting verification
 *   complexity), but this is accessory to the core topological claim. The
 *   bound's emergence is natural in the deepest sense: it follows from the
 *   plane's Euler characteristic and graph-embedding constraints in 2D space,
 *   requiring no appeal to human convention, institutional enforcement, or
 *   organizational structure.
 *
 * KEY AGENTS:
 *   - The Cartographer: Practical agent (powerless/analytical) — must respect the four-color bound in all map designs
 *   - The Graph Theorist: Disciplinary expert (moderate/analytical) — understands the chromatic number bound; cannot reduce it
 *   - The Mathematics Community: Institutional collective (organized/analytical) — verifies the proof; confirms the bound across cultures
 *   - The Analytical Observer: Civilizational perspective (analytical/analytical) — perceives the bound as topological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(four_color_theorem_topological_bound, 0.08).
domain_priors:suppression_score(four_color_theorem_topological_bound, 0.02).
domain_priors:theater_ratio(four_color_theorem_topological_bound, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, extractiveness, 0.08).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(four_color_theorem_topological_bound, mountain).
narrative_ontology:human_readable(four_color_theorem_topological_bound, "The Four Color Theorem").
narrative_ontology:topic_domain(four_color_theorem_topological_bound, "mathematical/topological").

domain_priors:emerges_naturally(four_color_theorem_topological_bound).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CARTOGRAPHER (MOUNTAIN) — From the standpoint of practical map-coloring, the four-color bound is an inescapable topological constraint. No matter how regions are arranged on a plane, no cartographer can construct a counterexample. The bound is not negotiable, not subject to institutional variation, not subject to exit or arbitrage. Complete accessibility collapse: the constraint appears as pure physical/topological fact.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE GRAPH THEORIST (MOUNTAIN) — The dual formulation (chromatic number of planar graphs ≤ 4) presents the same immutable bound from the graph-coloring perspective. A graph theorist cannot reduce the chromatic number bound below 4 through any algorithmic, organizational, or institutional choice. The constraint is invariant across all proof methodologies (computer-assisted proof, combinatorial argument, topological reduction). Zero degrees of freedom.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the highest analytical level, the four-color bound is a necessary consequence of the topological structure of the plane. The Euler characteristic χ = 2 for the sphere/plane, combined with the graph's edge-density constraints in 2D embedding, forces the chromatic number to ≤ 4. This is not a law discovered by humans but a structural property of topology itself. Universal scope, civilizational time horizon, zero exit options, zero degrees of freedom.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICS COMMUNITY (MOUNTAIN) — Even institutions (research programs, universities, funding agencies) cannot negotiate the four-color bound. No amount of institutional coordination, resource allocation, or organizational restructuring changes the topological fact. Proof verification (whether computer-assisted or human-verified) confirms the bound uniformly. The constraint is invariant across mathematical cultures, historical periods, and proof methodologies. Emerges naturally from topological structure, not from human convention.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(four_color_theorem_topological_bound_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, ExtMetricName, E),
    domain_priors:suppression_score(four_color_theorem_topological_bound, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(four_color_theorem_topological_bound),
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(four_color_theorem_topological_bound_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The four-color bound extracts nothing from any agent — it is a constraint on what is possible, not a mechanism for transferring resources or benefits. The bound does not create scarcity (colors are infinitely reproducible), does not create power asymmetries, and does not create losers and winners. Pure structural fact. Suppression (0.02): Minimal. There are no alternatives to suppress. No agent would choose a different chromatic number if they could — the bound is not sustained by coercion or lack of alternatives. It simply is. Theater ratio (0.15): Low, but nonzero. The computer-assisted proof introduces some theater: verification requires computational checking of thousands of cases, and confidence in the proof depends on trusting the code and hardware. This is not essential theater (the bound would be true without computer verification), but the proof's method has made verification less transparent than a human-verifiable combinatorial argument would be. The theater has increased from ~0.05 (when the proof was novel and questioned) to 0.15 (current state, where the proof is verified but verification complexity persists). Accessibility collapse (0.92): Very high. The constraint is accessible to any agent attempting to color a planar graph — there is nowhere to hide, no institutional shelter, no argument from complexity or convention. All practical tests confirm the bound. Resistance (0.08): Very low. There are no institutional, cultural, or argumentative mechanisms resisting the theorem's acceptance. The bound is universally accepted across all mathematical schools and applied fields. The 124-year proof delay did not reflect resistance to the bound itself but difficulty in proving what was already empirically obvious (all known maps colored with 4 colors, many cases reducing to smaller instances).
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the four-color constraint as Mountain. This is expected for a true topological necessity — the perspectival gap collapses to zero. The constraint appears identically immutable from the cartographer's practical standpoint, the graph theorist's algorithmic view, the mathematical community's institutional verification, and the analytical observer's topological understanding. The lack of perspectival divergence is itself diagnostic: genuine mountains should show zero to minimal gap. If perspectives had diverged into different constraint types (e.g., if some observers classified this as Rope or Piton), the claim to mountainhood would be invalidated by the framework's perspectival test.
 *
 * MANDATROPHY ANALYSIS:
 *   The four-color theorem does not involve mandatrophy — there is no risk of misclassifying extraction as coordination or vice versa. The constraint is purely a topological bound with no coordination function, no beneficiaries, no victims, no asymmetric extraction. All six types collapse to Mountain. The mandatrophy resolution is trivial: the theorem has never been misconstrued as Rope (coordination), Snare (extraction), or any other type because it exhibits zero extraction and zero coordination. It is a clean Mountain case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_necessity_vs_algorithm,
    'Does the necessity of the four-color bound derive from topological impossibility (truly a mountain) or from the algorithmic complexity of the proof-verification task (theater-laden institutional arrangement)?',
    'Exhibit a non-computational proof of the four-color theorem, or prove that all proofs must be computer-assisted. Compare proof necessity in topology vs other mathematical domains with computer-assisted proofs.',
    'If topological necessity: mountain classification confirmed. If algorithmic necessity only: reclassify as piton (institutional theater around proof verification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_necessity_vs_algorithm, conceptual, 'Whether the bound is topological fact or proof-verification theater').

omega_variable(
    higher_genus_generalization,
    'Does the five-color theorem for genus-1 surfaces, six-color for genus-2, etc., represent genuine topological generalizations or a failure of the four-color bound to extend, revealing it as contingent rather than universal?',
    'Prove or disprove the generalized Heawood conjecture for all genera. Examine whether the bound depends essentially on the plane''s topological properties or on contingent graph-embedding facts.',
    'If the bound is robust across topological spaces: mountain classification strengthened. If it fails to generalize in any direction: reveals contingency in the four-color structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(higher_genus_generalization, empirical, 'Robustness of the four-color bound across topological generalizations').

omega_variable(
    computational_accessibility,
    'Is the four-color bound computationally verifiable for all planar graphs, or does verification complexity grow with graph size in a way that makes the bound practically inaccessible for large instances?',
    'NP-completeness analysis of the chromatic number problem for planar graphs. Analysis of whether the bound is theoretically true but practically unverifiable for realistic graph sizes.',
    'If verifiable: accessibility_collapse confirmed. If practically unverifiable: accessibility_collapse drops significantly, revealing institutional/computational theater in the bound''s enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_accessibility, empirical, 'Computational accessibility of four-color verification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(four_color_theorem_topological_bound, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fct_tr_t0, four_color_theorem_topological_bound, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fct_tr_t50, four_color_theorem_topological_bound, theater_ratio, 50, 0.15).
narrative_ontology:measurement(fct_tr_t100, four_color_theorem_topological_bound, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(fct_be_t0, four_color_theorem_topological_bound, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fct_be_t50, four_color_theorem_topological_bound, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(fct_be_t100, four_color_theorem_topological_bound, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
