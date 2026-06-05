% ============================================================================
% CONSTRAINT STORY: thurston_geometrization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thurston_geometrization, []).

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
 *   constraint_id: thurston_geometrization
 *   human_readable: Thurston Geometrization Conjecture
 *   domain: mathematics/topology
 *
 * SUMMARY:
 *   The Thurston Geometrization Conjecture represents a foundational
 *   structural constraint in 3-dimensional topology: any closed, orientable
 *   3-manifold can be decomposed into pieces, each of which admits one of
 *   eight geometric structures (Euclidean, hyperbolic, spherical, and five
 *   others derived from matrix group actions). This constraint is classified
 *   as a Mountain because it expresses an invariant logical property of
 *   3-manifold structure that does not depend on observer position,
 *   institutional incentive, or measurement methodology. The constraint was
 *   conjectured by William Thurston in 1976 and proven by Grigori Perelman in
 *   2002-2003 using Ricci flow techniques. The proof does not negotiate or
 *   depend on consensus — it derives from the mathematical structure itself.
 *   Extractiveness (0.12) is non-zero because the proof required decades of
 *   development and multiple generations of mathematicians working on
 *   prerequisite techniques (Ricci flow, geometric PDE theory). However,
 *   extractiveness remains far below the rope threshold (0.45) because the
 *   structure itself imposes the constraint, not institutional negotiation.
 *   Suppression (0.03) is minimal — the proof is available to any
 *   mathematician with sufficient preparation in differential geometry.
 *   Accessibility collapse (0.92) is high — the conjecture is invariant
 *   across all valid mathematical frameworks and measurement approaches.
 *   Resistance (0.08) is low — the proof's logical structure does not permit
 *   plausible alternative interpretations.
 *
 * KEY AGENTS:
 *   - William Thurston: Mathematical originator (analytical/powerful) — proposed the classification framework that made the constraint explicit
 *   - Grigori Perelman: Proof provider (analytical/powerful) — resolved the conjecture using Ricci flow, establishing the invariant structure
 *   - Research community in geometric topology: Auxiliary contributors (analytical/moderate) — developed prerequisite techniques (Ricci flow stability theory, geometric PDE theory) that enabled the proof
 *   - Topological structure itself: The constraint bearer (analytical/trapped) — the 3-manifold space has the property whether or not humans discover it
 *   - Analytical observer: Perspective across frameworks (analytical/analytical) — sees the invariance across all consistent mathematical systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thurston_geometrization, 0.12).
domain_priors:suppression_score(thurston_geometrization, 0.03).
domain_priors:theater_ratio(thurston_geometrization, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thurston_geometrization, extractiveness, 0.12).
narrative_ontology:constraint_metric(thurston_geometrization, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(thurston_geometrization, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thurston_geometrization, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thurston_geometrization, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thurston_geometrization, mountain).
narrative_ontology:human_readable(thurston_geometrization, "Thurston Geometrization Conjecture").
narrative_ontology:topic_domain(thurston_geometrization, "mathematics/topology").

domain_priors:emerges_naturally(thurston_geometrization).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TOPOLOGICAL STRUCTURE (MOUNTAIN) — The space of 3-manifold geometries is invariant regardless of observer position or research incentive. The Thurston classification emerges as a consequence of the mathematical structure itself, not from institutional negotiation. The topology does not change based on who studies it.
constraint_indexing:constraint_classification(thurston_geometrization, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICS RESEARCH COMMUNITY (MOUNTAIN) — Even from the perspective of working mathematicians, the Thurston geometrization framework imposes an invariant logical constraint. Researchers cannot choose alternative classification schemes without abandoning the mathematical domain entirely. The structure is discovered, not constructed.
constraint_indexing:constraint_classification(thurston_geometrization, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From all possible observation positions and across all mathematical frameworks consistent with Euclidean geometry and manifold topology, the Thurston conjecture (now Perelman's theorem) represents an invariant structural fact. The classification of 3-manifolds into eight geometric types follows from the topological properties themselves.
constraint_indexing:constraint_classification(thurston_geometrization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thurston_geometrization_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thurston_geometrization, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thurston_geometrization, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thurston_geometrization, ExtMetricName, E),
    domain_priors:suppression_score(thurston_geometrization, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thurston_geometrization),
    narrative_ontology:constraint_metric(thurston_geometrization, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thurston_geometrization, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thurston_geometrization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.12): The constraint is classified as Mountain, which requires extractiveness ≤ 0.25 and suppression ≤ 0.05. The value 0.12 accounts for the historical contingency that the proof required specific technical developments (Hamilton's Ricci flow, PDE theory advances, geometric analysis). However, this technical prerequisite does not constitute 'extraction' in the DR sense — it is the normal cost of proving difficult theorems. The constraint itself (the geometrization property) is not contingent on these prerequisites; the proof is. Once proven, the constraint becomes invariant. The low value reflects that geometrization is a discovered invariant, not an institution-imposed extraction mechanism. SUPPRESSION (0.03): Minimal. The proof is published and accessible. There are no gates preventing mathematicians from engaging with the material. Suppressiveness would require institutional barriers, information asymmetry, or coercive limitation of alternatives — none of which apply. The only barrier is mathematical prerequisite knowledge, which is a property of learning the domain, not of extractive suppression. THEATER (0.08): Minimal. The proof's verification does not depend on performative ritual or social consensus. Mathematical proof is either correct or incorrect — there is no theater component where ceremonial agreement substitutes for logical necessity. ACCESSIBILITY COLLAPSE (0.92): High. The geometrization property is invariant across all mathematical frameworks, all measurement systems, and all observer positions. No amount of institutional manipulation or alternative framing can change the topological fact that 3-manifolds decompose into geometric pieces.
 *
 * PERSPECTIVAL GAP:
 *   Unlike high-extractiveness constraints that show dramatic perspectival differences based on observer power and exit options, the Thurston geometrization shows minimal perspectival gap. A powerless researcher and an analytical observer both classify it as Mountain — because the constraint is observer-invariant. The topological structure does not change based on who studies it. This uniformity is diagnostic of genuine mathematical invariants. When all six perspectives (if declared) produce identical or near-identical classifications, the constraint is either a true mountain or a degenerate case (uniform rope/piton). In this case, the mountain classification is reinforced by the absence of perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply meaningfully to pure mathematical invariants. The constraint is not an extraction mechanism flowing from beneficiaries to victims — it is a structural fact about 3-manifold topology. No agent benefits and no agent bears cost from the geometrization property itself. The proof required effort (which could be labeled as prerequisite 'cost'), but this is not extraction from a victim to a beneficiary; it is the inherent difficulty of proving difficult theorems. Accordingly, all perspectives treat the constraint as Mountain with minimal directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY: The Thurston geometrization does not exhibit the mandatrophy ambiguity because it is a pure mountain constraint with zero coordination function and zero asymmetric extraction. There is no mislabeling risk — the constraint cannot plausibly be interpreted as rope (pure coordination) or snare (pure extraction). It is a discovered invariant of mathematical structure. The constraint does not 'prevent' mislabeling through active enforcement; it simply is what it is. The proof resolves the conjecture and eliminates alternative interpretations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_vs_platonic,
    'Is the Thurston geometrization a discovered invariant of 3-manifold structure or a constructed classification framework?',
    'Examination of whether alternative geometric classification schemes are mathematically coherent and whether they partition the space of 3-manifolds differently. If alternative systems are equally valid, the constraint is partially constructed; if all valid systems converge on Thurston''s classification, it is discovered.',
    'If discovered: mountain classification is reinforced. If partially constructed: the constraint has a rope component (coordination function in how mathematicians agree to classify). If heavily constructed: reclassify as rope or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_vs_platonic, conceptual, 'Constructive vs platonic nature of geometric classification').

omega_variable(
    perelmans_proof_algorithmic_completeness,
    'Does Perelman''s proof provide an algorithmic procedure for decomposing any 3-manifold into its geometric components, or does it establish existence without constructive accessibility?',
    'Analysis of the Ricci flow proof methodology and whether it translates to computable decomposition algorithms. Assessment of whether the proof is constructive or relies on existence arguments that resist algorithmization.',
    'If fully algorithmic: accessibility_collapse remains ≥0.85 (complete verification possible). If existence-only: accessibility may degrade slightly but remains high (the theorem is proven; decomposition is just harder to compute).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(perelmans_proof_algorithmic_completeness, empirical, 'Algorithmic constructiveness of the geometrization decomposition').

omega_variable(
    geometric_universality_across_settings,
    'Does the Thurston geometrization extend identically to higher dimensions, or is it specific to 3-manifolds due to a structural property unique to dimension 3?',
    'Comparison with Ricci flow and geometrization attempts in dimensions 4+. Identification of which features of the 3-dimensional proof generalize and which are dimension-specific. Analysis of whether failure in higher dimensions indicates the 3-case is contingent or reveals deeper invariants.',
    'If universal across dimensions: strengthens the mountain claim (reflects a deep topological law). If unique to dimension 3: the constraint may have hidden contingency. If dimension 3 is special: mountain classification is still valid but reveals structural distinctness of 3-topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geometric_universality_across_settings, empirical, 'Universal scope of geometrization across dimensions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thurston_geometrization, 1976, 2003).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thurston_geometrization, information_standard).
narrative_ontology:affects_constraint(thurston_geometrization, ricci_flow_convergence).
narrative_ontology:affects_constraint(thurston_geometrization, geometric_pde_structure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
