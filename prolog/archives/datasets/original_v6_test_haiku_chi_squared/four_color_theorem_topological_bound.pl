% ============================================================================
% CONSTRAINT STORY: four_color_theorem_topological_bound
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: four_color_theorem_topological_bound
 *   human_readable: The Four Color Theorem: Topological Chromatic Bound
 *   domain: mathematics/topology/graph_theory
 *
 * SUMMARY:
 *   The Four Color Theorem is a fundamental topological constraint on planar
 *   graph coloring: no map on a plane or sphere requires more than four
 *   colors such that no two adjacent regions share the same color. Originally
 *   conjectured in 1852, the theorem was first 'proven' in 1879 by Alfred
 *   Kempe (with a flaw discovered in 1890), and finally resolved rigorously
 *   in 1976 by Kenneth Appel and Wolfgang Haken using computer-assisted
 *   exhaustive case analysis. Unlike computational theorems that depend on
 *   specific algorithms or implementations, the Four Color Theorem is a claim
 *   about intrinsic topological structure: the chromatic bound is invariant
 *   across all planar embeddings, all coloring strategies, and all
 *   applications. The theorem constrains no agent's behavior and extracts
 *   nothing from anyone. It is a structural limit of planar topology itself.
 *
 * KEY AGENTS:
 *   - Mathematician/Topologist: No structural role (mountain provides no victims or beneficiaries). Can apply the theorem but cannot negotiate, circumvent, or extract value from the constraint itself.
 *   - Applied Systems Designer: No structural role. Uses the theorem to inform register allocation, network design, and scheduling algorithms. Beneficiary of the knowledge but not subjected to extraction.
 *   - Student/Problem Solver: No structural role. Encounters the constraint as an absolute limit on map colorings. Not victimized; the constraint is a fact, not an enforced rule.
 *   - Axiom System/Formal Foundation: Implicit dependency. The theorem's truth status depends on the underlying logical framework, but no agent 'controls' the foundation in a way that creates extraction.
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
narrative_ontology:human_readable(four_color_theorem_topological_bound, "The Four Color Theorem: Topological Chromatic Bound").
narrative_ontology:topic_domain(four_color_theorem_topological_bound, "mathematics/topology/graph_theory").

domain_priors:emerges_naturally(four_color_theorem_topological_bound).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIZATIONAL ANALYTICAL OBSERVER (MOUNTAIN) — From the viewpoint of abstract mathematics, the Four Color Theorem is a topological invariant: a fundamental limit on chromatic complexity for planar graphs. This constraint emerges from the intrinsic structure of planar topology, not from any extractive institutional arrangement. d≈0.72, f(d)≈1.15, σ=1.0, but ε=0.08 and suppression=0.02 force the mountain classification regardless of directionality. This perspective sees zero degrees of freedom: the theorem is true for all maps, all times, all observers.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROFESSIONAL MATHEMATICIAN (MOUNTAIN) — A working topologist encounters the Four Color Theorem as an immutable constraint on any map coloring problem. Whether the mathematician is applying it to cartography, circuit design, or register allocation, the limit of four colors is invariant across all applications. d≈0.05, f(d)≈-0.12, σ=1.2. Even the beneficiary perspective (arbitrage access to a proven theorem) yields mountain classification because the underlying ε=0.08 and suppression=0.02 satisfy the natural law signature unconditionally.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: APPLIED SYSTEMS DESIGNER (MOUNTAIN) — An engineer designing network routing, scheduling systems, or register allocation in compilers must respect the chromatic bound as an irreducible constraint. The theorem offers no leverage point for negotiation or circumvention: the designer can choose the mapping strategy but not the underlying topological limit. d≈0.50, f(d)≈0.65, σ=1.1. Again, even this symmetric/moderate perspective cannot reframe the constraint: accessibility_collapse=0.92 and resistance=0.08 ensure mountain classification independent of power or exit options.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: STUDENT / LOCAL AGENT (MOUNTAIN) — A student tasked with coloring a map encounters the four-color limit as an absolute barrier. The student cannot negotiate, extract concessions, or establish alternative rules. The constraint is indifferent to the student's power or exit options. d≈0.95, f(d)≈1.42, σ=0.8. Even the maximally disadvantaged perspective yields mountain classification: the theorem's ε and suppression metrics are so low that f(d) and scope modifications cannot push it above the mountain thresholds.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

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
 *   Extractiveness (0.08): Very low. The Four Color Theorem is a mathematical fact, not a mechanism of resource extraction. No agent extracts from another through this constraint. The value 0.08 reflects minimal measurement uncertainty and the possibility that formalization choices (axiom system, proof strategy) introduce subtle dependencies — but these are philosophical ambiguities, not extractive mechanisms. Suppression (0.02): Negligible. The theorem imposes no barriers to exit, no coercion, and no silencing. Any agent can ignore the theorem (by refusing to engage with coloring problems) or dispute it (through valid proof objections). The 0.02 value captures only the irreducible suppression inherent in any mathematical fact: one cannot will a counterexample into existence. Theater ratio (0.15): Very low. The proof itself (computer-assisted enumeration of 1,936 reducible configurations) has modest theatrical elements — the exhaustive case-checking is somewhat opaque to human intuition — but the underlying mathematics is direct and verifiable. Over the measurement interval (0-100 years), theater_ratio rises slightly from 0.10 to 0.15 as the original intuitive proofs (Kempe-Heawood era) give way to computer-verified proofs, which are less transparent to traditional mathematical intuition but more formally rigorous.
 *
 * PERSPECTIVAL GAP:
 *   There is NO perspectival gap for the Four Color Theorem. All four tested perspectives (civilizational analytical, institutional, powerful applied, powerless local) yield mountain classification. The base metrics (ε=0.08, suppression=0.02) are so low that no combination of power, time horizon, exit options, or spatial scope can push the effective extractiveness above the mountain threshold (χ must remain ≤ 0.25 for the classification to hold, and χ = 0.08 × f(d) × σ(S) remains well below this ceiling across all reasonable values of f(d) ∈ [-0.12, 1.42] and σ(S) ∈ [0.8, 1.2]). This uniformity is a defining feature of natural law constraints: they appear identically binding to all observers because they reflect intrinsic structural limits, not institutional power asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to mountain constraints in the traditional extraction sense. However, the indexical tuple still captures each observer's relationship to the knowledge of the constraint: Analytical observer: d≈0.72 (sees the constraint as an abstract fact; neither benefits nor suffers). Institutional mathematician: d≈0.05 (benefits from the proven theorem; arbitrage access to a solved problem). Powerful applied designer: d≈0.50 (symmetric: the constraint limits design options but also provides a known bound that simplifies reasoning). Powerless student: d≈0.95 (trapped by the constraint; cannot negotiate or circumvent; maximum asymmetry in terms of agency over the mathematical fact itself). Despite these differing d values, all perspectives yield mountain classification because the ε and suppression metrics override the directionality contribution to χ. The framework correctly treats natural law constraints as independent of power asymmetries: the theorem is true regardless of who benefits or suffers from knowing it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_formalization_requirement,
    'Does the Computer-Assisted Proof (1976) count as a fully rigorous proof, or does the informal step (verification of 1,936 reducible configurations by exhaustive computer enumeration) introduce a residual axiom dependency that could eventually be resolved differently?',
    'Formal verification of the proof in a constructive type theory (Coq, Lean, Agda); examination of whether the proof relies on any non-constructive axioms or unproven subroutines',
    'If the CAP is constructively formalized: confidence in the mountain classification increases (ε remains ≤0.08). If residual dependencies exist: ε might rise to 0.12-0.15, potentially degrading to Piton (theatrical proof ritual without full verification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_formalization_requirement, empirical, 'Whether computer-assisted proof is fully rigorous or contains residual axiom dependencies').

omega_variable(
    planar_topology_axiom_independence,
    'Is the Four Color Theorem dependent on the choice of axiom system, or is it invariant across all consistent foundational frameworks (ZFC, type theory, intuitionistic logic, homotopy type theory)?',
    'Proof of equivalence or non-equivalence across foundational systems; investigation of whether any consistent axiom system permits a counterexample',
    'If invariant across all foundations: mountain classification is robust (ε remains universally ≤0.08). If axiom-dependent: constraint is situated within a specific axiomatic choice, potentially reducing ε stability across frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planar_topology_axiom_independence, conceptual, 'Whether the theorem is independent of foundational axiom choice').

omega_variable(
    planar_embedding_definition_variance,
    'Does the definition of ''planar graph'' or ''adjacent regions'' contain hidden assumptions that could yield different chromatic bounds on topologically equivalent but formally distinct embeddings?',
    'Systematic enumeration of equivalent topological configurations; verification that chromatic bound is invariant under all topologically equivalent re-embeddings and re-definitions of adjacency',
    'If variance exists: ε rises to 0.15-0.20 (residual interpretive freedom). If no variance: ε remains ≤0.08 and mountain classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(planar_embedding_definition_variance, conceptual, 'Whether different valid definitions of planar embedding yield different chromatic bounds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(four_color_theorem_topological_bound, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fct_tr_t0, four_color_theorem_topological_bound, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fct_tr_t50, four_color_theorem_topological_bound, theater_ratio, 50, 0.12).
narrative_ontology:measurement(fct_tr_t100, four_color_theorem_topological_bound, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(fct_be_t0, four_color_theorem_topological_bound, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(fct_be_t50, four_color_theorem_topological_bound, base_extractiveness, 50, 0.075).
narrative_ontology:measurement(fct_be_t100, four_color_theorem_topological_bound, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(four_color_theorem_topological_bound, information_standard).
narrative_ontology:affects_constraint(four_color_theorem_topological_bound, chromatic_polynomial_bounds).
narrative_ontology:affects_constraint(four_color_theorem_topological_bound, graph_coloring_algorithmic_complexity).
narrative_ontology:affects_constraint(four_color_theorem_topological_bound, planar_embedding_isomorphism).

% DUAL FORMULATION NOTE:
% The Four Color Theorem is a standalone natural law constraint with no decomposition needed. However, it affects several downstream constraints in graph theory and algorithmic complexity. The chromatic polynomial bounds constraint (ε≈0.05) is upstream (a more general statement of which FCT is a special case). Graph coloring algorithmic complexity (ε≈0.55) is downstream (the computational difficulty of finding optimal colorings despite knowing the bound exists). Planar embedding isomorphism (ε≈0.12) is a sibling constraint addressing topological equivalence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
