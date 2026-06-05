% ============================================================================
% CONSTRAINT STORY: planar_graph_embeddability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planar_graph_embeddability, []).

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
 *   constraint_id: planar_graph_embeddability
 *   human_readable: Planar Graph Embeddability (Kuratowski's Theorem)
 *   domain: discrete_mathematics/graph_theory
 *
 * SUMMARY:
 *   Planar graph embeddability is a canonical example of a mathematical
 *   constraint that qualifies as a Mountain under Deferential Realism: it is
 *   a topological necessity derived from the structure of Euclidean space and
 *   graph connectivity, not a contingent institutional arrangement or
 *   extractive mechanism. Kuratowski's theorem (1930) states that a finite
 *   graph can be drawn in the plane without edge crossings if and only if it
 *   does not contain a subdivision of K₅ or K₃,₃ as a subgraph. This is not a
 *   rule that can be negotiated, reformed, or made context-dependent. The
 *   impossibility of planarly embedding K₅ or K₃,₃ is not caused by anyone's
 *   action, is not enforced by any authority structure, and cannot be
 *   overcome through effort or appeal. It is a mathematical law grounded in
 *   the topological properties of Euclidean space itself.
 *
 * KEY AGENTS:
 *   - Planar Graphs: Structures that satisfy Kuratowski's criterion (no K₅ or K₃,₃ subdivisions). They have no exit options and no alternative — planarity is a topological invariant.
 *   - Non-Planar Graphs: Structures that contain K₅ or K₃,₃ subdivisions. They cannot become planar through algorithmic manipulation, incentive adjustment, or reinterpretation.
 *   - Algorithm Designers: Practitioners applying planarity testing and graph drawing algorithms. They encounter the constraint as a hard limit on what is computable.
 *   - Mathematical Community: Collective that grounds topology and discrete mathematics in Kuratowski's theorem as foundational. The constraint structures legitimate mathematical discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planar_graph_embeddability, 0.08).
domain_priors:suppression_score(planar_graph_embeddability, 0.02).
domain_priors:theater_ratio(planar_graph_embeddability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planar_graph_embeddability, extractiveness, 0.08).
narrative_ontology:constraint_metric(planar_graph_embeddability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(planar_graph_embeddability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(planar_graph_embeddability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(planar_graph_embeddability, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planar_graph_embeddability, mountain).
narrative_ontology:human_readable(planar_graph_embeddability, "Planar Graph Embeddability (Kuratowski's Theorem)").
narrative_ontology:topic_domain(planar_graph_embeddability, "discrete_mathematics/graph_theory").

domain_priors:emerges_naturally(planar_graph_embeddability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLANAR GRAPH (MOUNTAIN) — A finite graph either does or does not contain K₅ or K₃,₃ as a subdivision. This is a topological invariant, immutable across all observers, all times, all applications. The constraint is not imposed; it is discovered. Exit is meaningless — the property is constitutive of the graph itself.
constraint_indexing:constraint_classification(planar_graph_embeddability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM DESIGNER (MOUNTAIN) — Designers of planar graph algorithms (planarity testing, layout, drawing optimization) encounter Kuratowski's theorem as an absolute constraint on what algorithms can accomplish. They cannot make a non-planar graph planar through clever design. The constraint defines the problem domain itself — it is not an obstacle to overcome but a mathematical law that structures the design space.
constraint_indexing:constraint_classification(planar_graph_embeddability, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY (MOUNTAIN) — Kuratowski's theorem is a foundational result grounding topology and discrete mathematics. It is not negotiable, not subject to revision based on preference or context, and not extractive. The theorem structures legitimate mathematical discourse itself. Its truth is independent of who uses it or for what purpose.
constraint_indexing:constraint_classification(planar_graph_embeddability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational, universal scope, Kuratowski's theorem is a structural fact about Euclidean topology. The impossibility of drawing K₅ or K₃,₃ in the plane without edge crossings follows from the dimensionality and connectivity properties of Euclidean space itself. The constraint is not contingent on institutional arrangements, incentive structures, or observer perspective. It is a mathematical truth.
constraint_indexing:constraint_classification(planar_graph_embeddability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planar_graph_embeddability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(planar_graph_embeddability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planar_graph_embeddability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(planar_graph_embeddability, ExtMetricName, E),
    domain_priors:suppression_score(planar_graph_embeddability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(planar_graph_embeddability),
    narrative_ontology:constraint_metric(planar_graph_embeddability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(planar_graph_embeddability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(planar_graph_embeddability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. No extraction occurs. The constraint does not benefit anyone relative to anyone else — it is a topological fact. Suppression (0.02): Negligible. There are no alternatives to suppress; K₅ and K₃,₃ cannot be embedded planarly regardless of effort or appeal. The constraint is not enforced by anyone; it is discovered by all. Theater ratio (0.05): Near-zero. Kuratowski's theorem requires no performative maintenance or ritual to remain true. The proof stands alone and requires no institutional theater to sustain it. Accessibility collapse (0.92): Very high. The constraint is fully inaccessible to circumvention — no degrees of freedom exist. Resistance (0.03): Minimal. There is no resistance mechanism because there is no enforcer. The constraint simply is.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on the same classification (Mountain). This is the diagnostic signature of a genuine natural law. The powerless graph, the algorithm designer, the mathematical community, and the analytical observer all perceive the constraint as immutable and non-negotiable. There is no perspectival gap because no agent has an asymmetric relationship to a topological fact. The constraint is the same whether you are the graph being tested for planarity or the mathematician proving Kuratowski's theorem.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint. The d parameter (measuring the agent's position as beneficiary vs target in an extraction flow) has no meaning when no extraction occurs. All agents are equally subject to the topological constraint; none benefit from it relative to others. The sigmoid f(d) cannot be applied because the underlying extraction mechanism does not exist. This is mathematically clean and structurally revealing: the absence of directionality is itself proof of the mountain classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_proof_completeness,
    'Is Kuratowski''s theorem true in all formal mathematical systems, or only in classical Euclidean topology?',
    'Verification in alternative geometric frameworks (non-Euclidean space, abstract topological structures, constructivist mathematics). Testing whether the theorem holds or requires modification in intuitionistic logic or topos theory.',
    'If true universally: mountain classification is absolute. If context-dependent across formal systems: potential weak reclassification to rope (mathematical convention). Current evidence strongly supports universal validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_proof_completeness, empirical, 'Formal system independence of Kuratowski''s theorem').

omega_variable(
    definition_stability,
    'Are the terms ''planar graph'', ''Euclidean plane'', ''edge crossing'', and ''subdivision'' invariant in meaning across mathematical contexts, or do subtle definitional variations enable alternative interpretations?',
    'Review of definitional history across topology, graph theory, and combinatorics textbooks. Analysis of whether different formalizations (e.g., graph embeddings in abstract surfaces vs Euclidean plane) produce measurably different constraint structures.',
    'If definitions are stable: mountain classification holds. If definitional ambiguity exists at boundaries: mountain remains robust but with caveats for non-standard formalizations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_stability, conceptual, 'Definitional stability of core topological terms').

omega_variable(
    computational_accessibility,
    'Does computational difficulty in testing planarity (complexity of algorithm implementation, CPU resources required) reflect the mathematical constraint or obscure it behind practical barriers?',
    'Comparison of theoretical planarity (Kuratowski criterion) vs computational planarity (algorithm efficiency). Analysis of whether high-resource algorithms create perceived non-planarity artifacts in applied contexts.',
    'No impact on classification. The computational difficulty is orthogonal to the mathematical constraint itself. Kuratowski''s theorem remains mountain-class regardless of algorithmic complexity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_accessibility, empirical, 'Computational complexity vs theoretical constraint clarity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planar_graph_embeddability, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planar_graph_embeddability, information_standard).
narrative_ontology:affects_constraint(planar_graph_embeddability, planar_drawing_algorithm_complexity).
narrative_ontology:affects_constraint(planar_graph_embeddability, graph_coloring_four_color_theorem).

% DUAL FORMULATION NOTE:
% Kuratowski's theorem is a self-contained topological result with no meaningful decomposition. However, it structurally constrains two downstream results: (1) the computational complexity of planarity testing algorithms, which is a separate constraint involving computational resources and algorithmic efficiency, and (2) the four-color theorem for planar graphs, which depends on planarity as a prerequisite. These downstream constraints are not readings of Kuratowski but applications of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
