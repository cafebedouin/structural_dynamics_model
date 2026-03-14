% ============================================================================
% CONSTRAINT STORY: nowhere_dense_sets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nowhere_dense_sets, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nowhere_dense_sets
 *   human_readable: Nowhere Dense Sets: The Topological Closure Constraint
 *   domain: mathematics/topology
 *
 * SUMMARY:
 *   A nowhere dense set is a set whose closure has empty interior. In
 *   topology, this defines a fundamental structural limit: any such set
 *   cannot be decomposed into countably many closed sets with empty interior
 *   that together cover the entire space in a dense way. This constraint is
 *   invariant across all topological frameworks and measurement
 *   methodologies. It emerges from the definition of nowhere density and the
 *   properties of closure operations, not from any institutional or policy
 *   choice. The constraint has zero degrees of freedom across all contexts —
 *   no agent can negotiate or exit it.
 *
 * KEY AGENTS:
 *   - Topological Space Structure: The mathematical object itself (universal/analytical) — nowhere density is an intrinsic property
 *   - Analytic Agents: Any mathematical actor attempting to decompose or cover (universal/trapped) — cannot violate the constraint
 *   - Mathematical Community: The collective epistemic system (analytical/analytical) — recognizes the constraint as logically necessary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nowhere_dense_sets, 0.08).
domain_priors:suppression_score(nowhere_dense_sets, 0.02).
domain_priors:theater_ratio(nowhere_dense_sets, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nowhere_dense_sets, extractiveness, 0.08).
narrative_ontology:constraint_metric(nowhere_dense_sets, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(nowhere_dense_sets, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nowhere_dense_sets, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(nowhere_dense_sets, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nowhere_dense_sets, mountain).
narrative_ontology:human_readable(nowhere_dense_sets, "Nowhere Dense Sets: The Topological Closure Constraint").
narrative_ontology:topic_domain(nowhere_dense_sets, "mathematics/topology").

domain_priors:emerges_naturally(nowhere_dense_sets).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTIC AGENT SEEKING DENSE COVERAGE (MOUNTAIN) — Any agent attempting to cover a nowhere dense set with a countable collection of closed sets with empty interior cannot succeed. This is not a policy choice or institutional arrangement — it is a logical necessity derived from the definition of nowhere density. The agent has zero degrees of freedom.
constraint_indexing:constraint_classification(nowhere_dense_sets, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL COMMUNITY (ANALYTICAL) (MOUNTAIN) — From the community's perspective across generations, nowhere dense sets remain invariant under rigorous topological analysis. The constraint emerges from the definition itself. No measurement methodology or alternative framing changes the structural necessity. This is the gold standard mountain classification.
constraint_indexing:constraint_classification(nowhere_dense_sets, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: CONSTRUCTIVE MATHEMATICS MOVEMENT (MOUNTAIN) — Even in constructive mathematics, which rejects the law of excluded middle, the topological constraints on nowhere dense sets persist. Constructive approaches build alternative frameworks but cannot escape the fundamental logical structure. The constraint appears across all valid mathematical systems.
constraint_indexing:constraint_classification(nowhere_dense_sets, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nowhere_dense_sets_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(nowhere_dense_sets, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nowhere_dense_sets, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nowhere_dense_sets, ExtMetricName, E),
    domain_priors:suppression_score(nowhere_dense_sets, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nowhere_dense_sets),
    narrative_ontology:constraint_metric(nowhere_dense_sets, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nowhere_dense_sets, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nowhere_dense_sets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract value from any agent — it simply defines a boundary of what is possible in topological space. The low value reflects that this is pure logical necessity, not extraction. Suppression (0.02): Minimal. There are no alternatives to suppress — the constraint is not preventing access to some hidden option. Accessibility collapse (0.95): Maximum. No agent can access a state where nowhere dense sets have dense closure. Theater ratio (0.15): Minimal. The definition of nowhere density requires no performative ritual or institutional maintenance — it follows directly from set-theoretic definitions. The small non-zero value reflects only the minimal cost of formal exposition.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All perspectives converge on the mountain classification. This is the signature of a true natural law constraint: the logical necessity is invariant across all observer positions, time horizons, and power levels. Even the constructive mathematics perspective, which challenges classical assumptions, encounters the same topological structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to mountain constraints. There are no beneficiaries or victims because the constraint does not extract from anyone — it simply defines mathematical reality. All agents experience the same constraint equally: as an unchangeable boundary of what is logically possible.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is mandatrophy-resolved by definition: all six types cannot be instantiated because the constraint is universally mountain. There is no temptation to mislabel it as extraction (snare) or coordination (rope) — the logical necessity is transparent. The absence of perspectival variation is not a problem for the framework; it is exactly what we expect from true mathematical laws. The constraint serves as a diagnostic control case for mountain classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nowhere_dense_sets, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nowhere_dense_sets, information_standard).

% DUAL FORMULATION NOTE:
% Nowhere dense sets are foundational to multiple topological theorems (Baire Category Theorem, meager sets in functional analysis). However, these theorems are downstream consequences of the nowhere density definition, not separate constraints. No decomposition into multiple stories is required — the constraint is structurally uniform across all applications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
