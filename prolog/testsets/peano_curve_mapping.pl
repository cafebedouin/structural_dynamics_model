% ============================================================================
% CONSTRAINT STORY: peano_curve_mapping
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_peano_curve_mapping, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: peano_curve_mapping
 *   human_readable: Peano Space-Filling Curve
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   The Peano Curve is the first discovered space-filling curve, a continuous
 *   mapping from a one-dimensional interval onto a two-dimensional square. It
 *   represents a fundamental constraint on the relationship between dimension
 *   and cardinality, proving that a 1D line can "occupy" all points in a 2D space.
 *   This is a mathematical fact, an unchangeable feature of topology.
 *
 * KEY AGENTS (by structural relationship):
 *   - Classical Dimension Intuition (Victim): The observer's common-sense
 *     distinction between 1D and 2D, which is invalidated by the mapping.
 *   - Database Architects (Beneficiary): Agents using space-filling logic
 *     (e.g., Z-order or Hilbert curves) as a "Rope" for data locality.
 *   - A Point on the Curve (Target): A powerless agent whose position in 2D space
 *     is strictly determined by its 1D "time" parameter.
 *   - Analytical Observer: Sees the full mathematical structure and its
 *     applications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: 0.25. The curve "extracts" 1D locality (points that are close
% in 1D may be far in 2D) to satisfy the space-filling requirement. This is
% a structural cost of the mapping, at the ceiling for a Mountain.
domain_priors:base_extractiveness(peano_curve_mapping, 0.25).

% Rationale: 0.05. A mathematical theorem does not suppress alternatives in a
% coercive sense; it simply is. The alternatives (a world where such a mapping
% is impossible) do not exist. The suppression score is at the ceiling for a
% Mountain classification.
domain_priors:suppression_score(peano_curve_mapping, 0.05).

% Rationale: 0.01. The constraint is a formal mathematical truth with
% substantive applications and near-zero performative component.
domain_priors:theater_ratio(peano_curve_mapping, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peano_curve_mapping, extractiveness, 0.25).
narrative_ontology:constraint_metric(peano_curve_mapping, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(peano_curve_mapping, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(peano_curve_mapping, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(peano_curve_mapping, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(peano_curve_mapping, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(peano_curve_mapping).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Although the claim is Mountain, the use of the curve as a tool creates
% beneficiary/victim dynamics in application, justifying these declarations.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(peano_curve_mapping, spatial_database_architects).
narrative_ontology:constraint_beneficiary(peano_curve_mapping, image_compression_designers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(peano_curve_mapping, classical_dimension_intuition).

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

% PERSPECTIVE 1: A POINT ON THE CURVE (MOUNTAIN)
% For a point moving along the curve, the mapping is an unyielding law.
% If it is at parameter 't', it *must* be at coordinate (x,y). There are zero
% degrees of freedom.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE GIS ENGINEER (ROPE)
% For the engineer, the space-filling curve is a "Rope"—a tool for functional
% coordination. It allows them to "pull" 2D spatial data into a 1D
% linear order for efficient indexing and range searches.
constraint_indexing:constraint_classification(peano_curve_mapping, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: LOCALITY PRESERVATION (ROPE)
% The loss of locality is not pure extraction (Snare) but a structural cost
% of the coordination function. The curve trades perfect 1D locality for
% manageable 2D locality. This is a Rope, as the cost is inherent to the
% coordination benefit.
constraint_indexing:constraint_classification(peano_curve_mapping, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a purely mathematical standpoint, the existence of the curve is a
% fundamental, unchangeable fact of topology. Its properties are fixed.
% The metrics (ε=0.25, suppression=0.05) and natural emergence firmly place
% it in the Mountain category, which overrides any χ-based classification.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peano_curve_mapping_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the abstract point and the engineer using it.
    constraint_indexing:constraint_classification(peano_curve_mapping, mountain, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(peano_curve_mapping, rope, context(agent_power(institutional), _, _, _)).

test(locality_coordination_cost) :-
    % A powerless data sequence (constrained) sees the loss of locality as a
    % coordination cost (Rope), not a Snare, because the base extraction is too low.
    constraint_indexing:constraint_classification(peano_curve_mapping, rope, context(agent_power(powerless), _, exit_options(constrained), _)).

test(analytical_view_is_mountain) :-
    constraint_indexing:constraint_classification(peano_curve_mapping, mountain, context(agent_power(analytical), _, _, _)).

:- end_tests(peano_curve_mapping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core of this constraint is a mathematical theorem, which is a canonical
 *   Mountain. Base extractiveness (ε=0.25) represents the structural "cost"
 *   of the mapping in terms of lost 1D locality, a property inherent to the
 *   theorem. The suppression score (0.05) reflects that a mathematical truth
 *   does not coercively suppress alternatives, as they are logically impossible.
 *   The constraint is declared as `emerges_naturally` with full accessibility
 *   collapse (1.0) and zero resistance (0.0), satisfying the Natural Law profile.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the abstract mathematical fact (Mountain) and its
 *   practical application (Rope). For a point on the curve or an analytical
 *   observer, the mapping is an unchangeable law. For an engineer, it is a
 *   tool for coordination (mapping 2D data to 1D), making it a Rope. The
 *   loss of locality is better understood as a coordination cost inherent
 *   to the Rope's function, not a form of parasitic extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `spatial_database_architects` and `image_compression_designers`
 *     use the curve's coordination properties to build efficient systems.
 *   - Victims: `classical_dimension_intuition` represents the conceptual cost.
 *     The mapping demonstrates that our intuitive understanding of dimensionality
 *     is incomplete, a form of conceptual "extraction."
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the core constraint as a Mountain, we avoid mislabeling a
 *   fundamental mathematical truth as a social construct. The perspectival
 *   shift to Rope for its application correctly identifies its function as a
 *   coordination tool, where the "extraction" of locality is a necessary
 *   trade-off for the benefit of dimensional mapping, not a parasitic feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_peano_curve_mapping,
    "Is there a 'Rope' (a different curve) that preserves locality better than Peano's original construction, making it a comparatively worse tool?",
    "Comparison of Peano vs. Hilbert vs. Morton curves using a formal 'Clustering Factor' metric.",
    "If Hilbert is proven superior for all relevant use cases, then choosing the Peano curve over Hilbert could be seen as a Piton (inertial choice) or a mild Snare (extracting performance).",
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(peano_curve_mapping, 1890, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.25) is below the
% 0.46 threshold for mandatory lifecycle drift detection.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or network data declared for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No directionality overrides are needed. The structural derivation from
% beneficiary/victim declarations and exit options is accurate.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */