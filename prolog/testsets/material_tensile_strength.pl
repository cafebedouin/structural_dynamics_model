% ============================================================================
% CONSTRAINT STORY: material_tensile_strength
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-23
% ============================================================================

:- module(constraint_material_tensile_strength, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: material_tensile_strength
 *   human_readable: Ultimate Tensile Strength (UTS)
 *   domain: technological
 *
 * SUMMARY:
 *   Tensile strength is the maximum stress that a material can withstand while
 *   being stretched or pulled before breaking. It represents the fundamental
 *   cohesive limit of atomic bonding within a solid, dictating the maximum load
 *   a structure can carry per unit of cross-sectional area. This is a physical
 *   law, not a social construct, and serves as a canonical example of a
 *   Mountain constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Material Scientist: An analytical observer measuring a fixed property.
 *   - The Civil Engineer: An institutional agent designing structures around this fixed limit.
 *   - The Overloaded Component: A physical object subject to the limit (powerless/trapped).
 *   - Analytical Observer: Sees the full structure as a physical law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(material_tensile_strength, 0.02).
domain_priors:suppression_score(material_tensile_strength, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(material_tensile_strength, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(material_tensile_strength, extractiveness, 0.02).
narrative_ontology:constraint_metric(material_tensile_strength, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(material_tensile_strength, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(material_tensile_strength, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(material_tensile_strength, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(material_tensile_strength, mountain).
narrative_ontology:human_readable(material_tensile_strength, "Ultimate Tensile Strength (UTS)").

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(material_tensile_strength).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Not applicable. As a natural law (Mountain), this constraint has no
% structural beneficiaries or victims. It is an inert feature of reality.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   For Mountain constraints, χ is effectively zero regardless of perspective.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% UNIFORM-TYPE CONSTRAINT: This is a natural law, which classifies as a
% Mountain from all perspectives. The perspectival minimum is relaxed.

% PERSPECTIVE 1: THE MATERIAL SCIENTIST (MOUNTAIN)
% To the scientist, tensile strength is a Mountain. It is an unchangeable
% feature of the material's atomic "hardware."
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE STRUCTURAL ENGINEER (MOUNTAIN)
% For the engineer, the limit is still a Mountain. They don't change the limit;
% they design around it. It is a fixed feature of the landscape they must build on.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage), % Can choose different materials (mountains)
            spatial_scope(regional))).

% PERSPECTIVE 3: THE OVERLOADED COMPONENT (MOUNTAIN)
% For a cable under load, the limit is the point of failure. It is not a trap
% (Snare) but an immutable physical boundary. Reaching the boundary results in
% failure, but the boundary itself is a fixed, non-extractive Mountain.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(material_tensile_strength_tests).

test(perspectival_invariance) :-
    % Verify that as a natural law, it is a Mountain from all perspectives.
    constraint_indexing:constraint_classification(material_tensile_strength, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(material_tensile_strength, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(material_tensile_strength, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation_mountain) :-
    % Verify the metrics adhere to the Mountain classification thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(material_tensile_strength, ExtMetricName, E),
    narrative_ontology:constraint_metric(material_tensile_strength, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(material_tensile_strength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a physical law, the canonical example of a Mountain.
 * The base extractiveness (ε=0.02) and suppression (S=0.01) are set to near-zero
 * values, well within the Mountain thresholds (ε ≤ 0.25, S ≤ 0.05).
 *
 * To pass the natural law certification chain, this file includes the full
 * NL Profile:
 *   - `emerges_naturally(id)`: The constraint arises from physics, not human design.
 *   - `accessibility_collapse(1.0)`: Alternatives are physically inconceivable.
 *   - `resistance(0.0)`: Meaningful resistance is incoherent.
 *
 * Without these three declarations, the engine's mountain metric gate would not
 * fire, and the natural_law_signature certification would fail.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a physical law, the constraint is a
 *   Mountain from all perspectives, from the powerless component that fails
 *   when the limit is exceeded to the institutional engineer who designs
 *   around it.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. Directionality (d) is irrelevant for Mountain constraints
 *   because the base extractiveness (ε) is already near zero. The constraint
 *   has no structural beneficiaries or victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint serves as a baseline for what is NOT a social construct.
 *   An engineer using this law for coordination (e.g., specifying a certain
 *   grade of steel) does not change the law into a Rope; they are simply using
 *   a known Mountain as a reference point for a separate, constructed Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_metamaterial_bypass,
    "Could engineered metamaterials or nano-scale structures effectively bypass the classical tensile strength limits of bulk materials?",
    "Laboratory testing of carbon nanotube composites and graphene structures at macro-scale loads.",
    "If bypassed: The Mountain is conditional on material scale. If not: The physical law remains absolute.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(material_tensile_strength, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints. The properties of a physical
% law are constant over the interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No coordination function or network relationships are applicable for a
% fundamental physical constant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. Overrides are not needed for Mountain constraints.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */