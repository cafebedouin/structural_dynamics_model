% ============================================================================
% CONSTRAINT STORY: peano_curve_mapping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
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
 *   constraint_id: peano_curve_mapping
 *   human_readable: Peano Space-Filling Curve
 *   domain: mathematical/topology
 *
 * SUMMARY:
 *   The Peano space-filling curve is a foundational result in topology,
 *   discovered by Giuseppe Peano in 1890. It is a continuous function from
 *   the unit interval [0,1] onto the unit square [0,1]². This curve
 *   exemplifies a mathematical mountain: a proven, invariant property of
 *   continuous mappings that holds regardless of observer position,
 *   measurement framework, or time horizon. Unlike constraints that involve
 *   extraction or suppression of alternatives, the Peano curve is a pure
 *   mathematical fact. Its existence imposes no coercive overhead, no
 *   asymmetric extraction, and no performative theater—it simply is what it
 *   is. All agents (mathematicians, engineers, students, institutions,
 *   communities) encounter the same invariant structure. The constraint is
 *   universal and timeless.
 *
 * KEY AGENTS:
 *   - Pure mathematicians: Discover and prove the curve exists (beneficiary of fundamental knowledge)
 *   - Computer scientists: Use space-filling curves for spatial indexing and data structure design (beneficiary of practical algorithmic tools)
 *   - Applied engineers: Implement Peano variants in graphics, compression, and database systems (beneficiary of proven techniques)
 *   - Mathematical institutions: Canonize and preserve the proof in peer review and publication (neutral institutional role)
 *   - Open-source developers: Implement algorithms based on the curve's properties (beneficiary of published knowledge)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peano_curve_mapping, 0.08).
domain_priors:suppression_score(peano_curve_mapping, 0.02).
domain_priors:theater_ratio(peano_curve_mapping, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peano_curve_mapping, extractiveness, 0.08).
narrative_ontology:constraint_metric(peano_curve_mapping, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(peano_curve_mapping, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(peano_curve_mapping, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(peano_curve_mapping, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peano_curve_mapping, mountain).
narrative_ontology:human_readable(peano_curve_mapping, "Peano Space-Filling Curve").
narrative_ontology:topic_domain(peano_curve_mapping, "mathematical/topology").

domain_priors:emerges_naturally(peano_curve_mapping).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PURE MATHEMATICIAN (MOUNTAIN) — Peano's construction is a proven mathematical fact: a continuous function f: [0,1] → [0,1]² with image equal to the entire unit square. This is not contingent on measurement, framework, or observer position. The mapping is an invariant property of topology. No degrees of freedom exist. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED TECHNOLOGIST (MOUNTAIN) — Engineers implementing space-filling curve algorithms (Hilbert, Peano variants) for spatial data indexing, graphics rendering, or image compression encounter the same fixed property: the curve's topology is invariant. Whether one uses Peano or Hilbert curves, the fundamental constraint—that continuous 1D→2D mapping is possible—remains. d≈0.48, f(d)≈0.60, σ=1.1 → χ≈0.05.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPUTER SCIENCE STUDENT (MOUNTAIN) — Learning spatial data structures and computational geometry, the student encounters Peano curves as a fundamental mathematical fact. The curve's existence and properties cannot be bargained with, negotiated, or avoided. Its constraint on how 1D→2D mappings must behave is absolute. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MATHEMATICAL INSTITUTION (MOUNTAIN) — Topology departments, publishing bodies, and mathematical societies canonize the Peano curve as a proven theorem. The proof is invariant across all institutions and computational platforms. No institutional actor can reframe or redefine the mapping without abandoning mathematics itself. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: OPEN-SOURCE IMPLEMENTATION COMMUNITY (MOUNTAIN) — Developers implementing space-filling curve algorithms across platforms (graphics libraries, database indices, image processing) all converge on the same topological fact. Whether the code is proprietary or open, the mathematical constraint is invariant. Communities cannot coordinate their way out of Peano's theorem. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.03.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peano_curve_mapping_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(peano_curve_mapping, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peano_curve_mapping, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(peano_curve_mapping, ExtMetricName, E),
    domain_priors:suppression_score(peano_curve_mapping, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(peano_curve_mapping),
    narrative_ontology:constraint_metric(peano_curve_mapping, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(peano_curve_mapping, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(peano_curve_mapping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.08): Near-zero. The Peano curve is a mathematical fact, not a mechanism for extraction. No agent gains advantage over another through the curve's existence. The constraint imposes no costs on any group. Suppression (0.02): Minimal. Alternative formulations (Hilbert curves, Z-order curves) are all possible within the same mathematical framework. The curve's existence does not suppress competing approaches; it simply establishes a baseline property of continuous 1D→2D mappings. Theater ratio (0.05): Negligible. The proof is direct, formal, and non-performative. There is no ritual, no symbolic action, no proxy metric. The mathematics is its own verification. Accessibility collapse (0.92): Extremely high. The constraint is maximally accessible—the proof is available in every topology textbook and is independent of measurement apparatus, cultural context, or computational platform. No mechanism hides the constraint. Resistance (0.08): Very low. No force resists understanding the curve's existence. The curve is not suppressed by institutional power or cognitive bias. It is widely taught and cited. Emerges naturally (true): The curve is a logical consequence of the definition of continuity and the compactness of the unit interval. It emerges from first principles.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All five perspectives classify the constraint as a mountain. This is the defining property of a uniform-type constraint: the structural data (ε=0.08, suppression=0.02, theater=0.05, accessibility_collapse=0.92, resistance=0.08) are invariant across all observers. The pure mathematician, the technologist, the student, the institution, and the developer all encounter the same mathematical object with the same properties. The classification does not depend on power position, time horizon, exit options, or spatial scope. This invariance is what makes it a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive low d values and low/negative χ values because there is no extraction. The constraint does not benefit one agent at the expense of another. Pure mathematicians (d≈0.72) see a discovered fact, not a mechanism. Applied technologists (d≈0.48) use the fact as a tool. Students (d≈0.65) learn from it. Institutions (d≈0.05) canonize it. Communities (d≈0.40) implement from it. None of these relationships involve extraction. The directionality is nearly neutral across all perspectives because the constraint is a natural law, not a power relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   PURE MOUNTAIN: The Peano curve resolves mandatrophy trivially. There is no risk of misclassifying coordination as extraction or vice versa because there is no extraction, no coordination mechanism, and no institutional arrangement. The constraint is a logical fact. Mandatrophy requires a choice between types when structural data are ambiguous (e.g., Rope vs Snare). Here there is no ambiguity—all indices produce Mountain. The constraint's universality and invariance prevent any perspectival misclassification. This is the gold standard for mountain verification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_paradox_intentionality,
    'Is the apparent ''gap'' between Peano''s mathematical impossibility (prior intuition) and the curve''s existence (after proof) a property of the constraint itself or purely a property of human expectation?',
    'Formalization of what ''measuring'' the curve means—the impossibility was epistemic, not ontological. Peano proved the curve exists; later mathematicians proved continuous bijections from 1D to 2D are impossible (Brouwer''s Invariance). The apparent paradox dissolves once measurement basis (existence vs bijection) is specified.',
    'If measurement-dependent: the constraint might admit observer-relative aspects (not a true mountain). If ontologically fixed: the mountain classification is confirmed and the earlier intuition-paradox is purely epistemic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_paradox_intentionality, conceptual, 'Whether the impossibility-to-existence gap is measurement-dependent').

omega_variable(
    computational_realizability_ceiling,
    'Does the theoretical existence of the Peano curve imply a constraint on real computational space-filling algorithms, or is the theoretical limit sufficiently abstract that implementable approximations bypass it entirely?',
    'Analysis of Hausdorff dimension and self-similarity properties of finite-iteration approximations. Comparison of error bounds for Peano approximations vs alternatives (Hilbert, Z-order curves) in practical applications.',
    'If implementable: the mountain might have practical ''escape routes'' through approximation (weakening the universality). If truly limiting: practical algorithms are structurally constrained by the Peano existence proof.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_realizability_ceiling, empirical, 'Whether Peano existence implies computational constraints on practical algorithms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peano_curve_mapping, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peano_tr_t0, peano_curve_mapping, theater_ratio, 0, 0.05).
narrative_ontology:measurement(peano_tr_t75, peano_curve_mapping, theater_ratio, 75, 0.05).
narrative_ontology:measurement(peano_tr_t150, peano_curve_mapping, theater_ratio, 150, 0.05).

% Extraction over time
narrative_ontology:measurement(peano_be_t0, peano_curve_mapping, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(peano_be_t75, peano_curve_mapping, base_extractiveness, 75, 0.08).
narrative_ontology:measurement(peano_be_t150, peano_curve_mapping, base_extractiveness, 150, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peano_curve_mapping, information_standard).
narrative_ontology:affects_constraint(peano_curve_mapping, hausdorff_dimension_constraint).
narrative_ontology:affects_constraint(peano_curve_mapping, space_filling_curve_universality).

% DUAL FORMULATION NOTE:
% Peano's curve is a member of the space-filling curve family, upstream of specific implementation constraints (Hilbert curves, Z-order curves). The existence proof (ε=0.08, Mountain) is distinct from questions about optimal indexing (Hilbert efficiency) or practical approximations (finite-iteration convergence). Peano establishes the baseline existence; downstream constraints build on this foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
