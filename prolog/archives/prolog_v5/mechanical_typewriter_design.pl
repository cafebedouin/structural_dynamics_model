% ============================================================================
% CONSTRAINT STORY: mechanical_typewriter_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mechanical_typewriter_design, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mechanical_typewriter_design
 *   human_readable: Mechanical Typewriter Design Constraints
 *   domain: engineering/mechanical_systems
 *
 * SUMMARY:
 *   Mechanical typewriter design is constrained by immutable laws of
 *   mechanics and physics. The relationship between keystroke and character
 *   output is determined by lever mechanics, spring forces, kinetic energy
 *   transfer, and material properties. These constraints are not imposed by
 *   any agent or institution — they emerge from the physical structure of
 *   mechanical systems themselves. Every typewriter, regardless of
 *   manufacturer, era, or market position, must satisfy the same fundamental
 *   requirements: keystroke must reliably trigger hammer strike, ribbon must
 *   advance consistently, type-bars must return to home position, and paper
 *   must feed predictably. The constraint exhibits zero degrees of freedom
 *   across all dimensions — no designer can choose to violate these
 *   principles, no market force can make violation profitable, and no
 *   technological improvement can render the constraint obsolete without
 *   abandoning the mechanical medium entirely.
 *
 * KEY AGENTS:
 *   - Design Engineers: Organizational actors (institutional/trapped) — constrained by immutable mechanical principles, not by market or regulatory barriers
 *   - Manufacturing Enterprises: Institutional agents (institutional/trapped) — cannot violate mechanical constraints regardless of capital investment or production scale
 *   - Users: Distributed agents with no structural role in the constraint — benefit from reliable functionality that emerges from constraint satisfaction, not from constraint relaxation
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — sees the constraint as pure natural law with no contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mechanical_typewriter_design, 0.12).
domain_priors:suppression_score(mechanical_typewriter_design, 0.03).
domain_priors:theater_ratio(mechanical_typewriter_design, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mechanical_typewriter_design, extractiveness, 0.12).
narrative_ontology:constraint_metric(mechanical_typewriter_design, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(mechanical_typewriter_design, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mechanical_typewriter_design, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(mechanical_typewriter_design, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mechanical_typewriter_design, mountain).
narrative_ontology:human_readable(mechanical_typewriter_design, "Mechanical Typewriter Design Constraints").
narrative_ontology:topic_domain(mechanical_typewriter_design, "engineering/mechanical_systems").

domain_priors:emerges_naturally(mechanical_typewriter_design).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MECHANICAL REALITY (MOUNTAIN) — The fundamental constraints of mechanical linkage systems are immutable. Physical laws governing lever ratios, spring mechanics, and kinetic energy transfer cannot be circumvented. Any typewriter must respect these limits regardless of designer intent or market pressure.
constraint_indexing:constraint_classification(mechanical_typewriter_design, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: DESIGN ENGINEER (MOUNTAIN) — Even with optimal materials and manufacturing precision, the designer cannot escape the mathematical constraints of mechanical systems. Keystroke timing, hammer acceleration, ribbon-advance mechanics, and return-spring forces are determined by physics, not preference. Every typewriter design converges on similar solutions because the constraint is immutable.
constraint_indexing:constraint_classification(mechanical_typewriter_design, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: MANUFACTURING ENTERPRISE (MOUNTAIN) — Regardless of firm size or capital investment, the manufacturer cannot build a typewriter that violates mechanical principles. The constraint on part tolerances, assembly sequence, and field maintenance is not imposed by competitors or regulators — it emerges from the physics of the system itself.
constraint_indexing:constraint_classification(mechanical_typewriter_design, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the full structural perspective, mechanical typewriter design is governed by fixed physical laws. No observable, measurement basis, or technological breakthrough can change the fundamental constraints of mechanical systems. The constraint is natural law.
constraint_indexing:constraint_classification(mechanical_typewriter_design, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mechanical_typewriter_design_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(mechanical_typewriter_design, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mechanical_typewriter_design, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(mechanical_typewriter_design, ExtMetricName, E),
    domain_priors:suppression_score(mechanical_typewriter_design, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(mechanical_typewriter_design),
    narrative_ontology:constraint_metric(mechanical_typewriter_design, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(mechanical_typewriter_design, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(mechanical_typewriter_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint does not extract resources from any agent — it simply specifies which mechanical systems function and which do not. The value is non-zero because any constraint that forces design choices imposes some cost relative to unconstrained design space, but the extraction is negligible because the constraint is uniform across all agents. Suppression (0.03): Minimal. The constraint suppresses alternatives, but the suppression is structural, not coercive. Agents are not prevented from attempting designs that violate the constraint — they simply fail functionally. There is no enforcement mechanism because no agent controls access to the mechanical principles. Theater ratio (0.15): Very low. The constraint exhibits minimal performative content. A typewriter either functions or it does not; the functional requirement is not masked by ritual or symbolic activity.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this constraint — all perspectives converge on the same classification (mountain). This is characteristic of natural law constraints. The designer, the manufacturer, the user, and the analytical observer all perceive the same immutable limits. The constraint is invariant across all observables and measurement methodologies because it is grounded in universal physical principles that do not depend on context.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality applies to this constraint. Mountain classifications do not require beneficiary/victim declarations because the constraint is symmetric — it affects all mechanical systems identically. There is no agent toward whom extraction flows because the constraint extracts nothing. The physicality of the constraint means there is no power differential to exploit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy trivially: it is pure mountain with no hidden tangled_rope structure. There is no coordination function masked as extraction, no asymmetric extraction disguised as pure coordination. The mechanical principles are transparent and observable. The constraint satisfies all mountain gates: extractiveness ≤ 0.25, suppression ≤ 0.05, emerges_naturally = true, accessibility_collapse ≥ 0.85, resistance ≤ 0.15. The analytical observer's classification matches the powerless agent's classification, which matches the institutional agent's classification. This uniformity is the signature of natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrees_of_freedom_boundary,
    'Where exactly does the design space of mechanically-feasible typewriters end? How many topologically distinct solutions exist?',
    'Exhaustive enumeration of functional typewriter mechanisms; topological classification of viable linkage systems; determination of whether the solution set is finite or infinite',
    'If finite: the constraint is purely mathematical (mountain). If infinite: there may be hidden degrees of freedom that appear as apparent invariants but are actually contingent design choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(degrees_of_freedom_boundary, empirical, 'Boundary of mechanically-feasible typewriter design space').

omega_variable(
    user_performance_coupling,
    'Is the apparent ''naturalness'' of the QWERTY layout a consequence of mechanical constraints or of historical accident entrenched through user habit?',
    'Historical analysis of keyboard layouts developed independently in different mechanical systems; measurement of typing performance on alternative layouts implemented mechanically',
    'If mechanical necessity: layout follows from constraints (mountain). If historical accident: layout is a contingent social fact (rope/scaffold), and the constraint is weaker than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_performance_coupling, empirical, 'Coupling between mechanical constraints and keyboard layout').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mechanical_typewriter_design, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mech_tr_t0, mechanical_typewriter_design, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mech_tr_t50, mechanical_typewriter_design, theater_ratio, 50, 0.15).
narrative_ontology:measurement(mech_tr_t100, mechanical_typewriter_design, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(mech_be_t0, mechanical_typewriter_design, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(mech_be_t50, mechanical_typewriter_design, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(mech_be_t100, mechanical_typewriter_design, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mechanical_typewriter_design, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
