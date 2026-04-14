% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_of_calculus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_theorem_of_calculus, []).

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
 *   constraint_id: fundamental_theorem_of_calculus
 *   human_readable: Fundamental Theorem of Calculus
 *   domain: mathematics/analysis
 *
 * SUMMARY:
 *   The Fundamental Theorem of Calculus (FTC) is the mathematical constraint
 *   establishing that differentiation and integration are inverse operations.
 *   For a continuous function f on an interval [a,b], the theorem states that
 *   ∫[a to b] f(x)dx = F(b) - F(a), where F is an antiderivative of f. This
 *   constraint exhibits the defining properties of a Mountain: it emerges
 *   naturally from the logical definitions of the derivative and integral; it
 *   is invariant across all mathematical frameworks where these operations
 *   are defined; it admits zero degrees of freedom; it cannot be negotiated,
 *   relaxed, or escaped by any mathematical practice. The accessibility
 *   collapse (0.92) reflects the genuine mathematical difficulty in
 *   understanding WHY the relationship holds — the constraint is not hidden
 *   by social gatekeeping but by intrinsic logical complexity. The resistance
 *   (0.08) reflects that once properly understood, the theorem appears
 *   inevitable. No agent — student, mathematician, physicist, or institution
 *   — can operate outside this constraint. Unlike institutional constraints
 *   that extract value from some agents for the benefit of others, the FTC
 *   constrains all agents equally and imposes no net extraction. It is a
 *   coordinate system in which all mathematical practice operates, not a
 *   mechanism of asymmetric resource transfer.
 *
 * KEY AGENTS:
 *   - Mathematical practitioners (analytical/analytical): All mathematicians, physicists, engineers using calculus operate within the FTC. No beneficiary or victim — all agents are equally constrained by the same logical necessity.
 *   - Students (powerless/analytical): Learners confront the FTC as an absolute boundary condition with maximum accessibility collapse. Not oppressive (constraint is logical, not social) but genuinely difficult to penetrate.
 *   - Mathematical institutions (institutional/analytical): Universities, textbooks, pedagogical frameworks must acknowledge the FTC in every rigorous calculus curriculum. The constraint is not a social artifact that could be organized differently.
 *   - Applied scientists (analytical/analytical): Physicists, engineers, economists using calculus as a tool operate within the FTC constraint. The relationship is invariant across all applications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_theorem_of_calculus, 0.05).
domain_priors:suppression_score(fundamental_theorem_of_calculus, 0.02).
domain_priors:theater_ratio(fundamental_theorem_of_calculus, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, extractiveness, 0.05).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_theorem_of_calculus, mountain).
narrative_ontology:human_readable(fundamental_theorem_of_calculus, "Fundamental Theorem of Calculus").
narrative_ontology:topic_domain(fundamental_theorem_of_calculus, "mathematics/analysis").

domain_priors:emerges_naturally(fundamental_theorem_of_calculus).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL STRUCTURE (MOUNTAIN) — The FTC is a logical necessity flowing from the definitions of the derivative and integral. The relationship between accumulation and instantaneous change rate is invariant across all mathematical frameworks where these operations are defined. No agent can escape or modify this relationship; it is a structural feature of continuous mathematics itself.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: UNIVERSAL APPLICATION (MOUNTAIN) — From the perspective of any mathematical or physical application using calculus, the FTC is an immutable law. Engineers computing beam stress, physicists modeling electromagnetic fields, economists analyzing marginal utility — all operate within the constraint that differentiation and integration are inverse operations. This constraint has zero degrees of freedom.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: STUDENT LEARNER (MOUNTAIN) — The student confronting calculus for the first time experiences the FTC as an absolute boundary condition: integration and differentiation MUST be related in this way. The student has zero agency to modify or escape the relationship. Yet this is not oppressive because the constraint is logically necessary, not socially imposed. The accessibility collapse (0.92) reflects that understanding WHY the theorem holds requires surmounting genuine mathematical difficulty — the constraint is not hidden by social gatekeeping but by intrinsic logical complexity.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICAL INSTITUTION (MOUNTAIN) — From the perspective of universities, textbook publishers, and mathematics departments, the FTC is an invariant teaching obligation. Every rigorous calculus curriculum must establish this relationship; there is no alternative institutional arrangement that bypasses or weakens it. The constraint is not a coordination problem that institutions could solve differently — it is a fixed point that all mathematical practice must acknowledge.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_theorem_of_calculus_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, ExtMetricName, E),
    domain_priors:suppression_score(fundamental_theorem_of_calculus, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fundamental_theorem_of_calculus),
    narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fundamental_theorem_of_calculus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. The FTC imposes no asymmetric resource extraction. All agents — from powerless students to institutional mathematicians — face the same logical necessity. The slight non-zero value (not 0.00) reflects the pedagogical barrier: understanding the FTC requires time and cognitive effort, creating a brief temporal asymmetry where instructors must guide learners. This is not extraction (no value flows to instructors) but rather a genuine accessibility challenge. Suppression (0.02): Near-zero. The FTC is not suppressed by gatekeeping or institutional control. It is transparently known and taught. Alternative formulations exist (Lebesgue integral, measure theory, categorical frameworks), but all preserve the core inversion relationship. The minimal suppression reflects only that full mathematical rigor is inaccessible to complete novices — a feature of complexity, not control. Theater ratio (0.15): Very low. Mathematical proofs of the FTC are substantively functional, not performative. A rigorous proof demonstrates the logical necessity; there is no gap between appearance and reality. The slight non-zero value reflects pedagogical presentation choices: some instructors use intuitive examples before rigorous proofs. This is genuine scaffolding, not theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap across all four perspectives. All observers — analytical, powerless, institutional, applied — agree on classification (Mountain) because the FTC is logically invariant. A student struggling to understand the theorem and a Fields Medalist both operate within the same constraint. This uniformity of perspective is the defining diagnostic of a true Mountain: the logical structure that produces the classification is the same from every viewpoint. The absence of perspectival gap confirms that the constraint is not a social artifact susceptible to different interpretations from different power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality derivation does not apply to Mountains. The FTC has no beneficiaries or victims — no agent extracts value and no agent bears costs. All agents are equally constrained by a logical necessity. The derived directionality value d would be ~0.50 (symmetric) for all perspectives because there is no asymmetric resource flow. The f(d) sigmoid evaluates to ~0.65 (neutral-to-weak constraint experience), but this is not extraction — it is the experience of encountering a logical limit. No override is needed or appropriate.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy applies to the FTC. Mandatrophy is the trap of misclassifying a coordination mechanism as pure extraction (or vice versa). Mountains are exempt from mandatrophy because they have neither coordination nor extraction — they are logical necessities. The FTC cannot be confused with a Snare (which would involve asymmetric extraction), a Rope (which would involve coordination), or a Tangled Rope (which would involve both). The logical structure is transparent: differentiation and integration are inverse operations because that is what those definitions entail.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_euclidean_geometry_generalization,
    'Does the FTC hold in non-Euclidean geometric spaces and non-standard analysis frameworks with the same force as in classical real analysis?',
    'Formal proof review of FTC generalizations to Riemannian manifolds, hyperreal number systems, and categorical frameworks; assessment of whether all such generalizations preserve the core inversion relationship',
    'If true universally: Mountain classification is confirmed across all mathematical frameworks. If false (counterexamples exist): FTC is framework-dependent rather than universal natural law, potentially downgrading to Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_euclidean_geometry_generalization, empirical, 'Whether FTC generalizes invariantly to all mathematical frameworks').

omega_variable(
    computational_accessibility_bounds,
    'Is there a computational or pedagogical lower bound on how simply the FTC relationship can be explained, below which the resistance metric rises structurally?',
    'Empirical study of student comprehension with minimalist explanations; measurement of cognitive load for different proof structures; assessment of whether simplification degrades correctness',
    'If no bound exists: resistance (0.08) reflects only pedagogical choices, not structural necessity. If bound exists: resistance (0.08) captures a genuine accessibility collapse inherent to the mathematical content itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_accessibility_bounds, empirical, 'Structural lower bound on FTC pedagogical accessibility').

omega_variable(
    alternative_operator_inversion,
    'Could a mathematical framework exist in which differentiation and integration are NOT inverses, while still being internally consistent and operationally useful?',
    'Formal proof search for consistent mathematical systems where d/dx and ∫dx are non-inverse; assessment of whether such systems are logically coherent or operationally applicable',
    'If such systems exist: FTC is contingent on the choice of definitions, not logically necessary — potential downgrade to Scaffold or Rope. If impossible: confirms Mountain classification at the deepest logical level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_operator_inversion, conceptual, 'Logical necessity of the FTC inversion relationship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_theorem_of_calculus, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ftc_tr_t0, fundamental_theorem_of_calculus, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ftc_tr_t500, fundamental_theorem_of_calculus, theater_ratio, 500, 0.14).
narrative_ontology:measurement(ftc_tr_t1000, fundamental_theorem_of_calculus, theater_ratio, 1000, 0.15).

% Extraction over time
narrative_ontology:measurement(ftc_be_t0, fundamental_theorem_of_calculus, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(ftc_be_t500, fundamental_theorem_of_calculus, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(ftc_be_t1000, fundamental_theorem_of_calculus, base_extractiveness, 1000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_theorem_of_calculus, information_standard).
narrative_ontology:affects_constraint(fundamental_theorem_of_calculus, chain_rule_composition).
narrative_ontology:affects_constraint(fundamental_theorem_of_calculus, integration_by_parts).
narrative_ontology:affects_constraint(fundamental_theorem_of_calculus, divergence_theorem).
narrative_ontology:affects_constraint(fundamental_theorem_of_calculus, stokes_theorem).
narrative_ontology:affects_constraint(fundamental_theorem_of_calculus, lebesgue_measure_theory).

% DUAL FORMULATION NOTE:
% The FTC is the parent constraint enabling all higher-order calculus theorems. Generalizations (Divergence Theorem, Stokes' Theorem) are mathematically downstream, inheriting the same Mountain classification. Lebesgue measure-theoretic formulation is an alternative framework (same ε, same structure) rather than a separate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
