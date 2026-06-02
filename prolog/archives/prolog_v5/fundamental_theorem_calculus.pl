% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_calculus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_theorem_calculus, []).

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
 *   constraint_id: fundamental_theorem_calculus
 *   human_readable: The Fundamental Theorem of Calculus
 *   domain: mathematics/analysis
 *
 * SUMMARY:
 *   The Fundamental Theorem of Calculus (FTC) establishes the inverse
 *   relationship between differentiation and integration: the integral of a
 *   function's derivative recovers the original function (within specified
 *   domain constraints), and the derivative of an integral recovers the
 *   original function. This relationship is logically necessary in any
 *   mathematical framework where both operations are defined with the
 *   requisite continuity and measurability properties. The constraint is
 *   invariant across all teaching contexts, all applied domains, all
 *   mathematical formalisms, and all observational contexts. Unlike
 *   contingent institutional constraints that may vary by perspective, the
 *   FTC is unchangeable from every structural position. It is the canonical
 *   exemplar of a mountain constraint in the Deferential Realism system.
 *
 * KEY AGENTS:
 *   - Students and learners: Confront the immutable relationship without alternative—must internalize the constraint as a structural feature of valid calculus
 *   - Applied mathematicians and scientists: Work within the constraint across all domains (physics, engineering, economics, biology)—no escape from the bijectivity principle
 *   - Mathematical community: Maintains and extends formalization but cannot alter the fundamental relationship
 *   - Analytical observer: Recognizes the logical necessity across all possible extensions and generalizations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_theorem_calculus, 0.08).
domain_priors:suppression_score(fundamental_theorem_calculus, 0.02).
domain_priors:theater_ratio(fundamental_theorem_calculus, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_theorem_calculus, extractiveness, 0.08).
narrative_ontology:constraint_metric(fundamental_theorem_calculus, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fundamental_theorem_calculus, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_theorem_calculus, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fundamental_theorem_calculus, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_theorem_calculus, mountain).
narrative_ontology:human_readable(fundamental_theorem_calculus, "The Fundamental Theorem of Calculus").
narrative_ontology:topic_domain(fundamental_theorem_calculus, "mathematics/analysis").

domain_priors:emerges_naturally(fundamental_theorem_calculus).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT (MOUNTAIN) — Regardless of educational level or institutional context, any agent attempting to relate differentiation and integration encounters the same immutable constraint. The relationship is invariant across all mathematical frameworks where both operations are defined. No amount of negotiation, alternative pedagogy, or institutional bypass changes the mathematical structure.
constraint_indexing:constraint_classification(fundamental_theorem_calculus, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED MATHEMATICIAN (MOUNTAIN) — Whether solving differential equations in physics, engineering, economics, or biology, the fundamental theorem constrains all valid approaches. No applied domain can escape the relationship; it is equally immutable in every application. The constraint cannot be extracted from or negotiated with, only internalized as a structural feature of valid mathematical reasoning.
constraint_indexing:constraint_classification(fundamental_theorem_calculus, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY (MOUNTAIN) — Across all educational institutions, mathematical publication communities, and research traditions, the fundamental theorem is invariant. No organizational structure can change it. The consensus is not negotiated—it reflects the logical necessity of the relationship. The theorem persists identically whether presented in Riemann, Lebesgue, Darboux, or other formal frameworks.
constraint_indexing:constraint_classification(fundamental_theorem_calculus, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of logical analysis across all possible mathematical frameworks and all conceivable extensions, the fundamental theorem represents a necessary relationship between differentiation and integration. Its truth is invariant under all mathematical models that instantiate both operations with the requisite properties. No observable, measurement basis, or alternative formulation changes the classification.
constraint_indexing:constraint_classification(fundamental_theorem_calculus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_theorem_calculus_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fundamental_theorem_calculus, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_theorem_calculus, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fundamental_theorem_calculus, ExtMetricName, E),
    domain_priors:suppression_score(fundamental_theorem_calculus, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fundamental_theorem_calculus),
    narrative_ontology:constraint_metric(fundamental_theorem_calculus, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fundamental_theorem_calculus, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fundamental_theorem_calculus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The theorem does not extract value from agents—it does not reward or punish, benefit or harm. It is a structural relationship that all valid mathematics must respect. The minimal non-zero value reflects that the constraint requires cognitive effort to understand and apply, but this is not extraction in the DR sense (asymmetric cost distribution or coercive asymmetry). Suppression (0.02): Negligible. There are no barriers to exit because no agent is trapped by the theorem—it is simply an invariant feature of calculus that cannot be escaped by those who wish to use calculus. Once an agent accepts the framework of continuous functions and measurable spaces, the relationship is transparent. Theater ratio (0.05): Effectively zero. The presentation of the FTC in textbooks is direct and functional—proof and application. No performative layer masks the relationship. The minimal non-zero value reflects only that all mathematical presentation requires some formal language and pedagogical scaffolding, but this is not theater in the DR sense (proxy goals replacing function).
 *
 * PERSPECTIVAL GAP:
 *   NO PERSPECTIVAL GAP: This constraint exhibits zero perspectival variance. All four perspectives converge on the same classification: mountain. The student, applied mathematician, mathematical community, and analytical observer all encounter the same immutable relationship from their different structural positions. There is no gap between how powerless agents and institutional agents experience this constraint—they both experience it as unchangeable. This uniformity is the defining signature of a true natural law constraint. The absence of perspectival disagreement demonstrates that the constraint is not contingent on institutional framing, social position, or observational context.
 *
 * DIRECTIONALITY LOGIC:
 *   INAPPLICABLE FOR MOUNTAIN CONSTRAINTS: The directional machinery (d, f(d), beneficiary/victim relationships) has no application to the fundamental theorem because there is no extraction flow. No agent benefits at another's expense. No victim bears costs while beneficiaries accumulate gains. The constraint is invariant and symmetric—all agents face the same immutable relationship regardless of power level or exit options. The absence of asymmetric cost distribution is the definitive marker that this is a natural law (mountain) rather than an extractive or coordinative constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalization_across_structures,
    'Does the fundamental theorem extend identically to all mathematical structures that generalize differentiation and integration, or are there domains where the relationship fails or requires essential modification?',
    'Exhaustive analysis across generalized function spaces, non-Archimedean analysis, constructive mathematics, and category-theoretic formulations. Identification of domains where the bijectivity or equivalence relationship breaks down.',
    'If extends identically: mountain classification is confirmed universally. If domains exist where the relationship fails: constraint decomposes into separate stories (one for standard calculus as mountain, one for generalized domains as rope or tangled_rope). This would contradict the current universal mountain claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalization_across_structures, empirical, 'Whether the fundamental theorem generalizes identically across all mathematical frameworks').

omega_variable(
    intuitionistic_constructive_status,
    'In intuitionistic and constructive mathematics where the law of excluded middle is rejected, does the fundamental theorem retain its mountain status or does it degrade to rope (proof-dependency) or snare (constructive barriers)?',
    'Formal analysis of the fundamental theorem in constructive analysis frameworks; comparison of proof requirements and classical vs constructive computational equivalence.',
    'If mountain in constructive frameworks: constraint is truly universal. If weaker or context-dependent: the mountain classification may be specific to classical logic, requiring decomposition into separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intuitionistic_constructive_status, empirical, 'Status of the fundamental theorem in constructive and intuitionistic mathematics').

omega_variable(
    physical_realizability_gap,
    'The fundamental theorem is logically necessary in mathematics, but is there a structural distinction between the mathematical constraint (provably immutable) and the physical realizability of measurement-based verification of the relationship in empirical contexts?',
    'Analysis of measurement precision, computational approximation, and the gap between abstract theorem and empirical instantiation. Determine whether the ''constraint'' experienced by a physicist applying the theorem is truly mountain or is partly constrained by measurement/computation limits.',
    'If the constraint is truly universal and immutable: no distinction. If physical instantiation introduces contingent barriers: the constraint family decomposes (mathematical theorem = mountain, physical application = tangled_rope with suppression from measurement precision limits).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_realizability_gap, conceptual, 'Distinction between mathematical necessity and physical realizability of the fundamental theorem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_theorem_calculus, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fund_tr_t0, fundamental_theorem_calculus, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fund_tr_t50, fundamental_theorem_calculus, theater_ratio, 50, 0.05).
narrative_ontology:measurement(fund_tr_t100, fundamental_theorem_calculus, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(fund_be_t0, fundamental_theorem_calculus, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fund_be_t50, fundamental_theorem_calculus, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(fund_be_t100, fundamental_theorem_calculus, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_theorem_calculus, information_standard).

% DUAL FORMULATION NOTE:
% The Fundamental Theorem of Calculus is not decomposable into separate constraint stories because the ε-invariance principle does not apply. There is a single, unified mathematical relationship—not multiple structurally distinct claims that happen to share a label. The theorem is true identically across all observables and measurement methodologies. Attempting to decompose FTC by 'measuring it differently' does not produce alternative ε values because there are no alternative ways to measure a logical necessity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
