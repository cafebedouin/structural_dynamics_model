% ============================================================================
% CONSTRAINT STORY: statistical_consistency_estimators
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statistical_consistency_estimators, []).

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
 *   constraint_id: statistical_consistency_estimators
 *   human_readable: Statistical Consistency of Estimators Under Increasing Sample Size
 *   domain: mathematical_statistics/estimation_theory
 *
 * SUMMARY:
 *   Statistical consistency of estimators is a foundational theorem in
 *   mathematical statistics asserting that, as sample size increases, an
 *   estimator converges in probability to the true parameter being estimated.
 *   This constraint emerges from the axioms of probability theory and measure
 *   theory and is not enforced by any institutional actor, incentive
 *   structure, or policy mechanism. It is invariant across observational
 *   contexts, measurement methodologies, and application domains. No agent
 *   benefits from violating it, and no agent is victimized by it — it is a
 *   structural property of how learning from data works at asymptotic limits.
 *   The constraint exhibits all hallmarks of a mountain: zero degrees of
 *   freedom for all indices, emergence from logical necessity rather than
 *   institutional design, and universal applicability. Unlike empirical
 *   constraints subject to gaming or manipulation, the mathematical property
 *   of consistency is immune to extraction or suppression — agents cannot
 *   benefit from others' inability to verify consistency, because the
 *   property is mathematically transparent.
 *
 * KEY AGENTS:
 *   - Mathematical Theory: Source of the constraint (analytical/analytical) — consistency emerges from probability axioms, not from any actor's decision
 *   - Applied Statistician: Observer (moderate/constrained) — works within the constraint; depends on consistency for estimators to be usable, but cannot modify the constraint itself
 *   - Empirical Data Analyst: Observer (powerless/trapped) — must accept that their estimator's asymptotic behavior is determined by the data-generating process, regardless of model assumptions or intent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statistical_consistency_estimators, 0.08).
domain_priors:suppression_score(statistical_consistency_estimators, 0.02).
domain_priors:theater_ratio(statistical_consistency_estimators, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statistical_consistency_estimators, extractiveness, 0.08).
narrative_ontology:constraint_metric(statistical_consistency_estimators, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(statistical_consistency_estimators, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statistical_consistency_estimators, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(statistical_consistency_estimators, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statistical_consistency_estimators, mountain).
narrative_ontology:human_readable(statistical_consistency_estimators, "Statistical Consistency of Estimators Under Increasing Sample Size").
narrative_ontology:topic_domain(statistical_consistency_estimators, "mathematical_statistics/estimation_theory").

domain_priors:emerges_naturally(statistical_consistency_estimators).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — Statistical consistency is a mathematical theorem: an estimator θ̂ₙ is consistent if lim(n→∞) P(|θ̂ₙ - θ| > ε) = 0 for all ε > 0. This is a logical consequence of the Law of Large Numbers and convergence in probability. The constraint is not enforced by any agent or policy; it emerges from the axioms of probability and measure theory. No agent can violate it; no agent can exit it. It applies regardless of experimental context, measurement methodology, or observer position.
constraint_indexing:constraint_classification(statistical_consistency_estimators, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED STATISTICIAN (MOUNTAIN) — The practitioner who works with finite samples knows that consistency is an asymptotic property: as n increases, the estimator converges to the true parameter. This constraint holds across all domains — finance, medicine, engineering, social science. No applied statistician can escape the requirement that their estimator must be consistent, nor can they violate it through choice of method. The constraint is perceivable as invariant across all contexts and measurement regimes.
constraint_indexing:constraint_classification(statistical_consistency_estimators, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: DATA ANALYST UNDER MISSPECIFICATION (MOUNTAIN) — Even when the model is misspecified, consistency remains a constraint. If the true data-generating process differs from the assumed model, the estimator converges to a well-defined limit (the pseudo-true value), not to the assumed parameter. The analyst cannot escape this: the estimator's asymptotic behavior is determined by the mathematical structure, regardless of intent or awareness. Misspecification changes where consistency converges, not whether the constraint applies.
constraint_indexing:constraint_classification(statistical_consistency_estimators, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statistical_consistency_estimators_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(statistical_consistency_estimators, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statistical_consistency_estimators, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statistical_consistency_estimators, ExtMetricName, E),
    domain_priors:suppression_score(statistical_consistency_estimators, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statistical_consistency_estimators),
    narrative_ontology:constraint_metric(statistical_consistency_estimators, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statistical_consistency_estimators, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statistical_consistency_estimators_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint is mathematical, not institutional. There is no extraction flow — no agent gains advantage over another by restricting access to or knowledge of statistical consistency. The small non-zero value reflects that the property is asymptotic (applies at n→∞, not at finite n), so practical utility at finite samples requires additional results about convergence rates. Suppression (0.02): Negligible. The constraint is not suppressed by design or institutional force. Some practitioners may be unaware of consistency theory or may misapply it, but this is ignorance, not suppression — the constraint itself remains invariant. Theater ratio (0.05): Near-zero. The mathematical proof of consistency has no performative content; it either holds or does not. Applied researchers may perform unnecessary verification procedures (simulation studies confirming a known result), introducing mild theater, but the underlying constraint has no theatrical component. Accessibility collapse (0.92): Very high. The consistency requirement is accessible to any analyst with basic probability theory knowledge — it is not hidden behind complexity or gatekeeping. Resistance to circumvention (0.08): Very low. No valid statistical method can evade the consistency constraint; any attempt to do so either violates probability axioms or is not a valid estimator.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives perceive the constraint identically as a mountain because the constraint is truly universal and agent-independent. There is no perspectival gap. The applied statistician and data analyst perceive the same mathematical necessity as the analytical observer — they are not in different structural positions relative to the constraint. This uniformity is diagnostic of a genuine mountain: no matter where the observer stands, the constraint appears invariant and unbypassed.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint has no directionality because there are no beneficiaries or victims. Statistical consistency does not extract value from any agent or distribute value to any agent — it is a logical property of estimators that apply universally. No agent's d-value is defined because there is no extraction flow. The constraint does not require beneficiary/victim declarations because it is not an extractive or coordinative mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    finite_sample_vs_asymptotic,
    'Is the constraint the mathematical theorem (lim n→∞) or the practical utility at finite n?',
    'Formal distinction: if evaluating consistency as a mathematical property, it is a pure logical consequence (mountain). If evaluating it as a practical design principle for estimators (does this method work well in practice at n=100 or n=1000?), that is a different constraint subject to empirical measurement.',
    'If the constraint is understood as the asymptotic theorem: mountain classification confirmed. If understood as finite-sample performance: a separate practical constraint emerges with potentially higher extractiveness (depends on application domain and data availability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finite_sample_vs_asymptotic, conceptual, 'Ambiguity between asymptotic mathematical property and finite-sample practical utility').

omega_variable(
    measurability_under_model_misspecification,
    'When the data-generating process is unknown, how is ''consistency'' verified empirically?',
    'In practice, consistency is validated via bootstrap resampling, cross-validation, or comparison to known reference estimators. These empirical checks can be gamed or manipulated (publication bias toward consistent results, selective reporting of consistency metrics). The mathematical property is invariant; its empirical demonstration is not.',
    'If the constraint is the mathematical property: mountain. If the constraint is ''demonstrating consistency in scientific practice'': introduces a layer of institutional extraction through measurement choice and reporting bias, potentially degrading to snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurability_under_model_misspecification, empirical, 'Verification of consistency under model misspecification requires empirical demonstration subject to bias').

omega_variable(
    consistency_as_convergence_rate,
    'Does ''consistency'' include the rate of convergence, or only the asymptotic limit?',
    'Formal definition distinguishes consistency (lim n→∞) from rate of consistency (how fast the convergence occurs, e.g., √n or log(n)). These are mathematically distinct properties. Rate matters for finite-sample utility; limit matters for long-run behavior.',
    'Affects which estimators are considered practically useful. Two consistent estimators with different rates may produce opposite practical recommendations (fast-converging is preferred for small samples; slow-converging violates practical utility despite mathematical consistency).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consistency_as_convergence_rate, conceptual, 'Distinction between consistency (asymptotic limit) and rate of convergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statistical_consistency_estimators, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statcons_tr_t0, statistical_consistency_estimators, theater_ratio, 0, 0.02).
narrative_ontology:measurement(statcons_tr_t1, statistical_consistency_estimators, theater_ratio, 1, 0.03).
narrative_ontology:measurement(statcons_tr_t2, statistical_consistency_estimators, theater_ratio, 2, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statistical_consistency_estimators, information_standard).
narrative_ontology:affects_constraint(statistical_consistency_estimators, law_of_large_numbers).
narrative_ontology:affects_constraint(statistical_consistency_estimators, convergence_in_probability).
narrative_ontology:affects_constraint(statistical_consistency_estimators, asymptotic_normality_estimators).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
