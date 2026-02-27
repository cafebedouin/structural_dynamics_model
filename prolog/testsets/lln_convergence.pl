% ============================================================================
% CONSTRAINT STORY: lln_convergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lln_convergence, []).

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
 *   constraint_id: lln_convergence
 *   human_readable: Law of Large Numbers (LLN)
 *   domain: mathematical/probability_theory
 *
 * SUMMARY:
 *   The Law of Large Numbers is a fundamental theorem in probability theory
 *   and mathematical statistics stating that the average of results obtained
 *   from a large number of independent, identically distributed trials
 *   converges to the expected value. This constraint is a mathematical
 *   natural law: it emerges from the axioms of probability theory and applies
 *   universally across all observational contexts. No agent — individual,
 *   institutional, or analytical — can circumvent, extract from, or suppress
 *   the LLN's convergence guarantee. The constraint operates at the level of
 *   logical necessity. All perspectives converge on the mountain
 *   classification because the LLN has zero degrees of freedom: given
 *   independent, identically distributed trials with finite variance,
 *   convergence is guaranteed by theorem, not by institutional design or
 *   power relationship.
 *
 * KEY AGENTS:
 *   - Empiricists: Any agent conducting sampling or estimation — they confront the LLN as an immutable structural limit on convergence rates
 *   - Statisticians: Organized specialists who design experiments and interpret data; they optimize within the LLN's bounds but cannot exceed them
 *   - Policy Makers: Institutional actors relying on aggregate data — the LLN's guarantee applies uniformly regardless of their power or objectives
 *   - Mathematicians: Analysts of the formal system — the theorem is a logical consequence of probability axioms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lln_convergence, 0.08).
domain_priors:suppression_score(lln_convergence, 0.02).
domain_priors:theater_ratio(lln_convergence, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lln_convergence, extractiveness, 0.08).
narrative_ontology:constraint_metric(lln_convergence, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(lln_convergence, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lln_convergence, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lln_convergence, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lln_convergence, mountain).
narrative_ontology:human_readable(lln_convergence, "Law of Large Numbers (LLN)").
narrative_ontology:topic_domain(lln_convergence, "mathematical/probability_theory").

domain_priors:emerges_naturally(lln_convergence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Any agent attempting to estimate a population parameter through sampling confronts the LLN as an immutable structural limit. No amount of institutional power, coordination, or coercion can make finite samples converge faster than the theorem permits. The constraint is irrespective of the observer's objectives — it operates at the level of mathematical necessity.
constraint_indexing:constraint_classification(lln_convergence, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even highly organized statistical institutions (research consortia, regulatory bodies, central banks) cannot circumvent the LLN's convergence guarantee. The theorem binds all perspectives equally. Organizations can only optimize sampling design or reduce variance through better experimental control, but the fundamental convergence rate is non-negotiable.
constraint_indexing:constraint_classification(lln_convergence, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Large institutional actors (governments, corporations, central banks) rely on aggregate data to make decisions. The LLN guarantees that sufficiently large samples will stabilize their estimates. This is not a benefit they can extract or a cost they can externalize — it is a law that applies uniformly to all observers of large-scale phenomena.
constraint_indexing:constraint_classification(lln_convergence, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of formal mathematics, the LLN is a theorem derived from axioms of probability theory. It has zero degrees of freedom — it is not a constraint that can be negotiated, evaded, or strategically managed. The convergence is a logical consequence of the probability measure and the independence assumption.
constraint_indexing:constraint_classification(lln_convergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lln_convergence_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lln_convergence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lln_convergence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lln_convergence, ExtMetricName, E),
    domain_priors:suppression_score(lln_convergence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lln_convergence),
    narrative_ontology:constraint_metric(lln_convergence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lln_convergence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lln_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The LLN does not extract resources, reduce agency, or benefit specific agents. It is a constraint on the convergence behavior of averages, applicable equally to all observers. The small non-zero value reflects that any mathematical statement has minimal epistemic cost to state and verify — the 'extraction' is the intellectual labor of understanding the proof. Suppression (0.02): Negligible. No agent can suppress or hide the LLN's operation. It is transparent, universally applicable, and mathematically proven. The tiny residual reflects only the historical fact that the theorem was not fully formalized until the 19th century — once known, it cannot be unknown. Theater ratio (0.05): Negligible. The LLN has no performative dimension. Its operation is automatic and independent of any observer's intentions or institutional structures. The minimal value reflects only the inevitable gap between the theorem's statement and its infinite number of applications.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on mountain classification. This is the characteristic signature of a true natural law. The empiricist, statistician, policy maker, and mathematician all observe the same immutable constraint. There is no perspectival disagreement, no institutional variation, no power-dependent exit option. The LLN applies identically to the richest institutional actor and the most powerless individual conducting a simple coin-flip experiment. The absence of perspectival gap is itself the proof of mountain status: a constraint that would yield different classifications from different power levels is not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is not applicable to this constraint. Mountains have no beneficiaries or victims — the constraint applies symmetrically to all observers. There is no extraction flow, no asymmetric cost-benefit structure, and no power-dependent variation in how agents experience the constraint. All agents (powerless and institutional alike) experience identical convergence guarantees and identical rate limitations. The absence of directional asymmetry is a defining feature of the mountain type.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not face mandatrophy because it is a uniform-type mountain (natural law only). All perspectives yield the same classification. The mandatrophy would arise only if we tried to argue that the LLN was a coordination mechanism (rope), an institutional extraction device (snare), or a temporary support structure (scaffold) from some perspective — these framings would be category errors because the LLN has no institutional, power-dependent, or temporal aspect. The theorem applies as long as its premises (independence, identical distribution, finite variance) hold, regardless of any institutional context. The risk is not mandatrophy but the omega uncertainties: if the premises fail in a real-world system (e.g., trials are correlated, or variance is infinite), the mountain classification is overturned by mathematical fact, not by perspectival reframing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independence_assumption_validity,
    'Are the trials truly independent in the real-world system being sampled?',
    'Empirical testing for autocorrelation, measurement bias, or causal coupling between trials. Statistical diagnostics (Ljung-Box test, cross-correlation analysis).',
    'If independence is violated: convergence slows or fails entirely. The mathematical constraint applies only if premises hold. A Snare may emerge if dependence is hidden (extractive suppression of correlation data).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independence_assumption_validity, empirical, 'Whether trial independence assumption holds in practice').

omega_variable(
    identically_distributed_condition,
    'Do all trials follow the same probability distribution, or does the distribution drift over time?',
    'Time-series analysis of distributional parameters; testing for regime shifts, concept drift, or non-stationary processes.',
    'If distributions are non-identical: LLN does not apply. Convergence may fail. A Tangled Rope may emerge if drift is gradual and hidden (agents extracting value through informational asymmetry about regime change).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identically_distributed_condition, empirical, 'Whether distributions remain identical across trials').

omega_variable(
    finite_variance_premise,
    'Does the underlying distribution have finite variance?',
    'Analysis of tail behavior, moment existence, and empirical distributions with extreme outliers or fat tails. Diagnostic: Pareto index estimation.',
    'If variance is infinite (e.g., Cauchy distribution): LLN does not apply in its standard form. Convergence fails. The mountain is conditional on mathematical premises that may not hold empirically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finite_variance_premise, empirical, 'Whether the distribution has finite variance').

omega_variable(
    computational_feasibility_of_limit,
    'For practical finite samples, how close is the constraint to its asymptotic form?',
    'Rate-of-convergence analysis (Berry-Esseen bounds, empirical convergence speed); comparison of theoretical asymptotic behavior vs finite-sample behavior.',
    'If convergence is extremely slow: the mountain appears empirically as a constraint with high theater (agents may fake convergence, use data-massaging, or exploit the slow approach to the limit). This is conceptual rather than changing the mathematical classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_feasibility_of_limit, empirical, 'Rate of convergence to the limit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lln_convergence, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lln__tr_t0, lln_convergence, theater_ratio, 0, 0.04).
narrative_ontology:measurement(lln__tr_t50, lln_convergence, theater_ratio, 50, 0.05).
narrative_ontology:measurement(lln__tr_t100, lln_convergence, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(lln__be_t0, lln_convergence, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(lln__be_t50, lln_convergence, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(lln__be_t100, lln_convergence, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lln_convergence, information_standard).
narrative_ontology:affects_constraint(lln_convergence, central_limit_theorem).
narrative_ontology:affects_constraint(lln_convergence, statistical_consistency_estimators).
narrative_ontology:affects_constraint(lln_convergence, weak_law_large_numbers).

% DUAL FORMULATION NOTE:
% The Law of Large Numbers is the foundational constraint for all statistical inference. Downstream constraints (Central Limit Theorem, consistency of estimators) depend on the LLN's convergence guarantee. The LLN itself depends on the mathematical premises: independence, identical distribution, and finite variance. Each premise is a separate empirical uncertainty (captured in omegas) that can cause real-world systems to violate the mountain classification, but the mathematical theorem itself is irrevocable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
