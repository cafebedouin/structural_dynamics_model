% ============================================================================
% CONSTRAINT STORY: normal_approximation_conditions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_normal_approximation_conditions, []).

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
 *   constraint_id: normal_approximation_conditions
 *   human_readable: Normal Approximation Conditions in Statistical Inference
 *   domain: mathematical_statistics/foundational_limits
 *
 * SUMMARY:
 *   The normal approximation conditions represent a fundamental mathematical
 *   constraint on statistical inference. The Central Limit Theorem
 *   establishes that the sample mean approaches a normal distribution as
 *   sample size increases, but this convergence is not instantaneous and
 *   depends critically on: (1) sample size n being 'sufficiently large'
 *   (threshold varies by parent distribution shape), (2) the parent
 *   distribution having finite second moment (or at minimum satisfying
 *   Lindeberg conditions), and (3) samples being drawn independently. These
 *   conditions define the boundary between regimes where normal approximation
 *   is valid and where it fails. This constraint is invariant across all
 *   statistical schools and practical contexts — no amount of institutional
 *   pressure, methodological innovation, or policy reframing changes the
 *   mathematical fact that small samples from skewed or heavy-tailed
 *   distributions violate normality assumptions. The constraint appears as a
 *   mountain from every structural position because it is a mathematical
 *   theorem, not a policy or institutional arrangement.
 *
 * KEY AGENTS:
 *   - Applied Statistician: Powerless/trapped — must accept the constraint as immutable; no exit from the requirement that sample sizes be sufficiently large for normal approximation
 *   - Applied Researcher: Moderate/constrained — can choose alternative methods (t-distribution, nonparametric tests) but cannot eliminate the underlying mathematical constraint
 *   - Statistical Methods Institute: Institutional/arbitrage — can develop alternative inference systems (Bayesian, robust statistics, bootstrap) but these do not contradict or escape the normal approximation condition; they are independent inference pathways
 *   - Mathematical Analyst: Analytical/analytical — observes the constraint as a theorem with no structural ambiguity or observational relativity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(normal_approximation_conditions, 0.18).
domain_priors:suppression_score(normal_approximation_conditions, 0.03).
domain_priors:theater_ratio(normal_approximation_conditions, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(normal_approximation_conditions, extractiveness, 0.18).
narrative_ontology:constraint_metric(normal_approximation_conditions, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(normal_approximation_conditions, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(normal_approximation_conditions, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(normal_approximation_conditions, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(normal_approximation_conditions, mountain).
narrative_ontology:human_readable(normal_approximation_conditions, "Normal Approximation Conditions in Statistical Inference").
narrative_ontology:topic_domain(normal_approximation_conditions, "mathematical_statistics/foundational_limits").

domain_priors:emerges_naturally(normal_approximation_conditions).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: APPLIED STATISTICIAN WITH SMALL N (MOUNTAIN) — No escape from the constraint that small sample sizes violate normality assumptions. The limit exists as an invariant: below certain thresholds, the normal approximation fails universally. This is not a policy choice or institutional arrangement — it is a structural limit of statistical inference itself.
constraint_indexing:constraint_classification(normal_approximation_conditions, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED RESEARCHER (MOUNTAIN) — Even with moderate resources and some agency, the constraint is immutable. One can use alternative methods (t-distribution, permutation tests, bootstrapping) but cannot eliminate the gap between the small-sample reality and the normal model. The constraint persists regardless of motivation or institutional pressure.
constraint_indexing:constraint_classification(normal_approximation_conditions, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MATHEMATICAL ANALYST (MOUNTAIN) — The Central Limit Theorem and its failure conditions are mathematical facts independent of observer position. The constraint that sample size must be 'sufficiently large' for normality to hold is a theorem, not a convention. From the civilizational/universal analytical viewpoint, this is the deepest mountain: not just practically immutable but logically necessary.
constraint_indexing:constraint_classification(normal_approximation_conditions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: STATISTICAL METHODS INSTITUTE (MOUNTAIN) — Even institutional actors with resources and exit options cannot circumvent this limit. They can develop alternative methods (robust statistics, nonparametric approaches) but cannot eliminate the fundamental constraint that normal approximation requires conditions on sample size, distribution shape, and independence. The alternative methods DO NOT contradict the normal approximation limit — they escape it through different mechanisms (rank-based statistics, resampling) while leaving the underlying mathematical constraint intact.
constraint_indexing:constraint_classification(normal_approximation_conditions, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(normal_approximation_conditions_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(normal_approximation_conditions, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(normal_approximation_conditions, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(normal_approximation_conditions, ExtMetricName, E),
    domain_priors:suppression_score(normal_approximation_conditions, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(normal_approximation_conditions),
    narrative_ontology:constraint_metric(normal_approximation_conditions, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(normal_approximation_conditions, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(normal_approximation_conditions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The constraint does not extract from any agent in the sense of concentrating benefits or imposing asymmetric costs. All agents face the same mathematical requirement. Some agents have resources to develop alternative methods, but this does not constitute extraction — it is differential access to tools, not differential imposition of the constraint itself. The extractiveness value reflects that the constraint is a pure structural limit with no distributional asymmetry. Suppression (0.03): Minimal. The constraint does not suppress alternatives through force or legal prohibition. Applied researchers can and do use t-distributions, nonparametric tests, permutation tests, bootstrap methods, and Bayesian approaches when normal approximation conditions are violated. The suppression value reflects only that no alternative can eliminate the underlying mathematical fact. Theater ratio (0.15): Very low. The constraint involves minimal performative content. The normal approximation condition is stated as a precise mathematical theorem (Central Limit Theorem and its variants); compliance is measured through objective criteria (sample size, moment conditions, independence); and failure is unambiguous (convergence does not occur). There is no ritual masking or performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists for this constraint. All four perspectives (powerless, moderate, institutional, analytical) classify it identically as a mountain. This is characteristic of true natural law constraints: they are invariant across observational positions because they describe necessary mathematical relationships, not contingent institutional arrangements. The constraint appears the same to the individual researcher, the resource-rich institution, and the theoretical analyst because it IS the same — a theorem of probability theory.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there are no beneficiaries or victims. The normal approximation condition is not asymmetrically imposed; it applies universally to all agents doing statistical inference. No agent benefits from others violating the condition, and no agent bears costs that others escape. The constraint is perfectly symmetric: it is a structural limit of the method itself, not a distributional arrangement between agents. Canonical directionality would derive from the analytical context's default (d ≈ 0.73), but this is inapplicable because there is no meaningful extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clt_convergence_rate_ambiguity,
    'What sample size is ''sufficiently large'' for the normal approximation to apply across different parent distributions?',
    'Empirical convergence studies for specific distribution families (exponential, uniform, discrete Poisson); theoretical analysis of Berry-Esseen bounds for given distribution classes',
    'If convergence is slow for most practical cases: the constraint is tighter than often assumed, affecting more applied analyses. If convergence is rapid: the constraint affects primarily very small samples and non-standard distributions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clt_convergence_rate_ambiguity, empirical, 'Sample size thresholds for normal approximation across distribution families').

omega_variable(
    bounded_vs_unbounded_moment_distinction,
    'Does the distinction between bounded and unbounded moment distributions represent one immutable constraint or two structurally different constraints?',
    'Theoretical analysis of Lindeberg condition variants; empirical comparison of convergence rates for bounded vs heavy-tailed distributions',
    'If one constraint: mountain classification holds universally. If two distinct constraints: bounded-moment distributions might allow lower thresholds, creating a secondary Rope-type coordination problem (choosing sample size vs cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bounded_vs_unbounded_moment_distinction, conceptual, 'Whether moment boundedness creates a distinct constraint or modulates the same constraint').

omega_variable(
    dependence_structure_collapse,
    'How severe is the constraint violation when samples violate the independence assumption? Can weak dependence be treated as ''approximately'' independence for normal approximation?',
    'Convergence analysis under mixing conditions; empirical studies of dependent samples (time series, spatial data) using normal approximation; assessment of Type I error inflation rates',
    'If dependence creates fundamental constraint violation: independence is a separate mountain constraint. If weak dependence allows approximate normality: the constraint softens for correlated data, creating hybrid Rope/Mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependence_structure_collapse, empirical, 'Whether independence assumption can be relaxed to weak dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(normal_approximation_conditions, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nac_tr_t0, normal_approximation_conditions, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nac_tr_t50, normal_approximation_conditions, theater_ratio, 50, 0.15).
narrative_ontology:measurement(nac_tr_t100, normal_approximation_conditions, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(nac_be_t0, normal_approximation_conditions, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(nac_be_t50, normal_approximation_conditions, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(nac_be_t100, normal_approximation_conditions, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(normal_approximation_conditions, information_standard).
narrative_ontology:affects_constraint(normal_approximation_conditions, central_limit_theorem_validity).
narrative_ontology:affects_constraint(normal_approximation_conditions, small_sample_t_distribution).
narrative_ontology:affects_constraint(normal_approximation_conditions, nonparametric_statistical_inference).

% DUAL FORMULATION NOTE:
% The normal approximation conditions are upstream of several dependent constraints. The validity of the Central Limit Theorem itself is a mathematical theorem (mountain). The practical problem of inference with small samples is a coordination/method-selection problem (rope/scaffold) that arises GIVEN the normal approximation constraint. Nonparametric methods represent alternative inference pathways that bypass rather than contradict the normal approximation requirement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
