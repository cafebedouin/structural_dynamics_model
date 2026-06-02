% ============================================================================
% CONSTRAINT STORY: information_bottleneck_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_bottleneck_principle, []).

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
 *   constraint_id: information_bottleneck_principle
 *   human_readable: Information Bottleneck Principle
 *   domain: information_theory/mathematics
 *
 * SUMMARY:
 *   The Information Bottleneck (IB) Principle, formulated by Tishby and
 *   Schwartz-Ziv, states a mathematical necessity: given three variables —
 *   data X, a compressed representation T, and a target variable Y — there
 *   exists an irreducible tradeoff between compression (minimizing mutual
 *   information I(X;T)) and prediction accuracy (maximizing I(T;Y)). This
 *   constraint is not institutional, not contingent on measurement
 *   methodology, and not subject to reform or negotiation. It appears as a
 *   mountain from all perspectives because it names a property of the
 *   mathematical structure of information itself. The principle holds
 *   universally across discrete and continuous cases, across all possible
 *   compression algorithms, and across all observable bases. No agent —
 *   whether powerless data compressor or institutional optimizer — can
 *   violate it. The extraction and suppression metrics are essentially zero
 *   because the constraint does not extract resources from anyone: it
 *   describes what is mathematically impossible, not what is institutionally
 *   forced.
 *
 * KEY AGENTS:
 *   - Data Compressor: Any agent seeking to reduce data dimensionality while preserving predictive information — subject to the tradeoff but not exploited by it
 *   - Channel Engineer: Any agent transmitting information through a bandwidth-limited channel — faces the same irreducible limit
 *   - Machine Learning Practitioner: Implements algorithms constrained by the IB principle without experiencing extraction in the DR sense
 *   - Theoretical Observer: Recognizes the principle as a logical consequence of information-theoretic definitions
 *   - Analytical Observer: Views the principle as a fundamental mathematical constant, akin to logical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_bottleneck_principle, 0.12).
domain_priors:suppression_score(information_bottleneck_principle, 0.03).
domain_priors:theater_ratio(information_bottleneck_principle, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_bottleneck_principle, extractiveness, 0.12).
narrative_ontology:constraint_metric(information_bottleneck_principle, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(information_bottleneck_principle, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(information_bottleneck_principle, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(information_bottleneck_principle, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_bottleneck_principle, mountain).
narrative_ontology:human_readable(information_bottleneck_principle, "Information Bottleneck Principle").
narrative_ontology:topic_domain(information_bottleneck_principle, "information_theory/mathematics").

domain_priors:emerges_naturally(information_bottleneck_principle).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA COMPRESSOR (MOUNTAIN) — Any agent attempting to compress data while preserving information about a target variable faces an irreducible tradeoff: compression reduces mutual information with the target. This is not contingent policy or institutional arrangement. No escape exists at any scale or time horizon.
constraint_indexing:constraint_classification(information_bottleneck_principle, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CHANNEL ENGINEER (MOUNTAIN) — Attempting to transmit data through a limited channel while preserving fidelity to a target distribution encounters the same mathematical necessity: you cannot exceed the information-theoretic rate limit without sacrificing accuracy. The constraint is imposed by mathematics itself, not by institutional design.
constraint_indexing:constraint_classification(information_bottleneck_principle, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THEORETICAL OBSERVER (MOUNTAIN) — From the institutional mathematical perspective, the information bottleneck is a fundamental theorem: the functional relationship between compression ratio, channel capacity, and target prediction accuracy is logically derived. No institutional arrangement, measurement methodology, or observational framework can circumvent the derivation.
constraint_indexing:constraint_classification(information_bottleneck_principle, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — At the universal/civilizational timescale, the information bottleneck principle is a constraint of the same logical category as Gödel's Incompleteness and the Halting Problem: it names an irreducible limit on what is computationally or informationally possible. The principle holds across all observable bases and measurement frameworks.
constraint_indexing:constraint_classification(information_bottleneck_principle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_bottleneck_principle_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(information_bottleneck_principle, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_bottleneck_principle, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(information_bottleneck_principle, ExtMetricName, E),
    domain_priors:suppression_score(information_bottleneck_principle, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(information_bottleneck_principle),
    narrative_ontology:constraint_metric(information_bottleneck_principle, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(information_bottleneck_principle, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(information_bottleneck_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): This is not a constraint that extracts resources. Rather, it describes a property of what is mathematically impossible. The 0.12 value reflects the minimal structural complexity needed to state the principle — it is not zero because the principle does impose a real constraint on feasible design space, but it is far below the 0.25 threshold for mountains because the constraint imposes no asymmetric cost on any agent. All agents face the same mathematical necessity equally. Suppression (0.03): Negligible. The principle does not suppress alternatives — it clarifies what alternatives do not exist. There is no coercion mechanism, no barrier to understanding, no enforced ignorance. Theater ratio (0.02): Essentially zero. The principle has no performative component. Its truth does not depend on ritual, institutional ceremony, or social consensus. Accessibility collapse (0.92): Extremely high. Any attempt to circumvent the principle by changing how information is measured, compressed, or transmitted merely relocates the bottleneck without eliminating it. The constraint is inaccessible to manipulation. Resistance (0.08): Minimal. The principle is not resisted because it is universally recognized as logically sound once derived.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap. All four perspectives classify identically as Mountain, from all power levels, time horizons, exit options, and spatial scopes. The data compressor facing the tradeoff, the channel engineer facing bandwidth limits, the institutional optimizer, and the analytical observer all encounter the same mathematical necessity. This uniform classification is diagnostic of a true natural law constraint: it does not depend on the observer's position or interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality derivation does not apply to this mountain constraint. The principle imposes no directional extraction — it does not benefit one agent at another's expense. All agents face identical constraints. Therefore, beneficiaries and victims are not meaningful categories. The constraint is symmetric across all positions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discrete_continuous_boundary,
    'Does the information bottleneck principle hold identically for discrete and continuous random variables, or does the continuous case involve measure-theoretic subtleties that introduce contingency?',
    'Formal comparison of the discrete mutual information derivation with continuous differential entropy treatments; analysis of whether measure-theoretic assumptions (e.g., absolute continuity) reintroduce contingent elements',
    'If discrete and continuous cases are structurally identical: principle is universal. If continuous case requires additional assumptions: the principle may be contingent on the mathematical framework chosen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discrete_continuous_boundary, conceptual, 'Whether IB principle generalizes identically across discrete and continuous domains').

omega_variable(
    computational_realizability,
    'The information bottleneck is proven as an existence theorem. Can every optimal solution be computed algorithmically, or do uncomputable optimal points exist that violate practical realizability?',
    'Analysis of the Blahut-Arimoto algorithm convergence properties; investigation of whether the theoretical optimum always lies within the computable set or whether some extreme points are uncomputable',
    'If all optima are computable: the principle is practically binding for any agent with computational resources. If some optima are uncomputable: the principle defines a limit that may be unreachable even in principle, introducing a gap between theory and practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_realizability, empirical, 'Whether optimal information bottleneck solutions are algorithmically computable').

omega_variable(
    semantic_vs_syntactic_information,
    'The information bottleneck operates on Shannon mutual information (syntactic). Does it capture constraints on semantic information, or do semantic communication channels circumvent the syntactic limit?',
    'Formal analysis of the relationship between Shannon mutual information and semantic content; exploration of whether agents using semantic/pragmatic compression could exceed Shannon limits',
    'If semantic information is constrained by the same principle: IB is truly universal. If semantic channels bypass syntactic limits: the principle applies only to syntactic/mechanical compression, not to human-like semantic communication.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(semantic_vs_syntactic_information, conceptual, 'Whether semantic information escapes information bottleneck constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_bottleneck_principle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ibp_tr_t0, information_bottleneck_principle, theater_ratio, 0, 0.01).
narrative_ontology:measurement(ibp_tr_t5, information_bottleneck_principle, theater_ratio, 5, 0.02).
narrative_ontology:measurement(ibp_tr_t10, information_bottleneck_principle, theater_ratio, 10, 0.02).

% Extraction over time
narrative_ontology:measurement(ibp_be_t0, information_bottleneck_principle, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ibp_be_t5, information_bottleneck_principle, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(ibp_be_t10, information_bottleneck_principle, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_bottleneck_principle, information_standard).
narrative_ontology:affects_constraint(information_bottleneck_principle, rate_distortion_theory).
narrative_ontology:affects_constraint(information_bottleneck_principle, computational_complexity_limits).
narrative_ontology:affects_constraint(information_bottleneck_principle, efficient_coding_theorem).

% DUAL FORMULATION NOTE:
% The information bottleneck principle is foundational to information theory and affects all downstream compression and communication constraints. Rate-distortion theory is a direct application; computational complexity limits inherit the same mathematical structure; efficient coding theorem is a corollary. These constraints are linked by information-theoretic necessity, not institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
