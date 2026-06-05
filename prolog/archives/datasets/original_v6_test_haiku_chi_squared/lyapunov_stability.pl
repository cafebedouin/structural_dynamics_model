% ============================================================================
% CONSTRAINT STORY: lyapunov_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lyapunov_stability, []).

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
 *   constraint_id: lyapunov_stability
 *   human_readable: Lyapunov Stability Criteria
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   Lyapunov stability is a foundational concept in dynamical systems theory
 *   that characterizes whether a system returns to equilibrium after
 *   perturbation. This constraint exemplifies a pure mathematical mountain:
 *   the existence of a Lyapunov function (a scalar function that decreases
 *   along system trajectories) is logically equivalent to asymptotic
 *   stability of the equilibrium point. The criterion is
 *   observer-independent, contingency-independent, and holds with zero
 *   degrees of freedom across all measurement modalities. The mathematical
 *   structure is not socially constructed, not subject to regulatory
 *   arbitrage, not dependent on institutional power, and not verifiable
 *   through approval processes — it is verifiable through formal proof. The
 *   constraint applies uniformly to classical continuous dynamical systems
 *   (ODEs), discrete systems, hybrid systems with extensions, and even
 *   stochastic formulations through generalized Lyapunov theory. No agent can
 *   exit this constraint without abandoning the formal framework of dynamical
 *   systems altogether.
 *
 * KEY AGENTS:
 *   - Physical System: Constrained by the mathematical laws governing equilibrium behavior (powerless/trapped) — cannot violate stability criteria
 *   - Control Engineer: Institutional actor (institutional/constrained) — must design controllers within Lyapunov stability requirements; no institutional flexibility available
 *   - Research Community: Organized (organized/mobile) — can advance stability theory, define new stability concepts, but all are constrained by the foundational Lyapunov framework
 *   - Mathematical Analyst: Analytical perspective (analytical/analytical) — perceives the constraint as a logically necessary property of dynamical systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lyapunov_stability, 0.08).
domain_priors:suppression_score(lyapunov_stability, 0.02).
domain_priors:theater_ratio(lyapunov_stability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lyapunov_stability, extractiveness, 0.08).
narrative_ontology:constraint_metric(lyapunov_stability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(lyapunov_stability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lyapunov_stability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lyapunov_stability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lyapunov_stability, mountain).
narrative_ontology:human_readable(lyapunov_stability, "Lyapunov Stability Criteria").
narrative_ontology:topic_domain(lyapunov_stability, "mathematical/physical").

domain_priors:emerges_naturally(lyapunov_stability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICAL SYSTEM (MOUNTAIN) — A dynamical system at equilibrium cannot choose to violate Lyapunov stability criteria. The trajectory behavior is mathematically and physically determined. No exit option exists; the constraint is universal and immutable. d≈1.0, f(d)≈1.42, σ=1.0 → χ≈0.11.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL OBSERVER (MOUNTAIN) — Lyapunov stability is a theorem about the structural properties of dynamical systems. From the civilizational/universal analytical perspective, this is a pure mathematical fact: the existence of a Lyapunov function that decreases along trajectories is logically equivalent to stability of the equilibrium. This is not contingent on measurement, institutional arrangement, or social convention. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: CONTROL ENGINEER (MOUNTAIN) — For practitioners designing feedback control systems, Lyapunov stability is an inescapable constraint. Controllers must be designed to satisfy stability criteria or the system fails — there is no workaround, no institutional flexibility. The engineer experiences the constraint as a natural law binding their design options. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.06.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: RESEARCH COMMUNITY (MOUNTAIN) — The stability research field cannot choose to redefine stability in ways that violate Lyapunov criteria without breaking continuity with established theory. New stability concepts (input-to-state stability, exponential stability, orbital stability) are all defined in relation to the Lyapunov framework. The community has mobility within the framework but not out of it. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.03.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lyapunov_stability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lyapunov_stability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lyapunov_stability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lyapunov_stability, ExtMetricName, E),
    domain_priors:suppression_score(lyapunov_stability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lyapunov_stability),
    narrative_ontology:constraint_metric(lyapunov_stability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lyapunov_stability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lyapunov_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Lyapunov stability imposes no extraction in the DR sense — it is not a mechanism by which one agent benefits from another's constraint. The low value reflects that the constraint is purely structural: it describes what IS, not what BENEFITS. Suppression (0.02): Negligible. There is no coercive element. The constraint is not enforced by threat or penalty; it is logically intrinsic to the mathematical definition of stability. Theater ratio (0.05): Minimal. Verification of Lyapunov stability is mathematical (proof-based) rather than performative. A Lyapunov function either exists or it does not; there is no room for theatrical compliance or proxy metrics. The near-zero value reflects that the entire constraint is functional — every element is substantive verification, not ritual.
 *
 * PERSPECTIVAL GAP:
 *   Unlike the verification bottleneck exemplar, Lyapunov stability shows NO perspectival gap. All four perspectives — the physical system, the analyst, the control engineer, and the research community — perceive the constraint identically as a mountain. This uniformity is the defining signature of a true natural law: across all indexical positions, the classification is invariant. The mountain is not contingent on observer position, not dependent on exit options, not mediated by institutional power. This invariance is the mathematical constraint's strength as a gold-standard mountain in the DR corpus.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard canonical fallback applies uniformly: all power atoms map to mountain classification because ε≤0.25, suppression≤0.05, and accessibility_collapse≥0.85. No beneficiary/victim structure exists — the constraint is not extractive and imposes equal structural limits on all agents regardless of power. Directionality derivation is not needed; the constraint is beneficiary-neutral and victim-neutral.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_artifacts_in_real_systems,
    'Can measurement apparatus or observation methods create artifactual instability that violates idealized Lyapunov criteria in physical realizations?',
    'Comparison of Lyapunov analysis predictions against observed stability in real physical systems with quantified measurement precision and noise characteristics',
    'If true: the mountain status applies to the mathematical abstraction, not necessarily to any physical system. If false: the mountain extends to physical implementation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_artifacts_in_real_systems, empirical, 'Whether measurement artifacts can violate Lyapunov predictions in real systems').

omega_variable(
    nonstandard_dynamics_coverage,
    'Do Lyapunov criteria apply to all classes of dynamical systems (stochastic, delayed, hybrid, quantum), or only to deterministic ODEs?',
    'Systematic construction of extensions: stochastic Lyapunov theory (Brownian motion stability), time-delayed systems (Krasovskii functionals), quantum state stability (Lindblad equation). Verification that extensions reduce to classical Lyapunov in the classical limit.',
    'If fully generalizable: mountain status holds across all dynamics. If limited to classical deterministic ODEs: mountain is a local feature of a broader landscape that may contain constraints of other types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nonstandard_dynamics_coverage, conceptual, 'Coverage of Lyapunov criteria across all dynamical system classes').

omega_variable(
    algorithmic_decidability_of_stability,
    'For arbitrary polynomial systems, is Lyapunov stability decidable in finite time, or are there systems where Lyapunov stability is undecidable?',
    'Verification of algorithmic complexity bounds; identification of polynomial degree or dimension thresholds where decidability becomes uncomputable; proof-theoretic analysis of stability assertions in restricted logical fragments',
    'If decidable: Lyapunov stability is a computable property (mountain status preserved). If undecidable for some systems: the constraint becomes a type-dependent phenomenon (tangled with epistemology/computational limits), potentially downgrading to Snare for certain system classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_decidability_of_stability, empirical, 'Decidability of Lyapunov stability verification for polynomial systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lyapunov_stability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lyap_tr_t0, lyapunov_stability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lyap_tr_t50, lyapunov_stability, theater_ratio, 50, 0.05).
narrative_ontology:measurement(lyap_tr_t100, lyapunov_stability, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(lyap_be_t0, lyapunov_stability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(lyap_be_t50, lyapunov_stability, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(lyap_be_t100, lyapunov_stability, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lyapunov_stability, information_standard).
narrative_ontology:affects_constraint(lyapunov_stability, feedback_linearization).
narrative_ontology:affects_constraint(lyapunov_stability, asymptotic_stability_verification).
narrative_ontology:affects_constraint(lyapunov_stability, attractivity_basin_estimation).

% DUAL FORMULATION NOTE:
% Lyapunov stability is upstream of practical control verification and basin estimation constraints. The theoretical mountain status here is foundational; downstream constraints inherit its properties but may introduce institutional or epistemological complications in implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
