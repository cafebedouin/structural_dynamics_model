% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_of_calculus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The Fundamental Theorem of Calculus (FTC) is a pure constraint of logical
 *   structure, not an institutional arrangement subject to institutional
 *   power dynamics. Proven rigorously by Leibniz and Newton independently and
 *   formalized by Riemann, Lebesgue, and subsequent generations of analysts,
 *   the FTC establishes that differentiation and integration are inverse
 *   operations under appropriate regularity conditions. No agent — whether
 *   mathematician, engineer, student, or institution — can circumvent this
 *   relationship without abandoning the logical system of calculus itself.
 *   The theorem is invariant across all mathematical frameworks and scales
 *   equally to all observers. The constraint's theater ratio is minimal
 *   (0.15) because the theorem's content is almost entirely functional: it
 *   makes predictions (the integral of a derivative equals the function; the
 *   derivative of an integral equals the function) that are testable and
 *   either true or false, not performative. Extractiveness remains stable at
 *   0.08 across the interval because the FTC has never become more or less
 *   extractive — its content is fixed, and its relationship to agents is
 *   symmetric.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Shared agents (analytical/arbitrage) — all mathematicians must reason through the FTC; it constrains but does not extract because it is symmetric
 *   - Academic Institution: Institutional beneficiary (institutional/arbitrage) — universities benefit from the FTC's elegance and foundational role, but this is co-constitution, not extraction
 *   - Applied Engineer: Powerful agent (powerful/mobile) — depends on FTC through numerical methods and optimization, but has agency and mobility
 *   - Student: Powerless agent (powerless/trapped) — encounters the FTC as an immutable truth; cannot exit; benefits from its logical clarity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_theorem_of_calculus, 0.08).
domain_priors:suppression_score(fundamental_theorem_of_calculus, 0.02).
domain_priors:theater_ratio(fundamental_theorem_of_calculus, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, extractiveness, 0.08).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, resistance, 0.05).

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

% PERSPECTIVE 1: MATHEMATICAL COMMUNITY (MOUNTAIN) — The FTC is a logical necessity, not a contingent institutional arrangement. All mathematical reasoning about continuous functions must pass through the relationship between differentiation and integration. No agent can exit or circumvent this constraint without abandoning calculus itself. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. The theorem binds all participants equally.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ACADEMIC INSTITUTION (MOUNTAIN) — Universities cannot escape the FTC when teaching analysis. The constraint is baked into the logical structure of calculus pedagogy. Even with alternative pedagogies or computational approaches, any rigorous treatment of continuous functions encounters this relationship. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01. The institution benefits from the theorem's elegance and its role as a foundational result, but this is no extraction — it is co-constitution.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: APPLIED ENGINEER (MOUNTAIN) — Even engineers who never explicitly invoke the FTC depend on it. Numerical integration and differentiation methods are proven correct via the theorem. Optimization, control theory, and signal processing all rest on this foundation. No engineer can opt out. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.05. The constraint is universal and symmetric.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: STUDENT (MOUNTAIN) — The student learning calculus cannot exit the FTC. It is encountered as an immutable logical truth, not a choice or institutional arrangement. The constraint is experienced as natural law: differentiation and integration are inverses because of logical structure, not because of any agent's power. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.11. Even from the most constrained position, the FTC is mountain.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

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
 *   Extractiveness (0.08): Minimal. The FTC makes no extraction from any agent because it is a pure logical relationship. It does not redistribute resources, create barriers to entry, or concentrate benefits. All agents benefit equally from the clarity and necessity of the relationship. The small nonzero value (0.08) reflects only that understanding the FTC requires effort and epistemic work — there is a minimal 'cost of doing business' in the mathematical system, but this is not extraction. Suppression (0.02): Negligible. No alternatives are suppressed. Constructive mathematics, intuitionistic logic, and computational approximations all coexist with classical FTC. The theorem does not prevent other approaches; it exists alongside them. Theater ratio (0.15): Minimal. The FTC's content is almost entirely functional. Teaching the FTC involves some ritual (formal proofs, canonical examples) but this is pedagogical clarity, not performance masking emptiness. The theorem is testable and either true or false. Accessibility collapse (0.92): Very high. The FTC is inaccessible to those outside mathematics, but for those inside, it is fully accessible — the only barrier is the prerequisite mathematical education, not institutional gatekeeping. Resistance (0.05): Negligible. No agent resists the FTC; it is accepted universally within mathematics.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives yield mountain classification because the constraint is logically invariant. The perspectival gap is purely about scale of impact, not about disagreement on the constraint's nature. The student encounters the FTC as an immutable truth; the engineer depends on it implicitly; the institution teaches it as foundational; the mathematical community proves it. All agree it is natural law. Unlike the verification bottleneck (which has true perspectival disagreement — some see rope, some see snare), the FTC has uniform classification across all observers. This uniformity is itself diagnostic: it proves the constraint is mountain, not a conventionally-enforced social arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents have symmetric directionality because all are bound equally by the FTC. The mathematical community: d≈0.72 (analytical observer deriving canonical value). Academic institution: d≈0.05 (beneficiary with arbitrage). Applied engineer: d≈0.50 (symmetric position). Student: d≈0.95 (powerless, trapped, but benefiting from clarity). All perspectives produce f(d) values in the range [−0.12, 1.42], yet all yield mountain classification because the threshold values (ε ≤ 0.25, suppression ≤ 0.05) are met regardless of d or f(d). This is the key diagnostic: in a true mountain, directionality becomes irrelevant. The constraint's classification is independent of who measures it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_vs_classical,
    'Does the FTC hold equally in constructive mathematics (without excluded middle) as in classical logic?',
    'Formal proof systems in constructive analysis; comparison of theorem statements and their prerequisites across foundational frameworks',
    'If equally valid: FTC is pure mountain across all mathematical frameworks. If framework-dependent: FTC is a conditional mountain (true given classical logic) — still immutable within its domain but not universally necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_vs_classical, conceptual, 'Whether FTC holds in constructive mathematics').

omega_variable(
    computational_approximation_sufficiency,
    'Do numerical integration and differentiation methods achieve FTC-equivalent accuracy in practice, or is the theoretical equivalence always unattainable computationally?',
    'Empirical analysis of numerical error bounds; investigation of whether computational systems can instantiate the theorem''s logical content or only approximate it',
    'If fully achievable: FTC is computationally binding. If only approximated: FTC is a theoretical ideal that computers approach asymptotically — still mountain but with a computational gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_approximation_sufficiency, empirical, 'Whether numerical methods can instantiate FTC equivalence').

omega_variable(
    intuitionistic_vs_impredicative,
    'Can the FTC be proven without impredicative set-theoretic axioms, or does its proof inherently require power-set axioms?',
    'Proof-theoretic analysis of minimal axiomatization needed for the FTC; identification of which axioms are strictly necessary',
    'If predicatively provable: FTC is mountain in a minimal foundational system. If impredicativity required: FTC rests on stronger assumptions — still immutable within those assumptions but contingent on their acceptance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intuitionistic_vs_impredicative, conceptual, 'Minimal foundational requirements for FTC proof').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_theorem_of_calculus, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ftc_tr_t0, fundamental_theorem_of_calculus, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ftc_tr_t200, fundamental_theorem_of_calculus, theater_ratio, 200, 0.15).
narrative_ontology:measurement(ftc_tr_t400, fundamental_theorem_of_calculus, theater_ratio, 400, 0.15).

% Extraction over time
narrative_ontology:measurement(ftc_be_t0, fundamental_theorem_of_calculus, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ftc_be_t200, fundamental_theorem_of_calculus, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(ftc_be_t400, fundamental_theorem_of_calculus, base_extractiveness, 400, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_theorem_of_calculus, information_standard).
narrative_ontology:affects_constraint(fundamental_theorem_of_calculus, lebesgue_integration_theorem).
narrative_ontology:affects_constraint(fundamental_theorem_of_calculus, stokes_theorem).
narrative_ontology:affects_constraint(fundamental_theorem_of_calculus, inverse_function_theorem).

% DUAL FORMULATION NOTE:
% The FTC is part of a constraint family of foundational analysis theorems. It affects downstream theorems (Lebesgue, Stokes, inverse function theorem) which depend on the relationship between differentiation and integration. Each downstream theorem has its own ε value reflecting empirical vs theoretical status, but all are structurally dependent on the FTC's logical content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
