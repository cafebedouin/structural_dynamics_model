% ============================================================================
% CONSTRAINT STORY: halting_problem_incomputability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_halting_problem_incomputability, []).

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
 *   constraint_id: halting_problem_incomputability
 *   human_readable: Halting Problem Incomputability
 *   domain: theoretical_computer_science/computability
 *
 * SUMMARY:
 *   The halting problem is the classical incomputability result in
 *   theoretical computer science: no general algorithm can determine whether
 *   an arbitrary Turing machine (or equivalent computational system) will
 *   eventually terminate or run forever. Turing proved this via reduction
 *   from the Entscheidungsproblem using a diagonalization argument. The
 *   constraint is that this incomputability is not a limitation of current
 *   algorithms or computing technology — it is a logical necessity inherent
 *   to computation itself. No amount of engineering, optimization, or
 *   alternative design can produce a universal halting detector. This is a
 *   canonical example of a Mountain constraint: it emerges naturally from
 *   mathematical structure, exhibits complete resistance to circumvention,
 *   and applies universally across all Turing-complete computational systems.
 *
 * KEY AGENTS:
 *   - Programmers and Software Developers: Powerless/trapped — cannot escape the constraint; must work within or around it
 *   - Verification System Designers: Moderate/constrained — can build bounded methods but cannot achieve universality
 *   - Software Verification Industry: Institutional/arbitrage — profits from domain-specific solutions but cannot escape the underlying limit
 *   - Theoretical Computer Science Community: Analytical/analytical — documents and reasons about the constraint's necessity and universality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(halting_problem_incomputability, 0.08).
domain_priors:suppression_score(halting_problem_incomputability, 0.02).
domain_priors:theater_ratio(halting_problem_incomputability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(halting_problem_incomputability, extractiveness, 0.08).
narrative_ontology:constraint_metric(halting_problem_incomputability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(halting_problem_incomputability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(halting_problem_incomputability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(halting_problem_incomputability, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(halting_problem_incomputability, mountain).
narrative_ontology:human_readable(halting_problem_incomputability, "Halting Problem Incomputability").
narrative_ontology:topic_domain(halting_problem_incomputability, "theoretical_computer_science/computability").

domain_priors:emerges_naturally(halting_problem_incomputability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROGRAMMER SEEKING UNIVERSAL VERIFICATION (MOUNTAIN) — No algorithm can determine whether an arbitrary program halts. This constraint is binding across all computational systems and all programming contexts. The programmer cannot exit this limit; it is a structural property of computability itself, not a policy choice or institutional arrangement.
constraint_indexing:constraint_classification(halting_problem_incomputability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: VERIFICATION SYSTEM DESIGNER (MOUNTAIN) — Any attempt to build a general-purpose halting detector necessarily fails. The constraint is not imposed externally but emerges from the logical structure of computation. Trade-offs exist (time limits, heuristic analysis, restricted domains), but these are workarounds, not escapes. The designer faces an unchangeable computational reality.
constraint_indexing:constraint_classification(halting_problem_incomputability, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: SOFTWARE VERIFICATION INDUSTRY (MOUNTAIN) — The industry cannot sell a universal halting detector because none can exist. This is not a market failure or opportunity — it is a logical impossibility. Companies may profit from bounded heuristics (static analysis, model checking), but these are domain-specific solutions to the constraint, not solutions to the constraint itself. The incomputability remains.
constraint_indexing:constraint_classification(halting_problem_incomputability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a logical and mathematical perspective, the halting problem is provably uncomputable via the reduction from the Entscheidungsproblem and diagonalization arguments. The constraint is invariant across all observables and measurement methodologies. No reframing can eliminate the incomputability; it is a fundamental limit on what computation can express.
constraint_indexing:constraint_classification(halting_problem_incomputability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(halting_problem_incomputability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(halting_problem_incomputability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(halting_problem_incomputability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(halting_problem_incomputability, ExtMetricName, E),
    domain_priors:suppression_score(halting_problem_incomputability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(halting_problem_incomputability),
    narrative_ontology:constraint_metric(halting_problem_incomputability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(halting_problem_incomputability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(halting_problem_incomputability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Extremely low. The halting problem does not extract from agents — it is a pure mathematical limit. No actor benefits from the incomputability; it constrains all parties symmetrically. It does not favor one agent over another or redistribute resources. Suppression (0.02): Minimal. The constraint does not require active enforcement or suppression of alternatives. The incomputability is self-evident from formal proof, not sustained by coercion or institutional maintenance. Workarounds (bounded checking, heuristic analysis) remain available; they simply do not solve the universal problem. Theater ratio (0.05): Negligible. The halting problem is not maintained through performative activity. Its truth is established through pure mathematical argument (Turing's diagonalization), not through repeated ritual or institutional theater. Accessibility collapse (0.92): Extremely high. No one can access a universal halting detector — the mathematical proof demonstrates that it cannot exist. Resistance (0.04): Negligible. There is no resistance to the constraint because there is no alternative pathway. Agents do not resist the halting problem; they work around it through bounded methods.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows zero perspectival gap — all agents classify it identically as Mountain. The programmer, the system designer, the industry executive, and the theoretical analyst all perceive the same unalterable limit. This uniformity is diagnostic of a true natural law constraint. No agent experiences the halting problem as extractive (Snare), as coordination (Rope), or as temporary (Scaffold). The absence of perspectival disagreement indicates that the constraint is not a social arrangement or institutional artifact; it is a feature of the mathematical structure of computation itself.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries and no victims because it does not extract or coordinate. Directionality is not applicable — all agents face the same logical boundary. The constraint emerges naturally and applies symmetrically: everyone (programmers, systems, industries, theorists) encounters the same incomputability. This is a rare case where the base properties fully determine the classification without need for beneficiary/victim analysis or directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy trivially: all perspectives produce Mountain, confirming that the classification is not observer-dependent or framing-sensitive. The theorem that no universal halting detector exists is true from all observational positions. There is no mandatrophy — the constraint's type is invariant. This exemplifies how Mountain constraints behave in the Deferential Realism framework: they are logically and mathematically immutable, independent of political framing or institutional power. The halting problem proves this not through institutional weight but through formal proof.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_machine_exceptionality,
    'Do oracle machines (hypothetical computational devices with access to an external halting oracle) represent a genuine escape from the constraint or merely a definitional reframing?',
    'Philosophical and formal analysis: oracle machines can solve the halting problem by definition, but they violate the Church-Turing thesis (they are not Turing-computable). The question is whether this is a meaningful distinction or a semantic game.',
    'If oracle machines are genuine: the constraint applies only to standard Turing computation, not to all possible computation. If definitional reframing: the constraint is universal and inescapable within any constructive framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_machine_exceptionality, conceptual, 'Whether oracle machines represent genuine escape or definitional reframing').

omega_variable(
    practical_bounded_halting_sufficiency,
    'Are bounded halting-detection methods (time limits, resource caps, heuristic analysis) sufficient for all practical software verification needs, such that the theoretical incomputability becomes irrelevant?',
    'Empirical survey of real-world verification scenarios: what fraction of actual software bugs can be caught by bounded analysis without requiring universal halting detection? Cost-benefit analysis of heuristic methods vs the theoretical limit.',
    'If sufficient: the constraint is theoretically absolute but practically circumvented. If insufficient: the theoretical incomputability remains an operational problem for critical systems (medical devices, financial infrastructure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_bounded_halting_sufficiency, empirical, 'Whether bounded halting detection suffices for practical needs').

omega_variable(
    alternative_computational_models_universality,
    'Do alternative computational models (lambda calculus, rewriting systems, cellular automata) face the same halting incomputability, or are there models where halting is decidable?',
    'Formal proof that all Turing-complete systems face halting undecidability; identification of whether any useful computational model escapes this limit.',
    'If universal: the constraint holds across all sufficiently powerful computational formalisms. If escapable: halting decidability is possible in restricted models, and the constraint''s scope can be narrowed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_computational_models_universality, conceptual, 'Whether halting incomputability holds across all Turing-complete models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(halting_problem_incomputability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(halt_tr_t0, halting_problem_incomputability, theater_ratio, 0, 0.04).
narrative_ontology:measurement(halt_tr_t5, halting_problem_incomputability, theater_ratio, 5, 0.05).
narrative_ontology:measurement(halt_tr_t10, halting_problem_incomputability, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(halt_be_t0, halting_problem_incomputability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(halt_be_t5, halting_problem_incomputability, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(halt_be_t10, halting_problem_incomputability, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(halting_problem_incomputability, information_standard).
narrative_ontology:affects_constraint(halting_problem_incomputability, godel_incompleteness_undecidability).
narrative_ontology:affects_constraint(halting_problem_incomputability, entscheidungsproblem_undecidability).

% DUAL FORMULATION NOTE:
% The halting problem is a specific instantiation of the broader undecidability family in mathematical logic. Gödel's Incompleteness Theorem and Church's Entscheidungsproblem result are logically related — all three establish limits on what can be computed or proven. The halting problem can be derived from or mapped to the Entscheidungsproblem; it also has connections to computational universality. These are distinct constraints with different formulations but overlapping mathematical structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
