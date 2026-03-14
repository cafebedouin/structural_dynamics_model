% ============================================================================
% CONSTRAINT STORY: halting_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_halting_problem, []).

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
 *   constraint_id: halting_problem
 *   human_readable: The Halting Problem: Logical Undecidability in Computation
 *   domain: mathematical_logic/computability_theory
 *
 * SUMMARY:
 *   The halting problem is Turing's foundational result (1936) proving that
 *   no algorithm can determine whether an arbitrary program terminates or
 *   runs forever. This is a pure logical constraint — a mathematical limit on
 *   what computation itself can achieve. The constraint exhibits zero degrees
 *   of freedom: it is unchangeable across all observers, all exit options,
 *   all time horizons, and all spatial scopes. No agent benefits from the
 *   halting problem's existence; no agent bears extractive cost from
 *   another's action. The constraint is not institutional, contingent, or
 *   negotiable. It is a structural feature of any Turing-complete system,
 *   proven by contradiction — a hypothetical halting-solver would generate a
 *   paradox identical to the liar's sentence. This constraint serves as a
 *   diagnostic exemplar for mountain classification: emerges naturally from
 *   axioms, exhibits complete accessibility collapse (once understood, it
 *   cannot be escaped), generates zero suppression (the barrier is logical,
 *   not coercive), and produces zero extractiveness (no agent extracts value
 *   from the constraint's existence).
 *
 * KEY AGENTS:
 *   - Programmers and engineers: powerless/trapped — cannot use a general halting solver; must bound search spaces or accept incompleteness
 *   - Theoretical computer scientists: moderate/constrained — can work around the constraint in restricted domains but cannot eliminate it
 *   - Software verification institutions: institutional/arbitrage — develop bounded verification tools that sidestep the problem but do not solve it
 *   - Analytical observer: analytical/analytical — recognizes the constraint as a proven mathematical theorem, invariant across contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(halting_problem, 0.08).
domain_priors:suppression_score(halting_problem, 0.02).
domain_priors:theater_ratio(halting_problem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(halting_problem, extractiveness, 0.08).
narrative_ontology:constraint_metric(halting_problem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(halting_problem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(halting_problem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(halting_problem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(halting_problem, mountain).
narrative_ontology:human_readable(halting_problem, "The Halting Problem: Logical Undecidability in Computation").
narrative_ontology:topic_domain(halting_problem, "mathematical_logic/computability_theory").

domain_priors:emerges_naturally(halting_problem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS AGENT (MOUNTAIN) — A programmer wishing to verify that arbitrary programs terminate encounters an absolute logical barrier. No algorithm exists that can solve the general halting problem. This constraint is unchangeable at all time horizons and across all exit options — trapped universally.
constraint_indexing:constraint_classification(halting_problem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MODERATE AGENT (MOUNTAIN) — Even with domain expertise and access to restricted classes of programs, the fundamental undecidability persists. The constraint applies equally to specialists. High accessibility collapse — the constraint is always present once the domain is fully understood.
constraint_indexing:constraint_classification(halting_problem, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: INSTITUTIONAL OBSERVER (MOUNTAIN) — Formal verification systems, static analysis tools, and theorem provers can solve the halting problem for restricted domains and finite executions, but the general undecidability remains structural and inescapable. No amount of institutional resources or methodological sophistication eliminates the fundamental limit.
constraint_indexing:constraint_classification(halting_problem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The halting problem's undecidability is a proven theorem in computability theory (Turing 1936). It emerges logically from the definition of computation itself — any Turing-complete system that could solve its own halting problem would generate a contradiction. The constraint is a mathematical necessity, not a contingent institutional limitation.
constraint_indexing:constraint_classification(halting_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(halting_problem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(halting_problem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(halting_problem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(halting_problem, ExtMetricName, E),
    domain_priors:suppression_score(halting_problem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(halting_problem),
    narrative_ontology:constraint_metric(halting_problem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(halting_problem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(halting_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. The halting problem does not extract resources from any agent. It is not a mechanism through which some group gains at another's expense. The minimal non-zero value reflects that practical software development must allocate resources to work around the undecidability (bounded checking, static analysis, testing), but these are costs of adaptation, not extraction by a beneficiary. Suppression (0.02): Near-zero. No agent suppresses alternatives to the halting problem constraint. The undecidability is not maintained by coercion or obscurity — it follows logically and is publicly known. Theater ratio (0.05): Minimal. The halting problem's existence is not performed or maintained through ritual. It is stated once, proven once, and thereafter requires no theatrical reinforcement. The minimal value reflects only that any practical instantiation of the constraint (a program that fails to terminate) involves some observational performance.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All four perspectives — powerless programmer, moderate specialist, institutional verifier, and analytical observer — classify the constraint identically as mountain. The agent_power and exit_options parameters do not shift the classification because the constraint is invariant across all observable positions. This uniformity is the diagnostic signature of a true mountain: independent agents with different power levels and different exit routes all encounter the same unchangeable barrier.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is undefined for mountain constraints. The halting problem has no beneficiary and no victim. The constraint does not flow from one agent to another. All agents experience identical structural relationship to the undecidability: they encounter it as an external, unchangeable limit. The engine derives d from beneficiary/victim declarations, but mountains require no such declarations because the constraint is not extractive. The canonical d value for this constraint is 0.50 (symmetric) or undefined (non-relational), because the constraint is a property of the computational system itself, not of any dyadic interaction.
 *
 * MANDATROPHY ANALYSIS:
 *   The halting problem resolves mandatrophy by being a genuine mountain rather than a false summit. The constraint exhibits all four mountain gates: base extractiveness ≤ 0.25 (actual: 0.08), suppression ≤ 0.05 (actual: 0.02), accessibility_collapse ≥ 0.85 (actual: 0.92), resistance ≤ 0.15 (actual: 0.08), and emerges_naturally = true (proven theorem). There is no risk of misclassification as pure extraction (Snare) because no victims exist and no suppression mechanism maintains the constraint. There is no coordination function to confuse with extraction (ruling out Tangled Rope or Rope). The constraint is logically, not institutionally, determined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_substrate_independence,
    'Is the halting problem''s undecidability independent of physical implementation details, or could non-classical computation (quantum, hypercomputation) escape the logical barrier?',
    'Formal analysis of hypercomputation models and their relationship to standard Turing completeness; empirical exploration of quantum computational limits; axiomatic examination of what ''computation'' means in non-classical substrates',
    'If truly substrate-independent: halting problem remains mountain across all possible computational frameworks. If substrate-dependent: specific implementations may achieve decidability within physical constraints, downgrading from universal mountain to domain-restricted rope/scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_substrate_independence, conceptual, 'Whether halting problem undecidability transcends physical substrate').

omega_variable(
    practical_verification_adequacy,
    'For practical software systems, do restricted-domain halting solvers (temporal bounds, state-space limits, type systems) provide sufficient verification even though the general problem is undecidable?',
    'Empirical assessment of real-world software verification success rates using bounded model checking and static analysis; cost-benefit analysis of practical verification vs absolute proof',
    'If adequate: the constraint''s practical impact is lower than its theoretical status suggests — practitioners may treat halting verification as solved within acceptable bounds. If inadequate: the mountain classification remains functionally binding on real systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_verification_adequacy, empirical, 'Whether practical halting solvers provide adequate real-world verification').

omega_variable(
    definition_sensitivity,
    'Does the undecidability depend on the specific definition of ''halting'' (finite termination vs reaching a stable state vs satisfying a property), or is it invariant across reasonable interpretations?',
    'Formal analysis of generalized halting problems and their decidability; examination of partial halting solutions and their scope',
    'If definition-sensitive: some interpretations might be decidable, creating escape routes from the mountain. If invariant: undecidability is robust across reasonable problem formulations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_sensitivity, conceptual, 'Whether undecidability depends on specific halting definition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(halting_problem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(halt_tr_t0, halting_problem, theater_ratio, 0, 0.05).
narrative_ontology:measurement(halt_tr_t50, halting_problem, theater_ratio, 50, 0.05).
narrative_ontology:measurement(halt_tr_t100, halting_problem, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(halt_be_t0, halting_problem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(halt_be_t50, halting_problem, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(halt_be_t100, halting_problem, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
