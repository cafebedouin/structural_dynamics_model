% ============================================================================
% CONSTRAINT STORY: computational_halting_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_computational_halting_problem, []).

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
 *   constraint_id: computational_halting_problem
 *   human_readable: The Halting Problem (Turing Computability Limit)
 *   domain: mathematical_logic/computability_theory
 *
 * SUMMARY:
 *   The halting problem represents a fundamental limit in computability
 *   theory: no general algorithm can determine whether an arbitrary Turing
 *   machine will halt on a given input. Proven by Turing in 1936 through a
 *   diagonalization argument, this constraint is a mathematical law, not a
 *   contingent institutional or technological barrier. The constraint
 *   exhibits zero degrees of freedom across all observational contexts and
 *   time horizons. It classifies as Mountain from every structural
 *   perspective because the limitation is intrinsic to the logic of
 *   computation itself, not to any agent's power, exit options, or spatial
 *   scope. The constraint has not changed in 90 years of computational
 *   development and cannot change without altering the fundamental definition
 *   of Turing computability.
 *
 * KEY AGENTS:
 *   - Programmers/Developers: Trapped agents (powerless/trapped) — cannot obtain universal halting detection tools, must use pragmatic workarounds and domain-specific solutions
 *   - Software Verification Institutions: Institutional actors (institutional/analytical) — develop formal methods, static analysis, model checkers that solve halting for restricted domains but cannot achieve universality
 *   - Computational Theorists: Analytical observers (analytical/analytical) — establish the theoretical limit through proof; understand the constraint as mathematically proven necessity
 *   - Technology Corporations: Powerful agents (powerful/mobile) — invest in verification infrastructure but cannot overcome the fundamental limit; resources are irrelevant to logical impossibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(computational_halting_problem, 0.12).
domain_priors:suppression_score(computational_halting_problem, 0.02).
domain_priors:theater_ratio(computational_halting_problem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(computational_halting_problem, extractiveness, 0.12).
narrative_ontology:constraint_metric(computational_halting_problem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(computational_halting_problem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(computational_halting_problem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(computational_halting_problem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(computational_halting_problem, mountain).
narrative_ontology:human_readable(computational_halting_problem, "The Halting Problem (Turing Computability Limit)").
narrative_ontology:topic_domain(computational_halting_problem, "mathematical_logic/computability_theory").

domain_priors:emerges_naturally(computational_halting_problem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROGRAMMER SEEKING UNIVERSAL DETECTION (MOUNTAIN) — No computation can exist that solves the halting problem for all Turing machines. This is not a resource limitation (faster hardware, better algorithm) but a structural impossibility. The constraint is experienced as immutable across all time horizons and spatial scales.
constraint_indexing:constraint_classification(computational_halting_problem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: SOFTWARE VERIFICATION INSTITUTION (MOUNTAIN) — Formal verification systems, static analysis tools, and automated testing cannot provably guarantee termination for arbitrary programs. The constraint manifests as a hard technical ceiling on what verification systems can achieve universally. No amount of institutional investment overcomes the logical limit.
constraint_indexing:constraint_classification(computational_halting_problem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — The halting problem is a fundamental consequence of Turing completeness and self-reference. Gödel's incompleteness theorems and Rice's theorem establish that no general decision procedure can determine non-trivial semantic properties of arbitrary programs. The constraint is mathematically proven, not empirically contingent.
constraint_indexing:constraint_classification(computational_halting_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: TECHNOLOGY CORPORATION (MOUNTAIN) — Even with unlimited computational resources, no algorithm can solve the halting problem for the general case. Pragmatic solutions exist (timeouts, bounded verification, heuristic analysis) but these bypass rather than overcome the constraint. The fundamental limit persists regardless of market power or technological capability.
constraint_indexing:constraint_classification(computational_halting_problem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(computational_halting_problem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(computational_halting_problem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(computational_halting_problem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(computational_halting_problem, ExtMetricName, E),
    domain_priors:suppression_score(computational_halting_problem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(computational_halting_problem),
    narrative_ontology:constraint_metric(computational_halting_problem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(computational_halting_problem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(computational_halting_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The halting problem is not an extractive constraint in the sense of appropriating resources or concentration of power. Rather, it is a barrier to capability that affects all agents equally and universally. The value 0.12 reflects the minimal 'friction' of recognizing and working around the constraint — the overhead of pragmatic approximations (timeouts, bounded analysis, heuristic detection). Suppression (0.02): Minimal. There is no suppression mechanism because the constraint is purely logical — no coercion is needed, no alternatives are hidden. The limit simply exists. Theater ratio (0.05): Negligible. Very little performative activity surrounds the halting problem. Verification research is genuinely oriented toward solving what can be solved within the boundary rather than pretending the boundary doesn't exist. The small non-zero value reflects occasional rhetorical sleight where pragmatic approximations are marketed as 'near-universal' solutions.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints in the corpus, the halting problem shows NO perspectival gap. All agent positions experience Mountain classification because the constraint is agent-independent and time-independent. A programmer facing a halting problem has the same objective barrier as a technology corporation or a theorist. The constraint is invariant across power (powerless to institutional), across exit options (trapped to analytical), across time (immediate to civilizational), and across scope (local to universal). This uniformity is the signature of a true natural law. The absence of perspectival divergence is itself a diagnostic signal: when all perspectives converge on one type regardless of structural position, the constraint is likely mathematical/physical/logical rather than institutional.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is not applicable to this Mountain constraint. The halting problem has no beneficiaries or victims because it is not extractive. No agent benefits from the constraint; all agents are equally constrained by it. The constraint does not flow extraction from one group to another but rather establishes a uniform capability boundary for all computational agents. There is no d value, no f(d) sigmoid, no chi formula — the constraint operates at the level of logical structure, not at the level of institutional power dynamics. This is a definitional feature of Mountains in the DR framework: they bind all perspectives equally because they reflect constraints on the universe's structure, not on agents' relations to each other.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restricted_domain_sufficiency,
    'For practical software systems, can restricted-domain halting detection (analysis of specific program classes) substitute functionally for universal halting problem solution?',
    'Empirical survey of real-world verification needs; classification of programs into decidable subcategories; assessment of coverage for safety-critical systems',
    'If yes: the constraint is practically less binding than mathematically (mountain may be practically Rope for restricted domains). If no: the mathematical constraint translates directly to practical limitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(restricted_domain_sufficiency, empirical, 'Whether restricted-domain halting detection provides practical sufficiency').

omega_variable(
    oracle_accessibility_question,
    'If a halting oracle were empirically accessible (e.g., via physical hypercomputation), would the logical constraint dissolve or would new undecidable problems emerge at higher levels?',
    'Theoretical analysis of oracle-complete problems; examination of Turing jump hierarchy; assessment of whether oracles create new undecidable problems',
    'If new problems emerge: the constraint is intrinsic to the structure of problems, not just Turing machines (maintains mountain status). If oracle solves it completely: constraint is contingent on our computational model (could be rope under different model).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_accessibility_question, conceptual, 'Whether oracle accessibility would dissolve or regenerate undecidability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(computational_halting_problem, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, computational_halting_problem, theater_ratio, 0, 0.04).
narrative_ontology:measurement(comp_tr_t25, computational_halting_problem, theater_ratio, 25, 0.05).
narrative_ontology:measurement(comp_tr_t50, computational_halting_problem, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, computational_halting_problem, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(comp_be_t25, computational_halting_problem, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(comp_be_t50, computational_halting_problem, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(computational_halting_problem, information_standard).
narrative_ontology:affects_constraint(computational_halting_problem, godel_incompleteness_theorem).
narrative_ontology:affects_constraint(computational_halting_problem, rice_theorem_undecidability).
narrative_ontology:affects_constraint(computational_halting_problem, computational_universality_requirement).

% DUAL FORMULATION NOTE:
% The halting problem is the archetypal example of undecidability in computability theory. It is upstream to Rice's theorem (which generalizes the halting problem to all non-trivial semantic properties of programs) and to Gödel's incompleteness theorem (which exhibits the same diagonalization structure in formal logic). These are three instantiations of the same deep constraint on decidability and formal systems. The halting problem should be understood as a family member, not as an isolated constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
