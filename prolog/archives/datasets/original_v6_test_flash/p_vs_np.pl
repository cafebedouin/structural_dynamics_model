% ============================================================================
% CONSTRAINT STORY: p_vs_np
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_p_vs_np, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: p_vs_np
 *   human_readable: The P versus NP Problem
 *   domain: technological
 *
 * SUMMARY:
 *   The P vs NP problem is a major unsolved question in computer science,
 *   asking whether every problem whose solution can be quickly verified (NP)
 *   can also be quickly solved (P).
 *
 * KEY AGENTS:
 *   - Problem Solvers: Primary target (powerless/trapped) - face computationally intractable problems.
 *   - Cryptographers: Primary beneficiary (institutional/arbitrage) - rely on the difficulty of solving NP problems for secure cryptography.
 *   - Algorithm Developers: Secondary target (moderate/constrained) - constrained by the limitations of current algorithms.
 *   - Optimization Researchers: Secondary beneficiary (powerful/constrained) - benefit from developing algorithms, even if not perfectly efficient.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(p_vs_np, 0.6).
domain_priors:suppression_score(p_vs_np, 0.7).
domain_priors:theater_ratio(p_vs_np, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(p_vs_np, extractiveness, 0.6).
narrative_ontology:constraint_metric(p_vs_np, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(p_vs_np, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(p_vs_np, tangled_rope).
narrative_ontology:human_readable(p_vs_np, "The P versus NP Problem").
narrative_ontology:topic_domain(p_vs_np, "technological").

domain_priors:requires_active_enforcement(p_vs_np).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(p_vs_np, cryptographers).
narrative_ontology:constraint_beneficiary(p_vs_np, optimization_researchers).
narrative_ontology:constraint_victim(p_vs_np, problem_solvers).
narrative_ontology:constraint_victim(p_vs_np, algorithm_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of problem solvers, the lack of efficient algorithms for NP-complete problems is a snare, trapping them with intractable problems.
constraint_indexing:constraint_classification(p_vs_np, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% For cryptographers, the difficulty of solving NP problems is a rope, providing the foundation for secure encryption methods.
constraint_indexing:constraint_classification(p_vs_np, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, P vs NP is a tangled rope, a fundamental question that simultaneously constrains and enables progress in computer science.
constraint_indexing:constraint_classification(p_vs_np, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(p_vs_np_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(p_vs_np, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(p_vs_np, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(p_vs_np, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(p_vs_np_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: The inability to efficiently solve NP-complete problems extracts computational resources and limits the scope of solvable problems. Suppression: The lack of efficient algorithms suppresses the development of solutions to NP-complete problems. Theater: The theater ratio is relatively low because the focus is on finding genuine solutions, not merely performative ones.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing positions of problem solvers and cryptographers. For problem solvers, the difficulty of solving NP problems is a barrier. For cryptographers, it is a tool for building secure systems. The analytical observer recognizes the fundamental nature of the problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Cryptographers benefit from the difficulty of NP problems, while problem solvers are hindered by it. This difference in benefit and cost leads to different perspectives on the problem's nature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_np_equivalence,
    'Are P and NP equivalent?',
    'A proof or disproof of P = NP.',
    'If P = NP, current cryptographic systems would be insecure. If P != NP, the existing computational complexity landscape would be validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p_np_equivalence, conceptual, 'Whether P and NP are the same.').

omega_variable(
    approximation_algorithms_efficiency,
    'Can efficient approximation algorithms be developed for all NP-hard problems?',
    'Research into approximation algorithms and their limitations.',
    'If efficient approximation algorithms exist, the practical impact of P != NP would be lessened. If not, the intractability of NP-hard problems would be more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(approximation_algorithms_efficiency, empirical, 'The efficiency of approximation algorithms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(p_vs_np, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(p_vs_tr_t0, p_vs_np, theater_ratio, 0, 0.1).
narrative_ontology:measurement(p_vs_tr_t50, p_vs_np, theater_ratio, 50, 0.3).
narrative_ontology:measurement(p_vs_tr_t100, p_vs_np, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(p_vs_be_t0, p_vs_np, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(p_vs_be_t50, p_vs_np, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(p_vs_be_t100, p_vs_np, base_extractiveness, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(p_vs_np, information_standard).
narrative_ontology:affects_constraint(p_vs_np, computational_complexity).
narrative_ontology:affects_constraint(p_vs_np, cryptographic_security).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
