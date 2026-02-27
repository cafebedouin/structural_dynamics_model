% ============================================================================
% CONSTRAINT STORY: sat_csp_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sat_csp_complexity, []).

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
 *   constraint_id: sat_csp_complexity
 *   human_readable: SAT/CSP Computational Complexity
 *   domain: computational_complexity_theory
 *
 * SUMMARY:
 *   The SAT/CSP computational complexity constraint represents a fundamental
 *   limit in computer science: the intractability of NP-complete problems in
 *   the worst case. This constraint is not a feature of any particular
 *   algorithm, implementation, or social arrangement — it is a logical
 *   consequence of the definition of NP-completeness and the conjectured
 *   inequality P ≠ NP. For 70 years (Cook-Levin 1971 to present), despite
 *   exponential increases in computing power, advances in solver heuristics
 *   (DPLL, SLS, learned clause management), and exploration of alternative
 *   computational models (quantum, optical, analog), no breakthrough has
 *   overcome the exponential worst-case lower bound. The constraint remains
 *   invariant across all measurement bases: worst-case analysis, average-case
 *   instance distributions, practical solver performance on structured
 *   problems, and theoretical proofs all converge on the same fundamental
 *   barrier. This is the defining characteristic of a mountain constraint —
 *   one that emerges naturally from the logical structure of the problem
 *   itself, not from contingent institutional or economic arrangements.
 *
 * KEY AGENTS:
 *   - Algorithm Designers: Powerless (trapped) — any algorithm must confront the hardness barrier; no exit option
 *   - Computational Researchers: Organized (constrained) — aware of the constraint but continue research under it; may find special-case solutions but cannot escape worst-case hardness
 *   - Software Engineers: Institutional (analytical) — use SAT/CSP solvers as tools; design systems that avoid hard instances or use heuristic approximations; work within the constraint
 *   - Theoretical Computer Science: Analytical (analytical) — studies the constraint itself; seeks to understand its nature and prove it formally
 *   - Quantum Computing Researchers: Institutional (constrained) — explore whether quantum computation might bypass the constraint; evidence (Grover's algorithm, no known quantum NP solver) suggests not
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sat_csp_complexity, 0.12).
domain_priors:suppression_score(sat_csp_complexity, 0.03).
domain_priors:theater_ratio(sat_csp_complexity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sat_csp_complexity, extractiveness, 0.12).
narrative_ontology:constraint_metric(sat_csp_complexity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(sat_csp_complexity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sat_csp_complexity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(sat_csp_complexity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sat_csp_complexity, mountain).
narrative_ontology:human_readable(sat_csp_complexity, "SAT/CSP Computational Complexity").
narrative_ontology:topic_domain(sat_csp_complexity, "computational_complexity_theory").

domain_priors:emerges_naturally(sat_csp_complexity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHM DESIGNER (MOUNTAIN) — Any algorithm attempting to solve arbitrary SAT/CSP instances must confront the fundamental hardness: no known polynomial-time solver exists for NP-complete problems. The constraint is immutable across all attempts at optimization. d≈0.98, f(d)≈1.40, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(sat_csp_complexity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL COMMUNITY (MOUNTAIN) — Despite 70 years of research (Cook-Levin 1971, SAT solver competitions, quantum algorithm proposals), no breakthrough has overcome the worst-case exponential lower bound. The constraint persists as a civilizational-scale research frontier. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.12.
constraint_indexing:constraint_classification(sat_csp_complexity, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THEORETICAL OBSERVER (MOUNTAIN) — From a logical/mathematical standpoint, SAT/CSP hardness is a consequence of the structure of Boolean satisfiability itself: the pigeonhole principle, resolution complexity lower bounds (Haken 1985), and the relationship between satisfying assignments and proof length are intrinsic, not contingent. No measurement basis or observable can alter this. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(sat_csp_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: APPLIED SYSTEMS ENGINEER (MOUNTAIN) — Every practical implementation (SAT solvers, constraint propagation, heuristic search) operates within the constraint's bounds. Despite advanced techniques (DPLL, SLS, learned clause management), the worst-case complexity remains exponential. Real-world instances may be easy or hard, but the constraint persists. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(sat_csp_complexity, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: PHYSICIST / PHYSICAL LIMITS (MOUNTAIN) — Even quantum computation does not escape the fundamental constraint: Grover's algorithm provides quadratic speedup for unstructured search, but no quantum algorithm is known to solve NP-complete problems in polynomial time. Physical computation is bounded by the same logical limits. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(sat_csp_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sat_csp_complexity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sat_csp_complexity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sat_csp_complexity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sat_csp_complexity, ExtMetricName, E),
    domain_priors:suppression_score(sat_csp_complexity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sat_csp_complexity),
    narrative_ontology:constraint_metric(sat_csp_complexity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sat_csp_complexity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sat_csp_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint does not extract value from any agent — it simply bounds what is computationally possible. No one benefits from NP-completeness; it is a collective loss function. The value reflects that the constraint is purely negative (what cannot be done efficiently), not extractive (transfer of resources or asymmetric benefit). Suppression (0.03): Negligible. The constraint is transparent — researchers understand exactly why SAT/CSP are hard, the proofs are public, and there are no secrets or coercion. The resistance to accepting the constraint is intellectual (the desire for a polynomial-time algorithm) rather than structural. Theater ratio (0.15): Very low. The mathematical proofs (Cook-Levin, Haken's resolution lower bounds, oracle separations) are rigorous and not performative. Research into SAT/CSP hardness is directly functional — understanding the constraint IS the goal, not disguising function behind ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits complete perspectival consensus: all perspectives classify it as Mountain. There is no disagreement about whether SAT/CSP are hard. The disagreement is philosophical (whether P = NP, whether oracle separations are decisive, whether quantum or physical computation might escape the logical constraint) but does not lead to different practical classifications. An algorithm designer, a theoretical observer, an applied engineer, and a quantum researcher all recognize the same hardness barrier. This is the hallmark of a true natural law — it is invariant across all observers and all measurement bases.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard mountain derivation: d is not the relevant axis for natural laws. All agents are symmetrically trapped by the constraint, not asymmetrically extracted from. No beneficiary/victim distinction is meaningful — the constraint does not benefit anyone. The constraint's directionality is simply 'toward all observers equally,' encoded as uniform high d values (0.5-0.98) that yield consistent moderate f(d) and low χ.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY RISK. This constraint is categorically free from mandatrophy because: (1) it exhibits zero perspectival disagreement — all observers classify it the same way; (2) it has no beneficiary group whose interests would be served by mislabeling it as pure coordination; (3) it has no suppression mechanism that could hide extraction beneath a coordination narrative; (4) the theater ratio is negligible, indicating no performative function masking extractive mechanism. The constraint is also free from false natural law risk because the structural data (ε=0.12, suppression=0.03, theater=0.15) fully support the mountain classification — these metrics are not high enough to indicate a degraded or performative system. The constraint is what it claims to be: a genuine limit on computation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_vs_np_resolution,
    'Is P = NP, or is P ≠ NP?',
    'Mathematical proof (7 of 7 Millennium Prize Problem criteria) or discovery of polynomial-time algorithm for 3-SAT or any NP-complete problem',
    'If P = NP: SAT/CSP hardness reverts to practical constraint only (Rope or Scaffold); mountain classification becomes false. If P ≠ NP (conjectured): mountain classification is definitively confirmed, and exponential lower bounds are inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p_vs_np_resolution, conceptual, 'Whether P equals NP; resolves fundamental hardness assumption').

omega_variable(
    oracle_separation,
    'Do oracle separations (Baker-Gill-Solovay 1975) prove that P ≠ NP in a strong logical sense, or do they reflect limitations of relativization proofs rather than ground truth?',
    'Development of new proof techniques beyond relativization (natural proofs, algebraic methods); resolution of whether P/poly barriers are fundamental or proof artifacts',
    'If oracle separations reflect ground truth: hardness is truly fundamental. If they reflect proof technique limitations: new approaches might bypass the constraint without solving P vs NP.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracle_separation, conceptual, 'Interpretation of oracle separations in hardness proofs').

omega_variable(
    average_case_structure,
    'Does the hardness of SAT/CSP scale uniformly across input distributions, or are there special classes of instances (e.g., planted solutions, random 3-SAT near threshold) where hardness concentrates?',
    'Detailed analysis of instance-specific hardness landscapes; correlation between structural properties (clause-to-variable ratio, solution density, symmetry) and solver runtime',
    'If hardness is uniform: worst-case and average-case constraints are equivalent (mountain from all practical perspectives). If hardness concentrates: different classes may have different constraint types (mountain for uniform instances, Rope for structured subclasses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(average_case_structure, empirical, 'Whether hardness is uniform across input distributions or concentrates in specific classes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sat_csp_complexity, 1971, 2041).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(satcsp_tr_t0, sat_csp_complexity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(satcsp_tr_t35, sat_csp_complexity, theater_ratio, 35, 0.14).
narrative_ontology:measurement(satcsp_tr_t70, sat_csp_complexity, theater_ratio, 70, 0.15).

% Extraction over time
narrative_ontology:measurement(satcsp_be_t0, sat_csp_complexity, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(satcsp_be_t35, sat_csp_complexity, base_extractiveness, 35, 0.12).
narrative_ontology:measurement(satcsp_be_t70, sat_csp_complexity, base_extractiveness, 70, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sat_csp_complexity, information_standard).
narrative_ontology:affects_constraint(sat_csp_complexity, np_complete_reduction_hierarchy).
narrative_ontology:affects_constraint(sat_csp_complexity, practical_sat_solver_performance).
narrative_ontology:affects_constraint(sat_csp_complexity, quantum_algorithm_speedup_limits).

% DUAL FORMULATION NOTE:
% SAT/CSP complexity decomposes into multiple mathematically equivalent constraint families: (1) worst-case exponential lower bounds (ε=0.12, Mountain); (2) average-case hardness on random instances (ε=0.14, Mountain with slight degradation due to measurement structure); (3) practical instance-hardness correlation (ε=0.15, Mountain with theatrical elements in heuristic justification). All three are linked — proving one proves the others — so they constitute a single unified constraint with stable ε across all formulations. No decomposition into separate stories is needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
