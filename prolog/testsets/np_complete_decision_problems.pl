% ============================================================================
% CONSTRAINT STORY: np_complete_decision_problems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_np_complete_decision_problems, []).

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
 *   constraint_id: np_complete_decision_problems
 *   human_readable: NP-Completeness: The Computational Hardness Hierarchy
 *   domain: theoretical_computer_science/computational_complexity
 *
 * SUMMARY:
 *   NP-completeness is a structural property of computational decision
 *   problems that creates an apparent hierarchical barrier to efficient
 *   solution. A problem is NP-complete if: (1) its solutions can be verified
 *   in polynomial time, and (2) any problem in NP can be reduced to it in
 *   polynomial time. The class encompasses practical problems (traveling
 *   salesman, Boolean satisfiability, graph coloring, knapsack, constraint
 *   satisfaction) and theoretical abstractions (subset sum, clique detection,
 *   3-coloring). Despite 50+ years of intense research, no polynomial-time
 *   algorithm for any NP-complete problem has been discovered. This
 *   constraint appears identical from all perspectives: it is a mountain — an
 *   unchangeable barrier rooted in the structure of computation itself, not
 *   in institutional design, material scarcity, or suppression mechanisms.
 *
 * KEY AGENTS:
 *   - Algorithm Designer: Powerless/trapped — no escape from exponential runtime for worst-case instances of NP-complete problems
 *   - Software Engineer: Moderate/constrained — can use heuristics and approximations but cannot eliminate the underlying hardness
 *   - Research Community: Organized/mobile — has collectively pursued NP-complete problems without finding polynomial-time solutions despite enormous effort; can change research directions but cannot overcome the mathematical barrier
 *   - Institution: Institutional/arbitrage — can choose computational strategies and resource allocation but cannot remove the hardness through institutional design or funding
 *   - Complexity Theorist: Analytical/analytical — observes that NP-completeness is a mathematical invariant independent of observer position or practical context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(np_complete_decision_problems, 0.12).
domain_priors:suppression_score(np_complete_decision_problems, 0.03).
domain_priors:theater_ratio(np_complete_decision_problems, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(np_complete_decision_problems, extractiveness, 0.12).
narrative_ontology:constraint_metric(np_complete_decision_problems, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(np_complete_decision_problems, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(np_complete_decision_problems, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(np_complete_decision_problems, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(np_complete_decision_problems, mountain).
narrative_ontology:human_readable(np_complete_decision_problems, "NP-Completeness: The Computational Hardness Hierarchy").
narrative_ontology:topic_domain(np_complete_decision_problems, "theoretical_computer_science/computational_complexity").

domain_priors:emerges_naturally(np_complete_decision_problems).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHM DESIGNER CONFRONTING NP-COMPLETE INSTANCE (MOUNTAIN) — No escape from the computational hardness. Every known polynomial-time algorithm fails. Exponential worst-case runtime is unavoidable by the structure of the problem class itself, not by design choice or institutional constraint. The agent is trapped not by social suppression but by mathematical necessity.
constraint_indexing:constraint_classification(np_complete_decision_problems, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEER WITH PRODUCTION DEADLINE (MOUNTAIN) — Constrained by real-world time limits and resource budgets, but the constraint is mathematical, not institutional. If the problem is NP-complete, no polynomial-time algorithm exists (under P≠NP). Heuristics and approximations are available, but they do not solve the underlying hardness — they navigate around it. The constraint persists across all practical contexts.
constraint_indexing:constraint_classification(np_complete_decision_problems, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RESEARCH COMMUNITY SEEKING BREAKTHROUGH (MOUNTAIN) — Over 50 years, thousands of researchers have attacked NP-complete problems with no polynomial-time solution discovered. The hardness persists despite enormous coordinated effort, suggesting it is not a gap in human ingenuity but a structural barrier. Organized researchers have exit options (choose different problems, change research directions, work on approximations) but the mathematical constraint itself remains invariant.
constraint_indexing:constraint_classification(np_complete_decision_problems, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: INSTITUTION ALLOCATING COMPUTATIONAL RESOURCES (MOUNTAIN) — Institutions can choose to deploy approximation algorithms, constraint relaxation, or problem reformulation. They have maximum structural agency (arbitrage-level options). Yet if the underlying problem is NP-complete, no institutional choice removes the hardness — it only shifts the trade-off between solution quality, runtime, and resource consumption. The constraint is orthogonal to institutional power.
constraint_indexing:constraint_classification(np_complete_decision_problems, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / COMPLEXITY THEORIST (MOUNTAIN) — From first principles: NP-complete problems share a structural property — any proposed polynomial-time algorithm for one can be transformed into a polynomial-time algorithm for all others (via Karp reduction). If even one polynomial-time NP-complete algorithm exists, then P=NP, resolving a 50-year-old open conjecture. The absence of such algorithms across decades suggests P≠NP. The hardness is a mathematical invariant, not an artifact of incomplete knowledge.
constraint_indexing:constraint_classification(np_complete_decision_problems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(np_complete_decision_problems_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(np_complete_decision_problems, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(np_complete_decision_problems, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(np_complete_decision_problems, ExtMetricName, E),
    domain_priors:suppression_score(np_complete_decision_problems, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(np_complete_decision_problems),
    narrative_ontology:constraint_metric(np_complete_decision_problems, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(np_complete_decision_problems, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(np_complete_decision_problems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. NP-completeness does not extract value from any agent — it simply imposes a computational cost that is universal and unavoidable. The hardness is not redistributed; it is borne equally by all agents who encounter NP-complete problems. This is the signature of a natural law: the burden is intrinsic, not extracted by any beneficiary. Suppression (0.03): Minimal. There is no active suppression of alternatives — the mathematical structure simply makes polynomial-time algorithms impossible (if P≠NP). Alternatives (approximation, heuristic, constraint relaxation) are available and widely used; they are not suppressed. Theater ratio (0.08): Negligible. The computational hardness is demonstrable and not performative. The exponential runtime is observed empirically; it is not maintained through ritual or institutional inertia. Accessibility collapse (0.92): Extreme. The barrier to polynomial-time solution is absolute — no algorithm can access the interior of NP-complete problems efficiently under the standard model. Resistance (0.08): Minimal. The hardness does not resist individual escape attempts (researchers have freedom to use approximations); it resists class-level escape (no polynomial-time algorithm for the entire NP-complete class exists). The classification is invariant across all observational contexts.
 *
 * PERSPECTIVAL GAP:
 *   Unusually, there is no perspectival gap. All five perspectives converge on mountain classification. The algorithm designer, engineer, research community, institution, and theoretical observer all experience the same constraint identically: NP-completeness is a mathematical barrier that does not depend on structural position, power level, or time horizon. Even the organized research community, despite 50+ years of collective effort and institutional resources, cannot break the hardness. This uniformity is the diagnostic signature of a natural law — the constraint is invariant across all indexical positions.
 *
 * MANDATROPHY ANALYSIS:
 *   NP-COMPLETE CONSTRAINT EXEMPLAR OF UNIFORM NATURAL LAW: This constraint demonstrates the case where mandatrophy does not apply because all perspectives produce the same classification. The question 'which perspective is correct?' has no answer — all perspectives ARE correct. The mandatrophy arises when different perspectives produce different classifications, suggesting either perceptual distortion or stratified extraction. NP-completeness produces no such conflict. This uniformity demonstrates that true natural law constraints (mountains) are indexically invariant — they appear the same from all positions because they reflect mathematical or physical necessity, not contingent institutional arrangements or suppression mechanisms. The only residual mandatrophy would be if a perspective attempted to classify NP-completeness as non-mountain (e.g., claiming it is a social construct or institutional artifact) — but such a classification would be empirically false and would be rejected by the engine's threshold gates (extractiveness > 0.25 gate fails for any legitimate NP-complete problem).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_versus_np_conjecture,
    'Is P=NP or P≠NP?',
    'Constructive proof of a polynomial-time NP-complete algorithm, or proof that no such algorithm exists. One of the Millennium Prize Problems.',
    'If P=NP: the mountain classification is false — NP-complete problems are solvable in polynomial time, contradicting the hardness foundation. All exponential-time algorithm designs become obsolete. If P≠NP: the mountain classification is confirmed — hardness is fundamental and no algorithmic breakthrough can remove it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p_versus_np_conjecture, conceptual, 'The fundamental open question in computational complexity theory').

omega_variable(
    natural_hardness_versus_proof_artifact,
    'Is NP-completeness a property of the problem structure itself, or an artifact of how we reduce problems to instances?',
    'Analysis of whether different reduction schemes (Karp, Cook, Levin) produce the same hardness ranking across problem families. Investigation of whether natural problem instances (vs worst-case or adversarial instances) show exponential hardness empirically.',
    'If artifact: the hardness may not manifest in practice for structured or typical instances, suggesting heuristics and approximations are sufficient (mountain classification weakened). If structural: the hardness is intrinsic to the problem family and unavoidable (mountain classification strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_hardness_versus_proof_artifact, empirical, 'Whether NP-hardness reflects intrinsic structure or proof artifacts').

omega_variable(
    quantum_speedup_boundary,
    'Do quantum algorithms (Grover search, adiabatic algorithms, variational methods) achieve sub-exponential speedups on NP-complete problems, or are quantum computers subject to the same hardness barriers?',
    'Experimental demonstration of quantum algorithms on NP-complete instances; theoretical analysis of whether quantum speedup preserves P vs NP distinction at quantum abstraction level.',
    'If quantum speedup is available: the mountain classification shifts to different substrate (quantum hardness is lower than classical hardness). The classical computational limitation is not fundamental — it is substrate-relative. If no quantum advantage: the hardness persists across computational models, strengthening the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_speedup_boundary, empirical, 'Whether quantum computation circumvents NP-completeness hardness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(np_complete_decision_problems, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(np_c_tr_t0, np_complete_decision_problems, theater_ratio, 0, 0.08).
narrative_ontology:measurement(np_c_tr_t25, np_complete_decision_problems, theater_ratio, 25, 0.08).
narrative_ontology:measurement(np_c_tr_t50, np_complete_decision_problems, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(np_c_be_t0, np_complete_decision_problems, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(np_c_be_t25, np_complete_decision_problems, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(np_c_be_t50, np_complete_decision_problems, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(np_complete_decision_problems, information_standard).
narrative_ontology:affects_constraint(np_complete_decision_problems, traveling_salesman_problem).
narrative_ontology:affects_constraint(np_complete_decision_problems, boolean_satisfiability).
narrative_ontology:affects_constraint(np_complete_decision_problems, graph_coloring).
narrative_ontology:affects_constraint(np_complete_decision_problems, knapsack_problem).
narrative_ontology:affects_constraint(np_complete_decision_problems, clique_detection).

% DUAL FORMULATION NOTE:
% NP-completeness is a unifying property that links dozens of specific problems (TSP, SAT, 3-coloring, knapsack, etc.) to a common hardness class. Each specific problem has its own story with concrete beneficiaries/victims and practical implications. This story describes the general mathematical structure that constrains all of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
