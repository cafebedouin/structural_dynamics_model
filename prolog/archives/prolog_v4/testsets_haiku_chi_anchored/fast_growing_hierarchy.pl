% ============================================================================
% CONSTRAINT STORY: fast_growing_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fast_growing_hierarchy, []).

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
 *   constraint_id: fast_growing_hierarchy
 *   human_readable: The Fast-Growing Hierarchy (FGH)
 *   domain: computational_theory/mathematical_foundations
 *
 * SUMMARY:
 *   The Fast-Growing Hierarchy (FGH) is a family of functions f_α indexed by
 *   ordinals α that provides a stratification of computable function growth
 *   rates. At level f_0, the hierarchy includes the successor and projection
 *   functions (primitive recursive base). At successor levels f_{n+1}, new
 *   functions are defined by diagonalization over the previous level. At
 *   limit ordinals, the hierarchy transitions to new growth regimes. The FGH
 *   emerges as a mathematical structure discovered (not invented) through the
 *   interaction of formal systems, computability theory, and ordinal
 *   arithmetic. The constraint is that no computable algorithm can escape its
 *   ordinal-indexed growth bounds: a function computable in time scaling like
 *   f_ω cannot be computed in time scaling like f_{ω+1}, and this ordering is
 *   invariant across all models of computation. The FGH is a canonical
 *   mountain constraint: zero degrees of freedom for all agents, zero
 *   theatrical content, pure mathematical necessity.
 *
 * KEY AGENTS:
 *   - The Logician: Analytical observer (analytical/analytical) — recognizes the FGH as an irreducible structure of proof theory and ordinal arithmetic
 *   - The Applied Computer Scientist: Powerful agent (powerful/mobile) — encounters FGH as an absolute barrier to algorithmic acceleration
 *   - The Algorithm Designer: Moderate agent (moderate/constrained) — discovers FGH through resource constraints and problem-specific complexity lower bounds
 *   - The Mathematical Community: Institutional beneficiary (institutional/arbitrage) — uses FGH as a classification framework for decidability and complexity theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fast_growing_hierarchy, 0.12).
domain_priors:suppression_score(fast_growing_hierarchy, 0.03).
domain_priors:theater_ratio(fast_growing_hierarchy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fast_growing_hierarchy, extractiveness, 0.12).
narrative_ontology:constraint_metric(fast_growing_hierarchy, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fast_growing_hierarchy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fast_growing_hierarchy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fast_growing_hierarchy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fast_growing_hierarchy, mountain).
narrative_ontology:human_readable(fast_growing_hierarchy, "The Fast-Growing Hierarchy (FGH)").
narrative_ontology:topic_domain(fast_growing_hierarchy, "computational_theory/mathematical_foundations").

domain_priors:emerges_naturally(fast_growing_hierarchy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The FGH is a fundamental mathematical structure: a transfinite indexing of function growth rates. From the analytical standpoint, the FGH emerges as an irreducible limit of computability theory. No agent can modify, escape, or circumvent the ordering of growth rates defined by the hierarchy. The constraint is invariant across all computational models (Turing machines, lambda calculus, register machines). ε=0.12, suppression=0.03 → pure mathematical necessity.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even agents with significant computational resources encounter the FGH as an absolute barrier. Attempts to accelerate function evaluation beyond the ordinal-indexed limit fail not from lack of engineering, but from the mathematical structure itself. Computational complexity hierarchies cannot be compressed below their ordinal classification. The constraint appears as a physical law: growth rates are immutable. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08, still within mountain range.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% A practitioner designing algorithms discovers the FGH through resource constraints: certain computational problems provably require time/space complexity that scales faster than any primitive recursive function. No matter how clever the implementation, the ordinal growth rate cannot be reduced. The FGH appears as an external limit, like gravity. d≈0.65, f(d)≈1.00, σ=0.9 → χ≈0.11, mountain from all agent perspectives.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fast_growing_hierarchy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, ExtMetricName, E),
    domain_priors:suppression_score(fast_growing_hierarchy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fast_growing_hierarchy),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fast_growing_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The FGH does not extract resources or benefits from any agent. It is a purely descriptive classification of function growth rates. No agent is made worse off by the hierarchy's existence; it simply describes ordinal relationships between computational complexity classes. The small non-zero value reflects that the formal system requires labor to maintain and teach — knowledge of the FGH requires training — but this is institutional overhead, not extraction. Suppression (0.03): Minimal. The FGH has no coercive mechanism. It constrains what computations are possible, but through mathematical necessity, not enforcement. Agents who discover they cannot exceed ordinal bounds experience this as a limitation of physics/logic, not as coercion. Theater ratio (0.15): Very low. The FGH is purely functional. Its definition is formal, its properties are derivable, its predictions are testable. No performative layer exists — the hierarchy is not maintained through ritual or narrative; it stands or falls on mathematical rigor.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the FGH identically as a mountain. The perspectival gap collapses because the constraint is observer-invariant: no agent's power level, time horizon, or exit options changes the mathematical ordering of growth rates. A logician and a practitioner disagree on many things, but both encounter the same ordinal barriers. This uniformity is the defining signature of a true mountain. The FGH does not appear differently from different positions — it appears the same from all positions, or not at all (if the observer ignores computability theory).
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraints have no directionality, no beneficiaries, no victims. The FGH transcends the beneficiary/victim framework: it is a structural fact about the space of possible computations, not a relationship between agents. All agents — logicians, engineers, designers — face the same ordinal constraints. There is no extraction, no coordination, no strategic position. The FGH simply bounds what is computable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinal_realizability,
    'Are the transfinite ordinals indexing the FGH ontologically real structures or formal notational conveniences?',
    'Philosophical analysis of mathematical realism vs constructivism; empirical investigation of whether ordinal-indexed growth rates appear in physical computational systems',
    'If real: FGH is a mountain across all interpretations. If notational: FGH is a human classification system (shifts toward rope). This does not change the practical constraint but clarifies its source.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ordinal_realizability, conceptual, 'Ontological status of transfinite ordinals in the FGH').

omega_variable(
    hypercomputation_possibility,
    'Could a hypothetical hypercomputer (oracle Turing machine, physical oracle) exceed FGH-indexed growth rates?',
    'Theoretical analysis of oracle Turing machines and their relationship to the arithmetical hierarchy; examination of whether FGH bounds extend to hypercomputation or only to standard Turing computation',
    'If FGH is universal: mountain confirmed for all computational paradigms. If FGH only bounds standard computation: hypercomputation might escape the hierarchy, demoting FGH to rope-like coordination within the standard model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hypercomputation_possibility, empirical, 'Whether hypercomputation can exceed FGH-indexed growth rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fast_growing_hierarchy, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fgh_tr_t0, fast_growing_hierarchy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fgh_tr_t25, fast_growing_hierarchy, theater_ratio, 25, 0.15).
narrative_ontology:measurement(fgh_tr_t50, fast_growing_hierarchy, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(fgh_be_t0, fast_growing_hierarchy, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(fgh_be_t25, fast_growing_hierarchy, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(fgh_be_t50, fast_growing_hierarchy, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fast_growing_hierarchy, information_standard).
narrative_ontology:affects_constraint(fast_growing_hierarchy, busy_beaver_function).
narrative_ontology:affects_constraint(fast_growing_hierarchy, arithmetical_hierarchy).
narrative_ontology:affects_constraint(fast_growing_hierarchy, computational_complexity_classes).

% DUAL FORMULATION NOTE:
% The FGH is a foundational structure that constrains three downstream domains: the busy beaver function (which exemplifies growth rates beyond primitive recursion), the arithmetical hierarchy (which uses ordinal indexing for logical definability), and computational complexity classes (which are stratified by FGH-like ordinal bounds). The FGH itself is not downstream of any constraint; it is the upstream mathematical framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
