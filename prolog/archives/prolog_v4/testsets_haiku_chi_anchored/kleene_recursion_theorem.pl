% ============================================================================
% CONSTRAINT STORY: kleene_recursion_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kleene_recursion_theorem, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kleene_recursion_theorem
 *   human_readable: Kleene's Second Recursion Theorem
 *   domain: mathematical/theoretical_computer_science
 *
 * SUMMARY:
 *   Kleene's Second Recursion Theorem is a foundational result in
 *   computability theory stating that for any computable function φ that
 *   transforms programs, there exists a program e such that the program e and
 *   the program φ(e) compute the same function. This is a pure mathematical
 *   constraint with no agents, no beneficiaries, no victims, and no degrees
 *   of freedom. The theorem is a necessary consequence of the Turing model of
 *   computation: any universal computational system must permit programs to
 *   construct and inspect their own code. This is not a policy, institution,
 *   or enforced rule — it is a logical law of computation itself. The
 *   constraint exhibits zero extractiveness (ε=0.08, nearly at the
 *   mathematical floor), zero suppression (0.03), and minimal theater (0.10).
 *   There is no performance, no alternative framing, no observer-dependent
 *   variation. Kleene's theorem is invariant across all representations of
 *   Turing-complete computation (lambda calculus, combinatory logic, register
 *   machines, etc.) and all observational contexts.
 *
 * KEY AGENTS:
 *   - The Mathematical Structure: The constraint itself — no agent, no beneficiary, no victim. The theorem is a law of the formal system.
 *   - The Analytical Observer: Can verify the theorem through logical proof; sees the constraint as a logical necessity, not an empirical claim.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kleene_recursion_theorem, 0.08).
domain_priors:suppression_score(kleene_recursion_theorem, 0.03).
domain_priors:theater_ratio(kleene_recursion_theorem, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kleene_recursion_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(kleene_recursion_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(kleene_recursion_theorem, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kleene_recursion_theorem, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(kleene_recursion_theorem, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kleene_recursion_theorem, mountain).
narrative_ontology:human_readable(kleene_recursion_theorem, "Kleene's Second Recursion Theorem").
narrative_ontology:topic_domain(kleene_recursion_theorem, "mathematical/theoretical_computer_science").

domain_priors:emerges_naturally(kleene_recursion_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Kleene's theorem is a logical necessity. The fixed-point property is not contingent on implementation or observation — it follows from the Church-Turing model and the universal computability of program transformations. From a civilizational mathematical perspective, this is a natural law of computation itself.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Any Turing-complete language must satisfy Kleene's theorem. There is no workaround, no escape hatch. A language designer cannot suppress or avoid the fixed-point property without abandoning Turing-completeness. The constraint is immutable at the level of language design — it is bedrock.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% A program executing in a Turing-complete environment has no escape from the fact that it can reference its own code if a transformation function exists that provides it. The fixed-point exists whether the program is aware of it or not. Even 'hostile' programs cannot evade this — it is absolute constraint at the algorithmic level.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% Even a sophisticated AI system or formal verifier operating on programs cannot break Kleene's theorem. The constraint applies universally — to all computable functions and all programs. No amount of engineering, optimization, or redesign bypasses the theorem's logical requirement.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kleene_recursion_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(kleene_recursion_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kleene_recursion_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kleene_recursion_theorem, ExtMetricName, E),
    domain_priors:suppression_score(kleene_recursion_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kleene_recursion_theorem),
    narrative_ontology:constraint_metric(kleene_recursion_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kleene_recursion_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kleene_recursion_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Below the mountain floor (0.25) by design. The theorem extracts nothing from anyone — it is a pure structural property of Turing-complete computation. No asymmetry, no transfer of value, no coercion. The value 0.08 reflects only the minimal notational cost of expressing the constraint itself. Suppression (0.03): Near zero. The theorem cannot be suppressed, hidden, or reframed without abandoning formal rigor. Any Turing-complete system must exhibit the fixed-point property. Accessibility collapse (0.91): Very high. The theorem is accessible to mathematical logic but inaccessible to most practical programming contexts — the fixed-point is often invisible to working programmers. Yet from a foundational perspective, it is completely transparent: the proof is constructive and elegant. Theater ratio (0.10): Minimal. The theorem has no performative content — either the fixed-point exists or it does not. No ritual, no proxy goals, no maintenance through cultural inertia. Emerges naturally (true): The theorem emerges from the definition of Turing-completeness and the universality of computation — it is not imposed externally.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap. All four perspectives (mathematical logician, language designer, program, automated system) see the same constraint: an immutable logical necessity. The classification is Mountain from all positions. There is no disagreement, no asymmetry of extraction, no beneficiary/victim pair. This is a uniform-type constraint (mountain-only) where all perspectives converge on the same type because the constraint is genuinely invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality logic applies. Kleene's theorem has no beneficiaries or victims — it is a logical law, not an institutional arrangement. All agents (if we anthropomorphize the computational system as an agent) experience the same constraint identically. There is no d value, no f(d) computation, no asymmetric extraction. The constraint is symmetric from all viewpoints.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy to resolve. The constraint is pure Mountain (mountain-only type). The theorem is not a disguised Snare (extraction) or Tangled Rope (mixed coordination). It has no hidden agenda, no performative theater, no institutional inertia. The mathematical content and the structural content are identical: a logical necessity. The mandatory properties for mountains are all satisfied: emerges_naturally=true, accessibility_collapse≥0.85 (0.91), resistance≤0.15 (0.04), extractiveness≤0.25 (0.08), suppression≤0.05 (0.03). This is a clean, unambiguous natural law of computation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kleene_recursion_theorem, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kleene_recursion_theorem, halting_problem).
narrative_ontology:affects_constraint(kleene_recursion_theorem, rice_theorem_decidability).
narrative_ontology:affects_constraint(kleene_recursion_theorem, godel_incompleteness_arithmetic).

% DUAL FORMULATION NOTE:
% Kleene's Second Recursion Theorem is upstream of multiple decidability and incompleteness results. It establishes the foundational fact that Turing-complete systems permit self-reference and fixed-points; the Halting Problem and Rice's Theorem depend on this property. The network relationship reflects logical dependency, not causal influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
