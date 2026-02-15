% ============================================================================
% CONSTRAINT STORY: galois_theory_symmetry
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_galois_theory_symmetry, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: galois_theory_symmetry
 *   human_readable: Galois Theory (Symmetry of Roots)
 *   domain: mathematical
 *
 * SUMMARY:
 *   Galois Theory provides a fundamental connection between field theory and
 *   group theory. It characterizes the solvability of polynomials by radicals
 *   through the symmetry group of their roots (the Galois group). Most
 *   famously, it proves the structural impossibility of finding a general
 *   formula for the roots of polynomials of degree five or higher in terms
 *   of radicals. This is an unchangeable feature of algebraic structures.
 *
 * KEY AGENTS (by structural relationship):
 *   - Polynomial Roots (powerless/trapped): The mathematical objects whose
 *     relationships are inescapably governed by their internal symmetry group.
 *   - Abstract Algebraists (institutional/arbitrage): Primary beneficiaries who
 *     use the theory as a powerful coordination tool to navigate and prove
 *     results in complex algebraic structures.
 *   - Mathematics Students (moderate/constrained): Temporary victims of the
 *     theory's high cognitive cost, which can feel like a barrier to entry.
 *   - Analytical Observer: Sees the complete structure as a fundamental,
 *     unchangeable law of mathematics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Low (0.2). It is a structural truth. However, it "extracts"
% the possibility of "easy" radical solutions for high-degree equations,
% demanding significant cognitive labor to understand the underlying group laws.
domain_priors:base_extractiveness(galois_theory_symmetry, 0.20).

% Rationale: Very low (0.05). It reveals deeper truths rather than hiding them.
% It suppresses the "naive" hope for a general quintic formula, but this is
% a foreclosure of an impossible path, not coercion. Meets mountain threshold.
domain_priors:suppression_score(galois_theory_symmetry, 0.05).

% Rationale: Zero. The theory is pure substance with no performative aspect.
domain_priors:theater_ratio(galois_theory_symmetry, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(galois_theory_symmetry, extractiveness, 0.20).
narrative_ontology:constraint_metric(galois_theory_symmetry, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(galois_theory_symmetry, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility Collapse: It is impossible to conceive of a valid alternative.
narrative_ontology:constraint_metric(galois_theory_symmetry, accessibility_collapse, 1.0).
% Resistance: Meaningful resistance is incoherent. One cannot 'oppose' a theorem.
narrative_ontology:constraint_metric(galois_theory_symmetry, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(galois_theory_symmetry, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the axioms of fields and groups.
domain_priors:emerges_naturally(galois_theory_symmetry).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Although the claim is Mountain, other perspectives are non-Mountain. These
% declarations feed the directionality derivation for those perspectives.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(galois_theory_symmetry, abstract_algebraists).
narrative_ontology:constraint_beneficiary(galois_theory_symmetry, theoretical_physicists).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(galois_theory_symmetry, mathematics_students).
narrative_ontology:constraint_victim(galois_theory_symmetry, naive_mathematicians). % Those hoping for a general formula.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE ROOTS OF THE QUINTIC (MOUNTAIN)
% For the roots of a polynomial like x^5 - x - 1, the Galois group S_5 is an
% unchangeable destiny. Because S_5 is not a solvable group, the roots *cannot*
% be expressed by radicals. This is an algebraic Mountain that no computation
% can circumvent.
constraint_indexing:constraint_classification(galois_theory_symmetry, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PROFESSIONAL MATHEMATICIAN (ROPE)
% For the expert, Galois Theory is a "Rope"—a pure coordination mechanism. It
% allows them to "climb" from the low-level data of coefficients to the
% high-level structure of groups, making intractable problems solvable by
% switching representations.
constraint_indexing:constraint_classification(galois_theory_symmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context sees the constraint for what it is: a
% fundamental, unchangeable law of algebraic structures.
constraint_indexing:constraint_classification(galois_theory_symmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE GRADUATE STUDENT (SNARE)
% To the student, the Galois Correspondence often feels like a "Snare." It
% promises a "simple" link between subfields and subgroups, but the path to
% verifying that link extracts immense cognitive energy and can feel like a
% coercive barrier to understanding. This is a metaphorical classification
% reflecting the high cognitive cost imposed on a constrained agent.
constraint_indexing:constraint_classification(galois_theory_symmetry, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(galois_theory_symmetry_tests).

test(perspectival_gap_mountain_to_rope) :-
    % Verify the gap between the powerless (roots) and institutional (expert) views.
    constraint_indexing:constraint_classification(galois_theory_symmetry, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(galois_theory_symmetry, rope, context(agent_power(institutional), _, _, _)).

test(student_frustration_snare) :-
    % A moderate power agent in a constrained/biographical context sees the abstraction as a Snare.
    constraint_indexing:constraint_classification(galois_theory_symmetry, snare, context(agent_power(moderate), time_horizon(biographical), exit_options(constrained), spatial_scope(local))).

test(analytical_view_is_mountain) :-
    % The analytical view and claim must be Mountain.
    narrative_ontology:constraint_claim(galois_theory_symmetry, mountain),
    constraint_indexing:constraint_classification(galois_theory_symmetry, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_thresholds_met) :-
    narrative_ontology:constraint_metric(galois_theory_symmetry, extractiveness, E),
    narrative_ontology:constraint_metric(galois_theory_symmetry, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(galois_theory_symmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is fundamentally a Mountain, representing a timeless mathematical
 *   truth. The base extractiveness (0.20) is low but non-zero, modeling the
 *   cognitive labor required to understand the theory, which "extracts" the
 *   possibility of simpler solutions. The suppression score (0.05) is set at
 *   the maximum for a Mountain, as it forecloses the search for a general
 *   quintic formula, a path that is structurally impossible. The NL profile
 *   metrics (accessibility_collapse=1.0, resistance=0.0) are maximal/minimal
 *   as appropriate for a mathematical theorem.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant. For the polynomial roots, it is an inescapable
 *   Mountain. For the expert mathematician, it is a Rope—a tool for
 *   coordination and proof. For the student, the high cognitive barrier can
 *   feel like a Snare, extracting effort for passage. This demonstrates how
 *   even a fundamental Mountain can be experienced differently based on an
 *   agent's power, exit options, and relationship to the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are algebraists and physicists who leverage symmetry principles.
 *   Victims are students facing the steep learning curve and historical
 *   mathematicians whose search for a general formula was rendered void. These
 *   declarations allow the engine to derive a high directionality (d) for the
 *   student (victim + constrained exit), leading to a high effective
 *   extraction (χ) and a Snare classification, despite the low base
 *   extractiveness (ε).
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the analytical perspective as a Mountain, the system correctly
 *   identifies the core nature of the constraint as a structural limit. The
 *   other perspectives (Rope, Snare) are understood as context-dependent
 *   experiences of that fundamental reality, preventing the mislabeling of a
 *   mathematical law as a purely social or coercive construct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_inverse_galois_problem,
    'Is every finite group the Galois group of some extension of the rational numbers (Q)?',
    'Formal proof or counterexample; currently unsolved for many groups.',
    'If true, the landscape of algebraic structures is perfectly mirrored by group theory. If false, there are group structures that cannot be realized as symmetries of polynomial roots over Q, implying a mismatch between the two domains.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_inverse_galois_problem, empirical, 'The Inverse Galois Problem: Whether all finite groups can be realized as Galois groups over Q.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(galois_theory_symmetry, 1832, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. Base extractiveness is low (0.20 < 0.46) and the constraint
% is a static mathematical law with no lifecycle drift.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. The standard derivation from beneficiary/victim status and
% exit options correctly models the perspectival experiences.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */