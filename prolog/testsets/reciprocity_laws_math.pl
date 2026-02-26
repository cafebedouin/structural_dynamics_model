% ============================================================================
% CONSTRAINT STORY: reciprocity_laws_math
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_reciprocity_laws_math, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reciprocity_laws_math
 *   human_readable: Mathematical Reciprocity Laws (Quadratic)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Reciprocity laws, beginning with Gauss's Quadratic Reciprocity, describe a
 *   deep, non-obvious symmetry in the behavior of prime numbers. They state
 *   that the solvability of an equation x^2 ≡ p (mod q) is intrinsically
 *   linked to the solvability of x^2 ≡ q (mod p). This functions as a
 *   structural constraint on how primes interact, a fundamental feature of
 *   the integers. This story models the foundational (quadratic) law, which
 *   is a pure mountain.
 *
 * KEY AGENTS (by structural relationship):
 *   - Prime Numbers (p, q): Subjects of the law (powerless/trapped) — their relationship is fixed.
 *   - Cryptographer: User of the law (institutional/mobile) — applies the law's properties but cannot change them.
 *   - Number Theorist: Analytical observer — studies the law and its generalizations.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of 2 stories decomposed from "Reciprocity Laws".
 * Decomposed because ε differs across observables (ε-invariance principle).
 * This story covers the foundational Quadratic Reciprocity law, a proven theorem.
 * The cognitive complexity and contested nature of its generalizations are modeled
 * in a separate, more extractive constraint.
 * Related stories:
 *   - reciprocity_laws_math (ε=0.05, Mountain)
 *   - langlands_program_complexity (ε=0.45, Tangled Rope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: A proven mathematical theorem has near-zero extraction. It simply
% describes the structure of reality.
domain_priors:base_extractiveness(reciprocity_laws_math, 0.05).
% Rationale: The law does not suppress alternatives; it renders them logically
% incoherent within standard axioms. Suppression is extremely low.
domain_priors:suppression_score(reciprocity_laws_math, 0.02).
% Rationale: A mathematical truth has no performative aspect.
domain_priors:theater_ratio(reciprocity_laws_math, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reciprocity_laws_math, extractiveness, 0.05).
narrative_ontology:constraint_metric(reciprocity_laws_math, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(reciprocity_laws_math, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Rationale: A proven theorem completely forecloses alternatives.
narrative_ontology:constraint_metric(reciprocity_laws_math, accessibility_collapse, 1.0).
% Rationale: Resistance to a mathematical proof is incoherent.
narrative_ontology:constraint_metric(reciprocity_laws_math, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(reciprocity_laws_math, mountain).
narrative_ontology:human_readable(reciprocity_laws_math, "Mathematical Reciprocity Laws (Quadratic)").
narrative_ontology:topic_domain(reciprocity_laws_math, "mathematical/logical").

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the structure of number fields.
domain_priors:emerges_naturally(reciprocity_laws_math).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a pure mountain (natural law), this constraint
% does not have beneficiaries or victims in the structural sense. Its effects
% are universal and symmetric for all observers within its logical domain.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   This is a uniform-type constraint (Mountain-only). The classification is
   invariant across all perspectives because it is a natural law.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIME NUMBER (SUBJECT)
% For a prime number, the law is an unchangeable, fixed feature of its
% mathematical reality with zero degrees of freedom.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE CRYPTOGRAPHER (USER)
% An engineer applying the law cannot alter it. While its application can be
% seen as a tool (Rope), the law itself remains a Mountain—a fixed property
% of the landscape they are building on.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical observer sees a fundamental, proven symmetry of number
% theory, the very definition of a mathematical Mountain.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reciprocity_laws_math_tests).

test(classification_invariance) :-
    % Verify that the classification is 'mountain' from multiple key perspectives.
    constraint_indexing:constraint_classification(reciprocity_laws_math, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reciprocity_laws_math, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(reciprocity_laws_math, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(natural_law_metrics) :-
    % Verify that the constraint meets the metric thresholds for a mountain.
    narrative_ontology:constraint_metric(reciprocity_laws_math, extractiveness, E),
    narrative_ontology:constraint_metric(reciprocity_laws_math, suppression_requirement, S),
    config:param(mountain_extractiveness_max, EMax),
    config:param(mountain_suppression_ceiling, SMax),
    E =< EMax,
    S =< SMax.

test(emergence_flag_present) :-
    domain_priors:emerges_naturally(reciprocity_laws_math).

:- end_tests(reciprocity_laws_math_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint was refactored to be a pure Mountain, modeling only the
 *   foundational Quadratic Reciprocity law. Its metrics (ε=0.05, S=0.02) are
 *   set well below the Mountain thresholds to reflect its status as a proven,
 *   non-extractive mathematical theorem. The required Natural Law profile
 *   metrics (accessibility_collapse=1.0, resistance=0.0) and the
 *   `emerges_naturally` flag are included to ensure correct classification
 *   and certification by the engine.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a natural law (NL), the classification
 *   is Mountain from all possible indices (NL(C) → ■C[I] for all I). The
 *   various perspectives included demonstrate this invariance.
 *
 * DIRECTIONALITY LOGIC:
 *   As a pure Mountain, this constraint does not have beneficiaries or victims
 *   in the structural sense required for directionality derivation. Its
 *   effects are symmetric and universal. No beneficiary/victim declarations
 *   are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The original file conflated the simple, proven Quadratic Reciprocity law
 *   with the vastly more complex and partially unproven Langlands Program.
 *   This violated the ε-invariance principle, as the "extraction" (cognitive
 *   load) is vastly different. By decomposing the concept into two separate
 *   constraints (`reciprocity_laws_math` and `langlands_program_complexity`),
 *   we can accurately model the foundational law as a Mountain without being
 *   distorted by the Tangled Rope nature of its frontier generalizations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
% This question relates to the *next* constraint in the family, which this one affects.
%
% /5 form: narrative detail for story context
omega_variable(
    omega_reciprocity_laws_math,
    'Will the Langlands Program, the grand generalization of reciprocity, ever become a fully functional tool ("Rope") or will its complexity keep it a "Tangled Rope" for specialists?',
    'Verification of the global Langlands correspondence for GL(n) over number fields.',
    'If resolved, it would unify vast areas of mathematics. If not, number theory remains fragmented.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_reciprocity_laws_math, empirical, 'Unproven status of the global Langlands correspondence.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(reciprocity_laws_math, 1801, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.05) is below the 0.46 threshold for
% mandatory temporal tracking. As a mathematical law, its properties are static.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% --- Network Decomposition (Constraint Families) ---
% This constraint is the foundational Mountain that enables the more complex,
% extractive constraints related to its generalizations.
narrative_ontology:affects_constraint(reciprocity_laws_math, langlands_program_complexity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. This is a pure mountain with no directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */