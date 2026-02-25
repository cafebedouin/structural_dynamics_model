% ============================================================================
% CONSTRAINT STORY: cantors_diagonal_argument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cantors_diagonal_argument, []).

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
 *   constraint_id: cantors_diagonal_argument
 *   human_readable: Cantor's Diagonal Argument
 *   domain: mathematics/logic
 *
 * SUMMARY:
 *   Cantor's Diagonal Argument is a mathematical proof demonstrating that the
 *   set of real numbers is uncountably infinite, meaning it cannot be put
 *   into a one-to-one correspondence with the set of natural numbers. This
 *   establishes that there are different 'sizes' of infinity. The argument
 *   functions as a fundamental logical limit, with profound implications for
 *   mathematics, logic, and computer science, most notably in proving the
 *   existence of uncomputable numbers and forming the basis for proofs like
 *   the Halting Problem's undecidability.
 *
 * KEY AGENTS:
 *   - Mathematicians/Logicians: Analytical observers who work with the consequences of the proof as a foundational principle.
 *   - Computer Scientists: Institutional and analytical agents who treat the consequences of diagonalization (e.g., the Halting Problem) as a hard limit on computability.
 *   - Students: Powerless agents who encounter the proof as an immutable logical fact they must accept.
 *   - Mathematical Pluralists (e.g., Intuitionists): Analytical observers who may work in alternative logical systems but still recognize the argument's validity within classical mathematics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cantors_diagonal_argument, 0.01).
domain_priors:suppression_score(cantors_diagonal_argument, 0.02).
domain_priors:theater_ratio(cantors_diagonal_argument, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cantors_diagonal_argument, extractiveness, 0.01).
narrative_ontology:constraint_metric(cantors_diagonal_argument, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(cantors_diagonal_argument, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cantors_diagonal_argument, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(cantors_diagonal_argument, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cantors_diagonal_argument, mountain).
narrative_ontology:human_readable(cantors_diagonal_argument, "Cantor's Diagonal Argument").
narrative_ontology:topic_domain(cantors_diagonal_argument, "mathematics/logic").

domain_priors:emerges_naturally(cantors_diagonal_argument).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The canonical perspective. The argument is a fundamental, unchangeable feature of the mathematical landscape, a law of logic. Its consequences are inescapable within standard axiomatic systems.
constraint_indexing:constraint_classification(cantors_diagonal_argument, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% For a student encountering the proof, it is an inescapable logical conclusion. There is no 'exit' from its truth once the steps are understood; one is trapped by the force of the logic.
constraint_indexing:constraint_classification(cantors_diagonal_argument, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% For an institution, the argument's consequences (e.g., the uncountability of functions, the Halting Problem) are fixed constraints around which curricula and research programs must be built. The institution cannot change the constraint, only adapt to it.
constraint_indexing:constraint_classification(cantors_diagonal_argument, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Even from a perspective that rejects the underlying axioms (like the Law of Excluded Middle), the diagonal argument is recognized as a valid, unchangeable theorem *within* classical mathematics. The 'exit' is to a different mathematical universe (a different axiomatic system), not a change to this one.
constraint_indexing:constraint_classification(cantors_diagonal_argument, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cantors_diagonal_argument_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cantors_diagonal_argument, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cantors_diagonal_argument, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cantors_diagonal_argument, ExtMetricName, E),
    domain_priors:suppression_score(cantors_diagonal_argument, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cantors_diagonal_argument),
    narrative_ontology:constraint_metric(cantors_diagonal_argument, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cantors_diagonal_argument, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cantors_diagonal_argument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a canonical example of a Mountain. Extractiveness (ε=0.01) and Suppression (0.02) are near zero, as a mathematical proof does not extract value or coerce behavior; it merely describes a logical reality. The NL-profile metrics confirm this: it 'emerges_naturally' (true) from the axioms of set theory, has extremely high 'accessibility_collapse' (0.98) as its logic is compelling once understood, and very low 'resistance' (0.05) as challenging it requires rejecting fundamental mathematics. The theater ratio is zero, as the proof is pure function.
 *
 * PERSPECTIVAL GAP:
 *   A key feature of this constraint is the *absence* of a perspectival gap. It classifies as a Mountain from all perspectives, regardless of power, time horizon, or exit options. This invariance is the hallmark of a true natural law or logical limit. The power of the constraint lies in its universal applicability and the fact that all rational observers are forced into the same classification. It serves as a 'true summit' against which socially constructed 'false summits' can be measured.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint representing a logical truth, there are no structural beneficiaries or victims. The concept does not apply. The engine will use canonical directionality values for each power atom, but with an ε of 0.01, the effective extraction (χ) will be negligible for all observers, reinforcing the universal Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint represents a baseline case where no mandatrophy exists. It is unambiguously a Mountain. It cannot be mistaken for a Snare or Rope because it lacks the necessary properties of extraction, suppression, or coordination. It provides a grounding example of a constraint whose structure is entirely independent of social or political construction, making it a crucial calibration point for the entire classification system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cantors_diagonal_argument, 1891, 9999).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(cantors_diagonal_argument, halting_problem).
narrative_ontology:affects_constraint(cantors_diagonal_argument, goedels_incompleteness_theorems).

% DUAL FORMULATION NOTE:
% Cantor's argument is a foundational proof technique. Its structure is reused to establish other 'Mountain' constraints in logic and computer science, such as the undecidability of the Halting Problem and Gödel's Incompleteness Theorems. It is the upstream source of the diagonalization method.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
