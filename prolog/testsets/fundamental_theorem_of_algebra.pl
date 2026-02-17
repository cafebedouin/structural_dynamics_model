% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_of_algebra
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_fundamental_theorem_of_algebra, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fundamental_theorem_of_algebra
 *   human_readable: Fundamental Theorem of Algebra (FTA)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Fundamental Theorem of Algebra states that every non-constant
 *   single-variable polynomial with complex coefficients has at least one
 *   complex root. This establishes the algebraic closure of the complex
 *   numbers, acting as a fundamental, unchangeable law of the mathematical
 *   universe. It guarantees that solutions exist, foreclosing the possibility
 *   of "rootless" polynomials within this domain.
 *
 * KEY AGENTS (by structural relationship):
 *   - All mathematical practitioners (e.g., Numerical Analysts, Control Engineers):
 *     Agents operating within the mathematical framework where the FTA is an
 *     unalterable feature of the landscape.
 *   - The Polynomial Itself: The mathematical object whose properties are
 *     fixed by the theorem.
 *   - Analytical Observer: Recognizes the theorem as a foundational axiom
 *     with zero degrees of freedom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The theorem "extracts" the possibility of rootless polynomials,
% but this is a definitional closure, not an economic or social extraction.
% The value is minimal, reflecting its nature as a structural boundary.
domain_priors:base_extractiveness(fundamental_theorem_of_algebra, 0.01).

% Rationale: The theorem completely suppresses the existence of alternative
% outcomes (polynomials without roots in C). However, since this is a logical
% consequence of the field axioms, not an imposed rule, the score is low.
domain_priors:suppression_score(fundamental_theorem_of_algebra, 0.01).

% Rationale: A pure mathematical truth has no performative aspect.
domain_priors:theater_ratio(fundamental_theorem_of_algebra, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, extractiveness, 0.01).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Rationale: No alternative is conceivable within the axioms of complex numbers.
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, accessibility_collapse, 1.0).
% Rationale: Resistance is incoherent; one cannot "resist" a mathematical proof.
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fundamental_theorem_of_algebra, mountain).
narrative_ontology:human_readable(fundamental_theorem_of_algebra, "Fundamental Theorem of Algebra (FTA)").

% --- Binary flags ---
% No sunset clause, no active enforcement.

% --- Emergence flag (required for mountain constraints) ---
% The theorem is a logical consequence of the axioms of the complex field.
domain_priors:emerges_naturally(fundamental_theorem_of_algebra).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), the FTA does not have
% structurally distinct beneficiaries or victims. It is a universal feature
% of the mathematical landscape.

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

% UNIFORM-TYPE CONSTRAINT: The Fundamental Theorem of Algebra is a Mountain
% from all perspectives. Its status as a mathematical law is invariant.
% We include multiple perspectives to demonstrate this invariance.

% PERSPECTIVE 1: THE POLYNOMIAL (POWERLESS)
% The mathematical object itself has no freedom to violate the theorem.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE ENGINEER (INSTITUTIONAL)
% An engineer using the theorem relies on it as an unchangeable guarantee.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst recognizes the theorem as a fixed, foundational truth.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_theorem_of_algebra_tests).

test(uniform_mountain_classification) :-
    % Verify that the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(natural_law_profile_adherence) :-
    % Verify that the metrics meet the thresholds for a Mountain classification.
    narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, extractiveness, E),
    narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, suppression_requirement, S),
    config:param(mountain_extractiveness_max, EMax),
    config:param(mountain_suppression_ceiling, SMax),
    E =< EMax,
    S =< SMax.

test(natural_emergence) :-
    domain_priors:emerges_naturally(fundamental_theorem_of_algebra).

:- end_tests(fundamental_theorem_of_algebra_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Fundamental Theorem of Algebra is a canonical example of a Mountain
 *   constraint. Its base extractiveness (0.01) and suppression (0.01) are set
 *   to minimal non-zero values to reflect its function as a logical boundary
 *   that forecloses certain possibilities (rootless polynomials) as a
 *   consequence of its axiomatic system. The Natural Law profile metrics
 *   (accessibility_collapse=1.0, resistance=0.0) and the `emerges_naturally`
 *   flag are critical for ensuring it passes the engine's certification chain
 *   for a natural law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a uniform-type constraint, the FTA is
 *   classified as a Mountain from all possible indices. Its truth is not
 *   contingent on the observer's power, time horizon, exit options, or scope.
 *   The original file's attempt to frame it as a "Snare" for students of real
 *   analysis was rejected as a metaphorical interpretation that misrepresents
 *   the structural nature of the constraint. A student's cognitive difficulty
 *   is not equivalent to structural extraction by the theorem itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable. As a Mountain, the constraint has no
 *   structurally distinct beneficiaries or victims. It is a feature of the
 *   environment for all agents. Therefore, `constraint_beneficiary` and
 *   `constraint_victim` declarations are omitted.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a pure Mountain prevents any misinterpretation of
 *   this foundational mathematical law as a tool of coordination (Rope) or
 *   extraction (Snare). It correctly identifies it as part of the fixed
 *   background reality within which mathematical operations take place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_fta_constructivism,
    'Is the guarantee of a root (existence proof) structurally equivalent to providing a method to find it (constructive proof)?',
    'Analysis of the computational complexity of root-finding algorithms (e.g., Weierstrass method) versus the logical simplicity of the existence proofs (e.g., Liouville''s theorem).',
    'If existence without construction is sufficient, it remains a pure Mountain. If the difficulty of finding the guaranteed object imposes significant costs, it could imply a hidden extractive layer not captured by the theorem itself.',
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_fta_constructivism, conceptual, 'Distinction between existence proofs and constructive proofs and its structural impact.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fundamental_theorem_of_algebra, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a mathematical theorem, its properties are static and do
% not drift over time. Base extractiveness is below the 0.46 threshold for
% required temporal data.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for a foundational mathematical theorem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */