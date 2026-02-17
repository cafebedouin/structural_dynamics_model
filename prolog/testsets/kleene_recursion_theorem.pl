% ============================================================================
% CONSTRAINT STORY: kleene_recursion_theorem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
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
 *   constraint_id: kleene_recursion_theorem
 *   human_readable: Kleene's Second Recursion Theorem
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   Kleene's Second Recursion Theorem proves that for any computable function
 *   that transforms programs, there exists a "fixed-point" program that can
 *   access its own source code. It is the formal foundation for
 *   self-referential logic in any Turing-complete system. This constraint is
 *   a Mountain: a fundamental, unchangeable feature of the logical landscape
 *   of computation. While its *applications* can be Ropes (e.g., self-hosting
 *   compilers) or Snares (e.g., polymorphic malware), the theorem itself is
 *   an invariant fact with no inherent extractiveness.
 *
 * KEY AGENTS (by structural relationship):
 *   - Any Turing-Complete System: The subject domain where the theorem holds true (powerless/trapped).
 *   - The Computer Scientist: An observer who maps this feature of the logical landscape (analytical/analytical).
 *   - The Language Designer: An actor who must account for this property in system design (institutional/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% A mathematical theorem has no inherent extractiveness.
domain_priors:base_extractiveness(kleene_recursion_theorem, 0.02).
% It does not suppress alternatives; it is a logical consequence of the axioms of computation.
domain_priors:suppression_score(kleene_recursion_theorem, 0.01).
% The theorem is pure substance; no performative theater.
domain_priors:theater_ratio(kleene_recursion_theorem, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kleene_recursion_theorem, extractiveness, 0.02).
narrative_ontology:constraint_metric(kleene_recursion_theorem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(kleene_recursion_theorem, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Alternatives to the theorem are logically impossible within a Turing-complete system.
narrative_ontology:constraint_metric(kleene_recursion_theorem, accessibility_collapse, 1.0).
% One cannot "resist" a mathematical proof.
narrative_ontology:constraint_metric(kleene_recursion_theorem, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(kleene_recursion_theorem, mountain).
narrative_ontology:human_readable(kleene_recursion_theorem, "Kleene's Second Recursion Theorem").

% --- Emergence flag (required for mountain constraints) ---
% The theorem emerges naturally from the structure of partial recursive functions.
domain_priors:emerges_naturally(kleene_recursion_theorem).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), the theorem has no
% inherent beneficiaries or victims. These roles emerge from its applications,
% which are separate constraints.

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

% PERSPECTIVE 1: THE COMPUTATIONAL SYSTEM (MOUNTAIN)
% A function within a Turing-complete system cannot escape the logic of
% fixed points. It is a fundamental, unchangeable property of its environment.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE LANGUAGE DESIGNER (MOUNTAIN)
% An institutional actor designing a programming language must contend with
% the existence of self-reference. It is a feature of the logical terrain,
% not a design choice that can be opted out of without sacrificing universality.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a detached, analytical viewpoint, the theorem is a foundational truth
% of computability theory, classifying as a pure Mountain.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kleene_recursion_theorem_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(kleene_recursion_theorem, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kleene_recursion_theorem, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(kleene_recursion_theorem, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(natural_emergence) :-
    domain_priors:emerges_naturally(kleene_recursion_theorem).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kleene_recursion_theorem, ExtMetricName, E),
    config:param(mountain_extractiveness_max, MaxE),
    E =< MaxE.

:- end_tests(kleene_recursion_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core theorem is a fact of mathematical logic, not a system of
 *   extraction or coordination. Therefore, its base extractiveness (ε=0.02)
 *   and suppression (0.01) are minimal, qualifying it as a Mountain. The
 *   previous classification as Tangled Rope conflated the theorem with its
 *   downstream applications (e.g., malware), which are distinct constraints.
 *   This version adheres to the ε-invariance principle by modeling only the
 *   theorem itself. The Natural Law (NL) profile metrics are set to their
 *   extremes (accessibility_collapse=1.0, resistance=0.0) because a
 *   mathematical proof is both inescapable and impossible to resist.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a Mountain of logic, the theorem is
 *   classified identically from all perspectives. The different ways agents
 *   interact with it (e.g., using it as a Rope for compilers, or seeing it as
 *   a Snare in security) are properties of those *application constraints*,
 *   not of the theorem itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. As a Mountain, there are no inherent beneficiaries or
 *   victims, so no structural relationship declarations are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the theorem as a Mountain, we avoid mislabeling a
 *   fundamental property of reality as a system of coordination or
 *   extraction. This correctly separates the logical foundation from the
 *   social or technical systems built upon it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_kleene_recursion_theorem,
    'Does logical self-reference enable spontaneous goal-emergence in autonomous systems?',
    'Long-term observation of self-referential agent simulations with open-ended reward functions.',
    'If Yes: The application of the theorem becomes a Snare for safety engineers. If No: Its application remains a Rope for tool-building.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_kleene_recursion_theorem, conceptual, 'Does logical self-reference enable spontaneous goal-emergence in autonomous systems?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(kleene_recursion_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints (base_extractiveness <= 0.46).
% As a mathematical theorem, its properties are static and do not drift.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% As a Mountain, it has no coordination function.
% narrative_ontology:coordination_type(kleene_recursion_theorem, [type]).

% Network relationships (structural influence edges)
% This theorem would affect constraints related to computability and AI safety.
% narrative_ontology:affects_constraint(kleene_recursion_theorem, halting_problem).
% narrative_ontology:affects_constraint(kleene_recursion_theorem, godels_incompleteness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a uniform-type Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */