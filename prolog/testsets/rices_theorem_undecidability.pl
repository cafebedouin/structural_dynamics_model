% ============================================================================
% CONSTRAINT STORY: rices_theorem_undecidability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_rices_theorem_undecidability, []).

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
 *   constraint_id: rices_theorem_undecidability
 *   human_readable: Rice's Theorem (Undecidability of Semantic Properties)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   Rice's Theorem is a fundamental result in computability theory stating that
 *   any non-trivial semantic property of a program (i.e., about the language
 *   it recognizes) is undecidable. This means it is logically impossible to
 *   create a general-purpose algorithm that can determine any behavioral
 *   trait of arbitrary code, such as "will this program ever crash?" or "is
 *   this code malicious?". It is a direct generalization of the Halting Problem.
 *
 * KEY AGENTS (by structural relationship):
 *   - Software Verifier (e.g., static analysis tool developer): An institutional
 *     agent for whom the theorem is a fixed, natural boundary condition.
 *   - Cybersecurity Analyst: A moderate-power agent who must contend with the
 *     consequences of the theorem, as it prevents perfect automated threat detection.
 *   - Executing Program: The subject of analysis, whose properties are
 *     fundamentally unknowable by a general algorithm.
 *   - Analytical Observer: A computability theorist who sees the theorem as a
 *     foundational, unchangeable law of logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a mathematical theorem, its extractiveness is near zero. It
% does not extract value; it describes a logical boundary. The "cost" it
% imposes is a consequence of attempting to build systems that defy this limit.
domain_priors:base_extractiveness(rices_theorem_undecidability, 0.05).
% Rationale: The theorem suppresses impossible alternatives (e.g., a perfect
% general virus scanner). This suppression is a feature of logical reality.
domain_priors:suppression_score(rices_theorem_undecidability, 0.02).
% Rationale: A mathematical theorem has no theatrical component.
domain_priors:theater_ratio(rices_theorem_undecidability, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rices_theorem_undecidability, extractiveness, 0.05).
narrative_ontology:constraint_metric(rices_theorem_undecidability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(rices_theorem_undecidability, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Accessibility Collapse: 1.0. No alternative is conceivable within standard
% models of computation. The boundary is absolute.
narrative_ontology:constraint_metric(rices_theorem_undecidability, accessibility_collapse, 1.0).
% Resistance: 0.0. One cannot "resist" a mathematical theorem; one can only
% work within the boundaries it defines. Resistance is incoherent.
narrative_ontology:constraint_metric(rices_theorem_undecidability, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(rices_theorem_undecidability, mountain).
narrative_ontology:human_readable(rices_theorem_undecidability, "Rice's Theorem (Undecidability of Semantic Properties)").
narrative_ontology:topic_domain(rices_theorem_undecidability, "technological/mathematical").

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the logic of Turing machines and the Halting Problem.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(rices_theorem_undecidability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a mountain (natural law), the concepts of
% beneficiary and victim are inapplicable. The theorem is a statement of fact
% about a logical system, not a mechanism for social or economic transfer.

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

% UNIFORM-TYPE CONSTRAINT: As a natural law of computation, Rice's Theorem
% classifies as a Mountain from all perspectives. The classification is
% invariant. NL(C) → ■C[I] for all I.

% PERSPECTIVE 1: THE CYBERSECURITY ANALYST
% For the analyst, the theorem is an unchangeable wall that prevents the
% creation of a perfect, general-purpose malware detector. It is a fixed
% feature of their professional landscape.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE STATIC ANALYSIS ARCHITECT
% For the architect of a compiler or linter, the theorem is a natural law
% that defines the boundaries of their work. It justifies why tools can only
% offer heuristics and warnings, not absolute guarantees.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (COMPUTABILITY THEORIST)
% The theorist sees the theorem in its purest form: a logical, unchangeable
% truth derived from the axioms of computation.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rices_theorem_undecidability_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from different perspectives,
    % demonstrating its status as a uniform-type natural law.
    constraint_indexing:constraint_classification(rices_theorem_undecidability, Type1,
        context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(rices_theorem_undecidability, Type2,
        context(agent_power(institutional), _, _, _)),
    Type1 == mountain,
    Type2 == mountain.

test(natural_law_profile_metrics) :-
    % Verify that the required NL profile metrics are present and correct.
    narrative_ontology:constraint_metric(rices_theorem_undecidability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rices_theorem_undecidability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(natural_emergence) :-
    domain_priors:emerges_naturally(rices_theorem_undecidability).

:- end_tests(rices_theorem_undecidability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.05) and suppression (0.02) are set very low,
 *   reflecting the constraint's nature as a mathematical theorem. It is a
 *   statement of fact, not an extractive mechanism. The classification is
 *   uniformly 'mountain' because a logical impossibility is a fixed,
 *   unchangeable boundary for all agents, regardless of their power, time
 *   horizon, or exit options. The full Natural Law (NL) profile metrics
 *   (accessibility_collapse=1.0, resistance=0.0, emerges_naturally=true) are
 *   provided to ensure it passes the engine's `natural_law_signature`
 *   certification.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap; this is a uniform-type constraint. While
 *   different agents *experience the consequences* of the theorem differently
 *   (e.g., a security analyst's frustration vs. a theorist's intellectual
 *   satisfaction), the classification of the theorem *itself* remains
 *   invariant. It is a Mountain for everyone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations are omitted as they are inapplicable
 *   to a natural law. One cannot be a "victim" of a mathematical proof. This
 *   is a key distinction for Mountain constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The original file misclassified this constraint from multiple perspectives
 *   (Rope, Snare, Tangled Rope), conflating the theorem with the consequences
 *   of building systems in its shadow. By applying the ε-invariance principle,
 *   this version correctly identifies the theorem itself as a low-extraction
 *   Mountain. The "Snare" experienced by a security analyst is a separate,
 *   downstream constraint (e.g., `insecure_software_development_paradigm`)
 *   which is *affected by* Rice's Theorem but has a much higher ε. This
 *   decomposition prevents mislabeling a law of logic as an extractive tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_rices_theorem,
    'Can near-perfect heuristics (e.g., advanced AI) functionally erase the boundary set by Rice''s Theorem for practical purposes?',
    'Longitudinal study (10+ years) of false-negative rates for AI-driven code analysis tools against novel, complex malware.',
    'If yes, the practical impact of the Mountain is negligible, even if the logical boundary remains. If no, the Mountain continues to enforce a permanent, high cost on software security.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_rices_theorem, empirical, 'Whether heuristic methods can asymptotically approach perfect accuracy, mitigating the practical effects of undecidability.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rices_theorem_undecidability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.05) is below the 0.46 threshold for
% mandatory temporal tracking. As a mathematical theorem, its properties do
% not drift over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% Rice's Theorem is a direct generalization of the Halting Problem.
narrative_ontology:affects_constraint(halting_problem_undecidability, rices_theorem_undecidability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a uniform-type Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */