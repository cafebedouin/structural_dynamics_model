% ============================================================================
% CONSTRAINT STORY: hamiltonian_path_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hamiltonian_path_complexity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hamiltonian_path_complexity
 *   human_readable: Computational Complexity of the Hamiltonian Path Problem
 *   domain: mathematical/computational_complexity
 *
 * SUMMARY:
 *   The Hamiltonian path problem exemplifies a mathematical constraint:
 *   determining whether a path exists in a graph that visits each vertex
 *   exactly once is NP-complete. This means that while verifying a proposed
 *   solution takes polynomial time, constructing one for arbitrary instances
 *   requires exponential time under current computational models. The
 *   constraint is not institutional, policy-driven, or contingent on
 *   technology — it is a structural property of the problem class itself. No
 *   amount of engineering, funding, or organizational effort can overcome the
 *   theoretical lower bound for arbitrary instances. The constraint is
 *   invariant across all Turing-complete computational models and across all
 *   known optimization techniques. It emerges naturally from the definition
 *   of the problem and the formal theory of computational complexity.
 *
 * KEY AGENTS:
 *   - Computational Theorists (analytical/analytical): Discover and formalize the constraint; see it as a mathematical necessity
 *   - Algorithm Designers (powerful/constrained): Attempt to find faster algorithms within the bounds; work against the constraint
 *   - Application Domains (institutional/mobile): Logistics, chip design, bioinformatics; seek to solve instances; accept the constraint and work around it via approximation or restriction
 *   - Quantum Computing Researchers (analytical/mobile): Explore whether alternative computational models might escape the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hamiltonian_path_complexity, 0.12).
domain_priors:suppression_score(hamiltonian_path_complexity, 0.03).
domain_priors:theater_ratio(hamiltonian_path_complexity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hamiltonian_path_complexity, extractiveness, 0.12).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hamiltonian_path_complexity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hamiltonian_path_complexity, mountain).
narrative_ontology:human_readable(hamiltonian_path_complexity, "Computational Complexity of the Hamiltonian Path Problem").
narrative_ontology:topic_domain(hamiltonian_path_complexity, "mathematical/computational_complexity").

domain_priors:emerges_naturally(hamiltonian_path_complexity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL THEORIST (MOUNTAIN) — The Hamiltonian path problem is NP-complete; no known polynomial-time algorithm exists. This is a structural property of the problem space itself, not a consequence of insufficient engineering or research effort. The complexity is invariant across all computational models capable of recognizing the problem. No exit, no arbitrage, no benefit. Universal, civilizational, unchangeable.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM DESIGNER (MOUNTAIN) — Even the most sophisticated heuristics, approximations, and parallel approaches cannot overcome the exponential barrier for arbitrary instances. The constraint is that for large instances, verification is tractable (polynomial-time) but construction remains intractable. This asymmetry is structural, not a limitation of current tools. Constrained by the mathematics, not by institutional arrangements.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: APPLICATION DOMAIN (MOUNTAIN) — Industries seeking to solve the traveling salesman and route optimization variants face a genuine mathematical limit. They can reformulate as approximation problems, use heuristics, or apply constraints that make instances tractable, but they cannot exit the underlying complexity. The constraint remains even when agents have high agency and resources. The complexity is not imposed; it is discovered.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hamiltonian_path_complexity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, ExtMetricName, E),
    domain_priors:suppression_score(hamiltonian_path_complexity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hamiltonian_path_complexity),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hamiltonian_path_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint does not extract resources from anyone; it is a structural fact. The measurement reflects minimal institutional overhead (theorem-proving, conference attendance) required to maintain awareness of the constraint, not extraction per se. Suppression (0.03): Negligible. The constraint is not suppressed because there is no alternative to suppress. It simply exists as a mathematical fact. Theater ratio (0.15): Very low. The Hamiltonian path problem is stated precisely and verified directly through formal proof. There is minimal performative content — the complexity is demonstrated through rigorous reduction proofs, not through ritual or institutional arrangement. The slight nonzero value reflects that new proofs of NP-hardness variants do accumulate over time, some for novel rather than fundamental reasons.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives converge on the mountain classification. There is no disagreement about the fundamental nature of the constraint. The analytical theorist, the algorithm designer, and the application domain all recognize that the problem is genuinely hard. The perspectival variations are in time horizon (civilizational vs biographical vs generational) and exit options (analytical vs constrained vs mobile), but these do not change the classification. This is characteristic of a true mountain constraint: it appears the same to all observers because it is invariant across observables and measurement methodologies.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no directionality in the classical sense because there are no beneficiaries or victims. The constraint is not an extraction mechanism; it is a structural fact. All agents are equally subject to it. The 'extraction' flowing from the constraint is zero — no agent benefits at another's expense. All agents experience the constraint as an unchangeable limitation imposed by mathematics, not by institutional power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_vs_np_resolution,
    'If P=NP is proven true, does the Hamiltonian path problem cease to be a constraint?',
    'Mathematical proof of P=NP or proof of P≠NP. Resolves the ontological status of the complexity class.',
    'If P=NP: the constraint becomes a transient epistemological artifact (we don''t yet know the polynomial algorithm). Mountain persists but loses its permanence claim. If P≠NP is proven: mountain is confirmed as a mathematical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p_vs_np_resolution, conceptual, 'Whether P=NP resolution changes the constraint''s character').

omega_variable(
    quantum_supremacy_escape,
    'Could quantum computers solve Hamiltonian path in polynomial time, converting the classical mountain into a technological advantage for quantum agents?',
    'Experimental quantum algorithm achieving subexponential runtime on instances where classical methods require exponential time. Demonstration that the quantum speedup is genuine and scalable.',
    'If yes: the mathematical constraint persists but the practical constraint for quantum-equipped agents disappears (two different constraints). If no: mountain persists across all known computational models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_supremacy_escape, empirical, 'Whether quantum computing provides polynomial-time escape from Hamiltonian path NP-completeness').

omega_variable(
    instance_space_restriction,
    'Are all practical instances of Hamiltonian path instances in a restricted subclass that admits polynomial algorithms (planar graphs, specific degree bounds, special structure)?',
    'Empirical analysis of real-world optimization problems: are they drawn from a restricted instance space that the theory does not cover? Provable algorithmic results for restricted classes.',
    'If yes: the theoretical mountain applies to arbitrary graphs, but practical constraint is rope or scaffold (restricted to solvable subproblems). If no: mountain applies broadly to practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instance_space_restriction, empirical, 'Whether practical instances fall into polynomial-solvable subclasses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hamiltonian_path_complexity, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hpath_tr_t0, hamiltonian_path_complexity, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hpath_tr_t30, hamiltonian_path_complexity, theater_ratio, 30, 0.12).
narrative_ontology:measurement(hpath_tr_t60, hamiltonian_path_complexity, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(hpath_be_t0, hamiltonian_path_complexity, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hpath_be_t30, hamiltonian_path_complexity, base_extractiveness, 30, 0.11).
narrative_ontology:measurement(hpath_be_t60, hamiltonian_path_complexity, base_extractiveness, 60, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hamiltonian_path_complexity, np_completeness_reduction_class).
narrative_ontology:affects_constraint(hamiltonian_path_complexity, traveling_salesman_routing_hardness).
narrative_ontology:affects_constraint(hamiltonian_path_complexity, circuit_layout_verification).

% DUAL FORMULATION NOTE:
% The Hamiltonian path problem is upstream of many applied optimization constraints (TSP, circuit layout, protein folding) that inherit its NP-completeness through reduction. These downstream constraints may appear as snares or tangled ropes in practice because they involve institutional actors, extraction mechanisms, and workarounds. The mathematical mountain is the root constraint; the practical constraints are perspectival readings of how agents respond to the mountain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
