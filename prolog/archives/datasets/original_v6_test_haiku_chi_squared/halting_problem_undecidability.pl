% ============================================================================
% CONSTRAINT STORY: halting_problem_undecidability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_halting_problem_undecidability, []).

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
 *   constraint_id: halting_problem_undecidability
 *   human_readable: The Undecidability of the Halting Problem
 *   domain: mathematical/theoretical_computer_science
 *
 * SUMMARY:
 *   The Halting Problem, proven undecidable by Turing and independently by
 *   Church, is the archetypal mathematical constraint. It states: no
 *   general-purpose algorithm can exist that, given an arbitrary program and
 *   input, correctly decides whether the program will terminate or run
 *   forever. The proof is constructive (diagonal argument) and independent of
 *   any empirical facts, computational substrate, or historical era. The
 *   constraint exhibits zero degrees of freedom: it cannot be circumvented,
 *   negotiated, or mitigated through institutional design or technological
 *   innovation. The undecidability is not a contingent limitation of current
 *   computers — it is a limitation on all possible computation within the
 *   Turing model and all equivalent models. This makes it the canonical
 *   mountain constraint in computational theory.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Institutional beneficiary — the undecidability defines computability theory as a field and provides the research agenda for understanding decidability and complexity
 *   - Computer Science Educators: Institutional beneficiary — the constraint is foundational knowledge that justifies the teaching of theoretical computer science
 *   - Industrial Software Engineers: Powerful agents constrained — cannot build universal program verifiers despite technical resources
 *   - Developers of Safety-Critical Systems: Powerless agents facing practical limits — cannot obtain absolute guarantees of correctness for arbitrary programs
 *   - Theoretical Computer Science Community: Analytical observers — recognize the constraint as a logical invariant of formal systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(halting_problem_undecidability, 0.08).
domain_priors:suppression_score(halting_problem_undecidability, 0.02).
domain_priors:theater_ratio(halting_problem_undecidability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(halting_problem_undecidability, extractiveness, 0.08).
narrative_ontology:constraint_metric(halting_problem_undecidability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(halting_problem_undecidability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(halting_problem_undecidability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(halting_problem_undecidability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(halting_problem_undecidability, mountain).
narrative_ontology:human_readable(halting_problem_undecidability, "The Undecidability of the Halting Problem").
narrative_ontology:topic_domain(halting_problem_undecidability, "mathematical/theoretical_computer_science").

domain_priors:emerges_naturally(halting_problem_undecidability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL COMMUNITY (MOUNTAIN) — The undecidability of the halting problem is a universal logical limit. From the proof-verification standpoint, the constraint is invariant across all computational substrates and time horizons. No amount of computational power, cleverness, or alternative frameworks can overcome this limit. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. The mathematical observer sees an immutable law.
constraint_indexing:constraint_classification(halting_problem_undecidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTER SCIENCE FOUNDATIONAL THEORY (MOUNTAIN) — Academic institutions teaching computability theory treat the halting problem as an established, unchangeable constraint on what computation can ever achieve. The undecidability is a bedrock principle used to classify other decision problems (Rice's theorem, Post's problem). This perspective experiences the constraint as natural law: a ceiling on algorithmic power that no institutional capability can circumvent. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Institutional beneficiary of the constraint (defines the field; provides research agenda).
constraint_indexing:constraint_classification(halting_problem_undecidability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: INDUSTRIAL SOFTWARE ENGINEERING (MOUNTAIN) — Software engineers in industry encounter the halting problem as a practical constraint on static analysis and program verification. They cannot build a universal halting detector, no matter how many engineers or resources they deploy. The constraint is independent of market conditions, technology stacks, or funding. Mobile agents (companies can pivot to other verification approaches) still face the undecidable limit. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.05. Practical manifestation of an immutable limit.
constraint_indexing:constraint_classification(halting_problem_undecidability, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: DEVELOPERS SEEKING UNIVERSAL VERIFICATION (MOUNTAIN) — A developer building safety-critical systems (autonomous vehicles, medical devices, aircraft control) cannot escape the undecidability constraint. Even with all resources, expertise, and time, a universal halting detector cannot exist. The limit is the same for the powerless and powerful. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.11. The constraint manifests identically regardless of structural position because it is a logical limit, not a social extraction.
constraint_indexing:constraint_classification(halting_problem_undecidability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 5: LOGICAL STRUCTURE OF COMPUTATION (MOUNTAIN) — From the deepest analytical perspective, the halting problem undecidability is a consequence of diagonalization and self-reference, properties of formal systems themselves. It is not contingent on any implementation, any hardware, any sociological factor. The limit follows from axioms of logic and set theory. This is the purest mountain perspective: the constraint is an invariant of the mathematical universe, independent of observers. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(halting_problem_undecidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(halting_problem_undecidability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(halting_problem_undecidability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(halting_problem_undecidability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(halting_problem_undecidability, ExtMetricName, E),
    domain_priors:suppression_score(halting_problem_undecidability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(halting_problem_undecidability),
    narrative_ontology:constraint_metric(halting_problem_undecidability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(halting_problem_undecidability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(halting_problem_undecidability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The halting problem undecidability does not extract value from any agent or confer asymmetric benefit. It is a constraint on what is logically possible, not a mechanism for redistribution. All agents face the same wall. The minimal non-zero value (0.08 rather than 0.0) reflects that the constraint's existence is meaningful — it shapes research priorities and industrial strategy — but this is not extraction. Suppression (0.02): Negligible. The undecidability suppresses no alternatives because it is not an enforcement mechanism. It is a logical limit. There are no suppressed exit options to choose. Theater ratio (0.15): Very low. The proof of undecidability is direct and non-performative. A university course on the halting problem may include pedagogical scaffolding, but the constraint itself — the mathematical fact — requires no theater. The small non-zero value reflects pedagogical presentation, not functional degradation.
 *
 * PERSPECTIVAL GAP:
 *   The halting problem is rare: it produces minimal perspectival gaps because it is a true mountain. All agents, regardless of power, time horizon, or exit options, encounter the same logical limit. A powerless developer and a powerful AI researcher both face the same undecidability. An institutional computer science department and a startup both must work around the constraint, not against it. The perspectival gap arises only in how different agents respond to the constraint — institutions use it to define curricula, industry works with heuristics and partial correctness, safety-critical developers use model checking on finite state spaces — but all perspectives agree on the fundamental fact: no universal halting detector can exist. This agreement across all structural positions is the signature of a true mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Because the halting problem is a mountain (natural law), directionality is orthogonal to the classification. All agents have d≈0.72 from the analytical perspective (universal observer) or varying d values from specific structural positions, but all compute to mountain classification regardless. The constraint's universality means the sigmoid function f(d) applied to any reasonable d still yields χ-values consistent with a mountain (ε=0.08, suppression=0.02 are so low that χ remains negligible across all scopes and directional values). This is why mountains are invariant: the base metrics are so constraining that perspectival variation cannot push the constraint into another type.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. The halting problem exhibits zero mandatrophy because it is a pure mountain with no coordination function and no extraction. Mandatrophy arises when a constraint claims to be pure extraction (snare) but actually serves coordination, or claims to be coordination (rope) but extracts value. The halting problem is neither — it is a limit. The mathematical structure admits no alternative framing: logical diagonalization and self-reference are universal. The proof is independent of observer position, institutional context, or measurement methodology. This is why the constraint is fully resolved (not contested): the mathematical community (institutional beneficiary) and all other agents agree on the undecidability. The constraint's existence benefits the mathematical discipline by providing conceptual foundations, but this is not extraction — all agents benefit from clarity about the limits of computation. The constraint is stable across time (measurements show constant theater and extractiveness) because undecidability is timeless. No degradation from rope to piton occurs, and no transition from snare to coordination occurs, because the constraint never had those forms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hypercomputation_existence,
    'Could theoretical hypercomputation (oracle machines, infinite-time computation, quantum oracles) solve the halting problem outside the Turing model?',
    'Formal proof in extended models of computation; investigation of whether oracle machines can be universally constructed without begging the question; analysis of whether physical instantiation of hypercomputation is possible',
    'If hypercomputation exists and is physically realizable: the constraint is model-dependent (not universal mountain). If hypercomputation is logically incoherent or unphysical: undecidability is genuinely universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hypercomputation_existence, conceptual, 'Whether oracle machines or hypercomputation could solve halting outside Turing model').

omega_variable(
    physical_realizability_of_turing_machines,
    'Is the Turing machine model itself an accurate description of physical computation, or is it a limiting mathematical idealization?',
    'Investigation of quantum computing''s actual capabilities vs. Turing equivalence; analysis of whether the physical universe admits unbounded tape memory; determination of whether real physical systems can instantiate the infinite-precision arithmetic Turing machines assume',
    'If Turing machines are physically universal: halting undecidability is a constraint on all realizable computation. If Turing machines are an idealization: the undecidability may not apply to physical computation at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_realizability_of_turing_machines, empirical, 'Whether Turing model accurately describes physical computation').

omega_variable(
    oracle_relativization_consistency,
    'In a formal system with an oracle for the halting problem (oracle Turing machine), is that oracle itself subject to undecidability (at the next level), or does the oracle truly escape the problem?',
    'Proof analysis of oracle hierarchy; verification that relativized halting problems genuinely separate at each level; investigation of whether meta-level undecidability is a feature or artifact',
    'If oracle hierarchy is consistent: undecidability is universal (every level has uncomputable problems). If hierarchy collapses: lower-level undecidability may be an artifact of limited models.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_relativization_consistency, conceptual, 'Whether oracle hierarchy escapes or extends undecidability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(halting_problem_undecidability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(halt_tr_t0, halting_problem_undecidability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(halt_tr_t50, halting_problem_undecidability, theater_ratio, 50, 0.13).
narrative_ontology:measurement(halt_tr_t100, halting_problem_undecidability, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(halt_be_t0, halting_problem_undecidability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(halt_be_t50, halting_problem_undecidability, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(halt_be_t100, halting_problem_undecidability, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(halting_problem_undecidability, information_standard).
narrative_ontology:affects_constraint(halting_problem_undecidability, rices_theorem_undecidability).
narrative_ontology:affects_constraint(halting_problem_undecidability, godel_incompleteness).
narrative_ontology:affects_constraint(halting_problem_undecidability, posts_problem_classification).

% DUAL FORMULATION NOTE:
% The halting problem is a constraint family root. Rice's theorem, Gödel's incompleteness, and Post's problem are all downstream consequences of the same fundamental diagonalization principle. Each constraint story has its own ε and domain, but they share the mathematical foundation of undecidability. The halting problem story serves as the exemplar for how logical limits propagate through formal systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
