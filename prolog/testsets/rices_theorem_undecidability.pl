% ============================================================================
% CONSTRAINT STORY: rices_theorem_undecidability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
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
 *   constraint_id: rices_theorem_undecidability
 *   human_readable: Rice's Theorem (Undecidability of Semantic Properties)
 *   domain: mathematical/computational_theory
 *
 * SUMMARY:
 *   Rice's Theorem is a fundamental result in computability theory, proven by
 *   Henry Rice in 1951, stating that any non-trivial semantic property of a
 *   program (a property that depends on the behavior of the program, not
 *   merely its syntax) is undecidable. That is, no algorithm can correctly
 *   determine for all programs whether they possess a given semantic
 *   property. This constraint is a natural law of mathematics and computation
 *   — it emerges necessarily from the structure of formal computation and is
 *   invariant across all implementations, languages, and platforms. The
 *   theorem applies universally: there exists no general-purpose algorithm
 *   that can determine whether an arbitrary program halts, recognizes a
 *   regular language, is Turing-complete, uses only finite memory, or
 *   exhibits any other non-trivial semantic property. Every perspective —
 *   from program verifiers to the formal methods community to the software
 *   industry — encounters the same immutable boundary.
 *
 * KEY AGENTS:
 *   - Program Verifiers and Static Analysis Tools: Confronts undecidability when attempting to prove semantic properties of programs (powerless/trapped)
 *   - Software Industry (compiler vendors, security platforms, verification tool makers): Organized actors that have internalized Rice's Theorem and developed approximation strategies within its limits (organized/constrained)
 *   - Formal Methods Community: Institutional experts who work with Rice's Theorem as axiomatic foundation; design restricted problems that are decidable (institutional/arbitrage)
 *   - Analytical Observer: Views undecidability from mathematical logic perspective; sees the theorem as a logical necessity (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rices_theorem_undecidability, 0.12).
domain_priors:suppression_score(rices_theorem_undecidability, 0.02).
domain_priors:theater_ratio(rices_theorem_undecidability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rices_theorem_undecidability, extractiveness, 0.12).
narrative_ontology:constraint_metric(rices_theorem_undecidability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(rices_theorem_undecidability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rices_theorem_undecidability, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(rices_theorem_undecidability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rices_theorem_undecidability, mountain).
narrative_ontology:human_readable(rices_theorem_undecidability, "Rice's Theorem (Undecidability of Semantic Properties)").
narrative_ontology:topic_domain(rices_theorem_undecidability, "mathematical/computational_theory").

domain_priors:emerges_naturally(rices_theorem_undecidability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROGRAM VERIFIER (MOUNTAIN) — Any attempt to construct a universal decision procedure for semantic properties of programs confronts the same logical barrier, regardless of implementation language, computational substrate, or resource constraints. The undecidability is not a limitation of current technology but an immutable structural fact. No engineering innovation can overcome Rice's Theorem.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: SOFTWARE INDUSTRY (MOUNTAIN) — Across 60+ years of industrial development, every static analysis tool, type system, and automated verification framework operates within Rice's boundary. The constraint is not contingent on market structure or investment level — it is a universal law that forces the industry toward heuristic approximations, incomplete logics, and conservative over-approximations. The organized actors (compiler vendors, verification tool makers, security platforms) have accepted this as fundamental rather than as a market limitation to overcome.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FORMAL METHODS COMMUNITY (MOUNTAIN) — Mathematicians and theoretical computer scientists who work directly with Rice's Theorem see it as a logical law equivalent to Gödel's Incompleteness or the Halting Problem. The undecidability is not a barrier they are trying to circumvent — it is the foundation on which they build restricted decision problems (decidable subclasses, bounded domains, approximations). The constraint is universally binding.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of mathematical logic and computability theory, Rice's Theorem is a corollary of the Halting Problem's undecidability via a reduction argument. The undecidability of semantic properties is not an empirical constraint on implementations but a logical necessity: any algorithm that could decide a non-trivial semantic property could be transformed into a Halting Problem solver. The constraint is mathematical law.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rices_theorem_undecidability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(rices_theorem_undecidability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rices_theorem_undecidability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rices_theorem_undecidability, ExtMetricName, E),
    domain_priors:suppression_score(rices_theorem_undecidability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rices_theorem_undecidability),
    narrative_ontology:constraint_metric(rices_theorem_undecidability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rices_theorem_undecidability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rices_theorem_undecidability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Extremely low. Rice's Theorem imposes no extraction asymmetry — it binds all agents equally. No agent benefits from undecidability; all face the same structural constraint. The minimal non-zero value reflects only the abstract computational cost of attempting verification and the universal necessity of working around the limitation. Suppression (0.02): Negligible. There are no suppressed alternatives — every possible approach to universal semantic property verification hits the same undecidability barrier. Suppression would imply some alternative existed but was hidden; here, no alternative exists to suppress. Theater ratio (0.15): Very low. The formal statement and proof of Rice's Theorem are entirely explicit and unperformative. There is no ritual or theater in the constraint itself — only in the heuristic approximations that organizations build to work within its limits. Those approximations may be theatrical, but the constraint itself is pure mathematical law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for Rice's Theorem because the constraint classifies as Mountain from every structural position. All agents — powerless verifiers, organized tool builders, institutional logicians, analytical observers — encounter the same undecidable boundary. The theorem does not benefit one group at the expense of another; it applies universally and equally. This invariance across all (P,T,E,S) tuples is the defining characteristic of natural law constraints. The apparent gap between what industry attempts (comprehensive static analysis) and what mathematics proves (impossibility) is not a gap in the constraint's classification but a gap in expectations — the industry has gradually accepted the limit and built approximation frameworks that work within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there is no beneficiary-victim relationship. Rice's Theorem is a logical limit that applies uniformly. The engine's automatic directionality derivation from beneficiary/victim declarations does not apply — the constraint has no structural asymmetry in who bears costs. The suppression value (0.02) and extractiveness value (0.12) reflect only the abstract computational inevitability, not any extraction flow. All agents experience the constraint as a boundary, not as a distribution of costs and benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: Rice's Theorem demonstrates the canonical mountain type with no mandatrophy present. The constraint does not risk mislabeling as extraction (Snare) or false coordination (Rope) because the base properties decisively exclude those types. Extractiveness ≤ 0.12 immediately disqualifies Snare (ε ≥ 0.46). Suppression ≤ 0.02 is far below the thresholds required for any hybrid or extractive type. The accessibility_collapse (0.91) and resistance (0.08) confirm that the constraint emerges from mathematical necessity, not institutional design. All six perspectives classify as Mountain, which is the expected behavior for a true natural law: observers cannot disagree on whether an immutable boundary exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trivial_property_boundary,
    'What constitutes the precise boundary between trivial and non-trivial semantic properties?',
    'Formal definition of ''non-trivial'' in Rice''s Theorem (holds for some programs, fails for others); examination of edge cases like undecidable but recognizable properties',
    'If boundary is fuzzy: some heuristic analysis tools might operate in a gray zone where Rice''s Theorem''s assumptions weaken. If boundary is sharp: all practical semantic verification belongs to the undecidable region.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trivial_property_boundary, conceptual, 'Precise characterization of trivial vs non-trivial semantic properties').

omega_variable(
    approximation_completeness,
    'Can practical static analysis frameworks approximate all decidable semantic restrictions tightly enough to be useful for real programs?',
    'Empirical analysis of false positive/negative rates across industrial type systems and static analysis tools; measurement of gap between decidable syntactic approximations and target semantic property',
    'If yes: the constraint''s practical force is lower than pure undecidability suggests — approximations suffice. If no: undecidability remains a binding barrier even for restricted domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(approximation_completeness, empirical, 'Whether approximations of undecidable properties are sufficiently precise for practical use').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rices_theorem_undecidability, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rice_tr_t0, rices_theorem_undecidability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rice_tr_t30, rices_theorem_undecidability, theater_ratio, 30, 0.12).
narrative_ontology:measurement(rice_tr_t60, rices_theorem_undecidability, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(rice_be_t0, rices_theorem_undecidability, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(rice_be_t30, rices_theorem_undecidability, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(rice_be_t60, rices_theorem_undecidability, base_extractiveness, 60, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(rices_theorem_undecidability, halting_problem_undecidability).
narrative_ontology:affects_constraint(rices_theorem_undecidability, formal_verification_completeness_gap).
narrative_ontology:affects_constraint(rices_theorem_undecidability, static_type_system_soundness_incompleteness).

% DUAL FORMULATION NOTE:
% Rice's Theorem is the parent constraint in a family of undecidability results in computability theory. It establishes the universal impossibility of semantic property decision; downstream constraints (Halting Problem, specific formal verification systems, type system soundness) are special cases or applications of Rice's principle. The network links show that multiple constraints in formal methods and program verification derive their undecidability from Rice's foundational barrier.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
