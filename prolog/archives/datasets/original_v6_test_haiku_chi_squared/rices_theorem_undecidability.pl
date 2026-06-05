% ============================================================================
% CONSTRAINT STORY: rices_theorem_undecidability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: rices_theorem_undecidability
 *   human_readable: Rice's Theorem (Undecidability of Semantic Properties)
 *   domain: technological/mathematical/computability_theory
 *
 * SUMMARY:
 *   Rice's Theorem is a fundamental result in computability theory
 *   established by Henry Gordon Rice in 1953. The theorem states that any
 *   non-trivial semantic property of a Turing-recognizable language is
 *   undecidable. This means no universal algorithm can determine whether an
 *   arbitrary program has any non-syntactic property—termination,
 *   correctness, memory safety, equivalence to another program, or any other
 *   property dependent on the program's behavior. The theorem emerges as a
 *   mathematical law, not from institutional convention or design choice. It
 *   applies with equal force to the most resourced software verification
 *   teams and the smallest embedded systems programmer. This constraint is
 *   foundational to computational theory and shapes the entire landscape of
 *   formal methods, testing strategies, and verification tool design.
 *
 * KEY AGENTS:
 *   - Analytical Mathematician: Observer of the mathematical structure (analytical/analytical) — recognizes the theorem as provable limit on computation
 *   - Software Verification Industry: Resourced institutional actor (powerful/mobile) — develops tools and heuristics that work within Rice's constraints
 *   - Embedded Systems Programmer: Individual bearing the constraint (powerless/trapped) — cannot verify safety properties despite best efforts
 *   - Computer Science Discipline: Institutional knowledge structure (institutional/arbitrage) — uses Rice's Theorem to structure research and define problem scope
 *   - Safety-Critical System Community: Organized actors with constrained options (organized/constrained) — develops restricted formal languages and bounded verification methods
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rices_theorem_undecidability, 0.12).
domain_priors:suppression_score(rices_theorem_undecidability, 0.03).
domain_priors:theater_ratio(rices_theorem_undecidability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rices_theorem_undecidability, extractiveness, 0.12).
narrative_ontology:constraint_metric(rices_theorem_undecidability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(rices_theorem_undecidability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rices_theorem_undecidability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rices_theorem_undecidability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rices_theorem_undecidability, mountain).
narrative_ontology:human_readable(rices_theorem_undecidability, "Rice's Theorem (Undecidability of Semantic Properties)").
narrative_ontology:topic_domain(rices_theorem_undecidability, "technological/mathematical/computability_theory").

domain_priors:emerges_naturally(rices_theorem_undecidability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL MATHEMATICIAN (MOUNTAIN) — Rice's Theorem is a provable mathematical limit on what can be computed. For any non-trivial semantic property of programs, no universal decision procedure exists. This is not a policy or institutional choice but a fundamental constraint on computation itself. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14. Classification invariant across all observables and measurement methodologies.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: SOFTWARE VERIFICATION INDUSTRY (MOUNTAIN) — Even the most resourced verification teams (theorem provers, static analysis vendors, model checkers) cannot escape Rice's Theorem. They operate in the decidable fragment or with heuristics, accepting incompleteness. The theorem is not a market barrier but a mathematical ceiling. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.10. The constraint permits sophisticated verification tools but guarantees gaps remain.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: EMBEDDED SYSTEMS PROGRAMMER (MOUNTAIN) — The programmer cannot verify whether their code will terminate, use bounded memory, avoid buffer overflows, or achieve timing constraints. These are semantic properties undecidable by Rice's Theorem. No tool, no budget, no skill overcomes this. d≈0.88, f(d)≈1.30, σ=1.0 → χ≈0.16. The constraint is universal and unavoidable, but not extractive (no agent extracts value from the programmer's inability).
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: COMPUTER SCIENCE DISCIPLINE (MOUNTAIN) — Rice's Theorem is a foundational axiom of the field. It shapes research directions (formal methods focus on the decidable fragment), establishes the scope of what can be proven, and structures the entire hierarchy of computational complexity classes. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.01. The discipline benefits from knowing its limits; the theorem is generative of coherent inquiry rather than extractive.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFETY-CRITICAL SYSTEM COMMUNITY (MOUNTAIN) — Aerospace, nuclear, medical device engineers must work within Rice's Theorem's constraints. They develop restricted formal languages (decidable subsets), bounded model checkers, and domain-specific verification strategies. They cannot exit the theorem, but they have constrained mobility within the decidable fragment. d≈0.60, f(d)≈0.90, σ=1.0 → χ≈0.11. The constraint is binding but not extractive; it is part of the epistemic structure of their domain.
constraint_indexing:constraint_classification(rices_theorem_undecidability, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
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
 *   Extractiveness (0.12): Minimal. Rice's Theorem is not extractive in the sense that no agent gains advantage from others' inability to verify. The undecidability of semantic properties is a universal constraint, not an asymmetric extraction mechanism. Suppression (0.03): Negligible. The theorem does not require suppression of alternatives—it rules out the existence of an alternative (a universal decider for semantic properties). There is nothing to suppress because the desired tool cannot logically exist. Theater ratio (0.15): Very low. The theorem is purely functional in its application; there is minimal performative activity around invoking Rice's result. When verification tools claim to 'solve the undecidable problem,' that is not theater—it is either honest restriction to a decidable fragment or honest incompleteness. The small non-zero theater reflects that some tools may present heuristic approximations as more complete than they are, but the dominant practice is candid about Rice's limits. Claimed type: Mountain. All metrics satisfy the mountain gates: ε ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally = true.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits perspectival invariance, not perspectival gap. All five perspectives—mathematician, industry, programmer, discipline, safety-critical community—arrive at Mountain classification. The structural properties (ε=0.12, suppression=0.03, theater=0.15) are invariant across measurement methodologies. A programmer verifying code termination, an engineer building aerospace systems, a logician proving theorems, and a tool vendor building static analyzers all encounter the same mathematical constraint. The absence of a perspectival gap is a feature of natural law constraints: they classify identically from all observables because the undecidability is not relative to an observer's position. This is the key distinction from Tangled Rope constraints (which exhibit perspectival gaps based on beneficiary/victim status) or Snares (which depend on the target's exit options). Rice's Theorem applies universally.
 *
 * DIRECTIONALITY LOGIC:
 *   Rice's Theorem produces uniform low directionality across all agent positions because it is non-extractive. There is no beneficiary (no agent benefits from semantic undecidability) and no victim group (all agents are equally constrained by the same mathematical limit). Directionality is derived from the theorem's structure: Analytical observer d≈0.72 (observing the constraint without inhabiting it), Powerful actors d≈0.50 (resources cannot overcome the limit), Powerless actors d≈0.88 (equally constrained as everyone else), Institutional discipline d≈0.05 (the discipline benefits from knowing its boundaries), Safety-critical community d≈0.60 (constrained but with agency in working within the constraint). None of these variations produce high effective extraction χ because the theorem itself has zero asymmetry—it constrains everyone equally. The formula χ = ε × f(d) × σ(S) = 0.12 × f(d) × 1.0 ≈ 0.12 regardless of f(d), because ε is so low that scope and directionality contribute minimally. This is the signature of a mountain: even with unfavorable d values, χ remains too small to indicate extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_property_boundary,
    'Where precisely is the boundary between syntactic properties (decidable) and semantic properties (undecidable)? Is the boundary sharp or gradual?',
    'Formal classification of specific properties (termination, equivalence, type safety) along the syntax-semantics continuum. Historical analysis of properties initially thought decidable that proved undecidable.',
    'If boundary is sharp: Rice''s Theorem applies with absolute clarity. If gradual: some properties may have partial decidability or approximable verification. The classification of verification tools as Mountain vs Tangled Rope hinges on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(semantic_property_boundary, conceptual, 'Precise boundary between decidable and undecidable properties').

omega_variable(
    pragmatic_heuristic_scope,
    'How much of real-world program verification can be covered by decidable approximations and heuristic methods without violating Rice''s Theorem constraints?',
    'Empirical analysis of static analysis tool coverage on large codebases; measurement of false positive/negative rates on semantic properties; comparison of heuristic coverage across domains (web, systems, embedded).',
    'If coverage is > 90%: Rice''s Theorem is theoretically binding but practically manageable, and verification tools approach Rope (coordination) rather than Snare (extraction). If coverage is < 50%: the practical gap between theorem and practice remains severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatic_heuristic_scope, empirical, 'Extent of practical program verification coverage within Rice''s limits').

omega_variable(
    computational_universality_dependency,
    'Does Rice''s Theorem strictly require Turing-complete languages, or does it apply to subsets? Could a restricted computational model (e.g., bounded loops, typed languages) escape the theorem?',
    'Formal analysis of Rice''s proof and its dependence on Turing completeness. Classification of restricted languages (Datalog, Cobol, domain-specific languages) against Rice''s scope.',
    'If theorem requires full Turing completeness: languages can be designed to be decidable. If theorem applies more broadly: even restricted languages inherit undecidability. This determines whether language design is a real escape route or an illusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_universality_dependency, conceptual, 'Dependency of Rice''s Theorem on computational universality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rices_theorem_undecidability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rice_tr_t0, rices_theorem_undecidability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rice_tr_t50, rices_theorem_undecidability, theater_ratio, 50, 0.15).
narrative_ontology:measurement(rice_tr_t100, rices_theorem_undecidability, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(rice_be_t0, rices_theorem_undecidability, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(rice_be_t50, rices_theorem_undecidability, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(rice_be_t100, rices_theorem_undecidability, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rices_theorem_undecidability, information_standard).
narrative_ontology:affects_constraint(rices_theorem_undecidability, halting_problem_decidability).
narrative_ontology:affects_constraint(rices_theorem_undecidability, godel_incompleteness_theorem).
narrative_ontology:affects_constraint(rices_theorem_undecidability, turing_completeness_equivalence).

% DUAL FORMULATION NOTE:
% Rice's Theorem is a specialization of the Halting Problem's undecidability to the domain of semantic properties. The Halting Problem (can we decide if a program terminates?) is a specific instance of Rice's Theorem applied to the termination property. Gödel's Incompleteness Theorem is a parallel undecidability result in formal logic with similar structural implications for mathematical knowledge. Turing Completeness Equivalence is a prerequisite fact: Rice's Theorem depends on the language being Turing-complete or universal. These three constraints form a foundational cluster in computability theory, linked by shared undecidability structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
