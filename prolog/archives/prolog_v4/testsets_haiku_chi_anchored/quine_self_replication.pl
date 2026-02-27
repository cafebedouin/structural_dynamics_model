% ============================================================================
% CONSTRAINT STORY: quine_self_replication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quine_self_replication, []).

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
 *   constraint_id: quine_self_replication
 *   human_readable: Quines (Computational Self-Replication)
 *   domain: theoretical_computer_science/mathematics
 *
 * SUMMARY:
 *   Quines represent a fundamental constraint on computation: any program in
 *   a Turing-complete language can, in principle, produce a copy of its own
 *   source code as output. This is not a limitation or design choice — it is
 *   a logical necessity that follows from the expressive power of universal
 *   computation. The constraint emerges naturally from Gödel's fixed-point
 *   theorem and Kleene's recursion theorem. It has no beneficiaries or
 *   victims in the extractive sense; rather, it is a structural feature of
 *   computation itself. All observers — theorists, language designers,
 *   programmers — encounter the same immutable wall: self-reference cannot be
 *   escaped. The constraint is invariant across all Turing-complete
 *   languages, all computational substrates, and all attempts at restriction.
 *   Quines serve as a pedagogical tool for understanding this constraint, but
 *   the tool does not create the constraint — it merely makes visible what
 *   Turing completeness entails.
 *
 * KEY AGENTS:
 *   - Computational Theorists (analytical/arbitrage): Recognize quines as a consequence of fundamental theory
 *   - Programming Language Designers (powerful/arbitrage): Cannot eliminate quines without breaking Turing completeness
 *   - Programmers (powerless/trapped): Encounter the constraint as an inescapable logical fact
 *   - Educational Community (organized/mobile): Use quines as a coordination mechanism for teaching computation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quine_self_replication, 0.08).
domain_priors:suppression_score(quine_self_replication, 0.02).
domain_priors:theater_ratio(quine_self_replication, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quine_self_replication, extractiveness, 0.08).
narrative_ontology:constraint_metric(quine_self_replication, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(quine_self_replication, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quine_self_replication, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(quine_self_replication, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quine_self_replication, mountain).
narrative_ontology:human_readable(quine_self_replication, "Quines (Computational Self-Replication)").
narrative_ontology:topic_domain(quine_self_replication, "theoretical_computer_science/mathematics").

domain_priors:emerges_naturally(quine_self_replication).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL THEORIST (MOUNTAIN) — Quines are a fundamental consequence of Turing completeness and fixed-point theory. Any Turing-complete language admits quines by the Kleene fixed-point theorem and Gödel's diagonalization. The constraint is universal and invariant across all computational substrates. d=0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROGRAMMING LANGUAGE DESIGNER (MOUNTAIN) — No language design choice can eliminate quines. Turing completeness entails quines mathematically. Attempting to restrict the language to prevent quines reduces it below Turing completeness. The constraint is an immutable consequence of computational universality. d=0.48, f(d)≈0.60, σ=1.2 → χ≈0.06.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PROGRAMMER (MOUNTAIN) — A developer attempting to write code that cannot produce itself will discover this is impossible in any Turing-complete language. The constraint is mathematically unavoidable. There is no workaround, no escape. d=0.95, f(d)≈1.42, σ=0.8 → χ≈0.11.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: EDUCATIONAL COMMUNITY (ROPE) — Quines serve as a coordination mechanism for teaching fundamental concepts: Turing completeness, fixed-point theory, recursion, and self-reference. Quines enable collective understanding of how computation models itself. The constraint solves a genuine pedagogical problem with no coercive overhead. d=0.50, f(d)≈0.65, σ=1.2 → χ≈0.05.
constraint_indexing:constraint_classification(quine_self_replication, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quine_self_replication_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quine_self_replication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quine_self_replication, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(quine_self_replication, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quine_self_replication, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quine_self_replication, ExtMetricName, E),
    domain_priors:suppression_score(quine_self_replication, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quine_self_replication),
    narrative_ontology:constraint_metric(quine_self_replication, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quine_self_replication, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quine_self_replication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Extremely low. Quines do not extract value from any agent. The constraint is purely structural, not redistributive. The slight non-zero value (0.08 rather than 0.00) reflects the minimal informational cost of understanding the constraint — one must learn fixed-point theory. Suppression (0.02): Minimal. There are no coercive mechanisms involved. The constraint is self-enforcing through mathematical necessity. Theater ratio (0.15): Low. Quines are almost entirely functional — the constraint is what it is, with minimal performative overhead. The small theater reflects only the pedagogical framing required to teach the concept, not any systematic misrepresentation. These metrics satisfy the mountain classification gate: ε ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally = true.
 *
 * PERSPECTIVAL GAP:
 *   Remarkably, there is no perspectival gap. All observers — whether computationally powerful (language designers), analytically sophisticated (theorists), or constrained (programmers) — encounter the identical constraint. A programmer writing in Python or C, a theorist analyzing Turing machines, and a designer creating a new language all confront the same immutable fact: Turing completeness entails quines. The constraint is truly invariant. This invariance is the hallmark of a mountain-type constraint: the observer's position, power, and exit options do not change the classification. The only variation is rhetorical — theorists call it the Kleene fixed-point theorem, designers accept it as a consequence of universality, programmers experience it as an inescapable logical truth — but the underlying constraint is identical.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims. The constraint is not extractive. All agents face the same immutable wall, and no agent extracts benefit from others bearing the constraint. The constraint is a shared, universal fact of computation, not a redistributive mechanism. Directionality is uniform: d≈0.72 for all analytical observers (the universal position from which the constraint is viewed). The formula χ = ε × f(d) × σ(S) yields χ ≈ 0.08-0.09 for all observers, confirming the uniformity of the classification.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution required. This constraint is not subject to mandatrophy because it contains no extractive asymmetry to mask. Quines are not camouflaged coordination; they are not snares pretending to be ropes. The constraint is transparent: all observers see exactly what it is, a logical necessity of universal computation. The very low theater_ratio (0.15) and extractiveness (0.08) ensure that no reframing, institutional maintenance, or performative activity is required to sustain the constraint. It sustains itself through mathematical inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_computation_generalization,
    'Do quantum computers, which are Turing-universal via classical simulation, also necessarily admit quantum quines, or is the self-reference constraint different in the quantum domain?',
    'Proof of whether quantum Turing machines admit analogous fixed-point constructions; investigation of whether quantum measurement breaks self-reference symmetry',
    'If quantum quines exist identically: the constraint is a property of Turing universality alone, not of classical computation. If quantum quines require modification: the constraint is substrate-dependent at the quantum level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_computation_generalization, conceptual, 'Whether the quine constraint generalizes to quantum computation').

omega_variable(
    incompleteness_vs_quines_relationship,
    'Is the existence of quines fundamentally the same phenomenon as Gödel''s incompleteness, or are they structurally distinct fixed-point applications?',
    'Formal analysis of the proof structures; comparison of the diagonalization methods; examination of whether incompleteness implies quines or vice versa',
    'If identical: quines are a computational shadow of logical incompleteness, making this a single unified constraint across logic and computation. If distinct: they are separate fixed-point phenomena with different implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incompleteness_vs_quines_relationship, conceptual, 'Relationship between quines and Gödel incompleteness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quine_self_replication, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quine_tr_t0, quine_self_replication, theater_ratio, 0, 0.12).
narrative_ontology:measurement(quine_tr_t25, quine_self_replication, theater_ratio, 25, 0.14).
narrative_ontology:measurement(quine_tr_t50, quine_self_replication, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(quine_be_t0, quine_self_replication, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(quine_be_t25, quine_self_replication, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(quine_be_t50, quine_self_replication, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quine_self_replication, information_standard).
narrative_ontology:affects_constraint(quine_self_replication, godels_incompleteness_theorem).
narrative_ontology:affects_constraint(quine_self_replication, halting_problem_undecidability).

% DUAL FORMULATION NOTE:
% Quines are deeply related to Gödel's incompleteness theorem and the Halting problem through fixed-point and diagonalization arguments. However, quines are a distinct constraint: they concern self-representation in computation, while incompleteness concerns the limits of formal proof and the Halting problem concerns algorithmic decidability. Each constraint should be analyzed separately because they involve different fundamental concepts, even though they share common mathematical machinery (fixed-point theory). The network edges indicate that understanding one constraint informs understanding the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
