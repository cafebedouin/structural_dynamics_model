% ============================================================================
% CONSTRAINT STORY: van_der_waerden
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_van_der_waerden, []).

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
 *   constraint_id: van_der_waerden
 *   human_readable: Van der Waerden's Theorem
 *   domain: mathematical/combinatorics/ramsey_theory
 *
 * SUMMARY:
 *   Van der Waerden's theorem, proved by Bartel Leendert van der Waerden in
 *   1927, states that for any finite number of colors and any positive
 *   integer k, there exists a number N such that if the integers from 1 to N
 *   are colored using those colors, there must exist a monochromatic
 *   arithmetic progression of length k. This is a foundational result in
 *   Ramsey theory and has no degree of freedom in its conclusion. The
 *   constraint is not imposed by any agent or institution — it emerges
 *   necessarily from the structure of the integers under finite coloring.
 *   There is no escape, no negotiation, no alternative arrangement that
 *   avoids the conclusion. The theorem is equally immutable regardless of who
 *   observes it or what they might wish were true.
 *
 * KEY AGENTS:
 *   - The Integers: The underlying structure subject to coloring; cannot negotiate
 *   - Finite Colors: The constraint parameter; increasing colors cannot escape the theorem's conclusion
 *   - Monochromatic Progressions: The necessary consequence; emerges with certainty
 *   - Combinatorialists: Observer-participants; accept the theorem as immutable
 *   - Logicians and Proof Theorists: Analytical observers; establish the theorem's formal necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(van_der_waerden, 0.08).
domain_priors:suppression_score(van_der_waerden, 0.02).
domain_priors:theater_ratio(van_der_waerden, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(van_der_waerden, extractiveness, 0.08).
narrative_ontology:constraint_metric(van_der_waerden, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(van_der_waerden, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(van_der_waerden, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(van_der_waerden, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(van_der_waerden, mountain).
narrative_ontology:human_readable(van_der_waerden, "Van der Waerden's Theorem").
narrative_ontology:topic_domain(van_der_waerden, "mathematical/combinatorics/ramsey_theory").

domain_priors:emerges_naturally(van_der_waerden).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTEGERS UNDER COLORING (MOUNTAIN) — Any coloring of the positive integers with finitely many colors must contain a monochromatic arithmetic progression of arbitrary length. This is an intrinsic structural property of the integers under coloring; there is no escape, no alternative formulation, no degree of freedom. The integers cannot negotiate their way out of this constraint.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE COMBINATORIALIST (MOUNTAIN) — A mathematician working in Ramsey theory or additive combinatorics observes Van der Waerden's theorem as an unavoidable structural fact about the integers. The proof is non-constructive; the Ackermann function bounds on monochromatic progression length are astronomically large. But the theorem's truth is not contingent on proof method, computational tractability, or observational strategy. The combinatorialist experiences this as a mountain — an immutable boundary of what is possible.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE LOGICAL ANALYST (MOUNTAIN) — From the perspective of formal logic and proof theory, Van der Waerden's theorem is a statement about the structure of finite colorings and infinite sequences. Its proof (Ackermann 1937) is non-constructive, relying on the pigeonhole principle and the arithmetic structure of the integers. The theorem's truth is independent of any particular model or representation — it holds in ZFC and in any formal system with the axioms needed for basic arithmetic. The constraint is that coloring space + finite colors + infinite domain necessarily implies monochromatic progressions. This is a natural law of combinatorial logic.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE MATHEMATICAL INSTITUTION (MOUNTAIN) — Mathematics as a social institution accepts Van der Waerden's theorem as a proven fact. No organization can negotiate around it; no funding mechanism can change it; no alternative framework has yet succeeded in dodging its conclusion. The constraint holds across all mathematical schools and traditions that accept standard logic and arithmetic. The institutional perspective sees this as a fixed point in the landscape of mathematical knowledge.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(van_der_waerden_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(van_der_waerden, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(van_der_waerden, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(van_der_waerden, ExtMetricName, E),
    domain_priors:suppression_score(van_der_waerden, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(van_der_waerden),
    narrative_ontology:constraint_metric(van_der_waerden, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(van_der_waerden, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(van_der_waerden_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Van der Waerden's theorem does not extract from any agent in the sense of asymmetric power transfer. The theorem is a fact about integer structure, not a mechanism of coordination or coercion. No group benefits at another's expense. The small non-zero value reflects that the theorem limits what is possible — a very weak form of constraint — but this is not extractive, it is merely restrictive of impossible possibilities. Suppression (0.02): Negligible. There is no coercive mechanism, no silencing of alternatives, no prohibition. The theorem does not suppress any actor or alternative formulation. Alternatives were simply never possible. Theater ratio (0.05): Negligible. The proof is non-constructive (Ackermann 1937), but the statement is purely factual — no performative element, no narrative framing, no institutional ritual. The 'theater' in proving the theorem (pedagogical presentation, publication conventions) is minimal and orthogonal to the theorem's truth. Accessibility collapse (0.92): Very high. The theorem's conclusion is completely resistant to escape or circumvention. You cannot color the integers without creating monochromatic progressions. The accessibility of any alternative outcome collapses to zero. Resistance (0.03): Very low. The theorem is proven; resistance to its validity is confined to a tiny fraction of mathematical dissenters and is not a property of the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All perspectives classify this constraint as Mountain because the constraint is invariant across all structural positions, power levels, exit options, and time horizons. Whether viewed as a powerless agent (the integers), a moderate observer (the combinatorialist), an analytical observer (the logician), or an institutional actor (mathematics as a discipline), the conclusion is identical and immutable. This uniformity is the hallmark of a mountain constraint — the classification does not change with the observer's position.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality computation is relevant for a mountain constraint. There are no beneficiaries or victims. The constraint does not extract from anyone. The sigmoid directionality function f(d) and effective extractiveness χ are not applicable because the constraint has no asymmetric power structure. Every observer, regardless of power or exit options, experiences the same immutable fact: colorings imply monochromatic progressions. Directionality d is undefined in this context — the constraint is not a power relation.
 *
 * MANDATROPHY ANALYSIS:
 *   Van der Waerden's theorem presents no mandatrophy. The theorem is purely a mountain — a natural law of combinatorics. There is no risk of mislabeling it as coordination (rope) or extraction (snare) because all perspectives yield the same classification and there is no asymmetric benefit structure. The constraint is not contingent on measurement basis, observable choice, or institutional arrangement. The only 'omega' would be whether one accepts the axioms of arithmetic and logic, but that is philosophical, not structural to the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(van_der_waerden, 1927, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(van_der_waerden, ramsey_theorem_foundation).
narrative_ontology:affects_constraint(van_der_waerden, ackermann_function_bounds).
narrative_ontology:affects_constraint(van_der_waerden, pigeonhole_principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
