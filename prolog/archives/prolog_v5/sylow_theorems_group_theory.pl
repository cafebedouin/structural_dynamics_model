% ============================================================================
% CONSTRAINT STORY: sylow_theorems_group_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sylow_theorems_group_theory, []).

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
 *   constraint_id: sylow_theorems_group_theory
 *   human_readable: Sylow Theorems (Finite Group Structure)
 *   domain: mathematical/group_theory
 *
 * SUMMARY:
 *   The Sylow Theorems (proven by Ludvig Sylow in 1872) represent a
 *   paradigmatic example of a Mountain constraint in the Deferential Realism
 *   framework. They guarantee that any finite group G of order n = p^a × m
 *   (where p is prime and gcd(p, m) = 1) contains a subgroup of order p^a (a
 *   Sylow p-subgroup), and further constrain the number of such subgroups to
 *   be congruent to 1 modulo p and divide m. These theorems are logical
 *   consequences of group axioms — they emerge naturally from the definition
 *   of a finite group without requiring additional structure, enforcement, or
 *   suppression. They have never been violated, cannot be violated, and are
 *   understood identically across all mathematical cultures and time periods.
 *   There is no asymmetric extraction, no suppression of alternatives, no
 *   theater of verification. The theorems are simply true because their
 *   negation would contradict the axioms of group theory. This makes them a
 *   pure mathematical natural law.
 *
 * KEY AGENTS:
 *   - Finite Groups: The structural object constrained by the theorems — all finite groups must satisfy Sylow properties regardless of any external agent
 *   - Mathematical Community: Institutional observer and user of the theorems — constrained to accept them but also liberated by their certainty
 *   - Logical Framework: The axiomatic system (group axioms) from which the theorems derive as immutable consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sylow_theorems_group_theory, 0.08).
domain_priors:suppression_score(sylow_theorems_group_theory, 0.02).
domain_priors:theater_ratio(sylow_theorems_group_theory, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sylow_theorems_group_theory, extractiveness, 0.08).
narrative_ontology:constraint_metric(sylow_theorems_group_theory, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sylow_theorems_group_theory, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sylow_theorems_group_theory, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(sylow_theorems_group_theory, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sylow_theorems_group_theory, mountain).
narrative_ontology:human_readable(sylow_theorems_group_theory, "Sylow Theorems (Finite Group Structure)").
narrative_ontology:topic_domain(sylow_theorems_group_theory, "mathematical/group_theory").

domain_priors:emerges_naturally(sylow_theorems_group_theory).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT ENCOUNTERING SYLOW (MOUNTAIN) — From the learner's position, Sylow theorems appear as an irreducible logical/mathematical limit. The theorems constrain what finite groups can structurally contain — this constraint cannot be negotiated, circumvented, or extracted from. It is simply true for all finite groups everywhere, always. The student's access to the theorem is complete (no suppression) and the logical necessity is absolute (accessibility collapse ≥ 0.92).
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PURE MATHEMATICIAN / FORMAL DERIVATION (MOUNTAIN) — From the perspective of mathematical proof and logical necessity, Sylow theorems are provably invariant. The proof (via counting argument, fixed-point lemma, or conjugacy class enumeration) is completely transparent. No extraction, no suppression, no theater — only logical derivation from group axioms. The resistance to falsification is zero (no counterexample can exist); the accessibility is complete (the proof is published, verifiable, and reproducible by any competent group theorist).
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: RESEARCH COMMUNITY / INSTITUTIONAL TRUTH (MOUNTAIN) — From the perspective of mathematical institutions (universities, journals, conferences), Sylow theorems are a settled truth that structures graduate education and research scaffolding. No institution can exit or negotiate this constraint — it is the foundation of group-theoretic language. The theorem is reproduced identically across all mathematical communities worldwide. Resistance is zero (no mathematician contests the validity); suppression is zero (the proof is freely accessible).
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: FORMAL LOGIC FRAMEWORK (MOUNTAIN) — From the axiomatic perspective (group axioms → Sylow conclusions), the theorems are a direct logical consequence. The extraction value is literally zero — the theorem does not extract resources or impose asymmetric costs. It is a pure structural constraint: finite groups with order n = p^a × m (gcd(p, m)=1) must contain subgroups of order p^i for all 0 ≤ i ≤ a. This is an unchangeable feature of group structure itself, not a social or institutional arrangement that could be reformed.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sylow_theorems_group_theory_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sylow_theorems_group_theory, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sylow_theorems_group_theory, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sylow_theorems_group_theory, ExtMetricName, E),
    domain_priors:suppression_score(sylow_theorems_group_theory, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sylow_theorems_group_theory),
    narrative_ontology:constraint_metric(sylow_theorems_group_theory, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sylow_theorems_group_theory, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sylow_theorems_group_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Essentially zero. The Sylow Theorems do not extract resources, authority, or asymmetric benefit from any agent. They impose a logical constraint on finite group structure, but this is not extraction — it is mathematical truth. Suppression (0.02): Essentially zero. The proof is completely transparent, published, and reproducible. Access to the theorem and its proof is unrestricted. There are no alternatives to Sylow p-subgroups — they logically must exist. Resistance (0.08): Near zero. No mathematician contests the validity of Sylow theorems. No empirical counterexample can exist because the theorems are logical tautologies relative to group axioms. Theater ratio (0.15): Very low. The verification of Sylow theorems involves standard mathematical proof (induction, counting arguments, fixed-point lemmas). There is minimal performative content — the proof works or it doesn't, and it does. Some pedagogical theater exists when teaching the theorems (careful presentation, worked examples), but the core logical structure is not performative. Accessibility collapse (0.92): Very high. Once a student understands group axioms, the logical necessity of Sylow p-subgroups becomes apparent and unchangeable. The constraint is universally accessible (no hidden mechanism, no privileged information). The logical structure is completely transparent.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All four perspectives — student, pure mathematician, research community, formal logic — converge on the identical classification and reasoning. This is the defining feature of a Mountain constraint. The student sees the same necessity as the research community sees it as the logician sees it. The constraint is invariant across all observables, all measurement methodologies, and all contexts. This uniformity is the signature of mathematical natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values do not apply to Mountain constraints in the standard sense. All agents experience d → 0.5 (symmetric) or do not experience any extraction at all. Sylow theorems constrain all finite groups equally — there is no asymmetry, no targeted extraction, no beneficiary or victim relationship. The theorems are equally true for abelian groups, non-abelian groups, simple groups, and solvable groups. They are equally accessible to researchers in Beijing, São Paulo, and Stockholm. There is no extraction direction because there is no extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint definitively resolves mandatrophy by being the purest possible case of a Mountain. There is no confusion with Snare (no suppression of alternatives), no hybridization with Rope (no coordination function), and no degradation into Piton (full transparency, zero theater, zero inertia). The Sylow Theorems are mathematical law — they structure all finite group theory without cost, without suppression, and without negotiation. They demonstrate that true constraints are invariant, transparent, and universally binding, but only because they are logically necessary, not because they are enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sylow_theorems_group_theory, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sylow_theorems_group_theory, lagrange_theorem_finite_groups).
narrative_ontology:affects_constraint(sylow_theorems_group_theory, orbit_stabilizer_conjugacy_classes).

% DUAL FORMULATION NOTE:
% Sylow theorems are upstream in the finite group theory constraint family. Lagrange's theorem (any subgroup order divides the group order) is a necessary precondition; Sylow theorems strengthen this to guarantee the existence of subgroups of prime power order. Orbit-stabilizer theorem and conjugacy class enumeration are downstream applications that rely on Sylow structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
