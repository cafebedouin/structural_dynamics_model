% ============================================================================
% CONSTRAINT STORY: solvable_group_criterion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_solvable_group_criterion, []).

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
 *   constraint_id: solvable_group_criterion
 *   human_readable: Solvable Group Criterion
 *   domain: abstract_algebra/group_theory
 *
 * SUMMARY:
 *   The solvable group criterion is a fundamental structural property in
 *   finite group theory: a finite group G is solvable if and only if it
 *   admits a composition series whose factor groups are all cyclic of prime
 *   order (equivalently, all abelian). This constraint is invariant across
 *   all mathematical contexts where finite groups appear. The solvability
 *   criterion classifies as a mountain because it is a logically necessary
 *   property of group structure—not enforced by external coercion, not
 *   subject to negotiation, not dependent on measurement basis or observer
 *   perspective. Every finite group either satisfies or violates this
 *   criterion as a matter of logical necessity. The criterion enables crucial
 *   results in Galois theory (polynomial solvability by radicals), Burnside's
 *   theorem on groups of order p^a q^b, and Feit-Thompson classification of
 *   simple groups of odd order. The constraint is uniquely characterized by
 *   zero degrees of freedom: there is no way to 'exit' solvability or
 *   negotiate its requirements.
 *
 * KEY AGENTS:
 *   - Finite Group Theorist: Agent constrained by structural necessity (analytical/trapped) — cannot circumvent the criterion
 *   - Galois Theorist: Secondary agent (analytical/trapped) — solvability of Galois groups determines polynomial solvability by radicals
 *   - Algebraic Research Community: Institutional observer (organized/analytical) — uses solvability to structure research, but cannot escape its constraint
 *   - Mathematical Formalism: Abstract agent (analytical/analytical) — the criterion is part of the logical structure of group theory itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(solvable_group_criterion, 0.12).
domain_priors:suppression_score(solvable_group_criterion, 0.03).
domain_priors:theater_ratio(solvable_group_criterion, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solvable_group_criterion, extractiveness, 0.12).
narrative_ontology:constraint_metric(solvable_group_criterion, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(solvable_group_criterion, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(solvable_group_criterion, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(solvable_group_criterion, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(solvable_group_criterion, mountain).
narrative_ontology:human_readable(solvable_group_criterion, "Solvable Group Criterion").
narrative_ontology:topic_domain(solvable_group_criterion, "abstract_algebra/group_theory").

domain_priors:emerges_naturally(solvable_group_criterion).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FINITE GROUP THEORIST (MOUNTAIN) — Cannot circumvent the solvability criterion. Every group either admits a composition series of abelian factors or does not; this is intrinsic to the group's structure. No exit from this constraint.
constraint_indexing:constraint_classification(solvable_group_criterion, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — Solvability is a fundamental structural property of finite groups. The criterion derives from the definition of composition series and the Galois correspondence. No alternative measurement or framing changes this classification.
constraint_indexing:constraint_classification(solvable_group_criterion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ALGEBRAIC RESEARCH COMMUNITY (MOUNTAIN) — Across generations of research, the solvability criterion has remained invariant. The community cannot organize its way out of this structural fact. It defines research direction but does not constrain freedom of inquiry.
constraint_indexing:constraint_classification(solvable_group_criterion, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(solvable_group_criterion_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(solvable_group_criterion, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(solvable_group_criterion, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(solvable_group_criterion, ExtMetricName, E),
    domain_priors:suppression_score(solvable_group_criterion, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(solvable_group_criterion),
    narrative_ontology:constraint_metric(solvable_group_criterion, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(solvable_group_criterion, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(solvable_group_criterion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The solvability criterion does not extract value from any agent; it is a neutral structural property. The low value reflects that natural laws have no beneficiary-victim dynamic—they simply constrain all agents equally. Suppression (0.03): Minimal. The criterion creates no coercion or barriers to exit because exit is logically impossible. There is nothing to suppress because resistance itself is incoherent. Theater ratio (0.15): Low. The solvability criterion is purely functional—it has no performative component. Verification of solvability for a given group is straightforward (check for composition series); the criterion creates no need for theater or ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap. Every agent—from the finite group theorist to the algebraic research community to the analytical observer—classifies the solvability criterion as mountain. This invariance across all perspectives (P, T, E, S) tuples is the defining signature of a natural law. The constraint makes the same demand on every observer. There is no beneficiary or victim because the criterion is neutral with respect to all agents' interests.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation applies to this constraint because the solvability criterion is universal and symmetric. There are no beneficiaries (agents who profit from the constraint) and no victims (agents who bear costs). The constraint's applicability is purely logical: it defines which groups are solvable, independent of any agent's position. The d parameter is undefined for this constraint because the constraint has no extraction dynamic. All agents are equally constrained by the same logical necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution is required for this constraint because it is a pure mountain with no ambiguity in its classification. The solvability criterion is neither disguised extraction masquerading as coordination nor coordination masquerading as extraction. It is a structural property of group theory itself. All six types collapse to mountain for this constraint, confirming the invariance principle: when all perspectives agree on a single classification and the base properties (ε ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85) are satisfied, the constraint is a natural law by definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_circularity,
    'Is solvability a natural mathematical property or a definitional artifact of how we construct composition series?',
    'Demonstrate that solvability is equivalent to other structurally independent characterizations (Sylow subgroup structure, nilpotent/solvable tower existence, amenability on lattice of subgroups). If equivalences are fundamental, solvability is natural; if stipulative, it is definitional.',
    'If natural: mountain classification confirmed. If definitional: the constraint is a mathematical construction, not a law of structure. Classification would degrade to rope (a coordination mechanism for how we organize group theory).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definitional_circularity, conceptual, 'Whether solvability is natural or definitional').

omega_variable(
    feit_thompson_scope,
    'Does the solvability criterion fully characterize decidability and computational complexity for finite group problems, or are there solvable groups with intractable decision problems?',
    'Survey computational complexity of standard problems (membership, word problem, conjugacy) for families of solvable groups. Identify if all solvable groups admit polynomial-time algorithms or if some solvable families have undecidable variants.',
    'If solvability guarantees tractability: mountain extends to computational domain. If some solvable groups are intractable: solvability is a partial characterization, not a total structural law. Classification would shift toward tangled_rope (mixed coordination and limitation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feit_thompson_scope, empirical, 'Whether solvability guarantees computational tractability').

omega_variable(
    category_theoretic_naturality,
    'Is solvability natural from a category-theoretic perspective, or does it depend on the specific embedding of finite groups in the category of groups?',
    'Examine whether solvability is preserved under category-theoretic constructions (limits, colimits, adjunctions). Test whether the solvability property has a universal characterization in terms of adjoint functors or natural transformations.',
    'If natural in categories: mountain confirmed at higher level of abstraction. If dependent on embedding: solvability is specific to the group-theoretic context, not universal. Classification becomes domain-relative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_theoretic_naturality, conceptual, 'Whether solvability is natural from category-theoretic perspective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(solvable_group_criterion, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(solv_tr_t0, solvable_group_criterion, theater_ratio, 0, 0.15).
narrative_ontology:measurement(solv_tr_t1, solvable_group_criterion, theater_ratio, 1, 0.15).

% Extraction over time
narrative_ontology:measurement(solv_be_t0, solvable_group_criterion, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(solv_be_t1, solvable_group_criterion, base_extractiveness, 1, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(solvable_group_criterion, information_standard).
narrative_ontology:affects_constraint(solvable_group_criterion, galois_polynomial_solvability).
narrative_ontology:affects_constraint(solvable_group_criterion, burnside_order_theorem).
narrative_ontology:affects_constraint(solvable_group_criterion, feit_thompson_odd_order).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
