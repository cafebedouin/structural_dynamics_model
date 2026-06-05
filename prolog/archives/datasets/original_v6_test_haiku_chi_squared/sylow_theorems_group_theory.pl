% ============================================================================
% CONSTRAINT STORY: sylow_theorems_group_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: sylow_theorems_group_theory
 *   human_readable: Sylow Theorems (Finite Group Structure)
 *   domain: mathematics/group_theory
 *
 * SUMMARY:
 *   The Sylow Theorems represent one of the canonical examples of a
 *   mathematical constraint that is both necessary (follows logically from
 *   group axioms) and invariant across all observables and measurement
 *   regimes. Formulated by Ludvig Sylow in 1872, the theorems guarantee that
 *   for any finite group G and any prime power p^k dividing |G|, there exists
 *   a subgroup of order p^k (called a Sylow p-subgroup). The theorems further
 *   constrain the number of such subgroups modulo their conjugacy classes.
 *   These results are foundational to finite group classification and the
 *   understanding of group structure. They are not coordinating mechanisms,
 *   not extraction systems, not temporary scaffolds, and not degraded
 *   institutions. They are invariant mathematical facts: the structure of any
 *   finite group is constrained by its prime factorization, and these
 *   constraints are immutable across all mathematical contexts, proof
 *   techniques, and pedagogical approaches. The constraint exhibits zero
 *   degrees of freedom in the DR sense: no agent can negotiate or alter the
 *   theorems; no institutional framework can relax them; no alternative
 *   mathematical foundation (that preserves group axioms) can circumvent
 *   them.
 *
 * KEY AGENTS:
 *   - Finite Groups: Structural subjects (powerless/analytical) — any finite group instantiates the Sylow constraint; the group's structure is determined by the theorems, not the other way around
 *   - Sylow p-subgroups: Logical objects (analytical/analytical) — their existence, conjugacy class structure, and properties are fully determined by the theorems
 *   - Mathematical Community: Institutional knowledge-holder (institutional/analytical) — preserves and transmits the theorems across time and educational contexts
 *   - Student Learners: Knowledge-acquirers (moderate/biographical) — encounter the theorems as immutable facts in their first exposure to group theory
 *   - Professional Mathematicians: Specialists (powerful/civilizational) — apply the theorems as structural tools in research on finite groups, Galois theory, algebraic cryptography
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
narrative_ontology:topic_domain(sylow_theorems_group_theory, "mathematics/group_theory").

domain_priors:emerges_naturally(sylow_theorems_group_theory).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT LEARNER (MOUNTAIN) — From the perspective of a finite group of any order n, the existence of Sylow p-subgroups (for each prime p dividing |G|) is an immutable structural fact. No agent, no institution, no measurement regime can alter this mathematical law. The constraint emerges naturally from the axioms of group theory and the structure of integers. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL COMMUNITY (MOUNTAIN) — The Sylow Theorems are logical necessities that follow from the Lagrange-Cauchy structure of group orders and prime factorization. No institutional framework, funding regime, or pedagogical choice can circumvent these theorems. They hold in all mathematical universes where group axioms and integer properties hold. ε=0.08 reflects that the constraint imposes zero degrees of freedom: Sylow p-subgroups must exist, their count is determined modulo conjugacy classes, and their normality structure is fixed by the group's order. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL ESTABLISHMENT (MOUNTAIN) — The Sylow Theorems are invariant across all pedagogical frameworks, proof techniques, and institutional contexts. No mathematical society, journal, or research program can change the theorems' content. Universities teach them identically (up to notation and proof order) across all nations and centuries. This is the defining characteristic of a mountain constraint in mathematics: accessibility_collapse=0.92 (the theorems are equally true and equally structure-determining regardless of how they are presented), resistance=0.08 (minimal resistance to the facts, though resistance to understanding them pedagogically is higher). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: PROFESSIONAL MATHEMATICIAN (MOUNTAIN) — Whether one specializes in finite groups, algebraic topology, number theory, or cryptography, the Sylow Theorems impose the same immutable structure: any finite group's subgroup lattice is constrained by prime power divisibility of the group's order. This constraint cannot be negotiated, evaded, or reinterpreted. It is not extractive — it is constitutive of what 'finite group' means. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(powerful),
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
 *   Extractiveness (0.08): Minimal. The Sylow Theorems do not extract value from any agent for the benefit of another. They are constitutive facts about group structure — they tell you what any finite group must look like, not how institutional arrangements govern finite groups. The non-zero value (0.08 rather than 0.00) reflects that understanding the theorems requires effort and study; the theorems constrain what knowledge-seekers can achieve without mathematical background. But this is not extraction in the DR sense — it is the structural cost of entering a domain of knowledge. Suppression (0.02): Negligible. The theorems cannot be suppressed or hidden; they follow inevitably from group axioms. The small non-zero value reflects minor friction in pedagogy — some students struggle with the Cauchy existence proof or the conjugacy class enumeration — but this is learning resistance, not systemic suppression. Theater ratio (0.15): Very low. The proof of the Sylow theorems is substantive and directly addresses the theorems' claims; there is no performative component. The 0.15 value captures only the minor ritual of stating the theorems before proving them — the formalities of mathematical presentation. The theorems are mathematically pure with zero institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap: all observers (student, community, establishment, professional) classify this as a Mountain from their position. The gap that does exist is between those who understand the theorems (classification: Mountain, invariant truth) and those who do not (pedagogical resistance, treating the theorems as difficult rather than necessary). But this gap is about knowledge access, not about structural divergence of interests. Once the theorems are understood, all perspectives converge: the theorems are mathematically inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is not applicable to mountain constraints in the standard sense. All perspectives derive d-values that reflect 'analytical observer' status rather than beneficiary/victim positioning. No agent benefits from the Sylow constraint at others' expense. No agent is victimized by the constraint. The constraint is impersonal: it applies equally to all finite groups and all mathematical agents. The d-values computed (0.48-0.72) are near the symmetric point (0.50) or elevated toward 'observer' (0.72), reflecting that the constraints is measured analytically from outside any extractive relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The Sylow Theorems resolve the mandatrophy by exhibiting the canonical mountain signature: extractiveness ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally = true. This constraint unambiguously classifies as mountain from all perspectives because it satisfies the natural law certification chain. There is no alternative interpretation where the Sylow Theorems appear as extraction, coordination, scaffolding, or theater. The theorems are necessary mathematical truths, not contingent institutional arrangements. The mandatrophy is fully resolved: this is a canonical example of a constraint that cannot be misclassified as a snare (pure extraction) or a tangled rope (mixed extraction/coordination) because no coordination function and no beneficiary/victim structure exist. The constraint is structurally pure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_versus_classical,
    'Does the Sylow existence theorem require classical logic (law of excluded middle) or hold in constructive/intuitionistic mathematics?',
    'Formalization in constructive type theory (e.g., Coq, Agda); identification of which proof steps require classical logic vs. constructive content',
    'If classical-only: mountain classification weakens in constructive mathematics (depends on foundational choice). If fully constructive: mountain classification is confirmed across all foundational frameworks. Current status: the existence theorem is classically provable; constructive proofs are non-trivial (exist for specific cases) but full generalization is open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructive_versus_classical, conceptual, 'Dependence on classical logic vs. constructive foundations').

omega_variable(
    finte_versus_profinite,
    'Do the Sylow Theorems extend to profinite groups or infinite groups with finite index subgroups?',
    'Examination of inverse limit structures and compactness; comparison of finite Sylow structure with infinite profinite analogues',
    'If they extend: mountain classification applies to a wider domain. If they require finiteness essentially: constraint is boundary-dependent (mountain only for finite groups, scaffold or rope for infinite cases). Current status: profinite analogues exist but differ in structure (Sylow subgroups need not be cyclic in profinite case).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finte_versus_profinite, conceptual, 'Applicability to profinite and infinite groups').

omega_variable(
    categorical_invariance,
    'Are the Sylow Theorems invariant across different categorical encodings of group theory (e.g., group objects in a topos, group stacks, derived groups)?',
    'Formalization in category theory and higher-dimensional algebra; test whether Sylow structure persists under categorical abstraction',
    'If invariant: mountain classification extends across category-theoretic frameworks. If not: the theorems are specific to the classical group category, not universal mathematical truths. Current status: classical Sylow holds universally in group categories; higher categorical versions (derived Sylow) are emerging.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_invariance, conceptual, 'Invariance across categorical and higher-dimensional algebraic frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sylow_theorems_group_theory, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sylow_tr_t0, sylow_theorems_group_theory, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sylow_tr_t50, sylow_theorems_group_theory, theater_ratio, 50, 0.15).
narrative_ontology:measurement(sylow_tr_t150, sylow_theorems_group_theory, theater_ratio, 150, 0.15).

% Extraction over time
narrative_ontology:measurement(sylow_be_t0, sylow_theorems_group_theory, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(sylow_be_t50, sylow_theorems_group_theory, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(sylow_be_t150, sylow_theorems_group_theory, base_extractiveness, 150, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sylow_theorems_group_theory, information_standard).
narrative_ontology:affects_constraint(sylow_theorems_group_theory, lagrange_theorem_subgroup_order).
narrative_ontology:affects_constraint(sylow_theorems_group_theory, cauchy_theorem_element_order).
narrative_ontology:affects_constraint(sylow_theorems_group_theory, finite_group_classification).

% DUAL FORMULATION NOTE:
% The Sylow Theorems are foundational to the structure theory of finite groups. The constraint affects downstream results in group classification (e.g., classification of groups of order p^n, proof that groups of prime order are cyclic). The upstream constraints (Lagrange's Theorem on subgroup index, Cauchy's Theorem on element order) are necessary prerequisites for Sylow existence proofs, forming a mathematical dependency hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
