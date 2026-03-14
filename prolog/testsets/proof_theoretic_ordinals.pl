% ============================================================================
% CONSTRAINT STORY: proof_theoretic_ordinals
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proof_theoretic_ordinals, []).

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
 *   constraint_id: proof_theoretic_ordinals
 *   human_readable: Proof Theoretic Ordinals and Transfinite Induction
 *   domain: mathematical_logic/proof_theory
 *
 * SUMMARY:
 *   Proof-theoretic ordinals are a mathematical structure that ranks formal
 *   systems and proof methods by their capacity to establish termination and
 *   consistency claims. An ordinal is assigned to each formal system based on
 *   the strength of theorems it can prove about recursive functions and
 *   well-orderings. This ranking is not a human convention — it emerges from
 *   the logical structure of proofs themselves. The constraint is that any
 *   attempt to prove termination or consistency of a formal system must use
 *   proof methods that are themselves justified by proof-theoretic ordinals
 *   at or below a fixed limit (the ordinal of the system). This creates an
 *   irreducible hierarchy: one cannot 'escape' to a higher ordinal without
 *   changing the formal system itself. The structure is invariant across
 *   equivalent formal systems and represents a fundamental feature of how
 *   formal logic relates to the transfinite.
 *
 * KEY AGENTS:
 *   - Formal System Designer: Faces the ordinal hierarchy as a structural constraint on proof capacity (analytical/trapped)
 *   - Proof Searcher: Attempting to establish consistency or termination properties must respect ordinal ordering (powerless/trapped)
 *   - Mathematical Community: Uses ordinal hierarchies as a coordination standard for measuring proof strength (institutional/arbitrage)
 *   - Logical Foundation: The structure itself — ordinals emerge from the axioms, not from external constraints (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proof_theoretic_ordinals, 0.12).
domain_priors:suppression_score(proof_theoretic_ordinals, 0.03).
domain_priors:theater_ratio(proof_theoretic_ordinals, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proof_theoretic_ordinals, extractiveness, 0.12).
narrative_ontology:constraint_metric(proof_theoretic_ordinals, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(proof_theoretic_ordinals, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(proof_theoretic_ordinals, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(proof_theoretic_ordinals, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proof_theoretic_ordinals, mountain).
narrative_ontology:human_readable(proof_theoretic_ordinals, "Proof Theoretic Ordinals and Transfinite Induction").
narrative_ontology:topic_domain(proof_theoretic_ordinals, "mathematical_logic/proof_theory").

domain_priors:emerges_naturally(proof_theoretic_ordinals).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROOF SEARCHER (MOUNTAIN) — Any agent attempting to establish consistency or termination properties faces an immutable constraint: the ordinal hierarchy cannot be escaped. Proof-theoretic ordinals form a natural ordering that any valid proof of termination must respect. No strategy circumvents this structure — it is immanent to logic itself.
constraint_indexing:constraint_classification(proof_theoretic_ordinals, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From the most abstract vantage point, proof-theoretic ordinals represent an irreducible structural feature of formal systems. The ordering of proof complexity, termination depth, and definitional strength cannot be reorganized or avoided — it is entailed by the logical foundations themselves.
constraint_indexing:constraint_classification(proof_theoretic_ordinals, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proof_theoretic_ordinals_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(proof_theoretic_ordinals, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proof_theoretic_ordinals, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(proof_theoretic_ordinals, ExtMetricName, E),
    domain_priors:suppression_score(proof_theoretic_ordinals, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(proof_theoretic_ordinals),
    narrative_ontology:constraint_metric(proof_theoretic_ordinals, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(proof_theoretic_ordinals, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(proof_theoretic_ordinals_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint does not extract resources or impose asymmetric costs — it is a structural fact about formal systems. No agent 'benefits' from the ordinal hierarchy at the expense of others; all agents face the same immutable structure. The minimal non-zero value reflects that understanding and applying ordinal theory requires specialized knowledge, a small access cost, but not extraction in the DR sense. Suppression (0.03): Negligible. There are no alternatives to avoid — the ordinal hierarchy is not a coercive system that constrains behavior through threats or barriers. It is a logical necessity. Resistance (0.08): Minimal. The constraint cannot be resisted because it is entailed by the axioms of formal systems. Accessibility collapse (0.92): Very high. Once a formal system is chosen, its proof-theoretic ordinal is fully determined — there is no accessibility gradient or negotiation. Theater ratio (0.15): Minimal. The ordinal structure is functionally transparent — the theory and practice of proof-theoretic ordinals align with no performative gap.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives classify as mountain because the constraint is invariant across all observational positions. This is a core feature of NL constraints: the classification does not change when the observer's position changes. A proof searcher at any power level experiences the same ordinal hierarchy. An analytical observer from any time horizon sees the same structure. This uniformity is the signature of a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality computation is necessary for this constraint. Mountain classifications in the NL framework do not generate beneficiaries or victims — the constraint is not extractive and does not redistribute resources or capabilities. All agents face the same ordinal structure; none is advantaged or harmed by its existence. The constraint's binding power comes from logical necessity, not from enforcement or coercion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinal_assignment_uniqueness,
    'Is the assignment of proof-theoretic ordinals to formal systems unique and intrinsic, or dependent on the choice of formal framework?',
    'Comparative ordinal analysis across equivalent formal systems (different axiomatizations of the same mathematics); verification that ordinal rankings are invariant under conservative extensions',
    'If intrinsic: confirms mountain status across all formal models. If framework-dependent: suggests the constraint is a contingent feature of our chosen proof theory, potentially reducing to rope or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_assignment_uniqueness, conceptual, 'Uniqueness of proof-theoretic ordinal assignments').

omega_variable(
    ordinal_accessibility_complete,
    'Can all proof-theoretic ordinals be made explicit and computable within a fixed formal system, or does accessibility fundamentally break at some level?',
    'Investigation of whether every accessible ordinal can be defined and computed within ZFC or related systems; analysis of Feferman''s vague boundary between classical and predicatively acceptable ordinals',
    'If all accessible ordinals are definable: accessibility is a real constraint but engineerable within formal systems. If accessibility breaks: the constraint transcends any single formal framework, reinforcing mountain status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ordinal_accessibility_complete, conceptual, 'Computability and accessibility of proof-theoretic ordinals').

omega_variable(
    ordinal_necessity_vs_convention,
    'Does the ordinal hierarchy arise from the logical structure of proofs, or from conventional choices in how we measure proof complexity?',
    'Proof-theoretic analysis of whether ordinal rankings follow necessarily from the axioms of a formal system, or whether alternative complexity measures could yield equivalent results with different orderings',
    'If necessary: the constraint is a feature of logic. If conventional: the constraint is a human-chosen framework, reducing toward rope (coordination of measurement standards).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ordinal_necessity_vs_convention, conceptual, 'Whether ordinal hierarchy is logically necessary or conventionally chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proof_theoretic_ordinals, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pto_tr_t0, proof_theoretic_ordinals, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pto_tr_t25, proof_theoretic_ordinals, theater_ratio, 25, 0.13).
narrative_ontology:measurement(pto_tr_t50, proof_theoretic_ordinals, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(pto_be_t0, proof_theoretic_ordinals, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pto_be_t25, proof_theoretic_ordinals, base_extractiveness, 25, 0.11).
narrative_ontology:measurement(pto_be_t50, proof_theoretic_ordinals, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proof_theoretic_ordinals, information_standard).
narrative_ontology:affects_constraint(proof_theoretic_ordinals, godel_incompleteness_first).
narrative_ontology:affects_constraint(proof_theoretic_ordinals, church_turing_thesis).

% DUAL FORMULATION NOTE:
% Proof-theoretic ordinals are foundational to the structure of incompleteness theorems and computability theory. The ordinal hierarchy directly determines which propositions can be proven within fixed formal systems and which require stronger systems. These downstream constraints inherit the mountain status of the ordinal framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
