% ============================================================================
% CONSTRAINT STORY: reciprocity_laws_math
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reciprocity_laws_math, []).

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
 *   constraint_id: reciprocity_laws_math
 *   human_readable: Mathematical Reciprocity Laws (Quadratic)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Quadratic Reciprocity, first fully proved by Carl Friedrich Gauss in
 *   1801, is a theorem establishing a profound symmetry in the multiplicative
 *   structure of prime numbers. For odd primes p and q, the law states that
 *   the quadratic character of p modulo q and the quadratic character of q
 *   modulo p are related by a simple sign rule depending on p and q modulo 4.
 *   This constraint is a paradigmatic example of a mathematical Mountain: it
 *   emerges necessarily from the arithmetic of integer fields, cannot be
 *   violated or negotiated, and exhibits zero degrees of freedom across all
 *   observational contexts. No agent benefits from it; no agent is harmed; it
 *   is simply true. The extractiveness is minimal (ε=0.08) because the
 *   constraint imposes no costs and confers no selective advantage — it is a
 *   necessary feature of how primes behave. Theater ratio is low (0.15)
 *   because the theorem's truth is verifiable and contestable in principle,
 *   though its proof requires substantial sophistication.
 *
 * KEY AGENTS:
 *   - Number Theorists: Agents working within arithmetic (powerless/analytical) — discover and use reciprocity but cannot escape or negotiate it
 *   - Formal Systems: Logical frameworks (institutional/analytical) — instantiate arithmetic and therefore instantiate reciprocity as a necessary theorem
 *   - Mathematical Community: Organized researchers (organized/analytical) — generations of mathematicians have examined, generalized, and applied reciprocity but found no escape from its core constraint
 *   - The Prime Structure Itself: Analytical observer (analytical/analytical) — reciprocity is a property intrinsic to the multiplicative structure of integers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reciprocity_laws_math, 0.08).
domain_priors:suppression_score(reciprocity_laws_math, 0.02).
domain_priors:theater_ratio(reciprocity_laws_math, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reciprocity_laws_math, extractiveness, 0.08).
narrative_ontology:constraint_metric(reciprocity_laws_math, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(reciprocity_laws_math, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reciprocity_laws_math, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(reciprocity_laws_math, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reciprocity_laws_math, mountain).
narrative_ontology:human_readable(reciprocity_laws_math, "Mathematical Reciprocity Laws (Quadratic)").
narrative_ontology:topic_domain(reciprocity_laws_math, "mathematical/logical").

domain_priors:emerges_naturally(reciprocity_laws_math).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE NUMBER THEORIST (MOUNTAIN) — Cannot negotiate or escape quadratic reciprocity. The theorem is a fixed feature of arithmetic. Attempts to circumvent it fail universally. Zero degrees of freedom for all indices.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, quadratic reciprocity is an invariant property of the prime structure itself. No contingency, no institutional choice, no alternative formulation changes the underlying fact: the Legendre symbol exhibits this symmetry across all primes.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY (MOUNTAIN) — Despite centuries of research into reciprocity laws, cryptography, and number theory, no formulation of mathematics escapes this constraint. It is part of the bedrock of arithmetic itself. Organized mathematical efforts cannot overcome or negotiate this limit.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: FORMAL PROOF SYSTEM (MOUNTAIN) — Within any consistent formal system adequate to express number theory, quadratic reciprocity follows as a theorem. Institutions cannot declare it invalid or negotiate its truth. It is a property of the logical structure itself.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reciprocity_laws_math_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(reciprocity_laws_math, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reciprocity_laws_math, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reciprocity_laws_math, ExtMetricName, E),
    domain_priors:suppression_score(reciprocity_laws_math, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reciprocity_laws_math),
    narrative_ontology:constraint_metric(reciprocity_laws_math, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reciprocity_laws_math, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reciprocity_laws_math_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.08): Minimal. Quadratic reciprocity extracts nothing from any agent. It is a property of primes, not a redistributive mechanism. No one loses or gains relative position from its truth. The value is low because there is no coercive transfer — the constraint is purely informational about the structure of arithmetic. Suppression (0.02): Negligible. There are no alternatives to suppress, no exit paths to block, no competing claims to silence. The theorem is either true or false; it cannot be enforced (there is nothing to enforce) and cannot be resisted. Theater ratio (0.15): Low. The proof of quadratic reciprocity is long and non-obvious (Gauss provided eight different proofs), but the theorem itself is contestable in principle via attempted counterexample or formal disproof. No ritual performance is required to establish its truth — rigorous mathematical argument suffices. The ratio is not zero because reciprocity exhibits non-transparency: the symmetry it describes is non-obvious, and laypeople cannot immediately verify it. But among mathematicians, verification is straightforward, so theater is minimal.
 *
 * PERSPECTIVAL GAP:
 *   Quadratic Reciprocity exhibits NO perspectival gap. All four perspectives classify it as Mountain with identical supporting rationale: the constraint is a necessary property of the arithmetic structure itself. The theorem holds from the powerless number theorist's perspective (they cannot escape it), the analytical observer's perspective (it is a feature of logical structure), the mathematical community's perspective (no institution can negotiate or override it), and the formal system's perspective (it is a theorem in any consistent arithmetic). This invariance across all observational contexts confirms the Mountain classification and demonstrates that some natural laws are truly universal, not contingent on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Quadratic Reciprocity has no directionality in the technical sense because there is no extraction flow, no asymmetry, and no beneficiary-victim structure. The constraint is neutral toward all agents. The engine will derive d=0.50 (symmetric) for all agents because no agent is identified as beneficiary or victim. This symmetric directionality is appropriate and expected for Mountain constraints that are purely informational rather than redistributive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalizations_extent,
    'Are higher reciprocity laws (cubic, biquadratic, Artin reciprocity) manifestations of a single natural law or distinct mathematical phenomena?',
    'Class field theory analysis; evaluation of whether unified formulation subsumes all reciprocity laws under a single principle',
    'If unified: reciprocity is a single mountain. If distinct: each reciprocity law is a separate mountain with different structural properties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalizations_extent, conceptual, 'Whether reciprocity laws are unified or distinct phenomena').

omega_variable(
    model_independence,
    'Does quadratic reciprocity hold in all models of arithmetic (classical, constructive, intuitionistic), or only in classical logic?',
    'Formalization in alternative logical frameworks; proof-theoretic analysis of constructive validity',
    'If model-independent: pure mountain across all mathematical frameworks. If classical-only: mountain status is contingent on choice of logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_independence, conceptual, 'Whether reciprocity holds in all logical models').

omega_variable(
    computational_accessibility,
    'Is the Legendre symbol computation via quadratic reciprocity fundamentally necessary, or could an alternative algorithm achieve the same result without invoking the reciprocity property?',
    'Algorithmic analysis; comparison of computational complexity for reciprocity-based vs reciprocity-independent approaches',
    'If reciprocity is computationally necessary: it is a natural law of efficient arithmetic. If dispensable: it is a beautiful but contingent property.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_accessibility, empirical, 'Whether reciprocity is computationally necessary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reciprocity_laws_math, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(recip_tr_t0, reciprocity_laws_math, theater_ratio, 0, 0.1).
narrative_ontology:measurement(recip_tr_t50, reciprocity_laws_math, theater_ratio, 50, 0.12).
narrative_ontology:measurement(recip_tr_t200, reciprocity_laws_math, theater_ratio, 200, 0.15).

% Extraction over time
narrative_ontology:measurement(recip_be_t0, reciprocity_laws_math, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(recip_be_t50, reciprocity_laws_math, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(recip_be_t200, reciprocity_laws_math, base_extractiveness, 200, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reciprocity_laws_math, information_standard).
narrative_ontology:affects_constraint(reciprocity_laws_math, higher_reciprocity_laws).
narrative_ontology:affects_constraint(reciprocity_laws_math, quadratic_form_classification).
narrative_ontology:affects_constraint(reciprocity_laws_math, class_field_theory_foundation).

% DUAL FORMULATION NOTE:
% Quadratic Reciprocity is downstream of more fundamental properties of the integers (unique factorization, prime structure). It is upstream of cubic and higher reciprocity laws, which generalize its core insight. The constraint family includes Gauss's proof, higher reciprocity laws (Eisenstein, Kummer, Artin), and class field theory, which provides a unified framework for all reciprocity phenomena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
