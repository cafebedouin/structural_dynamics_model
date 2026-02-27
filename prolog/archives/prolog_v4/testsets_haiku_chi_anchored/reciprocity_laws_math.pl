% ============================================================================
% CONSTRAINT STORY: reciprocity_laws_math
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
 *   Quadratic Reciprocity, first proved by Gauss in 1801, describes a
 *   fundamental symmetry in number theory: for distinct odd primes p and q,
 *   the relationship between whether p is a quadratic residue modulo q and
 *   whether q is a quadratic residue modulo p follows a precise, non-obvious
 *   rule depending only on p mod 4 and q mod 4. This is the archetype of a
 *   mathematical constraint that has no negotiation surface, no extraction
 *   mechanism, no beneficiary/victim structure. The law governs the behavior
 *   of prime numbers themselves, not institutional arrangements. Gauss's
 *   original proof was notoriously difficult; he later provided multiple
 *   alternative proofs. Modern generalizations (Artin Reciprocity, Class
 *   Field Theory) deepen the understanding but do not alter the fundamental
 *   constraint. The constraint satisfies all criteria for a Mountain: ε=0.08
 *   (minimal base extraction — the law imposes structure, not takes
 *   resources), suppression=0.02 (no coercion needed; the constraint is
 *   intrinsic to mathematical systems), theater_ratio=0.05 (proofs are
 *   functionally necessary, nearly zero performative content),
 *   accessibility_collapse=0.92 (extremely difficult to access the full proof
 *   space; alternative axiomatic approaches collapse to the same result),
 *   resistance=0.08 (mathematicians cannot resist or circumvent the
 *   constraint). All perspectives classify as Mountain because the constraint
 *   is invariant across all observation contexts.
 *
 * KEY AGENTS:
 *   - Mathematical System: The substrate (prime integers, number fields) — neither beneficiary nor victim; the constraint is intrinsic
 *   - Number Theorists: Passive observers of the constraint — analytically engaged, deriving consequences and proofs
 *   - Mathematicians: Active explorers of consequence space — generating alternative proofs and generalizations without escaping the constraint
 *   - Mathematical Institutions: Repositories of the constraint (textbooks, curricula) — benefit from its immutability, not from extraction
 *   - Logical Foundations: Axiom systems (ZFC, constructive logic) — all respecting the constraint; none can negate it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reciprocity_laws_math, 0.08).
domain_priors:suppression_score(reciprocity_laws_math, 0.02).
domain_priors:theater_ratio(reciprocity_laws_math, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reciprocity_laws_math, extractiveness, 0.08).
narrative_ontology:constraint_metric(reciprocity_laws_math, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(reciprocity_laws_math, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reciprocity_laws_math, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(reciprocity_laws_math, resistance, 0.08).

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

% PERSPECTIVE 1: UNIVERSAL MATHEMATICAL OBSERVER (MOUNTAIN) — Quadratic Reciprocity is a statement about prime numbers and quadratic residues that holds universally across all number-theoretic systems where it applies. The symmetry is invariant under any observable or measurement framework. ε=0.08, no beneficiary/victim structure, d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. The constraint imposes zero degrees of freedom: the relationship between Legendre symbols (p/q) and (q/p) is determined uniquely by the structure of prime factorization.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: NUMBER THEORIST (MOUNTAIN) — From the working mathematician's viewpoint, Quadratic Reciprocity is a discovered fact, not an imposed rule. It constrains what is possible in number theory: any attempt to construct a number system or extension field must respect this symmetry or break the foundations of prime arithmetic. The mathematician cannot circumvent or negotiate the constraint. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.08. Zero beneficiary/victim relationship; the law simply exists.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: PROOF AUTHORITY (MOUNTAIN) — Mathematicians with strong agency (Gauss, Eisenstein, modern algebraists) have repeatedly proved Quadratic Reciprocity from different foundational axioms and discovered deeper generalizations (Artin Reciprocity, Class Field Theory). Yet none of these powerful agents can negate the fundamental symmetry — their agency consists entirely of finding new ways to represent and explain it, not of escaping it. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.048. The constraint exhibits zero degrees of freedom.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICAL CANON (MOUNTAIN) — From the institutional perspective of mathematical knowledge (textbooks, curricula, research programs), Quadratic Reciprocity is invariant under all institutional contexts. It appears in number theory, algebraic geometry, cryptography, and coding theory — different applications, identical constraint. No institution can relax or re-negotiate the law. d≈0.00, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Institutional beneficiary: mathematics as a whole benefits from having immutable foundations, but this is not extraction in the DR sense.
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
 *   Extractiveness (0.08): Minimal. Quadratic Reciprocity does not extract value from one agent to another. It is a structural property of integers and prime fields. The only sense in which it 'extracts' is that it constrains what is possible in algebraic systems — but this is not extraction in the economic sense. Suppression (0.02): Minimal. There is no coercion: mathematicians freely choose to study the constraint and derive from it. The law is transparent and universal. Theater ratio (0.05): Nearly zero. Proofs of Quadratic Reciprocity are functionally necessary — each step either advances toward the result or is wasted. Some proofs (Gauss's original) are longer and more intricate than others (Eisenstein's), but the variation is in depth of understanding, not in performance for an audience. Accessibility collapse (0.92): High. The constraint is extremely difficult to understand at the frontier (Class Field Theory requires sophisticated algebraic geometry), but every alternative approach (elementary, algebraic, analytic) leads to the same fundamental result. The accessibility space collapses to a single truth. Resistance (0.08): Low. Mathematicians do not resist the constraint — they embrace it, study it, and build on it.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for Quadratic Reciprocity. All perspectives — analytical observer, number theorist, proof authority, mathematical institution — classify it as Mountain. This uniformity is itself a signature of a true natural law constraint: the constraint is invariant across all observation contexts, all foundational axiom systems, all agent power levels, and all time horizons. The absence of a gap reflects the constraint's universality. When a constraint appears uniform-type (all Mountain or all Rope), the perspectival minimum is relaxed: we include multiple perspectives to show the invariance, but all may classify identically.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim structure exists for mathematical reciprocity laws. The constraint is not imposed by one agent on another; it is intrinsic to the mathematical system itself. All perspectives derive d≈0.6-0.72 (analytical/observer-like), producing f(d)≈0.6-1.15, scaled by σ=1.0 (universal scope). The resulting χ values are all near ε (0.08) because there is no asymmetric extraction. The constraint is a Mountain from all perspectives precisely because it has no directionality: it applies equally to all mathematical systems and all agents engaging with it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reciprocity_laws_math, 1801, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reciprocity_laws_math, higher_reciprocity_laws_artin).
narrative_ontology:affects_constraint(reciprocity_laws_math, quadratic_forms_classification).
narrative_ontology:affects_constraint(reciprocity_laws_math, prime_distribution_patterns).

% DUAL FORMULATION NOTE:
% Quadratic Reciprocity is the foundational constraint in a family of reciprocity laws of increasing abstraction. Artin Reciprocity (higher reciprocity laws) generalizes the quadratic case to arbitrary number fields and characters. Quadratic Forms Classification depends on understanding quadratic residues, which is governed by this constraint. Prime Distribution Patterns (Dirichlet, Chebotarev) rely on the symmetries revealed by Quadratic Reciprocity. These downstream constraints have higher ε values reflecting their greater empirical content and dependency on this mathematical foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
