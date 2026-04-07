% ============================================================================
% CONSTRAINT STORY: conditional_expectation_mathematical_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conditional_expectation_mathematical_structure, []).

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
 *   constraint_id: conditional_expectation_mathematical_structure
 *   human_readable: Conditional Expectation Mathematical Structure
 *   domain: probability_theory/measure_theory
 *
 * SUMMARY:
 *   Conditional expectation is a mathematical structure that defines how
 *   probability distributions must behave when restricted to smaller
 *   σ-algebras. Given a probability space (Ω, F, P) and a sub-σ-algebra G ⊆
 *   F, the conditional expectation E[X|G] is a G-measurable random variable
 *   satisfying the fundamental property: E[E[X|G]] = E[X]. The Radon-Nikodym
 *   theorem guarantees the existence and essential uniqueness of conditional
 *   expectation under absolute continuity conditions. This structure is
 *   invariant across all probability models, all σ-algebras, and all
 *   distributions. No observer can escape it without abandoning probability
 *   theory entirely. The constraint exhibits zero degrees of freedom: the
 *   tower property (E[E[X|G]|H] = E[X|H] for H ⊆ G), the pulling-out property
 *   (E[Xg(Y)|G] = gE[X|Y] under measurability), and the independence property
 *   must all hold universally. Theater ratio is negligible (0.05) because the
 *   mathematical proof is fully functional — no performative element clouds
 *   the logical structure. Extractiveness is minimal (0.12) because the
 *   constraint imposes no asymmetric burden on any observer; it simply
 *   defines the mathematical reality that all probability models must
 *   satisfy.
 *
 * KEY AGENTS:
 *   - Mathematical Logicians: Institutional/analytical observers — see the structure as following necessarily from measure-theoretic axioms
 *   - Applied Statisticians: Moderate/constrained observers — must respect the structure when estimating conditional distributions; the constraint is immutable but not harmful
 *   - Probability Theorists: Powerful/mobile observers — have full freedom to construct probability spaces, but cannot escape conditional expectation once spaces are defined
 *   - Students and Learners: Powerless/trapped observers — face the constraint as an irreducible feature of probability education; must learn the tower property as foundational
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conditional_expectation_mathematical_structure, 0.12).
domain_priors:suppression_score(conditional_expectation_mathematical_structure, 0.02).
domain_priors:theater_ratio(conditional_expectation_mathematical_structure, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conditional_expectation_mathematical_structure, extractiveness, 0.12).
narrative_ontology:constraint_metric(conditional_expectation_mathematical_structure, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(conditional_expectation_mathematical_structure, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(conditional_expectation_mathematical_structure, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(conditional_expectation_mathematical_structure, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conditional_expectation_mathematical_structure, mountain).
narrative_ontology:human_readable(conditional_expectation_mathematical_structure, "Conditional Expectation Mathematical Structure").
narrative_ontology:topic_domain(conditional_expectation_mathematical_structure, "probability_theory/measure_theory").

domain_priors:emerges_naturally(conditional_expectation_mathematical_structure).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — Conditional expectation emerges as a mathematical necessity from measure theory. The Radon-Nikodym theorem establishes that for any σ-algebra and absolutely continuous measure, a unique conditional expectation exists. This is a structural theorem, not a convention. No observer can escape the mathematical constraint — all probability models must satisfy the tower property, the pulling-out property, and the independence property. These are logical necessities, not empirical contingencies.
constraint_indexing:constraint_classification(conditional_expectation_mathematical_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL MODELER (MOUNTAIN) — Even agents with full modeling freedom face the conditional expectation constraint as immutable. A practitioner cannot construct a probability model that violates the tower property (E[E[X|G]|F] = E[X|F] for F ⊆ G) without abandoning probability theory entirely. The structure is invariant across all choice of probability space, all σ-algebras, all distributions. Resistance is zero — no meaningful alternative exists within the formal framework.
constraint_indexing:constraint_classification(conditional_expectation_mathematical_structure, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: APPLIED STATISTICIAN (MOUNTAIN) — When estimating conditional distributions from data, the constraint that conditional expectation satisfies the tower property is immutable. No amount of computational power or alternative methodology changes the mathematical fact. The statistician is constrained by the structure, but this is a constraint of logic, not coercion. Accessibility collapse reflects that this mathematical law is learned once and applied universally.
constraint_indexing:constraint_classification(conditional_expectation_mathematical_structure, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: STUDENT (MOUNTAIN) — A learner cannot construct a mental model of probability that violates the conditional expectation structure. The constraint is immutable from this vantage point — the tower property must be learned, not negotiated. However, this is educational immutability (the structure does not change based on effort), not extractive immutability (the structure does not harm by existing). The student's learning curve is steep, but the mathematical truth is invariant.
constraint_indexing:constraint_classification(conditional_expectation_mathematical_structure, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conditional_expectation_mathematical_structure_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(conditional_expectation_mathematical_structure, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conditional_expectation_mathematical_structure, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(conditional_expectation_mathematical_structure, ExtMetricName, E),
    domain_priors:suppression_score(conditional_expectation_mathematical_structure, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(conditional_expectation_mathematical_structure),
    narrative_ontology:constraint_metric(conditional_expectation_mathematical_structure, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(conditional_expectation_mathematical_structure, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(conditional_expectation_mathematical_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Conditional expectation imposes no asymmetric burden on any agent — the mathematical structure simply constrains what probability models can be. There is no extraction flow: no agent benefits at another's expense, and no asymmetry in how the constraint applies. The 0.12 value reflects only that any mathematical fact creates minor cognitive friction (learning cost), which is below the mountain threshold (≤0.25) and consistent with logical/mathematical constraints. Suppression (0.02): Negligible. There are no alternatives to conditional expectation within probability theory — the Radon-Nikodym theorem uniquely determines the structure. This is logical necessity, not suppression of alternatives. Agents can theoretically work outside probability theory, but that is a trivial 'alternative.' Theater ratio (0.05): Negligible. The mathematical proof of conditional expectation's properties is fully functional. No gap exists between the form (the stated tower property) and the function (what conditional expectation actually does). All four NL profile requirements are satisfied: accessibility_collapse (0.92) reflects that the mathematical structure is universally applicable and leaves no wiggle room for observer variation; resistance (0.08) reflects that the logical proof is airtight with minimal room for challenge.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on mountain classification. The analytical observer sees logical necessity from measure theory; the modeler sees immutability across all choice of probability space; the statistician sees invariance in data analysis; the student sees irreducible learning requirement. There is no perspectival gap because conditional expectation is truly invariant. The constraint does not change based on who is observing it or what power they hold. This uniformity across all perspectives is the signature of a true mountain — all observers, regardless of position, perceive the same immutable structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there is no extraction flow and no beneficiary/victim distinction. Conditional expectation creates no asymmetry in how its structure applies to different agents. The mathematical fact that E[E[X|G]|H] = E[X|H] holds equally for all observers, all probability spaces, and all random variables. The canonical d value for 'analytical' observers with 'analytical' exit options yields d ≈ 0.73, producing f(d) ≈ 1.15 and an effective extraction χ ≈ 0.14 × 1.15 × 1.0 ≈ 0.16. However, this is noise from the computational pipeline — the actual directionality is zero because there is no structural asymmetry to measure. The formula reproduces the canonical fallback because no beneficiary/victim data exists, which is correct: conditional expectation has no beneficiaries or victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measure_theoretic_necessity_vs_philosophical_status,
    'Is conditional expectation a mathematical necessity (follows from Radon-Nikodym and measure theory) or a philosophical commitment about how to extend probability to smaller σ-algebras?',
    'Reconstruct the proof of uniqueness of conditional expectation from first principles; identify the axioms (choice of σ-algebra, absolute continuity) that make the structure necessary vs contingent.',
    'If necessary: mountain classification is correct. If contingent on axiom choice: the constraint is a ''rope'' (convention) rather than a ''mountain'' (law). However, the universality across all probability models suggests necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measure_theoretic_necessity_vs_philosophical_status, conceptual, 'Whether conditional expectation is mathematical necessity or philosophical commitment').

omega_variable(
    alternative_conditional_frameworks,
    'Do non-Radon-Nikodym frameworks (quantum probability, fuzzy probability, interval-valued probability) genuinely provide alternatives to classical conditional expectation, or do they embed it as a limiting case?',
    'Comparison of conditional expectation structures across quantum probability (density operators), fuzzy logic (α-cuts), and interval probability (Dempster-Shafer). Check whether these reduce to classical conditional expectation under standard interpretations.',
    'If alternatives are genuinely independent: conditional expectation is one mountain among many. If all reduce to classical structure: the constraint is uniquely immutable within probability theory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_conditional_frameworks, empirical, 'Whether alternative conditional frameworks provide genuine alternatives').

omega_variable(
    constructive_vs_classical_mathematics,
    'Does constructive mathematics (rejecting excluded middle) provide an alternative conditional expectation structure that avoids classical measure-theoretic necessity?',
    'Formal reconstruction of conditional expectation in intuitionistic logic and constructive analysis; identification of differences in definability and uniqueness proofs.',
    'If constructive conditional expectation differs substantially: the classical structure is contingent on classical logic. If constructive version converges to classical: the structure is logically invariant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructive_vs_classical_mathematics, conceptual, 'Whether constructive mathematics provides alternatives to classical conditional expectation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conditional_expectation_mathematical_structure, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(condexp_tr_t0, conditional_expectation_mathematical_structure, theater_ratio, 0, 0.03).
narrative_ontology:measurement(condexp_tr_t50, conditional_expectation_mathematical_structure, theater_ratio, 50, 0.05).
narrative_ontology:measurement(condexp_tr_t100, conditional_expectation_mathematical_structure, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(condexp_be_t0, conditional_expectation_mathematical_structure, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(condexp_be_t50, conditional_expectation_mathematical_structure, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(condexp_be_t100, conditional_expectation_mathematical_structure, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conditional_expectation_mathematical_structure, information_standard).
narrative_ontology:affects_constraint(conditional_expectation_mathematical_structure, tower_property_immutability).
narrative_ontology:affects_constraint(conditional_expectation_mathematical_structure, filtering_theorem_necessity).

% DUAL FORMULATION NOTE:
% Conditional expectation itself is a single, unified mathematical structure. No decomposition is needed. The network links represent how other probability-theoretic results (filtering theorems, martingale properties) depend on conditional expectation as a foundational constraint. Conditional expectation is upstream of these results, not decomposable into them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
