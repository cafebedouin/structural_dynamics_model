% ============================================================================
% CONSTRAINT STORY: power_set
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_power_set, []).

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
 *   constraint_id: power_set
 *   human_readable: Axiomatic Set Theory's Power Set Axiom
 *   domain: mathematical/foundational_logic
 *
 * SUMMARY:
 *   The Power Set axiom in Zermelo-Fraenkel set theory (ZFC) asserts that for
 *   every set S, there exists a set P(S) containing all and only the subsets
 *   of S. This axiom classifies as a Mountain — an immutable logical
 *   necessity — because it is entailed by the very concept of 'set' in
 *   classical mathematics. The constraint exhibits zero degrees of freedom:
 *   no mathematical agent, observer, or alternative axiomatization can reject
 *   the power set closure without either (a) working in a non-ZFC framework
 *   with explicit different scope, or (b) incoherence. The axiom has remained
 *   unchanged since Zermelo's 1908 formulation and is universal across all
 *   ZFC-based mathematics. Its extractiveness (0.12) is minimal because the
 *   constraint is not extractive in the institutional sense — it does not
 *   benefit one mathematical community at the expense of another. Instead, it
 *   constitutes the very fabric of set-theoretic reasoning. Its suppression
 *   (0.02) is minimal because the axiom is transparent — its logical
 *   necessity is evident to any competent logician. Theater ratio (0.08) is
 *   minimal because there is no performative aspect; the axiom either holds
 *   or the entire ZFC system is incoherent.
 *
 * KEY AGENTS:
 *   - Foundational Logicians: Analytical observers (analytical/analytical) — see the axiom as a logical necessity, not an extractive constraint
 *   - Mathematics Community: Institutional actor (institutional/analytical) — adopts the axiom universally; no exit option available within ZFC
 *   - Non-Classical Set Theorists: Secondary agent (powerless/trapped) — cannot escape the underlying power-set concept even in alternative foundations; trapped within the logical structure
 *   - Applied Mathematicians: Pragmatic observers (analytical/immediate) — work with finite power sets algorithmically; the axiom's logical status is irrelevant to computation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_set, 0.12).
domain_priors:suppression_score(power_set, 0.02).
domain_priors:theater_ratio(power_set, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_set, extractiveness, 0.12).
narrative_ontology:constraint_metric(power_set, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(power_set, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(power_set, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(power_set, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(power_set, mountain).
narrative_ontology:human_readable(power_set, "Axiomatic Set Theory's Power Set Axiom").
narrative_ontology:topic_domain(power_set, "mathematical/foundational_logic").

domain_priors:emerges_naturally(power_set).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOUNDATIONAL LOGICIAN (MOUNTAIN) — The Power Set axiom is a logical necessity for any coherent first-order theory of sets. Without it, the notion of 'all subsets' becomes undefined, and the axiom is not extractive but constitutive of the mathematical structure itself. ε=0.12, suppression=0.02, χ≈0.10.
constraint_indexing:constraint_classification(power_set, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICS COMMUNITY (MOUNTAIN) — The Power Set axiom is adopted universally across mathematical practice because it is entailed by the concept of 'set' itself. No alternative axiomatization (NBG, ZF without Power Set) replaces it in foundational mathematics; rather, they are alternative frameworks with different scope. The constraint is immutable across all mathematical contexts where ZFC is the standard. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(power_set, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: NON-CLASSICAL SET THEORISTS (MOUNTAIN) — Even constructivists, intuitionists, and univalent foundations theorists who reject classical ZFC axioms must address the Power Set closure somehow. They cannot escape the underlying logical requirement without rejecting the entire concept of 'set' as a comprehensively complete object. The constraint is inescapable at the conceptual level. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.17. Still mountain because ε is immutable.
constraint_indexing:constraint_classification(power_set, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: APPLIED MATHEMATICIAN (MOUNTAIN) — When actually computing or reasoning about finite sets in applications (computer science, optimization), the Power Set axiom's logical necessity remains even though algorithmic computation cannot enumerate it. The axiom constrains what is thinkable, not what is computable. ε=0.12, suppression=0.02, χ≈0.09.
constraint_indexing:constraint_classification(power_set, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_set_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(power_set, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_set, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(power_set, ExtMetricName, E),
    domain_priors:suppression_score(power_set, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(power_set),
    narrative_ontology:constraint_metric(power_set, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(power_set, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(power_set_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The Power Set axiom is not extractive because it does not privilege one mathematical constituency over another. All mathematicians adopt it equally; it is constitutive of ZFC, not a tool of institutional extraction. The small non-zero value reflects the minimal overhead of having to accept the axiom as a precondition for working within ZFC — a cost borne equally by all. Suppression (0.02): Minimal. The axiom is fully transparent. No agent is prevented from knowing or critiquing it. No authority enforces acceptance through coercion — mathematicians adopt it because it is logically necessary, not because they are forced. Theater ratio (0.08): Minimal. There is virtually no performative content. The axiom either is or is not consistent with the rest of ZFC; there is no social performance involved in acceptance. The small value reflects minimal pedagogical theater in teaching the axiom, but this is not constitutive of the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most Mountain constraints, the Power Set axiom exhibits minimal perspectival gap because all mathematical observers agree on its necessity. The foundational logician sees it as a logical requirement. The mathematics community adopts it universally. Non-classical set theorists, even when rejecting ZFC, must address the power-set concept somehow — they cannot ignore it. The applied mathematician, while working with finite approximations, recognizes the axiom's logical status. All perspectives yield Mountain classification. The absence of perspectival gap is itself diagnostic: when a constraint is truly immutable and unextractive, all agents should perceive it identically.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundational Logician: analytical → d≈0.72, f(d)≈1.15. Mountain classification (ε=0.12 ensures Mountain gate regardless of d). Mathematics Community: institutional + analytical → d≈0.50, f(d)≈0.65. Equal participant in the axiom; no asymmetric extraction. Non-Classical Set Theorists: powerless + trapped → d≈0.92, f(d)≈1.38. Trapped within the conceptual structure, but no extraction occurs because the axiom is constitutive for all, not a tool of domination. Applied Mathematician: analytical + immediate → d≈0.70, f(d)≈1.14. Mountain classification holds across immediate and long-term horizons. No agent derives differential benefit; no agent bears differential cost. This uniform directionality across all perspectives is a hallmark of genuine Mountains — the constraint is invariant under all observational contexts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_validity,
    'Is the Power Set axiom a logical necessity or a foundational choice that could be rejected without incoherence?',
    'Analysis of consistent non-ZFC foundations (ZF-, Morse-Kelley, constructive type theories) to determine whether they address the power set concept or avoid it entirely. Formal proof of equivalence/incommensurability.',
    'If necessity: Mountain classification confirmed. If choice: reclassifies as Scaffold (temporary foundational commitment with sunset as category theory alternatives mature).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_validity, conceptual, 'Whether Power Set is logically necessary or foundational choice').

omega_variable(
    cardinality_ontology,
    'Does the Power Set axiom extract ontological commitment from mathematics, or does it merely formalize what is already implicit in the concept of ''set''?',
    'Historical analysis of pre-axiomatic set theory (Cantor, Dedekind) to determine whether the power set concept predates the axiom. Comparison of set-theoretic reasoning in non-ZFC contexts.',
    'If formalization: Mountain (constitutive, not extractive). If ontological imposition: Tangled Rope (adds new objects beyond intuitive notion of set).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cardinality_ontology, conceptual, 'Whether Power Set formalizes or adds ontological content').

omega_variable(
    computational_finiteness_gap,
    'For finite sets, does the Power Set axiom create a gap between the mathematical object (infinite closure) and the algorithmic reality (finite approximation)?',
    'Formal comparison of ZFC power set semantics with constructive/computable set theory semantics. Analysis of where applied mathematics diverges from axiomatic requirements.',
    'If significant gap: reveals that axiomatic constraint may be Snare for applied mathematicians (ε increases to 0.35+). If minimal: Mountain classification holds across applications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_finiteness_gap, empirical, 'Gap between axiomatic and computational power set semantics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(power_set, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(powerset_tr_t0, power_set, theater_ratio, 0, 0.04).
narrative_ontology:measurement(powerset_tr_t50, power_set, theater_ratio, 50, 0.07).
narrative_ontology:measurement(powerset_tr_t100, power_set, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(powerset_be_t0, power_set, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(powerset_be_t50, power_set, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(powerset_be_t100, power_set, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(power_set, information_standard).
narrative_ontology:affects_constraint(power_set, cantor_diagonal_argument).
narrative_ontology:affects_constraint(power_set, russell_paradox_resolution).
narrative_ontology:affects_constraint(power_set, aleph_number_hierarchy).

% DUAL FORMULATION NOTE:
% The Power Set axiom is foundational upstream to all results in transfinite set theory, cardinality theory, and the ZFC consistency proofs. Downstream constraints (Cantor diagonal, Russell paradox formalization, aleph hierarchies) depend on it as a logical prerequisite, not as an alternative framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
