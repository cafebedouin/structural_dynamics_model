% ============================================================================
% CONSTRAINT STORY: weierstrass_function
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_weierstrass_function, []).

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
 *   constraint_id: weierstrass_function
 *   human_readable: The Existence of Continuous, Nowhere-Differentiable Functions
 *   domain: mathematical_analysis/topology
 *
 * SUMMARY:
 *   The Weierstrass function (first published 1872) is the canonical
 *   mathematical constraint: a function that is continuous everywhere but
 *   differentiable nowhere. Its existence demonstrates that continuity and
 *   differentiability are structurally independent properties of real-valued
 *   functions — a result that shocked 19th-century mathematicians who
 *   intuited that 'continuous' and 'smooth' were nearly synonymous. The
 *   constraint classifies as Mountain from all perspectives because it
 *   emerges necessarily from the axioms of real analysis. The Weierstrass
 *   function itself is constructed as an infinite series of cosines with
 *   carefully chosen amplitudes and frequencies; variants include the Takagi
 *   curve, van der Waerden function, and Bolzano's earlier (unpublished)
 *   example. The constraint exhibits zero degrees of freedom: no mathematical
 *   framework extending the Peano axioms can avoid the existence of such
 *   functions. No agent can exit the constraint through theoretical
 *   innovation, computational workaround, or dimensional augmentation. This
 *   is a pure natural law in the mathematical domain.
 *
 * KEY AGENTS:
 *   - Mathematical Analyst: Observer (analytical/analytical) — sees the constraint as logically irreducible; derives its necessity from axioms
 *   - Applied Engineer: Victim-observer (powerful/mobile) — must account for pathological functions in proofs; cannot escape via applied domain specialization
 *   - Undergraduate Mathematician: Learner (moderate/constrained) — forced to internalize deeper topological understanding; no pedagogical escape route
 *   - Pure Mathematics Establishment: Institutional beneficiary (institutional/arbitrage) — constraint grounds institutional rigor and theoretical legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(weierstrass_function, 0.12).
domain_priors:suppression_score(weierstrass_function, 0.02).
domain_priors:theater_ratio(weierstrass_function, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(weierstrass_function, extractiveness, 0.12).
narrative_ontology:constraint_metric(weierstrass_function, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(weierstrass_function, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(weierstrass_function, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(weierstrass_function, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(weierstrass_function, mountain).
narrative_ontology:human_readable(weierstrass_function, "The Existence of Continuous, Nowhere-Differentiable Functions").
narrative_ontology:topic_domain(weierstrass_function, "mathematical_analysis/topology").

domain_priors:emerges_naturally(weierstrass_function).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL ANALYST (MOUNTAIN) — From the civilizational/universal vantage of mathematical logic, the existence of continuous nowhere-differentiable functions is a logically irreducible consequence of the axioms of real analysis. The Weierstrass function (or any of its variants: Takagi, van der Waerden, Bolzano) demonstrates that smoothness and continuity are structurally independent properties. No workaround exists; no alternative axiomatization eliminates this constraint. It is immutable across all formal systems extending the Peano axioms.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED ENGINEER (MOUNTAIN) — Even when designing control systems, signal processing algorithms, or numerical methods, the engineer cannot escape this constraint. Pathological functions exist and must be accounted for in stability proofs. The engineer has no exit: any real-valued function used in optimization or PDE solving could, in principle, exhibit nowhere-differentiability. This is not a contingent barrier but a structural property of the continuous real line.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: UNDERGRADUATE MATHEMATICIAN (MOUNTAIN) — Students learning calculus and real analysis encounter this constraint as a fundamental limit: they cannot assume that all continuous functions are piecewise smooth or that smoothness is generic. The constraint forces deeper understanding of measure theory and functional analysis. No pedagogical workaround changes the underlying mathematical reality.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PURE MATHEMATICS ESTABLISHMENT (MOUNTAIN) — Institutions (mathematics departments, journals, research funders) structure their entire theoretical frameworks around this constraint. The existence of pathological functions is not negotiable — it grounds rigorous proofs in topology, functional analysis, and approximation theory. The constraint enables institutional legitimacy (rigor) rather than extracting value from it.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(weierstrass_function_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(weierstrass_function, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(weierstrass_function, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(weierstrass_function, ExtMetricName, E),
    domain_priors:suppression_score(weierstrass_function, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(weierstrass_function),
    narrative_ontology:constraint_metric(weierstrass_function, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(weierstrass_function, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(weierstrass_function_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The Weierstrass function does not extract value from any agent — it is a structural property of the continuous real line that all agents must respect equally. The small nonzero value reflects the minimal 'cost' of incorporating pathological functions into proofs and algorithms (added complexity in stability analysis, need for Hölder continuity bounds instead of differentiability assumptions). Suppression (0.02): Negligible. The constraint suppresses no alternatives because there are no alternatives — it is not a choice or regulation, but a logical necessity. The minimal value reflects that the constraint cannot even be formulated as 'suppressing something,' only as 'establishing what must be true.' Theater ratio (0.15): Very low. The mathematical proof is substantive and not performative. The Weierstrass function's proof (constructive series definition) demonstrates actual differentiability failure via epsilon-delta logic; the rigor is not theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows no perspectival gap across the four perspectives — all classify as Mountain with identical underlying reasoning. The engineer, the analyst, the student, and the institution all encounter the same immutable mathematical structure. The near-identity of classifications confirms the mountain characterization: the constraint is observer-independent. A perspectival gap would indicate contingency (different observers see different constraints); the absence of gap indicates necessity (all observers see the same unescapable structure).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation does not apply to mountain constraints. Mountains have no beneficiaries or victims — they are natural laws that constrain all agents equally. The Weierstrass function is not extracted by any agent from any other agent; it simply exists as a logical consequence of real analysis. All agents experience d ≈ 0.5 (symmetric impact) because none can exit or avoid the constraint through structural positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. The Weierstrass function is a pure mountain — a natural law of mathematics. There is no risk of misclassifying coordination as extraction or vice versa because the constraint involves no coordination function and no extraction mechanism. The constraint is purely structural: it establishes what must exist in any formal system extending classical real analysis. The mathematical establishment does not extract from engineers or students via this constraint; the constraint is simply a discovered fact about the structure of continuous functions. No false summit detection is needed — the constraint genuinely exhibits mountain properties (irreducible logical necessity, universal across all observables, zero degrees of freedom for all agents).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_mathematics_alternative,
    'Does constructive mathematics (intuitionistic logic) eliminate or merely delay the existence of nowhere-differentiable functions?',
    'Formal proof that nowhere-differentiable functions exist in constructive settings, or demonstration that constructive axioms structurally forbid them',
    'If constructive logic permits them: constraint is truly universal across foundational systems. If constructive logic forbids them: the constraint is contingent on classical logic choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_mathematics_alternative, conceptual, 'Whether constructive mathematics avoids the nowhere-differentiable constraint').

omega_variable(
    generalizable_to_other_spaces,
    'Does the existence of continuous nowhere-differentiable functions generalize to all metric spaces or only to specific topologies?',
    'Systematic construction or proof-theoretic impossibility for nowhere-differentiable analogues in Banach spaces, Fréchet spaces, or alternative topologies',
    'If truly universal: constraint is topological invariant. If topology-dependent: constraint is contingent on the Euclidean structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalizable_to_other_spaces, empirical, 'Generalizability of nowhere-differentiable functions across metric spaces').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(weierstrass_function, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(weier_tr_t0, weierstrass_function, theater_ratio, 0, 0.1).
narrative_ontology:measurement(weier_tr_t150, weierstrass_function, theater_ratio, 150, 0.15).
narrative_ontology:measurement(weier_tr_t300, weierstrass_function, theater_ratio, 300, 0.15).

% Extraction over time
narrative_ontology:measurement(weier_be_t0, weierstrass_function, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(weier_be_t150, weierstrass_function, base_extractiveness, 150, 0.12).
narrative_ontology:measurement(weier_be_t300, weierstrass_function, base_extractiveness, 300, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(weierstrass_function, information_standard).
narrative_ontology:affects_constraint(weierstrass_function, lipschitz_continuity).
narrative_ontology:affects_constraint(weierstrass_function, holder_continuity).
narrative_ontology:affects_constraint(weierstrass_function, differentiability_degree_freedom).

% DUAL FORMULATION NOTE:
% The Weierstrass function is part of a constraint family documenting the independence of smoothness properties. Related constraints include Lipschitz continuity (stricter than Weierstrass, implies differentiability almost everywhere), Hölder continuity (intermediate), and degree-of-freedom measurements in functional spaces. The Weierstrass function establishes the lower bound: continuity without any differentiability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
