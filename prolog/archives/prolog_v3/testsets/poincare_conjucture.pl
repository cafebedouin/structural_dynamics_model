% ============================================================================
% CONSTRAINT STORY: poincare_conjecture
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_poincare_conjecture, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: poincare_conjecture
 *   human_readable: The Poincaré Conjecture (Mathematical Theorem)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Poincaré Conjecture asserts that every simply connected, closed
 *   3-manifold is homeomorphic to the 3-sphere. Proven by Grigori Perelman,
 *   it is now a theorem that functions as an unchangeable feature of
 *   topological space. This constraint story models the mathematical truth
 *   itself, which is a Mountain—a fixed, non-extractive law of a formal
 *   system. The social dynamics surrounding its proof and the associated
 *   prizes are a separate, coupled constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - Any 3-Manifold: The subject whose identity is fixed by its topological
 *     properties (powerless/trapped).
 *   - Topologists & Physicists: Analytical observers who use the theorem as a
 *     foundational tool for classification (institutional/analytical).
 *
 * DUAL FORMULATION NOTE:
 *   This constraint is one of two stories decomposed from the colloquial
 *   label "The Poincaré Conjecture". Decomposed because ε differs across
 *   observables (ε-invariance principle).
 *   - poincare_conjecture (ε=0.01, Mountain): The mathematical theorem itself.
 *   - professional_recognition_system (ε≈0.48, Snare): The social system of
 *     prizes and validation that extracts compliance from researchers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% A proven mathematical theorem has virtually zero extractiveness.
domain_priors:base_extractiveness(poincare_conjecture, 0.01).
% It does not suppress alternatives; it defines a category of object.
domain_priors:suppression_score(poincare_conjecture, 0.01).
% Mathematical truth has no performative aspect.
domain_priors:theater_ratio(poincare_conjecture, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(poincare_conjecture, extractiveness, 0.01).
narrative_ontology:constraint_metric(poincare_conjecture, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(poincare_conjecture, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% A topological necessity is nearly absolute.
narrative_ontology:constraint_metric(poincare_conjecture, accessibility_collapse, 0.99).
% Resistance to a proven theorem is mathematically incoherent.
narrative_ontology:constraint_metric(poincare_conjecture, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(poincare_conjecture, mountain).
narrative_ontology:human_readable(poincare_conjecture, "The Poincaré Conjecture (Mathematical Theorem)").
narrative_ontology:topic_domain(poincare_conjecture, "mathematical").

% --- Emergence flag (required for mountain constraints) ---
% The theorem emerges from the axioms of topology without human enforcement.
domain_priors:emerges_naturally(poincare_conjecture).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain, the theorem has no beneficiaries or
% victims in the structural sense; it is a feature of the landscape.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   As a Mountain, the classification is invariant across all perspectives.
   The low ε (0.01) ensures χ remains near zero for all indices.
   ========================================================================== */

% PERSPECTIVE 1: THE 3-MANIFOLD (THE SUBJECT)
% The manifold itself is bound by this topological law. It cannot change its
% nature. From its perspective, the constraint is an unchangeable fact.
constraint_indexing:constraint_classification(poincare_conjecture, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE WORKING MATHEMATICIAN
% For a topologist or physicist, the proven conjecture is a foundational
% piece of the landscape—a fixed point of reference.
constraint_indexing:constraint_classification(poincare_conjecture, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% From a detached, analytical viewpoint, the theorem is a fundamental,
% non-extractive truth about the structure of 3-manifolds.
constraint_indexing:constraint_classification(poincare_conjecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(poincare_conjecture_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from multiple perspectives,
    % demonstrating the invariance expected of a natural law.
    constraint_indexing:constraint_classification(poincare_conjecture, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(poincare_conjecture, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == mountain.

test(natural_law_profile) :-
    % Verify that the full Natural Law profile is declared.
    domain_priors:emerges_naturally(poincare_conjecture),
    narrative_ontology:constraint_metric(poincare_conjecture, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(poincare_conjecture, resistance, R), R =< 0.15.

test(threshold_validation) :-
    % Verify that the base metrics are within Mountain thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(poincare_conjecture, ExtMetricName, E),
    narrative_ontology:constraint_metric(poincare_conjecture, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(poincare_conjecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file conflated the mathematical theorem (ε≈0) with the
 *   social system of prizes and recognition surrounding its proof (ε>0).
 *   This violates the ε-invariance principle. This regenerated file models
 *   ONLY the mathematical theorem, which is a classic Mountain. Its base
 *   extractiveness and suppression are near zero, as a mathematical truth
 *   does not extract value or coerce behavior.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a Mountain, the classification is
 *   invariant across all indices. This uniformity is a key signature of a
 *   natural law or a fundamental logical constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, there are no structural beneficiaries or victims. The
 *   theorem is a feature of the formal landscape, not a mechanism for
 *   transferring value between groups. Therefore, no beneficiary/victim
 *   declarations are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   By decomposing the mathematical truth from the social system, this
 *   classification avoids mislabeling a fundamental law as extractive. The
 *   extractive dynamics (e.g., pressure to accept prizes) belong to a
 *   separate constraint story (`professional_recognition_system`), likely a
 *   Snare or Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_poincare_conjecture,
    'Is the perceived necessity of mathematical theorems a feature of the universe or a cognitive artifact of axiomatic systems?',
    'Resolution via formal models of cognition vs. fundamental physics, or evidence of mathematics being unreasonably effective.',
    'If purely a cognitive artifact, math is a Rope (a coordination tool for minds like ours). If a feature of the universe, it is a Mountain.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_poincare_conjecture, conceptual, 'Distinction between mathematical truth as universal law vs. cognitive construct.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(poincare_conjecture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints (base_extractiveness <= 0.46).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% --- Network Decomposition (Constraint Families) ---
% This constraint is decomposed from the colloquial label "The Poincaré
% Conjecture" to separate the mathematical truth from the social dynamics.
% The theorem (Mountain) provides the foundation upon which the social
% recognition system (Snare) operates.
narrative_ontology:affects_constraint(poincare_conjecture, professional_recognition_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */