% ============================================================================
% CONSTRAINT STORY: kirby_paris_theorem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_kirby_paris_theorem, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kirby_paris_theorem
 *   human_readable: The Kirby-Paris Theorem (Independence of Goodstein's Theorem)
 *   domain: technological
 *
 * SUMMARY:
 *   The Kirby-Paris theorem demonstrates that Goodstein's theorem—a true statement
 *   about the termination of specific sequences of natural numbers—is unprovable
 *   within Peano Arithmetic (PA). It is a foundational result in mathematical
 *   logic, showing an inherent limitation of a formal system. This constraint
 *   is a pure example of a Mountain: an unchangeable, non-coercive, and
 *   fundamental feature of the logical landscape.
 *
 * KEY AGENTS (by structural relationship):
 *   - Foundational Finitist: Agent attempting to work exclusively within the axioms of PA. Experiences the theorem as an impassable barrier.
 *   - Transfinite Logician: Agent utilizing stronger axiomatic systems (like ZFC). Experiences the theorem as a landmark confirming the necessity of their tools.
 *   - Analytical Observer: Views the theorem as a fixed, structural property of formal systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kirby_paris_theorem, 0.20).
domain_priors:suppression_score(kirby_paris_theorem, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(kirby_paris_theorem, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kirby_paris_theorem, extractiveness, 0.20).
narrative_ontology:constraint_metric(kirby_paris_theorem, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kirby_paris_theorem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(kirby_paris_theorem, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(kirby_paris_theorem, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(kirby_paris_theorem, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the structure of formal logic.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(kirby_paris_theorem).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a uniform-type Mountain, this constraint does not
% have structural beneficiaries or victims. Its effects are a consequence of
% an agent's position relative to the logical landscape, not of the constraint's
% design.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   For a Mountain, classification is invariant across all indices.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE ANALYTICAL OBSERVER (MOUNTAIN)
% Views the theorem as a permanent, unchangeable feature of the logical landscape.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE TRANSFINITE LOGICIAN (MOUNTAIN)
% For an agent with access to stronger systems (ZFC), the theorem is still a
% fixed landmark. It doesn't extract from them; it confirms the landscape
% they already inhabit.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE FINITIST (MOUNTAIN)
% For an agent committed to Peano Arithmetic, the theorem is an impassable
% wall. They are trapped by it, but it is still a feature of nature, not a
% coercive Snare. It is a limit, not a trap designed by another agent.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kirby_paris_theorem_tests).

test(invariance_across_perspectives) :-
    % Verify that this is a uniform-type Mountain, invariant across indices.
    constraint_indexing:constraint_classification(kirby_paris_theorem, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(kirby_paris_theorem, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(kirby_paris_theorem, Type3, context(agent_power(powerless), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(mountain_metric_thresholds) :-
    % Verify that the base metrics adhere to the Mountain classification thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    config:param(mountain_extractiveness_max, MountainEpsMax),
    config:param(mountain_suppression_ceiling, MountainSuppMax),
    narrative_ontology:constraint_metric(kirby_paris_theorem, ExtMetricName, E),
    narrative_ontology:constraint_metric(kirby_paris_theorem, SuppMetricName, S),
    E =< MountainEpsMax,
    S =< MountainSuppMax.

test(natural_law_profile_present) :-
    % Verify the required NL profile metrics are declared for certification.
    narrative_ontology:constraint_metric(kirby_paris_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kirby_paris_theorem, resistance, R),
    domain_priors:emerges_naturally(kirby_paris_theorem),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kirby_paris_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint was re-classified from a mixed-type (Tangled Rope/Snare/Rope)
 *   to a uniform-type Mountain. Mathematical theorems about the limits of
 *   formal systems are canonical examples of Mountains. They are fixed,
 *   non-negotiable, and emerge from the structure of logic itself.
 *
 *   - Base Extractiveness (0.20): Represents the semantic "cost" of the theorem,
 *     which is the loss of provability for a class of truths within a specific
 *     system (PA). This is an informational, not a material, extraction.
 *   - Suppression Score (0.05): Corrected from 0.4. The theorem does not
 *     coercively suppress alternatives; it is a statement of fact about an
 *     inherent limitation. The low score is required for Mountain classification.
 *   - NL Profile (AC=1.0, R=0.0): The theorem's consequences are absolute within
 *     PA (accessibility_collapse=1.0), and resistance is incoherent (resistance=0.0).
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in classification; all agents perceive a
 *   Mountain. The perceived difference is in the *implication* of the Mountain.
 *   For the finitist, it's a barrier. For the transfinite logician, it's a
 *   signpost justifying the need for stronger tools. The constraint itself,
 *   however, remains an invariant feature of the landscape for both.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Mountain correctly identifies it as a feature of
 *   reality rather than a system of control. The original classification of
 *   Snare/Tangled Rope was a category error, anthropomorphizing a logical
 *   limit as if it were a coercive instrument. The Mountain classification
 *   prevents this mislabeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
omega_variable(
    omega_kirby_paris_theorem,
    'Is human mathematical intuition fundamentally finitistic, making transfinite reasoning a purely formal tool, or does intuition extend to concepts like epsilon-zero induction?',
    'Cognitive studies on how mathematicians develop and use transfinite reasoning; formal analysis of the cognitive leaps required.',
    'If intuition is finitistic, the theorem reveals a hard cognitive limit (Mountain). If intuition can be extended, it is merely an educational challenge to overcome.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_kirby_paris_theorem, conceptual, 'The relationship between formal transfinite methods and innate human mathematical intuition.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(kirby_paris_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a mathematical theorem, its properties are fixed and do
% not drift over time. No measurement facts are needed.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable. This is a standalone logical fact with no coordination
% function or direct structural influence on other policy constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. As a Mountain, there are no beneficiaries or victims, so
% directionality is not a relevant concept.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */