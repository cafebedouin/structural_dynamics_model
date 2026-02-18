% ============================================================================
% CONSTRAINT STORY: ergodic_theorems
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_ergodic_theorems, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergodic_theorems
 *   human_readable: The Misapplication of Ergodic Theorems in Non-Ergodic Systems
 *   domain: economic/scientific
 *
 * SUMMARY:
 *   Ergodic theorems describe systems where the average over time for one path
 *   equals the average over an ensemble of parallel paths. This constraint is
 *   not the theorem itself (a Mountain of mathematics), but its misapplication
 *   in non-ergodic systems (e.g., personal finance, evolutionary survival) where
 *   path-dependency and ruin are critical. Assuming ergodicity allows for
 *   simplified aggregate models but creates severe risks for individuals whose
 *   single path can be terminated.
 *
 * KEY AGENTS (by structural relationship):
 *   - Path-Dependent Individuals: Primary target (powerless/trapped) — bears the risk of ruin from policies based on ensemble averages.
 *   - Institutional Aggregators: Primary beneficiary (institutional/arbitrage) — benefits from pooling risk and applying simplified ergodic models across large populations.
 *   - Mathematical Physicist: Analytical observer of the pure theorem (analytical/trapped) — sees the mathematical law as an unchangeable Mountain.
 *   - Systems Analyst: Analytical observer of the socio-economic system (analytical/analytical) — sees the full Tangled Rope structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The *assumption* of ergodicity in non-ergodic domains extracts
% value by justifying risky policies that work on average but cause individual ruin.
domain_priors:base_extractiveness(ergodic_theorems, 0.35).
% Rationale: Non-ergodicity is frequently suppressed in mainstream economic
% modeling. "Expected value" (ensemble average) is presented as the only rational
% choice, actively hiding the path-dependent risk of termination.
domain_priors:suppression_score(ergodic_theorems, 0.60).
domain_priors:theater_ratio(ergodic_theorems, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergodic_theorems, extractiveness, 0.35).
narrative_ontology:constraint_metric(ergodic_theorems, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(ergodic_theorems, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ergodic_theorems, tangled_rope).
narrative_ontology:human_readable(ergodic_theorems, "The Misapplication of Ergodic Theorems in Non-Ergodic Systems").
narrative_ontology:topic_domain(ergodic_theorems, "economic/scientific").

% --- Binary flags ---
% The suppression of non-ergodic viewpoints in economic curricula and policy
% discussions constitutes a form of active enforcement of the ergodic assumption.
domain_priors:requires_active_enforcement(ergodic_theorems).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ergodic_theorems, institutional_aggregators).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ergodic_theorems, path_dependent_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL GAMBLER (SNARE)
% For the individual facing a non-ergodic game, the "average" is a Snare.
% If a game has a positive expected value but also a non-zero chance of ruin,
% the ensemble average is irrelevant. The structure of the game strangles the
% player over time as the probability of hitting the absorbing barrier approaches 1.
constraint_indexing:constraint_classification(ergodic_theorems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSURANCE ACTUARY (ROPE)
% For the actuary, assuming ergodicity is a Rope. By pooling millions of
% individuals, the insurer's time average becomes the population's ensemble
% average. It is a powerful coordination mechanism for pricing risk and
% creating financial products, with very low effective extraction from this viewpoint.
constraint_indexing:constraint_classification(ergodic_theorems, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMS ANALYST (TANGLED ROPE)
% The analyst sees the full picture: a system that provides a genuine
% coordination function (for institutions) while simultaneously performing
% asymmetric extraction (on individuals). The active enforcement required to
% maintain the ergodic assumption in policy makes it a classic Tangled Rope.
constraint_indexing:constraint_classification(ergodic_theorems, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE MATHEMATICAL PHYSICIST (MOUNTAIN)
% To the physicist observing the pure mathematical theorem, ergodicity is a
% Mountain. It is a defined property of a dynamical system. A system either
% is or is not ergodic; this is an objective, unchangeable fact. This
% perspective is valid but narrow, as it ignores the socio-economic application.
constraint_indexing:constraint_classification(ergodic_theorems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergodic_theorems_tests).

test(perspectival_gap) :-
    % Verify the core Rope/Snare gap between beneficiary and victim.
    constraint_indexing:constraint_classification(ergodic_theorems, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergodic_theorems, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ergodic_theorems, tangled_rope, context(agent_power(analytical), _, exit_options(analytical), _)).

test(tangled_rope_thresholds) :-
    % Verify the metrics support the Tangled Rope classification.
    domain_priors:base_extractiveness(ergodic_theorems, E),
    domain_priors:suppression_score(ergodic_theorems, S),
    E >= 0.30,
    S >= 0.40.

test(analytical_claim_consistency) :-
    % The constraint_claim must match the primary analytical perspective.
    narrative_ontology:constraint_claim(ergodic_theorems, ClaimType),
    constraint_indexing:constraint_classification(ergodic_theorems, AnalyticalType,
        context(agent_power(analytical), _, exit_options(analytical), _)),
    ClaimType == AnalyticalType.

:- end_tests(ergodic_theorems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this was a Mountain, which conflicted
 *   with its high extraction (0.3) and suppression (0.6) scores. The fix is to
 *   re-center the story on the *misapplication* of the theorem, which is a
 *   Tangled Rope. Base extractiveness was set to 0.35 and suppression to 0.60
 *   to firmly place it in the Tangled Rope category. The pure mathematical
 *   theorem is still represented as a Mountain from a specific, narrow
 *   analytical perspective, but the overall socio-economic constraint is not.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. Institutional aggregators (insurers, pension funds)
 *   leverage the law of large numbers to create a Rope, a coordination tool
 *   that makes risk manageable. For path-dependent individuals, this same logic
 *   is a Snare, as a single "ruin" event terminates their path, making the
 *   ensemble average a dangerous fiction. The analyst sees both functions
 *   operating simultaneously, hence Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `institutional_aggregators` benefit from simplified models
 *     that allow risk-pooling and product creation. Their `arbitrage` exit
 *     option gives them a low directionality `d`, leading to a Rope classification.
 *   - Victim: `path_dependent_individuals` bear the uncompensated risk of ruin.
 *     Their `trapped` exit status gives them a high `d`, leading to a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves a critical Mandatrophy: the mislabeling of a socially
 *   constructed Snare as a natural Mountain. By framing the constraint as the
 *   *misapplication* of the theorem, we correctly identify the source of
 *   extraction and suppression. The pure theorem is a Mountain, but the policy
 *   derived from its misuse is a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ergodic_theorems,
    'Is the widespread assumption of ergodicity in economics a result of mathematical convenience or a deliberate suppression of path-dependent risk models?',
    'Historical analysis of economic curricula and financial regulation debates.',
    'If convenience (Tangled Rope), reform is possible. If deliberate (Snare), it implies a more deeply entrenched extractive structure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ergodic_theorems, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.35) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The assumption of ergodicity serves as a standard for building economic and
% financial models, simplifying complex realities into tractable forms.
narrative_ontology:coordination_type(ergodic_theorems, information_standard).

% DUAL FORMULATION NOTE:
% This constraint could be decomposed into two stories:
%   - ergodic_theorem_math (ε=0.05, Mountain): The pure mathematical statement.
%   - ergodic_assumption_policy (ε=0.35, Tangled Rope): The socio-economic
%     application of the theorem in domains where it does not hold.
% This file models the second, more complex constraint.
% narrative_ontology:affects_constraint(ergodic_theorem_math, ergodic_theorems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations and exit options correctly models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */