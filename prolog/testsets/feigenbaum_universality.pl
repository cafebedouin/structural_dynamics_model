% ============================================================================
% CONSTRAINT STORY: feigenbaum_universality
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_feigenbaum_universality, []).

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
    domain_priors:emerges_naturally/1,
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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feigenbaum_universality
 *   human_readable: Feigenbaum Constants (Universality in Chaos)
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   The Feigenbaum constants (e.g., δ ≈ 4.669) describe universal scaling
 *   properties for systems exhibiting period-doubling bifurcations on their
 *   route to chaos. This mathematical law dictates that the geometry of this
 *   transition is identical across disparate physical systems (e.g., fluid
 *   dynamics, electronic circuits, population models), regardless of their
 *   specific underlying equations. It functions as a fixed, unchangeable
 *   feature of a certain class of nonlinear dynamics.
 *
 * KEY AGENTS (by structural relationship):
 *   - The System State (e.g., a variable in a logistic map): Primary target (powerless/trapped) — its evolution is dictated by the constant.
 *   - The Control Engineer: Observer/User (institutional/mobile) — uses knowledge of the constant to predict and avoid chaotic regimes.
 *   - The Theoretical Physicist: Analytical observer — studies the constant as a fundamental law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a fundamental mathematical constant, it has near-zero
% structural extraction or suppression. It doesn't extract value; it describes
% an inherent property of a system's state space. The previous values (0.3/0.2)
% were based on a metaphorical interpretation of "extracting uniqueness" which
% is not a structural property. These values are corrected to meet the Mountain
% classification thresholds.
domain_priors:base_extractiveness(feigenbaum_universality, 0.05).
domain_priors:suppression_score(feigenbaum_universality, 0.01).
domain_priors:theater_ratio(feigenbaum_universality, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feigenbaum_universality, extractiveness, 0.05).
narrative_ontology:constraint_metric(feigenbaum_universality, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(feigenbaum_universality, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. A high accessibility_collapse score (>=0.85)
% indicates alternatives are structurally foreclosed. A low resistance
% score (<=0.15) indicates no meaningful opposition exists.
narrative_ontology:constraint_metric(feigenbaum_universality, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(feigenbaum_universality, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(feigenbaum_universality, mountain).

% --- Binary flags ---
% No active enforcement is required for a mathematical law.

% --- Emergence flag (required for mountain constraints) ---
% This flag is required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(feigenbaum_universality).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a uniform-type Mountain (a law of nature), this constraint does not have
% structural beneficiaries or victims in the sense of coordination or extraction.
% Its utility to engineers is an application of knowledge about the Mountain,
% not a coordination function provided by it. No enrichment needed.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because the base metrics (ε, suppression)
% are extremely low, forcing a Mountain classification regardless of the
% scaling factors f(d) and σ(S). The previous Rope/Snare classifications were
% based on metaphorical interpretations that are structurally incorrect.

% PERSPECTIVE 1: THE SYSTEM STATE (MOUNTAIN)
% For the mathematical state variable, the scaling constant is an absolute,
% unchangeable feature of the nonlinear landscape. It has no agency.
constraint_indexing:constraint_classification(feigenbaum_universality, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CONTROL ENGINEER (MOUNTAIN)
% For an engineer, the constant is not a coordination tool (Rope) but an
% unchangeable law of nature (Mountain) that they must design around.
% Their ability to use this knowledge does not change the nature of the constraint itself.
constraint_indexing:constraint_classification(feigenbaum_universality, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context correctly identifies the constant as a
% fundamental, unchangeable feature of mathematics.
constraint_indexing:constraint_classification(feigenbaum_universality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feigenbaum_universality_tests).

test(classification_invariance, [nondet]) :-
    % Verify that the constraint is a uniform-type Mountain from multiple key perspectives.
    constraint_indexing:constraint_classification(feigenbaum_universality, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feigenbaum_universality, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(feigenbaum_universality, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(mountain_threshold_adherence) :-
    % Verify the metrics are consistent with the Mountain classification.
    narrative_ontology:constraint_metric(feigenbaum_universality, extractiveness, E),
    narrative_ontology:constraint_metric(feigenbaum_universality, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(feigenbaum_universality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This file was regenerated to correct a MOUNTAIN_METRIC_CONFLICT and a
 *   MISSING_NL_PROFILE error. The original file claimed the constraint was a
 *   Mountain but assigned base_extractiveness (0.3) and suppression_score (0.2)
 *   that violated the strict Mountain thresholds (ε ≤ 0.25, suppression ≤ 0.05).
 *   It also lacked the required Natural Law profile metrics (accessibility_collapse,
 *   resistance) and the emerges_naturally flag, which are mandatory for a
 *   Mountain to pass the linter and the engine's certification chain.
 *
 *   The metrics have been corrected to ε=0.05 and suppression=0.01, and the
 *   full NL profile has been added, reflecting the structural reality that a
 *   mathematical law does not perform extraction or require active suppression.
 *   It is a feature of the landscape.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a uniform-type Mountain. The previous
 *   classifications of Rope (for an engineer) and Snare (for a simulation)
 *   were metaphorical. An engineer *uses* knowledge of the Mountain; the
 *   constraint itself is not a coordination Rope. A simulation hits a
 *   computational limit *because of* the Mountain's properties; the constraint
 *   is not an extractive Snare. The classification is invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, beneficiary and victim declarations are not applicable.
 *   The concept of directionality (d) does not meaningfully alter the
 *   classification due to the extremely low base extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   By correctly classifying this as a Mountain, we avoid mislabeling a
 *   fundamental law of nature as a social or economic construct like a Rope
 *   or Snare. This upholds the principle that the classification system
 *   describes the constraint's structure, not just how agents interact with it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_feigenbaum_universality,
    'Is the Feigenbaum constant a true universal, or does it fail for systems with non-quadratic extrema?',
    'Numerical audits of bifurcation rates in maps with different critical orders (e.g., |x|^z where z is not 2).',
    'If it fails, its classification would shift from a universal Mountain to a more context-dependent constraint. If it holds, its status as a fundamental Mountain is reinforced.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(feigenbaum_universality, 1975, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.05) is below the 0.46 threshold for
% mandatory temporal tracking. As a mathematical constant, its properties
% do not drift over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No coordination type, floor override, or network relationships are defined
% for this fundamental mathematical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The classification is insensitive to directionality
% due to the low base metrics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */