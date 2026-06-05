% ============================================================================
% CONSTRAINT STORY: three_body_problem_predictability_limit
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_three_body_problem_predictability_limit, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: three_body_problem_predictability_limit
 *   human_readable: Predictability Limit in the Three-Body Problem
 *   domain: technological
 *
 * SUMMARY:
 *   The three-body problem is a fundamental limit on predictability in dynamical systems.  Given the initial positions and velocities of three celestial bodies interacting via gravity, it is generally impossible to predict their motion for arbitrarily long times. This constraint arises from the chaotic nature of the system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Deterministic Prediction Algorithms: Primary target (powerless/trapped) — suffers from the limitations in predictability.
 *   - Chaos Theory Research: Primary beneficiary (institutional/arbitrage) — benefits from increased interest and funding.
 *   - Applied Physics and Engineering: Secondary actor (moderate/constrained) – affected by the limitations when designing stable systems.
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees the full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(three_body_problem_predictability_limit, 0.15).
domain_priors:suppression_score(three_body_problem_predictability_limit, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(three_body_problem_predictability_limit, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(three_body_problem_predictability_limit, extractiveness, 0.15).
narrative_ontology:constraint_metric(three_body_problem_predictability_limit, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(three_body_problem_predictability_limit, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.

narrative_ontology:constraint_metric(three_body_problem_predictability_limit, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(three_body_problem_predictability_limit, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(three_body_problem_predictability_limit, mountain).
narrative_ontology:human_readable(three_body_problem_predictability_limit, "Predictability Limit in the Three-Body Problem").
narrative_ontology:topic_domain(three_body_problem_predictability_limit, "technological").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(three_body_problem_predictability_limit).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(three_body_problem_predictability_limit). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.

domain_priors:emerges_naturally(three_body_problem_predictability_limit).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(three_body_problem_predictability_limit, chaos_theory_research).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(three_body_problem_predictability_limit, deterministic_prediction_algorithms).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(three_body_problem_predictability_limit, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(three_body_problem_predictability_limit, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(three_body_problem_predictability_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.
%
% Example — Regulatory capture:
%
% % Perspective 4A: Captured regulator (institutional, constrained exit)
% constraint_indexing:constraint_classification(three_body_problem_predictability_limit, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(national))).
%
% % Perspective 4B: Regulated company (institutional, arbitrage exit)
% constraint_indexing:constraint_classification(three_body_problem_predictability_limit, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(arbitrage),
%             spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(three_body_problem_predictability_limit_tests).

test(perspectival_agreement) :-
    % Verify perspectival agreement as Mountain.
    constraint_indexing:constraint_classification(three_body_problem_predictability_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(three_body_problem_predictability_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(three_body_problem_predictability_limit, ExtMetricName, E),
    E =< 0.25. % Mountain extraction must be low.

test(natural_law_properties) :-
    narrative_ontology:constraint_metric(three_body_problem_predictability_limit, accessibility_collapse, A),
    narrative_ontology:constraint_metric(three_body_problem_predictability_limit, resistance, R),
    A >= 0.85,
    R =< 0.15,
    domain_priors:emerges_naturally(three_body_problem_predictability_limit).

:- end_tests(three_body_problem_predictability_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The three-body problem is an inherent limitation on our ability to predict the future state of a system. It's base extractiveness is low as the limitation doesn't directly extract resources, but it does limit deterministic predictive power. Suppression is also low because alternative approaches, such as statistical mechanics, exist to deal with the problem. The theater ratio is low, as the limitation is factual and not obscured by theatrical compliance.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap, as this is considered a fundamental property regardless of perspective. All agents acknowledge the limitation.
 *
 * DIRECTIONALITY LOGIC:
 *   Chaos theory research benefits from increased study and funding because of the problem. Deterministic prediction algorithms are limited by the problem. The analytical observer sees the problem for what it is.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   N/A
 *
 * MANDATROPHY ANALYSIS:
 *   The classification of the three-body problem as a mountain prevents the mislabeling of this fundamental limitation as pure extraction, where predictability could be seen as something actively suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_three_body_problem,
    'Will advances in computation lead to more accurate approximation techniques for longer time scales?',
    'Improved computational methods and algorithms.',
    'If True: Extended predictability range. If False: Limit remains fundamentally unchanged.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(three_body_problem_predictability_limit, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(three_body_problem_predictability_limit_tr_t0, three_body_problem_predictability_limit, theater_ratio, 0, 0.10).
narrative_ontology:measurement(three_body_problem_predictability_limit_tr_t5, three_body_problem_predictability_limit, theater_ratio, 5, 0.10).
narrative_ontology:measurement(three_body_problem_predictability_limit_tr_t10, three_body_problem_predictability_limit, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(three_body_problem_predictability_limit_ex_t0, three_body_problem_predictability_limit, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(three_body_problem_predictability_limit_ex_t5, three_body_problem_predictability_limit, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(three_body_problem_predictability_limit_ex_t10, three_body_problem_predictability_limit, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(three_body_problem_predictability_limit, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
narrative_ontology:boltzmann_floor_override(three_body_problem_predictability_limit, 0.10).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(three_body_problem_predictability_limit, other_constraint_id).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(three_body_problem_predictability_limit, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */