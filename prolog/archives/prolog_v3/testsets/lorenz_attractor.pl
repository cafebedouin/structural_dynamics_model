% ============================================================================
% CONSTRAINT STORY: constraint_lorenz_sensitivity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-27
% ============================================================================

:- module(constraint_lorenz_sensitivity, []).

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
 *   constraint_id: constraint_lorenz_sensitivity
 *   human_readable: Sensitivity to Initial Conditions (Lorenz Attractor)
 *   domain: technological
 *
 * SUMMARY:
 *   The Lorenz Attractor exemplifies sensitivity to initial conditions in deterministic nonlinear systems.  Tiny differences in initial conditions lead to drastically different outcomes over time, making long-term prediction impossible despite the underlying system being fully deterministic. This constraint limits the precision of forecasting complex systems.
 *
 * KEY AGENTS (by structural relationship):
 *   - Weather Forecasters: Primary target (moderate/constrained) — bears extraction (prediction failures)
 *   - Algorithm Developers: Primary beneficiary (powerful/analytical) — benefits from developing tools to model chaos, even if imperfectly.
 *   - Public: Secondary actor (moderate/mobile) - impacted by forecasting inaccuracies
 *   - Theoretical Physicists: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constraint_lorenz_sensitivity, 0.15).
domain_priors:suppression_score(constraint_lorenz_sensitivity, 0.03).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(constraint_lorenz_sensitivity, 0.01).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constraint_lorenz_sensitivity, extractiveness, 0.15).
narrative_ontology:constraint_metric(constraint_lorenz_sensitivity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(constraint_lorenz_sensitivity, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.

narrative_ontology:constraint_metric(constraint_lorenz_sensitivity, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(constraint_lorenz_sensitivity, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(constraint_lorenz_sensitivity, mountain).
narrative_ontology:human_readable(constraint_lorenz_sensitivity, "Sensitivity to Initial Conditions (Lorenz Attractor)").
narrative_ontology:topic_domain(constraint_lorenz_sensitivity, "technological").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(constraint_lorenz_sensitivity).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(constraint_lorenz_sensitivity). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.

domain_priors:emerges_naturally(constraint_lorenz_sensitivity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(constraint_lorenz_sensitivity, algorithm_developers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(constraint_lorenz_sensitivity, weather_forecasters).
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
constraint_indexing:constraint_classification(constraint_lorenz_sensitivity, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(constraint_lorenz_sensitivity, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(constraint_lorenz_sensitivity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_lorenz_sensitivity_tests).

test(perspectival_alignment) :-
    % Verify that all perspectives agree on the Mountain classification.
    constraint_indexing:constraint_classification(constraint_lorenz_sensitivity, Type1, context(_, _, _, _)),
    constraint_indexing:constraint_classification(constraint_lorenz_sensitivity, Type2, context(_, _, _, _)),
    constraint_indexing:constraint_classification(constraint_lorenz_sensitivity, Type3, context(_, _, _, _)),

    Type1 = mountain,
    Type2 = mountain,
    Type3 = mountain.


test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constraint_lorenz_sensitivity, ExtMetricName, E),
    E =< 0.25. % Check for Mountain classification


:- end_tests(constraint_lorenz_sensitivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Lorenz Attractor is considered a mountain constraint because sensitivity to initial conditions is a fundamental property of chaotic systems, and places an inherent limit on predictability. This is difficult to overcome through computational means, given the inherent limitations in representation of reality.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap, because the fundamental sensitivity to initial conditions affects all parties, though the burden is borne more acutely by those who rely on forecasting.
 *
 * DIRECTIONALITY LOGIC:
 *   Algorithm developers benefit as they have a field of study to explore, and tools and techniques can be developed to mitigate the impact, even if they cannot be entirely eliminated. Weather forecasters are victimized by this constraint, as it limits the potential for perfect weather prediction.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Not applicable here.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification of this constraint as a mountain, based on metrics reflecting inherent limitations and resistance to change, prevents it from being mislabeled as a snare. A snare would imply that the "extraction" of poor weather predictions is an intentional design or result of exploitative relationships. It also prevents the classification from being mislabeled as pure coordination, because pure coordination lacks extraction. The fundamental limitation of predictability prevents this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_lorenz,
    'Can computational power or novel algorithms overcome the inherent sensitivity to initial conditions?',
    'Develop more efficient computing and more complex models.',
    'True: Weather prediction becomes significantly more accurate. False: Limits to prediction remain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(constraint_lorenz_sensitivity, 0, 10).

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
narrative_ontology:measurement(constraint_lorenz_sensitivity_tr_t0, constraint_lorenz_sensitivity, theater_ratio, 0, 0.01).
narrative_ontology:measurement(constraint_lorenz_sensitivity_tr_t5, constraint_lorenz_sensitivity, theater_ratio, 5, 0.01).
narrative_ontology:measurement(constraint_lorenz_sensitivity_tr_t10, constraint_lorenz_sensitivity, theater_ratio, 10, 0.01).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(constraint_lorenz_sensitivity_ex_t0, constraint_lorenz_sensitivity, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(constraint_lorenz_sensitivity_ex_t5, constraint_lorenz_sensitivity, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(constraint_lorenz_sensitivity_ex_t10, constraint_lorenz_sensitivity, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(constraint_lorenz_sensitivity, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(constraint_lorenz_sensitivity, 0.05).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(constraint_lorenz_sensitivity, [other_constraint_id]).

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
% constraint_indexing:directionality_override(constraint_lorenz_sensitivity, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */