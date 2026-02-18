% ============================================================================
% CONSTRAINT STORY: mvt_theorem_constraint
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_mvt_theorem_constraint, []).

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
 *   constraint_id: mvt_theorem_constraint
 *   human_readable: Application of the Mean Value Theorem
 *   domain: technological
 *
 * SUMMARY:
 *   The application of the Mean Value Theorem in specific scenarios (e.g., calculating speed from distance and time) can act as a constraint on possible outcomes. The theorem guarantees a specific relationship but can be misused or misinterpreted, leading to incorrect conclusions.
 *
 * KEY AGENTS (by structural relationship):
 *   - Students: Primary target (powerless/trapped) — bears extraction in the form of potential misapplication and errors.
 *   - Educators: Primary beneficiary (powerful/mobile) — benefits from using the theorem as a tool for teaching and assessment.
 *   - Software developers: Secondary actor (moderate/mobile) — may benefit by leveraging MVT, but can also contribute to its misuse.
 *   - Analysts: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mvt_theorem_constraint, 0.10).
domain_priors:suppression_score(mvt_theorem_constraint, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(mvt_theorem_constraint, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mvt_theorem_constraint, extractiveness, 0.10).
narrative_ontology:constraint_metric(mvt_theorem_constraint, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(mvt_theorem_constraint, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
narrative_ontology:constraint_metric(mvt_theorem_constraint, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(mvt_theorem_constraint, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(mvt_theorem_constraint, rope).
narrative_ontology:human_readable(mvt_theorem_constraint, "Application of the Mean Value Theorem").
narrative_ontology:topic_domain(mvt_theorem_constraint, "technological").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(mvt_theorem_constraint).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(mvt_theorem_constraint). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
domain_priors:emerges_naturally(mvt_theorem_constraint).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(mvt_theorem_constraint, educators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(mvt_theorem_constraint, students).
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
constraint_indexing:constraint_classification(mvt_theorem_constraint, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(mvt_theorem_constraint, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(mvt_theorem_constraint, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mvt_theorem_constraint_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(mvt_theorem_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mvt_theorem_constraint, TypeBeneficiary, context(agent_power(powerful), _, _, _)),
    TypeTarget = TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(mvt_theorem_constraint, ExtMetricName, E),
    E =< 0.25. % Mountain or low-extraction

:- end_tests(mvt_theorem_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Mean Value Theorem in its purest form is a mathematical truth, thus a Mountain. However, its application introduces potential errors, incorrect assumptions, or misuse, especially in contexts involving modeling real-world phenomena. Therefore, it is viewed as a 'Rope' because its proper use facilitates coordination but requires understanding and skill. The base extractiveness and suppression scores are relatively low because the potential negative impact is limited compared to other real-world constraints.
 *
 * PERSPECTIVAL GAP:
 *   The target (students) and beneficiaries (educators) essentially agree on the type of constraint since students see the utility of MVT as a tool to solve complex math problems.
 *
 * DIRECTIONALITY LOGIC:
 *   Educators benefit from having this theorem in their toolkit to teach calculus and analyze situations. Students bear the cost of potential misapplication, which can lead to errors in their work and understanding.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the theorem's use as pure extraction because it acknowledges the theorem's inherent value in providing accurate results when applied correctly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_mvt,
    'How frequently is the Mean Value Theorem misapplied in real-world contexts?',
    'Empirical studies of its use in physics, economics, and engineering.',
    'If often misapplied, it leans towards Snare; if rarely, remains Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(mvt_theorem_constraint, 0, 10).

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
narrative_ontology:measurement(mvt_theorem_constraint_tr_t0, mvt_theorem_constraint, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mvt_theorem_constraint_tr_t5, mvt_theorem_constraint, theater_ratio, 5, 0.10).
narrative_ontology:measurement(mvt_theorem_constraint_tr_t10, mvt_theorem_constraint, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(mvt_theorem_constraint_ex_t0, mvt_theorem_constraint, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mvt_theorem_constraint_ex_t5, mvt_theorem_constraint, base_extractiveness, 5, 0.10).
narrative_ontology:measurement(mvt_theorem_constraint_ex_t10, mvt_theorem_constraint, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(mvt_theorem_constraint, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(mvt_theorem_constraint, 0.05).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(mvt_theorem_constraint, [other_constraint_id]).

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
% constraint_indexing:directionality_override(mvt_theorem_constraint, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */