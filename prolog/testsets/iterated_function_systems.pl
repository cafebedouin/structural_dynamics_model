% ============================================================================
% CONSTRAINT STORY: iterated_function_system_convergence
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-28
% ============================================================================

:- module(constraint_iterated_function_system_convergence, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iterated_function_system_convergence
 *   human_readable: IFS Convergence and Computational Resources
 *   domain: technological
 *
 * SUMMARY:
 *   Iterated Function Systems (IFS) provide a method for generating fractals by repeatedly applying a set of affine transformations.  A constraint arises from the computational resources required to generate a visually complex fractal to a high degree of convergence. The limitation in processing power, memory, and time to achieve the desired result can be a barrier to certain applications.
 *
 * KEY AGENTS (by structural relationship):
 *   - Hobbyists/Small Researchers: Primary target (powerless/trapped) — Limited access to computing resources.
 *   - High-Budget Research Labs/Organizations: Primary beneficiary (institutional/arbitrage) — Possess powerful computing resources.
 *   - Algorithm Developers: Secondary actor (moderate/mobile) — Create and optimize IFS algorithms.
 *   - Analytical observer: Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iterated_function_system_convergence, 0.42).
domain_priors:suppression_score(iterated_function_system_convergence, 0.45).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(iterated_function_system_convergence, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iterated_function_system_convergence, extractiveness, 0.42).
narrative_ontology:constraint_metric(iterated_function_system_convergence, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(iterated_function_system_convergence, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(iterated_function_system_convergence, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(iterated_function_system_convergence, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(iterated_function_system_convergence, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(iterated_function_system_convergence).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(iterated_function_system_convergence). % Required for Tangled Rope
domain_priors:requires_active_enforcement(iterated_function_system_convergence).

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(iterated_function_system_convergence).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(iterated_function_system_convergence, high_budget_research).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(iterated_function_system_convergence, limited_resource_researchers).
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
constraint_indexing:constraint_classification(iterated_function_system_convergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(iterated_function_system_convergence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(iterated_function_system_convergence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iterated_function_system_convergence_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(iterated_function_system_convergence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iterated_function_system_convergence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(iterated_function_system_convergence, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(iterated_function_system_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores are assigned based on the fact that while IFS algorithms themselves are not inherently extractive or coercive, the limited access to computing resources poses a constraint on those with fewer resources, preventing them from generating high-resolution fractals. This creates a form of asymmetric extraction since the high-budget researchers benefit due to the computational resources at their disposal.
 *
 * PERSPECTIVAL GAP:
 *   The target, a powerless researcher with limited computing resources, sees the constraint as a snare since it directly limits their ability to explore complex fractals. The beneficiary, a high-budget research lab, sees this as a rope because they have access to the resources required for IFS and have no restrictions due to a lack of computing resources.
 *
 * DIRECTIONALITY LOGIC:
 *   High-budget research benefits from the relative scarcity of advanced computing power. Limited resource researchers bear the cost in the form of being excluded from detailed IFS simulation. Algorithm developers benefit indirectly from creating efficiencies, enabling wider use of IFS.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   N/A
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a tangled rope prevents it from being mislabeled as pure extraction because algorithm development serves a coordination function. It also prevents it from being mislabeled as a pure coordination mechanism because resources are required to benefit from this constraint, and a party may lack the resources to benefit from this constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ifs,
    'How rapidly will consumer-grade computing power advance relative to IFS algorithm complexity?',
    'Tracking the exponential growth curves of both factors.',
    'If computing power outpaces algorithm complexity, the constraint weakens; if not, it persists or intensifies.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(iterated_function_system_convergence, 0, 10).

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
narrative_ontology:measurement(ifs_tr_t0, iterated_function_system_convergence, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ifs_tr_t5, iterated_function_system_convergence, theater_ratio, 5, 0.10).
narrative_ontology:measurement(ifs_tr_t10, iterated_function_system_convergence, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ifs_ex_t0, iterated_function_system_convergence, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(ifs_ex_t5, iterated_function_system_convergence, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ifs_ex_t10, iterated_function_system_convergence, base_extractiveness, 10, 0.42).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(iterated_function_system_convergence, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(iterated_function_system_convergence, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(iterated_function_system_convergence, [other_constraint_id]).

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
% constraint_indexing:directionality_override(iterated_function_system_convergence, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */