% ============================================================================
% CONSTRAINT STORY: us_debt_ceiling
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_us_debt_ceiling, []).

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
 *   constraint_id: us_debt_ceiling
 *   human_readable: US Debt Ceiling
 *   domain: political
 *
 * SUMMARY:
 *   The US debt ceiling is a legal limit on the total amount of money the
 *   United States federal government is authorized to borrow to meet its
 *   existing legal obligations. Repeated political standoffs over raising
 *   or suspending the debt ceiling have created significant economic
 *   uncertainty and potential for crisis.
 *
 * KEY AGENTS (by structural relationship):
 *   - US Economy: Primary target (powerless/trapped) — suffers from economic disruption
 *   - US Congress: Primary beneficiary (institutional/constrained) — gains political leverage
 *   - Global Financial System: Secondary actor (institutional/constrained) - impacted by US solvency
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_debt_ceiling, 0.50).
domain_priors:suppression_score(us_debt_ceiling, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_debt_ceiling, 0.60).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_debt_ceiling, extractiveness, 0.50).
narrative_ontology:constraint_metric(us_debt_ceiling, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(us_debt_ceiling, theater_ratio, 0.60).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(us_debt_ceiling, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(us_debt_ceiling, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_debt_ceiling, tangled_rope).
narrative_ontology:human_readable(us_debt_ceiling, "US Debt Ceiling").
narrative_ontology:topic_domain(us_debt_ceiling, "political").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(us_debt_ceiling).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(us_debt_ceiling). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(us_debt_ceiling).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_debt_ceiling, us_congress).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_debt_ceiling, us_economy).
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
constraint_indexing:constraint_classification(us_debt_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + constrained exit → d ≈ 0.15 → f(d) ≈ 0.0 → low/negative χ
constraint_indexing:constraint_classification(us_debt_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
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
% constraint_indexing:constraint_classification(us_debt_ceiling, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(national))).
%
% % Perspective 4B: Regulated company (institutional, arbitrage exit)
% constraint_indexing:constraint_classification(us_debt_ceiling, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(arbitrage),
%             spatial_scope(national))).

% PERSPECTIVE 5: The Global Financial System (Institutional, Constrained)
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: Defaulting creditor perspective (powerless/trapped)
constraint_indexing:constraint_classification(us_debt_ceiling, snare,
context(agent_power(powerless),
time_horizon(biographical),
exit_options(trapped),
spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_debt_ceiling_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(us_debt_ceiling, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_debt_ceiling, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_debt_ceiling, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(us_debt_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The US debt ceiling serves as a mechanism for political leverage. While
 *   it is ostensibly meant to ensure fiscal responsibility, it is often used
 *   as a bargaining chip in political negotiations, leading to economic
 *   uncertainty. The extractiveness score reflects the potential economic
 *   harm caused by debt ceiling crises. The suppression score indicates the
 *   limited alternatives for managing the debt and the political constraints
 *   on raising or suspending the ceiling. The theater ratio recognizes the
 *   performative aspects of the debt ceiling debates, where grandstanding and
 *   political messaging often overshadow substantive policy discussions.
 *
 * PERSPECTIVAL GAP:
 *   The US economy perceives the debt ceiling as a snare because it faces
 *   economic disruption and potential default if the ceiling is not raised.
 *   The US Congress, on the other hand, perceives the debt ceiling as a
 *   rope because it provides them with political leverage and a means to
 *   influence fiscal policy.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Congress benefits from the debt ceiling by using it as a tool
 *   to exert political pressure and influence policy decisions. The US
 *   economy bears the costs of the debt ceiling in the form of economic
 *   uncertainty, potential default, and higher borrowing costs. The
 *   beneficiary declaration maps to the structural relationship because
 *   Congress holds the power to raise or suspend the debt ceiling, while
 *   the economy is subject to the consequences of their decisions.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   The Global Financial System, as a secondary actor, is constrained by its
 *   reliance on the stability of the US economy and its exposure to US debt.
 *   It has limited exit options as a consequence of the interconnectedness
 *   of the global financial system.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification of the US debt ceiling as a tangled rope prevents
 *   mislabeling it as pure extraction because it does have a coordination
 *   function in the form of fiscal oversight, even though it is often
 *   exploited for political gain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_debt_ceiling,
    'To what extent is the US debt ceiling used as a genuine tool for fiscal responsibility versus a tool for political leverage?',
    'Empirical analysis of legislative debates and economic outcomes following debt ceiling crises.',
    'If a tool for fiscal responsibility, it could be a rope. If a tool for political leverage, it is a tangled rope or snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_debt_ceiling, 0, 10).

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
narrative_ontology:measurement(us_debt_ceiling_tr_t0, us_debt_ceiling, theater_ratio, 0, 0.4).
narrative_ontology:measurement(us_debt_ceiling_tr_t5, us_debt_ceiling, theater_ratio, 5, 0.5).
narrative_ontology:measurement(us_debt_ceiling_tr_t10, us_debt_ceiling, theater_ratio, 10, 0.6).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(us_debt_ceiling_ex_t0, us_debt_ceiling, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_debt_ceiling_ex_t5, us_debt_ceiling, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(us_debt_ceiling_ex_t10, us_debt_ceiling, base_extractiveness, 10, 0.5).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(us_debt_ceiling, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(us_debt_ceiling, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(us_debt_ceiling, [other_constraint_id]).

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
% constraint_indexing:directionality_override(us_debt_ceiling, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */