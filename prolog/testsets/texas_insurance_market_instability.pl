% ============================================================================
% CONSTRAINT STORY: texas_insurance_market_instability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-25
% ============================================================================

:- module(constraint_texas_insurance_market_instability, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: texas_insurance_market_instability
 *   human_readable: Texas Insurance Market Instability
 *   domain: economic
 *
 * SUMMARY:
 *   The Texas insurance market, particularly for homeowners, is facing instability due to increasing natural disasters, regulatory constraints, and economic pressures. This situation results in rising premiums, reduced coverage availability, and increased risk for homeowners.
 *
 * KEY AGENTS (by structural relationship):
 *   - Texas Homeowners: Primary target (powerless/trapped) — bears extraction
 *   - Insurance Companies: Primary beneficiary (institutional/arbitrage) — benefits from constraint by raising premiums
 *   - Texas State Government/Insurance Regulators: Secondary actor (institutional/constrained)
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(texas_insurance_market_instability, 0.55).
domain_priors:suppression_score(texas_insurance_market_instability, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(texas_insurance_market_instability, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(texas_insurance_market_instability, extractiveness, 0.55).
narrative_ontology:constraint_metric(texas_insurance_market_instability, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(texas_insurance_market_instability, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(texas_insurance_market_instability, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(texas_insurance_market_instability, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(texas_insurance_market_instability, tangled_rope).
narrative_ontology:human_readable(texas_insurance_market_instability, "Texas Insurance Market Instability").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(texas_insurance_market_instability).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(texas_insurance_market_instability). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(texas_insurance_market_instability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(texas_insurance_market_instability, insurance_companies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(texas_insurance_market_instability, texas_homeowners).
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
constraint_indexing:constraint_classification(texas_insurance_market_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(texas_insurance_market_instability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(texas_insurance_market_instability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.

% Perspective 4A: Texas State Government/Insurance Regulators
%                   (institutional, constrained exit - political pressure)
constraint_indexing:constraint_classification(texas_insurance_market_instability, rope,
     context(agent_power(institutional),
             time_horizon(generational),
             exit_options(constrained),
             spatial_scope(national))).

% Perspective 4B: Insurance Companies (institutional, arbitrage exit)
% This is redundant with perspective 2, but explicitly stated for clarity
constraint_indexing:constraint_classification(texas_insurance_market_instability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, χ ≤ 0.30, theater ≤ 0.70.
% constraint_indexing:constraint_classification(texas_insurance_market_instability, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(texas_insurance_market_instability).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(texas_insurance_market_instability_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(texas_insurance_market_instability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(texas_insurance_market_instability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(texas_insurance_market_instability, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(texas_insurance_market_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set at 0.55 due to the observed increase in premiums and reduced coverage options, indicating a significant transfer of wealth from homeowners to insurance companies. The suppression score is high (0.70) because homeowners have limited alternatives, especially in high-risk areas. The theater ratio is low (0.30) as the primary function of insurance (risk transfer) is still happening, although with increasing friction and cost. This low theater ratio means the constraint is not a Piton.
 *
 * PERSPECTIVAL GAP:
 *   Homeowners perceive this as a Snare because they are forced to pay higher premiums for less coverage or face the risk of being uninsured. Insurance companies see it as a Rope because they are coordinating risk transfer within a market facing increased volatility. The analytical observer sees it as a Tangled Rope because there is both a coordination function (risk transfer) and asymmetric extraction (increased premiums outweighing increased risk for the insurance companies due to regulatory constraints and market dynamics).
 *
 * DIRECTIONALITY LOGIC:
 *   Insurance companies benefit from the increased premiums and reduced coverage options, making them beneficiaries. Homeowners bear the cost of higher premiums and increased risk, making them victims. The Texas State Government/Insurance Regulators attempt to balance the needs of both groups, making them a secondary actor with a constrained exit option.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   The Texas State Government/Insurance Regulators aim to ensure a stable insurance market but are constrained by political pressures, economic realities, and the increasing frequency of natural disasters. Insurance companies seek to maximize profits while managing risk, leading to higher premiums and reduced coverage options. These different objectives create an inter-institutional tension that contributes to market instability.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this situation as pure extraction (Snare). While extraction exists (higher premiums), there is also a genuine coordination function (risk transfer). It also prevents mislabeling it as pure coordination (Rope) because there's a significant asymmetric extraction that disproportionately burdens homeowners.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_texas_insurance_market_instability,
    'Will the frequency and intensity of natural disasters in Texas continue to increase?',
    'Long-term climate modeling and disaster trend analysis',
    'If True: Insurance market instability will worsen. If False: Market might stabilize but underlying issues remain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(texas_insurance_market_instability, 0, 10).

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
narrative_ontology:measurement(texas_insurance_market_instability_tr_t0, texas_insurance_market_instability, theater_ratio, 0, 0.20).
narrative_ontology:measurement(texas_insurance_market_instability_tr_t5, texas_insurance_market_instability, theater_ratio, 5, 0.25).
narrative_ontology:measurement(texas_insurance_market_instability_tr_t10, texas_insurance_market_instability, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(texas_insurance_market_instability_ex_t0, texas_insurance_market_instability, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(texas_insurance_market_instability_ex_t5, texas_insurance_market_instability, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(texas_insurance_market_instability_ex_t10, texas_insurance_market_instability, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(texas_insurance_market_instability, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(texas_insurance_market_instability, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(texas_insurance_market_instability, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(texas_insurance_market_instability, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(texas_insurance_market_instability, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */