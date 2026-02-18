% ============================================================================
% CONSTRAINT STORY: yc_equity_squeeze
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-28
% ============================================================================

:- module(constraint_yc_equity_squeeze, []).

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
 *   constraint_id: yc_equity_squeeze
 *   human_readable: Y Combinator Standard Equity Terms
 *   domain: economic
 *
 * SUMMARY:
 *   Y Combinator's standard SAFE (Simple Agreement for Future Equity) agreement and equity terms place a constraint on early-stage startups. While providing early funding, the SAFE and equity structure can lead to significant equity dilution for founders, especially if subsequent funding rounds are less favorable. This can disincentivize founders and concentrate control in the hands of YC and later investors.
 *
 * KEY AGENTS (by structural relationship):
 *   - Startup Founders: Primary target (powerless/trapped) — bears equity dilution risk
 *   - Y Combinator: Primary beneficiary (institutional/arbitrage) — benefits from equity upside with downside protection
 *   - Later Stage Investors: Secondary actor (powerful/constrained) — influence the cap table structure
 *   - Analytical Observer: Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(yc_equity_squeeze, 0.48). % Estimate of average equity dilution impact.
domain_priors:suppression_score(yc_equity_squeeze, 0.55).   % Alternatives exist, but are limited by YC's brand/network.
domain_priors:theater_ratio(yc_equity_squeeze, 0.20).       % Relatively low theater. The terms are clear, and the value is in the capital/network.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(yc_equity_squeeze, extractiveness, 0.48).
narrative_ontology:constraint_metric(yc_equity_squeeze, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(yc_equity_squeeze, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(yc_equity_squeeze, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(yc_equity_squeeze, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(yc_equity_squeeze, tangled_rope).
narrative_ontology:human_readable(yc_equity_squeeze, "Y Combinator Standard Equity Terms").
narrative_ontology:topic_domain(yc_equity_squeeze, "economic").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(yc_equity_squeeze).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(yc_equity_squeeze). % Required for Tangled Rope (YC actively manages its deals)

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(yc_equity_squeeze).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(yc_equity_squeeze, yc_and_investors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(yc_equity_squeeze, startup_founders).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (STARTUP FOUNDERS)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(yc_equity_squeeze, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (Y COMBINATOR)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(yc_equity_squeeze, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(yc_equity_squeeze, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: LATER STAGE INVESTORS
% They benefit from favorable deal terms and lower valuation for entry.
constraint_indexing:constraint_classification(yc_equity_squeeze, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained), % Can't easily change the existing cap table
            spatial_scope(national))).

% PERSPECTIVE 5: STARTUPS WITH OTHER FUNDING OPTIONS
% If a startup has alternative funding sources, they may see the YC terms as less of a constraint.
constraint_indexing:constraint_classification(yc_equity_squeeze, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(yc_equity_squeeze_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(yc_equity_squeeze, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(yc_equity_squeeze, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(yc_equity_squeeze, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(yc_equity_squeeze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores reflect the trade-off inherent in YC's funding model. Base extractiveness of 0.48 represents the potential for equity dilution. Suppression score of 0.55 reflects the limitations on alternative funding sources, particularly for early-stage startups seeking the YC network and brand. The low theater ratio reflects the transparency of the SAFE terms.
 *
 * PERSPECTIVAL GAP:
 *   Founders view the terms as a potential snare due to equity dilution and loss of control. YC views it as a rope, facilitating efficient funding and alignment. The analytical perspective sees it as a tangled rope: providing valuable coordination but also extracting value asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   YC benefits from favorable equity terms and access to promising startups. Startup founders bear the cost of potential equity dilution. This leads to the derived directionality values, impacting the Chi calculation. The 'yc_and_investors' group is the beneficiary because later investors also benefit from the lower valuation and favorable terms established by YC.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Later stage investors have some influence on the cap table. However, they benefit from YC having already standardized the initial investment terms, reducing their risk and improving their returns. This creates a layered effect where YC standardizes and investors exploit, further diluting the original founders.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a tangled rope prevents mislabeling the structure as pure extraction. It acknowledges that YC provides coordination (funding, mentorship, network) and isn't solely focused on extracting value. However, it also highlights the asymmetric power dynamic and potential for founders to be disadvantaged. By properly accounting for coordination via `constraint_beneficiary` the `tangled_rope` classification becomes possible, which wouldn't be without the `has_coordination_function` implicit derivative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_yc_exit_success,
    'What percentage of YC-funded companies achieve successful exits, justifying the equity dilution?',
    'Analyze long-term outcomes of YC-funded companies and correlate exit valuations with initial equity terms.',
    'If the success rate is high, the dilution might be justified. If low, it suggests excessive extraction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(yc_equity_squeeze, 0, 10).

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
narrative_ontology:measurement(yc_equity_squeeze_tr_t0, yc_equity_squeeze, theater_ratio, 0, 0.15).
narrative_ontology:measurement(yc_equity_squeeze_tr_t5, yc_equity_squeeze, theater_ratio, 5, 0.18).
narrative_ontology:measurement(yc_equity_squeeze_tr_t10, yc_equity_squeeze, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(yc_equity_squeeze_ex_t0, yc_equity_squeeze, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(yc_equity_squeeze_ex_t5, yc_equity_squeeze, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(yc_equity_squeeze_ex_t10, yc_equity_squeeze, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(yc_equity_squeeze, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(yc_equity_squeeze, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(yc_equity_squeeze, [other_constraint_id]).

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
% constraint_indexing:directionality_override(yc_equity_squeeze, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */