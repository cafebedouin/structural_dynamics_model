% ============================================================================
% DRL CORE - INDEXICAL CONSTRAINT CLASSIFICATION v4.0
% ============================================================================
% This module implements context-indexed Deferential Realism classification.
% Every Mountain/Rope/Snare judgment is INDEXED to WHO/WHEN/WHERE/HOW.
%
% BREAKING CHANGES FROM v3.x:
% - dr_type/2 now defaults to analytical context (backward compatible)
% - dr_type/3 is PRIMARY API (adds Context parameter)
% - All classification uses power-scaled extractiveness
% - Structural signatures integrated with indexical logic
% - Action routing context-aware
%
% Integration: Load after constraint_indexing.pl, before drl_modal_logic.pl
% ============================================================================

:- module(drl_core, [
    % PRIMARY API - Context-Indexed Classification
    dr_type/3,                      % dr_type(Constraint, Context, Type)
    dr_type/2,                      % Backward compat: uses default context
    
    % Action Routing (Indexed)
    dr_action/3,                    % dr_action(Constraint, Context, Action)
    dr_action/2,                    % Backward compat
    
    % Error Detection (Indexed)
    dr_mismatch/4,                  % dr_mismatch(C, Context, ErrorType, Severity)
    dr_mismatch/3,                  % Backward compat
    
    % Structural Signature Integration
    dr_signature/2,                 % dr_signature(Constraint, Signature)
    
    % Re-exported from constraint_indexing
    constraint_classification/3,    % Multifile hook for data files
    constraint_claim_indexed/2,     % Legacy wrapper
    multi_index_report/1,
    compare_perspectives/2,
    discover_my_context/1,

    % Centralize module references
    base_extractiveness/2,
    suppression_score/2,
    requires_active_enforcement/1,
    emerges_naturally/1,
    
    % Exposed helpers for modal_logic and testing
    is_mountain/3,                  % Indexed version
    is_rope/3,
    is_snare/3,
    is_tangled_rope/3,
    is_scaffold/3,
    is_piton/3,
    calculate_scaled_metric/5,
    get_raw_suppression/2,          % Raw suppression (no temporal/scope scaling)

    % Gate precondition for natural laws
    natural_law_without_beneficiary/1,

    % Shared classification (Single Source of Truth)
    classify_from_metrics/6,        % classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type)

    % Reform threshold (minimum power to change a snare)
    snare_reform_threshold/2        % snare_reform_threshold(Context, PowerLevel)
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(structural_signatures).
:- use_module(constraint_indexing).
:- use_module(constraint_instances).
:- use_module(domain_priors).

% Declare these as multifile to allow other modules to contribute data
:- multifile 
    base_extractiveness/2, 
    suppression_score/2, 
    requires_active_enforcement/1, 
    emerges_naturally/1.

% Delegate to domain_priors as the canonical source of truth.
% Multifile declarations above allow testsets to contribute directly,
% but v3.4 testsets assert into domain_priors: namespace, so we bridge here.
% Delegate to narrative_ontology for dynamic metric retrieval
base_extractiveness(C, V) :- 
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(C, ExtMetricName, V).
suppression_score(C, V) :- 
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(C, SuppMetricName, V).
requires_active_enforcement(C) :- domain_priors:requires_active_enforcement(C).
emerges_naturally(C) :- domain_priors:emerges_naturally(C).

% ============================================================================
% SCALING LAW - PENALIZE COORDINATION AT SCALE (NEW Jan 2026)
% ============================================================================
% As spatial scope increases, coordination becomes more difficult and
% the potential for extractive asymmetries grows. This penalty models
% the "Dunbar-sensitive" scaling observation from sociology.md.

scope_penalty(local,       0.0).  % Baseline
scope_penalty(regional,    0.15). % e.g., State/Province
scope_penalty(national,    0.40). % e.g., Country
scope_penalty(continental, 0.75). % e.g., EU
scope_penalty(global,      1.20). % Worldwide
scope_penalty(universal,   2.0).  % Abstract/Universal claims

%% temporal_penalty(+TimeHorizon, -PenaltyFactor)
%  Models the "discount" or reduced impact of constraints over longer time horizons.
%  Rule: Metric_eff = Metric * PenaltyFactor
temporal_penalty(immediate,      1.0). % No discount for immediate effects
temporal_penalty(biographical,   0.8). % Discount for effects over a lifetime
temporal_penalty(generational,   0.5). % Significant discount for generational effects
temporal_penalty(historical,     0.2). % Steep discount for historical scale
temporal_penalty(civilizational, 0.1). % Very steep discount for civilizational scale

%% calculate_scaled_metric(+ConfigParamName, +Constraint, +Scope, +Time, -ScaledValue)
%  Calculates a metric value adjusted for both spatial scope and time horizon.
%  ConfigParamName: The name of the config parameter holding the actual metric name (e.g., suppression_metric_name)
%  Constraint: The constraint ID
%  Scope: The spatial scope from the context
%  Time: The time horizon from the context
%  ScaledValue: The calculated metric value after applying scope and time adjustments.
calculate_scaled_metric(ConfigParamName, Constraint, Scope, Time, ScaledValue) :-
    config:param(ConfigParamName, ActualMetricName), % Get the actual metric name from config
    (narrative_ontology:constraint_metric(Constraint, ActualMetricName, BaseValue) -> true; BaseValue = 0),
    (scope_penalty(Scope, ScopeFactor) -> true ; ScopeFactor = 0),
    (temporal_penalty(Time, TimeFactor) -> true ; TimeFactor = 1.0),
    ScaledValue is BaseValue * (1 + ScopeFactor) * TimeFactor.

%% get_raw_suppression(+Constraint, -Value)
%  Suppression is a structural property of the constraint - no temporal or scope scaling.
%  Power-scaled extractiveness already handles the perspectival dimension.
%  Temporal scaling on suppression made it mathematically impossible for constraints
%  to reach snare (0.60) or tangled_rope (0.40) floors from analytical context.
get_raw_suppression(Constraint, Value) :-
    config:param(suppression_metric_name, ActualMetricName),
    (narrative_ontology:constraint_metric(Constraint, ActualMetricName, Value) -> true ; Value = 0).

% Re-export indexed classification predicates from constraint_indexing
:- reexport(constraint_indexing, [
    constraint_classification/3,
    constraint_claim_indexed/2,
    multi_index_report/1,
    compare_perspectives/2,
    discover_my_context/1
]).

% ============================================================================
% CLASSIFICATION LOGIC - INDEXED VERSION (PRIMARY)
% ============================================================================

% ----------------------------------------------------------------------------
% Type-Specific Tests (Indexed) — delegate to classify_from_metrics/6
% ----------------------------------------------------------------------------
% Each is_X/3 computes metrics and calls classify_from_metrics with Type bound.
% Succeeds with the type atom if matched, or 'fail' otherwise.
% External callers (dr_mismatch, psych_bridge, constraint_instances) unchanged.

is_mountain(C, Context, mountain) :-
    base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    get_raw_suppression(C, Supp),
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, mountain), !.
is_mountain(_C, _Context, fail).

is_snare(C, Context, snare) :-
    base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    get_raw_suppression(C, Supp),
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, snare), !.
is_snare(_C, _Context, fail).

is_scaffold(C, Context, scaffold) :-
    base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    get_raw_suppression(C, Supp),
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, scaffold), !.
is_scaffold(_C, _Context, fail).

is_rope(C, Context, rope) :-
    base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    get_raw_suppression(C, Supp),
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, rope), !.
is_rope(_C, _Context, fail).

is_tangled_rope(C, Context, tangled_rope) :-
    base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    get_raw_suppression(C, Supp),
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, tangled_rope), !.
is_tangled_rope(_C, _Context, fail).

is_piton(C, Context, piton) :-
    base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    get_raw_suppression(C, Supp),
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, piton), !.
is_piton(_C, _Context, fail).

% ============================================================================
% SHARED THRESHOLD CLASSIFICATION (Single Source of Truth)
% ============================================================================
% classify_from_metrics(+C, +BaseEps, +Chi, +Supp, +Context, -Type)
%
% Given pre-computed metrics, determines classification.
% This is the CANONICAL threshold logic — all modules delegate here.
% C needed for structural property lookups (coordination, enforcement, theater).
%
% Priority: Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > unknown

%% snare_immutability_check(+Context)
%  The snare gate should only be blocked by STRUCTURAL immutability
%  (genuine mountains like gravity), not POWER-INDEXED immutability
%  (constraints that appear immutable from powerless but are changeable
%  from organized/institutional power levels).
%
%  A serf cannot individually abolish serfdom — it appears as immutable
%  as gravity from biographical/trapped perspective. But organized
%  collective action CAN change it, proving it is a snare, not a mountain.
%  Mountains are immutable at EVERY power level.
%
%  Fast path: if changeable from current perspective, pass immediately.
%  Slow path: if current perspective says mountain, check whether ANY
%  standard higher-power perspective sees changeability (rope).
snare_immutability_check(Context) :-
    constraint_indexing:effective_immutability_for_context(Context, rope), !.
snare_immutability_check(_Context) :-
    standard_context(AltCtx),
    constraint_indexing:effective_immutability_for_context(AltCtx, rope), !.

%% power_order(?PowerAtom, ?Rank)
%  Ascending power ordering used by snare_reform_threshold/2.
%  Lower rank = less power. Analytical is highest because it requires
%  systemic-level view to perceive changeability.
power_order(powerless,     1).
power_order(moderate,      2).
power_order(powerful,      3).
power_order(organized,     4).
power_order(institutional, 5).
power_order(analytical,    6).

%% snare_reform_threshold(+Context, -PowerLevel)
%  Returns the minimum power level at which effective immutability
%  transitions from mountain (unchangeable) to rope (changeable).
%
%  When the snare gate fires via power-indexed immutability (the
%  current context sees mountain but a higher-power context sees rope),
%  this predicate identifies WHERE on the power ladder the transition
%  happens. This is actionable: "requires organized collective action"
%  vs "requires institutional capture."
%
%  If changeable from the current perspective already, returns that
%  power level (the constraint isn't power-indexed immutable at all).
snare_reform_threshold(Context, PowerLevel) :-
    % Fast path: changeable from current perspective
    constraint_indexing:effective_immutability_for_context(Context, rope), !,
    Context = context(agent_power(PowerLevel), _, _, _).
snare_reform_threshold(Context, PowerLevel) :-
    % Walk standard contexts in ascending power order
    Context = context(agent_power(CurrentPower), _, _, _),
    power_order(CurrentPower, CurrentRank),
    % Find the lowest-rank standard context above current that sees rope
    standard_context(AltCtx),
    AltCtx = context(agent_power(AltPower), _, _, _),
    power_order(AltPower, AltRank),
    AltRank > CurrentRank,
    constraint_indexing:effective_immutability_for_context(AltCtx, rope), !,
    PowerLevel = AltPower.

%% scaffold_temporality_check(+C)
%  Soft temporality gate: explicit sunset clause is strongest signal,
%  but absence of enforcement requirement also qualifies (non-enforced
%  coordination is inherently scaffold-like, not tangled-rope-like).
scaffold_temporality_check(C) :-
    narrative_ontology:has_sunset_clause(C), !.
scaffold_temporality_check(C) :-
    \+ requires_active_enforcement(C).

%% natural_law_without_beneficiary(+C)
%  True when a constraint emerges naturally, requires no enforcement,
%  and has no identifiable human beneficiary. Such constraints are
%  Mountains regardless of metric values — asymmetric impact is not
%  asymmetric extraction.
natural_law_without_beneficiary(C) :-
    emerges_naturally(C),
    \+ requires_active_enforcement(C),
    \+ narrative_ontology:constraint_beneficiary(C, _).

classify_from_metrics(_C, BaseEps, _Chi, Supp, Context, mountain) :-
    config:param(mountain_suppression_ceiling, SuppCeil),
    Supp =< SuppCeil,
    config:param(mountain_extractiveness_max, MaxX),
    BaseEps =< MaxX,
    constraint_indexing:effective_immutability_for_context(Context, mountain), !.

classify_from_metrics(C, _BaseEps, _Chi, _Supp, _Context, snare) :-
    natural_law_without_beneficiary(C), !, fail.     % Block snare for natural laws
classify_from_metrics(_C, BaseEps, Chi, Supp, Context, snare) :-
    config:param(snare_chi_floor, ChiFloor),
    Chi >= ChiFloor,
    config:param(snare_epsilon_floor, EpsFloor),
    BaseEps >= EpsFloor,
    config:param(snare_suppression_floor, SuppFloor),
    Supp >= SuppFloor,
    snare_immutability_check(Context), !.

classify_from_metrics(C, _BaseEps, Chi, _Supp, _Context, scaffold) :-
    config:param(scaffold_extraction_ceil, MaxX),
    Chi =< MaxX,
    narrative_ontology:has_coordination_function(C),
    scaffold_temporality_check(C),
    config:param(theater_metric_name, TheaterMetricName),
    \+ (narrative_ontology:constraint_metric(C, TheaterMetricName, TR), TR > 0.70), !.

classify_from_metrics(C, BaseEps, Chi, _Supp, Context, rope) :-
    config:param(rope_chi_ceiling, ChiCeil),
    Chi =< ChiCeil,
    % v6.0: When Chi =< 0, agent is a net beneficiary — skip base extraction gate.
    % Negative effective extraction means the constraint is experienced as
    % coordination infrastructure regardless of how high the raw base is.
    (Chi =< 0 -> true ; config:param(rope_epsilon_ceiling, EpsCeil), BaseEps =< EpsCeil),
    (   constraint_indexing:effective_immutability_for_context(Context, rope)
    ;   emerges_naturally(C)  % Domain-invariant: bypass power-indexed immutability
    ), !.

classify_from_metrics(C, _BaseEps, _Chi, _Supp, _Context, tangled_rope) :-
    natural_law_without_beneficiary(C), !, fail.     % Block tangled_rope for natural laws
classify_from_metrics(C, BaseEps, Chi, Supp, _Context, tangled_rope) :-
    config:param(tangled_rope_chi_floor, ChiFloor),
    config:param(tangled_rope_chi_ceil, ChiCeil),
    Chi >= ChiFloor,
    Chi =< ChiCeil,
    config:param(tangled_rope_epsilon_floor, EpsFloor),
    BaseEps >= EpsFloor,
    config:param(tangled_rope_suppression_floor, MinS),
    Supp >= MinS,
    requires_active_enforcement(C),
    narrative_ontology:has_coordination_function(C),
    narrative_ontology:has_asymmetric_extraction(C), !.

classify_from_metrics(C, BaseEps, Chi, _Supp, _Context, piton) :-
    config:param(piton_extraction_ceiling, XCeil),
    Chi =< XCeil,
    config:param(piton_epsilon_floor, EpsFloor),
    BaseEps > EpsFloor,
    config:param(theater_metric_name, TheaterMetricName),
    narrative_ontology:constraint_metric(C, TheaterMetricName, TR),
    config:param(piton_theater_floor, TRFloor),
    TR >= TRFloor, !.

classify_from_metrics(_C, _BaseEps, _Chi, _Supp, _Context, unknown).

% ============================================================================
% CANONICAL TYPE DETERMINATION (INDEXED)
% ============================================================================

% ----------------------------------------------------------------------------
% Primary Classification: dr_type/3
% ----------------------------------------------------------------------------
% Determines constraint type FROM A SPECIFIC CONTEXT
% Integrates: (1) Metric-based classification, (2) Structural signatures

dr_type(C, Context, Type) :-
    % Validate context
    constraint_indexing:valid_context(Context),
    
    % First: Try metric-based classification with power scaling
    metric_based_type_indexed(C, Context, MetricType),
    
    % Second: Check if structural signature overrides
    structural_signatures:integrate_signature_with_modal(C, MetricType, Type),
    !.

dr_type(_C, _Context, unknown).

% ----------------------------------------------------------------------------
% Metric-Based Classification (Indexed) - Helper
% ----------------------------------------------------------------------------
% Delegates to classify_from_metrics/6 (Single Source of Truth).

metric_based_type_indexed(C, Context, Type) :-
    base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    get_raw_suppression(C, Supp),
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type).

% ============================================================================
% BACKWARD COMPATIBILITY LAYER
% ============================================================================

% ----------------------------------------------------------------------------
% Legacy dr_type/2 - Defaults to Analytical Context
% ----------------------------------------------------------------------------

dr_type(C, Type) :-
    constraint_indexing:default_context(Ctx),
    dr_type(C, Ctx, Type).

% ============================================================================
% ACTION ROUTING (INDEXED)
% ============================================================================

% ----------------------------------------------------------------------------
% Primary Action Router: dr_action/3
% ----------------------------------------------------------------------------
% Recommends action based on constraint type FROM SPECIFIC CONTEXT

dr_action(C, Context, accept) :-
    dr_type(C, Context, mountain), !.

dr_action(C, Context, maintain) :-
    dr_type(C, Context, rope), !.

dr_action(C, Context, reform) :-
    dr_type(C, Context, tangled_rope), !.

dr_action(C, Context, cut) :-
    dr_type(C, Context, snare), !.

dr_action(C, Context, monitor_sunset) :-
    dr_type(C, Context, scaffold), !.

dr_action(C, Context, bypass) :-
    dr_type(C, Context, piton), !.

dr_action(_C, _Context, investigate).

% ----------------------------------------------------------------------------
% Legacy dr_action/2 - Defaults to Analytical Context
% ----------------------------------------------------------------------------

dr_action(C, Action) :-
    constraint_indexing:default_context(Ctx),
    dr_action(C, Ctx, Action).

% ============================================================================
% ERROR DETECTION (INDEXED)
% ============================================================================

% ----------------------------------------------------------------------------
% Type 1: False Mountain (Indexed)
% ----------------------------------------------------------------------------
% Claimed as Mountain but ISN'T from this context

dr_mismatch(C, Context, type_1_false_summit, severe) :-
    % Check if claimed as mountain in data
    narrative_ontology:constraint_claim(C, mountain),
    
    % Verify it's NOT a mountain from this context
    is_mountain(C, Context, fail),
    !.

% ----------------------------------------------------------------------------
% Type 3: Snare Misidentified as Rope (Indexed)
% ----------------------------------------------------------------------------
% Claimed as Rope but is actually Snare from this context

dr_mismatch(C, Context, type_3_snare_as_rope, severe) :-
    narrative_ontology:constraint_claim(C, rope),
    is_snare(C, Context, snare),
    !.

% ----------------------------------------------------------------------------
% Type 5: Piton Misidentified as Snare (Indexed)
% ----------------------------------------------------------------------------
% Claimed as Snare but is actually Piton from this context

dr_mismatch(C, Context, type_5_piton_as_snare, moderate) :-
    narrative_ontology:constraint_claim(C, theater_ratio, TR), TR >= 0.70,
    is_piton(C, Context, piton),
    !.

% ----------------------------------------------------------------------------
% Type 7: Perspectival Incoherence (NEW)
% ----------------------------------------------------------------------------
% Same constraint classified differently across meaningful perspectives
% This is NOT an error but a FEATURE - it indicates indexical relativity

dr_mismatch(C, perspectival_gap(Type1, Ctx1, Type2, Ctx2), 
            perspectival_incoherence, informational) :-
    % Find two different classifications
    constraint_indexing:constraint_classification(C, Type1, Ctx1),
    constraint_indexing:constraint_classification(C, Type2, Ctx2),
    
    % Types must differ
    Type1 \= Type2,
    
    % Contexts must differ in meaningful way (not analytical vs analytical)
    Ctx1 = context(agent_power(P1), _, _, _),
    Ctx2 = context(agent_power(P2), _, _, _),
    P1 \= analytical,
    P2 \= analytical,
    P1 \= P2,
    !.

% ----------------------------------------------------------------------------
% Legacy dr_mismatch/3 - Defaults to Analytical Context
% ----------------------------------------------------------------------------

dr_mismatch(C, ErrorType, Severity) :-
    constraint_indexing:default_context(Ctx),
    dr_mismatch(C, Ctx, ErrorType, Severity).

% ============================================================================
% STRUCTURAL SIGNATURE DETECTION
% ============================================================================

%% dr_signature(+Constraint, -Signature)
%  Detects structural signature: natural_law | coordination_scaffold | 
%  constructed_constraint | ambiguous
%
%  NOTE: Signatures are NOT indexed - they represent the constraint's
%        fundamental structure, not perspectival appearance

dr_signature(C, Signature) :-
    structural_signatures:constraint_signature(C, Signature).

% ============================================================================
% PERSPECTIVAL GAP DETECTION (NEW)
% ============================================================================

%% perspectival_gap(+Constraint, -GapReport)
%  Detects when same constraint classifies differently across perspectives
%  Returns structured gap information for Omega generation

perspectival_gap(C, gap(Type1, Ctx1, Type2, Ctx2, PowerDelta)) :-
    % Find two classifications
    constraint_indexing:constraint_classification(C, Type1, Ctx1),
    constraint_indexing:constraint_classification(C, Type2, Ctx2),

    % Must differ in type
    Type1 \= Type2,

    % Extract power levels
    Ctx1 = context(agent_power(P1), _, _, _),
    Ctx2 = context(agent_power(P2), _, _, _),

    % Must be non-analytical
    P1 \= analytical,
    P2 \= analytical,

    % Must differ in power
    P1 \= P2,

    % Calculate power delta using directionality (v5.0)
    constraint_indexing:canonical_d_for_power(P1, D1),
    constraint_indexing:canonical_d_for_power(P2, D2),
    PowerDelta is abs(D1 - D2),
    !.

% ============================================================================
% INDEXICAL ANALYSIS UTILITIES (NEW)
% ============================================================================

%% cross_context_analysis(+Constraint, -Analysis)
%  Analyzes how constraint appears across standard contexts
%  Useful for validation and debugging

cross_context_analysis(C, Analysis) :-
    findall(
        context_result(Ctx, Type),
        (standard_context(Ctx),
         dr_type(C, Ctx, Type)),
        Results
    ),
    Analysis = cross_context(C, Results).

% Standard contexts for testing
standard_context(context(agent_power(powerless), 
                        time_horizon(biographical), 
                        exit_options(trapped), 
                        spatial_scope(local))).

standard_context(context(agent_power(moderate),
                        time_horizon(biographical),
                        exit_options(mobile),
                        spatial_scope(national))).

standard_context(context(agent_power(institutional), 
                        time_horizon(generational), 
                        exit_options(arbitrage), 
                        spatial_scope(national))).

standard_context(context(agent_power(analytical), 
                        time_horizon(civilizational), 
                        exit_options(analytical), 
                        spatial_scope(global))).

% ============================================================================
% VERSION & COMPATIBILITY INFO
% ============================================================================

/*
VERSION HISTORY:
v4.0 (2025-01-17):
  - BREAKING: dr_type/3 is now primary API (added Context)
  - NEW: Full indexical relativity integration
  - NEW: Power-scaled extractiveness in all classifications
  - NEW: Perspectival gap detection
  - NEW: dr_action/3, dr_mismatch/4 (indexed versions)
  - CHANGED: All classification uses effective_extractiveness
  - MAINTAIN: Backward compatibility via dr_type/2, dr_action/2

v3.2:
  - Added structural signature integration
  - Signature overrides metric-based classification

v3.1:
  - Consolidated namespace
  - Added piton detection

v3.0:
  - Initial metric-based classification

MIGRATION GUIDE v3.x → v4.0:
  Old: dr_type(constraint, Type)
  New: dr_type(constraint, Context, Type)
  
  For backward compat, old form still works (uses analytical context).
  
  To get indexed classification:
    constraint_indexing:default_context(Ctx),  % or build custom context
    dr_type(constraint, Ctx, Type)
*/
