% ============================================================================
% DRIFT EVENTS — Event Detection, Severity, Velocity
% Split from drl_lifecycle.pl
% ============================================================================

:- module(metric_drift_events, [
    % Individual Drift Event Detectors
    detect_metric_substitution/1,
    detect_extraction_accumulation/1,
    detect_coordination_loss/1,
    detect_function_obsolescence/1,
    detect_sunset_violation/1,
    detect_extraction_dried_up/1,
    detect_is_piton/1,

    % Structured Drift Event API
    drift_event/3,
    drift_event/4,

    % Drift Velocity
    drift_velocity/3,

    % Severity Classification
    drift_severity/3,

    % Boltzmann Drift Events (v5.0)
    detect_coupling_drift/1,
    detect_boltzmann_floor_drift/1,
    reform_pressure/2,

    % Purity Drift (v5.1)
    detect_purity_drift/1,

    % Shared utilities (needed by transition_paths, network_dynamics, metric_drift_report)
    safe_metric/3,
    metric_trend/3,
    metric_at/4,
    collect_purity_decline_signals/2
]).

:- use_module(library(lists), [last/2]).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(boltzmann_compliance, [cross_index_coupling/2, excess_extraction/2, boltzmann_floor_for/2]).
:- use_module(purity_scoring, [purity_score/2]).

:- discontiguous drift_event/3.
:- discontiguous drift_event/4.
:- discontiguous drift_severity/3.

/* ================================================================
   1. UTILITY
   ================================================================ */

%% get_current_year(-Year)
%  Retrieves the current year from the system clock.
get_current_year(Year) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    DateTime = date(Year, _, _, _, _, _, _, _, _).

%% safe_metric(+C, +Metric, -Value)
%  Retrieves a metric or fails silently.
safe_metric(C, Metric, Value) :-
    narrative_ontology:constraint_metric(C, Metric, Value).

%% metric_delta(+C, +Metric, -T1, -T2, -Delta)
%  Finds the earliest and latest measurements and computes the change.
%
%  REDUCTION = ENDPOINT-ONLY (first/last of the sorted series; all intermediate
%  timepoints discarded). Module-PRIVATE (not exported), event-gate use only —
%  a spike-and-recover series (V=0.35→0.68→0.58) yields the same Delta as a
%  monotone climb to the same endpoint. NOT A TRAJECTORY SOURCE. For a faithful
%  full-series reading use raw narrative_ontology:measurement/5,
%  drl_composition:non_monotonic_trajectory/2, or the drift_trajectory subsystem
%  (OQ-83). (OQ-18; private so no serialized consumer reads the thrown-away
%  resolution — the only collapsing predicate here with no marked falsifier.)
metric_delta(C, Metric, T1, T2, Delta) :-
    findall(T-V, narrative_ontology:measurement(_, C, Metric, T, V), Pairs),
    Pairs = [_|_],
    sort(Pairs, Sorted),
    Sorted = [T1-V1|_],
    last(Sorted, T2-V2),
    T2 > T1,
    Delta is V2 - V1.

%% metric_at(+C, +Metric, +Time, -Value)
%  Retrieves measurement at a specific time.
metric_at(C, Metric, Time, Value) :-
    narrative_ontology:measurement(_, C, Metric, Time, Value).

%% metric_trend(+C, +Metric, -Trend)
%  Determines if a metric is increasing, decreasing, or stable.
%
%  SEMANTICS = NET-CHANGE DIRECTION: Trend buckets metric_delta's endpoint
%  Delta (= V_last − V_first) at ±0.05. Net change IS correctly the endpoints
%  (a series ending higher rose net, whatever the path), so this is NOT a
%  velocity-style reduction defect — it does, however, NOT measure *sustained*
%  trend: a spike-and-recede series with net Δ>0.05 reads `increasing` while a
%  least-squares fit is flat.
%
%  REACHES A SERIALIZED VERDICT: metric_trend(C, suppression_requirement, increasing)
%  gates cs_verdict(C, scaffold_suppression_escalating) (json_report.pl:570,
%  "cs_verdicts" → pipeline_output.json). FALSIFIER (witnessed, probe
%  audits/2026-06-25_oq18_temporal_reduction/oq18_metric_trend_flip.pl, control
%  live): net-change and regression-trend
%  diverge for 0 / 1 / 17 serialized verdicts on testsets/haiku/flash (haiku:
%  nicene_creed Δ=0.08 vs fit 0.0207; flash cases hairline at the ±0.05 cut).
%  Behavior is unchanged, but whether "escalating" should mean net-higher
%  (endpoints correct) or sustained-trend (LSQ more faithful) is a semantic seat
%  routed to OQ-183 (cross-linked to OQ-19, the ±0.05 magic-number adjacency).
%  NOT "safe / no resolution to corrupt."
%
%  Output vocabulary {increasing,decreasing,stable} (no `unknown`) collides with
%  logical_fingerprint:metric_trend/3's {rising,falling,stable,unknown} — see
%  OQ-183 limb (i). Positive-control lesson for any re-run: bind C from
%  corpus_loader:corpus_constraint/1 BEFORE cs_verdict/2 — an unbound query
%  returns a false 0 (dr_type/3 cannot generate C).
metric_trend(C, Metric, Trend) :-
    metric_delta(C, Metric, _, _, Delta),
    (   Delta > 0.05  -> Trend = increasing
    ;   Delta < -0.05 -> Trend = decreasing
    ;   Trend = stable
    ).

/* ================================================================
   2. INDIVIDUAL DRIFT EVENT DETECTORS
   Each detector both prints diagnostics AND succeeds/fails.
   ================================================================ */

%% detect_metric_substitution(+ConstraintID)
%  Goodhart drift: proxy metric becomes the goal.
%  Evidence: theater_ratio rising above 0.5 across time points.
detect_metric_substitution(C) :-
    narrative_ontology:measurement(_, C, theater_ratio, T1, TR1),
    narrative_ontology:measurement(_, C, theater_ratio, T2, TR2),
    T2 > T1,
    TR2 > TR1,
    TR2 > 0.5,
    format('  Drift: Metric Substitution in ~w~n', [C]),
    format('    Theater ratio ~2f (~w) -> ~2f (~w)~n', [TR1, T1, TR2, T2]).

%% detect_extraction_accumulation(+ConstraintID)
%  Rent-seeking accumulates over time.
detect_extraction_accumulation(C) :-
    narrative_ontology:measurement(_, C, base_extractiveness, T1, V1),
    narrative_ontology:measurement(_, C, base_extractiveness, T2, V2),
    T2 > T1,
    V2 > V1,
    format('  Drift: Extraction Accumulation in ~w~n', [C]),
    format('    Extractiveness ~2f (~w) -> ~2f (~w)~n', [V1, T1, V2, T2]).

%% detect_coordination_loss(+ConstraintID)
%  Coordination withers while extraction persists.
detect_coordination_loss(C) :-
    narrative_ontology:has_coordination_function(C),
    narrative_ontology:measurement(_, C, coordination_effectiveness, T1, CE1),
    narrative_ontology:measurement(_, C, coordination_effectiveness, T2, CE2),
    T2 > T1,
    CE2 < CE1,
    CE2 < 0.3,
    safe_metric(C, extractiveness, E),
    E > 0.4,
    format('  Drift: Coordination Loss in ~w~n', [C]),
    format('    Coordination dropped to ~2f, extraction still ~2f~n', [CE2, E]).

%% detect_function_obsolescence(+ConstraintID)
%  Original function obsolete due to alternatives.
detect_function_obsolescence(C) :-
    safe_metric(C, alternatives_available, Alt),
    Alt > 0,
    safe_metric(C, resistance_to_change, R),
    R < 0.3,
    safe_metric(C, theater_ratio, TR),
    TR > 0.5,
    format('  Drift: Function Obsolescence in ~w~n', [C]),
    format('    Alternatives exist (~w), low resistance (~2f), high theater (~2f)~n', [Alt, R, TR]).

%% detect_sunset_violation(+ConstraintID)
%  Temporary constraint became permanent.
detect_sunset_violation(C) :-
    narrative_ontology:has_sunset_clause(C),
    safe_metric(C, sunset_time, SunsetYear),
    get_current_year(CurrentYear),
    CurrentYear > SunsetYear,
    format('  Drift: Sunset Violation in ~w~n', [C]),
    format('    Sunset was ~w, now ~w~n', [SunsetYear, CurrentYear]).

%% detect_extraction_dried_up(+ConstraintID)
%  Extraction mechanism failed but structure persists (zombie constraint).
detect_extraction_dried_up(C) :-
    safe_metric(C, extractiveness, E),
    E < 0.10,
    safe_metric(C, suppression_requirement, S),
    S > 0.50,
    format('  Drift: Extraction Dried Up in ~w~n', [C]),
    format('    Extraction ~2f but suppression still ~2f~n', [E, S]).

%% detect_is_piton(+ConstraintID)
%  Internalized piton: extraction removed but habits remain.
detect_is_piton(C) :-
    safe_metric(C, extractiveness, E),
    E < 0.10,
    safe_metric(C, theater_ratio, TR),
    TR > 0.70,
    \+ domain_priors:requires_active_enforcement(C),   % OQ-145: predicate lives in domain_priors, not narrative_ontology (OQ-57 fixed sibling :236, missed this)
    format('  Drift: Internalized Piton in ~w~n', [C]),
    format('    Low extraction (~2f), high theater (~2f), no enforcement~n', [E, TR]).

/* ================================================================
   3. STRUCTURED DRIFT EVENT API
   Returns data structures instead of printing.
   ================================================================ */

%% drift_event(+ConstraintID, -EventType, -Evidence)
%  Non-indexed drift detection. Returns structured evidence.
drift_event(C, metric_substitution, evidence(theater_delta, T1, T2, TR1, TR2)) :-
    findall(T-V, narrative_ontology:measurement(_, C, theater_ratio, T, V), Pairs),
    Pairs = [_|_],
    sort(Pairs, Sorted),
    Sorted = [T1-TR1|_],
    last(Sorted, T2-TR2),
    T2 > T1, TR2 > TR1, TR2 > 0.5.

drift_event(C, extraction_accumulation, evidence(extraction_delta, T1, T2, V1, V2)) :-
    findall(T-V, narrative_ontology:measurement(_, C, base_extractiveness, T, V), Pairs),
    Pairs = [_|_],
    sort(Pairs, Sorted),
    Sorted = [T1-V1|_],
    last(Sorted, T2-V2),
    T2 > T1, V2 > V1.

drift_event(C, coordination_loss, evidence(coordination_drop, CE2, extraction_persist, E)) :-
    narrative_ontology:has_coordination_function(C),
    findall(T-V, narrative_ontology:measurement(_, C, coordination_effectiveness, T, V), Pairs),
    Pairs = [_|_],
    sort(Pairs, Sorted),
    Sorted = [T1-_CE1|_],
    last(Sorted, T2-CE2),
    T2 > T1, CE2 < 0.3,
    safe_metric(C, extractiveness, E), E > 0.4.

drift_event(C, function_obsolescence, evidence(alternatives, Alt, resistance, R, theater, TR)) :-
    safe_metric(C, alternatives_available, Alt), Alt > 0,
    safe_metric(C, resistance_to_change, R), R < 0.3,
    safe_metric(C, theater_ratio, TR), TR > 0.5.

drift_event(C, sunset_violation, evidence(sunset_year, SunsetYear, current_year, CurrentYear)) :-
    narrative_ontology:has_sunset_clause(C),
    safe_metric(C, sunset_time, SunsetYear),
    get_current_year(CurrentYear),
    CurrentYear > SunsetYear.

drift_event(C, extraction_dried_up, evidence(extraction, E, suppression, S)) :-
    safe_metric(C, extractiveness, E), E < 0.10,
    safe_metric(C, suppression_requirement, S), S > 0.50.

drift_event(C, internalized_piton, evidence(extraction, E, theater, TR)) :-
    safe_metric(C, extractiveness, E), E < 0.10,
    safe_metric(C, theater_ratio, TR), TR > 0.70,
    % OQ-57 fix (2026-06-04): requires_active_enforcement/1 is authored and
    % bridged under domain_priors: everywhere; narrative_ontology: never
    % defined it, so this guard threw existence_error whenever reached
    % (eps<0.10 AND theater>0.70 — first hit: kodashim memorial_archival),
    % aborting the whole drift scan. The \+ reads absence-as-no-enforcement,
    % which matches generation convention (the fact is emitted when true).
    \+ domain_priors:requires_active_enforcement(C).

%% drift_event(+ConstraintID, +Context, -EventType, -Evidence)
%  Context-indexed drift detection.
drift_event(C, Context, extraction_accumulation_indexed, evidence(effective_chi, Chi, trend, Trend)) :-
    constraint_indexing:valid_context(Context),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    config:param(rope_chi_ceiling, RopeCeil),
    Chi > RopeCeil,
    metric_trend(C, base_extractiveness, Trend),
    Trend = increasing.

drift_event(C, Context, false_mountain_drift, evidence(claimed, mountain, actual, ActualType)) :-
    constraint_indexing:valid_context(Context),
    narrative_ontology:constraint_claim(C, mountain),
    drl_core:dr_type(C, Context, ActualType),
    ActualType \= mountain.

drift_event(C, Context, load_bearing_degradation, evidence(type, Type, extraction, Chi)) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C, Context, Type),
    member(Type, [snare, tangled_rope]),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    config:param(snare_load_bearing_threshold, T),
    Chi > T,
    metric_trend(C, base_extractiveness, increasing).

/* ================================================================
   3B. BOLTZMANN DRIFT EVENTS (v5.0)
   ================================================================ */

%% detect_coupling_drift(+ConstraintID)
detect_coupling_drift(C) :-
    boltzmann_compliance:cross_index_coupling(C, CurrentCoupling),
    config:param(boltzmann_coupling_threshold, Threshold),
    CurrentCoupling > Threshold,
    metric_trend(C, base_extractiveness, increasing),
    format('  Drift: Coupling Drift in ~w~n', [C]),
    format('    Coupling score ~3f (threshold ~3f), extraction rising~n',
           [CurrentCoupling, Threshold]).

%% detect_boltzmann_floor_drift(+ConstraintID)
detect_boltzmann_floor_drift(C) :-
    boltzmann_compliance:excess_extraction(C, Excess),
    boltzmann_compliance:boltzmann_floor_for(C, Floor),
    safe_metric(C, extractiveness, CurrentEps),
    metric_trend(C, base_extractiveness, increasing),
    Excess < Floor * 0.5,
    format('  Drift: Boltzmann Floor Drift in ~w~n', [C]),
    format('    Current ε=~3f, floor=~3f, excess=~3f (likely necessary complexity)~n',
           [CurrentEps, Floor, Excess]).

%% drift_event/3 clauses for Boltzmann types
drift_event(C, coupling_drift,
            evidence(coupling_score, Score, threshold, Threshold, extraction_trend, Trend)) :-
    boltzmann_compliance:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_threshold, Threshold),
    Score > Threshold,
    metric_trend(C, base_extractiveness, Trend),
    Trend = increasing.

drift_event(C, boltzmann_floor_drift,
            evidence(current_eps, Eps, floor, Floor, excess, Excess, trend, Trend)) :-
    boltzmann_compliance:excess_extraction(C, Excess),
    boltzmann_compliance:boltzmann_floor_for(C, Floor),
    safe_metric(C, extractiveness, Eps),
    metric_trend(C, base_extractiveness, Trend),
    Trend = increasing,
    Excess < Floor * 0.5.

%% Context-indexed Boltzmann drift events
drift_event(C, Context, coupling_drift_indexed,
            evidence(coupling_score, Score, chi, Chi, trend, Trend)) :-
    constraint_indexing:valid_context(Context),
    boltzmann_compliance:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_threshold, Threshold),
    Score > Threshold,
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    metric_trend(C, base_extractiveness, Trend),
    Trend = increasing.

drift_event(C, Context, reform_pressure_detected,
            evidence(excess, Excess, floor, Floor, chi, Chi)) :-
    constraint_indexing:valid_context(Context),
    boltzmann_compliance:excess_extraction(C, Excess),
    boltzmann_compliance:boltzmann_floor_for(C, Floor),
    Excess > Floor,
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    config:param(tangled_rope_chi_floor, TRFloor),
    Chi >= TRFloor.

%% reform_pressure(+Constraint, -Pressure)
reform_pressure(C, Pressure) :-
    boltzmann_compliance:excess_extraction(C, Excess),
    boltzmann_compliance:boltzmann_floor_for(C, Floor),
    (   Floor > 0.001
    ->  Pressure is Excess / Floor
    ;   (   Excess > 0.001
        ->  Pressure = 99.0
        ;   Pressure = 0.0
        )
    ).

/* ================================================================
   3C. PURITY DRIFT EVENT (v5.1)
   ================================================================ */

%% detect_purity_drift(+ConstraintID)
detect_purity_drift(C) :-
    purity_scoring:purity_score(C, Purity),
    number(Purity), Purity >= 0.0,   % OQ-60: unknown -> clause fails (no drift event)
    collect_purity_decline_signals(C, Signals),
    Signals \= [],
    length(Signals, N),
    format('  Drift: Purity Drift in ~w~n', [C]),
    format('    Current purity: ~3f, ~d decline signal(s): ~w~n',
           [Purity, N, Signals]).

%% drift_event/3 clause for purity drift
drift_event(C, purity_drift,
            evidence(current_purity, Purity, decline_signals, Signals)) :-
    purity_scoring:purity_score(C, Purity),
    number(Purity), Purity >= 0.0,   % OQ-60: unknown -> clause fails (no drift event)
    collect_purity_decline_signals(C, Signals),
    Signals \= [].

%% Context-indexed purity drift
drift_event(C, Context, purity_drift_indexed,
            evidence(current_purity, Purity, chi, Chi, signals, Signals)) :-
    constraint_indexing:valid_context(Context),
    purity_scoring:purity_score(C, Purity),
    number(Purity), Purity >= 0.0,   % OQ-60: unknown -> clause fails (no drift event)
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    collect_purity_decline_signals(C, Signals),
    Signals \= [].

%% collect_purity_decline_signals(+C, -Signals)
collect_purity_decline_signals(C, Signals) :-
    findall(Signal, purity_decline_signal(C, Signal), Signals).

purity_decline_signal(C, extraction_rising) :-
    metric_trend(C, base_extractiveness, increasing).

purity_decline_signal(C, coupling_above_threshold(Score)) :-
    boltzmann_compliance:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_threshold, Threshold),
    Score > Threshold.

purity_decline_signal(C, theater_rising) :-
    config:param(theater_metric_name, TM),
    metric_trend(C, TM, increasing).

purity_decline_signal(C, excess_above_floor(Excess)) :-
    boltzmann_compliance:excess_extraction(C, Excess),
    Excess > 0.02.

/* ================================================================
   3E. NETWORK DRIFT — Integration clauses
   These clauses delegate to network_dynamics via runtime qualification
   to avoid circular use_module dependencies.
   ================================================================ */

%% drift_event/3 — non-indexed (uses default context)
drift_event(C, network_drift, Evidence) :-
    constraint_indexing:default_context(Ctx),
    network_dynamics:detect_network_contamination(C, Ctx, Evidence).

%% drift_event/4 — context-indexed
drift_event(C, Context, network_drift_indexed,
            evidence(drifting_neighbors, ContagionList, effective_purity, EP, velocity, V)) :-
    constraint_indexing:valid_context(Context),
    network_dynamics:detect_network_contamination(C, Context, evidence(drifting_neighbors, ContagionList, effective_purity, EP, _, _)),
    network_dynamics:network_drift_velocity(C, Context, V, _).

/* ================================================================
   5. DRIFT VELOCITY
   (drift_acceleration/3 + compute_acceleration/2 removed 2026-06-25, OQ-18:
    zero callers, first-3-points reduction, name actively misleading. Faithful
    full-series acceleration logged as a declared absence in design_gaps.md.)
   ================================================================ */

%% drift_velocity(+ConstraintID, +Metric, -RatePerYear)
%
%  REDUCTION = ENDPOINT RATE (Delta/Duration, first/last only — via metric_delta).
%  Diverges from the faithful least-squares slope (drl_composition:linear_slope/2)
%  on most live series: per-neighbor |Δrate| up to 0.0011 / 0.0057 / 0.0067 on
%  86/97, 890/954, 639/949 series (testsets/haiku/flash).
%
%  CONSUMERS — magnitude vs gate split (do NOT call this "render-only" or
%  "safe-as-gate"; both were witnessed false):
%   * MAGNITUDE is read only by render-only sites (metric_drift_report.pl:160/:174,
%     cascade_prediction — printed, never serialized).
%   * The RATE ALSO FEEDS A SERIALIZED VERDICT: drift_velocity →
%     network_dynamics:network_drift_velocity/4 (sum over Rate>0 contributors) →
%     cs_drift_mismatch:cs_is_metric_stable/1 (gate V >= network_drift_velocity_threshold)
%     → cs_drift_mismatch/2 → json_report.pl:2015 → pipeline_output.json. So the
%     endpoint reduction reaches a machine-consumed CS field, and it is FLIPPABLE
%     in principle (a spike-and-recover neighbor contributes 0 where a faithful
%     series would not).
%
%  CURRENTLY-UNCORRUPTING, WITH THIS FALSIFIER (re-witnessed 2026-06-25, probes
%  in audits/2026-06-25_oq18_temporal_reduction/ — oq18_flipped_probe.pl +
%  oq18_divergence_probe.pl, live control on every leg):
%  0 serialized cs_drift_mismatch verdicts flip under the faithful least-squares
%  velocity on any live leg (max faithful SUM 0.006745/0.007851/0.004333 < Thresh
%  0.01; closest headroom Thresh−Vf = 0.00215 on testsets_haiku — a SUM-level
%  margin, the gate's exact quantity).
%
%  KILL CONDITION: any serialized cs_drift_mismatch verdict whose FAITHFUL
%  network_drift_velocity sum crosses network_drift_velocity_threshold — at which
%  point this endpoint reduction is producing a wrong verdict and the faithful
%  least-squares replacement (OQ-184, with the sum-level tripwire) must land. The
%  0.00215 haiku headroom is the reason to PRIORITIZE OQ-184, not to guard now.
drift_velocity(C, Metric, Rate) :-
    metric_delta(C, Metric, T1, T2, Delta),
    Duration is T2 - T1,
    Duration > 0,
    Rate is Delta / Duration.

/* ================================================================
   6. SEVERITY CLASSIFICATION
   ================================================================ */

% --- Purity drift severity (v5.1) ---

drift_severity(C, purity_drift, critical) :-
    purity_scoring:purity_score(C, Purity),
    number(Purity), Purity < 0.30,   % OQ-60: unknown -> clause fails (falls through to warning/watch)
    collect_purity_decline_signals(C, Signals),
    length(Signals, N), N >= 3, !.

drift_severity(C, purity_drift, warning) :-
    purity_scoring:purity_score(C, Purity),
    number(Purity), Purity < 0.50, !.   % OQ-60: unknown -> clause fails (falls through to watch)
drift_severity(C, purity_drift, warning) :-
    collect_purity_decline_signals(C, Signals),
    length(Signals, N), N >= 2, !.

drift_severity(_, purity_drift, watch) :- !.

drift_severity(C, purity_drift_indexed, Severity) :-
    drift_severity(C, purity_drift, Severity), !.

% --- Standard severity ---

drift_severity(C, sunset_violation, critical) :-
    drift_event(C, sunset_violation, _),
    safe_metric(C, extractiveness, E), E > 0.3, !.
drift_severity(_, sunset_violation, warning) :- !.

drift_severity(C, extraction_accumulation, critical) :-
    drift_event(C, extraction_accumulation, evidence(_, _, _, _, V2)),
    config:param(snare_epsilon_floor, Floor),
    V2 >= Floor, !.
drift_severity(C, extraction_accumulation, warning) :-
    drift_event(C, extraction_accumulation, evidence(_, _, _, _, V2)),
    config:param(tangled_rope_epsilon_floor, Floor),
    V2 >= Floor, !.
drift_severity(_, extraction_accumulation, watch) :- !.

drift_severity(_, coordination_loss, critical) :-
    !.

drift_severity(_, internalized_piton, warning) :- !.

drift_severity(_, extraction_dried_up, warning) :- !.

drift_severity(C, metric_substitution, Severity) :-
    (   safe_metric(C, theater_ratio, TR), TR > 0.7
    ->  Severity = critical
    ;   Severity = warning
    ), !.

drift_severity(_, function_obsolescence, watch) :- !.

% --- Boltzmann drift severity (v5.0) ---

drift_severity(C, coupling_drift, critical) :-
    boltzmann_compliance:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_strong_threshold, StrongT),
    Score > StrongT,
    safe_metric(C, extractiveness, E),
    config:param(snare_epsilon_floor, SnareFloor),
    E >= SnareFloor, !.
drift_severity(C, coupling_drift, warning) :-
    boltzmann_compliance:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_strong_threshold, StrongT),
    Score > StrongT, !.
drift_severity(_, coupling_drift, watch) :- !.

drift_severity(_, boltzmann_floor_drift, watch) :- !.

drift_severity(C, reform_pressure_detected, critical) :-
    reform_pressure(C, P), P > 2.0, !.
drift_severity(C, reform_pressure_detected, warning) :-
    reform_pressure(C, P), P > 1.0, !.
drift_severity(_, reform_pressure_detected, watch) :- !.

drift_severity(C, coupling_drift_indexed, Severity) :-
    drift_severity(C, coupling_drift, Severity), !.

% --- Network drift severity — delegates via runtime qualification ---

drift_severity(C, network_drift, Severity) :-
    constraint_indexing:default_context(Ctx),
    network_dynamics:network_drift_severity(C, Ctx, Severity), !.

drift_severity(C, network_drift_indexed, Severity) :-
    constraint_indexing:default_context(Ctx),
    network_dynamics:network_drift_severity(C, Ctx, Severity), !.

% Default
drift_severity(_, _, watch).
