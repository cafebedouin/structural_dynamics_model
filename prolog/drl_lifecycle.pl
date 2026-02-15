:- module(drl_lifecycle, [
    % --- Individual Drift Event Detectors ---
    detect_metric_substitution/1,
    detect_extraction_accumulation/1,
    detect_coordination_loss/1,
    detect_function_obsolescence/1,
    detect_sunset_violation/1,
    detect_extraction_dried_up/1,
    detect_is_piton/1,

    % --- Structured Drift Event API ---
    drift_event/3,                  % drift_event(ConstraintID, EventType, Evidence)
    drift_event/4,                  % drift_event(ConstraintID, Context, EventType, Evidence)

    % --- Transition Path Detection ---
    transition_path/4,              % transition_path(ConstraintID, FromType, ToType, Evidence)
    degradation_chain/3,            % degradation_chain(ConstraintID, Chain, Evidence)
    predicted_terminal_state/3,     % predicted_terminal_state(ConstraintID, State, Confidence)

    % --- Drift Velocity ---
    drift_velocity/3,               % drift_velocity(ConstraintID, Metric, Rate)
    drift_acceleration/3,           % drift_acceleration(ConstraintID, Metric, Accel)

    % --- Severity Classification ---
    drift_severity/3,               % drift_severity(ConstraintID, EventType, Severity)

    % --- Unified Scan ---
    scan_all_drift/1,               % scan_all_drift(-Report)
    scan_constraint_drift/2,        % scan_constraint_drift(+ConstraintID, -Events)
    scan_constraint_drift/3,        % scan_constraint_drift(+ConstraintID, +Context, -Events)

    % --- Drift Report ---
    generate_drift_report/0,        % Print full drift report to stdout
    generate_drift_report/1,        % generate_drift_report(+ConstraintID)

    % --- Boltzmann Drift Events (v5.0) ---
    detect_coupling_drift/1,        % detect_coupling_drift(+ConstraintID)
    detect_boltzmann_floor_drift/1, % detect_boltzmann_floor_drift(+ConstraintID)
    reform_pressure/2,              % reform_pressure(+ConstraintID, -Pressure)

    % --- Purity Drift (v5.1) ---
    detect_purity_drift/1,          % detect_purity_drift(+ConstraintID)

    % --- Network Drift Dynamics (v5.2) ---
    detect_network_drift/3,         % detect_network_drift(+C, +Context, -Evidence)
    network_drift_contagion/3,      % network_drift_contagion(+C, +Context, -ContagionList)
    network_drift_velocity/4,       % network_drift_velocity(+C, +Context, -Velocity, -Contributors)
    cascade_prediction/3,           % cascade_prediction(+C, +Context, -Crossings)
    network_stability_assessment/2, % network_stability_assessment(+Context, -Assessment)
    network_drift_severity/3        % network_drift_severity(+C, +Context, -Severity)
]).

:- use_module(narrative_ontology).
:- use_module(domain_priors).
:- use_module(config).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(drl_modal_logic).

:- discontiguous drift_event/3.
:- discontiguous drift_event/4.
:- discontiguous drift_severity/3.

/* ================================================================
   DRL LIFECYCLE MODULE

   Detects constraint degradation patterns (drift events),
   transition paths, drift velocity, and severity.

   Ten drift event types:
   1. metric_substitution    - Proxy becomes goal (Goodhart)
   2. extraction_accumulation - Rent-seeking added over time
   3. coordination_loss      - Function withers, extraction persists
   4. function_obsolescence  - Environment shifts, constraint outdated
   5. sunset_violation       - Temporary becomes permanent
   6. extraction_dried_up    - Mechanism fails, structure persists
   7. internalized_piton     - Enforcer removed, habits remain
   8. coupling_drift         - Index dimensions become entangled (v5.0)
   9. boltzmann_floor_drift  - Minimum extraction increases (v5.0)
  10. purity_drift           - Structural purity declines (v5.1)
  11. network_drift          - Effective purity declining via neighbor contagion (v5.2)

   Canonical degradation paths from core.md:
   - rope -> tangled_rope -> snare -> piton
   - rope -> piton (direct obsolescence)
   - scaffold -> piton (sunset violation)
   - scaffold -> snare (calcification with extraction)
   - scaffold -> tangled_rope (extraction added during transition)
   - snare -> false_mountain (naturalization)
   - snare -> piton (internalized piton)
   ================================================================ */

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
    \+ narrative_ontology:requires_active_enforcement(C),
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
    \+ narrative_ontology:requires_active_enforcement(C).

%% drift_event(+ConstraintID, +Context, -EventType, -Evidence)
%  Context-indexed drift detection.
%  Same degradation can look different from different power positions.
%  A moderate-extraction constraint may appear as extraction_accumulation
%  from a powerless context (amplified by pi=1.5) but stable from
%  an institutional context (reduced by pi=-0.2).
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

   Type 8 — Coupling Drift (CD)
   A constraint that starts with independent index dimensions
   gradually becomes entangled — choices in "aisle A" start
   depending on choices in "aisle B." This is extractive coupling
   that emerges through structural drift, not raw ε increase.

   CD(C, t_drift) ≡
       CouplingTopology(C, t < t_drift) = independent
       ∧ CouplingTopology(C, t ≥ t_drift) = coupled
       ∧ ε(C, t ≥ t_drift) > ε(C, t < t_drift)

   Type 9 — Boltzmann Floor Drift (BFD)
   The minimum necessary extraction for a coordination type
   increases over time due to increasing system complexity.
   Distinguishes necessary complexity increase from extractive
   complexity increase.

   BFD(C, t_drift) ≡
       BoltzmannFloor(C, t_drift) > BoltzmannFloor(C, t₀)
       ∧ ε(C, t_drift) tracks the new floor

   Both types are in SHADOW MODE: detected and logged but do
   not trigger classification changes.
   ================================================================ */

%% detect_coupling_drift(+ConstraintID)
%  Detects when a constraint's coupling topology has become
%  more entangled over time. Requires temporal measurement data.
%  Evidence: coupling score now exceeds threshold AND extraction
%  has increased (ruling out pure functional entanglement).
detect_coupling_drift(C) :-
    structural_signatures:cross_index_coupling(C, CurrentCoupling),
    config:param(boltzmann_coupling_threshold, Threshold),
    CurrentCoupling > Threshold,
    % Check that extraction is also rising — coupling drift
    % is extractive only if accompanied by ε increase
    metric_trend(C, base_extractiveness, increasing),
    format('  Drift: Coupling Drift in ~w~n', [C]),
    format('    Coupling score ~3f (threshold ~3f), extraction rising~n',
           [CurrentCoupling, Threshold]).

%% detect_boltzmann_floor_drift(+ConstraintID)
%  Detects when a constraint's extraction has risen but is
%  potentially justified by increased coordination complexity.
%  This is the complement to extraction_accumulation: it asks
%  "is this extraction increase NECESSARY?"
detect_boltzmann_floor_drift(C) :-
    structural_signatures:excess_extraction(C, Excess),
    structural_signatures:boltzmann_floor_for(C, Floor),
    safe_metric(C, extractiveness, CurrentEps),
    metric_trend(C, base_extractiveness, increasing),
    % The constraint's extraction is rising but stays near the floor
    % → necessary complexity increase, not extraction
    Excess < Floor * 0.5,  % Excess is less than half the floor
    format('  Drift: Boltzmann Floor Drift in ~w~n', [C]),
    format('    Current ε=~3f, floor=~3f, excess=~3f (likely necessary complexity)~n',
           [CurrentEps, Floor, Excess]).

%% drift_event/3 clauses for new types
drift_event(C, coupling_drift,
            evidence(coupling_score, Score, threshold, Threshold, extraction_trend, Trend)) :-
    structural_signatures:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_threshold, Threshold),
    Score > Threshold,
    metric_trend(C, base_extractiveness, Trend),
    Trend = increasing.

drift_event(C, boltzmann_floor_drift,
            evidence(current_eps, Eps, floor, Floor, excess, Excess, trend, Trend)) :-
    structural_signatures:excess_extraction(C, Excess),
    structural_signatures:boltzmann_floor_for(C, Floor),
    safe_metric(C, extractiveness, Eps),
    metric_trend(C, base_extractiveness, Trend),
    Trend = increasing,
    Excess < Floor * 0.5.

%% Context-indexed Boltzmann drift events
drift_event(C, Context, coupling_drift_indexed,
            evidence(coupling_score, Score, chi, Chi, trend, Trend)) :-
    constraint_indexing:valid_context(Context),
    structural_signatures:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_threshold, Threshold),
    Score > Threshold,
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    metric_trend(C, base_extractiveness, Trend),
    Trend = increasing.

drift_event(C, Context, reform_pressure_detected,
            evidence(excess, Excess, floor, Floor, chi, Chi)) :-
    constraint_indexing:valid_context(Context),
    structural_signatures:excess_extraction(C, Excess),
    structural_signatures:boltzmann_floor_for(C, Floor),
    Excess > Floor,  % Extraction significantly exceeds Boltzmann floor
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    config:param(tangled_rope_chi_floor, TRFloor),
    Chi >= TRFloor.

%% reform_pressure(+Constraint, -Pressure)
%  Computes reform pressure as the ratio of excess extraction
%  to Boltzmann floor. High pressure = extraction far above
%  necessary minimum. Low pressure = extraction near floor.
%
%  Pressure > 1.0 means extraction is more than double the floor.
%  This mathematically defines when "reform is overdue."
reform_pressure(C, Pressure) :-
    structural_signatures:excess_extraction(C, Excess),
    structural_signatures:boltzmann_floor_for(C, Floor),
    (   Floor > 0.001
    ->  Pressure is Excess / Floor
    ;   % If floor ≈ 0 (natural law), any excess is infinite pressure
        (   Excess > 0.001
        ->  Pressure = 99.0  % Capped sentinel value
        ;   Pressure = 0.0
        )
    ).

/* ================================================================
   3C. PURITY DRIFT EVENT (v5.1)

   Type 10 — Purity Drift (PD)
   A constraint whose purity score declines over time, even if
   ε and χ remain stable. This is the "entropy increase" analogue
   for coordination: without active maintenance, coordination
   structures degrade toward extraction.

   PD(C) ≡
       PurityScore(C) ∈ [0, 1]
       ∧ ∃ decline_signal(C)

   Catches:
   - Bureaucratic accretion (complexity accumulates)
   - Coordination decay (original purpose withers)
   - Creeping entanglement (slow coupling increase)
   - Subtle extraction mechanisms (theater-masked extraction)

   Detection uses temporal signals from underlying metrics
   rather than stored purity history — purity is a derived
   quantity, so we detect its decline via its components.

   SHADOW MODE: Detected and logged. Does not trigger
   classification changes.
   ================================================================ */

%% detect_purity_drift(+ConstraintID)
%  Detects when a constraint's structural purity is declining.
%  Evidence: at least one decline signal in underlying metrics.
detect_purity_drift(C) :-
    structural_signatures:purity_score(C, Purity),
    Purity >= 0.0,
    collect_purity_decline_signals(C, Signals),
    Signals \= [],
    length(Signals, N),
    format('  Drift: Purity Drift in ~w~n', [C]),
    format('    Current purity: ~3f, ~d decline signal(s): ~w~n',
           [Purity, N, Signals]).

%% drift_event/3 clause for purity drift
drift_event(C, purity_drift,
            evidence(current_purity, Purity, decline_signals, Signals)) :-
    structural_signatures:purity_score(C, Purity),
    Purity >= 0.0,
    collect_purity_decline_signals(C, Signals),
    Signals \= [].

%% Context-indexed purity drift
drift_event(C, Context, purity_drift_indexed,
            evidence(current_purity, Purity, chi, Chi, signals, Signals)) :-
    constraint_indexing:valid_context(Context),
    structural_signatures:purity_score(C, Purity),
    Purity >= 0.0,
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    collect_purity_decline_signals(C, Signals),
    Signals \= [].

%% collect_purity_decline_signals(+C, -Signals)
%  Collects evidence of purity decline from temporal metric trends.
%  Each signal corresponds to a purity subscore that's degrading.
collect_purity_decline_signals(C, Signals) :-
    findall(Signal, purity_decline_signal(C, Signal), Signals).

% Signal 1: Extraction creeping up → factorization subscore declining
purity_decline_signal(C, extraction_rising) :-
    metric_trend(C, base_extractiveness, increasing).

% Signal 2: Coupling above threshold → coupling cleanliness declining
purity_decline_signal(C, coupling_above_threshold(Score)) :-
    structural_signatures:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_threshold, Threshold),
    Score > Threshold.

% Signal 3: Theater rising → IB noise increasing, coordination degrading
purity_decline_signal(C, theater_rising) :-
    config:param(theater_metric_name, TM),
    metric_trend(C, TM, increasing).

% Signal 4: Excess extraction above Boltzmann floor
purity_decline_signal(C, excess_above_floor(Excess)) :-
    structural_signatures:excess_extraction(C, Excess),
    Excess > 0.02.

% --- Purity drift severity (v5.1) ---

% Critical: low purity AND multiple decline signals (active degradation)
drift_severity(C, purity_drift, critical) :-
    structural_signatures:purity_score(C, Purity),
    Purity < 0.30,
    collect_purity_decline_signals(C, Signals),
    length(Signals, N), N >= 3, !.

% Warning: borderline purity OR multiple signals
drift_severity(C, purity_drift, warning) :-
    structural_signatures:purity_score(C, Purity),
    Purity < 0.50, !.
drift_severity(C, purity_drift, warning) :-
    collect_purity_decline_signals(C, Signals),
    length(Signals, N), N >= 2, !.

drift_severity(_, purity_drift, watch) :- !.

% Indexed follows same logic
drift_severity(C, purity_drift_indexed, Severity) :-
    drift_severity(C, purity_drift, Severity), !.

/* ================================================================
   3D. NETWORK DRIFT DYNAMICS (v5.2)

   Type 11 — Network Drift (ND)
   A constraint whose effective purity is declining because
   contamination from drifting neighbors is increasing over time.
   This bridges Stage 8 (network topology) with the temporal
   drift engine to detect induced degradation.

   ND(C) ≡
       ∃ neighbor N of C:
           EdgeContamination(N→C) > 0
           ∧ PurityDeclineSignal(N) exists
       ∧ EffectivePurity(C) < IntrinsicPurity(C)

   Key predicates:
   A. detect_network_drift/3       — Top-level detection
   B. network_drift_contagion/3    — Identify contaminating drifting neighbors
   C. network_drift_velocity/4     — Derived EP change rate from network
   D. cascade_prediction/3         — Threshold crossing time predictions
   E. network_stability_assessment/2 — Network-wide classification
   F. network_drift_severity/3     — Severity with hub/multi-source escalation
   G. drift_event/drift_severity   — Integration clauses

   SHADOW MODE: Detected and logged. Does not trigger
   classification changes.

   GRACEFUL DEGRADATION: When only one constraint is loaded,
   all network drift predicates return empty/zero/stable results
   and drift_event(C, network_drift, _) fails — completely
   invisible for single-constraint workflows.
   ================================================================ */

/* ----------------------------------------------------------------
   A. detect_network_drift/3
   Top-level: detects when C's effective purity is declining
   due to contamination from drifting neighbors.
   ---------------------------------------------------------------- */

%% detect_network_drift(+C, +Context, -Evidence)
%  Succeeds if at least one neighbor is both contaminating C
%  and showing purity decline signals.
detect_network_drift(C, Context, evidence(drifting_neighbors, ContagionList, effective_purity, EP, intrinsic_purity, IP)) :-
    constraint_indexing:valid_context(Context),
    drl_modal_logic:effective_purity(C, Context, EP, _Components),
    structural_signatures:purity_score(C, IP),
    IP >= 0.0,
    network_drift_contagion(C, Context, ContagionList),
    ContagionList \= [].

/* ----------------------------------------------------------------
   B. network_drift_contagion/3
   For each neighbor, checks if it is both currently contaminating
   C (Delta > 0, EdgeContam > 0) and showing purity decline signals.
   ---------------------------------------------------------------- */

%% network_drift_contagion(+C, +Context, -ContagionList)
%  Returns [contagion(Neighbor, EdgeContam, DriftSignals), ...]
network_drift_contagion(C, Context, ContagionList) :-
    constraint_indexing:valid_context(Context),
    structural_signatures:purity_score(C, MyPurity),
    (   MyPurity < 0.0
    ->  ContagionList = []
    ;   drl_modal_logic:constraint_neighbors(C, Context, Neighbors),
        findall(
            contagion(Other, EdgeContam, DriftSignals),
            (   member(neighbor(Other, EdgeStrength, _Src), Neighbors),
                compute_neighbor_contamination(MyPurity, Other, EdgeStrength, Context, EdgeContam),
                EdgeContam > 0.0,
                collect_purity_decline_signals(Other, DriftSignals),
                DriftSignals \= []
            ),
            ContagionList)
    ).

%% compute_neighbor_contamination(+MyPurity, +Other, +EdgeStrength, +Context, -EdgeContam)
%  Inline edge contamination using same formula as compute_edge_contamination/7.
compute_neighbor_contamination(MyPurity, Other, EdgeStrength, Context, EdgeContam) :-
    structural_signatures:purity_score(Other, OtherPurity),
    OtherPurity >= 0.0,
    Delta is max(0.0, MyPurity - OtherPurity),
    (   Delta > 0.0
    ->  config:param(purity_attenuation_factor, AttFactor),
        config:param(purity_contamination_cap, Cap),
        (   drl_core:dr_type(Other, Context, OtherType)
        ->  drl_modal_logic:type_contamination_strength(OtherType, TypeFactor)
        ;   TypeFactor = 0.0
        ),
        RawContam is Delta * EdgeStrength * AttFactor * TypeFactor,
        EdgeContam is min(Cap, RawContam)
    ;   EdgeContam = 0.0
    ).
compute_neighbor_contamination(_, _, _, _, 0.0).

/* ----------------------------------------------------------------
   C. network_drift_velocity/4
   Derived rate of effective purity change from network effects.
   For each contaminating neighbor with drift_velocity on
   base_extractiveness > 0, computes sensitivity and contribution.
   ---------------------------------------------------------------- */

%% network_drift_velocity(+C, +Context, -Velocity, -Contributors)
%  Velocity = sum of contributions (positive = degrading).
%  Contributors = [contributor(Neighbor, Rate, Sensitivity, Contribution), ...]
network_drift_velocity(C, Context, Velocity, Contributors) :-
    constraint_indexing:valid_context(Context),
    drl_modal_logic:constraint_neighbors(C, Context, Neighbors),
    structural_signatures:purity_score(C, MyPurity),
    (   MyPurity < 0.0
    ->  Velocity = 0.0, Contributors = []
    ;   drl_core:dr_type(C, Context, MyType),
        drl_modal_logic:type_immunity(MyType, Immunity),
        findall(
            contributor(Other, Rate, Sensitivity, Contribution),
            (   member(neighbor(Other, EdgeStrength, _Src), Neighbors),
                drift_velocity(Other, base_extractiveness, Rate),
                Rate > 0,
                config:param(purity_attenuation_factor, AttFactor),
                (   drl_core:dr_type(Other, Context, OtherType)
                ->  drl_modal_logic:type_contamination_strength(OtherType, TypeFactor)
                ;   TypeFactor = 0.0
                ),
                Sensitivity is EdgeStrength * AttFactor * TypeFactor * Immunity,
                Contribution is Rate * Sensitivity
            ),
            Contributors),
        sum_contributions(Contributors, Velocity)
    ).

%% sum_contributions(+Contributors, -Total)
sum_contributions([], 0.0).
sum_contributions([contributor(_, _, _, C)|Rest], Total) :-
    sum_contributions(Rest, RestTotal),
    Total is C + RestTotal.

/* ----------------------------------------------------------------
   D. cascade_prediction/3
   Given current effective purity and network drift velocity,
   predicts threshold crossings.
   ---------------------------------------------------------------- */

%% cascade_prediction(+C, +Context, -Crossings)
%  Returns [crossing(ThresholdName, ThresholdValue, TimeYears), ...]
%  Only succeeds if velocity >= network_drift_velocity_threshold.
cascade_prediction(C, Context, Crossings) :-
    constraint_indexing:valid_context(Context),
    drl_modal_logic:effective_purity(C, Context, EP, _),
    EP >= 0.0,
    network_drift_velocity(C, Context, Velocity, _),
    config:param(network_drift_velocity_threshold, VelThresh),
    Velocity >= VelThresh,
    findall(
        crossing(Name, Threshold, TimeYears),
        (   cascade_threshold(Name, Threshold),
            EP > Threshold,
            TimeYears is (EP - Threshold) / Velocity
        ),
        Crossings),
    Crossings \= [].

%% cascade_threshold(?Name, ?Value)
%  Threshold lookup from config parameters.
cascade_threshold(sound_floor, V) :-
    config:param(purity_action_sound_floor, V).
cascade_threshold(escalation_floor, V) :-
    config:param(purity_action_escalation_floor, V).
cascade_threshold(degraded_floor, V) :-
    config:param(purity_action_degraded_floor, V).

/* ----------------------------------------------------------------
   E. network_stability_assessment/2
   Network-wide classification of drift status.
   ---------------------------------------------------------------- */

%% network_stability_assessment(+Context, -Assessment)
%  Assessment ∈ {stable, degrading, cascading}
network_stability_assessment(Context, Assessment) :-
    constraint_indexing:valid_context(Context),
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        detect_network_drift(C, Context, _)
    ), DriftingCs),
    length(DriftingCs, NumDrifting),
    (   NumDrifting =:= 0
    ->  Assessment = stable
    ;   % Count how many have critical or warning severity
        findall(C, (
            member(C, DriftingCs),
            network_drift_severity(C, Context, Sev),
            member(Sev, [critical, warning])
        ), SevereCs),
        length(SevereCs, NumSevere),
        config:param(network_cascade_count_threshold, CascadeThresh),
        (   NumSevere >= CascadeThresh
        ->  Assessment = cascading
        ;   Assessment = degrading
        )
    ).

/* ----------------------------------------------------------------
   F. network_drift_severity/3
   Maps effective purity zone to base severity, with hub and
   multi-source escalation.
   ---------------------------------------------------------------- */

%% network_drift_severity(+C, +Context, -Severity)
%  Severity ∈ {critical, warning, watch}
network_drift_severity(C, Context, Severity) :-
    constraint_indexing:valid_context(Context),
    drl_modal_logic:effective_purity(C, Context, EP, _),
    ep_base_severity(EP, BaseSeverity),
    % Hub escalation: C has >= hub_degree_threshold neighbors AND is drifting
    drl_modal_logic:constraint_neighbors(C, Context, Neighbors),
    length(Neighbors, Degree),
    config:param(network_hub_degree_threshold, HubThresh),
    config:param(network_drift_hub_escalation, HubEnabled),
    (   HubEnabled =:= 1, Degree >= HubThresh,
        detect_network_drift(C, Context, _)
    ->  escalate_severity(BaseSeverity, HubEscalated)
    ;   HubEscalated = BaseSeverity
    ),
    % Multi-source escalation: C has >= 2 drifting neighbors
    (   network_drift_contagion(C, Context, ContagionList),
        length(ContagionList, NumSources),
        NumSources >= 2
    ->  escalate_severity(HubEscalated, Severity)
    ;   Severity = HubEscalated
    ).

%% ep_base_severity(+EP, -Severity)
ep_base_severity(EP, critical) :- EP < 0.30, !.
ep_base_severity(EP, warning) :- EP < 0.70, !.
ep_base_severity(_, watch).

%% escalate_severity(+Base, -Escalated)
escalate_severity(watch, warning).
escalate_severity(warning, critical).
escalate_severity(critical, critical).

/* ----------------------------------------------------------------
   G. drift_event and drift_severity clauses
   Following existing patterns exactly.
   ---------------------------------------------------------------- */

%% drift_event/3 — non-indexed (uses default context)
drift_event(C, network_drift, Evidence) :-
    constraint_indexing:default_context(Ctx),
    detect_network_drift(C, Ctx, Evidence).

%% drift_event/4 — context-indexed
drift_event(C, Context, network_drift_indexed,
            evidence(drifting_neighbors, ContagionList, effective_purity, EP, velocity, V)) :-
    constraint_indexing:valid_context(Context),
    detect_network_drift(C, Context, evidence(drifting_neighbors, ContagionList, effective_purity, EP, _, _)),
    network_drift_velocity(C, Context, V, _).

%% drift_severity/3 — dispatches to network_drift_severity/3
drift_severity(C, network_drift, Severity) :-
    constraint_indexing:default_context(Ctx),
    network_drift_severity(C, Ctx, Severity), !.

drift_severity(C, network_drift_indexed, Severity) :-
    constraint_indexing:default_context(Ctx),
    network_drift_severity(C, Ctx, Severity), !.

/* ================================================================
   4. TRANSITION PATH DETECTION
   Detects the canonical degradation paths from core.md.
   ================================================================ */

%% transition_path(+ConstraintID, -FromType, -ToType, -Evidence)
%  Detects a constraint in the process of transitioning between types.
%  Uses current metrics + drift events to infer the transition.

% Rope -> Tangled Rope (extraction accumulating into coordination)
transition_path(C, rope, tangled_rope, evidence(extraction_rising, E, has_coordination, true)) :-
    drl_core:dr_type(C, Type),
    Type = rope,
    safe_metric(C, extractiveness, E),
    config:param(rope_epsilon_ceiling, Ceil),
    E > Ceil * 0.7,  % Within 30% of ceiling
    narrative_ontology:has_coordination_function(C),
    metric_trend(C, base_extractiveness, increasing).

% Tangled Rope -> Snare (coordination dying while extraction grows)
transition_path(C, tangled_rope, snare, evidence(coordination_declining, true, extraction, E)) :-
    drl_core:dr_type(C, Type),
    Type = tangled_rope,
    safe_metric(C, extractiveness, E),
    config:param(snare_epsilon_floor, Floor),
    E > Floor * 0.8,  % Approaching snare threshold
    (   drift_event(C, coordination_loss, _)
    ;   metric_trend(C, coordination_effectiveness, decreasing)
    ).

% Rope -> Piton (direct obsolescence, no extraction phase)
transition_path(C, rope, piton, evidence(function_obsolete, true, theater_high, TR)) :-
    drl_core:dr_type(C, Type),
    Type = rope,
    safe_metric(C, theater_ratio, TR),
    TR > 0.5,
    drift_event(C, function_obsolescence, _).

% Scaffold -> Piton (sunset violation)
transition_path(C, scaffold, piton, evidence(sunset_violated, true)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    drift_event(C, sunset_violation, _).

% Scaffold -> Snare (calcification with extraction)
transition_path(C, scaffold, snare, evidence(extraction_added, E, sunset_violated, Violated)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    safe_metric(C, extractiveness, E),
    config:param(snare_epsilon_floor, Floor),
    E > Floor * 0.7,
    (   drift_event(C, sunset_violation, _)
    ->  Violated = true
    ;   Violated = false
    ).

% Scaffold -> Tangled Rope (extraction added during transition)
transition_path(C, scaffold, tangled_rope, evidence(extraction_emerging, E, coordination_intact, true)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    safe_metric(C, extractiveness, E),
    config:param(tangled_rope_epsilon_floor, Floor),
    E > Floor * 0.7,
    narrative_ontology:has_coordination_function(C).

% Snare -> I-Piton (internalization)
transition_path(C, snare, piton, evidence(internalized, true)) :-
    drl_core:dr_type(C, Type),
    Type = snare,
    drift_event(C, internalized_piton, _).

% Snare -> False Mountain (naturalization)
transition_path(C, snare, false_mountain, evidence(naturalized, true, claimed, mountain)) :-
    drl_core:dr_type(C, Type),
    Type = snare,
    narrative_ontology:constraint_claim(C, mountain).

%% degradation_chain(+ConstraintID, -Chain, -Evidence)
%  Detects multi-step degradation chains by examining measurement history.
%  Returns the full chain of types observed across time points.
degradation_chain(C, Chain, evidence(time_span, T1, T2)) :-
    findall(T-Type,
            (narrative_ontology:measurement(_, C, _, T, _),
             classify_snapshot(C, T, Type)),
            RawPairs),
    RawPairs \= [],
    sort(RawPairs, Sorted),
    pairs_values(Sorted, TypeList),
    deduplicate_consecutive(TypeList, Chain),
    length(Chain, Len),
    Len > 1,
    Sorted = [T1-_|_],
    last(Sorted, T2-_).

%% classify_snapshot(+C, +Time, -Type)
%  Classifies a constraint at a specific time using measurements available.
%  Delegates to drl_core:classify_from_metrics/6 (Single Source of Truth).
classify_snapshot(C, Time, Type) :-
    (   metric_at(C, base_extractiveness, Time, E)
    ->  true
    ;   safe_metric(C, extractiveness, E)
    ->  true
    ;   config:param(default_extractiveness, E)
    ),
    (   metric_at(C, suppression_requirement, Time, S)
    ->  true
    ;   safe_metric(C, suppression_requirement, S)
    ->  true
    ;   config:param(default_suppression, S)
    ),
    constraint_indexing:default_context(Context),
    Context = context(agent_power(Power), _, _, _),
    constraint_indexing:power_modifier(Power, Modifier),
    Chi is E * Modifier,
    drl_core:classify_from_metrics(C, E, Chi, S, Context, Type).

%% predicted_terminal_state(+ConstraintID, -State, -Confidence)
%  Predicts where a constraint is heading based on current drift events
%  and transition paths.
predicted_terminal_state(C, piton, high) :-
    drift_event(C, function_obsolescence, _),
    drift_event(C, extraction_dried_up, _), !.

predicted_terminal_state(C, piton, high) :-
    drift_event(C, internalized_piton, _), !.

predicted_terminal_state(C, piton, medium) :-
    drift_event(C, sunset_violation, _), !.

predicted_terminal_state(C, snare, high) :-
    transition_path(C, tangled_rope, snare, _), !.

predicted_terminal_state(C, snare, medium) :-
    drift_event(C, extraction_accumulation, _),
    drift_event(C, coordination_loss, _), !.

predicted_terminal_state(C, tangled_rope, medium) :-
    transition_path(C, rope, tangled_rope, _), !.

predicted_terminal_state(C, tangled_rope, low) :-
    drift_event(C, extraction_accumulation, _),
    narrative_ontology:has_coordination_function(C), !.

predicted_terminal_state(_, stable, low).

/* ================================================================
   5. DRIFT VELOCITY AND ACCELERATION
   ================================================================ */

%% drift_velocity(+ConstraintID, +Metric, -RatePerYear)
%  Rate of change per year for a given metric.
drift_velocity(C, Metric, Rate) :-
    metric_delta(C, Metric, T1, T2, Delta),
    Duration is T2 - T1,
    Duration > 0,
    Rate is Delta / Duration.

%% drift_acceleration(+ConstraintID, +Metric, -Acceleration)
%  Whether drift is accelerating, decelerating, or constant.
%  Uses three measurement points minimum.
drift_acceleration(C, Metric, Acceleration) :-
    findall(T-V, narrative_ontology:measurement(_, C, Metric, T, V), Pairs),
    sort(Pairs, Sorted),
    length(Sorted, N), N >= 3,
    compute_acceleration(Sorted, Acceleration).

compute_acceleration(Sorted, Acceleration) :-
    Sorted = [T1-V1, T2-V2, T3-V3|_],
    D1 is T2 - T1, D1 > 0,
    D2 is T3 - T2, D2 > 0,
    R1 is (V2 - V1) / D1,
    R2 is (V3 - V2) / D2,
    RateDelta is R2 - R1,
    (   RateDelta > 0.01  -> Acceleration = accelerating
    ;   RateDelta < -0.01 -> Acceleration = decelerating
    ;   Acceleration = constant
    ).

/* ================================================================
   6. SEVERITY CLASSIFICATION
   ================================================================ */

%% drift_severity(+ConstraintID, +EventType, -Severity)
%  Classifies drift severity: critical | warning | watch
%
%  Critical: Active harm likely without intervention.
%  Warning: Degradation in progress, intervention recommended.
%  Watch: Early indicators, monitor closely.

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
    !.  % Coordination loss is always critical (irreversible harm likely)

drift_severity(_, internalized_piton, warning) :- !.

drift_severity(_, extraction_dried_up, warning) :- !.

drift_severity(C, metric_substitution, Severity) :-
    (   safe_metric(C, theater_ratio, TR), TR > 0.7
    ->  Severity = critical
    ;   Severity = warning
    ), !.

drift_severity(_, function_obsolescence, watch) :- !.

% --- Boltzmann drift severity (v5.0) ---

% Coupling drift is critical if coupling is strong AND extraction is high
drift_severity(C, coupling_drift, critical) :-
    structural_signatures:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_strong_threshold, StrongT),
    Score > StrongT,
    safe_metric(C, extractiveness, E),
    config:param(snare_epsilon_floor, SnareFloor),
    E >= SnareFloor, !.
drift_severity(C, coupling_drift, warning) :-
    structural_signatures:cross_index_coupling(C, Score),
    config:param(boltzmann_coupling_strong_threshold, StrongT),
    Score > StrongT, !.
drift_severity(_, coupling_drift, watch) :- !.

% Boltzmann floor drift is informational (necessary complexity)
drift_severity(_, boltzmann_floor_drift, watch) :- !.

% Reform pressure: critical if pressure > 2x, warning if > 1x
drift_severity(C, reform_pressure_detected, critical) :-
    reform_pressure(C, P), P > 2.0, !.
drift_severity(C, reform_pressure_detected, warning) :-
    reform_pressure(C, P), P > 1.0, !.
drift_severity(_, reform_pressure_detected, watch) :- !.

% Coupling drift indexed follows same logic
drift_severity(C, coupling_drift_indexed, Severity) :-
    drift_severity(C, coupling_drift, Severity), !.

% Default
drift_severity(_, _, watch).

/* ================================================================
   7. UNIFIED SCAN
   ================================================================ */

%% scan_constraint_drift(+ConstraintID, -Events)
%  Scans a single constraint for all drift events.
%  Returns list of drift(EventType, Evidence, Severity).
scan_constraint_drift(C, Events) :-
    findall(
        drift(EventType, Evidence, Severity),
        (   drift_event(C, EventType, Evidence),
            drift_severity(C, EventType, Severity)
        ),
        Events
    ).

%% scan_constraint_drift(+ConstraintID, +Context, -Events)
%  Context-indexed scan: includes both standard and indexed drift events.
scan_constraint_drift(C, Context, Events) :-
    findall(
        drift(EventType, Evidence, Severity),
        (   (   drift_event(C, EventType, Evidence)
            ;   drift_event(C, Context, EventType, Evidence)
            ),
            drift_severity(C, EventType, Severity)
        ),
        Events
    ).

%% scan_all_drift(-Report)
%  Scans all known constraints for drift events.
%  Returns report(TotalConstraints, TotalEvents, Critical, Warning, Watch, Details).
scan_all_drift(Report) :-
    findall(C, narrative_ontology:constraint_claim(C, _), AllCs),
    sort(AllCs, Constraints),
    length(Constraints, NumConstraints),
    findall(
        constraint_drift(C, Events),
        (   member(C, Constraints),
            scan_constraint_drift(C, Events),
            Events \= []
        ),
        Details
    ),
    count_by_severity(Details, Critical, Warning, Watch),
    flatten_events(Details, AllEvents),
    length(AllEvents, TotalEvents),
    Report = report(NumConstraints, TotalEvents, Critical, Warning, Watch, Details).

count_by_severity(Details, Critical, Warning, Watch) :-
    flatten_events(Details, AllEvents),
    include(is_severity(critical), AllEvents, Cs), length(Cs, Critical),
    include(is_severity(warning), AllEvents, Ws), length(Ws, Warning),
    include(is_severity(watch), AllEvents, As), length(As, Watch).

is_severity(Sev, drift(_, _, Sev)).

flatten_events([], []).
flatten_events([constraint_drift(_, Events)|Rest], All) :-
    flatten_events(Rest, RestAll),
    append(Events, RestAll, All).

/* ================================================================
   8. DRIFT REPORT GENERATION
   ================================================================ */

%% generate_drift_report/0
%  Generates a full drift event report across all constraints.
generate_drift_report :-
    format('~n================================================================~n'),
    format('  DRIFT EVENT REPORT~n'),
    format('================================================================~n~n'),
    scan_all_drift(report(NumC, NumE, Critical, Warning, Watch, Details)),
    format('  Constraints scanned: ~w~n', [NumC]),
    format('  Total drift events:  ~w~n', [NumE]),
    format('  Critical: ~w | Warning: ~w | Watch: ~w~n~n', [Critical, Warning, Watch]),
    (   Details = []
    ->  format('  No drift events detected.~n')
    ;   forall(member(constraint_drift(C, Events), Details),
              print_constraint_drift(C, Events))
    ),
    format('~n--- Transition Path Analysis ---~n'),
    forall(
        (narrative_ontology:constraint_claim(C2, _), transition_path(C2, From, To, Ev)),
        format('  ~w: ~w -> ~w (~w)~n', [C2, From, To, Ev])
    ),
    format('~n--- Terminal State Predictions ---~n'),
    forall(
        (narrative_ontology:constraint_claim(C3, _), predicted_terminal_state(C3, State, Conf), State \= stable),
        format('  ~w -> ~w (confidence: ~w)~n', [C3, State, Conf])
    ),
    % --- Network Drift Analysis (v5.2) ---
    format('~n--- Network Drift Analysis ---~n'),
    constraint_indexing:default_context(DefaultCtx),
    network_stability_assessment(DefaultCtx, Stability),
    format('  Network stability: ~w~n', [Stability]),
    forall(
        (   narrative_ontology:constraint_claim(C4, _),
            \+ is_list(C4),
            detect_network_drift(C4, DefaultCtx, evidence(drifting_neighbors, CList, effective_purity, EP4, intrinsic_purity, IP4))
        ),
        (   network_drift_severity(C4, DefaultCtx, NDSev),
            format('  ~w [~w]: EP=~3f (intrinsic=~3f)~n', [C4, NDSev, EP4, IP4]),
            forall(member(contagion(Nbr, ECont, _Sigs), CList),
                   format('    <- ~w (edge_contam=~4f)~n', [Nbr, ECont])),
            (   cascade_prediction(C4, DefaultCtx, Crossings)
            ->  forall(member(crossing(TName, TVal, TYears), Crossings),
                       format('    Crosses ~w (~3f) in ~1f years~n', [TName, TVal, TYears]))
            ;   true
            )
        )
    ),
    format('~n================================================================~n').

%% generate_drift_report(+ConstraintID)
%  Generates drift report for a single constraint.
generate_drift_report(C) :-
    format('~n--- Drift Analysis: ~w ---~n', [C]),
    scan_constraint_drift(C, Events),
    (   Events = []
    ->  format('  No drift events detected.~n')
    ;   forall(member(drift(Type, Ev, Sev), Events),
              format('  [~w] ~w: ~w~n', [Sev, Type, Ev]))
    ),
    % Transition paths
    (   transition_path(C, From, To, TEv)
    ->  format('  Transition: ~w -> ~w (~w)~n', [From, To, TEv])
    ;   true
    ),
    % Terminal state prediction
    predicted_terminal_state(C, State, Conf),
    (   State \= stable
    ->  format('  Predicted terminal: ~w (confidence: ~w)~n', [State, Conf])
    ;   format('  No degradation trajectory detected.~n')
    ),
    % Velocity
    (   drift_velocity(C, base_extractiveness, Rate)
    ->  format('  Extraction drift velocity: ~4f/year~n', [Rate])
    ;   true
    ),
    % Non-monotonic trajectory flag
    (   drl_modal_logic:non_monotonic_trajectory(C, base_extractiveness)
    ->  format('  [!] Non-monotonic extraction trajectory (V-shaped or irregular)~n')
    ;   true
    ),
    % Network drift (v5.2)
    constraint_indexing:default_context(DCtx),
    (   detect_network_drift(C, DCtx, evidence(drifting_neighbors, CL, effective_purity, CEP, intrinsic_purity, CIP))
    ->  format('  Network drift: EP=~3f (intrinsic=~3f)~n', [CEP, CIP]),
        forall(member(contagion(Nbr, ECont, _), CL),
               format('    <- ~w (edge_contam=~4f)~n', [Nbr, ECont])),
        (   network_drift_velocity(C, DCtx, NV, _), NV > 0
        ->  format('  Network drift velocity: ~4f/year~n', [NV])
        ;   true
        ),
        (   cascade_prediction(C, DCtx, Crs)
        ->  forall(member(crossing(TN, TV, TY), Crs),
                   format('    Crosses ~w (~3f) in ~1f years~n', [TN, TV, TY]))
        ;   true
        )
    ;   true
    ),
    format('~n').

print_constraint_drift(C, Events) :-
    format('  ~w:~n', [C]),
    forall(member(drift(Type, Evidence, Severity), Events),
           format('    [~w] ~w~n        Evidence: ~w~n', [Severity, Type, Evidence])).

/* ================================================================
   9. HELPER PREDICATES
   ================================================================ */

%% deduplicate_consecutive(+List, -Deduped)
%  Removes consecutive duplicates: [a,a,b,b,a] -> [a,b,a]
deduplicate_consecutive([], []).
deduplicate_consecutive([X], [X]).
deduplicate_consecutive([X,X|Rest], Deduped) :-
    !, deduplicate_consecutive([X|Rest], Deduped).
deduplicate_consecutive([X,Y|Rest], [X|Deduped]) :-
    X \= Y,
    deduplicate_consecutive([Y|Rest], Deduped).

%% last(+List, -Last)
last([X], X).
last([_|T], X) :- last(T, X).

%% pairs_values(+Pairs, -Values)
pairs_values([], []).
pairs_values([_-V|Rest], [V|Vs]) :- pairs_values(Rest, Vs).
