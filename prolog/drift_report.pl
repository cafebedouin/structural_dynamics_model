% ============================================================================
% DRIFT REPORT — Unified Scan & Report Generation
% Split from drl_lifecycle.pl
% ============================================================================

:- module(drift_report, [
    scan_constraint_drift/2,
    scan_constraint_drift/3,
    scan_all_drift/1,
    generate_drift_report/0,
    generate_drift_report/1
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(constraint_indexing).
:- use_module(drift_events).
:- use_module(transition_paths).
:- use_module(network_dynamics).
:- use_module(drl_modal_logic).

/* ================================================================
   7. UNIFIED SCAN
   ================================================================ */

%% scan_constraint_drift(+ConstraintID, -Events)
%  Scans a single constraint for all drift events.
%  Returns list of drift(EventType, Evidence, Severity).
scan_constraint_drift(C, Events) :-
    findall(
        drift(EventType, Evidence, Severity),
        (   drift_events:drift_event(C, EventType, Evidence),
            drift_events:drift_severity(C, EventType, Severity)
        ),
        Events
    ).

%% scan_constraint_drift(+ConstraintID, +Context, -Events)
%  Context-indexed scan: includes both standard and indexed drift events.
scan_constraint_drift(C, Context, Events) :-
    findall(
        drift(EventType, Evidence, Severity),
        (   (   drift_events:drift_event(C, EventType, Evidence)
            ;   drift_events:drift_event(C, Context, EventType, Evidence)
            ),
            drift_events:drift_severity(C, EventType, Severity)
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
        (narrative_ontology:constraint_claim(C2, _), transition_paths:transition_path(C2, From, To, Ev)),
        format('  ~w: ~w -> ~w (~w)~n', [C2, From, To, Ev])
    ),
    format('~n--- Terminal State Predictions ---~n'),
    forall(
        (narrative_ontology:constraint_claim(C3, _), transition_paths:predicted_terminal_state(C3, State, Conf), State \= stable),
        format('  ~w -> ~w (confidence: ~w)~n', [C3, State, Conf])
    ),
    % --- Network Drift Analysis (v5.2) ---
    format('~n--- Network Drift Analysis ---~n'),
    constraint_indexing:default_context(DefaultCtx),
    network_dynamics:network_stability_assessment(DefaultCtx, Stability),
    format('  Network stability: ~w~n', [Stability]),
    forall(
        (   narrative_ontology:constraint_claim(C4, _),
            \+ is_list(C4),
            network_dynamics:detect_network_drift(C4, DefaultCtx, evidence(drifting_neighbors, CList, effective_purity, EP4, intrinsic_purity, IP4))
        ),
        (   network_dynamics:network_drift_severity(C4, DefaultCtx, NDSev),
            format('  ~w [~w]: EP=~3f (intrinsic=~3f)~n', [C4, NDSev, EP4, IP4]),
            forall(member(contagion(Nbr, ECont, _Sigs), CList),
                   format('    <- ~w (edge_contam=~4f)~n', [Nbr, ECont])),
            (   network_dynamics:cascade_prediction(C4, DefaultCtx, Crossings)
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
    (   transition_paths:transition_path(C, From, To, TEv)
    ->  format('  Transition: ~w -> ~w (~w)~n', [From, To, TEv])
    ;   true
    ),
    % Terminal state prediction
    transition_paths:predicted_terminal_state(C, State, Conf),
    (   State \= stable
    ->  format('  Predicted terminal: ~w (confidence: ~w)~n', [State, Conf])
    ;   format('  No degradation trajectory detected.~n')
    ),
    % Velocity
    (   drift_events:drift_velocity(C, base_extractiveness, Rate)
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
    (   network_dynamics:detect_network_drift(C, DCtx, evidence(drifting_neighbors, CL, effective_purity, CEP, intrinsic_purity, CIP))
    ->  format('  Network drift: EP=~3f (intrinsic=~3f)~n', [CEP, CIP]),
        forall(member(contagion(Nbr, ECont, _), CL),
               format('    <- ~w (edge_contam=~4f)~n', [Nbr, ECont])),
        (   network_dynamics:network_drift_velocity(C, DCtx, NV, _), NV > 0
        ->  format('  Network drift velocity: ~4f/year~n', [NV])
        ;   true
        ),
        (   network_dynamics:cascade_prediction(C, DCtx, Crs)
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
