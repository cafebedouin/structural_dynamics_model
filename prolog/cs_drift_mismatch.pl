% ============================================================================
% CS DRIFT MISMATCH — Cross-Axis False-Mountain Detector
% ============================================================================
% Flags readings that are metric-stable on the network-drift axis but
% CS-foreclosed on the commitment-system axis.
%
% Analogue of the two-hub note's false-mountain hub-conflict: a constraint
% that looks stable by the extraction/purity-network metrics carries a
% foreclosed axiom structure in the CS layer. The "committer axis" is the
% CS drift direction; the "observer axis" in the two-hub note is the
% extraction χ axis.
%
% CS-foreclosed: either
%   (a) cs_drift_trajectory(C, _, axiom_foreclosure) fires
%       — direction axiom_overriding + non-minor magnitude routes to
%         axiom_foreclosure terminal (regardless of acknowledgment for severe;
%         only unacknowledged for substantial)
%   (b) cs_axiom_foreclosed(C, Atom) fires
%       — empirically_contingent axiom + axiom_overriding + non-minor +
%         unacknowledged (strict subset of trajectory-foreclosure set)
%
% Metric-stable: detect_network_contamination is silent (fails for this constraint
%   at the analytical context) AND network_drift_velocity is below threshold.
%   Infrastructure failures are treated as stable (no evidence of drift).
%
% Usage (from prolog/ directory):
%   swipl -g "[cs_drift_mismatch], run_drift_mismatch_report, halt" \
%         -t "halt(1)"
% ============================================================================

:- module(cs_drift_mismatch, [
    cs_drift_mismatch/2,
    run_drift_mismatch_report/0
]).

:- use_module(narrative_ontology).
:- use_module(cs_drift_engine).
:- use_module(cs_axiom_engine).
:- use_module(constraint_indexing).
:- use_module(network_dynamics).
:- use_module(config).
:- use_module(library(lists)).

/* ================================================================
   FORECLOSURE ROUTING
   ================================================================ */

%% cs_drift_mismatch(+UID, -Source)
%  Fires when UID is both CS-foreclosed and metric-stable.
%  UID is the story_uid surrogate (UUIDv4); C is the reading name, looked up for
%  DR metric stability (cs_is_metric_stable remains C-keyed: DR is instance-blind;
%  two instances sharing C both see the same DR stability result — by design).
%  Source encodes which foreclosure paths fired:
%    trajectory_only         — only cs_drift_trajectory → axiom_foreclosure
%    both(trajectory, Atom)  — trajectory + cs_axiom_foreclosed(UID, Atom)
cs_drift_mismatch(UID, Source) :-
    narrative_ontology:cs_story_uid(C, UID),
    \+ is_list(C),
    cs_any_foreclosed(UID, Traj, AxFc),
    % At least one foreclosure path must fire
    (Traj = none, AxFc = none -> fail ; true),
    (   Traj \= none, AxFc \= none
    ->  AxFc = foreclosed_axiom(Atom),
        Source = both(axiom_foreclosure_trajectory, Atom)
    ;   Traj \= none
    ->  Source = trajectory_only
    ;   Source = AxFc
    ),
    cs_is_metric_stable(C).

%% cs_any_foreclosed(+UID, -Traj, -AxFc)
%  Checks both foreclosure paths for UID, returning none if absent.
cs_any_foreclosed(UID, Traj, AxFc) :-
    (cs_drift_engine:cs_drift_trajectory(UID, _, axiom_foreclosure) ->
        Traj = axiom_foreclosure_trajectory
    ;   Traj = none
    ),
    (cs_axiom_engine:cs_axiom_foreclosed(UID, Atom) ->
        AxFc = foreclosed_axiom(Atom)
    ;   AxFc = none
    ).

/* ================================================================
   METRIC STABILITY CHECK
   ================================================================ */

%% cs_is_metric_stable(+C)
%  Succeeds when the network drift machinery sees C as stable.
%  Catches infrastructure failures (missing purity data etc.) and
%  treats them as stable — absence of drift evidence is not positive
%  evidence of drift.
cs_is_metric_stable(C) :-
    constraint_indexing:default_context(Ctx),
    \+ catch(network_dynamics:detect_network_contamination(C, Ctx, _), _, fail),
    (   catch(
            (network_dynamics:network_drift_velocity(C, Ctx, V, _),
             config:param(network_drift_velocity_threshold, Thresh),
             V >= Thresh),
            _, fail)
    ->  fail
    ;   true
    ).

/* ================================================================
   STANDALONE RUNNER
   ================================================================ */

%% run_drift_mismatch_report/0
%  Loads testsets/*.pl, runs the mismatch detector, prints results.
%  Each mismatch is printed on a line starting with "MISMATCH:"
%  for downstream parsing by the Python H¹ analysis driver.
run_drift_mismatch_report :-
    expand_file_name('testsets/*.pl', Files),
    length(Files, NFiles),
    format("Loading ~w testset files...~n", [NFiles]),
    maplist([F]>>(catch(user:consult(F), _, true)), Files),
    findall(UID-Source, cs_drift_mismatch(UID, Source), Pairs),
    sort(Pairs, Mismatches),
    length(Mismatches, N),
    format("~n=== CS Drift Mismatch: cross-axis false-mountain detector ===~n~n"),
    format("Metric-stable + CS-foreclosed: ~w readings~n~n", [N]),
    (Mismatches = []
    ->  format("  None found.~n")
    ;   forall(member(UID-Src, Mismatches),
               ( (narrative_ontology:cs_story_uid(C, UID) -> true ; C = UID),
                 format("MISMATCH: ~w (~w) | ~w~n", [C, UID, Src]) ))
    ),
    nl,
    % Summary by source type
    findall(Src, member(_UID-Src, Mismatches), Srcs),
    count_source(Srcs, trajectory_only, NTrajOnly),
    count_source_prefix(Srcs, both, NBoth),
    format("  trajectory_only: ~w~n", [NTrajOnly]),
    format("  both (+ axiom_foreclosed): ~w~n", [NBoth]).

count_source(Srcs, Target, N) :-
    include(==(Target), Srcs, Matches),
    length(Matches, N).

count_source_prefix(Srcs, Prefix, N) :-
    findall(S, (member(S, Srcs), functor(S, Prefix, _)), Matches),
    length(Matches, N).
