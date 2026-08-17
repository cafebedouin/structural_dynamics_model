% control_demotion.pl — DISCRIMINATION CONTROL for the A1 measurement.
%
% A1 reports "the signature layer DEMOTED a real metric type to `unknown`: 0
% seats". A zero is only informative if the probe can see a demotion when one
% exists. This control makes one exist and re-runs the SAME counting code.
%
% Mechanism: `false_summit_override_target` (config.pl:506) is a live ablation
% lever whose committed default is `mountain` (= no overwrite). Set it to
% `unknown` and the `resolve_modal_signature_conflict(mountain,
% false_summit_mountain, Target)` clause (signature_detection.pl:967-974) turns
% a REAL metric type into `unknown` — exactly the cell A1 reports empty.
%
% Strength: this is an ABLATION-LEVER control, not an authored decoy — it uses
% a committed config switch on real corpus data, and the demoting clause is the
% engine's own. It is still one leg and one signature, so it licenses "the
% counter fires on a real demotion when one occurs", not "all demotion paths
% are covered".
%
% Read-only: with_overlay/3 snapshots and restores; nothing is written.

:- use_module(library(lists)).

count_transitions(Rescued, Demoted, Kept, RealReal) :-
    findall(MT-Tok,
        (   corpus_loader:corpus_constraint(C),
            stakeholder_seats:stakeholder_agent_seats(C, Ns),
            member(N, Ns),
            (   seat_metric_type(C, N, MT) -> true ; MT = fails ),
            stakeholder_seats:seat_type_token(C, N, Tok)
        ), Pairs),
    include([unknown-T]>>(T \== unknown), Pairs, R), length(R, Rescued),
    include([M-unknown]>>(M \== unknown), Pairs, D), length(D, Demoted),
    include(==(unknown-unknown), Pairs, K), length(K, Kept),
    include([M-T]>>(M \== unknown, T \== unknown), Pairs, RR), length(RR, RealReal).

report(Label) :-
    count_transitions(R, D, K, RR),
    format("~w~n", [Label]),
    format("    metric unknown -> final REAL     (RESCUE)  ~d~n", [R]),
    format("    metric REAL    -> final unknown  (DEMOTE)  ~d~n", [D]),
    format("    metric unknown -> final unknown  (kept)    ~d~n", [K]),
    format("    metric REAL    -> final REAL               ~d~n", [RR]).

run_control :-
    format("=== BASELINE (committed config: false_summit_override_target=mountain) ===~n"),
    report('  baseline'),
    nl,
    format("=== PLANTED (false_summit_override_target=unknown) ===~n"),
    probe_harness:with_overlay(
        [config:param(false_summit_override_target, _)],
        [config:param(false_summit_override_target, unknown)],
        report('  planted')),
    nl,
    format("=== RESTORED (must equal baseline) ===~n"),
    report('  restored').
