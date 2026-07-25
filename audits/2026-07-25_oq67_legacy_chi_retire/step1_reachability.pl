% OQ-67 Step 1 — reachability probe for the legacy chi path, one corpus leg per process.
% Usage from prolog/:
%   swipl -g "['../audits/2026-07-25_oq67_legacy_chi_retire/step1_reachability'], run_leg(testsets_haiku), halt" -t "halt(1)"
%
% Per-process controls (C0a/C0b) are re-run in EVERY leg: a forall over an empty or
% mis-overlaid corpus is vacuously true (Build Discipline Pattern 5).

:- [stack].
:- use_module(probe_harness).

run_leg(Dir) :-
    % asserta, NOT assertz — config.pl:502's default is the first clause and wins otherwise.
    asserta(config:param(corpus_path, Dir)),
    config:param(corpus_path, Live),
    format("~n=== LEG ~w (overlay took effect: corpus_path=~w) ===~n", [Dir, Live]),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format("DENOMINATOR N = ~w~n", [N]),
    ( N > 0 -> true ; format("*** N=0, forall would be VACUOUS — STOP ***~n"), fail ),

    % --- per-process positive controls ---
    ( probe_harness:with_asserted(
        [drl_composition:constraint_data(oq67_ctl_a,
            [base_extractiveness(0.9), coord_function(false)])],
        ( drl_composition:is_snare(oq67_ctl_a), drl_composition:is_rope(oq67_ctl_a) ))
    -> format("  control C0a PASS (is_snare + is_rope fire in this process)~n")
    ;  format("  control C0a *** FAIL *** — all-fail below is a fact about the probe~n"), fail ),
    ( probe_harness:with_asserted(
        [drl_composition:constraint_data(oq67_ctl_b,
            [base_extractiveness(0.40), coord_function(false)])],
        drl_composition:is_mountain(oq67_ctl_b))
    -> format("  control C0b PASS (is_mountain fires in this process)~n")
    ;  format("  control C0b *** FAIL *** — all-fail below is a fact about the probe~n"), fail ),

    % --- the reachability claim ---
    check(is_snare,    forall(corpus_loader:corpus_constraint(C), \+ drl_composition:is_snare(C))),
    check(is_mountain, forall(corpus_loader:corpus_constraint(C), \+ drl_composition:is_mountain(C))),
    check(is_rope,     forall(corpus_loader:corpus_constraint(C), \+ drl_composition:is_rope(C))),
    format("=== LEG ~w: N=~w, all three predicates unreachable ===~n", [Dir, N]).

check(Name, Goal) :-
    (   Goal
    ->  format("  ~w: ALL FAIL (unreachable) over the corpus~n", [Name])
    ;   format("  ~w: *** REACHABLE — at least one constraint succeeds — STOP ***~n", [Name]), fail
    ).
