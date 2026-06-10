% OQ-57 archive re-witness (2026-06-10). Exercises the internalized_piton
% clause on a PRE-RESET archived corpus (where the original 2026-06-04
% emitters actually lived), overlaying corpus_path. Set OQ57_CORPUS env to
% the archive dir (relative to prolog/), e.g. archives/datasets/kernel_v1.
% Read-only. Run from prolog/.

:- initialization(main).

reaches_piton_guard(C, E, TR) :-
    drift_events:safe_metric(C, extractiveness, E), E < 0.10,
    drift_events:safe_metric(C, theater_ratio, TR), TR > 0.70.

main :-
    [stack],
    getenv('OQ57_CORPUS', Dir),
    ( retract(config:param(corpus_path, _)) -> true ; true ),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    length(Cs, N),
    format("~n=== OQ-57 ARCHIVE RE-WITNESS: ~w ===~n", [Dir]),
    format("corpus_constraint denominator: ~w~n", [N]),

    % reachability (positive control on real data)
    findall(C-E-TR, reaches_piton_guard(C, E, TR), Piton),
    length(Piton, NP),
    format("~n[reachability] internalized_piton guard reached by: ~w constraint(s)~n", [NP]),
    forall(member(C-E-TR, Piton),
        format("  REACHES: ~w  (eps=~4f, theater=~4f)~n", [C, E, TR])),

    % symptom: drift_event/3 must return clean (the OQ-57 bug threw here)
    format("~n[symptom] drift_event(C, internalized_piton, _) on reaching constraints:~n", []),
    ( Piton == []
    -> format("  (none reach guard -- unexercised on this archive)~n", [])
    ;  forall(member(C3-_-_, Piton),
        ( catch(
            ( findall(Ev, drift_events:drift_event(C3, internalized_piton, Ev), Evs),
              format("  CLEAN: ~w -> ~w~n", [C3, Evs]) ),
            Err, format("  *** THREW on ~w: ~w~n", [C3, Err]) )
        ))
    ),

    % full-scan throw check across the whole archive
    format("~n[full-scan] drift_event/3 over all ~w constraints, any type:~n", [N]),
    ( catch(
        ( findall(_, (member(C4, Cs), drift_events:drift_event(C4, _, _)), Evs2),
          length(Evs2, NE),
          format("  full enumeration completed with NO throw (~w drift events total)~n", [NE]) ),
        ScanErr, format("  *** SCAN THREW: ~w~n", [ScanErr]) ) -> true ; true ),
    format("=== END ~w ===~n", [Dir]),
    halt.
main :- format("ARCHIVE PROBE FAILED (is OQ57_CORPUS set?)~n", []), halt(1).
