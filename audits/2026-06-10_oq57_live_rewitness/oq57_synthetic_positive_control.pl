% OQ-57 synthetic positive control (2026-06-10).
% Corpus-independent proof that the FIXED internalized_piton clause FIRES
% cleanly when its guard is reached. We assert metrics for a synthetic
% constraint that satisfies eps<0.10 AND theater>0.70 and has no
% requires_active_enforcement fact (so \+ succeeds), then confirm
% drift_event/3 returns evidence WITHOUT throwing. Throwaway process; the
% asserts are never persisted.

:- initialization(main).

main :-
    [stack],
    SynthC = oq57_synthetic_piton,
    assertz(narrative_ontology:constraint_metric(SynthC, extractiveness, 0.05)),
    assertz(narrative_ontology:constraint_metric(SynthC, theater_ratio, 0.85)),
    format("~n=== OQ-57 SYNTHETIC POSITIVE CONTROL ===~n", []),
    format("synthetic constraint: ~w (eps=0.05, theater=0.85, no enforcement fact)~n", [SynthC]),
    ( catch(
        ( findall(Ev, drift_events:drift_event(SynthC, internalized_piton, Ev), Evs),
          ( Evs == []
          -> format("  clause did NOT fire (guard reached but produced no event)~n", [])
          ;  format("  FIRED CLEAN: ~w~n", [Evs]) ) ),
        Err,
        format("  *** THREW: ~w~n", [Err]) ) -> true ; true ),
    format("=== END ===~n", []),
    halt.
main :- format("PROBE FAILED~n", []), halt(1).
