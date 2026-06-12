:- [stack].
:- corpus_loader:ensure_corpus_loaded.
three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).
probe :-
    A = employment_boundary_contradictions,
    constraint_indexing:default_context(Ctx),
    three_way(maxent_classifier:maxent_run(Ctx, _), R0),
    format('W14pre: maxent_run -> ~w~n', [R0]),
    three_way(maxent_classifier:maxent_indexed_run(Ctx, S14), R14),
    format('W14: maxent_indexed_run -> ~w summary=~w~n', [R14, S14]),
    (   maxent_classifier:maxent_indexed_profile(snare, suppression, P14)
    ->  format('W14b indexed profile present: ~w~n', [P14])
    ;   format('W14b indexed profile STILL ABSENT~n')
    ),
    three_way(maxent_classifier:maxent_type_log_likelihood_indexed(A, snare, Ctx, LL15, _), R15),
    format('W15 SINK (indexed): ~w -> ~w LL=~w~n', [A, R15, LL15]).
:- probe, halt.
:- halt(1).
