% OQ-112 item 4 — v3 addendum: complete W11 (indexed sink) properly.
% v2's W11 "success LL=-0.54" was profile-absent: maxent_indexed_profile/3 is
% populated only by maxent_indexed_run (json_report.pl:76 wraps that call in
% catch(_, true)). Drive the indexed run first, witness the profile, then the
% sink three-way.

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

    three_way(maxent_classifier:maxent_indexed_run(Ctx, S14), R14),
    format('W14: maxent_indexed_run -> ~w summary=~w~n', [R14, S14]),
    (   maxent_classifier:maxent_indexed_profile(snare, suppression, P14)
    ->  format('W14b indexed profile present: ~w~n~n', [P14])
    ;   format('W14b indexed profile STILL ABSENT~n~n')
    ),

    three_way(maxent_classifier:maxent_type_log_likelihood_indexed(A, snare, Ctx, LL15, _), R15),
    format('W15 SINK (indexed, profiles present): ~w -> ~w LL=~w~n~n', [A, R15, LL15]),

    % the boundary as json_report.pl:76 wraps it
    (   catch(maxent_classifier:maxent_type_log_likelihood_indexed(A, snare, Ctx, _, _), _, true)
    ->  format('W16 json_report-style catch(_, true): SUCCEEDS VACUOUSLY (throw absorbed)~n')
    ;   format('W16 json_report-style catch(_, true): failed~n')
    ).

:- probe, halt.
:- halt(1).
