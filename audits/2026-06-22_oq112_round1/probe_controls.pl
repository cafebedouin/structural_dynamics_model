% OQ-112 Round-1 two-sided constructed positive controls.
% Run from prolog/:  swipl ../audits/2026-06-22_oq112_round1/probe_controls.pl
:- initialization(main).
main :-
    consult(stack), consult(covering_analysis), consult(maxent_classifier),
    consult(dirac_classification), consult(diagnostic_summary),
    consult(post_synthesis), consult(json_report),
    constraint_indexing:default_context(Ctx),
    ( diagnostic_summary:probe_abductive(fake_no_abd, Ctx, snare, S1) -> true ; S1='FAILED' ),
    format('CTRL :198 no_abd_fact        -> ~w  (expect unavailable: didnt-look fails closed)~n',[S1]),
    assertz(json_report:abd_triggers(fake_empty, [])),
    ( diagnostic_summary:probe_abductive(fake_empty, Ctx, snare, S2) -> true ; S2='FAILED' ),
    format('CTRL :198 abd_triggers([])   -> ~w  (now unavailable; conservative)~n',[S2]),
    assertz(json_report:abd_triggers(fake_gen, [trigger(some_class, 0.9, some_anomaly, genuine)])),
    ( diagnostic_summary:probe_abductive(fake_gen, Ctx, snare, S3) -> true ; S3='FAILED' ),
    format('CTRL :198 genuine trigger    -> ~w  (nonempty path intact)~n',[S3]),
    ( catch(signature_detection:constraint_signature(fake_no_sig,Sg),_,fail) -> true ; Sg='(failed)' ),
    format('CTRL :212 constraint_signature(fake) = ~w  (TOTAL; :212 unreachable)~n',[Sg]),
    halt.
