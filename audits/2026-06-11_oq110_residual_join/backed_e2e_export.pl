/* OQ-110 §1.1 — full-corpus in-process residual export.

   Exports temporal_residual for EVERY corpus_loader:corpus_constraint/1
   (the authoritative denominator) through the SAME serializer the pipeline
   uses (json_report:write_temporal_residual/2), in a fresh process, so the
   diff against outputs/pipeline_output.json witnesses that the blocks
   downstream consumers read equal an independent in-engine recomputation on
   the same corpus+code. Constraints with zero measurement/5 facts are
   exported as null, mirroring the exporter gate (json_report.pl:482).

   Run from prolog/:
     swipl -g "consult('../audits/2026-06-11_oq110_residual_join/backed_e2e_export.pl'), run, halt" -t "halt(1)"
*/

/* json_report.pl is a NON-MODULE standalone script (no :- module/2); its
   predicates load into `user`, so write_temporal_residual/2 is called
   unqualified here (this probe file is likewise consulted into `user`). */
:- [stack].
:- [json_report].
:- corpus_loader:ensure_corpus_loaded.

run :-
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    open('../outputs/oq110_residual_inprocess.json', write, S),
    format(S, '{~n', []),
    write_cs(S, Cs),
    format(S, '}~n', []),
    close(S),
    length(Cs, N),
    format("exported ~w constraints to outputs/oq110_residual_inprocess.json~n", [N]).

write_cs(_, []).
write_cs(S, [C]) :- !,
    write_one(S, C),
    format(S, '~n', []).
write_cs(S, [C | R]) :-
    write_one(S, C),
    format(S, ',~n', []),
    write_cs(S, R).

write_one(S, C) :-
    format(S, '"~w": ', [C]),
    (   narrative_ontology:measurement(_, C, _, _, _)
    ->  write_temporal_residual(S, C)
    ;   format(S, 'null', [])
    ).
