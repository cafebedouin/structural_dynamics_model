% OQ-112 Round-1 empirical probe (read-only). Drives the REAL diagnostic path:
% load stack + corpus, assert abd_triggers via json_report:load_abductive_data,
% then for each corpus constraint record the abductive / signature / maxent probe
% signals and the headline verdict_join verdict. Prints one TSV row per constraint.

:- initialization(main).

main :-
    consult(stack),
    consult(covering_analysis),
    consult(maxent_classifier),
    consult(dirac_classification),
    consult(diagnostic_summary),
    consult(post_synthesis),
    consult(json_report),
    corpus_loader:load_all_testsets,
    json_report:load_abductive_data,           % asserts abd_triggers/2 for the 79
    constraint_indexing:default_context(Ctx),
    format('cid\tdettype\tabductive\tsignature\tmaxent\tjoin_verdict\thas_abd\thas_sig~n', []),
    forall(corpus_loader:corpus_constraint(C),
        report_row(C, Ctx)),
    % tallies
    findall(C, (corpus_loader:corpus_constraint(C),
                \+ json_report:abd_triggers(C, _)), NoAbd),
    length(NoAbd, NNoAbd),
    findall(C, (corpus_loader:corpus_constraint(C),
                \+ catch(signature_detection:constraint_signature(C,_),_,fail)), NoSig),
    length(NoSig, NNoSig),
    format('# NO_ABD_TRIGGERS=~w~n', [NNoAbd]),
    format('# NO_CONSTRAINT_SIGNATURE=~w~n', [NNoSig]),
    halt.

report_row(C, Ctx) :-
    ( catch(drl_core:dr_type(C, Ctx, DetType),_,fail) -> true ; DetType = unknown ),
    ( catch(diagnostic_summary:probe_abductive(C, Ctx, DetType, AbdSig),_,AbdSig=err) -> true ; AbdSig=failed ),
    ( catch(diagnostic_summary:probe_signature(C, DetType, SigSig),_,SigSig=err) -> true ; SigSig=failed ),
    ( catch(diagnostic_summary:probe_maxent(C, Ctx, DetType, MaxSig),_,MaxSig=err) -> true ; MaxSig=failed ),
    ( catch(diagnostic_summary:diagnostic_summary(C, Summary),_,fail),
      catch(diagnostic_summary:verdict_join(C, Summary, Join),_,fail),
      Join = verdict_join(JV,_,_,_,_,_,_) -> true ; JV = none ),
    ( json_report:abd_triggers(C,_) -> HasAbd = yes ; HasAbd = no ),
    ( catch(signature_detection:constraint_signature(C,_),_,fail) -> HasSig = yes ; HasSig = no ),
    sig_atom(AbdSig, A), sig_atom(SigSig, Sg), sig_atom(MaxSig, M),
    format('~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n', [C, DetType, A, Sg, M, JV, HasAbd, HasSig]).

% Compress signal terms to short atoms for the TSV.
sig_atom(agrees, agrees) :- !.
sig_atom(inconclusive, inconclusive) :- !.
sig_atom(unavailable, unavailable) :- !.
sig_atom(agrees_via_override(_), agrees_via_override) :- !.
sig_atom(disagrees(D), Out) :- !, ( compound(D) -> functor(D,F,_), atomic_list_concat([disagrees,F],':',Out) ; Out = disagrees ).
sig_atom(X, X).
