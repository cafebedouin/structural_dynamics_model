% C-gen successor harness: emit FAM (family), KERN (cs_kernel_id), and SHIFT
% (identity-derived fingerprint_shift) per constraint, for the substrate read.
% Invoke per leg:
%   swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
%         -l maxent_classifier.pl -l context_profile_mining.pl \
%         -l <this> -g "run_cgen_shift('testsets_haiku'), halt." -t "halt(1)"

run_cgen_shift(CorpusDir) :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, CorpusDir)),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorp),
    format(user_error, '[cgen2] corpus_path=~w corpus_constraint=~w~n', [CorpusDir, NCorp]),
    constraint_indexing:default_context(Context),
    context_profile_mining:trajectory_run(Context, _),
    aggregate_all(count, context_profile_mining:family_assignment(_,_), NFam),
    format(user_error, '[cgen2] family_assignment=~w~n', [NFam]),
    format('NCORP\t~w\t~w~n', [CorpusDir, NCorp]),
    ( context_profile_mining:family_assignment(C, F),
      format('FAM\t~w\t~w~n', [C, F]), fail ; true ),
    ( narrative_ontology:cs_kernel_id(C2, K),
      format('KERN\t~w\t~w~n', [C2, K]), fail ; true ),
    ( corpus_loader:corpus_constraint(C3),
      ( logical_fingerprint:fingerprint_shift(C3, S) -> true ; S = none ),
      format('SHIFT\t~w\t~q~n', [C3, S]), fail ; true ).
