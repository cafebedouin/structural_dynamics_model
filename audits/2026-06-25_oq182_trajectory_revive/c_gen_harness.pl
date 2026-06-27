% C-gen harness: run HAC trajectory clustering on a given corpus, emit the
% family partition + cs_kernel_id map for cross-leg ARI. No engine edits.
% Invoke:
%   swipl -l c_gen_harness.pl -g "run_cgen('testsets_haiku'), halt." -t "halt(1)"
% (cwd = prolog/; modules mirror run_pipeline.py:537-538 + narrative_ontology via stack)

:- initialization(true).

run_cgen(CorpusDir) :-
    % overlay corpus_path with asserta (NOT assertz — silent-ignore trap)
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, CorpusDir)),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorp),
    format(user_error, '[cgen] corpus_path=~w corpus_constraint count=~w~n', [CorpusDir, NCorp]),
    constraint_indexing:default_context(Context),
    context_profile_mining:trajectory_run(Context, _Summary),
    aggregate_all(count, context_profile_mining:family_assignment(_,_), NFam),
    format(user_error, '[cgen] family_assignment count=~w~n', [NFam]),
    % Emit machine-readable lines to stdout
    format('NCORP\t~w\t~w~n', [CorpusDir, NCorp]),
    ( context_profile_mining:family_assignment(C, F),
      format('FAM\t~w\t~w~n', [C, F]),
      fail ; true ),
    ( narrative_ontology:cs_kernel_id(C2, K),
      format('KERN\t~w\t~w~n', [C2, K]),
      fail ; true ).
