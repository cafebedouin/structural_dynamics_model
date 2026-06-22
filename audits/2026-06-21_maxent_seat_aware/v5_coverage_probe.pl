:- initialization(main).
:- [stack].
:- use_module(maxent_classifier).
g(G):-(catch(G,_,fail)->true;fail).
main :-
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,'archives/datasets/original_v5')),
  corpus_loader:load_all_testsets,
  constraint_indexing:default_context(Ctx),
  ( g(maxent_classifier:maxent_run(Ctx,_)) -> RunOK=yes ; RunOK=no ),
  findall(C,(corpus_loader:corpus_constraint(C), g(signature_detection:fcr_routed(C))),R0), sort(R0,R),
  length(R,NR),
  findall(C,(member(C,R), maxent_classifier:maxent_dist(C,Ctx,_)),WithDist),
  length(WithDist,NWD),
  findall(C,(member(C,R), \+ maxent_classifier:maxent_dist(C,Ctx,_)),NoDist),
  format("v5: maxent_run=~w | fcr_routed=~w | routed_with_dist=~w | routed_WITHOUT_dist=~w~n",[RunOK,NR,NWD,NoDist]),
  halt.
main:-write('FAIL'),halt(1).
