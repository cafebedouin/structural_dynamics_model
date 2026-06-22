% OQ-138 constructed-3 generality sweep. Run from prolog/:
%   CORPUS_DIR=testsets swipl -q -g true -t halt ../audits/.../constructed_generality_sweep.pl
:- initialization(main).
:- [stack].
main :-
  getenv('CORPUS_DIR',Dir),
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,Dir)),
  corpus_loader:load_all_testsets,
  constraint_indexing:default_context(Ctx),
  findall(C,(corpus_loader:corpus_constraint(C), signature_detection:constraint_signature(C,constructed_high_extraction)),Hs0),
  sort(Hs0,Hs),
  findall(C,(member(C,Hs), signature_detection:constructed_routed(C)),Routed),
  findall(C,(member(C,Routed), \+ drl_core:dr_type(C,Ctx,unknown)),RoutedNotUnknown),
  findall(C,(member(C,Routed), narrative_ontology:constraint_claim(C,mountain)),MtnRouted),
  findall(C,(member(C,MtnRouted), \+ signature_detection:signature_diagnostic_severity(C,constructed_high_extraction,severe)),MtnNoSevere),
  findall(C,(member(C,Routed), catch(maxent_classifier:maxent_top_type(C,Ctx,tangled_rope),_,fail)),MaxBoosted),
  length(Hs,NH),length(Routed,NR),length(RoutedNotUnknown,NRNU),length(MtnRouted,NMR),length(MtnNoSevere,NMNS),length(MaxBoosted,NMB),
  format("~w: ~w constructed_high | ~w routed | routed-not-unknown=~w (must 0) | mountain-routed=~w mtn-no-severe=~w (must 0) | maxent-top=tangled_rope on ~w/~w routed~n",
         [Dir,NH,NR,NRNU,NMR,NMNS,NMB,NR]),
  halt.
main :- write('SWEEP FAIL'),halt(1).
