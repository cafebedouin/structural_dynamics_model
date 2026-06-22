:- initialization(main).
:- [stack].
g(G,R):-(catch(G,_,fail)->R=yes;R=no).
main :-
  getenv('CORPUS_DIR',Dir),
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,Dir)),
  corpus_loader:load_all_testsets,
  constraint_indexing:default_context(Ctx),
  findall(C,(corpus_loader:corpus_constraint(C), signature_detection:constraint_signature(C,false_ci_rope)),Fs0),
  sort(Fs0,Fs),
  findall(C,(member(C,Fs), signature_detection:fcr_routed(C)),Routed),
  findall(C,(member(C,Fs), g(narrative_ontology:piton_candidate(C),yes)),Piton),
  % overlap check (must be empty): no fcr_routed seat is piton
  findall(C,(member(C,Routed), g(narrative_ontology:piton_candidate(C),yes)),Overlap),
  % routed seats must NOT be tangled_rope (they routed to their metric type)
  findall(C,(member(C,Routed), drl_core:dr_type(C,Ctx,tangled_rope)),RoutedStillTR),
  % piton seats must still be piton
  findall(C,(member(C,Piton), \+ drl_core:dr_type(C,Ctx,piton)),PitonNotPiton),
  length(Fs,NF),length(Routed,NR),length(Piton,NP),length(Overlap,NO),length(RoutedStillTR,NRT),length(PitonNotPiton,NPP),
  format("~w: ~w FCR-winners | ~w fcr_routed | ~w piton | routed∩piton=~w (must 0) | routed-still-tangled_rope=~w (must 0) | piton-not-piton=~w (must 0)~n",
         [Dir,NF,NR,NP,NO,NRT,NPP]),
  halt.
main :- write('SWEEP FAIL'),halt(1).
