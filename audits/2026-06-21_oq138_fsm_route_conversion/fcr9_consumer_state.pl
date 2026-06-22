:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).
nvic(C,N):-findall(V,narrative_ontology:constraint_victim(C,V),L),sort(L,Ls),length(Ls,N).
g(G,R):-(catch(G,_,fail)->R=yes;R=no).
probe_sig(C,Sig):- ( catch(diagnostic_summary:probe_signature(C, DetType, Sig0),_,fail) -> Sig=Sig0 ; Sig=err ), DetType=_.
main :-
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, testsets)),
  corpus_loader:load_all_testsets,
  constraint_indexing:default_context(Ctx),
  findall(C,(corpus_loader:corpus_constraint(C), signature_detection:constraint_signature(C,false_ci_rope)),Fs0),
  sort(Fs0,Fs),
  format("~nseat | metric->dr_type | vic | piton_cand | coord_dead | grade/sev | probe_signature_signal~n"),
  forall(member(C,Fs),(
    ( drl_core:metric_based_type_indexed(C,Ctx,MT)->true;MT=err ),
    ( drl_core:dr_type(C,Ctx,DT)->true;DT=err ),
    nvic(C,V), g(narrative_ontology:piton_candidate(C),PC), g(drl_core:coordination_dead(C),CD),
    ( signature_detection:signature_grade(C,G)->true;G=none ),
    ( signature_detection:signature_severity(C,S)->true;S=none ),
    ( catch(diagnostic_summary:probe_signature(C,DT,PS),_,PS=err)->true;PS=err ),
    ( MT==DT -> Tag='' ; Tag='*CHANGED*' ),
    format("  ~w | ~w->~w ~w | vic=~w | piton=~w | dead=~w | ~w/~w | ~w~n",[C,MT,DT,Tag,V,PC,CD,G,S,PS])
  )), halt.
main :- write('FAIL'),halt(1).
