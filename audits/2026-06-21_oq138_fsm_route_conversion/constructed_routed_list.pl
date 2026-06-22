:- initialization(main).
:- [stack].
main :-
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,testsets)),
  corpus_loader:load_all_testsets,
  constraint_indexing:default_context(Ctx),
  findall(C,(corpus_loader:corpus_constraint(C),
             signature_detection:constraint_signature(C,constructed_high_extraction),
             signature_detection:constructed_routed(C)),Rs0),
  sort(Rs0,Rs),
  format("~w constructed_routed seats:~n",[Rs]),
  forall(member(C,Rs),(
    ( drl_core:dr_type(C,Ctx,DT)->true;DT=err ),
    ( narrative_ontology:constraint_claim(C,Claim)->true;Claim=none ),
    ( signature_detection:signature_diagnostic_severity(C,constructed_high_extraction,Sev)->true;Sev=none ),
    ( catch(maxent_classifier:maxent_top_type(C,Ctx,MX),_,MX=err)->true;MX=err ),
    format("  ~w | claim=~w | dr_type=~w | sig_sev=~w | maxent_top(stack)=~w~n",[C,Claim,DT,Sev,MX])
  )), halt.
main :- write(fail),halt(1).
