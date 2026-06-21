:- initialization(main).
:- [stack].
main :-
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, testsets)),
  corpus_loader:load_all_testsets,
  findall(C, corpus_loader:corpus_constraint(C), Cs),
  constraint_indexing:default_context(Ctx),
  findall(Sig-Ch,
    ( member(C,Cs), signature_detection:constraint_signature(C,Sig),   % UNBOUND: true cascade winner
      member(Sig,[false_ci_rope,coupling_invariant_rope,constructed_low_extraction,
                  constructed_high_extraction,constructed_constraint,coordination_scaffold]),
      drl_core:metric_based_type_indexed(C,Ctx,MT), drl_core:dr_type(C,Ctx,DT),
      ( MT==DT -> Ch=inert ; Ch=changes ) ),
    Pairs),
  forall(member(Sig,[false_ci_rope,coupling_invariant_rope,constructed_low_extraction,
                     constructed_high_extraction,constructed_constraint,coordination_scaffold]),(
    aggregate_all(count, member(Sig-_, Pairs), N),
    aggregate_all(count, member(Sig-changes, Pairs), NC),
    format("~w: ~w true cascade-winners, ~w CHANGE type, ~w inert~n",[Sig,N,NC,N-NC])
  )), halt.
main :- write(fail), halt(1).
