:- initialization(main).
:- [stack].
main :-
  corpus_loader:load_all_testsets,
  constraint_indexing:default_context(Ctx),
  % all FCR-9 signature winners + routed subset
  findall(C, (corpus_loader:corpus_constraint(C),
              signature_detection:constraint_signature(C, false_ci_rope)), FcrAll0),
  sort(FcrAll0, FcrAll),
  findall(C, (member(C, FcrAll), signature_detection:fcr_routed(C)), FcrRouted),
  % all constructed_high winners + routed subset
  findall(C, (corpus_loader:corpus_constraint(C),
              signature_detection:constraint_signature(C, constructed_high_extraction)), ConAll0),
  sort(ConAll0, ConAll),
  findall(C, (member(C, ConAll), signature_detection:constructed_routed(C)), ConRouted),
  format("=== FCR-9 false_ci_rope winners: ~w ===~n", [FcrAll]),
  forall(member(C, FcrAll),
    ( drl_core:dr_type(C, Ctx, DT),
      ( signature_detection:fcr_routed(C) -> R = 'ROUTED' ; R = '.' ),
      format("  ~w  dr_type=~w  ~w~n", [C, DT, R]) )),
  format("=== constructed_high winners: ~w ===~n", [ConAll]),
  forall(member(C, ConAll),
    ( drl_core:dr_type(C, Ctx, DT2),
      ( signature_detection:constructed_routed(C) -> R2 = 'ROUTED' ; R2 = '.' ),
      format("  ~w  dr_type=~w  ~w~n", [C, DT2, R2]) )),
  length(FcrRouted, NFR), length(ConRouted, NCR),
  format("SUMMARY fcr_routed=~w constructed_routed=~w~n", [NFR, NCR]),
  halt.
main :- write('PROBE FAIL'), halt(1).
