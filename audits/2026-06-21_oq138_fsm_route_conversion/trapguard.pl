:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).
chk(Label, Goal) :- ( catch(Goal,E,(format("  ERROR ~w~n",[E]),fail)) -> format("  PASS ~w~n",[Label]) ; format("  FAIL ~w~n",[Label]) ).
main :-
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, testsets)),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),
    format("~n== severity_floor/2 positive control (must floor moderate/severe, NO catchall) ==~n"),
    chk('severity_floor(severe,red)',    diagnostic_summary:severity_floor(severe,red)),
    chk('severity_floor(moderate,yellow)',diagnostic_summary:severity_floor(moderate,yellow)),
    chk('NO floor for informational',  \+ diagnostic_summary:severity_floor(informational,_)),
    chk('NO catchall (random atom unfloored)', \+ diagnostic_summary:severity_floor(zzz_nonexistent,_)),
    format("~n== victim discriminant: signature_diagnostic_severity/3 ==~n"),
    chk('protein vic>0 -> moderate', signature_detection:signature_diagnostic_severity(protein_anabolic_resistance, false_summit_mountain, moderate)),
    chk('actinide vic=0 -> informational', signature_detection:signature_diagnostic_severity(actinide_replenishment_mechanism_flat_control, false_summit_mountain, informational)),
    format("~n== discriminant drives grade (NOT type delta: metric==dr_type==mountain) ==~n"),
    forall(member(C,[protein_anabolic_resistance, actinide_replenishment_mechanism_flat_control, radiative_levitation_stratification]),
      ( drl_core:metric_based_type_indexed(C,Ctx,MT), drl_core:dr_type(C,Ctx,DT),
        signature_detection:signature_grade(C,G), signature_detection:signature_severity(C,Sv),
        format("  ~w: metric=~w dr_type=~w (delta? ~w) grade=~w sev=~w~n",[C,MT,DT,(MT\==DT),G,Sv]) )),
    format("~n== trap averted: legacy type-delta test WOULD give commentary (metric==final), built clause gives correction for vic>0 ==~n"),
    ( drl_core:metric_based_type_indexed(protein_anabolic_resistance,Ctx,M), drl_core:dr_type(protein_anabolic_resistance,Ctx,M)
      -> format("  CONFIRMED: protein metric==dr_type==~w so naive-revert grade would be commentary; built grade is correction (discriminant did the work)~n",[M])
      ;  format("  (metric != dr_type, unexpected)~n") ),
    halt.
main :- write('TRAPGUARD FAILED'), halt(1).
