:- initialization(main).
:- [stack].
:- use_module(report_generator).
:- [json_report].
:- use_module(narrative_ontology).

main :-
    corpus_loader:ensure_corpus_loaded,
    C = sex_gender_category__identity_reading,
    nl, format("--- plan item 5: omega_from_gap mint witness for ~w ---~n",[C]),
    report_generator:detect_gap_pattern(C, Gap),
    format("gap = ~q~n", [Gap]),
    report_generator:omega_from_gap(C, Gap, OID, OType, Q),
    format("OmegaID  = ~w~n", [OID]),
    format("OType    = ~w~n", [OType]),
    format("Question = ~w~n", [Q]),
    report_generator:omega_severity(OID, Sev),
    format("Severity = ~w~n", [Sev]),

    nl, format("--- collect_omegas (serialization path) for ~w ---~n",[C]),
    collect_omegas(C, Omegas),
    forall(member(O, Omegas), format("  ~q~n", [O])),

    % OPEN-D: gap-omega vs authored omega_variable/3 — show authored set + dedup
    nl, format("--- authored omega_variable/3 facts on ~w ---~n",[C]),
    ( forall(narrative_ontology:omega_variable(OID2, T2, _),
        ( omega_for_constraint(OID2, C)
        -> format("  authored: ~w (~w)~n",[OID2,T2]) ; true ))
      -> true ; true ),
    nl, format("--- corpus-wide: # constraints with gap-omega, # with authored omega ---~n"),
    findall(Cg, (corpus_loader:corpus_constraint(Cg), report_generator:detect_gap_pattern(Cg,_)), Gs),
    length(Gs, NG),
    findall(Ca, (corpus_loader:corpus_constraint(Ca),
                 narrative_ontology:omega_variable(Oa,_,_), omega_for_constraint(Oa,Ca)), As0),
    sort(As0, As), length(As, NA),
    format("  gap-omega constraints: ~w   authored-omega constraints: ~w~n", [NG, NA]),
    halt.
