:- initialization(main).
:- use_module(library(lists)).
highd(payer). highd(excluded). highd(observer).
main :-
    [stack], corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    % For each constraint with a beneficiary seat: does any HIGH-d role seat have chi < beneficiary chi?
    findall(C-BenName-BenChi-HN-HR-HChi,
        ( member(C, Cs),
          narrative_ontology:constraint_stakeholder(C,BenName,beneficiary,_,_,_,_),
          \+ narrative_ontology:stakeholder_non_agent(C,BenName),
          stakeholder_seats:chi_for_stakeholder(C,BenName,BenChi),
          narrative_ontology:constraint_stakeholder(C,HN,HR,_,_,_,_),
          highd(HR),
          \+ narrative_ontology:stakeholder_non_agent(C,HN),
          stakeholder_seats:chi_for_stakeholder(C,HN,HChi),
          HChi < BenChi
        ), Anoms),
    sort(Anoms, AnomsU),
    length(AnomsU, NAnom),
    % distinct constraints
    findall(C, member(C-_-_-_-_-_, AnomsU), ACs0), sort(ACs0, ACs), length(ACs, NAC),
    format("~n===== PROBE D (role-aware, menu-b): high-d role seat chi < beneficiary chi =====~n"),
    format("anomalous (constraint,ben,high-d-seat) tuples : ~w~n",[NAnom]),
    format("distinct constraints with such an anomaly     : ~w~n",[NAC]),
    ( NAnom > 0 ->
        forall(member(C-BN-BC-HN-HR-HC, AnomsU),
            format("  ~w  ben=~4f  ~w(~w)=~4f~n",[C,BC,HN,HR,HC]))
    ; format("  -> NONE: no payer/excluded/observer seat is more-collapsed than the beneficiary.~n") ),
    % Also: role of the overall min-chi seat, to show the 270 are mostly agenda_setter
    format("~n--- role composition of the overall min-chi seat (the 270 from probe D v1) ---~n"),
    findall(MinRole,
        ( member(C, Cs),
          narrative_ontology:constraint_stakeholder(C,BN2,beneficiary,_,_,_,_),
          \+ narrative_ontology:stakeholder_non_agent(C,BN2),
          findall(Chi2-Role2,
              ( narrative_ontology:constraint_stakeholder(C,N2,Role2,_,_,_,_),
                \+ narrative_ontology:stakeholder_non_agent(C,N2),
                stakeholder_seats:chi_for_stakeholder(C,N2,Chi2) ), Ps),
          length(Ps,NPn), NPn>=2, keysort(Ps,SS), SS=[_-MinRole|_]
        ), MinRoles),
    msort(MinRoles, MR), clumped(MR, Clumps),
    format("  min-chi seat role tally: ~w~n",[Clumps]),
    halt.
