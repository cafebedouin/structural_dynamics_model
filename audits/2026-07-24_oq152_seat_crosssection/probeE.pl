:- initialization(main).
:- use_module(library(lists)).
main :-
    [stack], corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs), length(Cs,NC),
    % (1) DENOMINATOR FIX: distinct constraints with a beneficiary seat AND >=2 agent seats
    findall(C, ( member(C,Cs),
                 narrative_ontology:constraint_stakeholder(C,_,beneficiary,_,_,_,_),
                 aggregate_all(count,
                     ( narrative_ontology:constraint_stakeholder(C,N,_,_,_,_,_),
                       \+ narrative_ontology:stakeholder_non_agent(C,N) ), NA), NA>=2
               ), DC0), sort(DC0,DC), length(DC,NDC),
    format("~n(1) corpus constraints=~w ; distinct constraints w/ beneficiary seat + >=2 agent seats=~w~n",[NC,NDC]),
    % (2) per-CONSTRAINT min-chi seat role (one vote per constraint; if multiple beneficiary seats, still one constraint)
    findall(MinRole,
        ( member(C,DC),
          findall(Chi-Role, ( narrative_ontology:constraint_stakeholder(C,N,Role,_,_,_,_),
                              \+ narrative_ontology:stakeholder_non_agent(C,N),
                              stakeholder_seats:chi_for_stakeholder(C,N,Chi) ), Ps),
          keysort(Ps,S), S=[_-MinRole|_] ), MRs),
    msort(MRs,MRs1), clumped(MRs1,RoleTally),
    format("(2) per-CONSTRAINT min-chi seat role tally: ~w  (sums to ~w)~n",[RoleTally,NDC]),
    % (3) BAND STRUCTURE: when agenda_setter is min-chi, what TYPE does it read vs the beneficiary?
    findall(ASType-BenType,
        ( member(C,DC),
          findall(Chi-N-Role, ( narrative_ontology:constraint_stakeholder(C,N,Role,_,_,_,_),
                               \+ narrative_ontology:stakeholder_non_agent(C,N),
                               stakeholder_seats:chi_for_stakeholder(C,N,Chi) ), Ps),
          keysort(Ps,S), S=[_-MinN-agenda_setter|_],
          stakeholder_seats:dr_type_for_stakeholder(C,MinN,ASType),
          once(( narrative_ontology:constraint_stakeholder(C,BN,beneficiary,_,_,_,_),
                 stakeholder_seats:dr_type_for_stakeholder(C,BN,BenType) ))
        ), Pairs),
    msort(Pairs,P1), clumped(P1,BandTally),
    format("(3) when agenda_setter is min-chi: (agenda_setter_type - beneficiary_type) tally:~n    ~w~n",[BandTally]),
    % (4) EMPIRICAL bridgeability: observed chi-multiplier [f(d)*sigma] range per role
    format("(4) observed per-role chi range across corpus seats:~n"),
    forall(member(R,[agenda_setter,beneficiary,observer,payer,excluded]),
        ( findall(Chi, ( member(C,Cs),
                         narrative_ontology:constraint_stakeholder(C,N,R,_,_,_,_),
                         \+ narrative_ontology:stakeholder_non_agent(C,N),
                         stakeholder_seats:chi_for_stakeholder(C,N,Chi) ), Xs),
          ( Xs=[] -> format("    ~w: (none)~n",[R])
          ; min_list(Xs,Mn), max_list(Xs,Mx), length(Xs,LN),
            format("    ~w: n=~w  chi in [~4f, ~4f]~n",[R,LN,Mn,Mx]) ) )),
    halt.
