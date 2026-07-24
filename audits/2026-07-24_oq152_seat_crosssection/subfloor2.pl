:- initialization(main).
:- use_module(library(lists)).
main :-
    [stack], corpus_loader:load_all_testsets,
    config:param(extractiveness_metric_name,EM),
    findall(C-AN-BN,
        ( corpus_loader:corpus_constraint(C),
          findall(Chi-N-Role, ( narrative_ontology:constraint_stakeholder(C,N,Role,_,_,_,_),
                               \+ narrative_ontology:stakeholder_non_agent(C,N),
                               stakeholder_seats:chi_for_stakeholder(C,N,Chi) ), Ps),
          Ps \= [], keysort(Ps,S), S=[_-AN-agenda_setter|_],
          stakeholder_seats:dr_type_for_stakeholder(C,AN,naturalized),
          once(( narrative_ontology:constraint_stakeholder(C,BN,beneficiary,_,_,_,_),
                 stakeholder_seats:dr_type_for_stakeholder(C,BN,naturalized) ))
        ), Twelve),
    length(Twelve, NT),
    format("~ncases (agenda min-chi & both naturalized) = ~w~n",[NT]),
    % Correct verdict: is the agenda<beneficiary ordering fixed by role->d, and is the
    % metric-type (pre-signature) already naturalized (i.e. no signature override involved)?
    findall(V,
        ( member(C-AN-BN,Twelve),
          stakeholder_seats:derive_directionality_for_stakeholder(C,AN,AD),
          stakeholder_seats:derive_directionality_for_stakeholder(C,BN,BD),
          stakeholder_seats:chi_for_stakeholder(C,AN,AChi),
          stakeholder_seats:chi_for_stakeholder(C,BN,BChi),
          % metric-type (no signature layer) for the agenda seat
          stakeholder_seats:stakeholder_context(C,AN,ACtx),
          drl_core:base_extractiveness(C,AEps),
          drl_core:get_raw_suppression(C,ASupp),
          drl_core:classify_from_metrics(C,AEps,AChi,ASupp,ACtx,AMetricType),
          ( (AD < BD, AChi < BChi, AMetricType == naturalized) -> V=config
          ; (AD < BD, AChi < BChi) -> V=config_via_signature(AMetricType)
          ; V=seat_relative ),
          format("    ~w  Ad=~4f<Bd=~4f? Achi=~5f<Bchi=~5f? agenda_metric_type=~w -> ~w~n",
                 [C,AD,BD,AChi,BChi,AMetricType,V])
        ), Vs),
    include([X]>>(X==seat_relative), Vs, SR), length(SR,NSR),
    include([X]>>(X==config), Vs, CF), length(CF,NCF),
    format("~n  config (d-ordering + metric-naturalized): ~w~n",[NCF]),
    format("  config via signature layer               : ~w~n",[NT-NCF-NSR]),
    format("  genuinely seat-relative                  : ~w~n",[NSR]),
    halt.
