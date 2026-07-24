:- initialization(main).
:- use_module(library(apply)).
:- use_module(library(lists)).

ctx(context(agent_power(analytical),time_horizon(medium),exit_options(constrained),spatial_scope(national))).

main :-
    [stack], corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs),

    format("~n===== PROBE C: suppression-invariance runtime control =====~n"),
    ctx(Ctx),
    format("Fixed naturalized-profile eps=0.90 chi=0.20, sweep Supp:~n"),
    forall(member(S, [0.0,0.2,0.4,0.55,0.60,0.8,0.95]),
        ( ( drl_core:classify_from_metrics(t,0.90,0.20,S,Ctx,T) -> true ; T='(fail)' ),
          format("  supp=~4f -> ~w~n",[S,T]) )),
    format("Fixed snare-profile-chi eps=0.90 chi=0.70, sweep Supp (gate witness):~n"),
    forall(member(S, [0.0,0.4,0.55,0.59,0.60,0.61,0.8,0.95]),
        ( ( drl_core:classify_from_metrics(t,0.90,0.70,S,Ctx,T) -> true ; T='(fail)' ),
          format("  supp=~4f -> ~w~n",[S,T]) )),

    format("~n===== PROBE A: real domain of a per-seat collapse threshold =====~n"),
    include([C]>>(drl_core:get_raw_suppression(C,V), number(V), V>=0.60), Cs, SuppHi),
    length(SuppHi, NSuppHi),
    include([C]>>(stakeholder_seats:dr_type_for_stakeholder(C,_,naturalized)), Cs, HasNat),
    length(HasNat, NHasNat),
    intersection(SuppHi, HasNat, D1), length(D1, ND1),
    % where the flip is even DEFINED also needs eps>=snare_epsilon_floor 0.46
    include([C]>>(drl_core:base_extractiveness(C,E), E>=0.46), D1, D2), length(D2, ND2),
    format("constraints supp>=0.60                         : ~w~n",[NSuppHi]),
    format("constraints with >=1 naturalized seat reading  : ~w~n",[NHasNat]),
    format("intersection (supp>=0.60 AND naturalized seat) : ~w~n",[ND1]),
    format("  ... AND eps>=0.46 (flip DEFINED)             : ~w~n",[ND2]),
    format("  -> for the other ~w naturalized-seat constraints the collapse threshold is +inf (undefined)~n",
           [NatMinusDef]), NatMinusDef is NHasNat-ND2,

    format("~n===== PROBE D: beneficiary-last violation (authoring-consistency check) =====~n"),
    % constraints with a beneficiary agent seat AND >=2 agent seats
    findall(C-Viol-BenChi-MinChi-MinSeat,
        ( member(C, Cs),
          narrative_ontology:constraint_stakeholder(C,BenName,beneficiary,_,_,_,_),
          \+ narrative_ontology:stakeholder_non_agent(C,BenName),
          stakeholder_seats:chi_for_stakeholder(C,BenName,BenChi),
          findall(N2-Chi2,
              ( narrative_ontology:constraint_stakeholder(C,N2,_,_,_,_,_),
                \+ narrative_ontology:stakeholder_non_agent(C,N2),
                stakeholder_seats:chi_for_stakeholder(C,N2,Chi2) ), Pairs),
          length(Pairs, NP), NP >= 2,
          % min chi seat
          aggregate_all(min(Chi2-N2), member(N2-Chi2, Pairs), MinChi-MinSeat),
          ( MinChi < BenChi -> Viol = violated ; Viol = ok )
        ), Rows),
    include([_-V-_-_-_]>>(V==violated), Rows, Violated),
    length(Rows, NRows), length(Violated, NViol),
    format("constraints with a beneficiary seat + >=2 agent seats : ~w~n",[NRows]),
    format("  beneficiary is NOT the min-chi (most-collapsed) seat : ~w  (beneficiary-last VIOLATED)~n",[NViol]),
    ( NViol > 0 ->
        format("  violators (C / benChi / minChi / minSeat):~n"),
        forall(member(C-_-BC-MC-MS, Violated),
            format("    ~w  ben=~4f  min=~4f @ ~w~n",[C,BC,MC,MS]))
    ; format("  -> NONE. Beneficiary is min-chi in every multi-seat constraint (pure d-artifact).~n") ),
    halt.
