:- initialization(main).
:- use_module(library(lists)).
main :-
    [stack], corpus_loader:load_all_testsets,
    format("~n(a) sigmoid_f ground truth (run in engine, k/params from config):~n"),
    forall(member(D,[0.09,0.12,0.164,0.17,0.22,0.25,0.30,0.69,0.72,0.77,0.85,0.90]),
        ( constraint_indexing:sigmoid_f(D,F),
          format("    f(~4f) = ~6f~n",[D,F]) )),
    format("~n    boundary: f(D)=0 near D=~4f (below -> negative PowerMod)~n",[0.164]),

    format("~n(b) decomposition of 3 most-negative agenda_setter seats (chi = eps * f(D_eff) * sigma):~n"),
    findall(Chi-C-N,
        ( corpus_loader:corpus_constraint(C),
          narrative_ontology:constraint_stakeholder(C,N,agenda_setter,_,_,_,_),
          \+ narrative_ontology:stakeholder_non_agent(C,N),
          stakeholder_seats:chi_for_stakeholder(C,N,Chi) ), Ps0),
    keysort(Ps0,Sorted), length(Sorted,_),
    take3(Sorted,Three),
    forall(member(Chi-C-N,Three),
        ( narrative_ontology:constraint_stakeholder(C,N,Role,_,_,Exit,Scope),
          config:param(extractiveness_metric_name,EM),
          narrative_ontology:constraint_metric(C,EM,Eps),
          stakeholder_seats:derive_directionality_for_stakeholder(C,N,D),
          constraint_indexing:resolve_displacement(analytical,_Ignore),
          constraint_indexing:sigmoid_f(D,F),
          constraint_indexing:scope_modifier(Scope,Sig),
          Recompute is Eps*F*Sig,
          format("    ~w/~w role=~w exit=~w scope=~w~n",[C,N,Role,Exit,Scope]),
          format("      eps=~4f  d=~4f  f(d)=~6f  sigma=~4f  =>  eps*f*sigma=~6f   chi_for_stakeholder=~6f~n",
                 [Eps,D,F,Sig,Recompute,Chi]) )),

    format("~n(c) agent_power inertness control (same D & scope, different power atom -> same chi?):~n"),
    Ctx1 = context(agent_power(powerless),  time_horizon(medium),exit_options(constrained),spatial_scope(national)),
    Ctx2 = context(agent_power(institutional),time_horizon(medium),exit_options(constrained),spatial_scope(national)),
    ( corpus_loader:corpus_constraint(TC), narrative_ontology:constraint_metric(TC,extractiveness,_) -> true ; TC = catholic_church_1200 ),
    constraint_indexing:extractiveness_for_agent_d(TC,Ctx1,0.50,Chi1),
    constraint_indexing:extractiveness_for_agent_d(TC,Ctx2,0.50,Chi2),
    format("    constraint=~w  D=0.50 scope=national:  power=powerless->chi=~6f   power=institutional->chi=~6f~n",[TC,Chi1,Chi2]),
    ( abs(Chi1-Chi2)<1.0e-9 -> format("    => IDENTICAL: agent_power is inert for seat chi (uniform Delta=0)~n")
    ; format("    => DIFFER: power DOES enter seat chi~n") ),

    format("~n(d) base_extractiveness (eps) range across corpus:~n"),
    findall(E, ( corpus_loader:corpus_constraint(C2), drl_core:base_extractiveness(C2,E) ), Es),
    min_list(Es,EMn), max_list(Es,EMx), length(Es,NE),
    format("    n=~w  eps in [~4f, ~4f]~n",[NE,EMn,EMx]),
    halt.

take3([A,B,C|_],[A,B,C]) :- !.
take3(L,L).
