:- initialization(cl_main).

show(Label, Goal) :-
    (   catch(Goal, E, (format("~w: THREW ~q~n", [Label, E]), fail))
    ->  format("~w: ok~n", [Label])
    ;   format("~w: failed-or-threw~n", [Label])
    ).

cl_main :-
    format("--- flags ---~n"),
    forall(member(F, [access_level, unknown, protect_static_code]),
           ( catch(current_prolog_flag(F, V), _, V = '<no such flag>'),
             format("flag ~w = ~q~n", [F, V]) )),

    format("--- target predicate properties ---~n"),
    forall(member(P, [dynamic, static, defined, foreign, system, built_in, number_of_clauses(_)]),
           ( catch(predicate_property(
                     boltzmann_compliance:boltzmann_invariant_mountain(_,_), P), _, fail)
             -> format("  bim/2 HAS ~w~n", [P])
             ;  format("  bim/2 lacks ~w~n", [P]) )),

    format("--- clause/2 on the target (static, user module) ---~n"),
    (   catch(( clause(boltzmann_compliance:boltzmann_invariant_mountain(H1, H2), B1),
                format("  clause1 head=(~q,~q) body=~q~n", [H1,H2,B1]) ), E1,
              format("  THREW ~q~n", [E1]))
    ->  true ; format("  failed~n") ),

    format("--- control: clause/2 on a SYSTEM predicate (must be refused) ---~n"),
    (   catch(( clause(lists:append(_,_,_), _B2), format("  system clause/2 SUCCEEDED~n") ), E2,
              format("  system clause/2 THREW ~q~n", [E2]))
    ->  true ; format("  system clause/2 failed~n") ),
    (   catch(( clause(system:atom_length(_,_), _B3), format("  builtin clause/2 SUCCEEDED~n") ), E3,
              format("  builtin clause/2 THREW ~q~n", [E3]))
    ->  true ; format("  builtin clause/2 failed~n") ),

    format("--- did anything in this session make bim/2 dynamic? ---~n"),
    (   predicate_property(boltzmann_compliance:boltzmann_invariant_mountain(_,_), (dynamic))
    ->  format("  DYNAMIC — the guidance would be wrong~n")
    ;   format("  STATIC — clause/2 read a compiled static predicate~n") ),
    halt.
