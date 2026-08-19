% OQ-302 follow-on: run the HARNESS'S OWN rule-detector over every retract-side
% template used by any committed probe, GENERALIZED (all args fresh variables).
% Generalizing is conservative in the right direction: if the fully-general form
% matches no rule clause, no instantiation of it can either.
:- initialization(ts_main).

% (module, name, arity, where it is used)
tmpl(narrative_ontology, constraint_metric, 3,  'a1_probe, probe_r3, probe_seat_test, oq35, oq112r3, oq190-control').
tmpl(narrative_ontology, constraint_victim, 2,  'a2_probe, oq35, oq190').
tmpl(narrative_ontology, constraint_beneficiary, 2, 'oq122, oq50, probe_r3, oq190').
tmpl(narrative_ontology, constraint_claim, 2,   'oq122, oq35, oq124/149').
tmpl(narrative_ontology, measurement, 5,        'oq110 backed_semantic, pin_counterfactuals').
tmpl(narrative_ontology, stakeholder_gain_flow, 2, 'oq90 phase2, oq190').
tmpl(narrative_ontology, fixing_cost_class, 2,  'oq90 phase2').
tmpl(narrative_ontology, cs_authority_grounding, 2, 'probe_r3, probe_seat_test').
tmpl(narrative_ontology, founding_problem_status, 2, 'oq190').
tmpl(narrative_ontology, disappearance_verdict, 2,   'oq190').
tmpl(narrative_ontology, constraint_stakeholder, 7,  'oq190').
tmpl(config, param, 2,                          'step3_mechanism, oq69 delta, oq285 control_demotion').
tmpl(constraint_indexing, constraint_classification, 3, 'a1_probe.pl:77 (a1_mut_perspective) + probe_harness.pl HEADER EXAMPLE').

% The harness's own test, verbatim (probe_harness.pl:83-86).
harness_says_rules(M:T) :-
    catch(clause(M:T, Body), _, fail),
    Body \== true, !.

check(M, N, A, Where) :-
    functor(T, N, A),
    (   \+ catch(predicate_property(M:T, defined), _, fail)
    ->  format("  n/a      ~w:~w/~w~t~52| NOT DEFINED here — ~w~n", [M,N,A,Where])
    ;   harness_says_rules(M:T)
    ->  findall(B, catch(clause(M:T,B),_,fail), Bs),
        include(==(true), Bs, Fs), length(Bs,NB), length(Fs,NF), NR is NB-NF,
        format("  ** RULE ~w:~w/~w~t~52| clauses=~w facts=~w RULES=~w — ~w~n",
               [M,N,A,NB,NF,NR,Where])
    ;   findall(B, catch(clause(M:T,B),_,fail), Bs), length(Bs,NB),
        format("  safe     ~w:~w/~w~t~52| ~w fact-clause(s), 0 rules — ~w~n", [M,N,A,NB,Where])
    ).

ts_main :-
    format("=== retract-side template safety, harness's own detector, generalized ===~n"),
    forall(tmpl(M,N,A,W), check(M,N,A,W)),
    format("~n=== POSITIVE CONTROL: the detector must FIRE on a known rule ===~n"),
    (   harness_says_rules(boltzmann_compliance:boltzmann_invariant_mountain(_,_))
    ->  format("  detector FIRED on boltzmann_invariant_mountain/2 (a known rule)  OK~n")
    ;   format("  detector DID NOT FIRE on a known rule -- INSTRUMENT BROKEN~n") ),
    format("=== NEGATIVE CONTROL: must DECLINE on a known pure-fact table ===~n"),
    (   harness_says_rules(config:param(_,_))
    ->  format("  detector fired on config:param/2 -- INSTRUMENT OVER-PERMISSIVE~n")
    ;   format("  detector DECLINED on config:param/2 (184 facts, 0 rules)  OK~n") ),
    halt.
