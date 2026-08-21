% OQ-326 Phase 2, mechanical arm.
% Evaluates every distinct retract-side TEMPLATE (generalized to fresh args) and
% every assert-side PREDICATE on the five checks the strict harness will apply.
% Run: cd prolog && swipl -g "[stack], corpus_loader:load_all_testsets,
%        consult('<this>'), run_eval, halt" -t "halt(1)"

:- use_module(library(lists)).

% --- side(Kind, Module, Functor, Arity) --------------------------------------
side(retract, config, param, 2).
side(retract, constraint_data, base_extractiveness, 2).
side(retract, drl_core, base_extractiveness, 2).
side(retract, narrative_ontology, constraint_beneficiary, 2).
side(retract, narrative_ontology, constraint_claim, 2).
side(retract, narrative_ontology, constraint_metric, 3).
side(retract, narrative_ontology, constraint_victim, 2).
side(retract, narrative_ontology, cs_authority_grounding, 2).
side(retract, narrative_ontology, fixing_cost_class, 2).
side(retract, narrative_ontology, measurement, 5).
side(retract, narrative_ontology, stakeholder_gain_flow, 2).
side(retract, constraint_indexing, constraint_classification, 3).
side(retract, narrative_ontology, constraint_stakeholder, 7).
side(retract, narrative_ontology, founding_problem_status, 2).
side(retract, narrative_ontology, disappearance_verdict, 2).
side(assert,  constraint_indexing, directionality_override, 3).
side(assert,  domain_priors, emerges_naturally, 1).
side(assert,  drl_composition, agent_index, 2).
side(assert,  drl_composition, constraint_data, 2).
side(assert,  maxent_classifier, maxent_dist, 3).
side(assert,  narrative_ontology, cs_axiom, 3).
side(assert,  narrative_ontology, cs_drift_state, 3).
side(assert,  narrative_ontology, cs_axiom_grounding, 3).

% --- the five checks ---------------------------------------------------------
resolvable(M:T, Status) :-
    (   catch(predicate_property(M:T, defined), _, fail)
    ->  Status = ok
    ;   functor(T, N, _),
        (   current_predicate(M:N/A2)
        ->  Status = arity_mismatch(A2)
        ;   Status = undefined
        )
    ).

is_dynamic(M:T, Status) :-
    ( catch(predicate_property(M:T, (dynamic)), _, fail) -> Status = yes ; Status = no ).

% the harness's own detector, WITH copy_term -- never binds the caller's template
rule_clauses(M:T, Heads) :-
    findall(H, ( copy_term(M:T, M:C),
                 catch(clause(M:C, B), _, fail),
                 B \== true,
                 copy_term(C, H) ), Heads).

fact_count(M:T, N) :-
    findall(1, ( copy_term(M:T, M:C), catch(clause(M:C, true), _, fail) ), L),
    length(L, N).

eval(Kind, M, F, A) :-
    functor(T, F, A),
    resolvable(M:T, R),
    is_dynamic(M:T, D),
    ( R == ok -> rule_clauses(M:T, RH), length(RH, NR), fact_count(M:T, NF)
    ; NR = 'n/a', NF = 'n/a' ),
    ( R \== ok            -> Verdict = throws_unresolvable
    ; NR \== 'n/a', NR > 0 -> Verdict = throws_partial_rule
    ; NF == 0             -> Verdict = throws_empty
    ; D == no             -> Verdict = throws_immutable
    ;                        Verdict = clean ),
    format("~w\t~w:~w/~w\tresolvable=~w\tdynamic=~w\trules=~w\tfacts=~w\t~w~n",
           [Kind, M, F, A, R, D, NR, NF, Verdict]).

run_eval :-
    format("side\tpredicate\tresolvable\tdynamic\trules\tfacts\tverdict~n"),
    forall(side(K,M,F,A), eval(K,M,F,A)),
    nl, run_controls.

% --- two-sided instrument controls, one pair per check -----------------------
ctl(Label, Goal, Expect) :-
    ( catch(Goal, E, (format("  ~w: THREW ~q~n",[Label,E]), fail))
    ->  Got = fires ; Got = declines ),
    ( Got == Expect -> S = 'OK' ; S = 'CONTROL BROKEN' ),
    format("  ~w~w: expected ~w, got ~w   [~w]~n", ['', Label, Expect, Got, S]).

run_controls :-
    format("=== TWO-SIDED INSTRUMENT CONTROLS (one pair per check) ===~n"),

    format(" [check 5: dynamism]~n"),
    ctl('FIRES   static bim/2      ', \+ is_dynamic(boltzmann_compliance:boltzmann_invariant_mountain(_,_), yes), fires),
    ctl('DECLINES dynamic cm/3     ', \+ is_dynamic(narrative_ontology:constraint_metric(_,_,_), yes), declines),

    format(" [check 3: rule detector]~n"),
    ctl('FIRES   static bim/2      ', (rule_clauses(boltzmann_compliance:boltzmann_invariant_mountain(_,_), H1), H1 \== []), fires),
    ctl('DECLINES config:param/2   ', (rule_clauses(config:param(_,_), H2), H2 \== []), declines),

    format(" [check 2: resolvability]~n"),
    ctl('FIRES   bogus functor     ', (resolvable(narrative_ontology:no_such_pred_xyzzy(_), S1), S1 \== ok), fires),
    ctl('FIRES   wrong arity cm/2  ', (resolvable(narrative_ontology:constraint_metric(_,_), S2), S2 = arity_mismatch(_)), fires),
    ctl('DECLINES cm/3             ', (resolvable(narrative_ontology:constraint_metric(_,_,_), S3), S3 \== ok), declines),

    format(" [check 1: empty snapshot]~n"),
    ctl('FIRES   unpopulated dyn   ', (fact_count(probe_harness_eval_scratch:never_asserted(_), C1), C1 =:= 0), fires),
    ctl('DECLINES config corpus_path', (fact_count(config:param(corpus_path,_), C2), C2 =:= 0), declines),

    format(" [check 4: shadow at TEMPLATE shape]~n"),
    ctl('FIRES   survivor unifies  ', shadow_fires, fires),
    ctl('DECLINES full retract     ', shadow_declines, fires),

    format(" [binding-leak regression]~n"),
    ctl('DECLINES template unbound ', leak_check, fires).

:- dynamic probe_harness_eval_scratch:never_asserted/1.
:- dynamic ph_eval:p/2.

% survivor p(a,1) is NOT retracted by template p(a,2)-shape; asserting p(a,2)
% leaves it behind a unifying survivor at TEMPLATE shape p(a,_).
shadow_fires :-
    retractall(ph_eval:p(_,_)),
    assertz(ph_eval:p(a,1)),
    Tmpl = ph_eval:p(a,_),
    % simulate: retract nothing (snapshot taken at a NARROWER shape), assert p(a,2)
    findall(x, clause(ph_eval:p(a,1), true), Surv), Surv \== [],
    copy_term(Tmpl, ph_eval:TC), \+ \+ (ph_eval:TC = p(a,1)).

shadow_declines :-
    retractall(ph_eval:p(_,_)),
    assertz(ph_eval:p(a,1)),
    retractall(ph_eval:p(a,_)),
    \+ clause(ph_eval:p(a,_), true).

% the leak: rule_clauses/2 must NOT bind its caller's template
leak_check :-
    T = boltzmann_compliance:boltzmann_invariant_mountain(_,_),
    rule_clauses(T, _),
    T = _:Term, functor(Term,_,_),
    term_variables(Term, Vs), Vs \== [].
