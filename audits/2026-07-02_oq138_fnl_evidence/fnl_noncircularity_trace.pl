% ============================================================================
% OQ-138 FNL conversion — dr_type/3 non-circularity trace (Verification step 1)
% ============================================================================
% The fnl_routed/1 seat predicate calls drl_core:dr_type/3; the severity/grade
% machinery (converted_at_seat/2 -> fnl_routed/1) is only NON-circular if
% dr_type/3's reachable call set does not itself consult that machinery. This
% was asserted by analogy to fcr_routed/1 when FCR-9 landed (2026-06-21); a
% consult added to the dr_type/3 path since then would reintroduce the cycle.
% Witness at HEAD, don't assume.
%
% Method: clause/2 body-walk (check_axis_boundary.pl body_calls pattern),
% transitive closure from dr_type/3 over engine-defined predicates by NAME/ARITY
% (module-agnostic on the callee — conservative: over-approximates reachability,
% so a clean PASS is trustworthy).
%
% Forbidden set (the severity/seat machinery): fnl_routed/1 (pre-build: absent),
% fcr_routed/1, constructed_routed/1, converted_at_seat/2, signature_grade/2,
% signature_severity/2, signature_diagnostic_severity/3.
%
% POSITIVE CONTROLS (a closure that misses these never walked):
%   PC1: closure contains resolve_modal_signature_conflict/3 (known dr_type callee)
%   PC2: closure contains integrate_signature_with_modal/3
%   PC3: the walker, seeded at signature_grade/2 instead, DOES reach
%        converted_at_seat/2 (proves the forbidden-set test can fire).
%
% Run: cd prolog && swipl -q -g true -t halt ../audits/2026-07-02_oq138_fnl_evidence/fnl_noncircularity_trace.pl
% ============================================================================
:- initialization(main).
:- [stack].

% --- body_calls: copied from check_axis_boundary.pl (same descent set) ---
body_calls(V, _) :- var(V), !, fail.
body_calls((A,B), G)        :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls((A;B), G)        :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls((A->B), G)       :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls((A*->B), G)      :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls(\+(A), G)        :- !, body_calls(A,G).
body_calls(not(A), G)       :- !, body_calls(A,G).
body_calls(call(A), G)      :- !, body_calls(A,G).
body_calls(once(A), G)      :- !, body_calls(A,G).
body_calls(ignore(A), G)    :- !, body_calls(A,G).
body_calls(findall(_,A,_), G):- !, body_calls(A,G).
body_calls(findall(_,A,_,_), G):- !, body_calls(A,G).
body_calls(forall(A,B), G)  :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls(aggregate_all(_,A,_), G) :- !, body_calls(A,G).
body_calls(bagof(_,A,_), G) :- !, body_calls(A,G).
body_calls(setof(_,A,_), G) :- !, body_calls(A,G).
body_calls(catch(A,_,B), G) :- !, ( body_calls(A,G) ; body_calls(B,G) ).
body_calls(_^A, G)          :- !, body_calls(A,G).
body_calls(maplist(Goal,_), G)     :- !, meta_partial(Goal, G).
body_calls(maplist(Goal,_,_), G)   :- !, meta_partial(Goal, G).
body_calls(maplist(Goal,_,_,_), G) :- !, meta_partial(Goal, G).
body_calls(M:Goal, G) :- !, atom(M), body_calls(Goal, G).
body_calls(Goal, N/A) :- callable(Goal), functor(Goal, N, A).
meta_partial(Goal, N/A) :-
    callable(Goal),
    ( Goal = M:G2, atom(M) -> true ; G2 = Goal ),
    functor(G2, N, A).

engine_file(F) :-
    sub_atom(F, _, _, _, '/prolog/'),
    \+ sub_atom(F, _, _, _, '/library/'),
    \+ sub_atom(F, _, _, _, 'fnl_noncircularity_trace').

% direct_edge(Name/Ar, CalleeName/CalleeAr): any engine clause of Name/Ar (any
% defining module) calls Callee. Callee matched by name/arity across all modules.
direct_edge(Name/Ar, Callee) :-
    current_module(M),
    current_predicate(Name, M:Head),
    functor(Head, Name, Ar),
    \+ predicate_property(M:Head, imported_from(_)),
    catch(predicate_property(M:Head, file(F)), _, fail),
    engine_file(F),
    \+ predicate_property(M:Head, foreign),
    \+ predicate_property(M:Head, built_in),
    catch(clause(M:Head, Body), _, fail),
    body_calls(Body, Callee).

closure(Seed, Set) :-
    closure_iter([Seed], [Seed], Set).
closure_iter([], Acc, Acc).
closure_iter([P|Rest], Seen, Set) :-
    findall(C, (direct_edge(P, C), \+ memberchk(C, Seen)), New0),
    sort(New0, New),
    append(Seen, New, Seen1),
    append(Rest, New, Frontier),
    closure_iter(Frontier, Seen1, Set).

forbidden(fnl_routed/1).
forbidden(fcr_routed/1).
forbidden(constructed_routed/1).
forbidden(converted_at_seat/2).
forbidden(signature_grade/2).
forbidden(signature_severity/2).
forbidden(signature_diagnostic_severity/3).

main :-
    closure(dr_type/3, Set),
    length(Set, N),
    format("dr_type/3 closure size: ~w~n", [N]),
    % positive controls: the walker walked
    ( memberchk(resolve_modal_signature_conflict/3, Set)
    -> format("PC1 ok: resolve_modal_signature_conflict/3 in closure~n")
    ;  format("PC1 FAIL: known callee missing — walker broken~n"), halt(1) ),
    ( memberchk(integrate_signature_with_modal/3, Set)
    -> format("PC2 ok: integrate_signature_with_modal/3 in closure~n")
    ;  format("PC2 FAIL: known callee missing — walker broken~n"), halt(1) ),
    % PC3: seeded at signature_grade/2 the forbidden test CAN fire
    closure(signature_grade/2, GSet),
    ( memberchk(converted_at_seat/2, GSet)
    -> format("PC3 ok: signature_grade/2 closure reaches converted_at_seat/2~n")
    ;  format("PC3 FAIL: forbidden-set test cannot fire — control broken~n"), halt(1) ),
    % the actual invariant
    findall(F, (forbidden(F), memberchk(F, Set)), Hits),
    ( Hits == []
    -> format("NONCIRCULAR ok: dr_type/3 closure reaches none of the severity/seat machinery~n")
    ;  format("CIRCULAR: dr_type/3 reaches ~w — HALT, seat predicate would be circular~n", [Hits]), halt(1) ),
    halt.
main :- write('TRACE FAIL'), halt(1).
