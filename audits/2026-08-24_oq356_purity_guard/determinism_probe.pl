% OQ-356 — is drl_purity_network:effective_purity/4 SEMIDET per member?
%
% Load-bearing for the 1a/1b partition rewrite. The pre-fix code collects with
% findall/3, which takes ALL solutions per member; a single-pass if-then-else
% partition takes the FIRST. Those differ iff effective_purity/4 is nondet.
% Two-sided: a POSITIVE CONTROL over a deliberately nondet goal must report the
% multiplicity, or a zero here means "the probe cannot count", not "det".
% (run via -g)

main :-
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    length(Cs, N),
    format("members: ~w~n", [N]),
    findall(C-K,
            ( member(C, Cs),
              aggregate_all(count,
                  catch(drl_purity_network:effective_purity(C, Ctx, _, _), _, fail),
                  K),
              K \== 1 ),
            Odd),
    length(Odd, NOdd),
    format("members whose effective_purity/4 solution-count is NOT exactly 1: ~w~n", [NOdd]),
    ( NOdd > 0 -> forall(member(P, Odd), format("   ~w~n", [P])) ; true ),
    % ---- POSITIVE CONTROL: the same counting idiom over a known-nondet goal ----
    aggregate_all(count, member(_, [a,b,c]), KC),
    format("positive control (member/2 over a 3-list, same aggregate_all idiom): ~w~n", [KC]),
    ( KC =:= 3
    -> format("CONTROL OK — the probe CAN count multiple solutions, so the zero above is a measurement~n")
    ;  format("CONTROL FAILED — the zero above is UNINTERPRETABLE~n") ),
    % ---- second control: a nondet goal in the SAME shape (per-member) ----
    findall(X-K2, ( member(X, [p,q]),
                    aggregate_all(count, member(_, [1,2]), K2), K2 \== 1 ), Odd2),
    length(Odd2, NOdd2),
    format("control 2 (per-member nondet, must be 2): ~w~n", [NOdd2]).
