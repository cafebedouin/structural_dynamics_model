% Pass E: corpus scan of effective vs intrinsic purity at the analytical standard context.
run_fpn_scan :-
    logical_fingerprint:standard_context_for_power(analytical, Ctx),
    open('/tmp/purity_audit/corpus_ids.txt', read, In),
    read_lines3(In, IDs), close(In),
    open('/tmp/purity_audit/fpn_scan.tsv', write, S),
    format(S, 'constraint\tip\tep\tnbrs~n', []),
    forall(member(C, IDs),
           ( ( catch(purity_scoring:purity_score(C, IP), _, IP = err) -> true ; IP = fail ),
             ( catch(drl_purity_network:effective_purity(C, Ctx, EP, _), _, EP = err) -> true ; EP = fail ),
             ( catch(drl_purity_network:constraint_neighbors(C, Ctx, Ns), _, Ns = []) -> length(Ns, NN) ; NN = 0 ),
             format(S, '~w\t~w\t~w\t~w~n', [C, IP, EP, NN]) )),
    close(S),
    format(user_error, 'fpn_scan done~n', []).

read_lines3(In, IDs) :-
    read_line_to_string(In, L),
    (   L == end_of_file -> IDs = []
    ;   atom_string(A, L), IDs = [A|Rest], read_lines3(In, Rest)
    ).
