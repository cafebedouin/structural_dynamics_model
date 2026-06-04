% What structural_purity/2 WOULD return if its clause-1 guard worked as intended.
% Replicates clause 2 (signature_detection.pl:977-991) with the access check called correctly.

fixed_structural(C, Out) :-
    (   boltzmann_compliance:epistemic_access_check(C, Acc), Acc == false
    ->  Out = inconclusive
    ;   catch(
            ( signature_detection:purity_test_factorization(C, T1),
              signature_detection:purity_test_scope_invariance(C, T2),
              signature_detection:purity_test_coupling(C, T3),
              signature_detection:purity_test_excess(C, T4),
              include(boltzmann_compliance:is_failure, [T1,T2,T3,T4], Fails),
              (   Fails == []
              ->  ( signature_detection:determine_pure_subtype(C, Sub) -> Out = Sub ; Out = pure_unknown )
              ;   findall(N, (member(fail(N,_), Fails) ; member(fail(N), Fails)), Ns),
                  atomic_list_concat([contaminated|Ns], '+', Out)
              )
            ), E, Out = error(E))
    ).

run_fixed :-
    open('/tmp/purity_audit/corpus_ids.txt', read, In),
    read_lines2(In, IDs), close(In),
    open('/tmp/purity_audit/fixed_structural.tsv', write, S),
    format(S, 'constraint\tfixed_structural~n', []),
    forall(member(C, IDs),
           ( fixed_structural(C, Out),
             format(S, '~w\t~w~n', [C, Out]) )),
    close(S),
    format(user_error, 'fixed_structural done~n', []).

read_lines2(In, IDs) :-
    read_line_to_string(In, L),
    (   L == end_of_file -> IDs = []
    ;   atom_string(A, L), IDs = [A|Rest], read_lines2(In, Rest)
    ).
