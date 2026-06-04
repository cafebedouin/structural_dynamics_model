% Purity audit census — per-constraint scalar, subscores, data-presence, categorical class.
% Run: cd prolog && swipl -g "[stack], corpus_loader:ensure_corpus_loaded, ['/tmp/purity_audit/census.pl'], run_census, halt" -t "halt(1)"

probe(Goal, Out) :-
    (   catch(Goal, E, (Out = error(E)))
    ->  ( var(Out) -> Out = ok ; true )
    ;   Out = fail
    ).

census_line(S, C) :-
    % scalar purity: score / sentinel / fail / error
    (   catch(purity_scoring:purity_score(C, P), E1, (P = error, ErrP = E1))
    ->  (   P == error -> Status = error(ErrP), PV = na
        ;   P =:= -1.0 -> Status = sentinel, PV = -1.0
        ;   Status = scored, PV = P
        )
    ;   Status = fail, PV = na
    ),
    % subscores (independent probes)
    ( catch(purity_scoring:factorization_subscore(C, F), _, F = err) -> true ; F = fail ),
    ( catch(purity_scoring:scope_invariance_subscore(C, SI), _, SI = err) -> true ; SI = fail ),
    ( catch(purity_scoring:coupling_cleanliness_subscore(C, CC), _, CC = err) -> true ; CC = fail ),
    ( catch(purity_scoring:excess_extraction_subscore(C, EX), _, EX = err) -> true ; EX = fail ),
    % data-presence flags (default-fired detectors)
    ( catch(boltzmann_compliance:cross_index_coupling(C, _), _, fail) -> HasCoup = 1 ; HasCoup = 0 ),
    ( catch(boltzmann_compliance:scope_invariance_test(C, SR), _, SR = err) -> true ; SR = fail ),
    ( SR = invariant -> ScopeR = invariant
    ; SR = variant(Ts), is_list(Ts) -> length(Ts, NT), atom_concat(variant_, NT, ScopeR)
    ; ScopeR = SR
    ),
    ( catch((boltzmann_compliance:detect_nonsensical_coupling(C, Prs, _), Prs \= []), _, fail) -> HasNons = 1 ; HasNons = 0 ),
    ( catch(boltzmann_compliance:excess_extraction(C, _), _, fail) -> HasExc = 1 ; HasExc = 0 ),
    % categorical purity (#3)
    (   catch(signature_detection:structural_purity(C, Cls), _, Cls = error)
    ->  true
    ;   Cls = fail
    ),
    ( Cls = contaminated(Fs), is_list(Fs) -> length(Fs, NF), atom_concat(contaminated_, NF, ClsA)
    ; ClsA = Cls
    ),
    % zone from scalar
    ( number(PV), PV >= 0.0 -> logical_fingerprint:purity_zone(PV, Zone) ; Zone = na ),
    format(S, '~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~q\t~w~n',
           [C, Status, PV, F, SI, CC, EX, HasCoup, ScopeR, HasNons, HasExc, ClsA, Zone]).

run_census :-
    open('/tmp/purity_audit/corpus_ids.txt', read, In),
    read_lines(In, IDs),
    close(In),
    length(IDs, N),
    format(user_error, 'census_population=~w~n', [N]),
    open('/tmp/purity_audit/census.tsv', write, S),
    format(S, 'constraint\tstatus\tpurity\tF\tSI\tCC\tEX\thas_coupling\tscope_result\thas_nonsense\thas_excess\tstructural\tzone~n', []),
    forall(member(C, IDs), census_line(S, C)),
    close(S),
    format(user_error, 'census_done~n', []).

read_lines(In, IDs) :-
    read_line_to_string(In, L),
    (   L == end_of_file -> IDs = []
    ;   atom_string(A, L),
        IDs = [A|Rest],
        read_lines(In, Rest)
    ).
