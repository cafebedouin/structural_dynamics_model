/* Step 1b — follow-up on the NEW mismatch point challenge_as_commons_maintenance T=5
   (surfaced by step1_nbsetval_witness.pl; absent from the 2026-06-08 census).
   Q1: does the (C,T=5) mismatch close under the nb-global set-manipulation (same
       mechanism) or is it a different cause?
   Q2: does it touch a COUNTED flip — classify_at_time/5 type-change between
       consecutive measurement times with Backed=true at both endpoints?
   Run from prolog/:
     swipl -g "consult('../audits/2026-06-11_oq83_close/step1b_challenge_followup.pl'), run, halt" -t "halt(1)"
*/

:- [stack].

clear_globals :-
    catch(nb_delete(classify_at_time_theater), _, true),
    catch(nb_delete(classify_at_time_eps), _, true).
cache_clear :-
    catch(cache_registry:clear_all_caches, _, true).

run :-
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, 'archives/datasets/kernel_v2_test')),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),
    C = challenge_as_commons_maintenance,
    forall(member(M, [theater_ratio, base_extractiveness, suppression_requirement]),
           ( ( narrative_ontology:constraint_metric(C, M, V) -> true ; V = 'ABSENT' ),
             format("static ~w = ~w~n", [M, V]) )),
    findall(T0, narrative_ontology:measurement(_, C, _, T0, _), Ts0),
    sort(Ts0, Ts),
    format("measurement times: ~w~n~n", [Ts]),
    forall(member(T, Ts),
           ( clear_globals, cache_clear,
             ( transition_paths:snapshot_type(C, T, S) -> true ; S = 'FAIL' ),
             clear_globals, cache_clear,
             ( drl_composition:classify_at_time(C, T, Ctx, A, snap(_, Backed, Eps, Supp, TR))
             -> true ; A = 'FAIL', Backed = '-', Eps = '-', Supp = '-', TR = '-' ),
             clear_globals,
             ( S == A -> Tag = '' ; Tag = '   <-- MISMATCH' ),
             format("T=~w snapshot=~w classify_at_time=~w backed=~w (eps=~w supp=~w theater=~w)~w~n",
                    [T, S, A, Backed, Eps, Supp, TR, Tag]) )),
    % mechanism check at T=5
    T5 = 5,
    ( narrative_ontology:measurement(_, C, theater_ratio, T5, TRt)
    -> nb_setval(classify_at_time_theater, tr(C, TRt)), TRset = TRt
    ;  nb_setval(classify_at_time_theater, none), TRset = none ),
    ( narrative_ontology:measurement(_, C, base_extractiveness, T5, EpsT)
    -> true ; EpsT = 0.5 ),
    nb_setval(classify_at_time_eps, eps(C, EpsT)),
    cache_clear,
    ( transition_paths:snapshot_type(C, T5, S5set) -> true ; S5set = 'FAIL' ),
    clear_globals,
    ( drl_composition:classify_at_time(C, T5, Ctx, A5) -> true ; A5 = 'FAIL' ),
    clear_globals,
    format("~nT=5 with globals set (theater=~w eps=~w): snapshot=~w vs classify_at_time=~w~n",
           [TRset, EpsT, S5set, A5]),
    ( S5set == A5
    -> format("mechanism: nb-global state CLOSES the T=5 mismatch (same cause as clinical T=0)~n")
    ;  format("mechanism: T=5 mismatch NOT closed by nb-global state (different cause)~n") ).
