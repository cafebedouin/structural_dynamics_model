/* Step 1 witness — OQ-83 entry item 5 (classifier-sync, UNVERIFIED hypothesis):
   snapshot_type/3 calls classify_from_metrics WITHOUT the nb_setval temporal
   theater/eps state that classify_at_time/4 threads (drl_composition.pl:246-251)
   and that effective_theater_ratio (drl_core.pl:306) / excess_extraction
   (boltzmann_compliance.pl:498) read.

   Substrate: archives/datasets/kernel_v2_test — the prior live corpus (n=100)
   archived at 00c639da; the 2026-06-08 V3/V3b finding (3 unique mismatch
   (C,T) points; 2 flagged flips) was measured on it as then-live testsets/.

   Design (pre-registered):
   - Corpus-wide scan, globals cleared per call: reproduce "3 unique mismatch
     (C,T) points at default context" (substrate witness).
   - Per flagged point (clinical_deskilling_automation T=0,
     milblogger_legitimacy_erosion T=18):
       cleared -> expect snapshot_type =\= classify_at_time (mismatch reproduced)
       set     -> nb-globals set to the exact at-T values classify_at_time
                  threads; hypothesis CONFIRMED iff snapshot_type now agrees.
   - Positive control: same-constraint agreeing T (same clause path) must agree
     cleared AND stay agreeing under the same set-manipulation.
   Run from prolog/:
     swipl -g "consult('../audits/2026-06-11_oq83_close/step1_nbsetval_witness.pl'), run, halt" -t "halt(1)"
*/

:- [stack].

clear_globals :-
    catch(nb_delete(classify_at_time_theater), _, true),
    catch(nb_delete(classify_at_time_eps), _, true).

cache_clear :-
    catch(cache_registry:clear_all_caches, _, true).

snap(C, T, Type) :-
    ( transition_paths:snapshot_type(C, T, Type0) -> Type = Type0 ; Type = 'FAIL' ).

cat(C, T, Ctx, Type) :-
    ( drl_composition:classify_at_time(C, T, Ctx, Type0) -> Type = Type0 ; Type = 'FAIL' ).

% one comparison with globals cleared before each classifier call
compare_cleared(C, T, Ctx, S, A) :-
    clear_globals, cache_clear, snap(C, T, S),
    clear_globals, cache_clear, cat(C, T, Ctx, A),
    clear_globals.

% set the globals exactly as classify_at_time_with_supp would for (C,T)
set_globals_for(C, T, TRset, EpsSet) :-
    (   narrative_ontology:measurement(_, C, theater_ratio, T, TRt)
    ->  nb_setval(classify_at_time_theater, tr(C, TRt)), TRset = TRt
    ;   nb_setval(classify_at_time_theater, none), TRset = none
    ),
    (   narrative_ontology:measurement(_, C, base_extractiveness, T, EpsT)
    ->  EpsSet = EpsT
    ;   EpsSet = 0.5
    ),
    nb_setval(classify_at_time_eps, eps(C, EpsSet)).

probe_point(Label, C, T, Ctx) :-
    format("~n=== ~w: ~w T=~w ===~n", [Label, C, T]),
    compare_cleared(C, T, Ctx, S0, A0),
    format("  cleared: snapshot_type=~w  classify_at_time=~w~n", [S0, A0]),
    set_globals_for(C, T, TRset, EpsSet),
    cache_clear,
    snap(C, T, S1),
    clear_globals,
    format("  set(theater=~w eps=~w): snapshot_type=~w~n", [TRset, EpsSet, S1]),
    (   S0 \== A0, S1 == A0
    ->  format("  VERDICT: mismatch reproduced AND closed by nb-global state -> mechanism CONFIRMED~n")
    ;   S0 \== A0, S1 \== A0
    ->  format("  VERDICT: mismatch reproduced but NOT closed by nb-global state -> hypothesis NOT confirmed at this point~n")
    ;   S0 == A0, S1 == A0
    ->  format("  VERDICT: agreement, stable under set-manipulation (control PASS)~n")
    ;   format("  VERDICT: agreement broken by set-manipulation (control FAIL -> probe invalid)~n")
    ).

% corpus-wide mismatch scan, globals cleared per call; unique (C,T) points
scan(Ctx, Mismatches) :-
    findall(C-T,
            ( corpus_loader:corpus_constraint(C),
              findall(T0, narrative_ontology:measurement(_, C, _, T0, _), Ts0),
              sort(Ts0, Ts),
              Ts \= [],
              member(T, Ts),
              compare_cleared(C, T, Ctx, S, A),
              S \== A
            ),
            Mismatches).

run :-
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, 'archives/datasets/kernel_v2_test')),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format("loaded corpus: archives/datasets/kernel_v2_test, n=~w~n", [N]),
    constraint_indexing:default_context(Ctx),
    format("default_context: ~w~n", [Ctx]),
    % static facts at the two flagged constraints (the fallback values the
    % globals-less path reads)
    forall(member(C, [clinical_deskilling_automation, milblogger_legitimacy_erosion]),
           forall(member(M, [theater_ratio, base_extractiveness, suppression_requirement]),
                  ( ( narrative_ontology:constraint_metric(C, M, V) -> true ; V = 'ABSENT' ),
                    format("static ~w ~w = ~w~n", [C, M, V]) ))),
    % substrate witness: reproduce the 2026-06-08 unique-(C,T) mismatch census
    scan(Ctx, Mismatches),
    length(Mismatches, NM),
    format("~ncorpus-wide unique (C,T) mismatch points at default context: ~w~n", [NM]),
    forall(member(C-T, Mismatches), format("  mismatch: ~w T=~w~n", [C, T])),
    % flagged points
    probe_point('FLAGGED', clinical_deskilling_automation, 0, Ctx),
    probe_point('FLAGGED', milblogger_legitimacy_erosion, 18, Ctx),
    % positive controls: same constraints at an agreeing T (same clause path)
    (   member(CT, [0, 6, 12, 24]),
        compare_cleared(milblogger_legitimacy_erosion, CT, Ctx, S, S)
    ->  probe_point('CONTROL', milblogger_legitimacy_erosion, CT, Ctx)
    ;   format("~nWARNING: no agreeing T found for milblogger control~n")
    ),
    (   member(CT2, [2, 4, 6, 8]),
        compare_cleared(clinical_deskilling_automation, CT2, Ctx, S2, S2)
    ->  probe_point('CONTROL', clinical_deskilling_automation, CT2, Ctx)
    ;   format("~nWARNING: no agreeing T found for clinical control~n")
    ).
