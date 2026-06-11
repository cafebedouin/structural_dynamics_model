/* Step 1c — before/after witness for the determinism fix (operator ruling
   2026-06-11, option 1): snapshot_type clears the classify_at_time nb-globals
   at entry, removing the order-dependent stale-read while preserving its
   static-fallback semantics.

   Run IDENTICALLY before and after the edit (same script, two runs):
   - STALE-READ DEMO: snapshot_type(clinical,0) cleared, then again immediately
     after classify_at_time(clinical,0) left its globals set.
       pre-fix : cleared=piton, after-cat=unknown  (order-dependent — the bug)
       post-fix: cleared=piton, after-cat=piton    (deterministic)
   - MISMATCH PERSISTENCE: cleared snapshot_type vs classify_at_time at
     clinical T=0 must STAY piton/unknown (the fix must not read as sync).
   - CONTROLS UNDISTURBED: clinical T=2, milblogger T=0, milblogger T=18.
   Run from prolog/:
     swipl -g "consult('../audits/2026-06-11_oq83_close/step1c_determinism_fix_witness.pl'), run, halt" -t "halt(1)"
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

run :-
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, 'archives/datasets/kernel_v2_test')),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),
    C = clinical_deskilling_automation,
    % --- stale-read demo ---
    clear_globals, cache_clear, snap(C, 0, S_cleared),
    cat(C, 0, Ctx, A0),                 % leaves its globals set (no clear)
    cache_clear, snap(C, 0, S_after_cat),
    clear_globals,
    format("stale-read demo: cleared snapshot=~w | classify_at_time=~w | snapshot-right-after=~w~n",
           [S_cleared, A0, S_after_cat]),
    (   S_cleared == S_after_cat
    ->  format("  -> DETERMINISTIC (post-fix expected state)~n")
    ;   format("  -> ORDER-DEPENDENT (pre-fix bug state)~n")
    ),
    % --- mismatch persistence (must not read as sync) ---
    clear_globals, cache_clear, snap(C, 0, S0),
    clear_globals, cache_clear, cat(C, 0, Ctx, A0b),
    clear_globals,
    format("mismatch persistence: snapshot=~w vs classify_at_time=~w (~w)~n",
           [S0, A0b, ( S0 \== A0b -> persists ; closed )]),
    % --- controls ---
    forall(member(Cc-Tc, [clinical_deskilling_automation-2,
                          milblogger_legitimacy_erosion-0,
                          milblogger_legitimacy_erosion-18]),
           ( clear_globals, cache_clear, snap(Cc, Tc, Sc),
             clear_globals, cache_clear, cat(Cc, Tc, Ctx, Ac),
             clear_globals,
             format("control ~w T=~w: snapshot=~w classify_at_time=~w (~w)~n",
                    [Cc, Tc, Sc, Ac, ( Sc == Ac -> agree ; 'DISAGREE' )]) )).
