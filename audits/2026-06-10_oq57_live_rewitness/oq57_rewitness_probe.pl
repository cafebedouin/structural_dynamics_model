% OQ-57 re-witness probe (2026-06-10) — live (post-2026-06-05-reset) corpus.
% Read-only. Confirms the drift_events.pl:236 qualifier fix is EXERCISED, not
% vacuously passing, against the corpus that replaced the one the original
% (2026-06-04) witnesses ran on.

:- initialization(main).

reaches_piton_guard(C, E, TR) :-
    drift_events:safe_metric(C, extractiveness, E), E < 0.10,
    drift_events:safe_metric(C, theater_ratio, TR), TR > 0.70.

reaches_dried_up(C, E, S) :-
    drift_events:safe_metric(C, extractiveness, E), E < 0.10,
    drift_events:safe_metric(C, suppression_requirement, S), S > 0.50.

main :-
    [stack],
    corpus_loader:load_all_testsets,

    % --- denominator (authoritative corpus membership) -------------------
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    length(Cs, N),
    format("~n=== OQ-57 LIVE RE-WITNESS (2026-06-10) ===~n", []),
    format("corpus_constraint denominator: ~w~n", [N]),

    % --- POSITIVE CONTROL: reachability of the once-throwing clause ------
    findall(C-E-TR, reaches_piton_guard(C, E, TR), Piton),
    length(Piton, NP),
    format("~n[reachability] internalized_piton guard (eps<0.10 AND theater>0.70): ~w constraint(s)~n", [NP]),
    forall(member(C-E-TR, Piton),
        format("  REACHES: ~w  (eps=~4f, theater=~4f)~n", [C, E, TR])),

    findall(C2-E2-S2, reaches_dried_up(C2, E2, S2), Dried),
    length(Dried, ND),
    format("[reachability] extraction_dried_up clause (eps<0.10 AND supp>0.50): ~w constraint(s)~n", [ND]),

    % --- the OQ-57 symptom: does drift_event/3 THROW on reaching cs? -----
    format("~n[symptom] calling drift_event(C, internalized_piton, _) on reaching constraints:~n", []),
    (   Piton == []
    ->  format("  (none reach the guard on the live corpus -- fix is UNEXERCISED here)~n", [])
    ;   forall(member(C3-_-_, Piton),
            (   catch(
                    ( findall(Ev, drift_events:drift_event(C3, internalized_piton, Ev), Evs),
                      format("  CLEAN: ~w -> ~w~n", [C3, Evs]) ),
                    Err,
                    format("  *** THREW on ~w: ~w~n", [C3, Err]) )
            ))
    ),

    % --- full-scan throw check (the 'whole drift scan' that aborted) -----
    format("~n[full-scan] drift_event/3 over ALL corpus constraints, any event type:~n", []),
    ( catch(
        ( findall(C4-T, (member(C4, Cs), drift_events:drift_event(C4, T, _)), _AllEvs),
          format("  full enumeration completed with NO throw~n", []) ),
        ScanErr,
        format("  *** SCAN THREW: ~w~n", [ScanErr]) ) -> true ; true ),

    % --- DIAGNOSTIC POSITIVE CONTROL ------------------------------------
    % Prove the probe distinguishes throw from clean: the bridged predicate
    % resolves under domain_priors: (fix), and the WRONG qualifier
    % (narrative_ontology:, the pre-fix code) still throws existence_error.
    format("~n[diagnostic positive control]~n", []),
    ( catch( ( domain_priors:requires_active_enforcement(_) -> R1 = succeeded ; R1 = failed_cleanly ),
             E1, R1 = threw(E1) )
      ; R1 = failed_cleanly ),
    format("  domain_priors:requires_active_enforcement/1 (FIXED qualifier): ~w~n", [R1]),
    ( catch( ( narrative_ontology:requires_active_enforcement(_) -> R2 = succeeded ; R2 = failed_cleanly ),
             E2, R2 = threw(E2) )
      ; R2 = failed_cleanly ),
    format("  narrative_ontology:requires_active_enforcement/1 (PRE-FIX qualifier): ~w~n", [R2]),
    format("~n=== END ===~n", []),
    halt.
main :- format("PROBE FAILED~n", []), halt(1).
