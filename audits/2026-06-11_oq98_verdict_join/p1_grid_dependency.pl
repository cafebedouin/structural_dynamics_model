% ============================================================================
% P1 — grid-dependency probe (OQ-98 Step 0; read-only w.r.t. the repo)
% ============================================================================
% Question: is ANY of the 12 diagnostic_summary subsystems fed by the leveled
% measurement grid?  Decides operator ruling 1's branch:
%   Branch A (no subsystem grid-fed)  -> per-question CONDITIONAL tags
%   Branch B (any subsystem grid-fed) -> strict fail-closed (authored<total
%                                        => headline conditional)
%
% Method (plan Step 0 / gotchas §§4,6,7):
%   1. Load corpus + replicate the json_report precompute exactly.
%   2. Coverage precheck: witness the constraint<->interval mapping BEFORE
%      asserting (interval ID == constraint ID is a convention, not a checked
%      fact). Constraints with NO interval are unprobed-by-construction.
%   3. Record S0 = diagnostic_summary(C, .) for every corpus constraint, and
%      classify_interval/3 (the positive control's consumer) per interval.
%   4. assertz a full synthetic 32-slot leveled grid per interval
%      (probe source ID oq98_probe -> source_class authored; value 0.95).
%      Overlay-took-effect witness: grid_provenance before/after.
%   5. cache_registry:clear_all_caches, recompute S1 + classify_interval.
%   6. Per-constraint S0==S1 diff. Positive control: classify_interval must
%      change for >=1 interval, else the instrument is blind and the probe
%      is INVALID (not branch A).
%   7. Restore (retract probe facts), verify 0 remain, clear caches, full S2
%      sweep must equal S0 (per-item restore verification, gotchas §4).
%
% Run from prolog/ on the run_pipeline loader chain (run_pipeline.py:508-513):
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l dirac_classification.pl -l diagnostic_summary.pl \
%         -l post_synthesis.pl -l json_report.pl \
%         -l ../audits/2026-06-11_oq98_verdict_join/p1_grid_dependency.pl \
%         -g "p1_main, halt." -t "halt(1)"
% ============================================================================

:- use_module(library(lists)).

p1_main :-
    setup_call_cleanup(
        open('../audits/2026-06-11_oq98_verdict_join/p1_witness.txt', write, W),
        p1_run(W),
        close(W)).

p1_run(W) :-
    corpus_loader:load_all_testsets,
    % --- Replicate json_report precompute (json_report.pl:69-91) ---
    constraint_indexing:default_context(MaxEntCtx),
    measurement_layer:wasserstein_contexts(WCtxs),
    catch(maxent_classifier:maxent_multi_run(WCtxs, _), _, true),
    catch(maxent_classifier:maxent_indexed_run(MaxEntCtx, _), _, true),
    (   config:param(fpn_enabled, 1)
    ->  catch(drl_fpn:fpn_run(MaxEntCtx, _), _, true)
    ;   true
    ),
    catch(grothendieck_cohomology:corpus_cohomology(_), _, true),
    json_report:load_abductive_data,
    format(W, '=== P1 grid-dependency probe (OQ-98, 2026-06-11) ===~n', []),

    % --- Coverage precheck: constraint <-> interval mapping ---
    findall(C, corpus_loader:corpus_constraint(C), CsRaw), sort(CsRaw, Cs),
    length(Cs, NC),
    findall(I, narrative_ontology:interval(I, _, _), IsRaw), sort(IsRaw, Is),
    length(Is, NI),
    format(W, 'corpus constraints: ~w; distinct interval IDs: ~w~n', [NC, NI]),
    subtract(Cs, Is, NoInterval),
    subtract(Is, Cs, NonCorpusIntervals),
    length(NoInterval, NNoInt),
    format(W, 'constraints with NO interval (unprobed-by-construction, ~w): ~w~n',
           [NNoInt, NoInterval]),
    format(W, 'interval IDs that are NOT corpus constraints: ~w~n',
           [NonCorpusIntervals]),
    intersection(Cs, Is, Probed),
    length(Probed, NP),
    format(W, 'probed (constraint==interval pairs): ~w~n~n', [NP]),

    % --- Pre-state: grid + m_gen/repair census (also feeds P3) ---
    aggregate_all(count, narrative_ontology:measurement(_, _, _, _, _), NMeas),
    aggregate_all(count, narrative_ontology:measurement(m_gen, _, _, _, _), NMgen),
    aggregate_all(count,
        (narrative_ontology:measurement(Src, _, _, _, _), atom(Src),
         sub_atom(Src, 0, _, _, repair_m_)), NRep),
    format(W, 'measurement/5 facts: ~w (m_gen: ~w, repair_m_*: ~w)~n~n',
           [NMeas, NMgen, NRep]),
    forall(member(C0, Probed),
           ( p1_prov(C0, Prov0),
             format(W, 'prov_before ~w: ~w~n', [C0, Prov0]) )),

    % --- S0 sweep (every corpus constraint, incl. no-interval = specificity) ---
    findall(C-S, (member(C, Cs), p1_summary(C, S)), S0s),
    findall(I-PC, (member(I, Probed), p1_ci(I, PC)), CI0s),
    format(W, '~nS0 + classify_interval baselines recorded.~n', []),

    % --- Assert synthetic full grid per interval ---
    forall(( member(I, Probed), narrative_ontology:interval(I, T0, Tn) ),
           forall(( config:level(L), member(T, [T0, Tn]),
                    member(M, [accessibility_collapse(L), stakes_inflation(L),
                               suppression(L), resistance(L)]) ),
                  assertz(narrative_ontology:measurement(oq98_probe, I, M, T, 0.95)))),
    aggregate_all(count, narrative_ontology:measurement(oq98_probe, _, _, _, _),
                  NAsserted),
    format(W, 'asserted synthetic slots: ~w (expect 32 x ~w = ~w)~n',
           [NAsserted, NP, 32*NP]),
    % Overlay-took-effect witness (precedes any census read)
    forall(member(C1, Probed),
           ( p1_prov(C1, Prov1),
             format(W, 'prov_after ~w: ~w~n', [C1, Prov1]) )),

    % --- Clear memo caches, recompute ---
    cache_registry:clear_all_caches,
    findall(C-S, (member(C, Cs), p1_summary(C, S)), S1s),
    findall(I-PC, (member(I, Probed), p1_ci(I, PC)), CI1s),

    % --- Positive control: classify_interval must see the grid ---
    format(W, '~n--- positive control: report_generator:classify_interval/3 ---~n', []),
    findall(I, (member(I-PC0, CI0s), member(I-PC1, CI1s), PC0 \== PC1), CIChanged),
    forall(member(I-PC0, CI0s),
           ( member(I-PC1, CI1s),
             (   PC0 == PC1
             ->  format(W, 'ci ~w: unchanged ~w~n', [I, PC0])
             ;   format(W, 'ci ~w: CHANGED ~w -> ~w~n', [I, PC0, PC1])
             ) )),
    length(CIChanged, NCIChanged),
    (   NCIChanged > 0
    ->  format(W, 'POSITIVE CONTROL: PASS (~w/~w intervals changed — instrument sees a grid consumer)~n',
               [NCIChanged, NP])
    ;   format(W, 'POSITIVE CONTROL: FAIL — classify_interval blind to synthetic grid; PROBE INVALID~n', [])
    ),

    % --- The measurement: S0 vs S1 per constraint ---
    format(W, '~n--- diagnostic_summary S0 vs S1 ---~n', []),
    findall(C, (member(C-S0, S0s), member(C-S1, S1s), S0 \== S1), SDiff),
    forall(member(C-S0, S0s),
           ( member(C-S1, S1s),
             (   S0 == S1
             ->  format(W, 'S ~w: SAME~n', [C])
             ;   format(W, 'S ~w: DIFF~n  S0=~w~n  S1=~w~n', [C, S0, S1])
             ) )),
    length(SDiff, NSDiff),
    format(W, '~nS-diff count: ~w / ~w constraints~n', [NSDiff, NC]),

    % --- Verdict line (branch ruling) ---
    (   NCIChanged =:= 0
    ->  format(W, 'P1 RESULT: INVALID (positive control failed)~n', [])
    ;   NSDiff =:= 0
    ->  format(W, 'P1 RESULT: BRANCH A — no diagnostic_summary subsystem is grid-fed (witnessed under a live grid consumer control)~n', [])
    ;   format(W, 'P1 RESULT: BRANCH B — ~w constraints changed summary under synthetic grid; strict fail-closed ruling applies~n', [NSDiff])
    ),

    % --- Restore + per-item verification (gotchas §4) ---
    retractall(narrative_ontology:measurement(oq98_probe, _, _, _, _)),
    aggregate_all(count, narrative_ontology:measurement(oq98_probe, _, _, _, _),
                  NLeft),
    format(W, '~nrestore: probe facts remaining = ~w (must be 0)~n', [NLeft]),
    cache_registry:clear_all_caches,
    findall(C-S, (member(C, Cs), p1_summary(C, S)), S2s),
    findall(C, (member(C-S0, S0s), member(C-S2, S2s), S0 \== S2), RDiff),
    length(RDiff, NRDiff),
    format(W, 'restore verification: S2 vs S0 diff count = ~w (must be 0)~w~n',
           [NRDiff, '']),
    (   NRDiff > 0
    ->  format(W, 'RESTORE FAILED for: ~w~n', [RDiff])
    ;   true
    ).

p1_summary(C, S) :-
    (   catch(diagnostic_summary:diagnostic_summary(C, S0), _, fail)
    ->  S = S0
    ;   S = none
    ).

p1_ci(I, ci(P, Cf)) :-
    (   catch(report_generator:classify_interval(I, P0, Cf0), _, fail)
    ->  P = P0, Cf = Cf0
    ;   P = fail, Cf = fail
    ).

p1_prov(I, Prov) :-
    (   catch(data_repair:grid_provenance(I, Prov0), E, (Prov0 = error(E)))
    ->  Prov = Prov0
    ;   Prov = no_solution
    ).
