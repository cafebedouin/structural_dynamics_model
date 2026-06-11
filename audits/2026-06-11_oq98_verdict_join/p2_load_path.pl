% ============================================================================
% P2 — load-path probe (OQ-98 Step 0; OQ-57 class)
% ============================================================================
% verdict_join/3 will call data_repair:grid_provenance/2 (exported) and
% data_repair:source_class/2 (NOT exported) module-qualified from
% diagnostic_summary, inside the run_pipeline loader chain. Wrong-qualifier
% behavior is load-path-dependent (gotchas §1), so witness the exact calls on
% the exact chain BEFORE the output-changing commit.
%
% Run from prolog/ on the run_pipeline loader chain (run_pipeline.py:508-513):
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l dirac_classification.pl -l diagnostic_summary.pl \
%         -l post_synthesis.pl -l json_report.pl \
%         -l ../audits/2026-06-11_oq98_verdict_join/p2_load_path.pl \
%         -g "p2_main, halt." -t "halt(1)"
% ============================================================================

p2_main :-
    setup_call_cleanup(
        open('../audits/2026-06-11_oq98_verdict_join/p2_witness.txt', write, W),
        p2_run(W),
        close(W)).

p2_run(W) :-
    corpus_loader:load_all_testsets,
    format(W, '=== P2 load-path probe (OQ-98, 2026-06-11) ===~n', []),
    % grid_provenance through the chain
    (   catch(data_repair:grid_provenance(scale_ceiling, Prov), E1,
              (format(W, 'grid_provenance(scale_ceiling) THREW: ~w~n', [E1]), fail))
    ->  format(W, 'grid_provenance(scale_ceiling) = ~w~n', [Prov])
    ;   format(W, 'grid_provenance(scale_ceiling) FAILED (no solution)~n', [])
    ),
    % source_class (unexported) module-qualified through the chain —
    % the exact call shape verdict_join will use
    (   catch(data_repair:source_class(m_gen, Cls1), E2,
              (format(W, 'source_class(m_gen) THREW: ~w~n', [E2]), fail))
    ->  format(W, 'source_class(m_gen) = ~w (expect injected)~n', [Cls1])
    ;   format(W, 'source_class(m_gen) FAILED~n', [])
    ),
    (   catch(data_repair:source_class(repair_m_77, Cls2), _, fail)
    ->  format(W, 'source_class(repair_m_77) = ~w (expect imputed)~n', [Cls2])
    ;   format(W, 'source_class(repair_m_77) FAILED~n', [])
    ),
    (   catch(data_repair:source_class(m1, Cls3), _, fail)
    ->  format(W, 'source_class(m1) = ~w (expect authored)~n', [Cls3])
    ;   format(W, 'source_class(m1) FAILED~n', [])
    ),
    % measurement/5 enumeration for the witness constraint
    aggregate_all(count, narrative_ontology:measurement(_, scale_ceiling, _, _, _), NSc),
    format(W, 'measurement/5 facts for scale_ceiling: ~w~n', [NSc]),
    findall(Src-Metric-T,
            narrative_ontology:measurement(Src, scale_ceiling, Metric, T, _),
            Rows),
    forall(member(R, Rows), format(W, '  ~w~n', [R])),
    % dr_mismatch/3 through the chain for the witness constraint
    (   setof(Err-Sev, drl_core:dr_mismatch(scale_ceiling, Err, Sev), Mis)
    ->  format(W, 'dr_mismatch(scale_ceiling): ~w~n', [Mis])
    ;   format(W, 'dr_mismatch(scale_ceiling): none~n', [])
    ).
