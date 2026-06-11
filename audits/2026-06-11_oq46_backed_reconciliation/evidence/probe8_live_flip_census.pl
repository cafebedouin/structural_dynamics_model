:- [stack].
:- corpus_loader:load_all_testsets.
run :-
    temporal_residual:residual_contexts(Ctxs),
    findall(F-FA,
        ( corpus_loader:corpus_constraint(C),
          once(narrative_ontology:measurement(_, C, _, _, _)),
          member(Ctx, Ctxs),
          temporal_residual:residual_for_context(C, Ctx, ctx_residual(_,_,Flips,FA)),
          length(Flips, F) ),
        Rows),
    aggregate_all(sum(F), member(F-_, Rows), TF),
    aggregate_all(sum(FA), member(_-FA, Rows), TFA),
    aggregate_all(sum(NB), ( member(C2, [demographic_skill_mismatch,proxy_measurement_validity,
                                          regime_change_structural_break,solar_integration_mechanism,
                                          thermal_dissipation_constraint,transfer_gap_physics,
                                          voltage_regulation_tradeoff]),
                             member(Cx, Ctxs),
                             temporal_residual:residual_for_context(C2, Cx, ctx_residual(_,NB,_,_)) ), TNB),
    format("live flips=~w fab_adjacent=~w (expect 59/20); backed_times_sum_for_7=~w (was 0)~n", [TF, TFA, TNB]).
