:- [stack].
:- corpus_loader:load_all_testsets.

run :-
    constraint_indexing:default_context(Ctx),
    % Witness 1 (positive control): synthetic seriesless constraint WITHOUT marker
    % must come out Backed=false even though a scalar exists.
    assertz(narrative_ontology:constraint_metric(synthetic_unmarked_probe, suppression_requirement, 0.30)),
    assertz(narrative_ontology:measurement(syn_eps_0, synthetic_unmarked_probe, base_extractiveness, 0, 0.40)),
    drl_composition:classify_at_time(synthetic_unmarked_probe, 0, Ctx, T1, snap(_,B1,_,S1,_)),
    format("unmarked_seriesless_probe: type=~w supp=~w backed=~w  (expect backed=false)~n", [T1,S1,B1]),
    % Witness 1b: same constraint WITH marker must back.
    assertz(narrative_ontology:suppression_profile(synthetic_unmarked_probe, static)),
    drl_composition:classify_at_time(synthetic_unmarked_probe, 0, Ctx, T2, snap(_,B2,_,_,_)),
    format("marked_after_assert:       type=~w backed=~w  (expect backed=true)~n", [T2,B2]),
    % Witness 2: one of the recompiled 7 — rows back where eps is authored at T.
    forall(member(C, [thermal_dissipation_constraint, voltage_regulation_tradeoff]),
        ( temporal_residual:constraint_time_set(C, Times),
          findall(T-B, ( member(T, Times),
                         drl_composition:classify_at_time(C, T, Ctx, _, snap(_,B,_,_,_)) ), TBs),
          ( narrative_ontology:suppression_profile(C, static) -> M = marker_present ; M = MARKER_MISSING ),
          format("~w (~w): backed_per_row=~w~n", [C, M, TBs]) )),
    % Witness 2b: a misalignment constraint's substituted rows must STAY unbacked.
    temporal_residual:constraint_time_set(substantive_employment_reading, STimes),
    findall(T-B, ( member(T, STimes),
                   drl_composition:classify_at_time(substantive_employment_reading, T, Ctx, _, snap(_,B,_,_,_)) ), SBs),
    format("substantive_employment_reading (misalignment): backed_per_row=~w  (expect T3,T9 false)~n", [SBs]).
