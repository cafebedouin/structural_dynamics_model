:- [stack].
:- corpus_loader:load_all_testsets.

run :-
    constraint_indexing:default_context(Ctx),
    forall(member(C, [substantive_employment_reading, post_1998_convergence, truth_democracy_disinformation]),
        ( format("--- ~w (default ctx) ---~n", [C]),
          temporal_residual:constraint_time_set(C, Times),
          forall(member(T, Times),
            ( drl_composition:classify_at_time(C, T, Ctx, Type, snap(_,_,_,Supp,_)),
              ( narrative_ontology:measurement(_, C, suppression_requirement, T, _) -> Src = series ; Src = 'SCALAR-SUB' ),
              format("  T=~w supp=~2f src=~w type=~w~n", [T, Supp, Src, Type]) )) )),
    % type-uniformity of the 7 seriesless constraints across their timelines
    forall(member(C, [demographic_skill_mismatch,proxy_measurement_validity,regime_change_structural_break,
                      solar_integration_mechanism,thermal_dissipation_constraint,transfer_gap_physics,
                      voltage_regulation_tradeoff]),
        ( temporal_residual:constraint_time_set(C, Times),
          findall(Ty, ( member(T, Times), drl_composition:classify_at_time(C, T, Ctx, Ty, _) ), Tys),
          sort(Tys, U),
          format("seriesless ~w: types=~w~n", [C, U]) )).
