:- initialization(main).
main :-
    [stack],
    % Positive control: the naturalized profile (eps>0.45, chi<0.35) must fire naturalized.
    ( drl_core:classify_from_metrics(pc_nat, 0.90, 0.20, 0.50, context(agent_power(analytical),time_horizon(medium),exit_options(constrained),spatial_scope(national)), T1)
      -> format("PC naturalized-profile (eps=0.90 chi=0.20 supp=0.50) -> ~w~n", [T1]) ; writeln("PC naturalized-profile FAILED to classify") ),
    % Two-sided: raise chi above snare floor (0.66) -> should be snare, not naturalized.
    ( drl_core:classify_from_metrics(pc_snare, 0.90, 0.70, 0.70, context(agent_power(analytical),time_horizon(medium),exit_options(constrained),spatial_scope(national)), T2)
      -> format("PC snare-profile    (eps=0.90 chi=0.70 supp=0.70) -> ~w~n", [T2]) ; writeln("PC snare-profile FAILED to classify") ),
    % Discrimination: same as naturalized profile but chi in TR band (0.35<chi<=0.90) -> not naturalized
    ( drl_core:classify_from_metrics(pc_edge, 0.90, 0.34, 0.50, context(agent_power(analytical),time_horizon(medium),exit_options(constrained),spatial_scope(national)), T3)
      -> format("PC just-below-floor  (eps=0.90 chi=0.34 supp=0.50) -> ~w~n", [T3]) ; writeln("PC edge FAILED") ),
    ( drl_core:classify_from_metrics(pc_edge2, 0.90, 0.35, 0.50, context(agent_power(analytical),time_horizon(medium),exit_options(constrained),spatial_scope(national)), T4)
      -> format("PC at-floor(0.35)    (eps=0.90 chi=0.35 supp=0.50) -> ~w~n", [T4]) ; writeln("PC edge2 FAILED") ),
    halt.
