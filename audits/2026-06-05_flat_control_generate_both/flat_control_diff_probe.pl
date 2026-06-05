run_probe :-
    consult('testsets/flatctl_probe/colorblind_reading.pl'),
    consult('testsets/flatctl_probe/equal_protection_kernel_flat_control.pl'),
    forall(narrative_ontology:flat_control_of(Flat, Kernel),
           ( format('ALIGNMENT KEY: ~w is flat control of kernel ~w~n', [Flat, Kernel]),
             forall(narrative_ontology:cs_kernel_id(Reading, Kernel),
                    ( format('  pair: ~w (reading) vs ~w (flat)~n', [Reading, Flat]),
                      forall(( drl_core:standard_context(Ctx),
                               Ctx = context(agent_power(P), _, _, _) ),
                             ( drl_core:dr_type(Reading, Ctx, TR),
                               drl_core:dr_type(Flat, Ctx, TF),
                               ( TR == TF -> Tag = same ; Tag = 'DIVERGES' ),
                               format('    P=~w: reading=~w flat=~w ~w~n', [P, TR, TF, Tag])
                             ))
                    ))
           )).
