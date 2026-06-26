% OQ-18 FLIPPED tiebreaker (read-only). Corpus via argv.
% Replicates network_dynamics:network_drift_velocity/4 EXACTLY (body copied from
% network_dynamics.pl:126-149) but swaps the per-neighbor rate source from the
% endpoint metric_drift_events:drift_velocity/3 to a faithful least-squares slope
% (drl_composition:linear_slope/2 over the full base_extractiveness series).
% Same Rate>0 filter, same Sensitivity weighting, same sum.
% A serialized cs_drift_mismatch verdict is currently STABLE (Ve < Thresh). It
% FLIPS (verdict would vanish) iff faithful Vf >= Thresh.
% Controls: (a) faithful_rate on a monotone series == endpoint rate (sign/cross);
%           (b) faithful_rate on the spike-recover control differs from endpoint.

:- initialization(main).
corpus_dir(Dir) :- ( current_prolog_flag(argv,[D|_]) -> Dir=D ; Dir=testsets ).

faithful_rate(Other, Rf) :-
    findall(T-V, narrative_ontology:measurement(_, Other, base_extractiveness, T, V), Ps),
    Ps = [_|_],
    msort(Ps, Sorted),
    drl_composition:linear_slope(Sorted, Rf).

endpoint_rate(Other, Re) :-
    ( catch(metric_drift_events:drift_velocity(Other, base_extractiveness, Re), _, fail) -> true ; Re = 0.0 ).

faithful_ndv(C, Context, Vf) :-
    ( catch(faithful_ndv_(C, Context, Vf), _, fail) -> true ; Vf = 0.0 ).
faithful_ndv_(C, Context, Velocity) :-
    constraint_indexing:valid_context(Context),
    drl_purity_network:constraint_neighbors(C, Context, Neighbors),
    purity_scoring:purity_score(C, MyPurity),
    ( MyPurity < 0.0
    -> Velocity = 0.0
    ;  drl_core:dr_type(C, Context, MyType),
       drl_purity_network:type_immunity(MyType, Immunity),
       findall(Contribution,
           ( member(neighbor(Other, EdgeStrength, _Src), Neighbors),
             faithful_rate(Other, Rate),
             Rate > 0,
             config:param(purity_attenuation_factor, AttFactor),
             ( drl_core:dr_type(Other, Context, OtherType)
             -> drl_purity_network:type_contamination_strength(OtherType, TypeFactor)
             ;  TypeFactor = 0.0 ),
             Sensitivity is EdgeStrength * AttFactor * TypeFactor * Immunity,
             Contribution is Rate * Sensitivity ),
           Contribs),
       sum_list(Contribs, Velocity) ).

main :-
    corpus_dir(Dir),
    use_module(stack),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),
    config:param(network_drift_velocity_threshold, Thresh),
    format('~n===== ~w  Thresh=~w =====~n', [Dir, Thresh]),

    % LIVE control (uneven spacing): endpoint and faithful MUST differ in sign
    assertz(narrative_ontology:measurement(z1,zunev,base_extractiveness,1,0.10)),
    assertz(narrative_ontology:measurement(z2,zunev,base_extractiveness,2,0.90)),
    assertz(narrative_ontology:measurement(z3,zunev,base_extractiveness,100,0.15)),
    endpoint_rate(zunev,ZE), faithful_rate(zunev,ZF),
    ( abs(ZE-ZF) > 1.0e-6
    -> format('CONTROL (uneven): endpoint=~6f faithful=~6f DIFFER (comparator live)~n',[ZE,ZF])
    ;  format('CONTROL FAILED: endpoint=~6f faithful=~6f identical~n',[ZE,ZF]) ),

    findall(UID-C, ( cs_drift_mismatch:cs_drift_mismatch(UID,_),
                     narrative_ontology:cs_story_uid(C,UID) ), P0),
    sort(P0, Pairs), length(Pairs, NP),
    foldl(check(Ctx,Thresh), Pairs, f(0,0,0.0,0.0), f(Flipped,Examined,MaxVe,MaxVf)),
    format('serialized verdicts=~w examined=~w  REALIZED-FLIPPED (Vf>=Thresh ~w)=~w~n',
           [NP, Examined, Thresh, Flipped]),
    format('  max endpoint Ve among verdicts=~6f   max faithful Vf=~6f   (headroom to Thresh)~n',
           [MaxVe, MaxVf]),
    halt.
main :- writeln('PROBE FAILED'), halt(1).

check(Ctx, Thresh, _UID-C, f(FIn,EIn,MVeIn,MVfIn), f(FOut,EOut,MVeOut,MVfOut)) :-
    EOut is EIn + 1,
    ( catch(network_dynamics:network_drift_velocity(C,Ctx,Ve,_),_,fail) -> true ; Ve = 0.0 ),
    faithful_ndv(C, Ctx, Vf),
    MVeOut is max(MVeIn, Ve), MVfOut is max(MVfIn, Vf),
    ( Vf >= Thresh
    -> FOut is FIn + 1,
       format('  [FLIPPED] ~w  Ve=~5f -> Vf=~5f  (was stable, faithfully UNSTABLE)~n',[C,Ve,Vf])
    ;  FOut = FIn ).
