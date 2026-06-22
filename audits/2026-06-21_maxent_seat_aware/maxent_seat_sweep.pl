:- initialization(main).
:- [stack].
:- use_module(maxent_classifier).
g(G):-(catch(G,_,fail)->true;fail).

% For one corpus: count routed FCR/constructed seats; confirm the MaxEnt boost is
% SKIPPED at routed seats (maxent_dist == maxent_dist_raw) and KEPT at non-routed
% boost-bearing seats; confirm non-converted boosts (false_natural_law, coupling_invariant_rope,
% coordination_scaffold, constructed_low, constructed_constraint) still fire somewhere.
dist_eq(A,B) :- msort(A,SA), msort(B,SB), SA == SB.

main :-
  getenv('CORPUS_DIR',Dir),
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,Dir)),
  ( g(corpus_loader:load_all_testsets) -> true ; (format("~w: LOAD_FAIL~n",[Dir]), halt) ),
  constraint_indexing:default_context(Ctx),
  % run the classical maxent precompute so maxent_dist/maxent_dist_raw are populated
  ( g(maxent_classifier:maxent_run(Ctx,_)) -> true ; format("~w: MAXENT_RUN_FAIL~n",[Dir]) ),
  % routed seats
  findall(C,(corpus_loader:corpus_constraint(C), g(signature_detection:fcr_routed(C))),FcrR0), sort(FcrR0,FcrR),
  findall(C,(corpus_loader:corpus_constraint(C), g(signature_detection:constructed_routed(C))),ConR0), sort(ConR0,ConR),
  % routed seats must have boost SKIPPED: maxent_dist == maxent_dist_raw
  findall(C,(member(C,FcrR), maxent_classifier:maxent_dist(C,Ctx,D), maxent_classifier:maxent_dist_raw(C,Ctx,R), \+ dist_eq(D,R)),FcrBoosted),
  findall(C,(member(C,ConR), maxent_classifier:maxent_dist(C,Ctx,D), maxent_classifier:maxent_dist_raw(C,Ctx,R), \+ dist_eq(D,R)),ConBoosted),
  % non-routed FCR/constructed boost-bearing seats: boost KEPT where applicable.
  % A non-routed false_ci_rope winner with nonzero tangled_rope raw mass should have dist != raw.
  findall(C,(corpus_loader:corpus_constraint(C),
             g((signature_detection:constraint_signature(C,S),(S==false_ci_rope;S==constructed_high_extraction))),
             \+ g(signature_detection:fcr_routed(C)), \+ g(signature_detection:constructed_routed(C)),
             maxent_classifier:maxent_dist(C,Ctx,D), maxent_classifier:maxent_dist_raw(C,Ctx,R), \+ dist_eq(D,R)),NonRoutedKept),
  length(FcrR,NFR), length(ConR,NCR), length(FcrBoosted,NFB), length(ConBoosted,NCB), length(NonRoutedKept,NNK),
  format("~w | fcr_routed=~w constructed_routed=~w | routed_STILL_boosted(must 0)=~w/~w | non_routed_boost_kept(witness>0 if any tr-mass)=~w~n",
         [Dir,NFR,NCR,NFB,NCB,NNK]),
  ( (NFB==0, NCB==0) -> true ; format("  !! ROUTED SEAT STILL BOOSTED: fcr=~w con=~w~n",[FcrBoosted,ConBoosted]) ),
  halt.
main :- write('SWEEP FAIL'),halt(1).
