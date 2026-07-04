% C2/C3 controls on testsets/ (OQ-87 twins characterization, PLAN.md).
% Three hand-read stories (facts pasted in FINDINGS): the engine must match the hand read.
%   benchmark_saturation_interpretation      — hand: FORECLOSED (3x empirically_contingent
%                                              + gap(axiom_overriding,substantial,false)) => dead
%   animal_status_kernel__property_reading   — hand: NOT foreclosed (deontological/conventional),
%                                              gap(authority_erosion,substantial,false) => husk => dead
%   architectural_pattern_validity           — hand: empirically_contingent PRESENT but
%                                              gap(stable,minor,true) => stable_pattern => live
% Also emits the per-context dr_type vector + H0 for each (independent read of the
% observer side, for the C2 conjunction-cell identification).
% READ-ONLY. Run: cd prolog && swipl ../audits/2026-07-03_oq87_twins_ca3/c23_testsets_controls.pl
:- initialization(main, main).

death_terminal(axiom_foreclosure).
death_terminal(husk).
death_terminal(extinction).
death_terminal(repudiation).

control_story(benchmark_saturation_interpretation,    expect(foreclosed, dead)).
control_story(animal_status_kernel__property_reading, expect(not_foreclosed, dead)).
control_story(architectural_pattern_validity,         expect(not_foreclosed, live)).

main(_) :-
    consult(stack),
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, testsets)),
    corpus_loader:load_all_testsets,
    ( catch(cache_registry:clear_all_caches, _, true) -> true ; true ),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NLoaded),
    format('C4 loaded_count=~w (expect 119)~n', [NLoaded]),
    forall(control_story(C, Expect), report(C, Expect)),
    halt.

report(C, Expect) :-
    format('~n=== ~w (~w) ===~n', [C, Expect]),
    ( narrative_ontology:cs_story_uid(C, U)
    -> format('uid=~w~n', [U]),
       findall(A-G, ( cs_axiom_engine:cs_axiom_foreclosed(U, A),
                      ( narrative_ontology:cs_axiom_grounding(U, A, G) -> true ; G = '?' ) ),
               Fores),
       format('cs_axiom_foreclosed: ~q~n', [Fores]),
       findall(G-T, cs_drift_engine:cs_drift_trajectory(U, G, T), Trajs),
       format('cs_drift_trajectory: ~q~n', [Trajs]),
       ( ( Fores \= [] ; member(_-T2, Trajs), death_terminal(T2) )
       -> Com = dead ; Com = live ),
       format('committer_verdict=~w~n', [Com])
    ;  format('NO cs_story_uid — control broken~n', []) ),
    % observer side, two reads: per-context dr_type (independent) + cohomological H0
    constraint_indexing:site_contexts(Ctxs),
    findall(Ctx-T, ( member(Ctx, Ctxs),
                     ( catch(drl_core:dr_type(C, Ctx, T0), _, T0 = err) -> T = T0 ; T = fail ) ),
            Vec),
    format('per-context dr_type: ~q~n', [Vec]),
    ( catch(grothendieck_cohomology:cohomological_obstruction(C, H0, H1), _, (H0 = err, H1 = err))
    -> true ; H0 = fail, H1 = fail ),
    format('cohomological H0=~w H1=~w~n', [H0, H1]).
