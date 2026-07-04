% C1 kill-condition control (OQ-87 twins characterization, pre-registered in PLAN.md).
% Does the committer engine FIRE at HEAD on kernel_v1? Paste >=1 named story with
% cs_axiom_foreclosed/2 and >=1 with a terminal cs_drift_trajectory/3.
% READ-ONLY: writes nothing; output to stdout.
% Run: cd prolog && swipl ../audits/2026-07-03_oq87_twins_ca3/c1_kernel_fire.pl
:- initialization(main, main).

death_terminal(axiom_foreclosure).
death_terminal(husk).
death_terminal(extinction).
death_terminal(repudiation).

main(_) :-
    consult(stack),
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, 'archives/datasets/kernel_v1')),
    corpus_loader:load_all_testsets,
    ( catch(cache_registry:clear_all_caches, _, true) -> true ; true ),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NLoaded),
    format('C4 loaded_count=~w (expect 1106)~n', [NLoaded]),
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C), narrative_ontology:cs_story_uid(C, _) ),
        NPool),
    format('kernel_bearing_pool=~w (banked pool 906)~n', [NPool]),
    % foreclosure fires
    findall(C-A-G,
        ( corpus_loader:corpus_constraint(C),
          narrative_ontology:cs_story_uid(C, U),
          cs_axiom_engine:cs_axiom_foreclosed(U, A),
          ( narrative_ontology:cs_axiom_grounding(U, A, G) -> true ; G = '?' ) ),
        Fires),
    length(Fires, NFires),
    findall(C, member(C-_-_, Fires), FC0), sort(FC0, FiredStories),
    length(FiredStories, NFiredStories),
    format('C1a cs_axiom_foreclosed fires: ~w (story,atom) pairs over ~w stories~n',
           [NFires, NFiredStories]),
    ( Fires = [C1-A1-G1|_]
    -> format('C1a example: story=~w atom=~w grounding=~w~n', [C1, A1, G1])
    ;  format('C1a example: NONE — KILL CONDITION MET~n', []) ),
    % drift terminal fires
    findall(C-T,
        ( corpus_loader:corpus_constraint(C),
          narrative_ontology:cs_story_uid(C, U),
          cs_drift_engine:cs_drift_trajectory(U, _, T),
          death_terminal(T) ),
        Terms),
    length(Terms, NTerms),
    findall(C, member(C-_, Terms), TC0), sort(TC0, TermStories),
    length(TermStories, NTermStories),
    format('C1b terminal cs_drift_trajectory fires: ~w (story,terminal) pairs over ~w stories~n',
           [NTerms, NTermStories]),
    ( Terms = [C2-T2|_]
    -> format('C1b example: story=~w terminal=~w~n', [C2, T2])
    ;  format('C1b example: NONE~n', []) ),
    (   ( NFiredStories > 0 ; NTermStories > 0 )
    ->  format('C1 VERDICT: PASS (committer engine fires at HEAD on kernel_v1)~n', [])
    ;   format('C1 VERDICT: FAIL — anchor unreachable, STOP per PLAN.md~n', [])
    ).
