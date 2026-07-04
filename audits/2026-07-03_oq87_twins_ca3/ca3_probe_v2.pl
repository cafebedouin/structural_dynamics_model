% CA-3 arm probe v2 (OQ-87 twins characterization, PLAN.md 2026-07-03). READ-ONLY.
% Adapts the banked ca3_probe.pl + ca3_cause_probe.pl (2026-06-07) with:
%   (1) 3-way observer bucket per OQ-51 nullable H0 (coherent/incoherent/undetermined;
%       + err/fail, never silently dropped);
%   (2) retract+assertz corpus_path overlay AFTER consult(stack) + clear_all_caches;
%   (3) corpus parameterized via argv; loaded-count control (C4) emitted per arm;
%   (4) cause fields on EVERY row (fired atoms+groundings, terminals, ALL gap terms).
% Emits raw ROWs only — all tables computed downstream (Phase-1/Phase-2 separation).
% Run: cd prolog && swipl ../audits/2026-07-03_oq87_twins_ca3/ca3_probe_v2.pl <corpus_path>
:- initialization(main, main).

death_terminal(axiom_foreclosure).
death_terminal(husk).
death_terminal(extinction).
death_terminal(repudiation).

main([CorpusPathAtom|_]) :-
    consult(stack),
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, CorpusPathAtom)),
    corpus_loader:load_all_testsets,
    ( catch(cache_registry:clear_all_caches, _, true) -> true ; true ),
    format('ARM corpus_path=~w~n', [CorpusPathAtom]),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NLoaded),
    format('C4 loaded_count=~w~n', [NLoaded]),
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C), narrative_ontology:cs_story_uid(C, _) ),
        NPool),
    NNoUid is NLoaded - NPool,
    format('POOL kernel_bearing=~w no_uid=~w~n', [NPool, NNoUid]),
    forall(
        ( corpus_loader:corpus_constraint(C),
          C \== catholic_church_1200,
          narrative_ontology:cs_story_uid(C, U) ),
        emit_row(C, U)),
    halt.
main(_) :-
    format(user_error, 'usage: swipl ca3_probe_v2.pl <corpus_path>~n', []),
    halt(1).

obs_bucket(C, Obs) :-
    (   catch(grothendieck_cohomology:cohomological_obstruction(C, H0, _), _, H0 = err)
    ->  true
    ;   H0 = fail
    ),
    (   H0 == 1    -> Obs = coherent
    ;   H0 == 0    -> Obs = incoherent
    ;   H0 == null -> Obs = undetermined
    ;   Obs = H0                                  % err | fail
    ).

emit_row(C, U) :-
    obs_bucket(C, Obs),
    findall(A-G, ( cs_axiom_engine:cs_axiom_foreclosed(U, A),
                   ( narrative_ontology:cs_axiom_grounding(U, A, G) -> true ; G = '?' ) ),
            Fores),
    findall(T, ( cs_drift_engine:cs_drift_trajectory(U, _, T), death_terminal(T) ), Terms0),
    sort(Terms0, Terms),
    findall(Gap, narrative_ontology:cs_drift_state(U, _, Gap), Gaps),
    ( ( Fores \= [] ; Terms \= [] ) -> Com = dead ; Com = live ),
    ( narrative_ontology:cs_kernel_id(C, K) -> true ; K = '(none)' ),
    format('ROW|~w|~w|~w|~w|axfc=~q|terminals=~q|gaps=~q~n',
           [K, C, Obs, Com, Fores, Terms, Gaps]).
