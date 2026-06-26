% OQ-18 realized-flip probe 3 (read-only). Corpus via argv: swipl -q probe.pl -- testsets_haiku
% FIX over probe2: the flip is in conjunct (2) of cs_is_metric_stable
%   ( network_drift_velocity(C,Ctx,V,_), V >= Thresh ),
% which runs regardless of detect_network_contamination (that is conjunct 1,
% already satisfied for any serialized verdict). So FLIPPABLE(C) =
%   network_drift_velocity(C) has >=1 contributing neighbor with a non-monotone
%   base_extractiveness series (its endpoint drift_velocity feeds V; only a
%   non-monotone series can make endpoint-V differ from faithful-V across Thresh).
% Positive control: the non-monotone filter, applied to the known non-monotone
% set, MUST return it non-empty (proves the filter dispatches, not a dead 0).

:- initialization(main).

corpus_dir(Dir) :-
    ( current_prolog_flag(argv, [D|_]) -> Dir = D ; Dir = testsets ).

main :-
    corpus_dir(Dir),
    use_module(stack),
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),
    config:param(network_drift_velocity_threshold, Thresh),
    format('~n===== CORPUS: ~w   Thresh=~w =====~n', [Dir, Thresh]),

    % positive control for the non-monotone filter (non-vacuous)
    findall(C, ( corpus_loader:corpus_constraint(C),
                 drl_composition:non_monotonic_trajectory(C, base_extractiveness) ), NM),
    length(NM, NNM),
    ( NNM > 0
    -> format('CONTROL: non-monotone filter returns ~w constraints (dispatches, good)~n', [NNM])
    ;  format('CONTROL: non-monotone filter returns 0 -- treat flip-count as UNWITNESSED~n', []) ),

    % serialized verdicts
    findall(UID-C, ( cs_drift_mismatch:cs_drift_mismatch(UID, _),
                     narrative_ontology:cs_story_uid(C, UID) ), P0),
    sort(P0, Pairs), length(Pairs, NP),
    format('serialized cs_drift_mismatch verdicts: ~w~n', [NP]),

    foldl(report(Ctx, Thresh), Pairs, s(0,0,0), s(GateLive, Flip, _)),
    format('  verdicts whose velocity sum has >=1 contributor : ~w~n', [GateLive]),
    format('  verdicts REALIZED-FLIPPABLE (non-mono contributor): ~w~n', [Flip]),
    halt.
main :- writeln('PROBE FAILED'), halt(1).

report(Ctx, Thresh, _UID-C, s(GIn,FIn,X), s(GOut,FOut,X)) :-
    ( catch(network_dynamics:network_drift_velocity(C, Ctx, V, Contribs), _, (V= -1, Contribs=[]))
    -> true ; V = -1, Contribs = [] ),
    length(Contribs, NC),
    ( NC > 0 -> GOut is GIn + 1 ; GOut = GIn ),
    findall(O, ( member(contributor(O,_,_,_), Contribs),
                 drl_composition:non_monotonic_trajectory(O, base_extractiveness) ), NMNbrs),
    ( NMNbrs \= []
    -> FOut is FIn + 1,
       format('  [FLIPPABLE] ~w  V=~4f (Thresh=~w) contributors=~w non-mono=~w~n',
              [C, V, Thresh, NC, NMNbrs])
    ;  FOut = FIn,
       ( NC > 0
       -> format('  [gate-live ] ~w  V=~4f contributors=~w (all monotone)~n', [C, V, NC])
       ;  true ) ).
