% eps_census_dump.pl — read-only ε census over one corpus leg (OQ-205 spec recon).
% Usage: swipl -g "main" eps_census_dump.pl -- <corpus_path> [plant]
% Emits "id<TAB>eps" per corpus_constraint/1 via the LIVE read path
% (constraint_data:base_extractiveness/2). Writes nothing to the repo.
% With `plant`: asserts an in-memory planted constraint at snare_epsilon_floor + 0.0005
% (the positive control — must surface through the SAME enumeration + read path).

main :-
    current_prolog_flag(argv, Argv),
    ( append(_, ['--', CorpusPathAtom | Rest], Argv) -> true
    ; Argv = [CorpusPathAtom | Rest] ),
    [stack],
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, CorpusPathAtom)),
    corpus_loader:load_all_testsets,
    ( member(plant, Rest) ->
        config:param(snare_epsilon_floor, Floor),
        PlantVal is Floor + 0.0005,
        assertz(corpus_loader:corpus_constraint(census_planted_control)),
        assertz(narrative_ontology:constraint_metric(census_planted_control, extractiveness, PlantVal))
    ; true ),
    forall(corpus_loader:corpus_constraint(C),
        ( ( constraint_data:base_extractiveness(C, E) -> true ; E = no_eps ),
          format("~w\t~w~n", [C, E]) )),
    halt.
main :- halt(1).
