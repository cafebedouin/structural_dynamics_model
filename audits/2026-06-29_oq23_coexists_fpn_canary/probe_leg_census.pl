% Per-leg clean census. CORPUS env var selects the leg via corpus_path overlay.
:- initialization((
    catch(run, E, (print_message(error,E), halt(2))),
    halt(0)
)).
run :-
    ( getenv('CORPUS', CDir), CDir \== '' ->
        retractall(config:param(corpus_path, _)),
        asserta(config:param(corpus_path, CDir))
    ; true ),
    test_coexists_fpn_canary:run_coexists_census,
    ( getenv('FORECLOSES', _) -> test_coexists_fpn_canary:run_forecloses_census ; true ).
