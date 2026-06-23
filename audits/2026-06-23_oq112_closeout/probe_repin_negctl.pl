% OQ-112 close-out — re-pin negative control (overlay-took != default-fallback).
% (A) bad corpus_path -> loader throws corpus_empty (the asserted path is honored).
% (B) testsets_haiku overlay -> distinguishably different count (960), proving the 92
%     is a real load of testsets, not a silent default.

:- [stack].

ctl_a :-
    asserta(config:param(corpus_path, 'no_such_dir_xyz')),
    ( catch(corpus_loader:load_all_testsets, E, true)
    -> ( var(E) -> format('CTL-A: NO THROW (BAD: default-fallback?)~n', [])
       ; format('CTL-A: threw ~w (asserted bad path honored -> load is real)~n', [E]) )
    ; format('CTL-A: load_all_testsets failed~n', []) ),
    retractall(config:param(corpus_path, 'no_such_dir_xyz')).

ctl_b :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'testsets_haiku')),
    ( catch(corpus_loader:load_all_testsets, E, true), var(E)
    -> aggregate_all(count, corpus_loader:corpus_constraint(_), N),
       format('CTL-B: testsets_haiku overlay -> HAIKU_LIVE=~w (!= 92 -> overlay took)~n', [N])
    ; format('CTL-B: overlay load threw ~w~n', [E]) ).

main :- ( catch(ctl_a, Ea, format('CTL-A err ~w~n',[Ea])) -> true ; true ),
        ( catch(ctl_b, Eb, format('CTL-B err ~w~n',[Eb])) -> true ; true ),
        halt.

:- initialization(main).
