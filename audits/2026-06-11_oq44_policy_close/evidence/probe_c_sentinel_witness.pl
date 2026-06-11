:- [stack].
:- corpus_loader:load_all_testsets.
run :-
    constraint_indexing:default_context(Ctx),
    % Sentinel produced on absence
    drl_core:get_raw_suppression(no_such_constraint_xyz, V),
    format("absent scalar -> get_raw_suppression = ~w  (expect unknown)~n", [V]),
    % Guard fails closed, no throw
    (   catch(drl_core:classify_from_metrics(no_such_constraint_xyz, 0.4, 0.4, V, Ctx, T1),
              E, (T1 = threw(E)))
    ->  format("classify_from_metrics on sentinel -> ~w  (UNEXPECTED success/throw)~n", [T1])
    ;   format("classify_from_metrics on sentinel -> failed cleanly  (expect this)~n", [])
    ),
    % Positive control: numeric Supp still classifies on a real constraint
    corpus_loader:corpus_constraint(C),
    drl_core:get_raw_suppression(C, S), number(S),
    drl_core:classify_from_metrics(C, 0.7, 0.7, S, Ctx, T2), !,
    format("positive control ~w supp=~w -> ~w  (numeric path alive)~n", [C, S, T2]).
