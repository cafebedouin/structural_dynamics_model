:- initialization(main).
main :-
    [stack], corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    forall(member(R-T, [jewish_sovereignty_palestine__settler_colonial_reading-2024,
                        jewish_sovereignty_palestine__cultural_zionist_reading-2024,
                        jewish_sovereignty_palestine__settler_colonial_reading-1948,
                        jewish_sovereignty_palestine__cultural_zionist_reading-1948]),
        ( once(drl_composition:classify_at_time(R, T, Ctx, Type)),
          format("~w @T=~w -> ~w~n", [R, T, Type]) )),
    halt.
main :- write(FAIL), nl, halt(1).
