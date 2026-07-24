:- initialization(main).
main :-
    [stack], corpus_loader:load_all_testsets,
    use_module(data_validation),
    % pick a real kernel with >=2 CIDs
    once(( narrative_ontology:cs_kernel_id(C1,K), narrative_ontology:cs_kernel_id(C2,K), C1@<C2 )),
    format("kernel ~w has CIDs ~w, ~w~n",[K,C1,C2]),
    % (i) two DIFFERING authored values -> reported
    assertz(narrative_ontology:update_authority(C1, frozen)),
    assertz(narrative_ontology:update_authority(C2, licensed_revisable)),
    ( data_validation:inconsistent_update_authority(K, Vs) -> format("(i) differing -> REPORTED ~w ✓~n",[Vs]) ; format("(i) differing -> not reported ✗~n") ),
    % (ii) AGREE -> silent
    retract(narrative_ontology:update_authority(C2, licensed_revisable)),
    assertz(narrative_ontology:update_authority(C2, frozen)),
    ( data_validation:inconsistent_update_authority(K, _) -> format("(ii) agree -> reported ✗~n") ; format("(ii) agree -> silent ✓~n") ),
    % (iii) one authored / one unauthored -> partial, silent
    retract(narrative_ontology:update_authority(C2, frozen)),
    ( data_validation:inconsistent_update_authority(K, _) -> format("(iii) partial -> reported ✗~n") ; format("(iii) partial (1 authored) -> silent ✓~n") ),
    retract(narrative_ontology:update_authority(C1, frozen)),
    halt.
