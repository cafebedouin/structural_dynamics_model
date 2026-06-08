% CA-3 detection-independence density on kernel_v1 (READ-ONLY; kernel_v1-regime).
% Observer coherent = H0=1; committer dead = cs_axiom_foreclosed OR drift terminal in death-set.
% Emits one ROW per kernel-bearing constraint. diverge-A/B reported separately downstream.
:- initialization(main).

death_terminal(axiom_foreclosure).
death_terminal(husk).
death_terminal(extinction).
death_terminal(repudiation).

committer_dead(U) :- cs_axiom_engine:cs_axiom_foreclosed(U, _), !.
committer_dead(U) :- cs_drift_engine:cs_drift_trajectory(U, _, T), death_terminal(T), !.

main :-
    assertz(config:param(corpus_path, 'archives/datasets/kernel_v1')),
    consult(stack),
    corpus_loader:load_all_testsets,
    ( catch(cache_registry:clear_all_caches, _, true) -> true ; true ),
    forall(
        ( corpus_loader:corpus_constraint(C),
          C \== catholic_church_1200,
          narrative_ontology:cs_story_uid(C, U) ),
        ( ( catch(grothendieck_cohomology:cohomological_obstruction(C, H0, _), _, H0= err) -> true ; H0=fail ),
          ( H0 == 1 -> Obs=coherent ; Obs=incoherent ),
          ( committer_dead(U) -> Com=dead ; Com=live ),
          ( narrative_ontology:cs_kernel_id(C, K) -> true ; K='(none)' ),
          format('ROW ~w | ~w | ~w | ~w~n', [K, C, Obs, Com])
        )),
    halt.
