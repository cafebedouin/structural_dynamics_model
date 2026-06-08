% diverge-A cause-of-death witness (READ-ONLY; kernel_v1-regime).
% For each coherent(H0=1)+dead constraint, emit WHAT killed it:
%   the foreclosure path(s), the terminal, the drift gap(dir,mag,ack), the foreclosed atom+grounding.
% Heterogeneous cause across the 74 = real orthogonality; uniform same convention = saturation.
:- initialization(main).

death_terminal(axiom_foreclosure).
death_terminal(husk).
death_terminal(extinction).
death_terminal(repudiation).

main :-
    assertz(config:param(corpus_path, 'archives/datasets/kernel_v1')),
    consult(stack),
    corpus_loader:load_all_testsets,
    ( catch(cache_registry:clear_all_caches, _, true) -> true ; true ),
    forall(
        ( corpus_loader:corpus_constraint(C), C \== catholic_church_1200,
          narrative_ontology:cs_story_uid(C, U),
          catch(grothendieck_cohomology:cohomological_obstruction(C, 1, _), _, fail) ), % observer-coherent only
        ( % committer death cause
          ( cs_axiom_engine:cs_axiom_foreclosed(U, Atom)
            -> ( narrative_ontology:cs_axiom_grounding(U, Atom, G) -> true ; G='?' ), AxFc = ax(Atom,G)
            ;  AxFc = none ),
          findall(Term, ( cs_drift_engine:cs_drift_trajectory(U, _, Term), death_terminal(Term) ), Terms0),
          sort(Terms0, Terms),
          ( narrative_ontology:cs_drift_state(U, _, gap(Dir,Mag,Ack)) -> Gap=gap(Dir,Mag,Ack) ; Gap=no_drift ),
          ( (AxFc \= none ; Terms \= []) ->   % dead
              format('DA ~w | axfc=~w | terminals=~w | drift=~w~n', [C, AxFc, Terms, Gap])
          ; true )
        )),
    halt.
