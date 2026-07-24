:- initialization(main).
:- use_module(library(lists)).
main :-
    [stack], corpus_loader:load_all_testsets,
    use_module(cs_drift_engine),
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    % which corpus constraints even HAVE a drift trajectory (need cs_drift_state via uid)?
    findall(C-Term,
        ( member(C,Cs), narrative_ontology:cs_story_uid(C,UID),
          cs_drift_engine:cs_drift_trajectory(UID,_,Term) ), Traj),
    length(Traj, NT),
    format("corpus=~w ; constraints with a drift trajectory=~w~n", [_,NT]),
    ( Traj \= [] ->
        findall(T,member(_-T,Traj),Ts), msort(Ts,Ts1), clumped(Ts1,Dist),
        format("terminal distribution: ~w~n",[Dist]),
        % near-miss proxy: husk/extinction assigned WHERE the referent may be gone
        % (founding_problem_status=dead AND disappearance suggests the referent itself dissolved).
        % No authored referent-dissolution field exists (that IS the gap), so this is the closest proxy.
        findall(C-Term,
            ( member(C-Term,Traj), memberchk(Term,[husk,extinction]),
              narrative_ontology:founding_problem_status(C,dead) ), Cand),
        length(Cand,NC),
        format("~nhusk/extinction assignments with founding_problem_status=dead (referent-decay proxy): ~w~n",[NC]),
        forall(member(C-T,Cand), format("   ~w -> ~w~n",[C,T]))
    ; format("(no drift trajectories on this corpus leg)~n") ),
    halt.
