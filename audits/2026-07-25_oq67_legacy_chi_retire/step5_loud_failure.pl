% OQ-67 Step 5 — loud-failure control (KILL CONDITION #2).
% Witnesses the property Step 6 depends on: after the deletion the six predicates are
% UNDEFINED (existence_error), not defined-and-failing. Without this, Step 6's exit 0 is
% indistinguishable from a run that never looked.
% Run from prolog/:
%   swipl -g "['../audits/2026-07-25_oq67_legacy_chi_retire/step5_loud_failure'], run_step5, halt" -t "halt(1)"

:- [stack].

run_step5 :-
    format("~n=== OQ-67 STEP 5: LOUD-FAILURE CONTROL ===~n"),
    forall( member(G, [ drl_composition:is_snare(oq67_nonexistent),
                        drl_composition:is_mountain(oq67_nonexistent),
                        drl_composition:is_rope(oq67_nonexistent),
                        drl_composition:detect_perspectival_risk(x, y, z, _),
                        drl_composition:constraint_data(x, _),
                        drl_composition:agent_index(x, _) ]),
            report(G) ),
    format("--- now the conjunctive assertion (must succeed) ---~n"),
    (   forall( member(G2, [ drl_composition:is_snare(oq67_nonexistent),
                             drl_composition:is_mountain(oq67_nonexistent),
                             drl_composition:is_rope(oq67_nonexistent),
                             drl_composition:detect_perspectival_risk(x, y, z, _),
                             drl_composition:constraint_data(x, _),
                             drl_composition:agent_index(x, _) ]),
                ( catch(G2, E, true),
                  nonvar(E),
                  E = error(existence_error(procedure, _), _) ))
    ->  format("=== STEP 5 PASS — all six throw existence_error; deletion converted~n"),
        format("    the failure mode from SILENT to LOUD. Step 6 is now probative. ===~n")
    ;   format("=== STEP 5 *** FAIL *** — at least one goal fails silently.~n"),
        format("    Step 6 would prove NOTHING. STOP and reassess. ===~n"), fail
    ).

report(G) :-
    (   catch(G, E, true),
        nonvar(E),
        E = error(existence_error(procedure, PI), _)
    ->  format("  THROWS existence_error: ~w~n", [PI])
    ;   format("  *** does NOT throw (silent fail or success): ~w ***~n", [G])
    ).
