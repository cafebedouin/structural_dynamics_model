:- module(test_residual_signature_inert, []).
:- use_module(library(plunit)).

/* OQ-138 (2026-07-14) — the residual-signature MONITOR as a pipeline gate.
   The abstain guard prevents silent manufacture at runtime; this gate is the monitored surface
   that makes a future fire LOUD (pipeline abort) so the successor OQ auto-reopens for the owed
   fire-time discriminant ruling. Run by run_pipeline._prolog_residual_signature_gate over the
   loaded corpus. Fail-closed by construction (run_tests failing -> nonzero exit -> PrologError).
*/

:- begin_tests(residual_signature_inert).

% GATE: no residual clause fires on the loaded corpus. A nonzero count is the reopen witness.
test(residual_clauses_inert_on_corpus) :-
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          signature_detection:residual_signature_firing(C) ),
        N),
    (   N =:= 0
    ->  true
    ;   format(user_error, "~n[OQ-138] residual_signature_firing count = ~w on loaded corpus (>0 => reopen: a residual clause fired; guard abstained it to unknown, fire-time discriminant now owed)~n", [N]),
        fail
    ).

% Non-vacuity positive control (Build Discipline Pattern 5): the monitor CAN read the residual
% shape, so the 0 above is measured-empty, not didn't-look.
test(monitor_non_vacuous) :-
    signature_detection:residual_signature_pattern(mountain, coordination_scaffold),
    signature_detection:residual_signature_pattern(unknown, constructed_constraint),
    \+ signature_detection:residual_signature_pattern(rope, coupling_invariant_rope).

:- end_tests(residual_signature_inert).
