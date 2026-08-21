% OQ-67 Step 0 — pre-write positive controls (KILL CONDITION #1)
% Each probed predicate gets its own control because the chi cascade routes them apart.
% Run from prolog/:  swipl -g "['../audits/2026-07-25_oq67_legacy_chi_retire/step0_controls'], run_step0, halt" -t "halt(1)"

:- [stack].
:- use_module(probe_harness).

run_step0 :-
    corpus_loader:load_all_testsets,
    format("~n=== OQ-67 STEP 0 CONTROLS ===~n"),
    c0a, c0b, c0c,
    format("=== STEP 0: ALL THREE CONTROLS PASS ===~n").

c0a :-
    (   probe_harness:with_asserted(
          [
         reach_undeclared(retrofit('2026-08-21', "bare with_asserted/2: fixture planted into an undefined predicate; no template, so no declared query shape (OQ-326 clause 4')"),
           drl_composition:constraint_data(oq67_ctl_a,
              [base_extractiveness(0.9), coord_function(false)]))
        ],
          ( drl_composition:is_snare(oq67_ctl_a), drl_composition:is_rope(oq67_ctl_a) ))
    ->  format("C0a PASS  is_snare(oq67_ctl_a) AND is_rope(oq67_ctl_a) both succeed~n")
    ;   format("C0a *** FAIL *** — STOP, do not delete~n"), fail
    ).

c0b :-
    (   probe_harness:with_asserted(
          [
         reach_undeclared(retrofit('2026-08-21', "bare with_asserted/2: fixture planted into an undefined predicate; no template, so no declared query shape (OQ-326 clause 4')"),
           drl_composition:constraint_data(oq67_ctl_b,
              [base_extractiveness(0.40), coord_function(false)]))
        ],
          drl_composition:is_mountain(oq67_ctl_b))
    ->  format("C0b PASS  is_mountain(oq67_ctl_b) succeeds~n")
    ;   format("C0b *** FAIL *** — STOP, do not delete~n"), fail
    ).

c0c :-
    (   probe_harness:with_asserted(
          [
         reach_undeclared(retrofit('2026-08-21', "bare with_asserted/2: fixture planted into an undefined predicate; no template, so no declared query shape (OQ-326 clause 4')"),
           drl_composition:constraint_data(oq67_ctl_a,
                [base_extractiveness(0.9), coord_function(false)])),
         reach_undeclared(retrofit('2026-08-21', "bare with_asserted/2: fixture planted into an undefined predicate; no template, so no declared query shape (OQ-326 clause 4')"),
           drl_composition:agent_index(oq67_a1, context(powerless, x, x, x))),
         reach_undeclared(retrofit('2026-08-21', "bare with_asserted/2: fixture planted into an undefined predicate; no template, so no declared query shape (OQ-326 clause 4')"),
           drl_composition:agent_index(oq67_a2, context(institutional, x, x, x)))
        ],
          drl_composition:detect_perspectival_risk(oq67_ctl_a, oq67_a1, oq67_a2, type_iii))
    ->  format("C0c PASS  detect_perspectival_risk(...,type_iii) succeeds (agent_index channel closed)~n")
    ;   format("C0c *** FAIL *** — STOP, do not delete~n"), fail
    ).
