% ============================================================================
% TEST: cs_axiom_engine.pl
% ============================================================================
% Includes the minimal contradiction graph for the state_execution_authority
% kernel triplet (abolition / retributive / deterrence readings).
%
% Run from prolog/ directory:
%   swipl -l stack.pl \
%         -l testsets/abolition_reading.pl \
%         -l testsets/retributive_reading.pl \
%         -l testsets/deterrence_reading.pl \
%         -l test_cs_axiom_engine.pl \
%         -g "run_cs_axiom_tests, halt." -t "halt(1)"
% ============================================================================

:- use_module(cs_axiom_engine).
:- use_module(cs_kernel_registry).
:- use_module(narrative_ontology).

:- dynamic cs_axiom_test_result/3.

run_cs_axiom_tests :-
    format("~n=== CS AXIOM ENGINE TESTS ===~n", []),
    forall(cs_axiom_test(Name, Goal, Expected),
           run_axiom_test(Name, Goal, Expected)),
    aggregate_all(count, cs_axiom_test_result(_, pass, _), Passed),
    aggregate_all(count, cs_axiom_test_result(_, fail, _), Failed),
    Total is Passed + Failed,
    format("~n--- Results: ~w/~w passed ---~n", [Passed, Total]),
    (   Failed > 0
    ->  format("FAILED TESTS:~n"),
        forall(cs_axiom_test_result(N, fail, R),
               format("  [FAIL] ~w: ~w~n", [N, R]))
    ;   true
    ).

run_axiom_test(Name, Goal, Expected) :-
    (   catch(call(Goal), Err, (format(atom(ErrAtom), "~w", [Err]), fail))
    ->  Got = success
    ;   Got = fail
    ),
    (   Got == Expected
    ->  assert(cs_axiom_test_result(Name, pass, "")),
        format("  [PASS] ~w~n", [Name])
    ;   (var(Err) -> Reason = "unexpected_result" ; Reason = ErrAtom),
        assert(cs_axiom_test_result(Name, fail, Reason)),
        format("  [FAIL] ~w (expected ~w, got ~w)~n", [Name, Expected, Got])
    ).

/* ================================================================
   MINIMAL CONTRADICTION GRAPH — state_execution_authority kernel
   ================================================================
   Three axiom atoms cover the foundational disagreement:

   execution_mandatory_for_proportional_justice
     The abolition and deterrence readings reject this.
     The retributive reading asserts it (natural-law retributivism).

   execution_justified_by_deterrent_effect
     The abolition reading rejects this (deterrence empirically falsified).
     The retributive reading is indifferent (secondary).
     The deterrence reading asserts it.

   execution_categorically_impermissible
     Asserted by the abolition reading.
     Directly contradicts both of the above.

   Contradictions declared:
     A ↔ C (retributive ↔ abolition: proportional-justice vs. impermissible)
     B ↔ C (deterrence ↔ abolition: deterrent-effect vs. impermissible)

   Cross-reading conflict pair (A, C): retributive ↔ abolition.
   ================================================================ */

:- multifile narrative_ontology:cs_axiom/3.
:- multifile narrative_ontology:cs_axiom_status/2.
:- multifile narrative_ontology:cs_axiom_contradiction/2.

% Retributive reading: asserts mandatory proportional justice
narrative_ontology:cs_axiom(retributive_reading, foundational,
                             execution_mandatory_for_proportional_justice).

% Deterrence reading: asserts deterrent-effect justification
narrative_ontology:cs_axiom(deterrence_reading, foundational,
                             execution_justified_by_deterrent_effect).

% Abolition reading: categorically rejects both
narrative_ontology:cs_axiom(abolition_reading, foundational,
                             execution_categorically_impermissible).

% Axiom status
narrative_ontology:cs_axiom_status(execution_mandatory_for_proportional_justice, holdable).
narrative_ontology:cs_axiom_status(execution_justified_by_deterrent_effect,      holdable).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible,        holdable).

% Contradiction declarations (symmetric pairs)
% A ↔ C: mandatory-proportional-justice contradicts categorical impermissibility
narrative_ontology:cs_axiom_contradiction(
    execution_mandatory_for_proportional_justice,
    execution_categorically_impermissible).
narrative_ontology:cs_axiom_contradiction(
    execution_categorically_impermissible,
    execution_mandatory_for_proportional_justice).

% B ↔ C: deterrent-effect justification contradicts categorical impermissibility
narrative_ontology:cs_axiom_contradiction(
    execution_justified_by_deterrent_effect,
    execution_categorically_impermissible).
narrative_ontology:cs_axiom_contradiction(
    execution_categorically_impermissible,
    execution_justified_by_deterrent_effect).

/* ================================================================
   SYNTHETIC INCONSISTENCY FIXTURE
   ================================================================
   test_inconsistent_constraint holds BOTH execution_mandatory_...
   AND execution_categorically_impermissible — a Type B inconsistency.
   This does NOT correspond to any real constraint; it tests the
   cs_axiom_inconsistent/2 predicate on a manufactured bad actor.
   ================================================================ */

narrative_ontology:cs_axiom(test_inconsistent_constraint, foundational,
                             execution_mandatory_for_proportional_justice).
narrative_ontology:cs_axiom(test_inconsistent_constraint, secondary,
                             execution_categorically_impermissible).

/* ================================================================
   TEST CASES
   ================================================================ */

% cs_has_axioms/1 — field presence check
cs_axiom_test("has_axioms_abolition",
    cs_has_axioms(abolition_reading),
    success).

cs_axiom_test("has_axioms_retributive",
    cs_has_axioms(retributive_reading),
    success).

cs_axiom_test("has_axioms_deterrence",
    cs_has_axioms(deterrence_reading),
    success).

cs_axiom_test("has_axioms_absent_on_unknown",
    cs_has_axioms(nonexistent_constraint_xyz),
    fail).

% cs_axiom_inconsistent/2 — Type B: one constraint holds both sides
cs_axiom_test("inconsistent_fires_on_bad_actor",
    cs_axiom_inconsistent(test_inconsistent_constraint, _),
    success).

cs_axiom_test("inconsistent_pair_is_canonical",
    (cs_axiom_inconsistent(test_inconsistent_constraint, A-B), A @< B),
    success).

cs_axiom_test("inconsistent_silent_on_abolition",
    cs_axiom_inconsistent(abolition_reading, _),
    fail).

cs_axiom_test("inconsistent_silent_on_retributive",
    cs_axiom_inconsistent(retributive_reading, _),
    fail).

cs_axiom_test("inconsistent_silent_on_deterrence",
    cs_axiom_inconsistent(deterrence_reading, _),
    fail).

% cs_kernel_axiom_conflict/4 — cross-reading structural finding
% Retributive (A) ↔ Abolition (C): mandatory-proportional-justice vs. impermissible
cs_axiom_test("conflict_retributive_abolition_fires",
    cs_kernel_axiom_conflict(state_execution_authority,
                             abolition_reading, retributive_reading, _),
    success).

% Deterrence (B) ↔ Abolition (C): deterrent-effect vs. impermissible
cs_axiom_test("conflict_deterrence_abolition_fires",
    cs_kernel_axiom_conflict(state_execution_authority,
                             abolition_reading, deterrence_reading, _),
    success).

% Deterrence (B) ↔ Retributive (A): no contradiction declared between these two
cs_axiom_test("conflict_retributive_deterrence_silent",
    cs_kernel_axiom_conflict(state_execution_authority,
                             deterrence_reading, retributive_reading, _),
    fail).

% Non-existent kernel: no conflict
cs_axiom_test("conflict_silent_on_unknown_kernel",
    cs_kernel_axiom_conflict(nonexistent_kernel_xyz, _, _, _),
    fail).

% Pair is canonically ordered (C1 @< C2)
cs_axiom_test("conflict_pair_ordering",
    (cs_kernel_axiom_conflict(state_execution_authority, C1, C2, _), C1 @< C2),
    success).
