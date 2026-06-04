% ============================================================================
% TESTS — probe_harness.pl (with_overlay/with_retracted/with_asserted)
% ============================================================================
% Run: cd prolog && swipl -g "[stack], [tests/test_probe_harness], run_tests, halt" -t "halt(1)"
%
% Exercises the gotchas the harness encapsulates (swipl_load_path_and_probe_
% gotchas.md): mid-goal dispatch visibility (§3's "mid" control), restore on
% success / failure / exception (§2), restore verification (§4), and cache
% clearing (§7). Uses a scratch fact table (ph_scratch:pfact/1) plus one real
% registered cache (boltzmann_compliance:cached_coupling/2) for the §7 check.
% ============================================================================

:- use_module(probe_harness).
:- use_module(cache_registry).

:- dynamic ph_scratch:pfact/1.

:- begin_tests(probe_harness).

setup_scratch :-
    retractall(ph_scratch:pfact(_)),
    assertz(ph_scratch:pfact(a)),
    assertz(ph_scratch:pfact(b)),
    assertz(ph_scratch:pfact(c)).

% --- §3 mid control: the overlay is visible INSIDE the goal -----------------
test(retract_visible_mid_goal, [setup(setup_scratch)]) :-
    probe_harness:with_retracted(
        [ph_scratch:pfact(a)],
        ( \+ ph_scratch:pfact(a),     % retracted fact gone mid-goal
          ph_scratch:pfact(b)         % untouched fact still present
        )).

test(assert_visible_mid_goal, [setup(setup_scratch)]) :-
    probe_harness:with_asserted(
        [ph_scratch:pfact(z)],
        ph_scratch:pfact(z)).

% --- restore on success ------------------------------------------------------
test(restore_after_success, [setup(setup_scratch)]) :-
    probe_harness:with_retracted([ph_scratch:pfact(_)], true),
    findall(X, ph_scratch:pfact(X), Xs),
    msort(Xs, [a, b, c]).

test(asserted_removed_after_success, [setup(setup_scratch)]) :-
    probe_harness:with_asserted([ph_scratch:pfact(z)], true),
    \+ ph_scratch:pfact(z).

% --- restore on goal FAILURE (the with_overlay call fails, state restored) --
test(restore_after_failure, [setup(setup_scratch), fail]) :-
    probe_harness:with_retracted([ph_scratch:pfact(a)], fail).

test(restore_after_failure_state, [setup(setup_scratch)]) :-
    ( probe_harness:with_retracted([ph_scratch:pfact(a)], fail) -> true ; true ),
    ph_scratch:pfact(a).

% --- restore on EXCEPTION (rethrown, state restored) -------------------------
test(restore_after_exception, [setup(setup_scratch),
                               throws(probe_test_boom)]) :-
    probe_harness:with_retracted([ph_scratch:pfact(a)], throw(probe_test_boom)).

test(restore_after_exception_state, [setup(setup_scratch)]) :-
    catch(probe_harness:with_retracted([ph_scratch:pfact(a)],
                                       throw(probe_test_boom)),
          probe_test_boom, true),
    ph_scratch:pfact(a).

% --- §7: a registered memo cache is cleared by the overlay ------------------
test(cache_cleared_by_overlay, [setup(setup_scratch)]) :-
    assertz(boltzmann_compliance:cached_coupling(ph_test_c, 0.99)),
    probe_harness:with_retracted(
        [ph_scratch:pfact(a)],
        \+ boltzmann_compliance:cached_coupling(ph_test_c, _)).

% --- unqualified input fails loud --------------------------------------------
test(unqualified_template_throws,
     [throws(error(type_error(module_qualified_fact, _), _))]) :-
    probe_harness:with_retracted([pfact(a)], true).

:- end_tests(probe_harness).
