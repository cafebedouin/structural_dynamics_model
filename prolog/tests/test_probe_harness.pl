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
%
% Since OQ-326 (2026-08-21) it also carries the INSTALL-WITNESS suite: a
% {fires, declines} pair for each of the SIX checks (2, 3, 1, 4, 4', 5), the
% escape-scoping and escape-composition properties, the binding-leak
% regression, and the check-4 guard property (narrow the snapshot artificially
% and check 4 MUST fire — that test failing is the named signal that snapshot
% completeness or check ordering has regressed).
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
        [reach_undeclared(authored("suite fixture: bare assert, no query shape declared"),
                          ph_scratch:pfact(z))],
        ph_scratch:pfact(z)).

% --- restore on success ------------------------------------------------------
test(restore_after_success, [setup(setup_scratch)]) :-
    probe_harness:with_retracted([ph_scratch:pfact(_)], true),
    findall(X, ph_scratch:pfact(X), Xs),
    msort(Xs, [a, b, c]).

test(asserted_removed_after_success, [setup(setup_scratch)]) :-
    probe_harness:with_asserted(
        [reach_undeclared(authored("suite fixture: bare assert, no query shape declared"),
                          ph_scratch:pfact(z))], true),
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

% ===========================================================================
% INSTALL WITNESS (OQ-326) — a {fires, declines} pair per check.
% A control that only fires licenses nothing, so every check gets both sides.
% ===========================================================================

:- dynamic ph_scratch:qfact/2.
:- dynamic ph_scratch:unpopulated/1.
:- dynamic ph_scratch:ruled/1.
ph_scratch:ruled(X) :- X = derived.        % a RULE clause, for check 3

% STATIC and fact-bearing, with NO rule clauses: reaches check 5 without
% consuming any escape. Declared here rather than borrowed from the engine so
% the fixture cannot drift with the corpus.
ph_static:sfact(1).
ph_static:sfact(2).

setup_q :-
    retractall(ph_scratch:qfact(_,_)),
    assertz(ph_scratch:qfact(a,1)),
    assertz(ph_scratch:qfact(a,9)),
    assertz(ph_scratch:qfact(b,5)).

% --- check 2: resolvable ---------------------------------------------------
test(c2_fires_undefined,
     [throws(error(probe_overlay_unresolvable(_, undefined), _))]) :-
    probe_harness:with_retracted([ph_scratch:no_such_pred_xyzzy(_)], true).

test(c2_fires_arity_mismatch,
     [setup(setup_q), throws(error(probe_overlay_unresolvable(_, arity_mismatch(2)), _))]) :-
    probe_harness:with_retracted([ph_scratch:qfact(_,_,_)], true).

test(c2_declines, [setup(setup_q)]) :-
    probe_harness:with_retracted([ph_scratch:qfact(a,_)], true).

% --- check 3: rule-bearing template ----------------------------------------
test(c3_fires, [throws(error(probe_overlay_partial(_, _), _))]) :-
    probe_harness:with_retracted([ph_scratch:ruled(_)], true).

test(c3_declines, [setup(setup_q)]) :-
    probe_harness:with_retracted([ph_scratch:qfact(b,_)], true).

% --- check 1: empty snapshot ------------------------------------------------
test(c1_fires, [throws(error(probe_overlay_empty(_), _))]) :-
    probe_harness:with_retracted([ph_scratch:unpopulated(_)], true).

test(c1_declines, [setup(setup_q)]) :-
    probe_harness:with_retracted([ph_scratch:qfact(a,1)], true).

% an EMPTY template list stays legal by construction (oq35's null control)
test(c1_empty_template_list_legal) :-
    probe_harness:with_retracted([], true).

% --- check 4: shadowed at TEMPLATE shape ------------------------------------
% Currently UNREACHABLE given checks 1 and 3 in the ruled order plus snapshot
% completeness (WRITEUP §8). The guard property below is what pins that, and is
% the named signal if either premise regresses.
test(c4_guard_property_fires_on_narrowed_snapshot, [setup(setup_q)]) :-
    % hand shadow_survivors/3 a DELIBERATELY NARROWED snapshot, as the binding
    % leak used to produce: template qfact(a,_) but only qfact(a,1) collected.
    probe_harness:shadow_survivors(ph_scratch:qfact(a,_),
                                   [ph_scratch:qfact(a,1)], Surv),
    Surv \== [],
    memberchk(ph_scratch:qfact(a,9), Surv).

test(c4_declines_on_complete_snapshot, [setup(setup_q)]) :-
    probe_harness:snapshot_one(ph_scratch:qfact(a,_), t(_, Snap)),
    probe_harness:shadow_survivors(ph_scratch:qfact(a,_), Snap, Surv),
    Surv == [].

% --- check 4': reachability decidable at all --------------------------------
test(c4p_fires_bare_assert,
     [setup(setup_q), throws(error(probe_overlay_reach_undecidable(_), _))]) :-
    probe_harness:with_asserted([ph_scratch:qfact(a,2)], true).

test(c4p_declines_when_template_covered, [setup(setup_q)]) :-
    % the fact IS covered, so it reaches check 4 proper and 4' stays silent.
    % WITHOUT this test an implementation that fires 4' on EVERY fact passes
    % the whole suite, since every other 4' case is a fires-side case.
    probe_harness:with_overlay([ph_scratch:qfact(a,_)],
                               [ph_scratch:qfact(a,2)],
                               ph_scratch:qfact(a,2)).

test(c4p_per_fact_mixed_overlay,
     [setup(setup_q),
      throws(error(probe_overlay_reach_undecidable(ph_scratch:qother(1)), _))]) :-
    % one covered fact must NOT license an uncovered sibling
    probe_harness:with_overlay([ph_scratch:qfact(a,_)],
                               [ph_scratch:qfact(a,2), ph_scratch:qother(1)],
                               true).

test(c4p_escape_admits_uncovered_fact, [setup(setup_q)]) :-
    probe_harness:with_asserted(
        [reach_undeclared(retrofit('2026-08-21', "test"), ph_scratch:qfact(a,2))],
        ph_scratch:qfact(a,2)).

test(c4p_escape_on_covered_fact_is_type_error,
     [setup(setup_q),
      throws(error(type_error(probe_overlay_reach_undeclared_on_covered_fact, _), _))]) :-
    % the one test that stops reach_undeclared becoming a GLOBAL escape
    probe_harness:with_overlay(
        [ph_scratch:qfact(a,_)],
        [reach_undeclared(retrofit('2026-08-21', "test"), ph_scratch:qfact(a,2))],
        true).

% --- check 5: mutable target ------------------------------------------------
% ph_static:sfact/1 is STATIC, fact-bearing, NOT rule-bearing, so it reaches
% check 5 without consuming any escape. (boltzmann is the wrong fixture here: it
% is static AND all-rules, so check 1 pre-empts check 5 — that composition is
% pinned separately below.)
test(c5_fires_static_template,
     [throws(error(probe_overlay_immutable(_), _))]) :-
    probe_harness:with_retracted([ph_static:sfact(_)], true).

test(c5_declines_dynamic_target, [setup(setup_q)]) :-
    probe_harness:with_retracted([ph_scratch:qfact(a,1)], true).

% assert-side (F7): a STATIC assert target throws probe_overlay_immutable, NOT
% an unattributable permission_error from inside apply_overlay/2.
test(c5_fires_static_assert_target,
     [throws(error(probe_overlay_immutable(_), _))]) :-
    probe_harness:with_asserted(
        [reach_undeclared(authored("test"),
                          boltzmann_compliance:boltzmann_invariant_mountain(x,y))],
        true).

% ...and the declining side: an UNDEFINED assert target is LEGAL (assertz
% creates it dynamic). Without this, check 5 re-creates on the assert side the
% defect that keeping check 2 off it was meant to avoid.
test(c5_declines_undefined_assert_target) :-
    probe_harness:with_asserted(
        [reach_undeclared(authored("fixture planting into a fresh predicate"),
                          ph_scratch:brand_new_pred_abc(1))],
        ph_scratch:brand_new_pred_abc(1)).

% --- escape scoping and composition -----------------------------------------
test(escape_suppresses_only_its_own_clause,
     [throws(error(probe_overlay_empty(_), _))]) :-
    % allow_partial clears check 3; check 1 then fires on the SAME template,
    % because all its clauses are rules. Two declarations for two distinct facts.
    probe_harness:with_retracted(
        [allow_partial(authored("rules only"), ph_scratch:ruled(_))], true).

test(escape_composition_cannot_clear_check5,
     [throws(error(probe_overlay_immutable(_), _))]) :-
    % allow_partial + expect_empty on a STATIC target: both escapes consumed,
    % check 5 still rejects. No combination of wrappers can clear it.
    probe_harness:with_retracted(
        [allow_partial(authored("x"),
           expect_empty(authored("y"),
             boltzmann_compliance:boltzmann_invariant_mountain(_,_)))],
        true).

test(malformed_reason_is_type_error,
     [throws(error(type_error(probe_overlay_reason, _), _))]) :-
    probe_harness:with_retracted(
        [expect_empty("bare string is not a reason", ph_scratch:unpopulated(_))], true).

test(escape_on_wrong_side_is_type_error,
     [setup(setup_q),
      throws(error(type_error(probe_overlay_escape_side(_), _), _))]) :-
    probe_harness:with_overlay([ph_scratch:qfact(a,_)],
        [expect_empty(authored("x"), ph_scratch:qfact(a,2))], true).

% --- binding-leak regression -------------------------------------------------
test(rule_clauses_does_not_bind_caller_template) :-
    T = ph_scratch:ruled(_),
    probe_harness:rule_clauses(T, Heads),
    Heads \== [],                       % it DID find the rule
    T = _:Term,
    term_variables(Term, Vs),
    Vs \== [].                          % ...and left the caller's term UNBOUND

% --- state is restored after a preflight throw -------------------------------
% Preflight runs BEFORE setup_call_cleanup/3, so a refusal must not have
% touched the database at all. Fails if any check ever moves past the mutation
% point.
test(preflight_throw_leaves_state_untouched, [setup(setup_q)]) :-
    findall(X-Y, ph_scratch:qfact(X,Y), Before),
    catch(probe_harness:with_overlay(
              [ph_scratch:qfact(a,_)],
              [ph_scratch:qfact(a,2), ph_scratch:qother(1)], true),
          _, true),
    findall(X-Y, ph_scratch:qfact(X,Y), After),
    msort(Before, B), msort(After, A),
    B == A.

% --- the install witness itself ----------------------------------------------
test(report_carries_install_counts, [setup(setup_q)]) :-
    probe_harness:with_overlay([ph_scratch:qfact(a,_)],
                               [ph_scratch:qfact(a,2)],
                               Report, true),
    Report = overlay_report(RetractedN, AssertedN, PerTemplate, Reach),
    RetractedN =:= 2, AssertedN =:= 1,
    PerTemplate = [t(ph_scratch:qfact(a,_), 2)],
    Reach = [reach(ph_scratch:qfact(a,2), checked(_))].

test(report_distinguishes_declared_gap_from_checked, [setup(setup_q)]) :-
    probe_harness:with_overlay([],
        [reach_undeclared(authored("no query shape"), ph_scratch:qfact(a,2))],
        Report, true),
    Report = overlay_report(0, 1, [], [reach(_, declared_gap)]).

:- end_tests(probe_harness).
