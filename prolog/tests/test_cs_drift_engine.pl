% ============================================================================
% TEST: CS DRIFT ENGINE
% ============================================================================
% Tests cs_drift_trajectory/3 (t2 computation) and cs_drift_unacknowledged/2
% (Type-A static signal).
%
% SELF-CONTAINED FIXTURES (2026-07-02): this suite used to load the capital
% punishment triplet from testsets/ (abolition/retributive/deterrence_reading);
% those files were removed by the 2026-06-05 corpus reset and the suite has
% been silently red since (7/8 failing on fixture-load existence errors — the
% one "pass" was the [fail]-mode test passing vacuously). The gap terms below
% are the SAME authored t1 inputs those files carried; expected terminals are
% computed from the theory table, not tuned:
%   tst_cde_abolition   → husk              (authority_erosion + substantial + false)
%   tst_cde_retributive → stable_pattern    (stable + minor + true)
%   tst_cde_deterrence  → axiom_foreclosure (axiom_overriding + substantial + false)
% If these terminals change, report as finding — do not adjust t1 inputs.
%
% Also locks the OQ-137 row-disjointness fix (2026-07-02): every
% (Direction, Magnitude, Acknowledged) combination matches EXACTLY ONE
% attractor row — (stable, minor, _) used to match two rows (duplicate) and
% (revival_pressure|repudiation_pressure, minor, _) matched two rows with
% DIFFERENT terminals (order-dependent under once/1).
%
% Run: cd prolog && swipl -g "[stack], [tests/test_cs_drift_engine], \
%   run_tests(cs_drift_engine), halt" -t "halt(1)"
% ============================================================================

:- use_module(cs_drift_engine).
:- use_module(cs_pattern_detection).
:- use_module(narrative_ontology).
:- use_module(library(plunit)).

:- multifile narrative_ontology:cs_drift_state/3.

cde_fixture(tst_cde_abolition,   gap(authority_erosion, substantial, false)).
cde_fixture(tst_cde_retributive, gap(stable, minor, true)).
cde_fixture(tst_cde_deterrence,  gap(axiom_overriding, substantial, false)).

setup_cde_fixtures :-
    forall(cde_fixture(UID, Gap),
           assertz(narrative_ontology:cs_drift_state(UID, t1, Gap))).

cleanup_cde_fixtures :-
    forall(cde_fixture(UID, _),
           retractall(narrative_ontology:cs_drift_state(UID, _, _))).

:- begin_tests(cs_drift_engine,
               [setup(setup_cde_fixtures), cleanup(cleanup_cde_fixtures)]).

% --- Trajectory tests ---

test(abolition_trajectory) :-
    once(cs_drift_engine:cs_drift_trajectory(tst_cde_abolition, _Gap, Terminal)),
    Terminal == husk.

test(retributive_trajectory) :-
    once(cs_drift_engine:cs_drift_trajectory(tst_cde_retributive, _Gap, Terminal)),
    Terminal == stable_pattern.

test(deterrence_trajectory) :-
    once(cs_drift_engine:cs_drift_trajectory(tst_cde_deterrence, _Gap, Terminal)),
    Terminal == axiom_foreclosure.

% Distinctness: all three fixtures produce different terminals
test(distribution_distinct) :-
    findall(T, ( cde_fixture(UID, _),
                 cs_drift_engine:cs_drift_trajectory(UID, _, T) ), Terminals),
    list_to_set(Terminals, Unique),
    length(Unique, 3).

% --- Row disjointness (OQ-137, 2026-07-02): exactly one attractor row per
% --- combination, across the full authored vocabulary grid.
test(attractor_table_row_disjoint) :-
    forall(( member(D, [stable, authority_erosion, codification_collapse,
                        axiom_overriding, practice_drift,
                        revival_pressure, repudiation_pressure]),
             member(M, [minor, substantial, severe]),
             member(A, [true, false]) ),
           ( findall(T, cs_drift_engine:cs_terminal_attractor(D, M, A, T), Ts),
             Ts = [_] )).

% ...and the fixture whose gap sat in the old overlap now yields exactly one
% solution through the trajectory surface (was 2 identical, witnessed on 8
% corpus UIDs pre-fix).
test(stable_minor_exactly_one_solution) :-
    findall(T, cs_drift_engine:cs_drift_trajectory(tst_cde_retributive, _, T), Ts),
    Ts == [stable_pattern].

% minor pressure gaps keep the pre-fix first-solution semantics (absorbed by
% the minor→stable row), now as the UNIQUE solution
test(minor_pressure_is_stable) :-
    findall(T, cs_drift_engine:cs_terminal_attractor(revival_pressure, minor, true, T), Ts),
    Ts == [stable_pattern].

% --- cs_drift_unacknowledged/2 tests ---

% Fires for the abolition fixture: authority_erosion + substantial + false
test(unacknowledged_abolition) :-
    cs_pattern_detection:cs_drift_unacknowledged(tst_cde_abolition, _).

% Does NOT fire for the retributive fixture: direction is stable (excluded)
test(no_unacknowledged_retributive, [fail]) :-
    cs_pattern_detection:cs_drift_unacknowledged(tst_cde_retributive, _).

% Fires for the deterrence fixture: axiom_overriding + substantial + false
test(unacknowledged_deterrence) :-
    cs_pattern_detection:cs_drift_unacknowledged(tst_cde_deterrence, _).

% Independence: cs_drift_trajectory/3 succeeds for retributive even though
% cs_drift_unacknowledged/2 does not fire (acknowledged stable drift still gets t2)
test(trajectory_independent_of_unacknowledged) :-
    \+ cs_pattern_detection:cs_drift_unacknowledged(tst_cde_retributive, _),
    once(cs_drift_engine:cs_drift_trajectory(tst_cde_retributive, _, stable_pattern)).

:- end_tests(cs_drift_engine).
