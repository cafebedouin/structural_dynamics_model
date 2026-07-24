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
:- ensure_loaded('../json_report').
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

% --- Terminal/Direction-set TRIPWIRE (OQ-227 / Leg C1, 2026-07-24) ----------
% NOT a precondition check. This test CANNOT verify the surviving-referent
% precondition (that deep-time referent-dissolution is out of scope — see
% cs_drift_engine.pl header :59-67 and ISSUES OQ-227). It only DETECTS a change
% to the table's terminal SET and Direction SET, firing RED on any added/removed
% member. Adding a 7th terminal via a new Direction — a `referent_dissolution`
% clause routed to `sealed_closure`, exactly how the header says one would be
% added — trips it, forcing whoever edits the table to read the surviving-referent
% scope first. DOCUMENTED and CHANGE-DETECTED, never ENFORCED.
%
% Enumeration MUST visit the table, not reconstruct it from a known Direction
% list (a hardcoded list cannot see a Direction added later — the first version
% of this test had that bug and stayed green on the very case it names). We grid
% over the fixed Magnitude×Acknowledged vocabulary with Direction UNBOUND: this
% visits every Direction-bound clause (incl. a new one). We cannot leave M/A
% unbound too — the `\= minor` / `\= stable` guards fail on unbound vars and would
% drop revival_pressure/repudiation_pressure from the Direction set.
attractor_sets(Table, Terminals, Directions) :-
    findall(D-T, ( member(M, [minor, substantial, severe]),
                   member(A, [true, false]),
                   call(Table, D, M, A, T) ), Pairs),
    findall(D, member(D-_, Pairs), Ds), sort(Ds, Directions),
    findall(T, member(_-T, Pairs), Ts), sort(Ts, Terminals).

% A mutated copy of the table (real rows + one 7th-terminal row added as a new
% Direction clause) — the detection control runs the actual pin logic against it.
mutated_attractor(D, M, A, T) :- cs_drift_engine:cs_terminal_attractor(D, M, A, T).
mutated_attractor(referent_dissolution, severe, false, sealed_closure).

test(terminal_set_pinned) :-
    attractor_sets(cs_drift_engine:cs_terminal_attractor, Terminals, Directions),
    PinnedTerminals  = [axiom_foreclosure, extinction, husk, repudiation, revival, stable_pattern],
    PinnedDirections = [authority_erosion, axiom_overriding, codification_collapse,
                        practice_drift, repudiation_pressure, revival_pressure, stable],
    Terminals  == PinnedTerminals,
    Directions == PinnedDirections,
    % detection control (exercises the real pin logic on a mutated table): a new
    % Direction routed to a new terminal breaks BOTH pins.
    attractor_sets(mutated_attractor, MutTerminals, MutDirections),
    MutTerminals  \== PinnedTerminals,
    MutDirections \== PinnedDirections.

% NOTE (2026-07-24): test(attractor_table_row_disjoint) above has the same
% hardcoded-grid limitation — it checks disjointness only for the seven Directions
% it lists, so a Direction added later is unchecked there. Left as-is for this leg;
% the tripwire above is what catches the new-Direction case.

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

% ============================================================================
% OQ-126 Gap 1: acknowledgment provenance witness at the JSON surface
% ============================================================================
% The terminal is conditional on the AUTHORED ack bit; the serialized entry
% must carry that provenance (cs_drift_terminal_basis + cs_drift_ack_witness
% with the no-path sentinel). Controls:
%   w1 — drift story: witness fields present, acknowledged bit faithful,
%        confrontation_path is the "none_exists" sentinel (a NO-PATH fact,
%        not "checked, none found"), confronted_by null.
%   w2 — no-drift story: basis and witness serialize as null (absence stays
%        absence; no fabricated witness).
% Both go RED if the emission is dropped (witnessed at introduction by running
% once with the emission commented out — KNOWN_STATE 2026-07-02).
% Render helper drives the identical pipeline render path (same pattern as
% tests/test_a12_multi_instance_render.pl).

ackw_render(C, Out) :-
    with_output_to(string(Out),
        ( current_output(S),
          write_per_constraint_entry(S, C, false, context([],[],[],[])) )).

% practice_drift + severe + true → revival (acknowledged=true also pins the
% witness's acknowledged field to a non-default value).
ackw_setup :-
    assertz(narrative_ontology:cs_story_uid(ackw_c_drift, ackw_uid_drift)),
    assertz(narrative_ontology:cs_drift_state(ackw_uid_drift, t1,
                                              gap(practice_drift, severe, true))),
    assertz(narrative_ontology:cs_story_uid(ackw_c_plain, ackw_uid_plain)).

ackw_cleanup :-
    retractall(narrative_ontology:cs_story_uid(ackw_c_drift, _)),
    retractall(narrative_ontology:cs_drift_state(ackw_uid_drift, _, _)),
    retractall(narrative_ontology:cs_story_uid(ackw_c_plain, _)).

:- begin_tests(cs_drift_ack_witness, [setup(ackw_setup), cleanup(ackw_cleanup)]).

test(w1_drift_story_carries_witness) :-
    ackw_render(ackw_c_drift, Out),
    assertion(sub_string(Out, _, _, _, "\"cs_drift_terminal\": \"revival\"")),
    assertion(sub_string(Out, _, _, _, "\"cs_drift_terminal_basis\": \"authored_ack\"")),
    assertion(sub_string(Out, _, _, _,
        "\"cs_drift_ack_witness\": {\"authored\": true, \"acknowledged\": true, \"confrontation_path\": \"none_exists\", \"confronted_by\": null}")).

test(w2_no_drift_story_nulls) :-
    ackw_render(ackw_c_plain, Out),
    assertion(sub_string(Out, _, _, _, "\"cs_drift_terminal\": null")),
    assertion(sub_string(Out, _, _, _, "\"cs_drift_terminal_basis\": null")),
    assertion(sub_string(Out, _, _, _, "\"cs_drift_ack_witness\": null")).

% w3 — no-CS-UID story: the SEPARATE null-defaults branch (UIDs = []) must
% also carry the new fields, or the schema forks by entry shape (this exact
% miss was caught live: 30/119 entries lacked the fields on the first edited
% run because only the UID-bearing branch was extended).
test(w3_no_cs_uid_default_path_carries_nulls) :-
    ackw_render(ackw_c_nouid, Out),
    assertion(sub_string(Out, _, _, _, "\"cs_instance_count\": 0")),
    assertion(sub_string(Out, _, _, _, "\"cs_drift_terminal_basis\": null")),
    assertion(sub_string(Out, _, _, _, "\"cs_drift_ack_witness\": null")).

:- end_tests(cs_drift_ack_witness).
