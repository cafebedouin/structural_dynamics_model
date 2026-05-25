% ============================================================================
% TEST: CS DRIFT ENGINE
% ============================================================================
% Tests cs_drift_trajectory/3 (t2 computation) and cs_drift_unacknowledged/2
% (Type-A static signal) on the capital punishment triplet.
%
% Observed expected t2 output (computed from authored t1 inputs, not tuned):
%   abolition_reading  → husk          (authority_erosion + substantial + false)
%   retributive_reading → stable_pattern (stable + minor + true)
%   deterrence_reading  → axiom_foreclosure (axiom_overriding + substantial + false)
%
% If these terminals change, report as finding — do not adjust t1 inputs.
% ============================================================================

:- use_module(cs_drift_engine).
:- use_module(cs_pattern_detection).
:- use_module(narrative_ontology).
:- use_module(library(plunit)).

:- [testsets/abolition_reading].
:- [testsets/retributive_reading].
:- [testsets/deterrence_reading].

:- begin_tests(cs_drift_engine).

% --- Trajectory tests ---

test(abolition_trajectory) :-
    once(cs_drift_engine:cs_drift_trajectory(abolition_reading, _Gap, Terminal)),
    Terminal == husk.

test(retributive_trajectory) :-
    once(cs_drift_engine:cs_drift_trajectory(retributive_reading, _Gap, Terminal)),
    Terminal == stable_pattern.

test(deterrence_trajectory) :-
    once(cs_drift_engine:cs_drift_trajectory(deterrence_reading, _Gap, Terminal)),
    Terminal == axiom_foreclosure.

% Distinctness: all three readings produce different terminals
test(distribution_distinct) :-
    findall(T, (
        member(C, [abolition_reading, retributive_reading, deterrence_reading]),
        cs_drift_engine:cs_drift_trajectory(C, _, T)
    ), Terminals),
    list_to_set(Terminals, Unique),
    length(Unique, 3).

% --- cs_drift_unacknowledged/2 tests ---

% Fires for abolition_reading: authority_erosion + substantial + false
test(unacknowledged_abolition) :-
    cs_pattern_detection:cs_drift_unacknowledged(abolition_reading, _).

% Does NOT fire for retributive_reading: direction is stable (excluded)
test(no_unacknowledged_retributive, [fail]) :-
    cs_pattern_detection:cs_drift_unacknowledged(retributive_reading, _).

% Fires for deterrence_reading: axiom_overriding + substantial + false
test(unacknowledged_deterrence) :-
    cs_pattern_detection:cs_drift_unacknowledged(deterrence_reading, _).

% Independence: cs_drift_trajectory/3 succeeds for retributive even though
% cs_drift_unacknowledged/2 does not fire (acknowledged stable drift still gets t2)
test(trajectory_independent_of_unacknowledged) :-
    \+ cs_pattern_detection:cs_drift_unacknowledged(retributive_reading, _),
    once(cs_drift_engine:cs_drift_trajectory(retributive_reading, _, stable_pattern)).

:- end_tests(cs_drift_engine).
