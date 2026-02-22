% ============================================================================
% POST-SYNTHESIS DIVERGENCE TRIGGER (T12)
% ============================================================================
% Detects SYNTHESIS_DIAGNOSTIC_DIVERGENCE — cases where the diagnostic
% verdict and the abductive trigger set disagree about whether a
% constraint is problematic.
%
% Two cases:
%   Case 1 (red_without_triggers):
%     Red verdict + zero genuine abductive triggers.
%     A red diagnosis with no abductive explanation is suspicious.
%
%   Case 2 (green_with_triggers):
%     Green verdict + N or more genuine abductive triggers.
%     A clean bill of health despite multiple anomaly detections.
%
% This is a canary trigger — zero output is the expected baseline for
% a well-calibrated system. The value is detecting future divergence.
% ============================================================================

:- module(post_synthesis, [
    post_synthesis_check/3,
    post_synthesis_check_core/4,
    post_synthesis_selftest/0
]).

:- use_module(config).

/* ================================================================
   PUBLIC API
   ================================================================ */

%% post_synthesis_check(+C, +Summary, -Flags)
%  Queries abd_triggers/2 for constraint C, then delegates to core logic.
%  Summary is the diagnostic_summary/7 term from diagnostic_summary.pl.
post_synthesis_check(C, Summary, Flags) :-
    (   json_report:abd_triggers(C, Triggers)
    ->  true
    ;   Triggers = []
    ),
    post_synthesis_check_core(C, Summary, Triggers, Flags).

/* ================================================================
   CORE LOGIC (pure — testable without dynamic state)
   ================================================================ */

%% post_synthesis_check_core(+C, +Summary, +Triggers, -Flags)
%  Pure logic: given the diagnostic summary and trigger list, produce flags.
post_synthesis_check_core(_C, Summary, Triggers, Flags) :-
    % Extract verdict from Summary term
    Summary = diagnostic_summary(Verdict, _, _, _, _, _, _),
    % Count genuine triggers
    include(is_genuine_trigger, Triggers, GenuineList),
    length(GenuineList, NGenuine),
    length(Triggers, NTotal),
    % Check both cases
    check_case1(Verdict, NGenuine, NTotal, MaybeFlag1),
    check_case2(Verdict, NGenuine, GenuineList, MaybeFlag2),
    % Collect non-empty flags
    append(MaybeFlag1, MaybeFlag2, Flags).

/* ================================================================
   CASE 1: Red verdict + zero genuine triggers
   ================================================================ */

%% check_case1(+Verdict, +NGenuine, +NTotal, -Flags)
check_case1(red, 0, NTotal, [Flag]) :-
    !,
    Flag = flag(red_without_triggers, [
        verdict-red,
        genuine_trigger_count-0,
        total_trigger_count-NTotal
    ]).
check_case1(_, _, _, []).

/* ================================================================
   CASE 2: Green verdict + >= N genuine triggers
   ================================================================ */

%% check_case2(+Verdict, +NGenuine, +GenuineList, -Flags)
check_case2(green, NGenuine, GenuineList, [Flag]) :-
    config:param(post_synthesis_green_trigger_threshold, Threshold),
    NGenuine >= Threshold,
    !,
    findall(Class, member(trigger(Class, _, _, genuine), GenuineList), Classes),
    Flag = flag(green_with_triggers, [
        verdict-green,
        genuine_trigger_count-NGenuine,
        trigger_classes-Classes,
        threshold-Threshold
    ]).
check_case2(_, _, _, []).

/* ================================================================
   TRIGGER CLASSIFICATION
   ================================================================ */

%% is_genuine_trigger(+Trigger)
%  True if the trigger is classified as genuine (not artifact).
is_genuine_trigger(trigger(_, _, _, genuine)).

/* ================================================================
   SELFTEST
   ================================================================ */

%% post_synthesis_selftest/0
%  5 synthetic-input tests covering all code paths.
post_synthesis_selftest :-
    format(user_error, '[T12] Running post_synthesis selftest...~n', []),

    % Build a minimal Summary for each verdict
    GreenSummary = diagnostic_summary(green, [], [], [], [], 12, []),
    YellowSummary = diagnostic_summary(yellow, [], [], [], [], 10, [purity, drift]),
    RedSummary = diagnostic_summary(red, [], [], [], [], 12, []),

    % Test 1: Red + 0 genuine triggers → fires Case 1
    T1Triggers = [trigger(some_class, 0.5, some_anomaly, artifact)],
    post_synthesis_check_core(test1, RedSummary, T1Triggers, T1Flags),
    (   T1Flags = [flag(red_without_triggers, _)]
    ->  format(user_error, '  [PASS] T12-1: Red + 0 genuine → red_without_triggers~n', [])
    ;   format(user_error, '  [FAIL] T12-1: Expected red_without_triggers, got ~w~n', [T1Flags])
    ),

    % Test 2: Green + >=2 genuine triggers → fires Case 2
    T2Triggers = [
        trigger(maxent_shadow_divergence, 0.8, shadow, genuine),
        trigger(snare_leaning_tangled, 0.7, snare, genuine),
        trigger(convergent_structural_stress, 0.6, stress, genuine)
    ],
    post_synthesis_check_core(test2, GreenSummary, T2Triggers, T2Flags),
    (   T2Flags = [flag(green_with_triggers, _)]
    ->  format(user_error, '  [PASS] T12-2: Green + 3 genuine → green_with_triggers~n', [])
    ;   format(user_error, '  [FAIL] T12-2: Expected green_with_triggers, got ~w~n', [T2Flags])
    ),

    % Test 3: Yellow verdict → no flag (T12 only fires on red/green)
    T3Triggers = [trigger(some_class, 0.5, some_anomaly, genuine)],
    post_synthesis_check_core(test3, YellowSummary, T3Triggers, T3Flags),
    (   T3Flags = []
    ->  format(user_error, '  [PASS] T12-3: Yellow + genuine → no flag~n', [])
    ;   format(user_error, '  [FAIL] T12-3: Expected [], got ~w~n', [T3Flags])
    ),

    % Test 4: Green + 1 genuine (below default threshold of 2) → no flag
    T4Triggers = [trigger(single_trigger, 0.9, lone, genuine)],
    post_synthesis_check_core(test4, GreenSummary, T4Triggers, T4Flags),
    (   T4Flags = []
    ->  format(user_error, '  [PASS] T12-4: Green + 1 genuine (below threshold) → no flag~n', [])
    ;   format(user_error, '  [FAIL] T12-4: Expected [], got ~w~n', [T4Flags])
    ),

    % Test 5: Red + genuine triggers present → no flag (Case 1 only fires on 0 genuine)
    T5Triggers = [trigger(real_trigger, 0.8, real_anomaly, genuine)],
    post_synthesis_check_core(test5, RedSummary, T5Triggers, T5Flags),
    (   T5Flags = []
    ->  format(user_error, '  [PASS] T12-5: Red + 1 genuine → no flag~n', [])
    ;   format(user_error, '  [FAIL] T12-5: Expected [], got ~w~n', [T5Flags])
    ),

    format(user_error, '[T12] Post-synthesis selftest complete.~n', []).
