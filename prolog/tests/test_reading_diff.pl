% ============================================================================
% TESTS: reading_diff — the cyclopean disparity operator
% ============================================================================
% Run (from prolog/):
%   swipl -g "[stack], corpus_loader:load_all_testsets, \
%     [tests/test_reading_diff], run_tests, halt" -t "halt(1)"
%
% Freezes the witnessed exact-key numbers as the cross-implementation check:
%   self-diff (per reading)  6/0/0   (exact; and 0 fan-out)
%   absolute pair / exact    4/1/2   (disparity piton<->rope @ establishment cell)
%   graded pair  / exact     3/0/6
% plus the declared counting + verdict semantics:
%   fuzzy self-diff          0 disparity VANTAGES, but 2 disparity PAIRS (fan-out,
%                            documented non-zero — NOT a regression)
%   graded pair  / fuzzy     >=1 disparity vantage  -> regime flips -> key_fragile
%   absolute pair            >=1 disparity under every declared key -> robustly_binocular
% ============================================================================

:- use_module(reading_diff).

% Short names for the four test readings (all distinct, two sibling kernels).
abs_a(westphalia_sovereignty__absolute_non_intervention).      % westphalia_  kernel
abs_b(westphalian_sovereignty__absolute_sovereignty).          % westphalian_ kernel
grad_a(westphalia_sovereignty__graded_sovereignty).
grad_b(westphalian_sovereignty__graduated_sovereignty).

:- begin_tests(reading_diff).

% --- Positive control: self-diff under EXACT must see "no difference" --------
test(self_diff_exact_zero_disparity_zero_blind) :-
    abs_a(A),
    reading_diff:reading_diff(A, A, exact, Agree, Disp, Blind),
    Disp == [],
    Blind == [],
    length(Agree, NA),
    reading_diff:reading_cells(A, Cells), length(Cells, NCells),
    % every authored cell is its own vantage and agrees with itself
    NA =:= NCells.

test(self_diff_exact_zero_fanout) :-
    abs_a(A),
    reading_diff:aligned_pairs(exact, A, A, _Agree, DispPairs),
    DispPairs == [].

% --- Fuzzy self-diff: 0 disparity VANTAGES (a reading agrees with itself as a
%     set-valued map) but NON-ZERO disparity PAIRS in the fan-out detail.
%     Documented expectation — suppressing it would hide the operator's truth. -
test(self_diff_fuzzy_zero_vantage_disparity) :-
    abs_a(A),
    reading_diff:reading_diff(A, A, fuzzy_agent_power, _Agree, Disp, Blind),
    Disp == [],
    Blind == [].

test(self_diff_fuzzy_nonzero_fanout) :-
    abs_a(A),
    reading_diff:aligned_pairs(fuzzy_agent_power, A, A, _Agree, DispPairs),
    length(DispPairs, N),
    % absolute_non_intervention spans {rope,piton} at institutional across two
    % time-horizons -> 2 ordered disparity pairs at that power.
    N =:= 2.

% --- Absolute pair / exact : 4 agree / 1 disparity / 2 blind -----------------
test(absolute_exact_partition) :-
    abs_a(A), abs_b(B),
    reading_diff:reading_diff(A, B, exact, Agree, Disp, Blind),
    length(Agree, 4),
    length(Disp, 1),
    length(Blind, 2).

test(absolute_exact_establishment_disparity) :-
    abs_a(A), abs_b(B),
    reading_diff:reading_diff(A, B, exact, _Agree, Disp, _Blind),
    % the single disparity: institutional/civilizational/arbitrage/global,
    % A = piton (degraded theater) vs B = rope (live coordination).
    Disp = [disparity(Ctx, Ta, Tb)],
    Ctx == context(agent_power(institutional), time_horizon(civilizational),
                   exit_options(arbitrage), spatial_scope(global)),
    Ta == [piton],
    Tb == [rope].

% --- Graded pair / exact : 3 agree / 0 disparity / 6 blind (undersampled) ----
test(graded_exact_partition) :-
    grad_a(A), grad_b(B),
    reading_diff:reading_diff(A, B, exact, Agree, Disp, Blind),
    length(Agree, 3),
    length(Disp, 0),
    length(Blind, 6).

% --- Graded pair / fuzzy : regime flips to binocular ------------------------
test(graded_fuzzy_binocular) :-
    grad_a(A), grad_b(B),
    reading_diff:per_key_regime(A, B, fuzzy_agent_power, R),
    R == binocular.

% --- Stability verdicts (order-independent, over the declared chain) ---------
test(absolute_robustly_binocular) :-
    abs_a(A), abs_b(B),
    reading_diff:stability_verdict(A, B, V),
    V == robustly_binocular.

test(graded_key_fragile) :-
    grad_a(A), grad_b(B),
    reading_diff:stability_verdict(A, B, V),
    V == key_fragile.

:- end_tests(reading_diff).
