% test_battery_variants.pl
% ============================================================================
% Test harness variants for the alt-power-transform battery (Tests 1-3).
% Loaded by the Python test battery AFTER stack, so constraint_indexing
% module is already loaded and alt_sigmoid_f/3 is multifile-declared.
%
% All variants target range ≈ [-0.12, 1.42] or [-0.20, 1.50] and document
% which axes (sign-flip / range / smoothness) they change vs the baseline.
% ============================================================================

:- use_module(constraint_indexing, []).

% --- Test 1: Clean sign-flip isolation ---
% Both variants: smooth sigmoid, zero-crossing shifted to d≈0.10.
% Matched on: smoothness (both smooth sigmoid), range width (both 1.70 span).
% Differ on: sign-flip only.
%
% d0=0.436 chosen so that f(0.10) ≈ 0:
%   f(0.10) = -0.20 + 1.70/(1+exp(-6*(0.10-0.436)))
%           = -0.20 + 1.70/(1+exp(2.016))
%           = -0.20 + 1.70/8.503 ≈ 0.000  ✓

% (a) Smooth sigmoid, sign-flip preserved: range ≈ [-0.20, 1.50], f(0.10)≈0
constraint_indexing:alt_sigmoid_f(t1_smooth_flip, D, F) :-
    L is -0.20, U is 1.50, D0 is 0.436, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% (b) Same shape, shifted +0.20 so f ≥ 0 everywhere: range ≈ [0.00, 1.70]
% Identical steepness, identical d0; ONLY change is L=0, U=1.70 (shifted up).
constraint_indexing:alt_sigmoid_f(t1_smooth_noflip, D, F) :-
    L is 0.00, U is 1.70, D0 is 0.436, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% --- Test 2: Range collapse ---
% Hold baseline d0=0.50, k=6 (same as sigmoid). Vary output range only.
% Baseline (sigmoid, L=-0.20/U=1.50) is already done.

% (b) Compressed, no sign-flip: range [0.40, 0.90] — positive throughout
constraint_indexing:alt_sigmoid_f(t2_compressed_noflip, D, F) :-
    L is 0.40, U is 0.90, D0 is 0.50, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% (c) Compressed, sign-flip preserved: range [-0.05, 0.15]
% Zero-crossing: solve 0 = -0.05 + 0.20/(1+exp(-6*(d-0.50)))
%   → d ≈ 0.317
constraint_indexing:alt_sigmoid_f(t2_compressed_flip, D, F) :-
    L is -0.05, U is 0.15, D0 is 0.50, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% --- Test 3: Smoothness sweep ---
% Hold sign-flip at d=0.10 and range [-0.12, 1.42] fixed.
% Vary discretization level.
% (a) smooth sigmoid = t1_smooth_flip (already defined above)
% (b) piecewise linear (1 kink at d=0.10) = piecewise_linear (existing)

% (c) 5-level staircase: zero at d=0.10, same min/max as piecewise_linear
%   Levels: -0.12, 0.355, 0.710, 1.065, 1.420
%   Boundaries: [0,0.10), [0.10,0.325), [0.325,0.55), [0.55,0.775), [0.775,1.0]
constraint_indexing:alt_sigmoid_f(t3_step5, D, F) :-
    (   D < 0.10
    ->  F is -0.12
    ;   D < 0.325
    ->  F is 0.355
    ;   D < 0.55
    ->  F is 0.710
    ;   D < 0.775
    ->  F is 1.065
    ;   F is 1.420
    ).

% (d) 2-level staircase: binary flip at d=0.10 (maximum discretization)
%   f < 0 below 0.10, f = max above 0.10 — same sign-flip, no gradations.
constraint_indexing:alt_sigmoid_f(t3_step2, D, F) :-
    (   D < 0.10
    ->  F is -0.12
    ;   F is 1.420
    ).

% =============================================================================
% Range sweep variants (T2-redo): isolate range vs sign-flip with Hub 1 live.
% All use d0=0.50, k=6.0 (same shape as baseline sigmoid).
% Arm A: sign-flip preserved (L < 0); Arm B: no sign-flip (L=0.02 > 0).
% Spans verified against chi thresholds before running.
%   rope_chi_ceiling=0.35, TR_floor=0.40, snare_floor=0.66
% =============================================================================

% A2: sign-flip, span=1.20. L=-0.20, U=1.00.
%   f_max=0.943 → chi_max=0.660 (eps=0.70,sig=1.0) ≈ snare floor. Spans rope->TR->snare.
constraint_indexing:alt_sigmoid_f(range_a2, D, F) :-
    L is -0.20, U is 1.00, D0 is 0.50, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% A3: sign-flip, span=0.85. L=-0.20, U=0.65.
%   f_max=0.610 → chi_max=0.427 (eps=0.70,sig=1.0). Spans rope->TR (not snare).
constraint_indexing:alt_sigmoid_f(range_a3, D, F) :-
    L is -0.20, U is 0.65, D0 is 0.50, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% B1: no sign-flip, span=1.70. L=0.02, U=1.72. Matched span to A1.
%   f_max=1.639 → chi_max=1.147 (eps=0.70). Spans rope->TR->snare.
constraint_indexing:alt_sigmoid_f(range_b1, D, F) :-
    L is 0.02, U is 1.72, D0 is 0.50, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% B2: no sign-flip, span=1.20. L=0.02, U=1.22. Matched span to A2.
%   f_max=1.163 → chi_max=0.814 (eps=0.70). Spans rope->TR->snare.
constraint_indexing:alt_sigmoid_f(range_b2, D, F) :-
    L is 0.02, U is 1.22, D0 is 0.50, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% B3: no sign-flip, span=0.85. L=0.02, U=0.87. Matched span to A3.
%   f_max=0.830 → chi_max=0.581 (eps=0.70,sig=1.0); 0.697 at global. Spans rope->TR->snare(glo).
constraint_indexing:alt_sigmoid_f(range_b3, D, F) :-
    L is 0.02, U is 0.87, D0 is 0.50, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).
