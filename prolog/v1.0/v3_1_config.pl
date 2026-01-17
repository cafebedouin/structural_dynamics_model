:- module(v3_1_config, [
    param/2,
    aggregation_weights/5,
    influence_weight/2,
    level/1
]).

/* ================================================================
   1. HIERARCHY DEFINITIONS
   ================================================================ */

%% level(?Level)
% Defines the four analytical levels of the system.
level(structural).
level(organizational).
level(class).
level(individual).

/* ================================================================
   2. COMPONENT WEIGHTS (Alpha)
   Maps components (A, S, U, R) to magnitude (Kappa) per level.
   Formula: $\kappa = (\alpha_A \cdot A) + (\alpha_S \cdot S) + (\alpha_U \cdot U) + (\alpha_R \cdot R)$.
   ================================================================ */

%% aggregation_weights(+Level, -AlphaA, -AlphaS, -AlphaU, -AlphaR)
% Weights are shifted based on level-specific significance.
aggregation_weights(structural,     0.30, 0.20, 0.20, 0.20).
aggregation_weights(organizational, 0.30, 0.40, 0.15, 0.15).
aggregation_weights(class,          0.30, 0.20, 0.30, 0.30).
aggregation_weights(individual,     0.25, 0.25, 0.25, 0.25).

/* ================================================================
   3. INFLUENCE WEIGHTS (w_i)
   Determines how much each level contributes to the System Gradient (Gsys).
   ================================================================ */

%% influence_weight(+Level, -Weight)
influence_weight(structural,     0.40). % Highest impact on system stability.
influence_weight(organizational, 0.30).
influence_weight(class,          0.20).
influence_weight(individual,     0.10).

/* ================================================================
   4. INTENT & DETECTION THRESHOLDS
   Consolidated parameters for the intent_engine and pattern_analysis.
   ================================================================ */

% --- Gradient Thresholds ---
% Minimum change to be considered non-stable.
param(system_gradient_threshold, 0.01).
% Threshold for "Strong" intent classification.
param(system_gradient_strong_threshold, 1.00).

% --- Beneficiary Asymmetry ---
% Min power gain required to be identified as the "Main Beneficiary".
param(beneficiary_gain_min, 0.50).
% Max gain allowed for other classes to maintain "Asymmetry".
param(loser_loss_max_gain, 0.10).

% --- Suppression & Resistance Alignment ---
% Required thresholds for structural-level alignment.
param(structural_suppression_min, 0.70). 
param(structural_resistance_min, 0.70).

% --- Data Integrity ---
% Thresholds for high/medium confidence classification.
param(data_high_threshold, 0.95).
param(data_medium_threshold, 0.75).
