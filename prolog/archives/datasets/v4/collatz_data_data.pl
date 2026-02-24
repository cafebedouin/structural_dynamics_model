% =============================================================================
% CLINICAL SENSOR MEASUREMENT: The Collatz Conjecture (3n + 1)
% Version: 3.2 (DR Modal Logic + Structural Signatures Edition)
% Status: VALIDATION SUCCESSFUL - NATURAL LAW DETECTED (Empirical Mountain)
% =============================================================================

% -----------------------------------------------------------------------------
% Section 1: Entities & Intervals
% -----------------------------------------------------------------------------
entity(collatz_conjecture, class).
entity(dynamical_systems, structural).
entity(number_theory_limit, structural).

% Interval from Lothar Collatz's proposal (1937) to current verification (2026)
% T=0 (1937), T=40 (1977), T=89 (2026)
interval(collatz_era, 0, 89).

% -----------------------------------------------------------------------------
% Section 2: Constraint Claims & Metrics
% -----------------------------------------------------------------------------
% The Collatz Conjecture is a Mountain (Unproven but Empirically Absolute).
% It claims that for any positive integer n, the sequence eventually reaches 1.
constraint_claim(collatz_conjecture, mountain).

% Current state metrics (T=89)
% Zero extraction (mathematical property), zero suppression (it is a law of 
% arithmetic), and zero resistance (one cannot "opt-out" of the sequence).
constraint_metric(collatz_conjecture, extractiveness, 0.00).
constraint_metric(collatz_conjecture, suppression_requirement, 0.00).
constraint_metric(collatz_conjecture, resistance, 0.00).

% -----------------------------------------------------------------------------
% Section 3: Temporal Measurements (CRITICAL FOR SIGNATURE DETECTION)
% -----------------------------------------------------------------------------

% T=0: 1937 - Conjecture proposed by Lothar Collatz
measurement(m1, collatz_conjecture, extractiveness, 0, 0.00).
measurement(m2, collatz_conjecture, suppression_requirement, 0, 0.00).
measurement(m3, collatz_conjecture, resistance, 0, 0.00).

% T=40: 1977 - Extensive computer verification begins (up to ~10^12)
measurement(m4, collatz_conjecture, extractiveness, 40, 0.00).
measurement(m5, collatz_conjecture, suppression_requirement, 40, 0.00).
measurement(m6, collatz_conjecture, resistance, 40, 0.00).

% T=89: 2026 - Modern verification (verified up to ~2^68)
% Absolute temporal stability (Variance = 0.00)
measurement(m7, collatz_conjecture, extractiveness, 89, 0.00).
measurement(m8, collatz_conjecture, suppression_requirement, 89, 0.00).
measurement(m9, collatz_conjecture, resistance, 89, 0.00).

% -----------------------------------------------------------------------------
% Section 4: Viable Alternatives (SIGNATURE: NATURAL LAW)
% -----------------------------------------------------------------------------
% CRITICAL: No viable alternatives exist. The path of the sequence is an 
% immutable result of the arithmetic rules. It is a discovery of a numerical 
% phenomenon, not a social or engineering choice.
% [NO intent_viable_alternative facts provided - Signature: Natural Law]

% -----------------------------------------------------------------------------
% Section 5: Dependencies
% -----------------------------------------------------------------------------
% The (unproven) status of Collatz depends on the limits of current number theory.
affects_constraint(collatz_conjecture, number_theory_completeness).
affects_constraint(collatz_conjecture, dynamical_system_stability).

% -----------------------------------------------------------------------------
% Section 6: Intent Evidence
% -----------------------------------------------------------------------------
% Universal behavior. The sequence behaves the same for all integers regardless 
% of the context of the observer.
% No intent_power_change facts (Delta = 0.00).

% -----------------------------------------------------------------------------
% Section 7: Recommendations
% -----------------------------------------------------------------------------
recommendation(rec1, 'Accept as Natural Law. While unproven, its universal stability across all tested domains classifies it as a hard mountain. It serves as a limit-marker for our understanding of simple recursive functions.').
affects_constraint(rec1, collatz_conjecture).

% -----------------------------------------------------------------------------
% Section 8: Signature Diagnosis (Mental Model)
% -----------------------------------------------------------------------------
% - Accessibility Collapse: 1.0 (The sequence is identical for every calculator)
% - Suppression/Resistance: 0.0 (Arithmetic is self-enforcing)
% - Alternatives: NO (The rules 3n+1 and n/2 are fixed; the path is determined)
% - CONCLUSION: The Collatz Conjecture is a Natural Law.
