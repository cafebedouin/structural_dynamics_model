% =============================================================================
% CLINICAL SENSOR MEASUREMENT: The Stable Marriage Problem (Gale-Shapley)
% Version: 3.2 (DR Modal Logic + Structural Signatures Edition)
% Status: VALIDATION SUCCESSFUL
% =============================================================================

% -----------------------------------------------------------------------------
% Section 1: Entities & Intervals
% -----------------------------------------------------------------------------
entity(gale_shapley_alg, class).
entity(matching_participants, class).
entity(decentralized_matching, scaffold).
entity(stable_roommates_extension, scaffold).

% Interval covering discovery to modern widespread adoption
% T=0 (1962), T=30 (1992), T=64 (2026)
interval(gale_shapley_era, 0, 64).

% -----------------------------------------------------------------------------
% Section 2: Constraint Claims & Metrics
% -----------------------------------------------------------------------------
% The algorithm is claimed as a Mountain (mathematical necessity of stability)
constraint_claim(gale_shapley_alg, mountain).

% Current state metrics (T=64)
% Characterized by high efficiency, low enforcement, and low friction.
constraint_metric(gale_shapley_alg, extractiveness, 0.05).
constraint_metric(gale_shapley_alg, suppression_requirement, 0.02).
constraint_metric(gale_shapley_alg, resistance, 0.05).

% -----------------------------------------------------------------------------
% Section 3: Temporal Measurements (CRITICAL FOR SIGNATURE DETECTION)
% -----------------------------------------------------------------------------

% T=0: Discovery/Initial implementation (1962)
measurement(m1, gale_shapley_alg, extractiveness, 0, 0.05).
measurement(m2, gale_shapley_alg, suppression_requirement, 0, 0.02).
measurement(m3, gale_shapley_alg, resistance, 0, 0.10).

% T=30: Expansion into NRMP (Residency Match) and Kidney Exchange
measurement(m4, gale_shapley_alg, extractiveness, 30, 0.08).
measurement(m5, gale_shapley_alg, suppression_requirement, 30, 0.05).
measurement(m6, gale_shapley_alg, resistance, 30, 0.08).

% T=64: Current ubiquity in market design
measurement(m7, gale_shapley_alg, extractiveness, 64, 0.05).
measurement(m8, gale_shapley_alg, suppression_requirement, 64, 0.02).
measurement(m9, gale_shapley_alg, resistance, 64, 0.05).

% -----------------------------------------------------------------------------
% Section 4: Viable Alternatives (SIGNATURE: COORDINATION SCAFFOLD)
% -----------------------------------------------------------------------------
% While the mathematical "stability" is a property, the SPECIFIC algorithm choice 
% and the bipartite restriction had alternatives.

% Alternative 1: Decentralized Search (higher friction, unstable)
intent_viable_alternative(gale_shapley_era, decentralized_matching, 
    'Uncoordinated bilateral search and offer exchange').
intent_alternative_rejected(gale_shapley_era, decentralized_matching, 
    'Uncoordinated bilateral search and offer exchange').

% Alternative 2: Stable Roommates (Non-bipartite matching)
intent_viable_alternative(gale_shapley_era, stable_roommates_extension, 
    'Irving Stable Roommates algorithm for non-bipartite sets').
intent_alternative_rejected(gale_shapley_era, stable_roommates_extension, 
    'Irving Stable Roommates algorithm for non-bipartite sets').

% -----------------------------------------------------------------------------
% Section 5: Dependencies
% -----------------------------------------------------------------------------
% Modern medical residency markets depend on this constraint for stability.
affects_constraint(gale_shapley_alg, medical_residency_stability).
affects_constraint(gale_shapley_alg, organ_exchange_efficiency).

% -----------------------------------------------------------------------------
% Section 6: Intent Evidence
% -----------------------------------------------------------------------------
% Symmetrical benefits for participants (efficient matching)
intent_beneficiary_class(gale_shapley_era, matching_participants).
intent_power_change(gale_shapley_era, matching_participants, 0.10).

% -----------------------------------------------------------------------------
% Section 7: Recommendations
% -----------------------------------------------------------------------------
recommendation(rec1, 'Maintain as Coordination Scaffold; the low resistance and lack of enforcement confirm voluntary adoption of a superior standard.').
affects_constraint(rec1, gale_shapley_alg).

% -----------------------------------------------------------------------------
% Section 8: Signature Diagnosis (Mental Model)
% -----------------------------------------------------------------------------
% - Accessibility Collapse: High (The "Stable" solution is a mathematical limit)
% - Suppression/Resistance: Low (≤ 0.15)
% - Alternatives: YES (Coordination Scaffold)
% - CONCLUSION: Gale-Shapley is a Coordination Scaffold, not a Natural Law, 
%   because the choice of "deferred acceptance" was a specific design decision 
%   among other possible matching mechanisms.
