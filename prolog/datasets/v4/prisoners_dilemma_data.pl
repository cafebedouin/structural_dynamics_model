% =============================================================================
% CLINICAL SENSOR MEASUREMENT: The Prisoner's Dilemma
% Version: 3.2 (DR Modal Logic + Structural Signatures Edition)
% Status: VALIDATION SUCCESSFUL - NATURAL LAW DETECTED (Game Theory)
% =============================================================================

% -----------------------------------------------------------------------------
% Section 1: Entities & Intervals
% -----------------------------------------------------------------------------
entity(prisoners_dilemma, class).
entity(nash_equilibrium, structural).
entity(pareto_optimality, structural).
entity(iterated_pd, scaffold).

% Interval from formulation (1950) to modern evolutionary game theory (2026)
% T=0 (1950), T=34 (1984 - Axelrod), T=76 (2026)
interval(game_theory_era, 0, 76).

% -----------------------------------------------------------------------------
% Section 2: Constraint Claims & Metrics
% -----------------------------------------------------------------------------
% The Prisoner's Dilemma is a Hard Mountain (Mathematical Law of Incentives).
% It demonstrates why two completely rational individuals might not cooperate, 
% even if it appears that it is in their best interest to do so.
constraint_claim(prisoners_dilemma, mountain).

% Current state metrics (T=76)
% Zero extractiveness (it is a property of choice), zero suppression requirement,
% and zero resistance. The tension between Nash and Pareto is a logical fact.
constraint_metric(prisoners_dilemma, extractiveness, 0.00).
constraint_metric(prisoners_dilemma, suppression_requirement, 0.00).
constraint_metric(prisoners_dilemma, resistance, 0.00).

% -----------------------------------------------------------------------------
% Section 3: Temporal Measurements (CRITICAL FOR SIGNATURE DETECTION)
% -----------------------------------------------------------------------------

% T=0: 1950 - Merrill Flood and Melvin Dresher formulate the game at RAND
measurement(m1, prisoners_dilemma, extractiveness, 0, 0.00).
measurement(m2, prisoners_dilemma, suppression_requirement, 0, 0.00).
measurement(m3, prisoners_dilemma, resistance, 0, 0.00).

% T=34: 1984 - Robert Axelrod's "The Evolution of Cooperation"
measurement(m4, prisoners_dilemma, extractiveness, 34, 0.00).
measurement(m5, prisoners_dilemma, suppression_requirement, 34, 0.00).
measurement(m6, prisoners_dilemma, resistance, 34, 0.00).

% T=76: 2026 - Current verification in biology, economics, and AI safety
% Stability is absolute (Variance < 0.01)
measurement(m7, prisoners_dilemma, extractiveness, 76, 0.00).
measurement(m8, prisoners_dilemma, suppression_requirement, 76, 0.00).
measurement(m9, prisoners_dilemma, resistance, 76, 0.00).

% -----------------------------------------------------------------------------
% Section 4: Viable Alternatives (SIGNATURE: NATURAL LAW)
% -----------------------------------------------------------------------------
% CRITICAL: No viable alternatives exist to the incentive structure itself. 
% You cannot "negotiate" with the payoff matrix to remove the dominant 
% strategy (defection) without changing the game entirely.
% [NO intent_viable_alternative facts provided - Signature: Natural Law]

% -----------------------------------------------------------------------------
% Section 5: Dependencies
% -----------------------------------------------------------------------------
% Foundational to understanding nuclear deterrence, climate change accords, 
% and evolutionary biology (altruism).
affects_constraint(prisoners_dilemma, collective_action_problems).
affects_constraint(prisoners_dilemma, tragedy_of_the_commons).

% -----------------------------------------------------------------------------
% Section 6: Intent Evidence
% -----------------------------------------------------------------------------
% The mathematical trap of the PD is "impartial." It captures any agents with 
% specific incentive structures regardless of their identity.
% No intent_power_change facts (Delta = 0.00).

% -----------------------------------------------------------------------------
% Section 7: Recommendations
% -----------------------------------------------------------------------------
recommendation(rec1, 'Accept as Natural Law. The Prisoner\'s Dilemma is a structural mountain of incentives. To achieve cooperation, one must deploy Coordination Scaffolds like "Tit-for-Tat" in iterated versions or change the payoff matrix through external institutions.').
affects_constraint(rec1, prisoners_dilemma).

% -----------------------------------------------------------------------------
% Section 8: Signature Diagnosis (Mental Model)
% -----------------------------------------------------------------------------
% - Accessibility Collapse: 1.0 (The same incentive trap appears in all domains)
% - Suppression/Resistance: 0.0 (The payoff logic is self-enforcing)
% - Alternatives: NO (The incentive structure is a discovered mathematical property)
% - CONCLUSION: The Prisoner's Dilemma is a Natural Law.
