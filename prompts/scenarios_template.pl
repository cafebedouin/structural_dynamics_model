% ============================================================================
% SCENARIO: [Scenario_ID]
% ============================================================================
:- module([scenario_id], []).
:- use_module('../engine/drl_engine').

% 1. Context Binding: Who is in the room?
% participant(AgentID, Index)
participant(investor_alpha, context(individual_powerless, biographical, trapped, national)).
participant(irs_admin, context(institutional, historical, arbitrage, national)).

% 2. Constraint Activation: What rules are active?
% active_constraint(ConstraintID, Interval)
active_constraint('26usc469', [0, 10]).

% 3. Interaction Logic: What happens when they clash?
% Measures Kappa (Aggregate Tension) across the interval.
evaluate_clash(Agent1, Agent2, Constraint) :-
    participant(Agent1, Index1),
    participant(Agent2, Index2),
    drl_engine:evaluate_tension(Constraint, Index1, R1),
    drl_engine:evaluate_tension(Constraint, Index2, R2),
    % Output identifies the Perspectival Gap
    format('Agent1 (~w) sees ~w | Agent2 (~w) sees ~w', [Agent1, R1, Agent2, R2]).