% ============================================================================
% MODULE: belief_constraint_indexing
% ============================================================================
% Handles indexical parameters for personal belief auditing.
% ============================================================================

:- module(belief_constraint_indexing, [
    compare_perspectives/2
]).

:- use_module(individual_belief_battery).

%% compare_perspectives(+Belief, +Context)
% Compares your individual classification against institutional claims.
compare_perspectives(Belief, MyContext) :-
    format('~n=== PERSPECTIVE COMPARISON: ~w ===~n', [Belief]),
    
    % Individual Perspective
    (individual_belief_battery:belief_classification(Belief, MyType) ->
        format('  - Your Classification (~w): ~w~n', [MyContext, MyType])
    ;   format('  - Your Classification: UNDEFINED~n')),
    
    % Institutional Perspective
    (individual_belief_battery:institutional_claim(Belief, InstType) ->
        format('  - Institutional Claim: ~w~n', [InstType]),
        (individual_belief_battery:type_1_error(Belief) ->
            format('  ! ALERT: Potential Type I Error (False Mountain) detected.~n'),
            format('    Extraction point identified in institutional framing.~n')
        ;   true)
    ;   format('  - Institutional Claim: None recorded.~n')).
