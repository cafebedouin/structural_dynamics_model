% ============================================================================
% MODULE: belief_audit
% ============================================================================
% Diagnostic interface for the Individual Practical Battery.
% ============================================================================

:- module(belief_audit, [
    audit/1,
    audit_all/0,
    find_nooses/0
]).

:- use_module(individual_belief_battery).
:- use_module(belief_constraint_indexing).

%% audit(+Belief)
% Audit a single belief point.
audit(Belief) :-
    individual_belief_battery:my_context(Ctx),
    belief_constraint_indexing:compare_perspectives(Belief, Ctx).

%% audit_all
% Scans the entire battery for structural integrity.
audit_all :-
    individual_belief_battery:my_context(Ctx),
    format('~n=== FULL BELIEF AUDIT: INDIVIDUAL PRACTICAL BATTERY ===~n'),
    format('Context: ~w~n~n', [Ctx]),
    
    format('--- TIER 0 (Universal Constraints) ---~n'),
    forall(individual_belief_battery:belief_classification(B, mountain),
           format('  [Mountain] ~w~n', [B])),
           
    format('~n--- TIER 1 (Pragmatic Ropes) ---~n'),
    forall(individual_belief_battery:belief_classification(B, rope),
           format('  [Rope]     ~w~n', [B])).

%% find_nooses
% Specifically identifies items where your "Rope" is sold as an institutional "Mountain."
find_nooses :-
    format('~n=== EXTRACTION POINT DETECTION ===~n'),
    forall(individual_belief_battery:type_1_error(B),
           ( format('  ⚠️  ~w~n', [B]),
             format('     Actually: Snare (Extractive)~n'),
             format('     Claimed:  Mountain (Unchangeable)~n~n'))).
