:- module(constraint_bridge, [
    dr_diagnostic_report/1,
    constraint_status/3,
    recommendation_feasibility/3
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_coercion_projection).

/* ================================================================
   1. CONTEXTUAL CONSTRAINT FILTERING
   ================================================================ */

%% constraint_status(+ConstraintName, -State, -Intensity)
%  Maps CE v2.0 types to diagnostic intensities.
constraint_status(Name, binding_limit, I) :-
    narrative_ontology:constraint_claim(Name, mountain),
    narrative_ontology:constraint_metric(Name, inevitability, I), !.

constraint_status(Name, extractive_noose, I) :-
    narrative_ontology:constraint_claim(Name, noose),
    narrative_ontology:constraint_metric(Name, extractiveness, I), !.

constraint_status(Name, coordination_rope, I) :-
    narrative_ontology:constraint_claim(Name, rope),
    (narrative_ontology:constraint_metric(Name, theater_ratio, T) -> I is 1.0 - T ; I = 1.0), !.

constraint_status(Name, inertial_zombie, I) :-
    narrative_ontology:constraint_claim(Name, zombie),
    (narrative_ontology:constraint_metric(Name, theater_ratio, I) ; I = 1.0), !.

/* ================================================================
   2. UNIQUE VETO LOGIC & FEASIBILITY
   ================================================================ */

recommendation_feasibility(RecID, Status, UniqueVetoes) :-
    narrative_ontology:recommendation(RecID, _), % CRITICAL: Verify RecID is a recommendation
    % --- 1. THE STRUCTURAL AUDIT (Theorem 3) ---
    % If a recommendation affects a Noose (high extraction), 
    % it is load-bearing. Cutting it without a Scaffold is a systemic risk.
    (   narrative_ontology:affects_constraint(RecID, Target),
        narrative_ontology:constraint_metric(Target, extractiveness, X), 
        X > 0.7, % Load-bearing threshold
        \+ (narrative_ontology:entity(_, scaffold)) % No safety net provided
    ->  Status = blocked_scaffold_vacuum,
        UniqueVetoes = [] % Blocked by physics/logic, not individuals
    ;   
    % --- 2. THE POLITICAL AUDIT (Veto Logic) ---
    % Check for agents whose interests are explicitly threatened.
    (setof(Actor, narrative_ontology:veto_exposed(Actor, RecID), UniqueVetoes) 
     ; UniqueVetoes = []),
    length(UniqueVetoes, Count),
    (   Count == 0 -> Status = viable 
    ;   Count < 2  -> Status = high_veto_risk % Adjusted sensitivity
    ;   Status = blocked_by_veto)
    ).

%% is_safe_to_cut(+ConstraintID)
% Theorem 3: Cutting a load-bearing Noose requires a Scaffold.
is_safe_to_cut(C) :-
    narrative_ontology:constraint_metric(C, extractiveness, X), X > 0.7, % Load-bearing Noose
    narrative_ontology:entity(_, scaffold). % A scaffold MUST exist in the KB

/* ================================================================
   3. SCENARIO-AWARE DIAGNOSTIC REPORTING
   ================================================================ */

%% dr_diagnostic_report(+IntervalID)
%  Filters output to only show items linked to the current interval.
dr_diagnostic_report(IntervalID) :-
    format('~n=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: ~w ===~n', [IntervalID]),
    
    format('~n[CONSTRAINT INVENTORY]~n'),
    % Use setof to ensure each unique Name/State/Intensity combination is only printed once
    (   setof(line(Name, State, Intensity), 
              (constraint_status(Name, State, Intensity), 
               narrative_ontology:constraint_metric(Name, _, _)), 
              UniqueLines)
    ->  forall(member(line(N, S, I), UniqueLines),
               format('  - ~w: ~w (Intensity: ~2f)~n', [N, S, I]))
    ;   format('  No active constraints found.~n')
    ),
    
    format('~n[FEASIBILITY BRIDGE]~n'),
    % Only show recommendations that affect a constraint in the current inventory
    forall((narrative_ontology:recommendation(RID, Summary),
            narrative_ontology:affects_constraint(RID, ConsName),
            narrative_ontology:constraint_claim(ConsName, _)),
           (recommendation_feasibility(RID, Stat, Vs),
            format('  - ~w (~w): ~w | Vetoes: ~w~n', [RID, Summary, Stat, Vs]))),
    format('====================================================~n').
