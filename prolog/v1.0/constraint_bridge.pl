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

%% recommendation_feasibility(+RecID, -Status, -UniqueVetoes)
%  Uses setof/3 to ensure actors are only counted once.
recommendation_feasibility(RecID, Status, UniqueVetoes) :-
    (setof(Actor, narrative_ontology:veto_exposed(Actor, RecID), UniqueVetoes) 
     ; UniqueVetoes = []),
    length(UniqueVetoes, Count),
    (Count == 0 -> Status = viable 
    ; Count < 2  -> Status = high_veto_risk % Adjusted sensitivity
    ; Status = blocked).

/* ================================================================
   3. SCENARIO-AWARE DIAGNOSTIC REPORTING
   ================================================================ */

%% dr_diagnostic_report(+IntervalID)
%  Filters output to only show items linked to the current interval.
dr_diagnostic_report(IntervalID) :-
    format('~n=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: ~w ===~n', [IntervalID]),
    
    format('~n[CONSTRAINT INVENTORY]~n'),
    % Only show constraints if they have a metric linked to this scenario
    forall((constraint_status(Name, State, Intensity), 
            narrative_ontology:constraint_metric(Name, _, _)),
           format('  - ~w: ~w (Intensity: ~2f)~n', [Name, State, Intensity])),

    format('~n[FEASIBILITY BRIDGE]~n'),
    % Only show recommendations that affect a constraint in the current inventory
    forall((narrative_ontology:recommendation(RID, Summary),
            narrative_ontology:affects_constraint(RID, ConsName),
            narrative_ontology:constraint_claim(ConsName, _)),
           (recommendation_feasibility(RID, Stat, Vs),
            format('  - ~w (~w): ~w | Vetoes: ~w~n', [RID, Summary, Stat, Vs]))),
    format('====================================================~n').

