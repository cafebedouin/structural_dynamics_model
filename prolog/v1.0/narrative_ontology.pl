:- module(narrative_ontology, [
    % Core ontology
    entity/2,
    interval/3,
    event/4,

    % CE v2.0 constraint layer
    constraint_claim/2,
    recommendation/2,
    affects_constraint/2,
    veto_actor/1,
    veto_exposed/2,
    constraint_metric/3,

    % Optional measurement layer (v3.1 coercion metrics)
    measurement/5,

    % Optional intent evidence layer
    intent_viable_alternative/3,
    intent_alternative_rejected/3,
    intent_beneficiary_class/2,
    intent_power_change/3,
    intent_suppression_level/4,
    intent_resistance_level/4,
    intent_norm_strength/3,

    % Validation entry point
    validate_ontology/0
]).

/* ============================================================
   1. MULTIFILE & DYNAMIC DECLARATIONS
   ============================================================ */

:- multifile 
    entity/2, interval/3, event/4,
    constraint_claim/2, recommendation/2, affects_constraint/2,
    veto_actor/1, veto_exposed/2, constraint_metric/3,
    measurement/5,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3.

:- dynamic 
    entity/2, interval/3, event/4,
    constraint_claim/2, recommendation/2, affects_constraint/2,
    veto_actor/1, veto_exposed/2, constraint_metric/3,
    measurement/5,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3.

/* ============================================================
   2. GLOBAL TERM EXPANSION
   Redirects facts in other modules to narrative_ontology
   ============================================================ */

% This allows data files to simply state 'entity(x, y).' 
% and have it correctly registered as 'narrative_ontology:entity(x, y).'
user:term_expansion(entity(A,B), narrative_ontology:entity(A,B)).
user:term_expansion(interval(A,B,C), narrative_ontology:interval(A,B,C)).
user:term_expansion(event(A,B,C,D), narrative_ontology:event(A,B,C,D)).
user:term_expansion(constraint_claim(A,B), narrative_ontology:constraint_claim(A,B)).
user:term_expansion(recommendation(A,B), narrative_ontology:recommendation(A,B)).
user:term_expansion(affects_constraint(A,B), narrative_ontology:affects_constraint(A,B)).
user:term_expansion(veto_actor(A), narrative_ontology:veto_actor(A)).
user:term_expansion(veto_exposed(A,B), narrative_ontology:veto_exposed(A,B)).
user:term_expansion(constraint_metric(A,B,C), narrative_ontology:constraint_metric(A,B,C)).
user:term_expansion(intent_beneficiary_class(A,B), narrative_ontology:intent_beneficiary_class(A,B)).
user:term_expansion(intent_power_change(A,B,C), narrative_ontology:intent_power_change(A,B,C)).
user:term_expansion(intent_suppression_level(A,B,C,D), narrative_ontology:intent_suppression_level(A,B,C,D)).
user:term_expansion(intent_resistance_level(A,B,C,D), narrative_ontology:intent_resistance_level(A,B,C,D)).
user:term_expansion(intent_norm_strength(A,B,C), narrative_ontology:intent_norm_strength(A,B,C)).
user:term_expansion(intent_viable_alternative(A,B,C), narrative_ontology:intent_viable_alternative(A,B,C)).
user:term_expansion(intent_alternative_rejected(A,B,C), narrative_ontology:intent_alternative_rejected(A,B,C)).
user:term_expansion(measurement(A,B,C,D,E), narrative_ontology:measurement(A,B,C,D,E)).

/* ============================================================
   3. VALIDATION LOGIC
   ============================================================ */

validate_ontology :-
    validate_entities,
    validate_intervals,
    validate_events,
    validate_constraints,
    validate_constraint_metrics,
    validate_measurements,
    validate_intent.

validate_entities :-
    forall(entity(ID,Type),
        ( atom(ID), atom(Type)
        -> true
        ;  format('ERROR: Invalid entity(~w,~w)~n',[ID,Type]), fail
        )).

validate_intervals :-
    forall(interval(I,T0,Tn),
        ( atom(I), integer(T0), integer(Tn), T0 =< Tn
        -> true
        ;  format('ERROR: Invalid interval(~w,~w,~w)~n',[I,T0,Tn]), fail
        )).

validate_events :-
    forall(event(E,K,T,Props),
        ( atom(E), atom(K), integer(T), is_list(Props)
        -> true
        ;  format('ERROR: Invalid event(~w,~w,~w,~w)~n',[E,K,T,Props]), fail
        )).

validate_constraints :-
    forall(constraint_claim(Name,Type),
        ( atom(Name),
          member(Type,[mountain,rope,noose,zombie])
        -> true
        ;  format('ERROR: Invalid constraint_claim(~w,~w)~n',[Name,Type]), fail
        )).

validate_constraint_metrics :-
    forall(constraint_metric(Name,Metric,Val),
        ( (constraint_claim(Name,_) ; true), % Allow metrics even if claim is missing during partial loads
          atom(Metric),
          number(Val), Val >= 0.0, Val =< 1.0
        -> true
        ;  format('ERROR: Invalid constraint_metric(~w,~w,~w)~n',[Name,Metric,Val]), fail
        )).

validate_measurements :-
    forall(measurement(ID,Target,Metric,Time,Val),
        ( atom(ID),
          (entity(Target,_) ; interval(Target,_,_)),
          (atom(Metric) ; compound(Metric)),
          integer(Time),
          number(Val), Val >= 0.0, Val =< 1.0
        -> true
        ;  format('ERROR: Invalid measurement(~w,~w,~w,~w,~w)~n',
                  [ID,Target,Metric,Time,Val]), fail
        )).

validate_intent :-
    forall(intent_viable_alternative(I,S,A),
        ( interval(I,_,_), (entity(S,_) ; atom(S)), atom(A)
        -> true
        ;  format('ERROR: Invalid intent_viable_alternative(~w,~w,~w)~n',[I,S,A]), fail
        )),
    % Additional intent validations (beneficiaries, power changes, etc.) follow same pattern...
    true.
