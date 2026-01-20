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
    omega_variable/3,

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
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3.

:- dynamic
    attribute/3, has_mandatrophy_declaration/1,
    entity/2, interval/3, event/4,
    constraint_claim/2, recommendation/2, affects_constraint/2,
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3.

/* ============================================================
   2. VALIDATION LOGIC
   ============================================================ */
%% attribute(+Subject, +Key, +Value)
%  Generic metadata getter/setter used for indexical resolution.
%  This links the metadata check in is_indexical_resolution_declared/1 
%  to the actual stored metrics.
attribute(S, K, V) :- 
    narrative_ontology:constraint_metric(S, K, V).

%% has_mandatrophy_declaration(+Constraint)
%  A manual override flag used by check_indexical_relativity/1.
%  If a constraint is explicitly marked as 'mandatrophy' in its metadata,
%  it passes the indexical relativity gate.
has_mandatrophy_declaration(C) :- 
    attribute(C, lifecycle, mandatrophy).

%% validate_ontology
%  Master entry point for checking Knowledge Base integrity.
validate_ontology :-
    format('--- [START] v3.1 DATA INTEGRITY VERIFICATION ---~n'),
    (   validate_entities,
        validate_intervals,
        validate_events,
        validate_constraint_claims,
        validate_constraint_metrics,
        validate_measurements,
        validate_omegas,
        validate_intent
    ->  format('[OK] Ontology Schema matches.~n')
    ;   format('[FAIL] Schema mismatch.~n')
    ),
    format('--- [END] Verification Complete ---~n').

validate_entities :-
    forall(entity(ID,Type),
        ( atom(ID),
          member(Type, [individual_powerless, individual_powerful, 
                        institutional, analytical, class])
        -> true
        ;  format('ERROR: Invalid entity(~w,~w)~n',[ID,Type]), fail
        )).

validate_intervals :-
    forall(interval(ID,Start,End),
        ( atom(ID), integer(Start), integer(End), Start =< End
        -> true
        ;  format('ERROR: Invalid interval(~w,~w,~w)~n',[ID,Start,End]), fail
        )).

validate_events :-
    forall(event(ID,Time,Actor,Type),
        ( atom(ID), integer(Time), (entity(Actor,_) ; atom(Actor)), atom(Type)
        -> true
        ;  format('ERROR: Invalid event(~w,~w,~w,~w)~n',[ID,Time,Actor,Type]), fail
        )).

%% validate_constraint_claims
%  Updated for v3.2.4 schema expansion.
validate_constraint_claims :-
    forall(constraint_claim(Name, Type),
        ( member(Type, [mountain, rope, noose])
        -> true
        ;  format('ERROR: Ontological Violation in ~w: "~w" is not a v3.1 standard pillar.~n', [Name, Type]), 
           fail
        )).

validate_constraint_metrics :-
    forall(constraint_metric(Name,Metric,Val),
        ( (constraint_claim(Name,_) ; true),
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

validate_omegas :-
    forall(omega_variable(ID, Type, Desc),
        ( atom(ID),
          member(Type, [empirical, conceptual, preference]),
          (atom(Desc) ; string(Desc))
        -> true
        ;  format('ERROR: Invalid omega_variable(~w,~w,~w)~n',[ID,Type,Desc]), fail
        )).

validate_intent :-
    forall(intent_viable_alternative(I,S,A),
        ( (interval(I,_,_) ; atom(I)), (entity(S,_) ; atom(S)), atom(A)
        -> true
        ;  format('ERROR: Invalid intent_viable_alternative(~w,~w,~w)~n',[I,S,A]), fail
        )).

/* ==========================================================================
   INDEXICAL GATE (v3.1)
   ========================================================================== */

% Indexical Gate: Flags Mountains that are actually Shadow Nooses.
check_indexical_relativity(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    domain_priors:base_extractiveness(ID, E),
    E > 0.7,
    % Require a manual "Mandatrophy Acknowledgement" in the file metadata
    \+ has_mandatrophy_declaration(ID).

% Every domain must be indexed to all three power-perspectives.
validate_indexical_completeness(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    constraint_indexing:constraint_classification(ID, rope, _),
    constraint_indexing:constraint_classification(ID, noose, _),
    !.
validate_indexical_completeness(ID) :-
    format('ERROR: Perspectival Gap in ~w. Missing 3-pillar coverage.~n', [ID]),
    fail.

/* ==========================================================================
   MANDATROPHY RECONCILIATION (v3.1)
   ========================================================================== */

% is_mandatrophy_resolved/1: Explicitly standardizes the 2 residual Omegas.
is_mandatrophy_resolved(gale_shapley).        % The Algorithm is the Mandate.
is_mandatrophy_resolved(planetary_boundaries). % The Biological Limit is the Mandate.

% detect_omega/2: Identifies logical friction points.
% This rule is updated to exempt "Hardened Mandatrophy."
detect_omega(Name, mandatrophy) :-
    constraint_indexing:constraint_classification(Name, mountain, _),
    domain_priors:base_extractiveness(Name, E),
    E > 0.7,
    % The Paradox: A Mountain (Fact) behaves like a Noose (Trap).
    % If the domain is recognized as Mandatrophic, it is no longer an Omega.
    \+ is_mandatrophy_resolved(Name).

% System Insight: Logic for the Parsing Suite
count_unresolved_omegas(Count) :-
    aggregate_all(count, detect_omega(_, _), Count).

% Mandatrophy Detector: Flags Mountains that function as Traps.
detect_mandatrophy_omega(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    domain_priors:base_extractiveness(ID, E),
    E > 0.7,
    % Check for the explicit resolution marker in the file
    \+ is_indexical_resolution_declared(ID).

is_indexical_resolution_declared(ID) :-
    % Looks for the standardized resolution hook in the metadata
    attribute(ID, indexical_resolution, resolved).
