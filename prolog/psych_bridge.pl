:- module(psych_bridge, [
    is_substrate/3,
    is_negotiable_pattern/3,
    is_hybrid_pattern/3,
    is_self_extraction/3,
    is_developmental_support/3,
    is_abandoned_camp/3
]).

:- use_module(drl_core).
:- use_module(narrative_ontology).

% Helper predicate to temporarily swap metrics for psychological analysis.
% It checks for 'internalization_depth' and maps it to 'suppression_requirement'
% for the duration of a goal's execution.
with_psych_metric(Constraint, Goal) :-
    (   narrative_ontology:constraint_metric(Constraint, internalization_depth, Depth)
    ->  % If internalization_depth exists, use it for suppression_requirement
        asserta(narrative_ontology:constraint_metric(Constraint, suppression_requirement, Depth)),
        call(Goal),
        retract(narrative_ontology:constraint_metric(Constraint, suppression_requirement, Depth))
    ;   % Otherwise, just call the goal with existing metrics
        call(Goal)
    ).

%% is_substrate(?Constraint, ?Context, ?Type)
%  Psychological alias for is_mountain. Represents biological or neurological limits.
is_substrate(C, Context, substrate) :-
    with_psych_metric(C, drl_core:is_mountain(C, Context, mountain)).

%% is_negotiable_pattern(?Constraint, ?Context, ?Type)
%  Psychological alias for is_rope. Represents healthy, functional habits.
is_negotiable_pattern(C, Context, negotiable_pattern) :-
    with_psych_metric(C, drl_core:is_rope(C, Context, rope)).

%% is_hybrid_pattern(?Constraint, ?Context, ?Type)
%  Psychological alias for is_tangled_rope. Represents habits that both serve and harm.
is_hybrid_pattern(C, Context, hybrid_pattern) :-
    with_psych_metric(C, drl_core:is_tangled_rope(C, Context, tangled_rope)).

%% is_self_extraction(?Constraint, ?Context, ?Type)
%  Psychological alias for is_snare. Represents self-destructive patterns like addiction.
is_self_extraction(C, Context, self_extraction) :-
    with_psych_metric(C, drl_core:is_snare(C, Context, snare)).

%% is_developmental_support(?Constraint, ?Context, ?Type)
%  Psychological alias for is_scaffold. Represents temporary supports like therapy or mentorship.
is_developmental_support(C, Context, developmental_support) :-
    with_psych_metric(C, drl_core:is_scaffold(C, Context, scaffold)).

%% is_abandoned_camp(?Constraint, ?Context, ?Type)
%  Psychological alias for is_piton. Represents outdated trauma responses or obsolete coping mechanisms.
is_abandoned_camp(C, Context, abandoned_camp) :-
    with_psych_metric(C, drl_core:is_piton(C, Context, piton)).
