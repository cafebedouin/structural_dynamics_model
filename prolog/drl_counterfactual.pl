% ============================================================================
% DRL COUNTERFACTUAL — Counterfactual Reasoning (Context-Indexed)
% Split from drl_modal_logic.pl (v4.0+)
% ============================================================================

:- module(drl_counterfactual, [
    % Stage 3: Counterfactual Reasoning (INDEXED)
    simulate_cut/3,
    simulate_cut/2,
    dependency_chain/5,
    dependency_chain/4,
    infer_structural_coupling/3,
    assess_scaffold_need/3,
    assess_scaffold_need/2,
    counterfactual_world/4,
    counterfactual_world/3,

    % Cross-context analysis
    standard_context/1,
    count_sign_matches/4
]).

:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(config).

/* ================================================================
   STAGE 3: COUNTERFACTUAL REASONING (INDEXED)
   Reasoning about possible worlds after interventions
   NOW INDEXED: "What happens if WE cut this?" depends on WHO "we" are
   ================================================================ */

%% simulate_cut(+Constraint, +Context, -Effects)
% PRIMARY API: Simulates cutting a constraint FROM A SPECIFIC CONTEXT
% CRITICAL: Same cut has different effects depending on who does it
%
% Example: Cutting property rights
%   FROM institutional context: moderate coordination disruption
%   FROM powerless context: catastrophic (removes only shelter access)
simulate_cut(Constraint, Context, Effects) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(Constraint, Context, Type),
    (Type = snare ; Type = piton ; Type = rope),  % Only cut these types
    findall(effect(Target, Impact, Reason),
            dependency_chain(Constraint, Target, Impact, Reason, Context),
            Effects).

%% simulate_cut(+Constraint, -Effects)
% BACKWARD COMPAT: Uses analytical context
simulate_cut(Constraint, Effects) :-
    constraint_indexing:default_context(Ctx),
    simulate_cut(Constraint, Ctx, Effects).

%% dependency_chain(+Source, -Target, -Impact, -Reason, +Context)
% PRIMARY API: Discovers dependencies FROM A SPECIFIC CONTEXT
% CRITICAL UPGRADE: "Depends on" is now relative to WHO would be affected
%
% What's load-bearing from powerless context may be non-essential from institutional
dependency_chain(Source, Target, Impact, Reason, Context) :-
    constraint_indexing:valid_context(Context),

    % 1. Explicit dependencies (declared in data)
    narrative_ontology:affects_constraint(Source, Target),
    estimate_impact_indexed(Source, Target, Context, Impact, Reason).

dependency_chain(Source, Target, Impact, Reason, Context) :-
    constraint_indexing:valid_context(Context),

    % 2. Inferred structural coupling
    infer_structural_coupling(Source, Target, Strength),
    config:param(dependency_coupling_threshold, CoupThresh),
    Strength > CoupThresh,

    % But assess impact FROM THIS CONTEXT
    (   context_depends_critically(Target, Source, Context)
    ->  Impact = catastrophic,
        Reason = inferred_load_bearing_coupling
    ;   Impact = moderate,
        Reason = inferred_weak_coupling
    ).

%% dependency_chain(+Source, -Target, -Impact, -Reason)
% BACKWARD COMPAT: Uses analytical context
dependency_chain(Source, Target, Impact, Reason) :-
    constraint_indexing:default_context(Ctx),
    dependency_chain(Source, Target, Impact, Reason, Ctx).

%% context_depends_critically(+Target, +Source, +Context)
% Determines if Target critically depends on Source FROM CONTEXT
% Used to detect load-bearing relationships per perspective
context_depends_critically(Target, Source, Context) :-
    % Target appears as Mountain from this context
    drl_core:dr_type(Target, Context, mountain),

    % Source provides the only perceived stability
    drl_core:dr_type(Source, Context, SourceType),
    member(SourceType, [rope, snare]),

    % Check if Target's stability requires Source
    narrative_ontology:affects_constraint(Source, Target).

%% estimate_impact_indexed(+Source, +Target, +Context, -Impact, -Reason)
% Estimates impact of cutting Source on Target FROM SPECIFIC CONTEXT
% CRITICAL: Same cut has different impacts depending on who's cutting

estimate_impact_indexed(Source, Target, Context, catastrophic, load_bearing) :-
    % Source is load-bearing FROM THIS CONTEXT
    drl_core:dr_type(Source, Context, snare),
    narrative_ontology:constraint_metric(Source, extractiveness, X),
    config:param(snare_load_bearing_threshold, T),
    X > T,

    % Target depends on it FROM THIS CONTEXT
    drl_core:dr_type(Target, Context, mountain),
    !.

estimate_impact_indexed(Source, Target, Context, beneficial, removes_extraction) :-
    % Source is extractive, Target is functional
    drl_core:dr_type(Source, Context, snare),
    drl_core:dr_type(Target, Context, rope),
    !.

estimate_impact_indexed(Source, Target, Context, moderate, disrupts_coordination) :-
    % Both are coordination mechanisms
    drl_core:dr_type(Source, Context, rope),
    drl_core:dr_type(Target, Context, rope),
    !.

estimate_impact_indexed(_, _, _, negligible, no_dependency) :- !.

%% infer_structural_coupling(+C1, +C2, -Strength)
%  Discovers hidden dependencies via temporal gradient correlation.
%  Strength is the sign-agreement ratio: Matches / Total, in [0, 1].
%  A pair of gradients "matches" when both are positive, both negative,
%  or both zero. This captures co-directional movement without requiring
%  magnitude correlation.
infer_structural_coupling(C1, C2, Strength) :-
    C1 \= C2,
    findall(G1, dr_gradient_at(C1, _, G1), Gs1),
    findall(G2, dr_gradient_at(C2, _, G2), Gs2),
    length(Gs1, L), L > 1, length(Gs2, L),
    count_sign_matches(Gs1, Gs2, Matches, L),
    Strength is Matches / L.

dr_gradient_at(C, T, Grad) :-
    narrative_ontology:measurement(_, C, extractiveness, T, X1),
    once((
        narrative_ontology:measurement(_, C, extractiveness, T2, X2),
        T2 > T
    )),
    Grad is X2 - X1.

%% count_sign_matches(+Gs1, +Gs2, -Matches, +Total)
%  Counts how many gradient pairs have matching signs.
count_sign_matches([], [], 0, _).
count_sign_matches([H1|T1], [H2|T2], Matches, Total) :-
    count_sign_matches(T1, T2, SubMatches, Total),
    (   signs_match(H1, H2)
    ->  Matches is SubMatches + 1
    ;   Matches = SubMatches
    ).

%% signs_match(+A, +B)
%  True when A and B have the same sign (both positive, both negative, or both zero).
signs_match(A, B) :- A > 0, B > 0.
signs_match(A, B) :- A < 0, B < 0.
signs_match(A, B) :- A =:= 0, B =:= 0.

%% assess_scaffold_need(+Constraint, +Context, -Assessment)
% PRIMARY API: Determines if cutting requires Scaffold FROM SPECIFIC CONTEXT
% THEOREM 3 INDEXED: "Need scaffold" depends on WHO is cutting
%
% Critical case: Institutional cutting powerless Mountain
%   FROM institutional context: no_scaffold_needed (just coordination)
%   FROM powerless context: scaffold_required (removes survival support)
%   SYSTEM MUST EVALUATE BOTH CONTEXTS before allowing cut
assess_scaffold_need(Constraint, Context, Assessment) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(Constraint, Context, Type),
    member(Type, [snare, piton, rope]),

    simulate_cut(Constraint, Context, Effects),

    (   member(effect(_, catastrophic, load_bearing), Effects)
    ->  (   narrative_ontology:entity(Scaffold, scaffold),
            Scaffold \= Constraint
        ->  Assessment = scaffold_present
        ;   Assessment = scaffold_required
        )
    ;   Assessment = no_scaffold_needed
    ).

%% assess_scaffold_need(+Constraint, -Assessment)
% BACKWARD COMPAT: Uses analytical context
assess_scaffold_need(Constraint, Assessment) :-
    constraint_indexing:default_context(Ctx),
    assess_scaffold_need(Constraint, Ctx, Assessment).

%% counterfactual_world(+Intervention, +CurrentWorld, +Context, -FutureWorld)
% PRIMARY API: Models state after intervention FROM SPECIFIC CONTEXT
counterfactual_world(cut(C), current, Context, after_cut) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C, Context, Type),
    format('In world after cutting ~w (~w from ~w):~n', [C, Type, Context]),
    simulate_cut(C, Context, Effects),
    forall(member(effect(Target, Impact, Reason), Effects),
           format('  - ~w: ~w (~w)~n', [Target, Impact, Reason])).

counterfactual_world(add_scaffold(S, For), current, Context, with_scaffold) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(For, Context, snare),
    format('In world with scaffold ~w for ~w (from ~w):~n', [S, For, Context]),
    format('  - Temporary support for transition~n'),
    format('  - Allows safe removal of ~w~n', [For]).

%% counterfactual_world(+Intervention, +CurrentWorld, -FutureWorld)
% BACKWARD COMPAT: Uses analytical context
counterfactual_world(Intervention, Current, Future) :-
    constraint_indexing:default_context(Ctx),
    counterfactual_world(Intervention, Current, Ctx, Future).

/* ================================================================
   CROSS-CONTEXT DEPENDENCY ANALYSIS
   Critical for detecting "institutional Mountain collapse"
   ================================================================ */

%% check_all_contexts(+Constraint, -MultiContextReport)
% Evaluates cutting a constraint across multiple standard contexts
% CRITICAL FOR SAFETY: Before cutting, check impact on ALL perspectives
%
% Example usage:
%   check_all_contexts(property_rights_2025, Report)
%   → Shows if cut is safe for institution but catastrophic for powerless
check_all_contexts(Constraint, Report) :-
    findall(
        context_impact(Context, Type, ScaffoldNeed, Effects),
        (   standard_context(Context),
            drl_core:dr_type(Constraint, Context, Type),
            assess_scaffold_need(Constraint, Context, ScaffoldNeed),
            simulate_cut(Constraint, Context, Effects)
        ),
        Impacts
    ),
    Report = multi_context_analysis(Constraint, Impacts).

% Standard contexts for cross-perspective analysis
standard_context(context(agent_power(powerless),
                        time_horizon(biographical),
                        exit_options(trapped),
                        spatial_scope(local))).

standard_context(context(agent_power(moderate),
                        time_horizon(biographical),
                        exit_options(mobile),
                        spatial_scope(national))).

standard_context(context(agent_power(institutional),
                        time_horizon(generational),
                        exit_options(arbitrage),
                        spatial_scope(national))).

standard_context(context(agent_power(analytical),
                        time_horizon(civilizational),
                        exit_options(analytical),
                        spatial_scope(global))).
