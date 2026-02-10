% ============================================================================
% DRL MODAL LOGIC - INDEXICAL VERSION v4.0
% ============================================================================
% This module implements context-indexed modal reasoning for Deferential Realism.
% All dependency chains, transformations, and counterfactuals are relative to
% WHO is analyzing/intervening and WHEN they're doing it.
%
% BREAKING CHANGES FROM v3.x:
% - dependency_chain/4 → dependency_chain/5 (added Context)
% - assess_scaffold_need/2 → assess_scaffold_need/3 (added Context)
% - simulate_cut/2 → simulate_cut/3 (added Context)
% - composite_type/3 → composite_type/4 (added Context)
% - All counterfactual reasoning now indexed
%
% KEY INSIGHT: "Load-bearing" depends on WHO is cutting the constraint.
% What's load-bearing for powerless individual may be non-essential for institution.
%
% Integration: Load after drl_core.pl (v4.0+)
% ============================================================================

:- module(drl_modal_logic, [
    % Stage 1: Composition Rules (INDEXED)
    composite_type/4,               % Added Context parameter
    composite_type/3,               % Backward compat
    composition_rule/3,
    detect_extraction_dominance/2,
    detect_necessity_inheritance/2,
    
    % Stage 2: Transformation Tracking (Context-aware)
    constraint_history/3,           % Added Context parameter
    constraint_history/2,           % Backward compat
    transformation_detected/5,      
    transformation_type/6,
    canonical_transformation/6,
    predict_transformation/3,
    
    % Stage 3: Counterfactual Reasoning (INDEXED)
    simulate_cut/3,                 % Added Context parameter
    simulate_cut/2,                 % Backward compat
    dependency_chain/5,             % Added Context parameter
    dependency_chain/4,             % Backward compat
    infer_structural_coupling/3,
    assess_scaffold_need/3,         % Added Context parameter
    assess_scaffold_need/2,         % Backward compat
    counterfactual_world/4,         % Added Context parameter
    counterfactual_world/3,         % Backward compat

    % Stage 4: Audit
    possibly/1,
    necessarily/1,
    is_snare/1,
    is_mountain/1,
    is_rope/1,

    % Stage 5: Boltzmann-Aware Analysis (v5.0)
    reformability_score/3,              % reformability_score(C, Context, Score)
    reformability_score/2,              % Backward compat (analytical context)
    boltzmann_invariant_check/2,        % boltzmann_invariant_check(C, Result)
    coupling_aware_scaffold_need/3,     % coupling_aware_scaffold_need(C, Context, Assessment)

    % Stage 6: Purity-Aware Reform Recommendations (v5.1)
    purity_reform_target/2,             % purity_reform_target(C, Target)
    purity_reform_recommendation/2,     % purity_reform_recommendation(C, Recommendation)

    % Stage 7: Purity-Qualified Action Algebra (v5.1)
    purity_qualified_action/4,          % purity_qualified_action(C, Context, QAction, Rationale)
    purity_qualified_action/3,          % Backward compat (analytical context)
    purity_adjusted_energy/4,           % purity_adjusted_energy(C, Context, BaseAction, EnergyCost)
    action_composition_gate/3,          % action_composition_gate(C, CompositeAction, GateResult)
    purity_scaffold_urgency/4,          % purity_scaffold_urgency(C, Context, Urgency, Factors)

    % Stage 8: Purity Propagation Network (v5.2)
    constraint_neighbors/3,             % constraint_neighbors(C, Context, Neighbors)
    constraint_neighbors/2,             % Backward compat
    shared_agent_link/4,                % shared_agent_link(C1, C2, LinkType, Agent)
    effective_purity/4,                 % effective_purity(C, Context, EffPurity, Components)
    effective_purity/3,                 % Backward compat
    purity_contamination_pressure/4,    % purity_contamination_pressure(Src, Tgt, Context, Pressure)
    network_purity_metrics/2,           % network_purity_metrics(Context, Metrics)
    cluster_purity/3,                   % cluster_purity(Constraints, Context, Score)
    contamination_path/5,               % contamination_path(Src, Tgt, Context, Path, Loss)
    weakest_link_purity/3,              % weakest_link_purity(Context, Constraint, Purity)
    network_qualified_action/4,         % network_qualified_action(C, Context, QAction, Rationale)
    network_qualified_action/3,         % Backward compat
    type_contamination_strength/2,      % type_contamination_strength(Type, Strength)
    type_immunity/2                     % type_immunity(Type, Immunity)
]).

:- use_module(drl_audit_core).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(coercion_projection).
:- use_module(structural_signatures).

:- dynamic agent_index/2.
:- dynamic constraint_data/2.

% Scaled for the Powerless
is_snare(C) :- 
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, powerless, Chi),
    drl_audit_core:structural_signature(Chi, Data, snare).

% Scaled for the Observer
is_mountain(C) :- 
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, analytical, Chi),
    drl_audit_core:structural_signature(Chi, Data, mountain).

% Scaled for the Beneficiary
is_rope(C) :- 
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, institutional, Chi),
    drl_audit_core:structural_signature(Chi, Data, rope).

/* ================================================================
   MODAL LOGIC EXTENSION FOR DEFERENTIAL REALISM v4.0
   
   VERSION 4.0 CHANGES:
   - Full indexical relativity integration
   - All dependency analysis is context-relative
   - "Load-bearing" now indexed to WHO is cutting
   - Scaffold assessment indexed to WHO needs transition
   - Theorem 3 ("cutting load-bearing Snare requires Scaffold")
     now evaluates per perspective
   
   CRITICAL THEOREM 3 UPGRADE:
   OLD: "If Snare is load-bearing, cutting requires Scaffold"
   NEW: "If Snare is load-bearing FROM CONTEXT C, cutting from C requires Scaffold"
   
   Example: Property rights
   - FROM powerless context: load-bearing Mountain (shelter dependency)
   - FROM institutional context: non-essential Rope (coordination choice)
   - Institutional cutting WITHOUT considering powerless dependency = catastrophe
   
   This module implements three stages of indexed modal reasoning:
   1. Composition Rules - how constraints interact (context-aware)
   2. Transformation Tracking - temporal evolution (context-aware)
   3. Counterfactual Reasoning - intervention simulation (context-indexed)
   ================================================================ */

/* ================================================================
   STAGE 1: COMPOSITION RULES (INDEXED)
   Modal logic for how constraints interact and compose
   ================================================================ */

%% composite_type(+C1, +C2, +Context, -ResultType)
% PRIMARY API: Determines composite type FROM A SPECIFIC CONTEXT
composite_type(C1, C2, Context, Result) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C1, Context, T1),
    drl_core:dr_type(C2, Context, T2),
    composition_rule(T1, T2, Result).

%% composite_type(+C1, +C2, -ResultType)
% BACKWARD COMPAT: Uses analytical context
composite_type(C1, C2, Result) :-
    constraint_indexing:default_context(Ctx),
    composite_type(C1, C2, Ctx, Result).

%% composition_rule(+Type1, +Type2, -CompositeType)
% Formal modal composition rules from DR logic
% NOTE: These rules are NOT indexed - they're about logical structure
%       Context affects input types, not composition rules themselves

% Necessity Inheritance: ■ C₁ ∧ (C₁ → C₂) ⇒ ■ C₂
% If C1 is a Mountain and implies C2, then C2 is also a Mountain
composition_rule(mountain, _, mountain) :- !.
composition_rule(_, mountain, mountain) :- !.

% Extraction Dominance: ⊞C₁ ∧ ⊠ C₂ ∧ Embedded(C₂, C₁) ⇒ ⊠ (C₁ ∧ C₂)
% When a Snare is embedded in a Rope, the whole becomes extractive
composition_rule(rope, snare, snare) :- !.
composition_rule(snare, rope, snare) :- !.
composition_rule(tangled_rope, snare, snare) :- !.
composition_rule(snare, tangled_rope, snare) :- !.

% Snare Dominance: Multiple Snares compound
composition_rule(snare, snare, snare) :- !.

% Rope Composition: ⊞C₁ ∧ ⊞C₂ ∧ Compatible(C₁, C₂) ⇒ ⊞(C₁ ∧ C₂)
% Compatible Ropes can be composed into compound Ropes
composition_rule(rope, rope, rope) :- !.

% Tangled interactions
composition_rule(tangled_rope, tangled_rope, tangled_rope) :- !.
composition_rule(rope, tangled_rope, tangled_rope) :- !.
composition_rule(tangled_rope, rope, tangled_rope) :- !.

% Piton contamination
composition_rule(piton, _, piton) :- !.
composition_rule(_, piton, piton) :- !.

% Unknown fallback
composition_rule(_, _, unknown).

%% detect_extraction_dominance(+Composite, -Evidence)
% Detects when a Rope is corrupted by an embedded Snare
% NOTE: Uses default analytical context for detection
detect_extraction_dominance(Composite, Evidence) :-
    narrative_ontology:affects_constraint(Composite, Component),
    drl_core:dr_type(Component, snare),
    narrative_ontology:constraint_metric(Component, extractiveness, X),
    config:param(snare_epsilon_floor, Floor),
    X >= Floor,
    Evidence = embedded_snare(Component, X).

%% detect_necessity_inheritance(+Source, -Derived)
% Detects when a Mountain constraint logically implies another constraint
% NOTE: Uses default analytical context for detection
detect_necessity_inheritance(Source, Derived) :-
    drl_core:dr_type(Source, mountain),
    narrative_ontology:affects_constraint(Source, Derived),
    narrative_ontology:constraint_metric(Source, suppression_requirement, E_source),
    E_source =< 0.05,
    % If the derived constraint should also be a Mountain
    narrative_ontology:constraint_metric(Derived, suppression_requirement, E_derived),
    E_derived =< 0.05.

/* ================================================================
   STAGE 2: TRANSFORMATION TRACKING (CONTEXT-AWARE)
   Temporal modal logic for constraint evolution
   ================================================================ */

%% constraint_history(+C, +Context, -Timeline)
% PRIMARY API: Collects constraint history FROM A SPECIFIC CONTEXT
% Same constraint may have different transformation patterns from different perspectives
constraint_history(C, Context, Timeline) :-
    constraint_indexing:valid_context(Context),
    findall(state(T, Type), 
            (narrative_ontology:measurement(_, C, _, T, _),
             dr_type_at(C, T, Context, Type)),
            TimelineUnsorted),
    sort(TimelineUnsorted, Timeline).

%% constraint_history(+C, -Timeline)
% BACKWARD COMPAT: Uses analytical context
constraint_history(C, Timeline) :-
    constraint_indexing:default_context(Ctx),
    constraint_history(C, Ctx, Timeline).

%% dr_type_at(+C, +Time, +Context, -Type)
% Determines constraint type at specific time FROM SPECIFIC CONTEXT
% Delegates to drl_core:classify_from_metrics/6 (Single Source of Truth)
dr_type_at(C, Time, Context, Type) :-
    (narrative_ontology:measurement(_, C, suppression_requirement, Time, Supp) -> true ; Supp = 0.5),
    (narrative_ontology:measurement(_, C, extractiveness, Time, BaseX) -> true ; BaseX = 0.5),
    Context = context(agent_power(Power), _, _, _),
    constraint_indexing:power_modifier(Power, Modifier),
    Chi is BaseX * Modifier,
    drl_core:classify_from_metrics(C, BaseX, Chi, Supp, Context, Type).

%% transformation_detected(+C, +FromType, +ToType, -T1, -T2)
% Detects when constraint transformed from one type to another
% Uses actual measurement times (not iteration)
transformation_detected(C, From, To, T1, T2) :-
    constraint_history(C, Timeline),
    member(state(T1, From), Timeline),
    member(state(T2, To), Timeline),
    T2 > T1,
    From \= To,
    % Ensure no intermediate different type
    \+ (member(state(Tm, Mid), Timeline),
        Tm > T1, Tm < T2,
        Mid \= From, Mid \= To).

%% transformation_type(+C, +From, +To, +T1, +T2, -Label)
% Classifies the type of transformation with semantic label

transformation_type(C, rope, snare, T1, T2, capture) :-
    transformation_detected(C, rope, snare, T1, T2),
    check_capture_between(C, T1, T2).

transformation_type(C, rope, piton, T1, T2, obsolescence) :-
    transformation_detected(C, rope, piton, T1, T2),
    \+ check_capture_between(C, T1, T2).

transformation_type(C, scaffold, snare, T1, T2, calcification) :-
    transformation_detected(C, scaffold, snare, T1, T2),
    narrative_ontology:entity(C, scaffold),
    check_capture_between(C, T1, T2).

transformation_type(C, mountain, rope, T1, T2, discovery) :-
    transformation_detected(C, mountain, rope, T1, T2),
    narrative_ontology:constraint_claim(C, mountain).

transformation_type(C, mountain, snare, T1, T2, discovery) :-
    transformation_detected(C, mountain, snare, T1, T2),
    narrative_ontology:constraint_claim(C, mountain).

%% canonical_transformation(?C, ?From, ?To, -T1_earliest, -T2_latest, ?Label)
% Returns canonical (deduplicated) transformation
canonical_transformation(C, From, To, T1_earliest, T2_latest, Label) :-
    setof((T1, T2), transformation_type(C, From, To, T1, T2, Label), Pairs),
    findall(T1, member((T1, _), Pairs), T1s),
    findall(T2, member((_, T2), Pairs), T2s),
    min_list(T1s, T1_earliest),
    max_list(T2s, T2_latest).

%% check_capture_between(+C, +T1, +T2)
% Helper: detects if beneficiaries became concentrated
check_capture_between(C, T1, T2) :-
    narrative_ontology:measurement(_, C, extractiveness, T1, X1),
    narrative_ontology:measurement(_, C, extractiveness, T2, X2),
    X2 > X1,
    config:param(snare_epsilon_floor, Floor),
    X2 >= Floor.

%% predict_transformation(+C, +CurrentType, -LikelyFutureType)
% Predicts likely future transformation based on trajectory
% NOTE: Uses analytical context for prediction
%
% TODO (Issue #8 — Statistical Review):
%   CONCERN: N >= 2 is insufficient for trend detection. With only 2 data
%   points, any monotonic change is trivially "increasing" and there is no
%   way to distinguish signal from noise. Additionally, comparing only the
%   first and last elements ignores the trajectory shape (e.g. a V-shaped
%   recovery would be misclassified as increasing).
%   SUGGESTED FIX: Require N >= 5 and use a linear regression slope or
%   Mann-Kendall trend test instead of first-vs-last comparison. At minimum,
%   check that the trend is monotonic across all points, not just endpoints.
predict_transformation(C, rope, snare) :-
    findall(X, narrative_ontology:measurement(_, C, extractiveness, _, X), Xs),
    length(Xs, N), N >= 2,
    last(Xs, X_latest),
    X_latest > 0.5,
    config:param(snare_epsilon_floor, Floor),
    X_latest < Floor,
    Xs = [X_first|_],
    X_latest > X_first.

predict_transformation(C, rope, piton) :-
    findall(E, narrative_ontology:measurement(_, C, suppression_requirement, _, E), Es),
    length(Es, N), N >= 2,
    last(Es, E_latest),
    E_latest > 0.3,
    narrative_ontology:constraint_metric(C, extractiveness, X),
    X < 0.35.

predict_transformation(C, tangled_rope, snare) :-
    narrative_ontology:constraint_metric(C, extractiveness, X),
    X > 0.5.

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
    Strength > 0.85,
    
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
% NEW: Determines if Target critically depends on Source FROM CONTEXT
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
% Discovers hidden dependencies via temporal correlation
% NOTE: This is NOT indexed - coupling is structural fact
%
% TODO (Issue #8 — Statistical Review):
%   CONCERN 1: Sign-matching is NOT a valid correlation metric. This
%   counts co-directional gradient pairs but ignores magnitude entirely.
%   Two series with gradients [+0.001, +0.001] and [+0.999, +0.999]
%   would score identically to [+0.5, +0.5] and [+0.5, +0.5]. A proper
%   Pearson or Spearman correlation would capture both direction AND
%   magnitude/rank relationships.
%   CONCERN 2: calculate_coupling_strength/3 is UNBOUNDED. The base case
%   returns 1.0, and each matching pair adds 0.2, so N matching pairs
%   yields 1.0 + 0.2*N. For 10 matching pairs, Strength = 3.0 — not a
%   valid [0,1] metric. Downstream consumers (coupling_factor/2) compute
%   Factor = max(0.0, 1.0 - Strength), which clamps to 0.0 for any
%   Strength >= 1.0, making the coupling factor binary in practice.
%   CONCERN 3: dr_gradient_at/3 uses cut (!) after finding the first
%   T2 > T, but measurement ordering is not guaranteed by findall. This
%   may compute gradients from non-adjacent time points.
%   SUGGESTED FIX: Use msort on time points, compute proper Pearson
%   correlation, and normalize Strength to [0, 1].
infer_structural_coupling(C1, C2, Strength) :-
    C1 \= C2,
    findall(G1, dr_gradient_at(C1, _, G1), Gs1),
    findall(G2, dr_gradient_at(C2, _, G2), Gs2),
    length(Gs1, L), L > 1, length(Gs2, L),
    calculate_coupling_strength(Gs1, Gs2, Strength).

dr_gradient_at(C, T, Grad) :-
    narrative_ontology:measurement(_, C, extractiveness, T, X1),
    narrative_ontology:measurement(_, C, extractiveness, T2, X2),
    T2 > T, !,
    Grad is X2 - X1.

calculate_coupling_strength([], [], 1.0).
calculate_coupling_strength([H1|T1], [H2|T2], S) :-
    ( (H1 > 0, H2 > 0) ; (H1 < 0, H2 < 0) ; (H1 == 0, H2 == 0) ),
    calculate_coupling_strength(T1, T2, SubS),
    S is 0.2 + SubS.
calculate_coupling_strength([_|T1], [_|T2], S) :-
    calculate_coupling_strength(T1, T2, S).

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
   CROSS-CONTEXT DEPENDENCY ANALYSIS (NEW)
   Critical for detecting "institutional Mountain collapse"
   ================================================================ */

%% check_all_contexts(+Constraint, -MultiContextReport)
% NEW: Evaluates cutting a constraint across multiple standard contexts
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

% ============================================================================
% NEW: INDEXICAL PERSPECTIVE AUDIT
% ============================================================================
% Checks if two agents experience a structural conflict (Risk Gap)
% Implements Section IV-B
detect_perspectival_risk(ConstraintID, Agent1, Agent2, RiskLabel) :-
    % 1. Get the base extraction score for the constraint
    constraint_data(ConstraintID, Data),
    member(base_extractiveness(X_base), Data),
    
    % 2. Calculate what each agent "sees" based on their power index
    % Uses the pi scaling function from drl_audit_core
    agent_index(Agent1, context(Power1, _, _, _)),
    agent_index(Agent2, context(Power2, _, _, _)),
    
    drl_audit_core:effective_extraction(X_base, Power1, Chi1),
    drl_audit_core:effective_extraction(X_base, Power2, Chi2),
    
    % 3. Determine the structural type for each agent
    drl_audit_core:structural_signature(Chi1, Data, Type1),
    drl_audit_core:structural_signature(Chi2, Data, Type2),
    
    % 4. Match against the Risk Table
    drl_audit_core:omega_risk(Type1, Type2, RiskLabel, _).

% possibly(C) is true if the constraint is a Rope (changeable/contingent)
possibly(C) :- 
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, rope).

% necessarily(C) is true if the constraint is a Mountain (fixed/required)
necessarily(C) :- 
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, mountain).

/* ================================================================
   STAGE 5: BOLTZMANN-AWARE ANALYSIS (v5.0)

   Based on Tamuz & Sandomirskiy (2025), "On the origin of the
   Boltzmann distribution."

   Provides:
   1. Coupling-aware reformability score
   2. Boltzmann-invariant mountain check
   3. Coupling-aware scaffold need assessment

   These predicates integrate the Boltzmann compliance engine
   (structural_signatures.pl) with the modal logic layer,
   enabling more precise intervention planning.
   ================================================================ */

/* ----------------------------------------------------------------
   COUPLING-AWARE REFORMABILITY SCORE
   ----------------------------------------------------------------
   Extends the existing Prob_Reform_Success formula with coupling
   topology awareness.

   Reformability(C) = f(Separability, CouplingTopology, ExcessExtraction)

   Where:
     Separability = can coordination be separated from extraction?
     CouplingTopology = how entangled are the index dimensions?
     ExcessExtraction = how much extraction is above the Boltzmann floor?

   Strong coupling → low reformability (can't separate components)
   Weak coupling → moderate reformability
   Independent dimensions → high reformability (clean separation possible)

   Score range: [0.0, 1.0] where 1.0 = fully reformable
   ---------------------------------------------------------------- */

%% reformability_score(+Constraint, +Context, -Score)
%  PRIMARY API: Computes coupling-aware reformability from context.
reformability_score(C, Context, Score) :-
    constraint_indexing:valid_context(Context),

    % Factor 1: Separability — does the constraint have both
    % coordination AND extraction that can be disentangled?
    separability_factor(C, SepFactor),

    % Factor 2: Coupling topology — how entangled are the dimensions?
    coupling_factor(C, CouplingFactor),

    % Factor 3: Excess extraction — how much is above Boltzmann floor?
    excess_extraction_factor(C, ExcessFactor),

    % Weighted combination (coupling topology most important for reform)
    % TODO (Issue #8 — Statistical Review):
    %   CONCERN: Weights 0.30/0.40/0.30 appear to be heuristic estimates
    %   without empirical calibration or sensitivity analysis. The 40%
    %   weight on coupling is justified by the comment "most important for
    %   reform" but this claim is not backed by data. Consider deriving
    %   weights from labeled reformability outcomes via logistic regression,
    %   or at minimum documenting the theoretical rationale and performing
    %   sensitivity analysis across the test corpus.
    Score is min(1.0, max(0.0,
        0.30 * SepFactor +
        0.40 * CouplingFactor +
        0.30 * ExcessFactor
    )).

%% reformability_score(+Constraint, -Score)
%  BACKWARD COMPAT: Uses analytical context.
reformability_score(C, Score) :-
    constraint_indexing:default_context(Ctx),
    reformability_score(C, Ctx, Score).

%% separability_factor(+C, -Factor)
%  Measures whether coordination and extraction can be separated.
%  High factor = separable (good for reform).
separability_factor(C, 0.9) :-
    narrative_ontology:has_coordination_function(C),
    narrative_ontology:has_asymmetric_extraction(C),
    % Has both coordination and extraction — but are they in
    % different structural components?
    narrative_ontology:constraint_beneficiary(C, _),
    narrative_ontology:constraint_victim(C, _), !.
separability_factor(C, 0.3) :-
    % Only extraction, no coordination — nothing to preserve
    \+ narrative_ontology:has_coordination_function(C),
    narrative_ontology:has_asymmetric_extraction(C), !.
separability_factor(C, 1.0) :-
    % Only coordination, no extraction — already reformed
    narrative_ontology:has_coordination_function(C),
    \+ narrative_ontology:has_asymmetric_extraction(C), !.
separability_factor(_, 0.5).  % Unknown structure

%% coupling_factor(+C, -Factor)
%  Converts coupling topology into reformability factor.
%  Independent = easy to reform (high factor).
%  Strongly coupled = hard to reform (low factor).
coupling_factor(C, Factor) :-
    (   structural_signatures:cross_index_coupling(C, CouplingScore)
    ->  % Invert: low coupling → high reformability
        Factor is max(0.0, 1.0 - CouplingScore)
    ;   Factor = 0.5  % Unknown coupling → moderate assumption
    ).

%% excess_extraction_factor(+C, -Factor)
%  Higher excess extraction → lower reformability (more entrenched).
%  But also → higher MOTIVATION to reform.
%  We model this as: moderate excess = highest reformability
%  (enough motivation, not too entrenched).
% TODO (Issue #8 — Statistical Review):
%   CONCERN: The threshold buckets (0.10, 0.30, 0.50) and their output
%   values (0.8, 1.0, 0.6, 0.3) encode a non-monotonic "inverted-U"
%   hypothesis (moderate excess = highest reformability) as a step
%   function with unjustified breakpoints. The discontinuities at
%   boundaries (e.g. Excess=0.10 yields 0.8 but Excess=0.11 yields 1.0)
%   create sensitivity to small measurement changes. Consider replacing
%   with a smooth function, e.g. a Gaussian centered on the hypothesized
%   sweet spot: Factor = exp(-((Excess - 0.20)^2) / (2 * 0.15^2)).
%   The specific bucket boundaries should be validated against empirical
%   reform outcomes if available.
excess_extraction_factor(C, Factor) :-
    (   structural_signatures:excess_extraction(C, Excess)
    ->  (   Excess =< 0.10
        ->  Factor = 0.8   % Low excess: easy reform, low urgency
        ;   Excess =< 0.30
        ->  Factor = 1.0   % Moderate excess: sweet spot
        ;   Excess =< 0.50
        ->  Factor = 0.6   % High excess: entrenched interests
        ;   Factor = 0.3   % Extreme excess: deeply entrenched
        )
    ;   Factor = 0.5  % No data
    ).

/* ----------------------------------------------------------------
   BOLTZMANN-INVARIANT MOUNTAIN CHECK
   ----------------------------------------------------------------
   Delegates to structural_signatures:boltzmann_invariant_mountain/2
   but adds modal context: checks if the Mountain classification
   is also consistent across counterfactual worlds.
   ---------------------------------------------------------------- */

%% boltzmann_invariant_check(+Constraint, -Result)
%  Full Boltzmann invariance check for Mountains.
%  Combines structural signature test with modal consistency.
%
%  Result = boltzmann_check(InvarianceResult, ModalConsistency)
boltzmann_invariant_check(C, boltzmann_check(InvResult, ModalConsistency)) :-
    % Structural invariance from signatures module
    structural_signatures:boltzmann_invariant_mountain(C, InvResult),

    % Modal consistency: is it a Mountain from ALL standard contexts?
    findall(
        context_type(Ctx, Type),
        (   standard_context(Ctx),
            drl_core:dr_type(C, Ctx, Type)
        ),
        ContextTypes
    ),
    findall(Type, member(context_type(_, Type), ContextTypes), Types),
    sort(Types, UniqueTypes),
    (   UniqueTypes = [mountain]
    ->  ModalConsistency = consistent(all_mountain)
    ;   member(mountain, UniqueTypes)
    ->  ModalConsistency = partial(UniqueTypes)
    ;   ModalConsistency = inconsistent(UniqueTypes)
    ).

/* ----------------------------------------------------------------
   COUPLING-AWARE SCAFFOLD NEED ASSESSMENT
   ----------------------------------------------------------------
   Extends assess_scaffold_need/3 with coupling awareness.
   A strongly-coupled constraint is HARDER to scaffold because
   cutting any part risks cascading through coupled dimensions.
   ---------------------------------------------------------------- */

%% coupling_aware_scaffold_need(+Constraint, +Context, -Assessment)
%  Enhanced scaffold assessment that considers coupling topology.
%
%  Assessment is one of:
%    no_scaffold_needed
%    scaffold_required(Urgency)     — Urgency in {low, moderate, high}
%    scaffold_present
%    reform_preferred(Score)         — Reformability score suggests reform over cut

coupling_aware_scaffold_need(C, Context, Assessment) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C, Context, Type),

    % Only relevant for cuttable/reformable types
    (   member(Type, [snare, tangled_rope, piton, rope])
    ->  % Get base scaffold assessment
        assess_scaffold_need(C, Context, BaseAssessment),

        % Get coupling topology
        (   structural_signatures:cross_index_coupling(C, CouplingScore)
        ->  true
        ;   CouplingScore = 0.0
        ),

        % Get reformability
        reformability_score(C, Context, ReformScore),

        % Decision logic
        (   Type = tangled_rope, ReformScore > 0.60
        ->  Assessment = reform_preferred(ReformScore)
        ;   BaseAssessment = scaffold_required,
            CouplingScore > 0.30
        ->  Assessment = scaffold_required(high)
        ;   BaseAssessment = scaffold_required
        ->  Assessment = scaffold_required(moderate)
        ;   BaseAssessment = scaffold_present
        ->  Assessment = scaffold_present
        ;   Assessment = no_scaffold_needed
        )
    ;   Assessment = no_scaffold_needed
    ).

/* ================================================================
   STAGE 6: PURITY-AWARE REFORM RECOMMENDATIONS (v5.1)

   Now that purity is a scalar, we can give the action layer
   quantitative reform targets: "Reform until purity ≥ 0.85."

   This extends the existing reformability_score and reform_pressure
   with:
   1. purity_reform_target/2 — minimum acceptable purity after reform
   2. purity_reform_recommendation/2 — structured actionable advice

   The target formula:
     Target = max(current_purity, 0.85)
   Meaning:
     - If purity is 0.40 → reform to at least 0.85 ("sound")
     - If purity is 0.92 → maintain at 0.92 (don't regress)
     - 0.85 is the "sound" zone threshold from the purity scale

   The recommendation combines target, gap, reformability, pressure,
   and specific purity deficits into a single structured term for
   the decision/action layer.
   ================================================================ */

%% purity_reform_target(+Constraint, -Target)
%  Returns the minimum acceptable purity score after reform.
%  Target is always at least 0.85 ("sound" zone), or higher if the
%  constraint's current purity already exceeds 0.85.
purity_reform_target(C, Target) :-
    structural_signatures:purity_score(C, Purity),
    Purity >= 0.0, !,
    Target is max(Purity, 0.85).
purity_reform_target(_, 0.85).  % Fallback if purity inconclusive

%% purity_reform_recommendation(+Constraint, -Recommendation)
%  Generates a structured reform recommendation combining purity
%  target, reformability, pressure, and specific deficits.
%
%  Recommendation = reform_recommendation(
%      CurrentPurity,      % Current purity score [0,1]
%      TargetPurity,       % Target purity score [0.85, 1.0]
%      PurityGap,          % How much improvement needed
%      Reformability,      % Can this be reformed? [0,1]
%      ReformPressure,     % How urgent is reform? [0, 99]
%      Deficits,           % List of specific purity deficits
%      Urgency             % none | low | moderate | high | critical
%  )

purity_reform_recommendation(C, reform_recommendation(
        CurrentPurity, TargetPurity, PurityGap,
        Reformability, Pressure, Deficits, Urgency)) :-
    % Get current purity
    structural_signatures:purity_score(C, CurrentPurity),
    CurrentPurity >= 0.0,

    % Compute target and gap
    purity_reform_target(C, TargetPurity),
    PurityGap is max(0.0, TargetPurity - CurrentPurity),

    % Get reformability score
    (   reformability_score(C, Reformability)
    ->  true
    ;   Reformability = 0.5
    ),

    % Get reform pressure from lifecycle module
    (   drl_lifecycle:reform_pressure(C, Pressure)
    ->  true
    ;   Pressure = 0.0
    ),

    % Identify specific purity deficits
    identify_purity_deficits(C, Deficits),

    % Compute urgency from gap, pressure, and reformability
    compute_reform_urgency(PurityGap, Pressure, Reformability, Urgency).

%% identify_purity_deficits(+C, -Deficits)
%  Returns list of specific purity components scoring below 0.85.
%  Each deficit identifies which structural aspect needs improvement.
identify_purity_deficits(C, Deficits) :-
    findall(Deficit, purity_deficit(C, Deficit), Deficits).

% Factorization deficit: coupling across index dimensions
purity_deficit(C, factorization_deficit(Score)) :-
    structural_signatures:factorization_subscore(C, Score),
    Score < 0.85.

% Scope invariance deficit: classification changes with scope
purity_deficit(C, scope_invariance_deficit(Score)) :-
    structural_signatures:scope_invariance_subscore(C, Score),
    Score < 0.85.

% Coupling cleanliness deficit: nonsensical coupling present
purity_deficit(C, coupling_cleanliness_deficit(Score)) :-
    structural_signatures:coupling_cleanliness_subscore(C, Score),
    Score < 0.85.

% Excess extraction deficit: extraction above Boltzmann floor
purity_deficit(C, excess_extraction_deficit(Score)) :-
    structural_signatures:excess_extraction_subscore(C, Score),
    Score < 0.85.

%% compute_reform_urgency(+Gap, +Pressure, +Reformability, -Urgency)
%  Maps quantitative inputs to categorical urgency level.
%  Critical: large gap AND high pressure (active harm, reform overdue)
%  High: large gap OR high pressure (significant but not crisis)
%  Moderate: meaningful gap with reasonable reformability
%  Low: small gap, system still in "sound" territory
%  None: no gap (purity at or above target)
%
% TODO (Issue #8 — Statistical Review):
%   CONCERN 1: All thresholds (Gap: 0.05/0.15/0.30/0.40; Pressure: 1.5/2.0)
%   are hardcoded without documented justification. These should either be
%   derived from config:param/2 for tuning, or backed by calibration data.
%   CONCERN 2: The "high" clause uses disjunction (Gap > 0.30 OR Pressure > 1.5)
%   while "critical" uses conjunction (AND). This means a Gap of 0.31 with
%   zero Pressure classifies as "high" — the same level as Gap=0.41 with
%   Pressure=1.9. Consider whether the disjunction is intentional or if
%   "high" should require both conditions above separate thresholds.
%   CONCERN 3: Reformability is only used in the "moderate" clause and
%   completely ignored for critical/high/low/none. A deeply entrenched
%   constraint (Reformability=0.1) with Gap=0.45, Pressure=2.1 gets
%   "critical" urgency despite being nearly impossible to reform.
compute_reform_urgency(Gap, Pressure, _, critical) :-
    Gap > 0.40, Pressure > 2.0, !.
compute_reform_urgency(Gap, Pressure, _, high) :-
    (Gap > 0.30 ; Pressure > 1.5), !.
compute_reform_urgency(Gap, _, Reformability, moderate) :-
    Gap > 0.15, Reformability > 0.50, !.
compute_reform_urgency(Gap, _, _, low) :-
    Gap > 0.05, !.
compute_reform_urgency(_, _, _, none).

/* ================================================================
   STAGE 7: PURITY-QUALIFIED ACTION ALGEBRA (v5.1)

   The action algebra (dr_action/3 in drl_core.pl) is a simple
   6-way type→action switch with no awareness of structural purity.
   This creates a gap: the system can detect that a Rope's
   coordination has been contaminated (purity=0.35) but still
   recommends 'maintain'.

   Stage 7 wraps dr_action/3 with a purity-qualified layer that
   upgrades/downgrades actions based on purity thresholds.

   SHADOW MODE: drl_core.pl is NOT modified. These predicates
   are additive — callers opt in by using purity_qualified_action
   instead of dr_action.

   Four predicates:
   A. purity_qualified_action/4  — Core action qualifier
   B. purity_adjusted_energy/4   — Energy cost with purity multiplier
   C. action_composition_gate/3  — Purity prerequisites for composites
   D. purity_scaffold_urgency/4  — Extends scaffold assessment
   ================================================================ */

/* ----------------------------------------------------------------
   A. PURITY-QUALIFIED ACTION — Core Action Qualifier
   ----------------------------------------------------------------
   Returns qualified_action(BaseAction, Qualifier, Priority) where:
     Qualifier ∈ {stable, monitor, escalate_reform, escalate_cut,
                  accelerate_sunset, degraded, inconclusive}
     Priority  ∈ {none, low, moderate, high, critical}

   When purity = -1.0 (insufficient data): returns
     qualified_action(BaseAction, inconclusive, none)
   — no qualification without evidence.
   ---------------------------------------------------------------- */

%% purity_qualified_action(+C, +Context, -QAction, -Rationale)
%  PRIMARY API: Qualifies the base action with purity awareness.
%  Rationale is a human-readable atom explaining the qualification.
purity_qualified_action(C, Context, QAction, Rationale) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_action(C, Context, BaseAction),
    structural_signatures:purity_score(C, Purity),
    qualify_action(BaseAction, Purity, C, QAction, Rationale).

%% purity_qualified_action(+C, -QAction, -Rationale)
%  BACKWARD COMPAT: Uses analytical context.
purity_qualified_action(C, QAction, Rationale) :-
    constraint_indexing:default_context(Ctx),
    purity_qualified_action(C, Ctx, QAction, Rationale).

%% qualify_action(+BaseAction, +Purity, +C, -QAction, -Rationale)
%  Core qualification logic. Dispatches by base action and purity zone.

% Inconclusive purity — no qualification without evidence
qualify_action(BaseAction, Purity, _C, qualified_action(BaseAction, inconclusive, none),
              insufficient_purity_data) :-
    Purity < 0.0, !.

% accept (Mountain) — natural laws need no purity gate
qualify_action(accept, _Purity, _C, qualified_action(accept, stable, none),
              natural_law_no_gate) :- !.

% maintain (Rope) — purity determines coordination health
qualify_action(maintain, Purity, _C, qualified_action(maintain, stable, low),
              sound_coordination) :-
    config:param(purity_action_sound_floor, SoundFloor),
    Purity >= SoundFloor, !.
qualify_action(maintain, Purity, _C, qualified_action(maintain, monitor, moderate),
              purity_declining) :-
    config:param(purity_action_escalation_floor, EscFloor),
    Purity >= EscFloor, !.
qualify_action(maintain, Purity, _C, qualified_action(maintain, escalate_reform, high),
              coordination_contaminated) :-
    config:param(purity_action_degraded_floor, DegFloor),
    Purity >= DegFloor, !.
qualify_action(maintain, _Purity, _C, qualified_action(maintain, escalate_cut, critical),
              degraded_beyond_coordination_value) :- !.

% reform (Tangled Rope) — purity determines reform feasibility
qualify_action(reform, Purity, _C, qualified_action(reform, stable, low),
              careful_reform_sufficient) :-
    config:param(purity_action_sound_floor, SoundFloor),
    Purity >= SoundFloor, !.
qualify_action(reform, Purity, _C, qualified_action(reform, stable, moderate),
              standard_surgical_reform) :-
    config:param(purity_action_escalation_floor, EscFloor),
    Purity >= EscFloor, !.
qualify_action(reform, Purity, _C, qualified_action(reform, stable, high),
              aggressive_reform_needed) :-
    config:param(purity_action_degraded_floor, DegFloor),
    Purity >= DegFloor, !.
qualify_action(reform, _Purity, _C, qualified_action(reform, escalate_cut, critical),
              unreformable_cut) :- !.

% cut (Snare) — purity informs energy only; cut unchanged
qualify_action(cut, Purity, _C, qualified_action(cut, stable, Priority),
              cut_purity_informs_energy) :-
    purity_to_cut_priority(Purity, Priority), !.

% monitor_sunset (Scaffold) — purity determines scaffold health
qualify_action(monitor_sunset, Purity, _C, qualified_action(monitor_sunset, stable, low),
              scaffold_healthy) :-
    config:param(purity_action_sound_floor, SoundFloor),
    Purity >= SoundFloor, !.
qualify_action(monitor_sunset, Purity, _C, qualified_action(monitor_sunset, monitor, moderate),
              scaffold_purity_declining) :-
    config:param(purity_scaffold_health_gate, HealthGate),
    Purity >= HealthGate, !.
qualify_action(monitor_sunset, _Purity, _C, qualified_action(monitor_sunset, accelerate_sunset, high),
              dissolve_sooner) :- !.

% bypass (Piton) — terminal state
qualify_action(bypass, _Purity, _C, qualified_action(bypass, degraded, none),
              terminal_state) :- !.

% investigate — passthrough
qualify_action(investigate, _Purity, _C, qualified_action(investigate, stable, none),
              passthrough) :- !.

% Fallback
qualify_action(BaseAction, _Purity, _C, qualified_action(BaseAction, stable, none),
              no_qualification_rule).

%% purity_to_cut_priority(+Purity, -Priority)
%  Maps purity to priority for cut actions (energy-only impact).
purity_to_cut_priority(Purity, low) :-
    config:param(purity_action_sound_floor, SoundFloor),
    Purity >= SoundFloor, !.
purity_to_cut_priority(Purity, moderate) :-
    config:param(purity_action_escalation_floor, EscFloor),
    Purity >= EscFloor, !.
purity_to_cut_priority(Purity, high) :-
    config:param(purity_action_degraded_floor, DegFloor),
    Purity >= DegFloor, !.
purity_to_cut_priority(_, critical).

/* ----------------------------------------------------------------
   B. PURITY-ADJUSTED ENERGY — Energy Cost with Purity Multiplier
   ----------------------------------------------------------------
   Returns energy_cost(BaseComplexity, Multiplier, AdjustedComplexity).

   Multiplier formulas per action:
     reform:        M = min(MaxMult, 1.0 + max(0, 0.70 - Purity) × 3.0)
     cut:           M = min(MaxMult, 1.0 + max(0, 0.50 - Purity) × 1.5)
     maintain:      M = max(0.8, 1.0 - max(0, Purity - 0.70) × 0.5)
     accept/bypass/monitor_sunset: M = 1.0

   Base complexities:
     accept=0, maintain=1, reform=4, cut=2, monitor_sunset=2, bypass=1
   ---------------------------------------------------------------- */

%% purity_adjusted_energy(+C, +Context, +BaseAction, -EnergyCost)
%  PRIMARY API: Computes purity-adjusted energy cost for an action.
purity_adjusted_energy(C, Context, BaseAction, energy_cost(BaseCost, Mult, Adjusted)) :-
    constraint_indexing:valid_context(Context),
    base_action_complexity(BaseAction, BaseCost),
    structural_signatures:purity_score(C, Purity),
    config:param(purity_energy_max_multiplier, MaxMult),
    energy_multiplier(BaseAction, Purity, MaxMult, Mult),
    Adjusted is BaseCost * Mult.

%% base_action_complexity(+Action, -Cost)
base_action_complexity(accept,         0).
base_action_complexity(maintain,       1).
base_action_complexity(reform,         4).
base_action_complexity(cut,            2).
base_action_complexity(monitor_sunset, 2).
base_action_complexity(bypass,         1).
base_action_complexity(investigate,    1).

%% energy_multiplier(+Action, +Purity, +MaxMult, -Mult)
%  Computes the energy multiplier based on action type and purity.

% Inconclusive purity — use baseline multiplier
energy_multiplier(_, Purity, _, 1.0) :-
    Purity < 0.0, !.

% Reform: harder at lower purity (more entangled)
energy_multiplier(reform, Purity, MaxMult, Mult) :-
    RawMult is 1.0 + max(0.0, 0.70 - Purity) * 3.0,
    Mult is min(MaxMult, RawMult), !.

% Cut: harder at lower purity (more collateral damage)
energy_multiplier(cut, Purity, MaxMult, Mult) :-
    RawMult is 1.0 + max(0.0, 0.50 - Purity) * 1.5,
    Mult is min(MaxMult, RawMult), !.

% Maintain: pristine coordination is cheaper to maintain
energy_multiplier(maintain, Purity, _, Mult) :-
    Mult is max(0.8, 1.0 - max(0.0, Purity - 0.70) * 0.5), !.

% All others: no purity effect
energy_multiplier(_, _, _, 1.0).

/* ----------------------------------------------------------------
   C. ACTION COMPOSITION GATE — Purity Prerequisites
   ----------------------------------------------------------------
   Returns gate(Pass, Reason) where Pass ∈ {pass, fail}.

   Gates:
     surgical_reform:        Purity ≥ 0.30 AND Reformability > 0.50
     safe_transition:        Scaffold purity ≥ 0.50
     efficient_coordination: Rope purity ≥ 0.50

   For unknown composite actions or purity = -1.0 →
     gate(pass, no_gate_defined)
   ---------------------------------------------------------------- */

%% action_composition_gate(+C, +CompositeAction, -GateResult)
%  Checks purity prerequisites for composite actions.

action_composition_gate(C, surgical_reform, gate(Pass, Reason)) :-
    structural_signatures:purity_score(C, Purity),
    (   Purity < 0.0
    ->  Pass = pass, Reason = no_gate_defined
    ;   config:param(purity_surgical_reform_gate, MinPurity),
        (   Purity >= MinPurity
        ->  (   reformability_score(C, ReformScore),
                ReformScore > 0.50
            ->  Pass = pass, Reason = purity_and_reformability_sufficient
            ;   Pass = fail, Reason = reformability_too_low
            )
        ;   Pass = fail, Reason = purity_below_surgical_threshold
        )
    ), !.

action_composition_gate(C, safe_transition, gate(Pass, Reason)) :-
    structural_signatures:purity_score(C, Purity),
    (   Purity < 0.0
    ->  Pass = pass, Reason = no_gate_defined
    ;   config:param(purity_scaffold_health_gate, MinPurity),
        (   Purity >= MinPurity
        ->  Pass = pass, Reason = scaffold_structurally_sound
        ;   Pass = fail, Reason = scaffold_purity_insufficient
        )
    ), !.

action_composition_gate(C, efficient_coordination, gate(Pass, Reason)) :-
    structural_signatures:purity_score(C, Purity),
    (   Purity < 0.0
    ->  Pass = pass, Reason = no_gate_defined
    ;   config:param(purity_action_escalation_floor, MinPurity),
        (   Purity >= MinPurity
        ->  Pass = pass, Reason = coordination_structurally_sound
        ;   Pass = fail, Reason = coordination_purity_insufficient
        )
    ), !.

% Unknown composite action or fallback
action_composition_gate(_, _, gate(pass, no_gate_defined)).

/* ----------------------------------------------------------------
   D. PURITY SCAFFOLD URGENCY — Extends Scaffold Assessment
   ----------------------------------------------------------------
   Returns urgency ∈ {none, low, moderate, high, critical}
   with explanatory factors.

   Combines coupling_aware_scaffold_need/3 output with purity:
     - Base scaffold_required(high) + purity < 0.30 → critical
     - Base scaffold_required(_) + purity drift → escalate one level
     - Base reform_preferred(Score) + purity < 0.50 →
       escalate to scaffold_required(high) (reform impractical)
   ---------------------------------------------------------------- */

%% purity_scaffold_urgency(+C, +Context, -Urgency, -Factors)
%  PRIMARY API: Purity-enhanced scaffold urgency assessment.
purity_scaffold_urgency(C, Context, Urgency, Factors) :-
    constraint_indexing:valid_context(Context),
    (   coupling_aware_scaffold_need(C, Context, BaseAssessment)
    ->  true
    ;   BaseAssessment = no_scaffold_needed
    ),
    structural_signatures:purity_score(C, Purity),
    has_purity_drift(C, DriftDetected),
    compute_scaffold_urgency(BaseAssessment, Purity, DriftDetected, Urgency, Factors).

%% has_purity_drift(+C, -Detected)
%  Checks if purity drift is detected for the constraint.
%  Fails gracefully if drl_lifecycle is not loaded.
has_purity_drift(C, true) :-
    catch(drl_lifecycle:detect_purity_drift(C), _, fail), !.
has_purity_drift(_, false).

%% compute_scaffold_urgency(+BaseAssessment, +Purity, +DriftDetected, -Urgency, -Factors)

% Inconclusive purity — pass through base assessment
compute_scaffold_urgency(BaseAssessment, Purity, _, BaseUrgency, [base(BaseAssessment), purity_inconclusive]) :-
    Purity < 0.0, !,
    base_assessment_urgency(BaseAssessment, BaseUrgency).

% scaffold_required(high) + purity < 0.30 → critical
compute_scaffold_urgency(scaffold_required(high), Purity, _, critical,
                         [base(scaffold_required(high)), purity(Purity), escalated(degraded_purity)]) :-
    config:param(purity_action_degraded_floor, DegFloor),
    Purity < DegFloor, !.

% scaffold_required(_) + drift → escalate one level
compute_scaffold_urgency(scaffold_required(BaseLevel), Purity, true, Escalated,
                         [base(scaffold_required(BaseLevel)), purity(Purity), escalated(purity_drift)]) :-
    escalate_urgency(BaseLevel, Escalated), !.

% reform_preferred(Score) + purity < 0.50 → scaffold_required(high)
compute_scaffold_urgency(reform_preferred(_Score), Purity, _, high,
                         [base(reform_preferred), purity(Purity), escalated(reform_impractical_at_purity)]) :-
    config:param(purity_action_escalation_floor, EscFloor),
    Purity < EscFloor, !.

% Default: derive urgency from base assessment without escalation
compute_scaffold_urgency(BaseAssessment, Purity, _, Urgency,
                         [base(BaseAssessment), purity(Purity)]) :-
    base_assessment_urgency(BaseAssessment, Urgency).

%% base_assessment_urgency(+Assessment, -Urgency)
%  Maps base coupling_aware_scaffold_need assessment to urgency.
base_assessment_urgency(scaffold_required(high), high).
base_assessment_urgency(scaffold_required(moderate), moderate).
base_assessment_urgency(scaffold_required(low), low).
base_assessment_urgency(scaffold_required(_), moderate).  % catch-all for scaffold_required
base_assessment_urgency(scaffold_present, low).
base_assessment_urgency(reform_preferred(_), moderate).
base_assessment_urgency(no_scaffold_needed, none).
base_assessment_urgency(_, none).

%% escalate_urgency(+Base, -Escalated)
%  Escalates urgency by one level.
escalate_urgency(low, moderate).
escalate_urgency(moderate, high).
escalate_urgency(high, critical).
escalate_urgency(critical, critical).  % Cannot escalate beyond critical
escalate_urgency(none, low).

/* ================================================================
   STAGE 8: PURITY PROPAGATION THROUGH CONSTRAINT NETWORKS (v5.2)

   Constraints don't exist in isolation. When a Snare's purity is 0.31,
   that contamination should create pressure on connected Ropes that
   naively show 0.88 purity. This stage implements a one-hop
   contamination propagation engine.

   Key design decisions:
   - One-hop only: no transitive propagation (avoids convergence complexity)
   - Contamination only, no uplift: purity degrades spontaneously,
     requires active intervention to improve (thermodynamic)
   - Per-edge cap: purity_contamination_cap prevents one bad actor
     from destroying the network
   - Context-indexed: same constraint may be Mountain (immune) from one
     context and Snare (emitting contamination) from another

   Graceful degradation: when only one constraint is loaded,
   effective_purity equals intrinsic purity and network_qualified_action
   delegates to purity_qualified_action.
   ================================================================ */

/* ----------------------------------------------------------------
   A. NETWORK DISCOVERY
   Discovers constraint neighbors from three edge sources:
   1. Explicit: affects_constraint/2
   2. Inferred: infer_structural_coupling/3
   3. Shared agent: constraint_beneficiary/2, constraint_victim/2
   ---------------------------------------------------------------- */

%% constraint_neighbors(+C, +Context, -Neighbors)
%  PRIMARY API: Discovers all neighbors of C.
%  Returns [neighbor(Other, Strength, Source), ...] where
%  Source ∈ {explicit, inferred_coupling, shared_beneficiary, shared_victim}
constraint_neighbors(C, Context, Neighbors) :-
    constraint_indexing:valid_context(Context),
    findall(neighbor(Other, 1.0, explicit),
            ( narrative_ontology:affects_constraint(C, Other), Other \= C ),
            ExplicitOut),
    findall(neighbor(Other, 1.0, explicit),
            ( narrative_ontology:affects_constraint(Other, C), Other \= C ),
            ExplicitIn),
    config:param(network_coupling_threshold, CoupThresh),
    findall(neighbor(Other, Strength, inferred_coupling),
            ( infer_structural_coupling(C, Other, Strength),
              Strength >= CoupThresh ),
            Inferred),
    findall(neighbor(Other, AgentStrength, LinkType),
            ( shared_agent_link(C, Other, LinkType, _),
              shared_agent_edge_strength(C, Other, LinkType, AgentStrength) ),
            SharedRaw),
    append([ExplicitOut, ExplicitIn, Inferred, SharedRaw], AllRaw),
    deduplicate_neighbors(AllRaw, Neighbors).

%% constraint_neighbors(+C, -Neighbors)
%  BACKWARD COMPAT: Uses analytical context.
constraint_neighbors(C, Neighbors) :-
    constraint_indexing:default_context(Ctx),
    constraint_neighbors(C, Ctx, Neighbors).

%% shared_agent_link(+C1, -C2, -LinkType, -Agent)
%  Discovers constraints linked through shared agent classes.
shared_agent_link(C1, C2, shared_beneficiary, Agent) :-
    narrative_ontology:constraint_beneficiary(C1, Agent),
    narrative_ontology:constraint_beneficiary(C2, Agent),
    C1 \= C2.
shared_agent_link(C1, C2, shared_victim, Agent) :-
    narrative_ontology:constraint_victim(C1, Agent),
    narrative_ontology:constraint_victim(C2, Agent),
    C1 \= C2.

%% shared_agent_edge_strength(+C1, +C2, +LinkType, -Strength)
%  Edge strength = 0.3 × N (capped at 1.0) where N = shared links.
shared_agent_edge_strength(C1, C2, LinkType, Strength) :-
    findall(A, shared_agent_link(C1, C2, LinkType, A), Agents),
    sort(Agents, Unique),
    length(Unique, N),
    Strength is min(1.0, 0.3 * N).

%% deduplicate_neighbors(+Raw, -Deduped)
%  Keeps the strongest edge per neighbor. Removes duplicates.
deduplicate_neighbors(Raw, Deduped) :-
    msort(Raw, Sorted),
    dedup_sorted(Sorted, Deduped).

dedup_sorted([], []).
dedup_sorted([neighbor(C, S1, Src1), neighbor(C, S2, _Src2) | Rest], Out) :-
    !,
    MaxS is max(S1, S2),
    dedup_sorted([neighbor(C, MaxS, Src1) | Rest], Out).
dedup_sorted([H|T], [H|Out]) :-
    dedup_sorted(T, Out).

/* ----------------------------------------------------------------
   B. PURITY PROPAGATION
   Core algorithm: effective_purity = max(0, intrinsic - contamination × immunity)
   ---------------------------------------------------------------- */

%% type_contamination_strength(+Type, -Strength)
%  How much contamination a type emits as a source.
type_contamination_strength(snare,        1.0) :- !.
type_contamination_strength(piton,        0.8) :- !.
type_contamination_strength(tangled_rope, 0.5) :- !.
type_contamination_strength(scaffold,     0.2) :- !.
type_contamination_strength(rope,         0.1) :- !.
type_contamination_strength(mountain,     0.0) :- !.
type_contamination_strength(_,            0.0).

%% type_immunity(+Type, -Immunity)
%  Susceptibility factor for the target type.
%  0.0 = immune (Mountain), 1.0 = fully susceptible (Rope).
type_immunity(mountain,      0.0) :- !.
type_immunity(piton,         0.3) :- !.
type_immunity(snare,         0.5) :- !.
type_immunity(tangled_rope,  0.8) :- !.
type_immunity(scaffold,      0.9) :- !.
type_immunity(rope,          1.0) :- !.
type_immunity(_,             0.5).

%% effective_purity(+C, +Context, -EffPurity, -Components)
%  PRIMARY API: Computes effective purity accounting for neighbor contamination.
%  Components = purity_components(IntrinsicPurity, TotalContamination, Detail)
%  where Detail = no_neighbors | contamination_detail(NeighborList)
effective_purity(C, Context, EffPurity, purity_components(Intrinsic, TotalContam, Detail)) :-
    constraint_indexing:valid_context(Context),
    structural_signatures:purity_score(C, Intrinsic),
    constraint_neighbors(C, Context, Neighbors),
    (   Neighbors = []
    ->  TotalContam = 0.0,
        Detail = no_neighbors,
        EffPurity = Intrinsic
    ;   Intrinsic < 0.0
    ->  TotalContam = 0.0,
        Detail = no_neighbors,
        EffPurity = Intrinsic
    ;   drl_core:dr_type(C, Context, MyType),
        type_immunity(MyType, Immunity),
        compute_total_contamination(C, Intrinsic, Neighbors, Context, TotalContam, NeighborDetails),
        Detail = contamination_detail(NeighborDetails),
        RawEff is Intrinsic - TotalContam * Immunity,
        EffPurity is max(0.0, RawEff)
    ).

%% effective_purity(+C, -EffPurity, -Components)
%  BACKWARD COMPAT: Uses analytical context.
effective_purity(C, EffPurity, Components) :-
    constraint_indexing:default_context(Ctx),
    effective_purity(C, Ctx, EffPurity, Components).

%% compute_total_contamination(+C, +MyPurity, +Neighbors, +Context, -Total, -Details)
%  Sums contamination pressure from all neighbors (capped per edge).
compute_total_contamination(_, _, [], _, 0.0, []).
compute_total_contamination(C, MyPurity, [neighbor(Other, EdgeStrength, _Src)|Rest], Context, Total, [Detail|RestDetails]) :-
    compute_edge_contamination(C, MyPurity, Other, EdgeStrength, Context, EdgeContam, Detail),
    compute_total_contamination(C, MyPurity, Rest, Context, RestTotal, RestDetails),
    Total is EdgeContam + RestTotal.

%% compute_edge_contamination(+C, +MyPurity, +Other, +EdgeStrength, +Context, -Contam, -Detail)
%  Computes contamination from one neighbor.
%  Contamination flows downward only (from lower purity to higher purity).
compute_edge_contamination(_C, MyPurity, Other, EdgeStrength, Context, Contam, edge(Other, Delta, Contam)) :-
    structural_signatures:purity_score(Other, OtherPurity),
    OtherPurity >= 0.0,
    !,
    Delta is max(0.0, MyPurity - OtherPurity),
    (   Delta > 0.0
    ->  config:param(purity_attenuation_factor, AttFactor),
        config:param(purity_contamination_cap, Cap),
        (   drl_core:dr_type(Other, Context, OtherType)
        ->  type_contamination_strength(OtherType, TypeFactor)
        ;   TypeFactor = 0.0
        ),
        Attenuation is EdgeStrength * AttFactor,
        RawContam is Delta * Attenuation * TypeFactor,
        Contam is min(Cap, RawContam)
    ;   Contam = 0.0
    ).
compute_edge_contamination(_C, _MyPurity, Other, _EdgeStrength, _Context, 0.0, edge(Other, 0.0, 0.0)).

%% purity_contamination_pressure(+Src, +Tgt, +Context, -Pressure)
%  Computes contamination pressure from one specific Source onto Target.
%  Returns pressure(Delta, Attenuation, TypeFactor).
purity_contamination_pressure(Src, Tgt, Context, pressure(Delta, Attenuation, TypeFactor)) :-
    constraint_indexing:valid_context(Context),
    structural_signatures:purity_score(Src, SrcPurity),
    structural_signatures:purity_score(Tgt, TgtPurity),
    SrcPurity >= 0.0,
    TgtPurity >= 0.0,
    Delta is max(0.0, TgtPurity - SrcPurity),
    config:param(purity_attenuation_factor, AttFactor),
    constraint_neighbors(Tgt, Context, Neighbors),
    (   member(neighbor(Src, EdgeStrength, _), Neighbors)
    ->  Attenuation is EdgeStrength * AttFactor
    ;   Attenuation = 0.0
    ),
    (   drl_core:dr_type(Src, Context, SrcType)
    ->  type_contamination_strength(SrcType, TypeFactor)
    ;   TypeFactor = 0.0
    ).

/* ----------------------------------------------------------------
   C. NETWORK METRICS
   Aggregates across all loaded constraints.
   ---------------------------------------------------------------- */

%% network_purity_metrics(+Context, -Metrics)
%  Aggregates effective purity across all loaded constraints.
%  Metrics = network_metrics(WeakestLink, AvgPurity, AtRiskCount, TotalConstraints)
network_purity_metrics(Context, network_metrics(WeakestLink, AvgPurity, AtRiskCount, Total)) :-
    constraint_indexing:valid_context(Context),
    findall(C-EP, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        effective_purity(C, Context, EP, _),
        EP >= 0.0
    ), Pairs),
    length(Pairs, Total),
    (   Total > 0
    ->  findall(EP, member(_-EP, Pairs), EPs),
        sum_list(EPs, SumEP),
        AvgPurity is SumEP / Total,
        config:param(purity_action_escalation_floor, EscFloor),
        include(below_floor_pair(EscFloor), Pairs, AtRiskPairs),
        length(AtRiskPairs, AtRiskCount),
        find_weakest(Pairs, WeakestLink)
    ;   AvgPurity = 0.0,
        AtRiskCount = 0,
        WeakestLink = none
    ).

%% below_floor_pair(+Floor, +Pair)
below_floor_pair(Floor, _-EP) :- EP < Floor.

%% find_weakest(+Pairs, -WeakestLink)
find_weakest([], none).
find_weakest([C-EP], weakest(C, EP)) :- !.
find_weakest([C1-EP1, C2-EP2 | Rest], Weakest) :-
    (   EP1 =< EP2
    ->  find_weakest([C1-EP1 | Rest], Weakest)
    ;   find_weakest([C2-EP2 | Rest], Weakest)
    ).

%% cluster_purity(+Constraints, +Context, -Score)
%  Weighted average purity for a given list of constraints.
%  Weight = sum of edge strengths to other cluster members.
%  Note: effective_purity/4 clamps to [0.0, 1.0] via max(0.0, RawEff),
%  so all EP values are non-negative. Isolated nodes (no intra-cluster
%  neighbors) intentionally get Weight=1.0 so they contribute their
%  intrinsic purity without distorting the cluster average.
cluster_purity(Constraints, Context, Score) :-
    constraint_indexing:valid_context(Context),
    findall(EP-Weight, (
        member(C, Constraints),
        effective_purity(C, Context, EP, _),
        constraint_neighbors(C, Context, Neighbors),
        findall(S, (
            member(neighbor(Other, S, _), Neighbors),
            member(Other, Constraints)
        ), Strengths),
        (   Strengths = []
        ->  Weight = 1.0
        ;   sum_list(Strengths, Weight)
        )
    ), Pairs),
    (   Pairs = []
    ->  Score = 0.0
    ;   foldl(weighted_sum_acc, Pairs, 0.0-0.0, TotalWeightedEP-TotalWeight),
        (   TotalWeight > 0.0
        ->  Score is TotalWeightedEP / TotalWeight
        ;   Score = 0.0
        )
    ).

weighted_sum_acc(EP-W, AccEP-AccW, NewAccEP-NewAccW) :-
    NewAccEP is AccEP + EP * W,
    NewAccW is AccW + W.

%% contamination_path(+Src, +Tgt, +Context, -Path, -Loss)
%  BFS trace from Source to Target through the neighbor graph.
%  Returns path of [step(C, Purity, Delta), ...] and cumulative loss.
contamination_path(Src, Tgt, Context, Path, Loss) :-
    constraint_indexing:valid_context(Context),
    structural_signatures:purity_score(Src, SrcP),
    bfs_path(Src, Tgt, Context, [Src], RevPath),
    reverse(RevPath, FwdPath),
    build_path_steps(FwdPath, Context, Path, 0.0, Loss, SrcP).

%% bfs_path(+Current, +Target, +Context, +Visited, -Path)
bfs_path(Target, Target, _Context, _Visited, [Target]) :- !.
bfs_path(Current, Target, Context, Visited, [Current|RestPath]) :-
    constraint_neighbors(Current, Context, Neighbors),
    member(neighbor(Next, _, _), Neighbors),
    \+ member(Next, Visited),
    bfs_path(Next, Target, Context, [Next|Visited], RestPath).

%% build_path_steps(+Nodes, +Context, -Steps, +AccLoss, -TotalLoss, +PrevPurity)
build_path_steps([], _, [], Loss, Loss, _).
build_path_steps([C], Context, [step(C, EP, 0.0)], AccLoss, AccLoss, _) :-
    effective_purity(C, Context, EP, _), !.
build_path_steps([C|Rest], Context, [step(C, EP, Delta)|RestSteps], AccLoss, TotalLoss, PrevP) :-
    effective_purity(C, Context, EP, _),
    Delta is max(0.0, PrevP - EP),
    NewAcc is AccLoss + Delta,
    build_path_steps(Rest, Context, RestSteps, NewAcc, TotalLoss, EP).

%% weakest_link_purity(+Context, -Constraint, -Purity)
%  Returns the constraint with the lowest effective purity.
weakest_link_purity(Context, Constraint, Purity) :-
    network_purity_metrics(Context, network_metrics(WeakestLink, _, _, _)),
    WeakestLink = weakest(Constraint, Purity).

/* ----------------------------------------------------------------
   D. STAGE 7 INTEGRATION — Network-Qualified Actions
   Wraps purity_qualified_action/4 with effective purity.
   ---------------------------------------------------------------- */

%% network_qualified_action(+C, +Context, -QAction, -Rationale)
%  PRIMARY API: Qualifies action using network-aware effective purity.
%  If effective < intrinsic by ≥0.05, uses effective purity for
%  qualification (stricter). Otherwise delegates to purity_qualified_action/4.
network_qualified_action(C, Context, QAction, Rationale) :-
    constraint_indexing:valid_context(Context),
    effective_purity(C, Context, EffPurity, _Components),
    structural_signatures:purity_score(C, Intrinsic),
    (   Intrinsic >= 0.0,
        EffPurity >= 0.0,
        Drop is Intrinsic - EffPurity,
        Drop >= 0.05
    ->  % Network contamination is significant — use effective purity
        drl_core:dr_action(C, Context, BaseAction),
        qualify_action(BaseAction, EffPurity, C, BaseQAction, BaseRationale),
        QAction = BaseQAction,
        Rationale = network_contaminated(BaseRationale, Drop)
    ;   % No significant network effect — delegate to Stage 7
        purity_qualified_action(C, Context, QAction, Rationale)
    ).

%% network_qualified_action(+C, -QAction, -Rationale)
%  BACKWARD COMPAT: Uses analytical context.
network_qualified_action(C, QAction, Rationale) :-
    constraint_indexing:default_context(Ctx),
    network_qualified_action(C, Ctx, QAction, Rationale).

/* ================================================================
   UTILITY PREDICATES
   ================================================================ */

%% last(+List, -Last)
% Gets the last element of a list
last([X], X) :- !.
last([_|Xs], Last) :- last(Xs, Last).

% Provide a fail-safe default so they are "defined" even if no data is loaded
agent_index(_, _) :- fail.
constraint_data(_, _) :- fail.

/* ================================================================
   VERSION & MIGRATION INFO
   ================================================================ */

/*
VERSION HISTORY:
v4.0 (2025-01-17):
  - BREAKING: Full indexical relativity integration
  - NEW: dependency_chain/5 (added Context)
  - NEW: assess_scaffold_need/3 (added Context)
  - NEW: simulate_cut/3 (added Context)
  - NEW: context_depends_critically/3
  - NEW: estimate_impact_indexed/5
  - NEW: check_all_contexts/2 (multi-perspective safety check)
  - CHANGED: All dependency analysis is context-relative
  - MAINTAIN: Backward compatibility via /4, /3, /2 versions

v3.x:
  - Non-indexed modal logic
  - Single "God's eye view" dependency chains

MIGRATION GUIDE v3.x → v4.0:
  Old: dependency_chain(source, target, impact, reason)
  New: dependency_chain(source, target, impact, reason, context)
  
  Old: assess_scaffold_need(snare, assessment)
  New: assess_scaffold_need(snare, context, assessment)
  
  CRITICAL NEW REQUIREMENT:
  Before cutting any constraint, MUST evaluate:
    check_all_contexts(constraint, Report)
  to detect if cut is safe from one context but catastrophic from another.
  
  This prevents "institutional Mountain collapse" - where institution
  cuts what they see as Rope, unknowingly destroying powerless Mountain.
*/
