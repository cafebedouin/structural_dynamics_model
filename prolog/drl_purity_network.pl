% ============================================================================
% DRL PURITY NETWORK — Stage 8: Purity Propagation Through Constraint Networks
% Split from drl_modal_logic.pl (v5.2)
% ============================================================================

:- module(drl_purity_network, [
    % Network Discovery
    constraint_neighbors/3,
    constraint_neighbors/2,
    shared_agent_link/4,

    % Purity Propagation
    effective_purity/4,
    effective_purity/3,
    purity_contamination_pressure/4,
    type_contamination_strength/2,
    type_immunity/2,

    % Network Metrics
    network_purity_metrics/2,
    cluster_purity/3,
    contamination_path/5,
    weakest_link_purity/3,

    % Network-Qualified Actions
    network_qualified_action/4,
    network_qualified_action/3
]).

:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(structural_signatures).
:- use_module(drl_counterfactual).
:- use_module(drl_boltzmann_analysis).

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
            ( drl_counterfactual:infer_structural_coupling(C, Other, Strength),
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
type_contamination_strength(indexically_opaque, 0.3) :- !.
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
type_immunity(indexically_opaque, 0.7) :- !.
type_immunity(_,             0.5).

% Categorical: Contravariant purity propagation — effective purity decreases when neighbors have lower purity
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
        % CONVERGENCE-CRITICAL: MyPurity must be the constant Intrinsic here, NOT
        % the iterating EffPurity. Stage 8b's fixed-point monotonicity proof depends
        % on this: using Intrinsic ensures contamination is non-decreasing across
        % iterations, guaranteeing convergence without damping. If this ever changes,
        % the FPN iterator would need a contraction/damping factor.
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
        drl_boltzmann_analysis:qualify_action(BaseAction, EffPurity, C, BaseQAction, BaseRationale),
        QAction = BaseQAction,
        Rationale = network_contaminated(BaseRationale, Drop)
    ;   % No significant network effect — delegate to Stage 7
        drl_boltzmann_analysis:purity_qualified_action(C, Context, QAction, Rationale)
    ).

%% network_qualified_action(+C, -QAction, -Rationale)
%  BACKWARD COMPAT: Uses analytical context.
network_qualified_action(C, QAction, Rationale) :-
    constraint_indexing:default_context(Ctx),
    network_qualified_action(C, Ctx, QAction, Rationale).
