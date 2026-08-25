% ============================================================================
% GIANT COMPONENT ANALYSIS: Erdos-Renyi Phase Transition Investigation
% ============================================================================
% Investigates whether the constraint network exhibits a phase transition
% in connected component structure as the coupling threshold varies.
%
% Phase 1: Map the existing network at default settings
% Phase 2: Threshold sweep (0.10 to 0.90, step 0.05)
% Phase 3: Contamination propagation through the giant component
% Phase 4: Context comparison (institutional, moderate, analytical)
%
% Usage:
%   cd prolog && swipl -l stack.pl -l giant_component_analysis.pl \
%     -g run_giant_component_analysis -t halt \
%     > ../outputs/giant_component_analysis.md \
%     2> ../outputs/giant_component_analysis.log
%
% Individual phases:
%   swipl -l stack.pl -l giant_component_analysis.pl -g run_phase1 -t halt
%   swipl -l stack.pl -l giant_component_analysis.pl -g run_phase2 -t halt
% ============================================================================

:- module(giant_component_analysis, [
    run_giant_component_analysis/0,
    run_phase1/0,
    run_phase2/0,
    run_phase3/0,
    run_phase4/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(purity_scoring, [purity_score/2]).
:- use_module(boltzmann_compliance, [coupling_test_context/3]).
:- use_module(drl_core).
:- use_module(drl_purity_network, [constraint_neighbors/3, effective_purity/4, type_contamination_strength/2, type_immunity/2]).
:- use_module(drl_counterfactual).
:- use_module(corpus_loader).
:- use_module(cache_registry).      % OQ-193: clear_all_caches for the retract-recompute split
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).
:- use_module(library(aggregate)).
:- use_module(library(http/json)).  % OQ-193: giant_component_analysis.raw.json co-product

/* ================================================================
   DYNAMIC FACT DECLARATIONS
   ================================================================ */

:- dynamic gc_edge/4.             % gc_edge(C1, C2, Strength, Source) where C1 @< C2
:- dynamic gc_inferred_edge/3.    % gc_inferred_edge(C1, C2, Strength) where C1 @< C2
:- dynamic gc_node_type/3.        % gc_node_type(C, Context, Type)
:- dynamic gc_node_purity/3.      % gc_node_purity(C, IntrinsicP, EffP)
:- dynamic gc_sweep_result/5.     % gc_sweep_result(Thresh, NEdges, NComps, LargestSize, LargestFrac)
:- dynamic adj/2.                 % adj(A, B) — symmetric adjacency for BFS
:- dynamic gc_edges_precomputed/0.

%% all_corpus_constraints(-Constraints)
%  Discovers all constraint atoms with extractiveness data.
all_corpus_constraints(Constraints) :-
    config:param(extractiveness_metric_name, ExtName),
    findall(C,
        (   narrative_ontology:constraint_metric(C, ExtName, _),
            atom(C)
        ),
        CsRaw),
    sort(CsRaw, Constraints).

/* ================================================================
   EDGE PRE-COMPUTATION
   ================================================================ */

%% precompute_all_edges(+Constraints, +Context)
%  Temporarily lowers network_coupling_threshold to 0.01 to capture
%  ALL potential inferred_coupling edges. Stores in gc_edge/4.
%  Explicit and shared_agent edges are always included.
precompute_all_edges(Constraints, Context) :-
    (   gc_edges_precomputed
    ->  true  % already done
    ;   retractall(gc_edge(_, _, _, _)),
        retractall(gc_inferred_edge(_, _, _)),
        % Save original threshold
        config:param(network_coupling_threshold, OrigThresh),
        % Set to near-zero to capture everything
        retract(config:param(network_coupling_threshold, OrigThresh)),
        assertz(config:param(network_coupling_threshold, 0.01)),
        length(Constraints, N),
        format(user_error, '[giant] Pre-computing edges for ~w constraints (threshold=0.01)...~n', [N]),
        precompute_edges_loop(Constraints, Constraints, Context, 0, N),
        % Separately capture inferred_coupling edges for ALL pairs
        % of constraints that have gradient data. This bypasses
        % deduplicate_neighbors which loses inferred sources when
        % an explicit/shared edge exists for the same pair.
        precompute_inferred_edges(Constraints),
        % Restore original threshold
        retract(config:param(network_coupling_threshold, 0.01)),
        assertz(config:param(network_coupling_threshold, OrigThresh)),
        % Count total edges
        aggregate_all(count, gc_edge(_, _, _, _), EdgeCount),
        aggregate_all(count, gc_inferred_edge(_, _, _), InferredCount),
        format(user_error, '[giant] Pre-computed ~w non-inferred + ~w inferred edge records.~n',
               [EdgeCount, InferredCount]),
        assertz(gc_edges_precomputed)
    ).

precompute_edges_loop([], _, _, _, _).
precompute_edges_loop([C|Cs], AllNodes, Context, Done, Total) :-
    (   catch(
            drl_purity_network:constraint_neighbors(C, Context, Neighbors),
            _Err, Neighbors = [])
    ->  true
    ;   Neighbors = []
    ),
    % OQ-95: only assert edges whose far endpoint is an enumerated node.
    % constraint_neighbors/3 already drops zero-fact phantoms; this guard
    % additionally keeps claimed-but-unenumerated constraints (e.g. engine
    % demos, metric-less sidecars) out of the component graph, so component
    % size can never exceed the node count by construction. AllNodes comes
    % from all_corpus_constraints/1, which sorts — ord_memberchk is safe.
    forall(
        (   member(neighbor(Other, Strength, Source), Neighbors),
            ord_memberchk(Other, AllNodes)
        ),
        assert_edge_canonical(C, Other, Strength, Source)
    ),
    Done1 is Done + 1,
    (   0 =:= Done1 mod 50
    ->  format(user_error, '[giant]   ~w/~w nodes processed~n', [Done1, Total])
    ;   true
    ),
    precompute_edges_loop(Cs, AllNodes, Context, Done1, Total).

%% assert_edge_canonical(+C1, +C2, +Strength, +Source)
%  Stores edge with canonical ordering (C1 @< C2).
%  If edge already exists, keeps the stronger one.
assert_edge_canonical(C1, C2, Strength, Source) :-
    (   C1 @< C2 -> A = C1, B = C2 ; A = C2, B = C1 ),
    (   gc_edge(A, B, ExistStr, _)
    ->  (   Strength > ExistStr
        ->  retract(gc_edge(A, B, ExistStr, _)),
            assertz(gc_edge(A, B, Strength, Source))
        ;   true
        )
    ;   assertz(gc_edge(A, B, Strength, Source))
    ).

%% assert_inferred_canonical(+C1, +C2, +Strength)
%  Stores inferred_coupling edge with canonical ordering (C1 @< C2).
%  If edge already exists, keeps the stronger one.
assert_inferred_canonical(C1, C2, Strength) :-
    (   C1 @< C2 -> A = C1, B = C2 ; A = C2, B = C1 ),
    (   gc_inferred_edge(A, B, ExistStr)
    ->  (   Strength > ExistStr
        ->  retract(gc_inferred_edge(A, B, ExistStr)),
            assertz(gc_inferred_edge(A, B, Strength))
        ;   true
        )
    ;   assertz(gc_inferred_edge(A, B, Strength))
    ).

%% precompute_inferred_edges(+Constraints)
%  Discovers all inferred_coupling edges by enumerating all pairs of
%  constraints that have temporal gradient data. infer_structural_coupling/3
%  requires both arguments to be bound, so we must enumerate explicitly.
precompute_inferred_edges(Constraints) :-
    % Find constraints that have gradient data (needed for coupling inference)
    findall(C,
        (   member(C, Constraints),
            drl_counterfactual:dr_gradient_at(C, _, _)
        ),
        GradRaw),
    sort(GradRaw, GradCs),
    length(GradCs, NG),
    format(user_error, '[giant] Found ~w constraints with gradient data for inferred coupling.~n', [NG]),
    % Enumerate all ordered pairs and try to infer coupling
    forall(
        (   member(C1, GradCs),
            member(C2, GradCs),
            C1 @< C2,
            catch(drl_counterfactual:infer_structural_coupling(C1, C2, Str), _, fail),
            Str >= 0.01
        ),
        assert_inferred_canonical(C1, C2, Str)
    ).

%% edges_at_threshold(+Threshold, -Edges)
%  Returns edges that survive a given coupling threshold.
%  An (A,B) pair survives if:
%    - gc_edge has an always-surviving source (explicit, shared_beneficiary, shared_victim), OR
%    - gc_inferred_edge has strength >= Threshold
%  Deduplicates by (A,B) pair.
edges_at_threshold(Threshold, Edges) :-
    % Collect always-surviving edges as A-B pairs
    findall(
        A-B,
        (   gc_edge(A, B, _, Src),
            always_survives(Src)
        ),
        AlwaysPairs
    ),
    % Collect inferred edges that pass the threshold
    findall(
        A-B,
        (   gc_inferred_edge(A, B, Strength),
            Strength >= Threshold
        ),
        InferredPairs
    ),
    % Merge and deduplicate by pair
    append(AlwaysPairs, InferredPairs, AllPairs),
    sort(AllPairs, UniquePairs),
    % Convert back to edge/4 terms (use best available strength/source)
    findall(
        edge(A, B, S, Src),
        (   member(A-B, UniquePairs),
            best_edge_info(A, B, S, Src)
        ),
        Edges
    ).

%% always_survives(+Source)
%  Sources that survive regardless of threshold.
always_survives(explicit).
always_survives(shared_beneficiary).
always_survives(shared_victim).

%% best_edge_info(+A, +B, -Strength, -Source)
%  Returns the best strength/source for a canonical pair.
%  Prefers gc_edge info if available, falls back to gc_inferred_edge.
best_edge_info(A, B, S, Src) :-
    (   gc_edge(A, B, S, Src)
    ->  true
    ;   gc_inferred_edge(A, B, S),
        Src = inferred_coupling
    ).

/* ================================================================
   GRAPH ALGORITHMS
   ================================================================ */

%% build_adjacency_facts(+Edges)
%  Builds symmetric adj/2 facts for BFS.
build_adjacency_facts(Edges) :-
    retractall(adj(_, _)),
    forall(
        member(edge(A, B, _, _), Edges),
        ( assertz(adj(A, B)), assertz(adj(B, A)) )
    ).

%% compute_components(+Nodes, -Components)
%  BFS flood fill to find all connected components.
%  Returns list of component(Size, Members) sorted by size descending.
compute_components(Nodes, Components) :-
    sort(Nodes, SortedNodes),
    flood_fill(SortedNodes, 1, [], Components0),
    sort(1, @>=, Components0, Components).

flood_fill([], _, Acc, Acc).
flood_fill([Start|Rest], CompID, Acc, Components) :-
    bfs_component(Start, Members),
    length(Members, Size),
    ord_subtract(Rest, Members, StillUnvisited),
    CompID1 is CompID + 1,
    flood_fill(StillUnvisited, CompID1, [component(Size, Members)|Acc], Components).

%% bfs_component(+Start, -Members)
%  BFS from Start using adj/2 facts, returns sorted list of reachable nodes.
bfs_component(Start, Members) :-
    bfs_queue([Start], [Start], Members).

bfs_queue([], Visited, Visited).
bfs_queue([Current|Queue], Visited, Members) :-
    findall(N, (adj(Current, N), \+ ord_memberchk(N, Visited)), NewRaw),
    sort(NewRaw, NewNodes),
    ord_union(Visited, NewNodes, NewVisited),
    append(Queue, NewNodes, NewQueue),
    bfs_queue(NewQueue, NewVisited, Members).

%% compute_degree_list(+Nodes, -DegreePairs)
%  Returns list of C-Degree pairs.
compute_degree_list(Nodes, DegreePairs) :-
    findall(
        C-Deg,
        (   member(C, Nodes),
            aggregate_all(count, adj(C, _), Deg)
        ),
        DegreePairs
    ).

/* ================================================================
   STATISTICS HELPERS
   ================================================================ */

%% distribution_stats(+Values, -Stats)
%  Stats = stats(Min, Q1, Median, Q3, Max, Mean, N)
distribution_stats([], stats(0, 0, 0, 0, 0, 0.0, 0)) :- !.
distribution_stats(Values, stats(Min, Q1, Median, Q3, Max, Mean, N)) :-
    msort(Values, Sorted),
    length(Sorted, N),
    N > 0,
    nth1(1, Sorted, Min),
    last(Sorted, Max),
    Q1Idx is max(1, round(N * 0.25)),
    MedIdx is max(1, round(N * 0.50)),
    Q3Idx is max(1, round(N * 0.75)),
    nth1(Q1Idx, Sorted, Q1),
    nth1(MedIdx, Sorted, Median),
    nth1(Q3Idx, Sorted, Q3),
    sumlist(Values, Sum),
    Mean is Sum / N.

%% count_in_int_range(+Values, +Lo, +Hi, -Count)
%  Counts values where Lo =< V =< Hi.
count_in_int_range(Values, Lo, Hi, Count) :-
    include(in_int_range(Lo, Hi), Values, Matching),
    length(Matching, Count).

in_int_range(Lo, Hi, V) :- V >= Lo, V =< Hi.

count_above(Values, Threshold, Count) :-
    include(above_threshold(Threshold), Values, Matching),
    length(Matching, Count).

above_threshold(T, V) :- V > T.

%% last(+List, -Last)
last([X], X) :- !.
last([_|Xs], L) :- last(Xs, L).

/* ================================================================
   NODE PROPERTY PRE-COMPUTATION
   ================================================================ */

%% precompute_node_properties(+Constraints, +Context)
%  Caches type, intrinsic purity, and effective purity for each constraint.
precompute_node_properties(Constraints, Context) :-
    retractall(gc_node_type(_, _, _)),
    retractall(gc_node_purity(_, _, _)),
    length(Constraints, N),
    format(user_error, '[giant] Computing node properties for ~w constraints...~n', [N]),
    precompute_props_loop(Constraints, Context, 0, N).

precompute_props_loop([], _, _, _).
precompute_props_loop([C|Cs], Context, Done, Total) :-
    % Type
    (   catch(drl_core:dr_type(C, Context, Type), _, Type = unknown)
    ->  true
    ;   Type = unknown
    ),
    assertz(gc_node_type(C, Context, Type)),
    % Purity
    % OQ-60: `unknown` (no-data) is not an exception, so the catch recovery does
    % not fire — collapse it to the gc cache's -1.0 no-data sentinel so the
    % `IP >= 0.0` distribution filters exclude it (as they do -1.0) rather than
    % throwing in sumlist. Inert until a producer emits `unknown`.
    (   catch(purity_scoring:purity_score(C, IntrinsicP0), _, IntrinsicP0 = -1.0),
        number(IntrinsicP0)
    ->  IntrinsicP = IntrinsicP0
    ;   IntrinsicP = -1.0
    ),
    % OQ-60: effective_purity can now propagate `unknown` (0a) — not an exception,
    % so collapse to the gc cache's -1.0 no-data sentinel (as with IntrinsicP above)
    % so the `EP >= 0.0` distribution filters exclude it rather than throwing.
    (   catch(drl_purity_network:effective_purity(C, Context, EffP0, _), _, EffP0 = -1.0),
        number(EffP0)
    ->  EffP = EffP0
    ;   EffP = -1.0
    ),
    assertz(gc_node_purity(C, IntrinsicP, EffP)),
    Done1 is Done + 1,
    (   0 =:= Done1 mod 100
    ->  format(user_error, '[giant]   ~w/~w properties computed~n', [Done1, Total])
    ;   true
    ),
    precompute_props_loop(Cs, Context, Done1, Total).

/* ================================================================
   PHASE 1: MAP EXISTING NETWORK
   ================================================================ */

run_phase1 :-
    load_all_testsets,
    all_corpus_constraints(Cs),
    constraint_indexing:default_context(Ctx),
    length(Cs, NC),
    format(user_error, '[phase1] Starting with ~w constraints...~n', [NC]),
    % Pre-compute all edges at low threshold for later sweep
    precompute_all_edges(Cs, Ctx),
    % Filter to current threshold for Phase 1
    config:param(network_coupling_threshold, CurrThresh),
    edges_at_threshold(CurrThresh, Edges),
    length(Edges, NE),
    % Build adjacency and components
    build_adjacency_facts(Edges),
    compute_components(Cs, Components),
    compute_degree_list(Cs, DegreePairs),
    % Pre-compute node properties
    precompute_node_properties(Cs, Ctx),
    % Report
    format('## Phase 1: Network Topology at Default Threshold~n~n'),
    format('**Context**: analytical/global (default)  ~n'),
    format('**Coupling threshold**: ~3f~n~n', [CurrThresh]),
    report_network_summary(NC, NE, Components, DegreePairs),
    report_degree_distribution(DegreePairs),
    report_components(Components, NC),
    report_type_distribution(Cs, Ctx),
    report_purity_landscape(Cs, Ctx),
    report_super_spreaders(Cs, Ctx),
    format('~n').

/* ---- Phase 1 Report Generators ---- */

report_network_summary(NC, NE, Components, DegreePairs) :-
    format('### Network Summary~n~n'),
    findall(C, member(C-0, DegreePairs), Isolated),
    length(Isolated, NIsolated),
    NConnected is NC - NIsolated,
    MaxEdges is NC * (NC - 1) / 2,
    (   MaxEdges > 0 -> Density is NE / MaxEdges ; Density = 0.0 ),
    length(Components, NComp),
    AvgDeg is (2 * NE) / max(1, NC),
    ERCritEdges is NC / 2.0,
    format('| Metric | Value |~n'),
    format('|--------|-------|~n'),
    format('| Total nodes (constraints) | ~w |~n', [NC]),
    format('| Connected nodes (degree > 0) | ~w |~n', [NConnected]),
    format('| Isolated nodes (degree 0) | ~w |~n', [NIsolated]),
    format('| Edges | ~w |~n', [NE]),
    format('| Graph density | ~6f |~n', [Density]),
    format('| Average degree | ~2f |~n', [AvgDeg]),
    format('| Connected components | ~w |~n', [NComp]),
    format('| E-R critical edge count (n/2) | ~1f |~n~n', [ERCritEdges]).

report_degree_distribution(DegreePairs) :-
    format('### Degree Distribution~n~n'),
    findall(D, member(_-D, DegreePairs), Degrees),
    distribution_stats(Degrees, stats(Min, Q1, Med, Q3, Max, Mean, N)),
    format('| Stat | Value |~n'),
    format('|------|-------|~n'),
    format('| N | ~w |~n', [N]),
    format('| Min | ~w |~n', [Min]),
    format('| Q1 | ~w |~n', [Q1]),
    format('| Median | ~w |~n', [Med]),
    format('| Q3 | ~w |~n', [Q3]),
    format('| Max | ~w |~n', [Max]),
    format('| Mean | ~2f |~n~n', [Mean]),
    % Histogram
    format('#### Degree Histogram~n~n'),
    format('| Degree Range | Count |~n'),
    format('|-------------|-------|~n'),
    count_in_int_range(Degrees, 0, 0, N0),
    count_in_int_range(Degrees, 1, 1, N1),
    count_in_int_range(Degrees, 2, 3, N2_3),
    count_in_int_range(Degrees, 4, 6, N4_6),
    count_in_int_range(Degrees, 7, 10, N7_10),
    count_in_int_range(Degrees, 11, 20, N11_20),
    count_above(Degrees, 20, N20plus),
    format('| 0 (isolated) | ~w |~n', [N0]),
    format('| 1 | ~w |~n', [N1]),
    format('| 2-3 | ~w |~n', [N2_3]),
    format('| 4-6 | ~w |~n', [N4_6]),
    format('| 7-10 | ~w |~n', [N7_10]),
    format('| 11-20 | ~w |~n', [N11_20]),
    format('| 21+ | ~w |~n~n', [N20plus]).

report_components(Components, NC) :-
    format('### Connected Components~n~n'),
    length(Components, NComp),
    format('**~w components** found.~n~n', [NComp]),
    (   Components = [component(LargestSize, _)|_]
    ->  LargestFrac is LargestSize / max(1, NC),
        format('**Largest component**: ~w nodes (~1f% of network)~n~n',
               [LargestSize, LargestFrac * 100])
    ;   LargestSize = 0, LargestFrac = 0.0,
        format('No components found.~n~n')
    ),
    % Giant component assessment
    (   LargestFrac > 0.5
    ->  format('**Giant component detected.** The largest component contains >50% of all nodes.~n~n')
    ;   LargestFrac > 0.25
    ->  format('**Near-giant component.** The largest component is substantial but below 50%.~n~n')
    ;   format('**No giant component.** The network is fragmented at this threshold.~n~n')
    ),
    % Top components by size
    format('#### Top Components by Size~n~n'),
    format('| Rank | Size | Fraction |~n'),
    format('|------|------|----------|~n'),
    report_top_components(Components, NC, 1, 15).

report_top_components([], _, _, _) :- !.
report_top_components(_, _, Rank, Max) :- Rank > Max, !.
report_top_components([component(Size, _)|Rest], NC, Rank, Max) :-
    Frac is Size / max(1, NC),
    format('| ~w | ~w | ~3f |~n', [Rank, Size, Frac]),
    Rank1 is Rank + 1,
    report_top_components(Rest, NC, Rank1, Max).

report_type_distribution(Cs, Ctx) :-
    format('### Type Distribution~n~n'),
    length(Cs, NC),
    TypeOrder = [mountain, rope, scaffold, tangled_rope, piton, snare,
                 naturalized, unknown],
    format('| Type | Count | Fraction |~n'),
    format('|------|-------|----------|~n'),
    forall(member(T, TypeOrder), (
        findall(C, (member(C, Cs), gc_node_type(C, Ctx, T)), TCs),
        length(TCs, NT),
        (   NT > 0
        ->  Frac is NT / max(1, NC),
            format('| ~w | ~w | ~3f |~n', [T, NT, Frac])
        ;   true
        )
    )),
    format('~n').

report_purity_landscape(Cs, _Ctx) :-
    format('### Purity Landscape~n~n'),
    % Intrinsic purity distribution
    findall(IP, (member(C, Cs), gc_node_purity(C, IP, _), IP >= 0.0), IPs),
    length(IPs, NIP),
    % OQ-60 0b (R3/R4): denominator carried unconditionally (excluded rows =
    % -1.0 gate-fail sentinel + unknown no-data, both cached as -1.0 here).
    length(Cs, NTotalCs),
    format('#### Intrinsic Purity (~w/~w constraints with valid scores)~n~n', [NIP, NTotalCs]),
    (   IPs \= []
    ->  distribution_stats(IPs, stats(IMin, IQ1, IMed, IQ3, IMax, IMean, _)),
        format('| Stat | Value |~n'),
        format('|------|-------|~n'),
        format('| Min | ~3f |~n', [IMin]),
        format('| Q1 | ~3f |~n', [IQ1]),
        format('| Median | ~3f |~n', [IMed]),
        format('| Q3 | ~3f |~n', [IQ3]),
        format('| Max | ~3f |~n', [IMax]),
        format('| Mean | ~3f |~n~n', [IMean])
    ;   format('No valid intrinsic purity scores.~n~n')
    ),
    % Effective purity distribution
    findall(EP, (member(C, Cs), gc_node_purity(C, _, EP), EP >= 0.0), EPs),
    length(EPs, NEP),
    format('#### Effective Purity (~w/~w constraints with valid scores)~n~n', [NEP, NTotalCs]),
    (   EPs \= []
    ->  distribution_stats(EPs, stats(EMin, EQ1, EMed, EQ3, EMax, EMean, _)),
        format('| Stat | Value |~n'),
        format('|------|-------|~n'),
        format('| Min | ~3f |~n', [EMin]),
        format('| Q1 | ~3f |~n', [EQ1]),
        format('| Median | ~3f |~n', [EMed]),
        format('| Q3 | ~3f |~n', [EQ3]),
        format('| Max | ~3f |~n', [EMax]),
        format('| Mean | ~3f |~n~n', [EMean])
    ;   format('No valid effective purity scores.~n~n')
    ),
    % Zone analysis
    format('#### Purity Zone Distribution~n~n'),
    config:param(purity_action_sound_floor, SoundFloor),
    config:param(purity_action_escalation_floor, EscFloor),
    config:param(purity_action_degraded_floor, DegFloor),
    % Intrinsic zones
    count_in_zone(IPs, SoundFloor, 1.01, NSoundI),
    count_in_zone(IPs, EscFloor, SoundFloor, NBorderI),
    count_in_zone(IPs, DegFloor, EscFloor, NWarnI),
    count_in_zone(IPs, -0.01, DegFloor, NDegI),
    % Effective zones
    count_in_zone(EPs, SoundFloor, 1.01, NSoundE),
    count_in_zone(EPs, EscFloor, SoundFloor, NBorderE),
    count_in_zone(EPs, DegFloor, EscFloor, NWarnE),
    count_in_zone(EPs, -0.01, DegFloor, NDegE),
    format('| Zone | Intrinsic | Effective | Shift |~n'),
    format('|------|-----------|-----------|-------|~n'),
    ShiftSound is NSoundI - NSoundE,
    ShiftBorder is NBorderI - NBorderE,
    ShiftWarn is NWarnI - NWarnE,
    ShiftDeg is NDegI - NDegE,
    format('| Sound (>= ~2f) | ~w | ~w | ~w |~n', [SoundFloor, NSoundI, NSoundE, ShiftSound]),
    format('| Borderline (~2f - ~2f) | ~w | ~w | ~w |~n', [EscFloor, SoundFloor, NBorderI, NBorderE, ShiftBorder]),
    format('| Warning (~2f - ~2f) | ~w | ~w | ~w |~n', [DegFloor, EscFloor, NWarnI, NWarnE, ShiftWarn]),
    format('| Degraded (< ~2f) | ~w | ~w | ~w |~n~n', [DegFloor, NDegI, NDegE, ShiftDeg]),
    % How many shifted zones due to network effects?
    findall(C,
        (   member(C, Cs),
            gc_node_purity(C, IP, EP),
            IP >= 0.0, EP >= 0.0,
            action_band(IP, ZoneI),
            action_band(EP, ZoneE),
            ZoneI \= ZoneE
        ),
        Shifted),
    length(Shifted, NShifted),
    format('**~w constraints shifted purity zone** due to network contamination effects.~n~n', [NShifted]).

count_in_zone(Values, Lo, Hi, Count) :-
    include(in_float_range(Lo, Hi), Values, Matching),
    length(Matching, Count).

in_float_range(Lo, Hi, V) :- V >= Lo, V < Hi.

% OQ-62/OQ-60: fail closed on both absence tokens before any arithmetic. The
% sole call site (:584-585) pre-filters with `IP >= 0.0, EP >= 0.0`, so this is
% defense in depth — but that filter is a property of one caller, not of the
% predicate, and `unknown` here previously threw type_error(evaluable,unknown/0).
% Order is load-bearing: `< 0.0` throws on the atom.
%% action_band(+P, -Band)
%  Bands purity against the config ACTION floors (purity_action_*) — this is an
%  operational escalation ladder, not the spec purity vocabulary.
%  OQ-62: renamed from purity_zone/2; the atoms carry a `gc_` prefix so the
%  rename's back-substitution witness cannot collide with the generic English
%  words (monitor/watch/escalate) that occur in this module's narrative prose,
%  and so disjointness holds by construction rather than by luck.
%  `unknown` is the one deliberately shared token (see fpn_report:ep_band/2).
action_band(P, unknown) :- \+ number(P), !.
action_band(P, unknown) :- P < 0.0, !.
action_band(P, gc_monitor) :-
    config:param(purity_action_sound_floor, F), P >= F, !.
action_band(P, gc_watch) :-
    config:param(purity_action_escalation_floor, F), P >= F, !.
action_band(P, gc_escalate) :-
    config:param(purity_action_degraded_floor, F), P >= F, !.
action_band(_, gc_override).

report_super_spreaders(Cs, Ctx) :-
    format('### Super-spreaders (Highest Contamination Potential)~n~n'),
    findall(
        scored(C, Potential, Type, Deg, CS, EP),
        (   member(C, Cs),
            gc_node_type(C, Ctx, Type),
            drl_purity_network:type_contamination_strength(Type, CS),
            CS > 0.0,
            aggregate_all(count, adj(C, _), Deg),
            Deg > 0,
            gc_node_purity(C, _, EP),
            Potential is Deg * CS
        ),
        Scored
    ),
    sort(1, @>=, Scored, Sorted0),
    % Sort by Potential descending (first field after scored functor)
    predsort(compare_potential, Scored, Sorted),
    format('| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |~n'),
    format('|------------|------|--------|------------|------------|-----------|~n'),
    report_top_spreaders(Sorted, 20),
    format('~n'),
    % Ignore Sorted0 warning
    ignore(Sorted0 = _).

compare_potential(Order, scored(_, P1, _, _, _, _), scored(_, P2, _, _, _, _)) :-
    (   P1 > P2 -> Order = (<)
    ;   P1 < P2 -> Order = (>)
    ;   Order = (=)
    ).

report_top_spreaders([], _) :- !.
report_top_spreaders(_, 0) :- !.
report_top_spreaders([scored(C, Pot, Type, Deg, CS, EP)|Rest], N) :-
    format('| ~w | ~w | ~w | ~2f | ~3f | ~2f |~n', [C, Type, Deg, CS, EP, Pot]),
    N1 is N - 1,
    report_top_spreaders(Rest, N1).

/* ================================================================
   PHASE 2: THRESHOLD SWEEP
   ================================================================ */

run_phase2 :-
    load_all_testsets,
    all_corpus_constraints(Cs),
    constraint_indexing:default_context(Ctx),
    length(Cs, NC),
    % Ensure edges are pre-computed
    precompute_all_edges(Cs, Ctx),
    format('## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)~n~n'),
    % Short-circuit: if no inferred edges exist, the sweep is degenerate —
    % every threshold yields the same edge set (only explicit/shared survive).
    aggregate_all(count, gc_inferred_edge(_, _, _), NInferred),
    (   NInferred =:= 0
    ->  format('**No inferred coupling edges** in the corpus (0 constraints with gradient data).~n'),
        format('Threshold sweep is degenerate: all thresholds produce the same edge set '),
        format('(only `explicit` and `shared_agent` edges survive regardless of threshold).~n~n'),
        % Record a single sweep result at the default threshold for downstream phases
        retractall(gc_sweep_result(_, _, _, _, _)),
        config:param(network_coupling_threshold, CurrThresh),
        edges_at_threshold(CurrThresh, Edges),
        length(Edges, NE),
        build_adjacency_facts(Edges),
        compute_components(Cs, Components),
        (   Components = [component(LargestSize, _)|_]
        ->  LargestFrac is LargestSize / max(1, NC)
        ;   LargestSize = 0, LargestFrac = 0.0
        ),
        length(Components, NComp),
        assertz(gc_sweep_result(CurrThresh, NE, NComp, LargestSize, LargestFrac)),
        format('| Threshold | Edges | Components | Largest | Fraction |~n'),
        format('|-----------|-------|------------|---------|----------|~n'),
        format('| ~3f (all) | ~w | ~w | ~w | ~3f |~n~n',
               [CurrThresh, NE, NComp, LargestSize, LargestFrac])
    ;   format('Sweeping `network_coupling_threshold` from 0.10 to 0.90 in steps of 0.05.~n'),
        format('For each threshold, only `inferred_coupling` edges are filtered; '),
        format('`explicit` and `shared_agent` edges always survive.~n~n'),
        numlist_float(0.10, 0.05, 0.90, Points),
        length(Points, NP),
        format(user_error, '[phase2] Sweeping ~w threshold values for ~w constraints...~n', [NP, NC]),
        retractall(gc_sweep_result(_, _, _, _, _)),
        sweep_thresholds(Points, Cs, NC),
        report_sweep_table(NC),
        report_phase_transition(NC)
    ),
    format('~n').

%% numlist_float(+Start, +Step, +End, -List)
numlist_float(Start, _, End, []) :- Start > End + 0.001, !.
numlist_float(Start, Step, End, [Start|Rest]) :-
    Next is Start + Step,
    numlist_float(Next, Step, End, Rest).

sweep_thresholds([], _, _).
sweep_thresholds([Thresh|Rest], Cs, NC) :-
    format(user_error, '[phase2]   threshold = ~3f~n', [Thresh]),
    edges_at_threshold(Thresh, Edges),
    length(Edges, NE),
    build_adjacency_facts(Edges),
    compute_components(Cs, Components),
    (   Components = [component(LargestSize, _)|_]
    ->  LargestFrac is LargestSize / max(1, NC)
    ;   LargestSize = 0, LargestFrac = 0.0
    ),
    length(Components, NComp),
    assertz(gc_sweep_result(Thresh, NE, NComp, LargestSize, LargestFrac)),
    sweep_thresholds(Rest, Cs, NC).

report_sweep_table(NC) :-
    format('### Sweep Results~n~n'),
    format('| Threshold | Edges | Components | Largest | Fraction |~n'),
    format('|-----------|-------|------------|---------|----------|~n'),
    forall(
        gc_sweep_result(Thresh, NE, NComp, LargestSize, LargestFrac),
        format('| ~3f | ~w | ~w | ~w | ~3f |~n',
               [Thresh, NE, NComp, LargestSize, LargestFrac])
    ),
    format('~n'),
    % Context: ER prediction
    ERCrit is NC / 2.0,
    format('**Erdos-Renyi prediction**: For a random graph with n=~w nodes, ', [NC]),
    format('the giant component emerges when the number of edges exceeds n/2 = ~1f.~n~n', [ERCrit]).

report_phase_transition(NC) :-
    format('### Phase Transition Analysis~n~n'),
    % Collect sweep data sorted by threshold (ascending)
    findall(Thresh-Frac, gc_sweep_result(Thresh, _, _, _, Frac), Pairs),
    msort(Pairs, SortedPairs),
    % As threshold DECREASES (reading right-to-left), more edges appear and
    % the giant component grows. We look for the steepest INCREASE in fraction
    % when reading left-to-right (i.e., the steepest DECREASE when threshold
    % increases — the fragmentation point).
    adjacent_pairs(SortedPairs, AdjPairs),
    find_steepest_jump(AdjPairs, BestT1, BestT2, BestDelta, BestF1, BestF2),
    format('**Steepest jump**: threshold ~3f (fraction=~3f) -> ~3f (fraction=~3f), delta = ~3f~n~n',
           [BestT1, BestF1, BestT2, BestF2, BestDelta]),
    MidThresh is (BestT1 + BestT2) / 2,
    format('**Critical threshold (midpoint of steepest jump)**: ~3f~n~n', [MidThresh]),
    % Find transition width: where does fraction cross 10% and 50%?
    find_crossing(SortedPairs, 0.50, CrossHigh),
    find_crossing(SortedPairs, 0.10, CrossLow),
    (   CrossHigh \= none, CrossLow \= none
    ->  TransWidth is abs(CrossHigh - CrossLow),
        format('**Transition width**: Largest component goes from <10% at threshold ~3f ', [CrossLow]),
        format('to >50% at threshold ~3f (width = ~3f)~n~n', [CrossHigh, TransWidth])
    ;   format('**Transition width**: Could not identify clean 10%->50% crossing range.~n~n')
    ),
    % ER comparison
    ERCritEdges is NC / 2.0,
    format('### Comparison to Erdos-Renyi Prediction~n~n'),
    format('- **ER critical edge count**: ~1f (for n=~w nodes)~n', [ERCritEdges, NC]),
    % Find edge count at critical threshold
    (   gc_sweep_result(BestT1, NE1, _, _, _)
    ->  format('- **Edges at threshold ~3f**: ~w~n', [BestT1, NE1])
    ;   true
    ),
    (   gc_sweep_result(BestT2, NE2, _, _, _)
    ->  format('- **Edges at threshold ~3f**: ~w~n', [BestT2, NE2])
    ;   true
    ),
    format('~n'),
    % Qualitative assessment
    (   BestDelta > 0.30
    ->  format('**Verdict**: The network exhibits a **sharp phase transition** (delta > 0.30). '),
        format('This is sharper than typical Erdos-Renyi, suggesting structural features '),
        format('(hubs, clustering) that amplify the transition.~n~n')
    ;   BestDelta > 0.15
    ->  format('**Verdict**: The network exhibits a **moderate phase transition** (delta 0.15-0.30). '),
        format('The giant component emerges over a modest threshold range.~n~n')
    ;   BestDelta > 0.05
    ->  format('**Verdict**: The network exhibits a **gradual transition** (delta 0.05-0.15). '),
        format('The giant component grows incrementally rather than erupting.~n~n')
    ;   format('**Verdict**: **No clear phase transition** (delta < 0.05). '),
        format('The network may be naturally resilient to cascading connectivity, '),
        format('or the edge types may be too heterogeneous for a clean ER transition.~n~n')
    ).

adjacent_pairs([], []).
adjacent_pairs([_], []).
adjacent_pairs([A, B | Rest], [A-B | Pairs]) :-
    adjacent_pairs([B | Rest], Pairs).

%% find_steepest_jump(+AdjPairs, -T1, -T2, -Delta, -F1, -F2)
%  Finds the adjacent threshold pair with the steepest change in fraction.
find_steepest_jump(AdjPairs, BestT1, BestT2, BestDelta, BestF1, BestF2) :-
    foldl(max_delta_acc, AdjPairs,
          0.0-0.0-0.0-0.0-0.0,
          BestT1-BestT2-BestDelta-BestF1-BestF2).

max_delta_acc((T1-F1)-(T2-F2), AccT1-AccT2-AccDelta-AccF1-AccF2,
              OutT1-OutT2-OutDelta-OutF1-OutF2) :-
    Delta is abs(F1 - F2),
    (   Delta > AccDelta
    ->  OutT1 = T1, OutT2 = T2, OutDelta = Delta, OutF1 = F1, OutF2 = F2
    ;   OutT1 = AccT1, OutT2 = AccT2, OutDelta = AccDelta,
        OutF1 = AccF1, OutF2 = AccF2
    ).

%% find_crossing(+SortedPairs, +TargetFrac, -Threshold)
%  Finds the threshold where fraction crosses TargetFrac (going from high to low).
%  Returns 'none' if not found.
find_crossing([], _, none).
find_crossing([_], _, none).
find_crossing([_T1-F1, T2-F2 | Rest], Target, Threshold) :-
    (   F1 >= Target, F2 < Target
    ->  Threshold = T2   % Fraction dropped below target at T2
    ;   find_crossing([T2-F2 | Rest], Target, Threshold)
    ).

/* ================================================================
   PHASE 3: CONTAMINATION THROUGH THE GIANT COMPONENT
   ================================================================ */

run_phase3 :-
    load_all_testsets,
    all_corpus_constraints(Cs),
    constraint_indexing:default_context(Ctx),
    length(Cs, NC),
    % Ensure edges pre-computed and properties cached
    precompute_all_edges(Cs, Ctx),
    (   \+ gc_node_type(_, _, _)
    ->  config:param(network_coupling_threshold, CurrThresh),
        edges_at_threshold(CurrThresh, Edges),
        build_adjacency_facts(Edges),
        precompute_node_properties(Cs, Ctx)
    ;   config:param(network_coupling_threshold, CurrThresh),
        edges_at_threshold(CurrThresh, Edges),
        build_adjacency_facts(Edges)
    ),
    compute_components(Cs, Components),
    format('## Phase 3: Contamination Through the Giant Component~n~n'),
    format('**Threshold**: ~3f (default)~n~n', [CurrThresh]),
    (   Components = [component(GCSize, GCMembers)|_],
        GCFrac is GCSize / max(1, NC),
        GCFrac > 0.10   % At least 10% to be interesting
    ->  format('**Giant component size**: ~w nodes (~1f% of network)~n~n',
               [GCSize, GCFrac * 100]),
        report_gc_composition(GCMembers, Ctx),
        report_contamination_sources(GCMembers, Ctx),
        report_multihop_contamination(GCMembers, Ctx),
        report_sound_constraint_exposure(GCMembers, Ctx),
        report_contamination_collapse_analysis(GCMembers, Ctx)
    ;   format('**No significant component found** at threshold ~3f. ', [CurrThresh]),
        format('The largest component contains fewer than 10% of nodes.~n~n'),
        format('This means the network is naturally fragmented at the current coupling '),
        format('threshold. Contamination cannot cascade across the full network because '),
        format('constraints are organized into small, isolated clusters.~n~n'),
        % Try at a lower threshold to see if anything interesting emerges
        format('### Contamination at Lower Threshold~n~n'),
        find_interesting_threshold(Cs, NC, Components, Ctx)
    ).

%% find_interesting_threshold(+Cs, +NC, +DefaultComponents, +Ctx)
%  If no giant component at default, find the threshold where one appears.
find_interesting_threshold(Cs, NC, _, Ctx) :-
    numlist_float(0.10, 0.05, 0.50, LowThresholds),
    find_first_giant(LowThresholds, Cs, NC, Ctx).

find_first_giant([], _, _, _) :-
    format('No giant component (>25% of nodes) found at any threshold from 0.10 to 0.50.~n'),
    format('The network is inherently fragmented.~n~n').
find_first_giant([T|Ts], Cs, NC, Ctx) :-
    edges_at_threshold(T, Edges),
    build_adjacency_facts(Edges),
    compute_components(Cs, Components),
    (   Components = [component(LSize, LMembers)|_],
        LFrac is LSize / max(1, NC),
        LFrac > 0.25
    ->  format('Giant component first appears at threshold **~3f** ', [T]),
        format('(~w nodes, ~1f% of network).~n~n', [LSize, LFrac * 100]),
        format('Running contamination analysis at this threshold:~n~n'),
        precompute_node_properties(Cs, Ctx),
        report_gc_composition(LMembers, Ctx),
        report_contamination_sources(LMembers, Ctx),
        report_multihop_contamination(LMembers, Ctx),
        report_sound_constraint_exposure(LMembers, Ctx)
    ;   find_first_giant(Ts, Cs, NC, Ctx)
    ).

/* ---- Phase 3 Report Generators ---- */

report_gc_composition(Members, Ctx) :-
    format('### Giant Component Composition~n~n'),
    length(Members, Size),
    TypeOrder = [mountain, rope, scaffold, tangled_rope, piton, snare,
                 naturalized, unknown],
    format('| Type | Count | Fraction |~n'),
    format('|------|-------|----------|~n'),
    forall(member(T, TypeOrder), (
        findall(C, (member(C, Members), gc_node_type(C, Ctx, T)), TCs),
        length(TCs, NT),
        (   NT > 0
        ->  Frac is NT / max(1, Size),
            format('| ~w | ~w | ~3f |~n', [T, NT, Frac])
        ;   true
        )
    )),
    format('~n'),
    % Purity distribution within giant component
    findall(IP, (member(C, Members), gc_node_purity(C, IP, _), IP >= 0.0), IPs),
    findall(EP, (member(C, Members), gc_node_purity(C, _, EP), EP >= 0.0), EPs),
    format('#### Purity Within Giant Component~n~n'),
    % OQ-60 0b (R3/R4): descriptive stats compute over scorable rows and carry
    % their denominator unconditionally (excluded = -1.0 sentinel + unknown).
    length(Members, NMembersPur),
    length(IPs, NIPs),
    length(EPs, NEPs),
    format('- Coverage: intrinsic ~w/~w scorable, effective ~w/~w scorable~n',
           [NIPs, NMembersPur, NEPs, NMembersPur]),
    (   IPs \= []
    ->  distribution_stats(IPs, stats(IMin, _, IMed, _, IMax, IMean, _)),
        format('- **Intrinsic**: min=~3f, median=~3f, max=~3f, mean=~3f~n',
               [IMin, IMed, IMax, IMean])
    ;   true
    ),
    (   EPs \= []
    ->  distribution_stats(EPs, stats(EMin, _, EMed, _, EMax, EMean, _)),
        format('- **Effective**: min=~3f, median=~3f, max=~3f, mean=~3f~n~n',
               [EMin, EMed, EMax, EMean])
    ;   format('~n')
    ),
    % Active contamination sources
    config:param(purity_contamination_source_floor, SrcFloor),
    findall(C, (member(C, Members), gc_node_purity(C, IP, _), IP >= 0.0, IP < SrcFloor), SrcCs),
    length(SrcCs, NSrc),
    % Sound constraints
    config:param(purity_action_sound_floor, SoundFloor),
    findall(C, (member(C, Members), gc_node_purity(C, _, EP), EP >= SoundFloor), SoundCs),
    length(SoundCs, NSound),
    format('- **Active contamination sources** (intrinsic purity < ~2f): ~w~n', [SrcFloor, NSrc]),
    format('- **Sound constraints** (effective purity >= ~2f): ~w~n~n', [SoundFloor, NSound]).

report_contamination_sources(Members, Ctx) :-
    format('### Contamination Sources (Super-spreaders in Giant Component)~n~n'),
    findall(
        scored(C, Potential, Type, Deg, CS, EP),
        (   member(C, Members),
            gc_node_type(C, Ctx, Type),
            drl_purity_network:type_contamination_strength(Type, CS),
            CS > 0.0,
            aggregate_all(count, (adj(C, N), member(N, Members)), Deg),
            Deg > 0,
            gc_node_purity(C, _, EP),
            Potential is Deg * CS
        ),
        Scored
    ),
    predsort(compare_potential, Scored, Sorted),
    length(Sorted, NSpreaders),
    format('**~w contamination-capable nodes** in the giant component.~n~n', [NSpreaders]),
    format('| Constraint | Type | Intra-GC Degree | Contam Str | Eff Purity | Potential |~n'),
    format('|------------|------|-----------------|------------|------------|-----------|~n'),
    report_top_spreaders(Sorted, 20),
    format('~n').

report_multihop_contamination(Members, Ctx) :-
    format('### Multi-hop Contamination Simulation~n~n'),
    format('Simulating contamination propagation beyond the current one-hop model.~n'),
    format('Attenuation: 0.50 per hop. Stop when attenuation * strength < 0.01.~n~n'),
    % Find significant contamination sources
    findall(scored(C, Pot, Type, IP)-C,
        (   member(C, Members),
            gc_node_type(C, Ctx, Type),
            drl_purity_network:type_contamination_strength(Type, CS),
            CS >= 0.5,
            gc_node_purity(C, IP, _),
            IP >= 0.0, IP < 0.50,
            aggregate_all(count, adj(C, _), Deg),
            Pot is Deg * CS
        ),
        ScoredPairs),
    pairs_keys(ScoredPairs, ScoredAll),
    pairs_values(ScoredPairs, SourcesAll),
    sort(SourcesAll, UniqSources),
    length(UniqSources, NSrc),
    format('**~w active contamination sources** (type strength >= 0.5, purity < 0.50)~n~n', [NSrc]),
    (   NSrc > 0
    ->  % Report only top 50 by potential to avoid O(sources × nodes) blowup
        predsort(compare_potential_scored, ScoredAll, SortedScored),
        length(SortedScored, NAll),
        ReportN is min(50, NAll),
        (   NAll > 50
        ->  format('*Showing top 50 of ~w sources by contamination potential.*~n~n', [NAll])
        ;   true
        ),
        format('| Source | Type | Purity | 1-hop | 2-hop | 3-hop | Total Reach |~n'),
        format('|--------|------|--------|-------|-------|-------|-------------|~n'),
        report_multihop_top(SortedScored, Members, ReportN),
        format('~n'),
        % Aggregate reach: single multi-source BFS from ALL sources at once
        multihop_reach_multi(UniqSources, Members, 3, ReachByHop),
        aggregate_hop_counts(ReachByHop, NReached),
        length(Members, GCSize),
        ReachedFrac is NReached / max(1, GCSize),
        format('**Total unique nodes reached** within 3 hops of any source: ~w (~1f% of giant component)~n~n',
               [NReached, ReachedFrac * 100])
    ;   format('No active contamination sources found in the giant component.~n~n')
    ).

compare_potential_scored(Order, scored(_, P1, _, _), scored(_, P2, _, _)) :-
    (   P1 > P2 -> Order = (<)
    ;   P1 < P2 -> Order = (>)
    ;   Order = (=)
    ).

report_multihop_top(_, _, 0) :- !.
report_multihop_top([], _, _) :- !.
report_multihop_top([scored(C, _, Type, IP)|Rest], Members, N) :-
    multihop_reach(C, Members, 3, ReachByHop),
    hop_count(ReachByHop, 1, N1),
    hop_count(ReachByHop, 2, N2),
    hop_count(ReachByHop, 3, N3),
    TotalReach is N1 + N2 + N3,
    format('| ~w | ~w | ~3f | ~w | ~w | ~w | ~w |~n',
           [C, Type, IP, N1, N2, N3, TotalReach]),
    N1rem is N - 1,
    report_multihop_top(Rest, Members, N1rem).

%% multihop_reach_multi(+Sources, +Members, +MaxHops, -ReachByHop)
%  Multi-source BFS: starts from ALL sources simultaneously.
%  Returns nodes at each hop distance from the nearest source.
multihop_reach_multi(Sources, Members, MaxHops, ReachByHop) :-
    sort(Members, SortedMembers),
    sort(Sources, SortedSources),
    multihop_bfs(SortedSources, SortedSources, SortedMembers, 1, MaxHops, [], ReachByHop).

%% aggregate_hop_counts(+ReachByHop, -Total)
aggregate_hop_counts(ReachByHop, Total) :-
    findall(N, (member(hop(_, Nodes), ReachByHop), length(Nodes, N)), Counts),
    sumlist(Counts, Total).

%% multihop_reach(+Source, +Members, +MaxHops, -ReachByHop)
%  BFS from Source restricted to Members, returns nodes at each hop distance.
multihop_reach(Source, Members, MaxHops, ReachByHop) :-
    sort(Members, SortedMembers),
    multihop_bfs([Source], [Source], SortedMembers, 1, MaxHops, [], ReachByHop).

multihop_bfs(_, _, _, Hop, MaxHops, Acc, Acc) :- Hop > MaxHops, !.
multihop_bfs([], _, _, _, _, Acc, Acc) :- !.
multihop_bfs(Frontier, Visited, Members, Hop, MaxHops, Acc, ReachByHop) :-
    findall(N,
        (   member(F, Frontier),
            adj(F, N),
            ord_memberchk(N, Members),
            \+ ord_memberchk(N, Visited)
        ),
        NewRaw),
    sort(NewRaw, NewNodes),
    (   NewNodes = []
    ->  ReachByHop = Acc
    ;   ord_union(Visited, NewNodes, NewVisited),
        Hop1 is Hop + 1,
        multihop_bfs(NewNodes, NewVisited, Members, Hop1, MaxHops,
                     [hop(Hop, NewNodes)|Acc], ReachByHop)
    ).

hop_count(ReachByHop, H, N) :-
    (   member(hop(H, Nodes), ReachByHop)
    ->  length(Nodes, N)
    ;   N = 0
    ).

report_sound_constraint_exposure(Members, Ctx) :-
    format('### Sound Constraint Exposure to Contamination~n~n'),
    config:param(purity_action_sound_floor, SoundFloor),
    findall(C,
        (   member(C, Members),
            gc_node_purity(C, _, EP),
            EP >= SoundFloor
        ),
        SoundCs),
    length(SoundCs, NSound),
    format('**~w sound constraints** (effective purity >= ~2f) in the giant component.~n~n',
           [NSound, SoundFloor]),
    % Find contamination sources
    findall(Src,
        (   member(Src, Members),
            gc_node_type(Src, Ctx, Type),
            drl_purity_network:type_contamination_strength(Type, CS),
            CS >= 0.3,
            gc_node_purity(Src, IP, _),
            IP >= 0.0, IP < 0.50
        ),
        Sources),
    sort(Sources, UniqSources),
    (   UniqSources \= [], SoundCs \= []
    ->  % Single multi-source BFS from all sources to compute distances for all nodes
        sort(Members, SortedMembers),
        sort(UniqSources, SortedSources),
        retractall(gc_source_dist(_, _, _)),
        bfs_all_distances(SortedSources, SortedSources, SortedMembers, 0, 10),
        % Report table (capped at 50 rows for readability)
        length(SoundCs, NSoundTotal),
        ReportN is min(50, NSoundTotal),
        (   NSoundTotal > 50
        ->  format('*Showing first 50 of ~w sound constraints.*~n~n', [NSoundTotal])
        ;   true
        ),
        format('| Sound Constraint | Eff Purity | Nearest Source | Distance | Would Cross Threshold? |~n'),
        format('|------------------|------------|----------------|----------|----------------------|~n'),
        report_sound_rows(SoundCs, Ctx, ReportN),
        format('~n'),
        % Summary using pre-computed distances
        count_within_distance(SoundCs, 1, NW1),
        count_within_distance(SoundCs, 2, NW2),
        count_within_distance(SoundCs, 3, NW3),
        format('**Hop distance summary**:~n'),
        format('- Within 1 hop of a contamination source: ~w/~w sound constraints~n', [NW1, NSound]),
        format('- Within 2 hops: ~w/~w~n', [NW2, NSound]),
        format('- Within 3 hops: ~w/~w~n~n', [NW3, NSound]),
        retractall(gc_source_dist(_, _, _))
    ;   format('No contamination sources or no sound constraints in the giant component.~n~n')
    ).

:- dynamic gc_source_dist/3.   % gc_source_dist(Node, NearestSource, Distance)

%% bfs_all_distances(+Frontier, +Visited, +Members, +Dist, +MaxDist)
%  Multi-source BFS that records the nearest source distance for every reachable node.
%  Starts from all sources at distance 0.
bfs_all_distances([], _, _, _, _) :- !.
bfs_all_distances(_, _, _, Dist, MaxDist) :- Dist > MaxDist, !.
bfs_all_distances(Frontier, Visited, Members, Dist, MaxDist) :-
    % Record distance for all frontier nodes (first visit = shortest distance)
    forall(member(F, Frontier),
        (   gc_source_dist(F, _, _) -> true
        ;   assertz(gc_source_dist(F, source, Dist))
        )),
    % Expand frontier
    findall(N,
        (   member(F, Frontier),
            adj(F, N),
            ord_memberchk(N, Members),
            \+ ord_memberchk(N, Visited)
        ),
        NewRaw),
    sort(NewRaw, NewNodes),
    ord_union(Visited, NewNodes, NewVisited),
    Dist1 is Dist + 1,
    bfs_all_distances(NewNodes, NewVisited, Members, Dist1, MaxDist).

report_sound_rows(_, _, 0) :- !.
report_sound_rows([], _, _) :- !.
report_sound_rows([SC|Rest], Ctx, N) :-
    gc_node_purity(SC, _, EP),
    (   gc_source_dist(SC, _, Dist)
    ->  NearestSrc = nearby_source
    ;   Dist = 999, NearestSrc = none
    ),
    would_cross_threshold(SC, NearestSrc, Dist, Ctx, CrossResult),
    format('| ~w | ~3f | ~w | ~w | ~w |~n', [SC, EP, NearestSrc, Dist, CrossResult]),
    N1 is N - 1,
    report_sound_rows(Rest, Ctx, N1).

count_within_distance(Cs, MaxDist, Count) :-
    include(within_dist(MaxDist), Cs, Matching),
    length(Matching, Count).

within_dist(MaxDist, C) :-
    gc_source_dist(C, _, D), D =< MaxDist.

%% find_nearest_source(+Node, +Sources, +Members, -NearestSrc, -Dist)
%  BFS from Node to find the closest contamination source.
find_nearest_source(Node, Sources, Members, NearestSrc, Dist) :-
    sort(Members, SortedMembers),
    nearest_source_bfs([Node], [Node], SortedMembers, Sources, 0, NearestSrc, Dist).

nearest_source_bfs([], _, _, _, _, none, 999) :- !.
nearest_source_bfs(Frontier, Visited, Members, Sources, CurrDist, NearestSrc, Dist) :-
    % Check if any frontier node is a source
    (   member(F, Frontier), member(F, Sources)
    ->  NearestSrc = F, Dist = CurrDist
    ;   CurrDist > 10   % give up after 10 hops
    ->  NearestSrc = none, Dist = 999
    ;   % Expand frontier
        findall(N,
            (   member(F, Frontier),
                adj(F, N),
                ord_memberchk(N, Members),
                \+ ord_memberchk(N, Visited)
            ),
            NewRaw),
        sort(NewRaw, NewNodes),
        ord_union(Visited, NewNodes, NewVisited),
        NextDist is CurrDist + 1,
        nearest_source_bfs(NewNodes, NewVisited, Members, Sources, NextDist, NearestSrc, Dist)
    ).

%% would_cross_threshold(+Target, +Source, +Dist, +Ctx, -Result)
%  Estimates whether multi-hop contamination from Source would push
%  Target below the sound threshold.
would_cross_threshold(_, none, _, _, 'N/A') :- !.
would_cross_threshold(_, _, 999, _, 'N/A') :- !.
would_cross_threshold(_, nearby_source, _, _, '~') :- !.  % batch BFS, no individual source info
would_cross_threshold(Target, Source, Dist, Ctx, Result) :-
    gc_node_purity(Target, _, TargetEP),
    gc_node_purity(Source, SrcIP, _),
    gc_node_type(Source, Ctx, SrcType),
    drl_purity_network:type_contamination_strength(SrcType, CS),
    drl_purity_network:type_immunity(Ctx, _),  % just checking module is loaded
    gc_node_type(Target, Ctx, TgtType),
    drl_purity_network:type_immunity(TgtType, Immunity),
    % Attenuation = 0.50^Dist (multi-hop decay)
    Attenuation is 0.50 ** Dist,
    % Delta = target purity - source purity
    Delta is max(0.0, TargetEP - max(0.0, SrcIP)),
    % Contamination pressure
    Pressure is Delta * Attenuation * CS,
    config:param(purity_contamination_cap, Cap),
    EffPressure is min(Cap, Pressure) * Immunity,
    NewPurity is TargetEP - EffPressure,
    config:param(purity_action_sound_floor, SoundFloor),
    (   NewPurity < SoundFloor
    ->  format(atom(Result), 'YES (~3f -> ~3f)', [TargetEP, NewPurity])
    ;   format(atom(Result), 'No (~3f -> ~3f)', [TargetEP, NewPurity])
    ).

report_contamination_collapse_analysis(Members, Ctx) :-
    format('### Contamination Collapse Analysis~n~n'),
    format('At what contamination settings would sound constraints in the giant '),
    format('component collapse into the degraded zone?~n~n'),
    config:param(purity_contamination_cap, OrigCap),
    config:param(purity_attenuation_factor, OrigAtt),
    config:param(purity_action_sound_floor, SoundFloor),
    config:param(purity_action_degraded_floor, DegFloor),
    % Count sound constraints at current settings
    findall(C, (member(C, Members), gc_node_purity(C, _, EP), EP >= SoundFloor), SoundCs),
    length(SoundCs, NSound),
    format('Current settings: cap=~2f, attenuation=~2f~n', [OrigCap, OrigAtt]),
    format('Sound constraints in giant component: ~w~n~n', [NSound]),
    % OQ-356 coverage line. The table below counts only members with a NUMERIC
    % effective purity, so without this line a shrinking scorable domain reads
    % as falling contamination — an absence presenting as a presence. Printing
    % |Members| and both partition halves lets a reader check two identities
    % rather than one:
    %   (1) NS + NB + NW + ND == NKept       band coverage of the filtered domain
    %   (2) NKept + NExcluded == |Members|   the guard's partition
    % Split deliberately: the four bands cover [-0.01, 1.01) while the filter
    % admits ANY numeric EP >= 0.0, so a value at or above 1.01 would land in no
    % band and break (1) — as a false alarm attributed to the guard if the two
    % were conflated into one identity.
    length(Members, NMembers),
    count_by_action_band(Members, Ctx, SoundFloor, DegFloor, _, _, _, _,
                         NKeptCov, NExcludedCov),
    format('**Purity coverage**: ~w of ~w giant-component members have a numeric effective purity; ~w excluded (members with no numeric effective purity).~n~n',
           [NKeptCov, NMembers, NExcludedCov]),
    format('Sweeping contamination_cap from 0.10 to 1.00 (attenuation fixed at ~2f):~n~n', [OrigAtt]),
    format('| Cap | Sound (>=~2f) | Borderline | Warning | Degraded (<~2f) |~n',
           [SoundFloor, DegFloor]),
    format('|-----|', []),
    format('--------|', []),
    format('------------|', []),
    format('---------|', []),
    format('---------|~n', []),
    numlist_float(0.10, 0.10, 1.00, CapValues),
    forall(member(Cap, CapValues), (
        % Temporarily override cap
        retract(config:param(purity_contamination_cap, _)),
        assertz(config:param(purity_contamination_cap, Cap)),
        % Recompute effective purities for GC members
        count_by_action_band(Members, Ctx, SoundFloor, DegFloor, NS, NB, NW, ND, _, _),
        format('| ~2f | ~w | ~w | ~w | ~w |~n', [Cap, NS, NB, NW, ND])
    )),
    % Restore original cap
    retract(config:param(purity_contamination_cap, _)),
    assertz(config:param(purity_contamination_cap, OrigCap)),
    format('~n').

%% count_by_action_band(+Members, +Ctx, +SoundFloor, +DegFloor, -NS, -NB, -NW, -ND,
%%                       -NKept, -NExcluded)
%  Recomputes effective purity for members, counts by zone, and returns the
%  sizes of the guard's partition alongside.
%
%  OQ-356 (2026-08-24): THIRD ingest of effective_purity/4 in this module and
%  the one OQ-60 missed. `unknown` (no-data, 0a propagation) is a RETURN VALUE,
%  not an exception, so the catch/3 below intercepts NOTHING and the bare
%  `EP >= 0.0` threw type_error(evaluable, unknown/0) — killing the entire
%  Phase-3 contamination block on 17 of 20 corpora (the three that passed did so
%  by unreachability or by having no unknown-purity GC member, never by being
%  safe). Guard order is load-bearing: number/1 must come BEFORE the comparison,
%  because `>=` throws on the atom before any later check could run. The two
%  siblings already guarded this way are precompute_props_loop/4 at :362-369 in
%  this file — whose comment names this exact hazard — and
%  drl_purity_network.pl:352-353.
%
%  NKept and NExcluded are ACCUMULATED INDEPENDENTLY by the single pass that
%  applies the guard. NKept is NOT derived as |Members| - NExcluded: deriving it
%  would make the caller's conservation identity NKept + NExcluded == |Members|
%  true by construction, i.e. a check that cannot fail — which is this OQ's own
%  defect class. Do not "simplify" this back to /8 or /9; that silently deletes
%  the test while leaving the assertion in place.
count_by_action_band(Members, Ctx, SoundFloor, DegFloor, NS, NB, NW, ND,
                     NKept, NExcluded) :-
    config:param(purity_action_escalation_floor, EscFloor),
    partition_scorable_purity(Members, Ctx, EPs, Excluded),
    length(EPs, NKept),
    length(Excluded, NExcluded),
    count_in_zone(EPs, SoundFloor, 1.01, NS),
    count_in_zone(EPs, EscFloor, SoundFloor, NB),
    count_in_zone(EPs, DegFloor, EscFloor, NW),
    count_in_zone(EPs, -0.01, DegFloor, ND).

%% partition_scorable_purity(+Members, +Ctx, -EPs, -Excluded)
%  ONE pass over Members, split on exactly the conjunction the guard applies —
%  never a second, independently-written test. A member is KEPT iff
%  effective_purity/4 succeeds AND yields a number AND that number is >= 0.0.
%  Everything else is EXCLUDED, and that covers three silently different
%  populations which a naive "count the unknowns" gets wrong:
%    (a) effective_purity SUCCEEDS with a non-number  — the OQ-60 defect class
%    (b) effective_purity THROWS                      — dropped by the catch/3
%    (c) effective_purity FAILS                       — the conjunct fails
%  If the excluded count covered only (a) while the guard also drops (b) and
%  (c), the caller's conservation identity would break on any leg with a
%  throwing or failing member — and break as a FALSE ALARM attributed to the
%  guard, which is worse than no check at all.
%
%  Behaviour-preserving against the pre-fix findall/3 for every value that used
%  to survive it: the if-then-else commits to the first solution where findall
%  took all, and effective_purity/4 was measured SEMIDET on 2,241 members across
%  three legs (audits/2026-08-24_oq356_purity_guard/determinism_witness.txt,
%  with a live positive control that the probe can count multiplicity).
partition_scorable_purity([], _, [], []).
partition_scorable_purity([C|Cs], Ctx, EPs, Excluded) :-
    (   catch(drl_purity_network:effective_purity(C, Ctx, EP, _), _, fail),
        number(EP), EP >= 0.0
    ->  EPs = [EP|EPs1], Excluded = Excluded1
    ;   EPs = EPs1,      Excluded = [C|Excluded1]
    ),
    partition_scorable_purity(Cs, Ctx, EPs1, Excluded1).

/* ================================================================
   PHASE 4: CONTEXT COMPARISON
   ================================================================ */

run_phase4 :-
    load_all_testsets,
    all_corpus_constraints(Cs),
    length(Cs, _NC),
    % Ensure edges pre-computed
    constraint_indexing:default_context(DefaultCtx),
    precompute_all_edges(Cs, DefaultCtx),
    format('## Phase 4: Context Comparison~n~n'),
    format('The edge set is context-independent (edges come from `affects_constraint`, '),
    format('`infer_structural_coupling`, and `shared_agent_link` — none of which '),
    format('depend on observer context). What changes across contexts is the '),
    format('**type classification** and hence the **contamination dynamics**.~n~n'),
    config:param(network_coupling_threshold, CurrThresh),
    edges_at_threshold(CurrThresh, Edges),
    build_adjacency_facts(Edges),
    compute_components(Cs, Components),
    length(Edges, NE),
    (   Components = [component(LSize, _)|_]
    ->  true
    ;   LSize = 0
    ),
    length(Components, NComp),
    format('**Fixed topology**: ~w edges, ~w components, largest = ~w nodes (threshold = ~3f)~n~n',
           [NE, NComp, LSize, CurrThresh]),
    % Compare type distributions across contexts
    Contexts = [
        ctx(institutional, local, 'Institutional/Local'),
        ctx(moderate, national, 'Moderate/National'),
        ctx(analytical, global, 'Analytical/Global (default)')
    ],
    format('### Type Distribution by Context~n~n'),
    TypeOrder = [mountain, rope, scaffold, tangled_rope, piton, snare,
                 naturalized, unknown],
    format('| Type |'),
    forall(member(ctx(_, _, Label), Contexts), format(' ~w |', [Label])),
    format('~n|------|'),
    forall(member(_, Contexts), format('------|')),
    format('~n'),
    % Pre-compute type for each (Constraint, Context) pair — single dr_type call each.
    % Skip contexts already cached by earlier phases (e.g., analytical/global from Phase 1).
    forall(member(ctx(Power, Scope, _), Contexts), (
        boltzmann_compliance:coupling_test_context(Power, Scope, Ctx),
        (   gc_node_type(_, Ctx, _)   % already cached for this context
        ->  true
        ;   forall(member(C, Cs), (
                (   catch(drl_core:dr_type(C, Ctx, CType), _, CType = unknown)
                ->  true
                ;   CType = unknown
                ),
                assertz(gc_node_type(C, Ctx, CType))
            ))
        )
    )),
    forall(member(T, TypeOrder), (
        format('| ~w |', [T]),
        forall(member(ctx(Power, Scope, _), Contexts), (
            boltzmann_compliance:coupling_test_context(Power, Scope, Ctx),
            aggregate_all(count, gc_node_type(_, Ctx, T), NT),
            format(' ~w |', [NT])
        )),
        format('~n')
    )),
    format('~n'),
    % Contamination source comparison
    format('### Contamination Source Comparison~n~n'),
    format('Number of constraints that are active contamination sources (type strength > 0, '),
    format('acts as contamination emitter) by context:~n~n'),
    format('| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |~n'),
    format('|---------|-------|-------|-------------|----------|---------------|~n'),
    forall(member(ctx(Power, Scope, Label), Contexts), (
        boltzmann_compliance:coupling_test_context(Power, Scope, Ctx),
        count_type_in_context(Cs, Ctx, snare, NSnare),
        count_type_in_context(Cs, Ctx, piton, NPiton),
        count_type_in_context(Cs, Ctx, tangled_rope, NTR),
        count_type_in_context(Cs, Ctx, scaffold, NScaff),
        Total is NSnare + NPiton + NTR + NScaff,
        format('| ~w | ~w | ~w | ~w | ~w | ~w |~n',
               [Label, NSnare, NPiton, NTR, NScaff, Total])
    )),
    format('~n'),
    % Key finding
    format('### Key Finding~n~n'),
    format('Since edges are context-independent, the network topology (connected '),
    format('components, component sizes, degree distribution) is identical across '),
    format('all contexts. What changes is WHICH nodes are contamination sources. '),
    format('A constraint classified as a snare from one context (high contamination '),
    format('strength = 1.0) may be classified as a rope from another (low strength = 0.1). '),
    format('This means the effective contamination pressure varies by context '),
    format('even though the network structure does not.~n~n').

count_type_in_context(_Cs, Ctx, Type, Count) :-
    aggregate_all(count, gc_node_type(_, Ctx, Type), Count).

/* ================================================================
   EMBEDDED PROLOG FACTS
   ================================================================ */

report_embedded_facts :-
    format('## Embedded Prolog Facts~n~n'),
    format('```prolog~n'),
    format('%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)~n'),
    forall(gc_sweep_result(T, NE, NC, LS, LF),
        format('gc_sweep_result(~3f, ~w, ~w, ~w, ~3f).~n', [T, NE, NC, LS, LF])),
    format('```~n~n').

/* ================================================================
   PROVENANCE SPLIT (OQ-193)
   ================================================================
   Presentation-only split (operator ruling (c), 2026-07-02): the engine
   topology keeps same-kernel sibling affects_constraint edges (siblings are
   intended topology; NO topology guard). But the single pooled headline reads
   within-kernel reading-plurality as cross-kernel coupling, so we ALSO report a
   sibling-stripped stratum. Method: retract-recompute (adapted from the frozen
   probe audits/2026-07-02_oq193_giant_comp_ruling/probe_giant_ripple.pl) —
   deduplicate_neighbors keeps the strongest edge per pair, so a post-hoc gc_edge
   filter would miss an inferred edge that resurfaces on recompute; stripping the
   affects_constraint SUBSTRATE and recomputing is the faithful strip.

   Placed DEAD-LAST in run_giant_component_analysis, in a subprocess that then
   exits — nothing downstream reads the stripped topology, so there is NO restore
   (the probe's re-assert step is intentionally dropped). Does its own fresh
   pooled measure first because phases 2/4 mutate gc state at other
   thresholds/contexts.
   ================================================================ */

%% same_kernel_explicit_edge(?A, ?B)
%  An explicit affects_constraint edge whose endpoints share a cs_kernel_id
%  (the OQ-193 sibling discriminant; probe strip_edge/2). Stored direction —
%  the retraction below removes the fact as authored.
same_kernel_explicit_edge(A, B) :-
    narrative_ontology:affects_constraint(A, B),
    narrative_ontology:cs_kernel_id(A, K),
    narrative_ontology:cs_kernel_id(B, K).

%% same_kernel_pair(+A, +B)
%  True when A and B share a cs_kernel_id (tests surviving edges for the
%  second control — "cross-kernel" is a misnomer if any survive).
same_kernel_pair(A, B) :-
    narrative_ontology:cs_kernel_id(A, K),
    narrative_ontology:cs_kernel_id(B, K).

canon_pair(A-B, Lo-Hi) :- ( A @< B -> Lo = A, Hi = B ; Lo = B, Hi = A ).

%% measure_topology(+Cs, +Ctx, -NComp, -GiantSize, -NE, -Components)
%  Fresh full recompute of the component topology at the default coupling
%  threshold over the CURRENT affects_constraint substrate (probe measure/6
%  core, also returning Components for per-constraint membership). Clears all gc
%  edge state + caches first so a prior phase's precompute cannot leak. Leaves
%  adj/2 populated for the just-measured stratum — the caller reads per-node
%  degree from adj/2 BEFORE the next measure overwrites it.
measure_topology(Cs, Ctx, NComp, GiantSize, NE, Components) :-
    retractall(gc_edge(_, _, _, _)),
    retractall(gc_edges_precomputed),
    retractall(gc_inferred_edge(_, _, _)),
    retractall(adj(_, _)),
    cache_registry:clear_all_caches,
    precompute_all_edges(Cs, Ctx),
    config:param(network_coupling_threshold, Thresh),
    edges_at_threshold(Thresh, Edges),
    length(Edges, NE),
    build_adjacency_facts(Edges),
    compute_components(Cs, Components),
    length(Components, NComp),
    ( Components = [component(GiantSize, _)|_] -> true ; GiantSize = 0 ).

%% snapshot_per_constraint(+Cs, +Components, -Rows)
%  Rows = [C-row(CompSize, InGiant, Degree), ...] for the CURRENT adj/2 stratum.
%  MUST run immediately after measure_topology. Members are sorted (BFS builds
%  Visited via ord_union), so ord_memberchk is safe.
snapshot_per_constraint(Cs, Components, Rows) :-
    ( Components = [component(_, GiantMembers)|_] -> true ; GiantMembers = [] ),
    findall(C-row(CompSize, InGiant, Deg),
        (   member(C, Cs),
            component_size_of(C, Components, CompSize),
            ( ord_memberchk(C, GiantMembers) -> InGiant = true ; InGiant = false ),
            aggregate_all(count, adj(C, _), Deg)
        ),
        Rows).

component_size_of(C, Components, Size) :-
    ( member(component(S, Members), Components), ord_memberchk(C, Members)
    -> Size = S
    ;  Size = 0 ).

%% compute_provenance_split(+Ctx, -Split)
%  Split = split(Control, NNodes, NStrip, pooled(NE,NComp,Giant),
%                stratum(NE,NComp,Giant), surviving(M,M1,M2), Label,
%                PooledRows, StratumRows, CtxAtom)
compute_provenance_split(Ctx, Split) :-
    all_corpus_constraints(Cs),
    length(Cs, NNodes),
    % Raw strip list (stored direction) — for retraction + control count, as the
    % probe does; NStrip counts facts, not canonical pairs (two authored
    % directions retract two facts, so the control drop stays consistent).
    findall(A-B, same_kernel_explicit_edge(A, B), StripRaw0),
    sort(StripRaw0, StripEdges),
    length(StripEdges, NStrip),
    % Canonical strip set — for the survivor partition (survivor edges are A@<B).
    maplist(canon_pair, StripEdges, StripCanon0),
    sort(StripCanon0, StripCanon),
    % Pooled (fresh) measure — dead-last, after phases 2/4 mutated gc state.
    measure_topology(Cs, Ctx, NCompP, GiantP, NEP, ComponentsP),
    aggregate_all(count, narrative_ontology:affects_constraint(_, _), NAff0),
    snapshot_per_constraint(Cs, ComponentsP, PooledRows),
    % Strip the same-kernel explicit edges from the substrate (NO restore).
    forall(member(A-B, StripEdges), retract(narrative_ontology:affects_constraint(A, B))),
    % Stripped measure.
    measure_topology(Cs, Ctx, NCompS, GiantS, NES, ComponentsS),
    aggregate_all(count, narrative_ontology:affects_constraint(_, _), NAff1),
    snapshot_per_constraint(Cs, ComponentsS, StratumRows),
    % Positive control: raw affects_constraint count dropped by exactly NStrip.
    Drop is NAff0 - NAff1,
    ( Drop =:= NStrip -> Control = ok ; Control = failed ),
    % Second control: same-kernel edges surviving the stripped recompute.
    config:param(network_coupling_threshold, Thresh),
    edges_at_threshold(Thresh, SurvEdges),
    findall(A-B, ( member(edge(A, B, _, _), SurvEdges), same_kernel_pair(A, B) ), SurvSK0),
    sort(SurvSK0, SurvSK),
    length(SurvSK, M),
    ord_intersection(SurvSK, StripCanon, DedupResurfaced),
    ord_subtract(SurvSK, StripCanon, NeverStripped),
    length(DedupResurfaced, M1),
    length(NeverStripped, M2),
    % Partition identity (free positive control on the new cause-partition logic).
    ( M1 + M2 =:= M -> true
    ; throw(error(oq193_partition_broken(M, M1, M2), _)) ),
    ( M =:= 0 -> Label = cross_kernel ; Label = explicit_sibling_stripped ),
    term_to_atom(Ctx, CtxAtom),
    Split = split(Control, NNodes, NStrip,
                  pooled(NEP, NCompP, GiantP),
                  stratum(NES, NCompS, GiantS),
                  surviving(M, M1, M2),
                  Label, PooledRows, StratumRows, CtxAtom).

%% report_provenance_split(+Split)
%  The `## Provenance split (OQ-193)` markdown section. NOT wrapped in catch —
%  joins the plain conjunction so any exception fails the -g goal.
report_provenance_split(Split) :-
    Split = split(Control, NNodes, NStrip,
                  pooled(NEP, NCompP, GiantP),
                  stratum(NES, NCompS, GiantS),
                  surviving(M, M1, M2),
                  Label, _PooledRows, _StratumRows, _CtxAtom),
    format('## Provenance split (OQ-193)~n~n'),
    format('*Pooled topology counts within-kernel reading-plurality (sibling '),
    format('`affects_constraint` edges) as coupling. The stratum strips explicit '),
    format('same-kernel sibling edges (retract-recompute) to expose cross-kernel '),
    format('structure. Operator ruling (c), 2026-07-02: siblings STAY in the engine '),
    format('topology — this is a presentation split only, no engine-behavior change.*~n~n'),
    GiantFracP is GiantP / max(1, NNodes),
    format('**Sibling edges stripped**: ~w  ~n', [NStrip]),
    format('**same_kernel_edges_surviving**: ~w (dedup-resurfaced ~w, never-stripped ~w)  ~n',
           [M, M1, M2]),
    (   Control == ok
    ->  format('**Positive control**: ok — raw `affects_constraint` dropped by exactly ~w.~n~n',
               [NStrip])
    ;   format('**Positive control**: FAILED — the strip did not reach the substrate by '),
        format('the expected count; the stratum is reported UNDETERMINED below.~n~n')
    ),
    format('| Stratum | Edges | Components | Giant size | Giant fraction |~n'),
    format('|---------|-------|------------|------------|----------------|~n'),
    format('| Pooled | ~w | ~w | ~w | ~3f |~n', [NEP, NCompP, GiantP, GiantFracP]),
    (   Control == ok
    ->  GiantFracS is GiantS / max(1, NNodes),
        ( Label == cross_kernel
        -> LabelStr = 'Cross-kernel'
        ;  LabelStr = 'Explicit-sibling-stripped' ),
        format('| ~w | ~w | ~w | ~w | ~3f |~n~n', [LabelStr, NES, NCompS, GiantS, GiantFracS])
    ;   format('| Stratum | UNDETERMINED | UNDETERMINED | UNDETERMINED | UNDETERMINED |~n~n')
    ),
    (   Label == explicit_sibling_stripped, Control == ok
    ->  format('> **Label note**: `same_kernel_edges_surviving > 0` — the stripped stratum '),
        format('retains same-kernel coupling by inference, so the honest label is '),
        format('`explicit_sibling_stripped`, not `cross_kernel`. Operator ruling owed on '),
        format('whether retaining inferred same-kernel coupling in the stratum is intended.~n~n')
    ;   true
    ).

%% write_provenance_split_json(+Split)
%  Emit ../outputs/giant_component_analysis.raw.json (cwd-relative; run_pipeline
%  enforces cwd=PROLOG_DIR). stratum + per-constraint stratum fields are null when
%  the positive control failed.
write_provenance_split_json(Split) :-
    Split = split(Control, NNodes, NStrip,
                  pooled(NEP, NCompP, GiantP),
                  stratum(NES, NCompS, GiantS),
                  surviving(M, M1, M2),
                  Label, PooledRows, StratumRows, CtxAtom),
    (   Control == ok
    ->  StratumDict = _{n_edges: NES, n_components: NCompS, giant_size: GiantS},
        ControlAtom = ok
    ;   StratumDict = null,
        ControlAtom = failed
    ),
    per_constraint_json(PooledRows, StratumRows, Control, PerConstraint),
    Out = _{
        pooled: _{n_nodes: NNodes, n_edges: NEP, n_components: NCompP, giant_size: GiantP},
        stratum: StratumDict,
        stratum_label: Label,
        n_sibling_edges_stripped: NStrip,
        same_kernel_edges_surviving: M,
        surviving_dedup_resurfaced: M1,
        surviving_never_stripped: M2,
        positive_control: ControlAtom,
        context: CtxAtom,
        per_constraint: PerConstraint
    },
    RawPath = '../outputs/giant_component_analysis.raw.json',
    setup_call_cleanup(
        open(RawPath, write, S),
        json_write_dict(S, Out, [width(80), null(null), true(true), false(false)]),
        close(S)
    ),
    dict_pairs(PerConstraint, _, PCPairs),
    length(PCPairs, NPC),
    format(user_error, '[giant] wrote ~w (per_constraint=~w, control=~w, label=~w)~n',
           [RawPath, NPC, ControlAtom, Label]).

%% per_constraint_json(+PooledRows, +StratumRows, +Control, -Dict)
%  PooledRows and StratumRows are both keyed by C in the same all_corpus_constraints
%  order, so they zip position-wise; merge_row unifies the two C args (a free
%  key-alignment check).
per_constraint_json(PooledRows, StratumRows, ok, Dict) :-
    maplist(merge_row, PooledRows, StratumRows, Pairs),
    dict_pairs(Dict, _, Pairs).
per_constraint_json(PooledRows, _StratumRows, failed, Dict) :-
    maplist(pooled_only_row, PooledRows, Pairs),
    dict_pairs(Dict, _, Pairs).

merge_row(C-row(SzP, InGP, DP), C-row(SzS, InGS, DS),
          C-_{component_size_pooled: SzP, in_giant_pooled: InGP, degree_pooled: DP,
              component_size_stratum: SzS, in_giant_stratum: InGS, degree_stratum: DS}).

pooled_only_row(C-row(SzP, InGP, DP),
          C-_{component_size_pooled: SzP, in_giant_pooled: InGP, degree_pooled: DP,
              component_size_stratum: null, in_giant_stratum: null, degree_stratum: null}).

%% run_provenance_split
%  Compute once, render md, write JSON. Plain conjunction (no catch): a failure
%  fails run_giant_component_analysis -> swipl nonzero -> PrologError.
run_provenance_split :-
    constraint_indexing:default_context(Ctx),
    compute_provenance_split(Ctx, Split),
    report_provenance_split(Split),
    write_provenance_split_json(Split).

/* ================================================================
   MAIN ENTRY POINT
   ================================================================ */

run_giant_component_analysis :-
    format('# Giant Component Analysis: Erdos-Renyi Phase Transition~n~n'),
    format('*Investigates whether the constraint network exhibits a phase transition*  ~n'),
    format('*in connected component structure as coupling threshold varies.*~n~n'),
    format('---~n~n'),
    run_phase1,
    format('---~n~n'),
    run_phase2,
    format('---~n~n'),
    run_phase3,
    format('---~n~n'),
    run_phase4,
    format('---~n~n'),
    report_embedded_facts,
    format('---~n~n'),
    run_provenance_split,
    format('---~n~n'),
    format('*End of giant component analysis*~n').
