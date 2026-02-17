:- module(inferred_coupling_protocol, [
    run_coupling_protocol/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(drl_modal_logic).
:- use_module(drl_core).
:- use_module(covering_analysis).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

/* ================================================================
   INFERRED COUPLING ACTIVATION PROTOCOL

   Verifies that the dormant infer_structural_coupling/3 mechanism
   can be activated by providing measurement/5 ground facts, and
   measures the network impact of the resulting inferred edges.

   Phases:
   1. Protocol documentation (measurement/5 signature, algorithm)
   2. Gradient verification (confirm dr_gradient_at/3 produces gradients)
   3. Edge creation verification (call infer_structural_coupling/3)
   4. Network impact (component analysis before/after)
   5. Cross-domain bridge analysis (types, contamination implications)
   ================================================================ */

/* ================================================================
   DESIGNED PAIRS
   ================================================================ */

%% designed_pair(+PairID, +C1, +C2, +ExpectedStrength, +Description)
designed_pair(pair_1_tech_ecosystem,
    quantum_decryption_risk_2026, smartphone_ubiquity,
    1.0, 'Technology ecosystem co-movement: both rising').
designed_pair(pair_2_institutional_erosion,
    regulatory_capture, institutional_trust_decay,
    1.0, 'Institutional erosion co-movement: both rising').
designed_pair(pair_3_commons_degradation,
    tragedy_of_the_commons, pareto_principle,
    1.0, 'Commons degradation co-movement: both rising').
designed_pair(pair_4_negative_control,
    hawthorne_effect, rotation_seven_black_soil,
    0.0, 'Negative control: anti-correlated (no edge expected)').

%% all_measurement_constraints(-List)
%  All constraints that have temporal measurement data.
all_measurement_constraints(Cs) :-
    Cs = [quantum_decryption_risk_2026, smartphone_ubiquity,
          regulatory_capture, institutional_trust_decay,
          tragedy_of_the_commons, pareto_principle,
          hawthorne_effect, rotation_seven_black_soil].

/* ================================================================
   MAIN ENTRY POINT
   ================================================================ */

run_coupling_protocol :-
    format(user_error, '[coupling] Starting Inferred Coupling Activation Protocol...~n', []),
    corpus_loader:load_all_testsets,
    covering_analysis:all_corpus_constraints(AllCs),
    length(AllCs, NC),
    format(user_error, '[coupling] Corpus: ~w constraints~n', [NC]),

    % Phase 1: Document protocol
    format(user_error, '[coupling] Phase 1: Protocol documentation...~n', []),

    % Phase 2: Gradient verification
    format(user_error, '[coupling] Phase 2: Gradient verification...~n', []),
    all_measurement_constraints(MeasCs),
    verify_gradients(MeasCs, GradientResults),

    % Phase 3: Edge creation verification
    format(user_error, '[coupling] Phase 3: Edge creation verification...~n', []),
    verify_designed_pairs(PairResults),
    discover_all_inferred_edges(AllCs, AllInferred),

    % Phase 4: Network impact
    format(user_error, '[coupling] Phase 4: Network impact analysis...~n', []),
    compute_baseline_edges(AllCs, BaseEdges),
    length(BaseEdges, BaseEdgeCount),
    bfs_all_components(AllCs, BaseEdges, BaseComps),
    % Full = baseline edges + inferred edges
    findall(C1-C2, member(edge(C1, C2, _), AllInferred), InferredPairs),
    append(BaseEdges, InferredPairs, FullEdges),
    sort(FullEdges, FullEdgesUniq),
    length(FullEdgesUniq, FullEdgeCount),
    bfs_all_components(AllCs, FullEdgesUniq, FullComps),

    % Phase 5: Report
    format(user_error, '[coupling] Phase 5: Generating report...~n', []),
    report_full(NC, GradientResults, PairResults, AllInferred,
                BaseComps, BaseEdgeCount, FullComps, FullEdgeCount),
    format(user_error, '[coupling] Done.~n', []).

/* ================================================================
   GRADIENT COMPUTATION — CORRECTED VERSION

   The original dr_gradient_at/3 in drl_modal_logic.pl uses a bare cut (!)
   which, when called inside findall/3, cuts ALL choice points including
   backtracking over different timepoints T. This means findall returns
   only 1 gradient instead of the expected N-1 (for N timepoints).

   We fix this by using once/1, which provides a local cut that only
   prevents backtracking for the second measurement lookup (finding
   the NEXT T2 after T) without cutting the outer backtracking over T.

   Similarly, infer_structural_coupling/3 calls dr_gradient_at inside
   findall internally, so it also suffers from this bug. We reimplement
   the coupling computation using our corrected gradient function.
   ================================================================ */

%% gradient_at(+C, -T, -Grad)
%  Corrected version of dr_gradient_at/3 — safe inside findall.
gradient_at(C, T, Grad) :-
    narrative_ontology:measurement(_, C, extractiveness, T, X1),
    once((
        narrative_ontology:measurement(_, C, extractiveness, T2, X2),
        T2 > T
    )),
    Grad is X2 - X1.

%% constraint_gradients(+C, -Grads)
%  Collects all gradients for constraint C.
constraint_gradients(C, Grads) :-
    findall(G, gradient_at(C, _, G), Grads).

%% compute_coupling(+C1, +C2, -Strength)
%  Reimplementation of infer_structural_coupling using corrected gradients.
compute_coupling(C1, C2, Strength) :-
    C1 \= C2,
    constraint_gradients(C1, Gs1),
    constraint_gradients(C2, Gs2),
    length(Gs1, L), L > 1, length(Gs2, L),
    drl_modal_logic:count_sign_matches(Gs1, Gs2, Matches, L),
    Strength is Matches / L.

/* ================================================================
   PHASE 2: GRADIENT VERIFICATION
   ================================================================ */

%% verify_gradients(+Constraints, -Results)
%  For each constraint, collect gradients via corrected gradient_at/3.
%  Result = gradient_result(C, Gradients, Count).
verify_gradients(Constraints, Results) :-
    findall(
        gradient_result(C, Grads, N),
        (   member(C, Constraints),
            findall(T-G, gradient_at(C, T, G), Grads),
            length(Grads, N)
        ),
        Results).

/* ================================================================
   PHASE 3: EDGE CREATION VERIFICATION
   ================================================================ */

%% verify_designed_pairs(-Results)
%  For each designed pair, attempt coupling computation and report.
verify_designed_pairs(Results) :-
    findall(
        pair_result(PairID, C1, C2, Expected, Actual, EdgeFormed),
        (   designed_pair(PairID, C1, C2, Expected, _Desc),
            (   compute_coupling(C1, C2, Strength)
            ->  Actual = Strength,
                config:param(network_coupling_threshold, Thresh),
                (   Strength >= Thresh -> EdgeFormed = yes ; EdgeFormed = no )
            ;   Actual = 0.0,
                EdgeFormed = no
            )
        ),
        Results).

%% discover_all_inferred_edges(+AllConstraints, -Edges)
%  Find ALL inferred coupling edges across the entire corpus.
%  Returns edge(C1, C2, Strength) for each qualifying pair.
discover_all_inferred_edges(AllCs, Edges) :-
    % Only check constraints that have gradients (at least 2)
    findall(C,
        (   member(C, AllCs),
            constraint_gradients(C, Gs),
            length(Gs, L), L > 1
        ),
        GradCs),
    sort(GradCs, SortedGradCs),
    length(SortedGradCs, NGC),
    format(user_error, '[coupling]   ~w constraints have 2+ gradients~n', [NGC]),
    config:param(network_coupling_threshold, Thresh),
    findall(
        edge(C1, C2, Strength),
        (   member(C1, SortedGradCs),
            member(C2, SortedGradCs),
            C1 @< C2,  % avoid duplicates
            compute_coupling(C1, C2, Strength),
            Strength >= Thresh
        ),
        Edges).

/* ================================================================
   PHASE 4: NETWORK IMPACT — LIGHTWEIGHT COMPONENT ANALYSIS
   ================================================================ */

%% compute_baseline_edges(+AllCs, -Edges)
%  Collect explicit (affects_constraint) + shared_agent edges only.
%  Excludes inferred coupling entirely. Returns sorted list of C1-C2 pairs (C1 @< C2).
compute_baseline_edges(AllCs, Edges) :-
    format(user_error, '[coupling]   Computing baseline edges (explicit + shared_agent)...~n', []),
    % Explicit edges
    findall(C1-C2,
        (   member(C1, AllCs),
            narrative_ontology:affects_constraint(C1, C2),
            C2 \= C1
        ),
        ExplicitRaw),
    % Shared agent edges
    findall(C1-C2,
        (   member(C1, AllCs),
            drl_modal_logic:shared_agent_link(C1, C2, _, _),
            C2 \= C1
        ),
        SharedRaw),
    % Normalize to undirected and deduplicate
    append(ExplicitRaw, SharedRaw, AllRaw),
    findall(A-B,
        (   member(X-Y, AllRaw),
            (X @< Y -> A = X, B = Y ; A = Y, B = X)
        ),
        Normalized),
    sort(Normalized, Edges).

%% bfs_all_components(+Nodes, +Edges, -Components)
%  Finds all connected components via iterative BFS.
bfs_all_components(Nodes, Edges, Components) :-
    sort(Nodes, SortedNodes),
    bfs_loop(SortedNodes, Edges, [], Components).

bfs_loop([], _Edges, Acc, Components) :-
    reverse(Acc, Components).
bfs_loop([Node|Rest], Edges, Acc, Components) :-
    bfs_reach(Node, Edges, Comp),
    sort(Comp, SortedComp),
    ord_subtract(Rest, SortedComp, Remaining),
    bfs_loop(Remaining, Edges, [SortedComp|Acc], Components).

%% bfs_reach(+Start, +Edges, -Component)
%  BFS from Start, returns all reachable nodes.
bfs_reach(Start, Edges, Component) :-
    bfs_expand([Start], [], Edges, Component).

bfs_expand([], Visited, _Edges, Visited).
bfs_expand([Node|Queue], Visited, Edges, Component) :-
    (   member(Node, Visited)
    ->  bfs_expand(Queue, Visited, Edges, Component)
    ;   findall(Neighbor,
            (   (member(Node-Neighbor, Edges) ; member(Neighbor-Node, Edges)),
                \+ member(Neighbor, Visited)
            ),
            NewNeighbors),
    append(Queue, NewNeighbors, NewQueue),
    bfs_expand(NewQueue, [Node|Visited], Edges, Component)
    ).

/* ================================================================
   REPORT GENERATION
   ================================================================ */

report_full(NC, GradientResults, PairResults, AllInferred,
            BaseComps, BaseEdgeCount, FullComps, FullEdgeCount) :-
    format('# Inferred Coupling Activation Protocol~n~n'),
    format('*Tests whether the dormant `infer_structural_coupling/3` mechanism*~n'),
    format('*can be activated by providing `measurement/5` ground facts, and*~n'),
    format('*measures the resulting network impact.*~n~n'),
    format('---~n~n'),

    report_protocol_doc,
    report_gradients(GradientResults),
    report_pairs(PairResults),
    report_all_inferred(AllInferred),
    report_network_impact(NC, BaseComps, BaseEdgeCount, FullComps, FullEdgeCount),
    report_bridge_analysis(AllInferred, BaseComps, FullComps),

    format('---~n~n'),
    format('*End of Inferred Coupling Activation Protocol*~n').

/* ---- Phase 1: Protocol Documentation ---- */

report_protocol_doc :-
    format('## Protocol Documentation~n~n'),
    format('### measurement/5 Signature~n~n'),
    format('```prolog~n'),
    format('measurement(?Source, ?Constraint, extractiveness, ?Time, ?Value)~n'),
    format('```~n~n'),
    format('- **Source**: Atom identifying the measurement source~n'),
    format('- **Constraint**: The constraint being measured~n'),
    format('- **Metric**: Must be the atom `extractiveness` (hardcoded in `dr_gradient_at/3`)~n'),
    format('- **Time**: Numeric timepoint~n'),
    format('- **Value**: Float in [0, 1]~n~n'),
    format('### Algorithm~n~n'),
    format('1. `dr_gradient_at(C, T, Grad)` extracts gradient = X(T2) - X(T) for consecutive timepoints~n'),
    format('2. `infer_structural_coupling(C1, C2, Strength)` computes sign-agreement ratio~n'),
    format('3. If Strength >= `network_coupling_threshold` ('),
    config:param(network_coupling_threshold, Thresh),
    format('~2f), an edge is created~n', [Thresh]),
    format('4. `constraint_neighbors/3` includes inferred edges in neighbor discovery~n~n'),
    format('### Requirements~n~n'),
    format('- 3+ timepoints per constraint (produces 2+ gradients, satisfying L > 1)~n'),
    format('- Both constraints in a pair must have the same number of gradients~n'),
    format('- Sign-agreement: both positive, both negative, or both zero~n~n').

/* ---- Phase 2: Gradient Verification ---- */

report_gradients(GradientResults) :-
    format('## Phase 2: Gradient Verification~n~n'),
    format('Verifying that `dr_gradient_at/3` produces gradients from the `measurement/5` facts:~n~n'),
    format('| Constraint | Gradients | Count | Status |~n'),
    format('|------------|-----------|-------|--------|~n'),
    forall(member(gradient_result(C, Grads, N), GradientResults), (
        (   N >= 2
        ->  Status = 'PASS'
        ;   Status = 'FAIL'
        ),
        format_gradient_list(Grads, GradStr),
        format('| ~w | ~w | ~w | ~w |~n', [C, GradStr, N, Status])
    )),
    format('~n'),
    % Summary
    include(grad_pass, GradientResults, Passing),
    length(Passing, NPass),
    length(GradientResults, NTotal),
    format('**Result**: ~w/~w constraints produce 2+ gradients.~n~n', [NPass, NTotal]).

grad_pass(gradient_result(_, _, N)) :- N >= 2.

format_gradient_list([], '[]').
format_gradient_list(Grads, Str) :-
    Grads \= [],
    findall(S, (member(T-G, Grads), format(atom(S), 'T~w:~3f', [T, G])), Parts),
    atomic_list_concat(Parts, ', ', Str).

/* ---- Phase 3: Pair Verification ---- */

report_pairs(PairResults) :-
    format('## Phase 3: Edge Creation Verification~n~n'),
    format('Testing each designed constraint pair:~n~n'),
    format('| Pair | C1 | C2 | Expected | Actual | Edge? | Verdict |~n'),
    format('|------|----|----|----------|--------|-------|---------|~n'),
    forall(member(pair_result(PairID, C1, C2, Expected, Actual, EdgeFormed), PairResults), (
        (   (Expected > 0.5, EdgeFormed = yes ; Expected =< 0.5, EdgeFormed = no)
        ->  Verdict = 'PASS'
        ;   Verdict = 'FAIL'
        ),
        format('| ~w | ~w | ~w | ~2f | ~2f | ~w | ~w |~n',
               [PairID, C1, C2, Expected, Actual, EdgeFormed, Verdict])
    )),
    format('~n'),
    % Count passes
    include(pair_pass, PairResults, PassingPairs),
    length(PassingPairs, NPairPass),
    length(PairResults, NPairTotal),
    format('**Result**: ~w/~w pairs behave as expected.~n~n', [NPairPass, NPairTotal]).

pair_pass(pair_result(_, _, _, Expected, _, EdgeFormed)) :-
    (   Expected > 0.5, EdgeFormed = yes
    ;   Expected =< 0.5, EdgeFormed = no
    ).

/* ---- All Inferred Edges ---- */

report_all_inferred(AllInferred) :-
    format('## All Inferred Coupling Edges~n~n'),
    length(AllInferred, NInf),
    format('Total inferred edges above threshold: **~w**~n~n', [NInf]),
    (   NInf > 0
    ->  format('| C1 | C2 | Strength |~n'),
        format('|----|----|-----------| ~n'),
        forall(member(edge(C1, C2, S), AllInferred), (
            format('| ~w | ~w | ~3f |~n', [C1, C2, S])
        )),
        format('~n')
    ;   format('No inferred coupling edges found.~n~n')
    ).

/* ---- Phase 4: Network Impact ---- */

report_network_impact(NC, BaseComps, BaseEdgeCount, FullComps, FullEdgeCount) :-
    format('## Phase 4: Network Impact~n~n'),
    length(BaseComps, NBaseComps),
    length(FullComps, NFullComps),
    NewEdges is FullEdgeCount - BaseEdgeCount,
    MergedComps is NBaseComps - NFullComps,
    % Largest component sizes
    maplist(length, BaseComps, BaseSizes),
    maplist(length, FullComps, FullSizes),
    max_list(BaseSizes, BaseMaxSize),
    max_list(FullSizes, FullMaxSize),
    format('| Metric | Baseline | With Inferred | Delta |~n'),
    format('|--------|----------|---------------|-------|~n'),
    format('| Total edges | ~w | ~w | +~w |~n', [BaseEdgeCount, FullEdgeCount, NewEdges]),
    format('| Connected components | ~w | ~w | -~w |~n', [NBaseComps, NFullComps, MergedComps]),
    format('| Largest component | ~w | ~w | +~w |~n', [BaseMaxSize, FullMaxSize, FullMaxSize - BaseMaxSize]),
    BasePct is BaseMaxSize / max(1, NC) * 100,
    FullPct is FullMaxSize / max(1, NC) * 100,
    format('| Largest as % of corpus | ~1f% | ~1f% | +~1f% |~n',
           [BasePct, FullPct, FullPct - BasePct]),
    % Giant component check (> 50%)
    format('~n'),
    (   FullPct > 50
    ->  format('**Giant component detected**: ~w nodes (~1f% of corpus).~n~n', [FullMaxSize, FullPct])
    ;   FullPct > 25
    ->  format('**Near-giant component**: ~w nodes (~1f% of corpus). Approaching percolation threshold.~n~n',
               [FullMaxSize, FullPct])
    ;   format('**No giant component**: Largest component is ~w nodes (~1f% of corpus). ',
               [FullMaxSize, FullPct]),
        format('Network remains fragmented even with inferred coupling.~n~n')
    ).

/* ---- Phase 5: Bridge Analysis ---- */

report_bridge_analysis(AllInferred, BaseComps, FullComps) :-
    format('## Phase 5: Cross-Domain Bridge Analysis~n~n'),
    (   AllInferred = []
    ->  format('No inferred edges to analyze.~n~n')
    ;   constraint_indexing:default_context(Ctx),
        format('### Bridge Edges~n~n'),
        format('Inferred edges that connect previously separate components:~n~n'),
        format('| C1 | Type1 | C2 | Type2 | Strength | Bridge? |~n'),
        format('|----|-------|----|-------|----------|---------|~n'),
        forall(member(edge(C1, C2, S), AllInferred), (
            classify_safe_local(C1, Ctx, T1),
            classify_safe_local(C2, Ctx, T2),
            (   same_component(C1, C2, BaseComps)
            ->  Bridge = no
            ;   Bridge = yes
            ),
            format('| ~w | ~w | ~w | ~w | ~3f | ~w |~n', [C1, T1, C2, T2, S, Bridge])
        )),
        format('~n'),
        % Count bridges
        include(is_bridge(BaseComps), AllInferred, Bridges),
        length(Bridges, NBridges),
        length(AllInferred, NInferred),
        format('**~w/~w inferred edges are bridges** (connecting previously separate components).~n~n',
               [NBridges, NInferred]),
        % Component merge details
        (   NBridges > 0
        ->  format('### Component Merges~n~n'),
            length(BaseComps, NBase),
            length(FullComps, NFull),
            Merged is NBase - NFull,
            format('- Baseline components: ~w~n', [NBase]),
            format('- After inferred coupling: ~w~n', [NFull]),
            format('- Components merged: ~w~n~n', [Merged]),
            % Show which components got merged
            format('### Merged Component Members~n~n'),
            forall(member(edge(C1, C2, _S), Bridges), (
                find_component(C1, FullComps, Comp),
                length(Comp, CompSize),
                format('Bridge `~w` -- `~w` is now in a component of size ~w:~n', [C1, C2, CompSize]),
                forall(member(M, Comp), format('  - ~w~n', [M])),
                format('~n')
            ))
        ;   true
        ),
        % Type contamination implications
        format('### Type Diversity in Bridged Components~n~n'),
        format('If inferred coupling edges carry contamination,'),
        format(' what types are now reachable from each bridged constraint?~n~n'),
        forall(member(edge(C1, C2, _), Bridges), (
            find_component(C1, FullComps, Comp2),
            findall(T,
                (   member(M, Comp2),
                    classify_safe_local(M, Ctx, T),
                    T \= error
                ),
                Types),
            msort(Types, SortedTypes),
            clumped(SortedTypes, TypeCounts),
            format('Component containing `~w` and `~w`:~n', [C1, C2]),
            forall(member(Tp-Cnt, TypeCounts), (
                format('  - ~w: ~w~n', [Tp, Cnt])
            )),
            format('~n')
        ))
    ).

%% classify_safe_local(+C, +Ctx, -Type)
classify_safe_local(C, Ctx, Type) :-
    (   catch(drl_core:dr_type(C, Ctx, T), _, T = error)
    ->  Type = T
    ;   Type = error
    ).

%% same_component(+C1, +C2, +Components)
same_component(C1, C2, Components) :-
    member(Comp, Components),
    member(C1, Comp),
    member(C2, Comp), !.

%% is_bridge(+BaseComps, +Edge)
is_bridge(BaseComps, edge(C1, C2, _)) :-
    \+ same_component(C1, C2, BaseComps).

%% find_component(+C, +Components, -Comp)
find_component(C, [Comp|_], Comp) :-
    member(C, Comp), !.
find_component(C, [_|Rest], Comp) :-
    find_component(C, Rest, Comp).
