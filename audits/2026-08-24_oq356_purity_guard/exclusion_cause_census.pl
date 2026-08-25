% OQ-356 — of the giant-component members the guard EXCLUDES, how many fail the
% rejecting conjunction for each of its four distinguishable reasons?
%
%   (a) effective_purity SUCCEEDS with a NON-NUMBER   -- the OQ-60 defect class
%   (b) effective_purity THROWS                       -- dropped by the catch/3
%   (c) effective_purity FAILS                        -- the conjunct fails
%   (d) effective_purity returns a NUMBER < 0.0       -- the -1.0 gate-fail
%                                                        sentinel; excluded by
%                                                        the PRE-EXISTING filter,
%                                                        not by the new guard
%
% This is the equivalence requirement measured rather than argued: if the split
% is not all-(a), then an excluded count written as "count the unknowns" would
% have been wrong, and naming the argument NExcluded rather than NUnknown is
% load-bearing.
%
% Reproduces the giant component the report uses: same default coupling
% threshold, same compute_components, first (largest) component.
% (run via -g)

cause(C, Ctx, Cause) :-
    (   catch(drl_purity_network:effective_purity(C, Ctx, EP, _), E, true)
    ->  (   nonvar(E)  -> Cause = b_throws
        ;   \+ number(EP) -> Cause = a_non_number
        ;   EP < 0.0   -> Cause = d_negative_number
        ;   Cause = kept
        )
    ;   Cause = c_fails
    ).

main :-
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    giant_component_analysis:all_corpus_constraints(Cs),
    length(Cs, NC),
    % Reproduce run_phase3's setup EXACTLY (:836-849). A first draft skipped
    % precompute_all_edges/2 + edges_at_threshold/2 + build_adjacency_facts/1
    % and every node came back isolated -- a "giant component" of size 1. That
    % was a probe bug, not a finding; it is recorded here so the corrected
    % version is visibly the one that reproduces the report's own component.
    giant_component_analysis:precompute_all_edges(Cs, Ctx),
    config:param(network_coupling_threshold, CurrThresh),
    giant_component_analysis:edges_at_threshold(CurrThresh, Edges),
    giant_component_analysis:build_adjacency_facts(Edges),
    giant_component_analysis:precompute_node_properties(Cs, Ctx),
    giant_component_analysis:compute_components(Cs, Components),
    Components = [component(GCSize, Members)|_],
    GCFrac is GCSize / max(1, NC),
    format("corpus ~w, giant component ~w (~2f%)~n", [NC, GCSize, GCFrac*100]),
    findall(Cause, (member(M, Members), cause(M, Ctx, Cause)), Causes),
    msort(Causes, Sorted),
    clumped(Sorted, Counts),
    format("cause breakdown over the GC members:~n"),
    forall(member(K-V, Counts), format("   ~w~t~20|~w~n", [K, V])),
    aggregate_all(count, (member(X, Causes), X \== kept), NExcl),
    format("total EXCLUDED: ~w   (kept: ~w)~n", [NExcl, GCSize - NExcl]),
    % --- the point: is the excluded set broader than `unknown`? ---
    ( memberchk(a_non_number-_, Counts) -> A = yes ; A = no ),
    aggregate_all(count, (member(X, Causes), X \== kept, X \== a_non_number), NOther),
    format("excluded for a reason OTHER than 'succeeds with a non-number': ~w~n", [NOther]),
    (   NOther > 0
    ->  format("=> an excluded count written as 'count the unknowns' would MISS ~w member(s).~n", [NOther]),
        format("   Naming the argument NExcluded rather than NUnknown is load-bearing.~n")
    ;   format("=> on THIS leg every exclusion is cause (a); the broader name is not yet~n"),
        format("   discriminated by data here (has_non_number=~w). Report as untested, not as equal.~n", [A])
    ).
