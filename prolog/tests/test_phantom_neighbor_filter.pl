% ============================================================================
% test_phantom_neighbor_filter.pl — OQ-95 phantom-node filter regression tests.
%
% Covers the fail-closed neighbor filter in drl_purity_network:
% constraint_neighbors/3 (phantom_neighbor/1): an affects_constraint/2 fact
% whose target has NO ontology presence (no constraint_claim, no
% constraint_metric — an LLM-authored cross-reference to a testset that does
% not exist) must not appear as a neighbor, in either edge direction.
% Pre-fix, such atoms entered graph walks as traversable nodes: giant_comp
% reported a largest component of 118.9% of the network on the live corpus
% (259.9% on original_v6). Witness: audits/2026-06-10_oq95_phantom_node_fix/.
%
% Three layers:
%   (a) Positive control — the same synthetic edge wired to a REAL corpus
%       constraint IS returned as a neighbor, proving the probe fires and the
%       filter is not silently dropping everything (Build Discipline: a gate
%       over a possibly-empty table must be shown able to pass).
%   (b) Phantom exclusion — forward and reverse: the phantom is absent from
%       the real constraint's neighbor list, and the phantom itself has no
%       neighbors (pre-fix it acquired them via the reverse-edge clause).
%   (c) Corpus-level census — zero phantom endpoints across every loaded
%       constraint's neighbor list at the default context.
%
% Setup asserts synthetic affects_constraint facts; cleanup retracts them.
%
% Run: cd prolog && swipl -g "[stack], [tests/test_phantom_neighbor_filter], run_tests, halt" -t "halt(1)"
% ============================================================================

:- corpus_loader:ensure_corpus_loaded.

:- begin_tests(phantom_neighbor_filter).

% Two real corpus constraints (loaded testsets, have claims + metrics) and
% one atom with zero facts of any kind.
real_a(ai_governance_accountability).
real_b(retirement_security_deficit).
phantom(oq95_test_phantom_target__does_not_exist).

setup_edges :-
    real_a(A), real_b(B), phantom(P),
    assertz(narrative_ontology:affects_constraint(A, P)),
    assertz(narrative_ontology:affects_constraint(A, B)).

cleanup_edges :-
    real_a(A), real_b(B), phantom(P),
    retractall(narrative_ontology:affects_constraint(A, P)),
    retractall(narrative_ontology:affects_constraint(A, B)),
    cache_registry:clear_all_caches.

% (a) Positive control: the synthetic edge to a REAL constraint is returned.
% Proves an explicit affects_constraint edge asserted by this setup reaches
% the neighbor list — so test (b)'s absence is the filter, not a dead probe.
test(real_target_edge_fires,
     [ setup(setup_edges), cleanup(cleanup_edges) ]) :-
    real_a(A), real_b(B),
    constraint_indexing:default_context(Ctx),
    drl_purity_network:constraint_neighbors(A, Ctx, Ns),
    memberchk(neighbor(B, _, _), Ns).

% (b) Phantom exclusion, forward: the zero-fact target is not a neighbor.
test(phantom_target_excluded,
     [ setup(setup_edges), cleanup(cleanup_edges) ]) :-
    real_a(A), phantom(P),
    constraint_indexing:default_context(Ctx),
    drl_purity_network:constraint_neighbors(A, Ctx, Ns),
    \+ memberchk(neighbor(P, _, _), Ns).

% (b) Phantom exclusion, reverse: the phantom acquires no neighbors of its
% own (pre-fix, the ExplicitIn clause handed it the real source, making it
% a traversable node for bfs_path/contamination walks).
test(phantom_not_traversable,
     [ setup(setup_edges), cleanup(cleanup_edges) ]) :-
    phantom(P),
    constraint_indexing:default_context(Ctx),
    drl_purity_network:constraint_neighbors(P, Ctx, Ns),
    Ns == [].

% (c) Corpus-level census: no loaded constraint's neighbor list contains an
% endpoint without ontology presence. Runs on the live corpus as loaded —
% this is the corpus-wide form of the OQ-95 witness (25 phantoms pre-fix).
test(no_phantom_endpoints_corpus_wide) :-
    constraint_indexing:default_context(Ctx),
    findall(C-Other,
            ( corpus_loader:corpus_constraint(C),
              drl_purity_network:constraint_neighbors(C, Ctx, Ns),
              member(neighbor(Other, _, _), Ns),
              \+ narrative_ontology:constraint_claim(Other, _),
              \+ narrative_ontology:constraint_metric(Other, _, _)
            ),
            Phantoms),
    Phantoms == [].

:- end_tests(phantom_neighbor_filter).
