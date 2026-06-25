% C1 (family non-degeneracy) + C3-identity (re-run partition stability) + twin-vacuity observation.
% Load: swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
%   -l maxent_classifier.pl -l context_profile_mining.pl -l <this> -g "<goal>, halt." -t "halt(1)"

:- use_module(library(lists)).
:- use_module(library(pairs)).

run_and_partition(Partition, NTwins, NPairs, NDomains) :-
    constraint_indexing:default_context(Context),
    context_profile_mining:trajectory_run(Context, Summary),
    Summary = trajectory_summary(_, _, cross_domain_twins(NTwins), _),
    % canonical partition: sorted list of sorted member-lists, family labels discarded
    findall(FID-C, context_profile_mining:family_assignment(C, FID), Pairs0),
    keysort(Pairs0, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    findall(SMembers, (member(_-Ms, Grouped), sort(Ms, SMembers)), Fams0),
    sort(Fams0, Partition),
    % pair count (n choose 2 over clustered constraints) and distinct name-prefix domains
    findall(C, context_profile_mining:family_assignment(C, _), AllC), length(AllC, N),
    NPairs is N * (N - 1) // 2,
    findall(D, (member(C, AllC), context_profile_mining:constraint_domain(C, D)), Ds),
    sort(Ds, UDs), length(UDs, NDomains).

% --- C1: family-size distribution + degeneracy verdict + twin-vacuity observation ---
c1_distribution :-
    corpus_loader:load_all_testsets,
    run_and_partition(Partition, NTwins, NPairs, NDomains),
    length(Partition, NFam),
    findall(Sz, (member(F, Partition), length(F, Sz)), Sizes0),
    msort(Sizes0, Sizes), max_list(Sizes, MaxSz), min_list(Sizes, MinSz),
    include([X]>>(X =:= 1), Sizes, Singletons), length(Singletons, NSingle),
    sum_list(Sizes, NClustered),
    format("C1 family count        : ~w~n", [NFam]),
    format("C1 clustered constraints: ~w~n", [NClustered]),
    format("C1 family sizes (sorted): ~w~n", [Sizes]),
    format("C1 max family size      : ~w  (min ~w)~n", [MaxSz, MinSz]),
    format("C1 singletons           : ~w~n", [NSingle]),
    ( NFam >= 2, MaxSz < NClustered
      -> format("C1 VERDICT: NON-DEGENERATE (not all-singletons, not one giant cluster)~n", [])
      ;  format("C1 VERDICT: DEGENERATE -- ESCALATE (cut-height mismatch vs subsystem failure)~n", []) ),
    nl,
    format("TWIN-VACUITY OBSERVATION (parallel; NOT a family gate):~n", []),
    format("  cross_domain_twins      : ~w~n", [NTwins]),
    format("  total clustered pairs   : ~w~n", [NPairs]),
    Frac is NTwins / max(1, NPairs),
    format("  twin fraction of pairs  : ~4f~n", [Frac]),
    format("  distinct name-prefix domains: ~w (gate D1\\=D2 near-vacuous => twin product OPEN)~n", [NDomains]).

% --- C3-identity: print canonical partition signature for cross-process diff ---
c3_partition_signature :-
    corpus_loader:load_all_testsets,
    run_and_partition(Partition, _, _, _),
    term_to_atom(Partition, A),
    format("PARTITION_SIG: ~w~n", [A]).
