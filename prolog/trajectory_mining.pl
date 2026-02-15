% ============================================================================
% TRAJECTORY PATTERN-MINING SYSTEM (v6.4)
% ============================================================================
% Extends the 24 orbit families (type-only) into richer structural families
% incorporating continuous metrics, entropy, coupling, drift, and fingerprint
% voids. Detects structural isomorphisms — constraints from different domains
% that behave identically under observer shift.
%
% Architecture:
%   Phase 1: Trajectory computation (per-constraint, per-context)
%   Phase 2: Distance metric (4-component weighted distance)
%   Phase 3: Hierarchical agglomerative clustering (average linkage)
%   Phase 4: Isomorphism detection + cross-domain twin discovery
%
% This module is DIAGNOSTIC ONLY. It does not modify any classification,
% purity score, or existing output.
%
% Standalone run:
%   swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
%         -l maxent_classifier.pl -l trajectory_mining.pl \
%         -g "trajectory_mining:trajectory_selftest, halt."
% ============================================================================

:- module(trajectory_mining, [
    % Configuration
    trajectory_enabled/0,

    % Core API (Phase 1)
    trajectory_run/2,               % trajectory_run(Context, Summary)
    constraint_trajectory/3,        % constraint_trajectory(C, Context, Trajectory)
    trajectory_cleanup/0,

    % Distance (Phase 2)
    trajectory_distance/4,          % trajectory_distance(C1, C2, Context, Distance)
    type_distance/3,                % type_distance(T1, T2, D)

    % Clustering (Phase 3)
    trajectory_cluster/3,           % trajectory_cluster(CutLevel, Context, Clusters)
    structural_family/2,            % structural_family(C, FamilyID)
    cluster_members/2,              % cluster_members(FamilyID, Members)

    % Isomorphism (Phase 4)
    structural_isomorphism/4,       % structural_isomorphism(C1, C2, Level, Evidence)
    cross_domain_twins/3,           % cross_domain_twins(Context, Threshold, Pairs)

    % Selftest
    trajectory_selftest/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(dirac_classification).
:- use_module(logical_fingerprint).
:- use_module(drl_lifecycle).

:- use_module(library(lists)).

/* ================================================================
   DYNAMIC FACTS
   ================================================================ */

:- dynamic trajectory_cached/3.     % trajectory_cached(Constraint, Context, Trajectory)
:- dynamic pair_dist/3.             % pair_dist(C1, C2, Distance) — C1 @< C2
:- dynamic cluster_member/2.        % cluster_member(ClusterID, Constraint)
:- dynamic cluster_merge/4.         % cluster_merge(Step, ClusterA, ClusterB, MergeDist)
:- dynamic trajectory_run_info/4.   % trajectory_run_info(Context, NTrajectories, NFamilies, Timestamp)
:- dynamic family_assignment/2.     % family_assignment(Constraint, FamilyID)

/* ================================================================
   CONFIGURATION
   ================================================================ */

trajectory_enabled :-
    config:param(trajectory_enabled, 1).

/* ================================================================
   CLEANUP
   ================================================================ */

trajectory_cleanup :-
    retractall(trajectory_cached(_, _, _)),
    retractall(pair_dist(_, _, _)),
    retractall(cluster_member(_, _)),
    retractall(cluster_merge(_, _, _, _)),
    retractall(trajectory_run_info(_, _, _, _)),
    retractall(family_assignment(_, _)).

/* ================================================================
   PHASE 1: TRAJECTORY COMPUTATION
   ================================================================ */

%% standard_contexts(-Contexts)
%  The 4 standard contexts from dirac_classification, as context terms.
standard_contexts(Contexts) :-
    findall(Ctx, dirac_classification:standard_context(Ctx), Contexts).

%% context_label(+Context, -Label)
%  Human-readable label for a standard context.
context_label(context(agent_power(P), _, _, _), P).

% Categorical: Natural transformation representative — full presheaf evaluation across all site points
%% constraint_trajectory(+Constraint, +Context, -Trajectory)
%  Public API: retrieves a cached trajectory, or computes and caches it.
constraint_trajectory(C, Context, Trajectory) :-
    trajectory_cached(C, Context, Trajectory), !.
constraint_trajectory(C, Context, Trajectory) :-
    compute_trajectory(C, Context, Trajectory),
    assertz(trajectory_cached(C, Context, Trajectory)).

%% compute_trajectory(+Constraint, +BaseContext, -Trajectory)
%  Builds the full trajectory for a single constraint.
compute_trajectory(C, BaseContext, trajectory(C, Points, Summary)) :-
    standard_contexts(Contexts),
    maplist(build_trajectory_point(C, BaseContext), Contexts, Points),
    build_trajectory_summary(C, Summary).

%% build_trajectory_point(+Constraint, +BaseContext, +Context, -Point)
%  Assembles one trajectory point from existing predicates.
build_trajectory_point(C, BaseContext, Ctx, Point) :-
    context_label(Ctx, Label),
    % Type from deterministic classifier
    (   catch(drl_core:dr_type(C, Ctx, Type), _, fail)
    ->  true
    ;   Type = unknown
    ),
    % Chi (extractiveness for agent)
    (   catch(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0), _, fail)
    ->  Chi = Chi0
    ;   Chi = 0.0
    ),
    % MaxEnt distribution and entropy for this context
    get_maxent_for_context(C, Ctx, BaseContext, Dist, HNorm, Conf),
    Point = trajectory_point(
        context(Label),
        type(Type),
        chi(Chi),
        maxent(Dist),
        entropy(HNorm),
        confidence(Conf)
    ).

%% get_maxent_for_context(+C, +Ctx, +BaseContext, -Dist, -HNorm, -Conf)
%  Gets MaxEnt data for a specific context. Falls back to base context,
%  then to unavailable.
get_maxent_for_context(C, Ctx, _BaseContext, Dist, HNorm, Conf) :-
    catch(maxent_classifier:maxent_distribution(C, Ctx, Dist), _, fail),
    catch(maxent_classifier:maxent_entropy(C, Ctx, HNorm), _, fail),
    Conf is 1.0 - HNorm, !.
get_maxent_for_context(C, _Ctx, BaseContext, Dist, HNorm, Conf) :-
    catch(maxent_classifier:maxent_distribution(C, BaseContext, Dist), _, fail),
    catch(maxent_classifier:maxent_entropy(C, BaseContext, HNorm), _, fail),
    Conf is 1.0 - HNorm, !.
get_maxent_for_context(_, _, _, unavailable, unavailable, unavailable).

%% build_trajectory_summary(+Constraint, -Summary)
%  Aggregates context-invariant signals.
build_trajectory_summary(C, Summary) :-
    % Orbit family
    (   catch(dirac_classification:gauge_orbit(C, OrbitPoints), _, fail)
    ->  findall(T, member(orbit_point(T, _), OrbitPoints), Types),
        sort(Types, OrbitFamily)
    ;   OrbitFamily = [unknown]
    ),
    % Shift pattern
    (   catch(logical_fingerprint:fingerprint_shift(C, Shift), _, fail)
    ->  true
    ;   Shift = shift(unknown, unknown, unknown, unknown)
    ),
    % Preservation
    (   catch(dirac_classification:preserved_under_context_shift(C, Preservation), _, fail)
    ->  true
    ;   Preservation = unknown
    ),
    % Coupling
    (   catch(structural_signatures:cross_index_coupling(C, Coupling), _, fail)
    ->  true
    ;   Coupling = 0.0
    ),
    % Purity
    (   catch(structural_signatures:purity_score(C, Purity), _, fail)
    ->  true
    ;   Purity = -1.0
    ),
    % Signature
    (   catch(structural_signatures:constraint_signature(C, Sig), _, fail)
    ->  true
    ;   Sig = unknown
    ),
    % Drift events
    (   catch(drl_lifecycle:scan_constraint_drift(C, DriftEvents), _, fail)
    ->  true
    ;   DriftEvents = []
    ),
    length(DriftEvents, DriftCount),
    max_drift_severity(DriftEvents, MaxSeverity),
    % Voids
    (   catch(logical_fingerprint:fingerprint_voids(C, Voids), _, fail)
    ->  true
    ;   Voids = []
    ),
    % Zone
    (   catch(logical_fingerprint:fingerprint_zone(C, Zone), _, fail)
    ->  true
    ;   Zone = zone(unknown, unknown)
    ),
    % Boltzmann compliance
    (   catch(structural_signatures:boltzmann_compliant(C, Boltzmann), _, fail)
    ->  true
    ;   Boltzmann = inconclusive(no_data)
    ),
    Summary = trajectory_summary(
        orbit_family(OrbitFamily),
        shift(Shift),
        preservation(Preservation),
        coupling(Coupling),
        purity(Purity),
        signature(Sig),
        drift(DriftEvents),
        drift_count(DriftCount),
        drift_max_severity(MaxSeverity),
        voids(Voids),
        zone(Zone),
        boltzmann(Boltzmann)
    ).

%% max_drift_severity(+DriftEvents, -MaxSeverity)
max_drift_severity([], none).
max_drift_severity(Events, MaxSev) :-
    findall(S, member(drift(_, _, S), Events), Sevs),
    (   Sevs = []
    ->  MaxSev = none
    ;   severity_max(Sevs, MaxSev)
    ).

severity_max(Sevs, Max) :-
    (   member(critical, Sevs) -> Max = critical
    ;   member(warning, Sevs) -> Max = warning
    ;   member(watch, Sevs) -> Max = watch
    ;   Max = none
    ).

%% compute_all_trajectories(+Context)
%  Iterates over the corpus, computes and caches trajectories.
compute_all_trajectories(Context) :-
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C), atom(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),
    length(Constraints, N),
    format(user_error, '[trajectory] Computing ~w trajectories...~n', [N]),
    compute_trajectories_list(Constraints, Context, 0, N).

compute_trajectories_list([], _, _, _).
compute_trajectories_list([C|Rest], Context, Done, Total) :-
    (   compute_trajectory(C, Context, Trajectory)
    ->  assertz(trajectory_cached(C, Context, Trajectory))
    ;   true  % Skip constraints where computation fails
    ),
    Done1 is Done + 1,
    (   0 =:= Done1 mod 200
    ->  format(user_error, '[trajectory] ~w/~w computed...~n', [Done1, Total])
    ;   true
    ),
    compute_trajectories_list(Rest, Context, Done1, Total).

/* ================================================================
   PHASE 2: DISTANCE METRIC
   ================================================================ */

%% type_distance(+T1, +T2, -Distance)
%  Type-pair distance matrix from the design document.
%  Reflects the Dirac first-class/second-class decomposition.
type_distance(T, T, 0.0) :- !.
type_distance(T1, T2, D) :-
    (T1 @< T2 -> A = T1, B = T2 ; A = T2, B = T1),
    type_distance_lookup(A, B, D), !.
type_distance(_, _, 0.5).  % Default for unknown type pairs

type_distance_lookup(mountain, piton, 0.7).
type_distance_lookup(mountain, rope, 0.8).
type_distance_lookup(mountain, scaffold, 0.7).
type_distance_lookup(mountain, snare, 1.0).
type_distance_lookup(mountain, tangled_rope, 0.9).
type_distance_lookup(mountain, unknown, 0.5).
type_distance_lookup(mountain, indexically_opaque, 0.5).
type_distance_lookup(piton, rope, 0.4).
type_distance_lookup(piton, scaffold, 0.3).
type_distance_lookup(piton, snare, 0.6).
type_distance_lookup(piton, tangled_rope, 0.5).
type_distance_lookup(piton, unknown, 0.4).
type_distance_lookup(piton, indexically_opaque, 0.4).
type_distance_lookup(rope, scaffold, 0.2).
type_distance_lookup(rope, snare, 0.6).
type_distance_lookup(rope, tangled_rope, 0.3).
type_distance_lookup(rope, unknown, 0.4).
type_distance_lookup(rope, indexically_opaque, 0.4).
type_distance_lookup(scaffold, snare, 0.5).
type_distance_lookup(scaffold, tangled_rope, 0.4).
type_distance_lookup(scaffold, unknown, 0.4).
type_distance_lookup(scaffold, indexically_opaque, 0.4).
type_distance_lookup(snare, tangled_rope, 0.3).
type_distance_lookup(snare, unknown, 0.4).
type_distance_lookup(snare, indexically_opaque, 0.4).
type_distance_lookup(tangled_rope, unknown, 0.4).
type_distance_lookup(tangled_rope, indexically_opaque, 0.4).
type_distance_lookup(indexically_opaque, unknown, 0.3).

%% trajectory_distance(+C1, +C2, +Context, -Distance)
%  Computes the weighted 4-component distance between two trajectories.
trajectory_distance(C1, C2, Context, Distance) :-
    ensure_trajectory(C1, Context, T1),
    ensure_trajectory(C2, Context, T2),
    shift_distance(T1, T2, DShift),
    metric_distance(T1, T2, DMetric),
    stability_distance(T1, T2, DStability),
    pathology_distance(T1, T2, DPathology),
    config:param(trajectory_distance_shift_weight, WShift),
    config:param(trajectory_distance_metric_weight, WMetric),
    config:param(trajectory_distance_stability_weight, WStability),
    config:param(trajectory_distance_pathology_weight, WPathology),
    Distance is WShift * DShift + WMetric * DMetric
             + WStability * DStability + WPathology * DPathology.

%% ensure_trajectory(+C, +Context, -Trajectory)
ensure_trajectory(C, Context, T) :-
    trajectory_cached(C, Context, T), !.
ensure_trajectory(C, Context, T) :-
    compute_trajectory(C, Context, T),
    assertz(trajectory_cached(C, Context, T)).

%% Extract types from trajectory points
get_trajectory_types(trajectory(_, Points, _), Types) :-
    maplist(point_type, Points, Types).

point_type(trajectory_point(_, type(T), _, _, _, _), T).

%% Extract chi values from trajectory points
get_trajectory_chis(trajectory(_, Points, _), Chis) :-
    maplist(point_chi, Points, Chis).

point_chi(trajectory_point(_, _, chi(Chi), _, _, _), Chi).

%% Extract entropy values from trajectory points
get_trajectory_entropies(trajectory(_, Points, _), Es) :-
    maplist(point_entropy, Points, Es).

point_entropy(trajectory_point(_, _, _, _, entropy(E), _), E).

%% Extract confidence values from trajectory points
get_trajectory_confidences(trajectory(_, Points, _), Cs) :-
    maplist(point_confidence, Points, Cs).

point_confidence(trajectory_point(_, _, _, _, _, confidence(C)), C).

%% Component 1: Shift distance
shift_distance(T1, T2, DShift) :-
    get_trajectory_types(T1, Types1),
    get_trajectory_types(T2, Types2),
    maplist(type_distance, Types1, Types2, Dists),
    sum_list(Dists, Sum),
    length(Dists, N),
    (N > 0 -> DShift is Sum / N ; DShift = 0.0).

%% Component 2: Metric distance
metric_distance(T1, T2, DMetric) :-
    get_trajectory_chis(T1, Chis1),
    get_trajectory_chis(T2, Chis2),
    get_trajectory_entropies(T1, Es1),
    get_trajectory_entropies(T2, Es2),
    get_trajectory_confidences(T1, Cs1),
    get_trajectory_confidences(T2, Cs2),
    metric_distance_contexts(Chis1, Chis2, Es1, Es2, Cs1, Cs2, 0.0, 0, Sum, Count),
    (Count > 0 -> DMetric is Sum / Count ; DMetric = 0.0).

metric_distance_contexts([], [], [], [], [], [], Sum, Count, Sum, Count).
metric_distance_contexts([Chi1|Chis1], [Chi2|Chis2], [E1|Es1], [E2|Es2],
                         [C1|Cs1], [C2|Cs2], AccSum, AccCount, Sum, Count) :-
    chi_diff(Chi1, Chi2, ChiD),
    entropy_diff(E1, E2, ED),
    conf_diff(C1, C2, CD),
    D is 0.5 * ChiD + 0.3 * ED + 0.2 * CD,
    NewSum is AccSum + D,
    NewCount is AccCount + 1,
    metric_distance_contexts(Chis1, Chis2, Es1, Es2, Cs1, Cs2,
                             NewSum, NewCount, Sum, Count).

chi_diff(Chi1, Chi2, D) :-
    (number(Chi1), number(Chi2) -> D is min(1.0, abs(Chi1 - Chi2)) ; D = 0.0).

entropy_diff(E1, E2, D) :-
    (number(E1), number(E2) -> D is abs(E1 - E2) ; D = 0.0).

conf_diff(C1, C2, D) :-
    (number(C1), number(C2) -> D is abs(C1 - C2) ; D = 0.0).

%% Component 3: Stability distance
stability_distance(T1, T2, DStability) :-
    T1 = trajectory(_, _, S1),
    T2 = trajectory(_, _, S2),
    S1 = trajectory_summary(_, _, preservation(Pres1), coupling(Coup1),
                            purity(Pur1), _, _, _, _, _, _, boltzmann(Boltz1)),
    S2 = trajectory_summary(_, _, preservation(Pres2), coupling(Coup2),
                            purity(Pur2), _, _, _, _, _, _, boltzmann(Boltz2)),
    % Coupling difference
    (   number(Coup1), number(Coup2)
    ->  CoupDiff is abs(Coup1 - Coup2)
    ;   CoupDiff = 0.0
    ),
    % Purity difference (handle -1.0 inconclusive as 0.5)
    normalize_purity(Pur1, NPur1),
    normalize_purity(Pur2, NPur2),
    PurDiff is abs(NPur1 - NPur2),
    % Preservation distance
    preservation_distance(Pres1, Pres2, PresDist),
    % Boltzmann distance
    boltzmann_distance(Boltz1, Boltz2, BoltzDist),
    DStability is 0.30 * CoupDiff + 0.25 * PurDiff
               + 0.25 * PresDist + 0.20 * BoltzDist.

normalize_purity(P, 0.5) :- (P =:= -1.0 ; \+ number(P)), !.
normalize_purity(P, P).

preservation_distance(preserved(_), preserved(_), 0.0) :- !.
preservation_distance(violated(_), violated(_), 0.0) :- !.
preservation_distance(_, _, 1.0).

boltzmann_distance(compliant(_), compliant(_), 0.0) :- !.
boltzmann_distance(non_compliant(_, _), non_compliant(_, _), 0.0) :- !.
boltzmann_distance(inconclusive(_), _, 0.5) :- !.
boltzmann_distance(_, inconclusive(_), 0.5) :- !.
boltzmann_distance(_, _, 1.0).

%% Component 4: Pathology distance
pathology_distance(T1, T2, DPathology) :-
    T1 = trajectory(_, _, S1),
    T2 = trajectory(_, _, S2),
    S1 = trajectory_summary(_, _, _, _, _, signature(Sig1), _,
                            drift_count(DC1), drift_max_severity(Sev1),
                            voids(V1), _, _),
    S2 = trajectory_summary(_, _, _, _, _, signature(Sig2), _,
                            drift_count(DC2), drift_max_severity(Sev2),
                            voids(V2), _, _),
    % Drift count distance (normalized, max 10)
    DriftDist is min(1.0, abs(DC1 - DC2) / 5),
    % Void Jaccard distance
    jaccard_distance(V1, V2, VoidDist),
    % Severity distance
    severity_distance(Sev1, Sev2, SevDist),
    % Signature distance
    signature_distance(Sig1, Sig2, SigDist),
    DPathology is 0.35 * DriftDist + 0.30 * VoidDist
               + 0.20 * SevDist + 0.15 * SigDist.

%% jaccard_distance(+Set1, +Set2, -Distance)
jaccard_distance([], [], 0.0) :- !.
jaccard_distance(S1, S2, D) :-
    sort(S1, SS1), sort(S2, SS2),
    intersection(SS1, SS2, Inter),
    union(SS1, SS2, Uni),
    length(Inter, NI), length(Uni, NU),
    (NU > 0 -> D is 1.0 - NI / NU ; D = 0.0).

%% severity_distance(+Sev1, +Sev2, -D)
severity_distance(S, S, 0.0) :- !.
severity_distance(S1, S2, D) :-
    severity_rank(S1, R1),
    severity_rank(S2, R2),
    D is min(1.0, abs(R1 - R2) / 3).

severity_rank(none, 0).
severity_rank(watch, 1).
severity_rank(warning, 2).
severity_rank(critical, 3).

%% signature_distance(+Sig1, +Sig2, -D)
signature_distance(S, S, 0.0) :- !.
signature_distance(natural_law, false_natural_law, 0.8) :- !.
signature_distance(false_natural_law, natural_law, 0.8) :- !.
signature_distance(coupling_invariant_rope, false_ci_rope, 0.6) :- !.
signature_distance(false_ci_rope, coupling_invariant_rope, 0.6) :- !.
signature_distance(natural_law, constructed_low_extraction, 0.5) :- !.
signature_distance(constructed_low_extraction, natural_law, 0.5) :- !.
signature_distance(_, _, 0.4).  % Default moderate distance

%% compute_pairwise_distances(+Context)
%  O(n^2) pairwise distance computation with progress reporting.
compute_pairwise_distances(Context) :-
    findall(C, trajectory_cached(C, Context, _), AllCs),
    sort(AllCs, Constraints),
    length(Constraints, N),
    NPairs is N * (N - 1) // 2,
    format(user_error, '[trajectory] Computing ~w pairwise distances (~w constraints)...~n',
           [NPairs, N]),
    compute_pairs(Constraints, Context, 0, NPairs).

compute_pairs([], _, _, _).
compute_pairs([_], _, _, _).
compute_pairs([C1|Rest], Context, Done, Total) :-
    compute_pairs_with(C1, Rest, Context, Done, Total, NewDone),
    compute_pairs(Rest, Context, NewDone, Total).

compute_pairs_with(_, [], _, Done, _, Done).
compute_pairs_with(C1, [C2|Rest], Context, Done, Total, FinalDone) :-
    trajectory_distance(C1, C2, Context, Dist),
    (C1 @< C2 -> assertz(pair_dist(C1, C2, Dist))
    ;              assertz(pair_dist(C2, C1, Dist))),
    Done1 is Done + 1,
    (   0 =:= Done1 mod 50000
    ->  format(user_error, '[trajectory] ~w/~w pairs computed...~n', [Done1, Total])
    ;   true
    ),
    compute_pairs_with(C1, Rest, Context, Done1, Total, FinalDone).

%% get_pair_dist(+C1, +C2, -D)
%  Symmetric distance lookup.
get_pair_dist(C1, C2, D) :-
    (C1 @< C2 -> pair_dist(C1, C2, D) ; pair_dist(C2, C1, D)), !.
get_pair_dist(C, C, 0.0) :- !.
get_pair_dist(_, _, 1.0).  % Fallback if pair not computed

/* ================================================================
   PHASE 3: HIERARCHICAL AGGLOMERATIVE CLUSTERING
   ================================================================
   Two-stage approach for performance:
   Stage 1: Group by shift pattern (free, from fingerprint data)
   Stage 2: Hierarchical clustering within each shift group
   Cross-group isomorphisms detected by comparing cluster centroids.
   ================================================================ */

%% run_hierarchical_clustering(+Context)
%  Groups by shift pattern first, then clusters within groups.
run_hierarchical_clustering(Context) :-
    format(user_error, '[trajectory] Starting clustering...~n', []),
    findall(C, trajectory_cached(C, Context, _), AllCs),
    sort(AllCs, Constraints),
    % Group by shift pattern
    group_by_shift(Constraints, ShiftGroups),
    length(ShiftGroups, NGroups),
    format(user_error, '[trajectory] ~w shift groups to cluster.~n', [NGroups]),
    % Cluster within each group
    cluster_all_groups(ShiftGroups, 0),
    % Assign family IDs at configured cut level
    config:param(trajectory_family_cut_level, CutLevel),
    assign_families(CutLevel),
    format(user_error, '[trajectory] Clustering complete.~n', []).

%% group_by_shift(+Constraints, -Groups)
%  Groups constraints by their shift pattern.
%  Groups = [shift_group(Pattern, [C1, C2, ...]), ...]
group_by_shift(Constraints, Groups) :-
    findall(
        Shift-C,
        (   member(C, Constraints),
            (   catch(logical_fingerprint:fingerprint_shift(C, Shift), _, fail)
            ->  true
            ;   Shift = shift(unknown, unknown, unknown, unknown)
            )
        ),
        Pairs
    ),
    msort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist(make_shift_group, Grouped, Groups).

make_shift_group(Shift-Members, shift_group(Shift, Members)).

%% cluster_all_groups(+Groups, +FamilyOffset)
%  Runs hierarchical clustering on each shift group.
cluster_all_groups([], _).
cluster_all_groups([shift_group(_, Members)|Rest], Offset) :-
    length(Members, N),
    (   N =< 1
    ->  % Singleton — directly assign as its own cluster
        (   Members = [C]
        ->  FamilyID is Offset + 1,
            assertz(cluster_member(FamilyID, C))
        ;   true
        ),
        NewOffset is Offset + 1
    ;   % Run HAC on this group
        cluster_group(Members, Offset, NewOffset)
    ),
    cluster_all_groups(Rest, NewOffset).

%% cluster_group(+Members, +Offset, -NewOffset)
%  Hierarchical agglomerative clustering with average linkage.
%  Uses a sorted list approach for the priority queue.
cluster_group(Members, Offset, NewOffset) :-
    config:param(trajectory_family_cut_level, CutLevel),
    % Initialize singleton clusters: cluster(ID, [Member])
    init_clusters(Members, Offset, Clusters, NextID),
    % Build initial distance list
    build_distance_list(Clusters, DistList0),
    msort(DistList0, DistList),
    % Run merge loop (stops when min distance > CutLevel)
    hac_loop(Clusters, DistList, NextID, CutLevel, _FinalClusters, _MergeStep),
    % All remaining clusters become family assignments
    NewOffset is NextID + 1000.  % Leave room for merge IDs

init_clusters([], _, [], ID) :- ID = 0.
init_clusters(Members, Offset, Clusters, NextID) :-
    init_clusters_acc(Members, Offset, 1, Clusters, NextID).

init_clusters_acc([], _, NextID, [], NextID).
init_clusters_acc([M|Ms], Offset, ID, [cluster(CID, [M])|Rest], NextID) :-
    CID is Offset + ID,
    assertz(cluster_member(CID, M)),
    ID1 is ID + 1,
    init_clusters_acc(Ms, Offset, ID1, Rest, NextID).

%% build_distance_list(+Clusters, -DistList)
%  Creates dist(D, ID1, ID2) pairs for all cluster pairs.
build_distance_list(Clusters, DistList) :-
    findall(
        dist(D, ID1, ID2),
        (   member(cluster(ID1, Mems1), Clusters),
            member(cluster(ID2, Mems2), Clusters),
            ID1 < ID2,
            average_linkage_dist(Mems1, Mems2, D)
        ),
        DistList
    ).

%% average_linkage_dist(+Members1, +Members2, -Dist)
average_linkage_dist(M1, M2, Dist) :-
    findall(D,
        (member(C1, M1), member(C2, M2), get_pair_dist(C1, C2, D)),
        Dists),
    (   Dists \= []
    ->  sum_list(Dists, Sum),
        length(Dists, N),
        Dist is Sum / N
    ;   Dist = 1.0
    ).

%% hac_loop(+Clusters, +DistList, +NextMergeID, +CutLevel, -FinalClusters, -MergeStep)
%  The main merge loop. Finds closest pair, merges, updates distances.
%  Stops when: (a) 1 cluster left, (b) no distances, or (c) min distance > CutLevel.
hac_loop(Clusters, _, _, _, Clusters, 0) :-
    length(Clusters, N), N =< 1, !.
hac_loop(Clusters, [], _, _, Clusters, 0) :- !.
hac_loop(Clusters, [dist(D, _, _)|_], _, CutLevel, Clusters, 0) :-
    D > CutLevel, !.
hac_loop(Clusters, [dist(D, ID1, ID2)|_RestDist], NextID, CutLevel, FinalClusters, FinalStep) :-
    % Check both clusters still exist
    member(cluster(ID1, Mems1), Clusters),
    member(cluster(ID2, Mems2), Clusters),
    !,
    % Merge
    append(Mems1, Mems2, MergedMems),
    MergedID is NextID + 10000,
    assertz(cluster_merge(MergedID, ID1, ID2, D)),
    % Update cluster_member facts: retract old, assert new
    forall(member(M, MergedMems), (
        retractall(cluster_member(ID1, M)),
        retractall(cluster_member(ID2, M)),
        assertz(cluster_member(MergedID, M))
    )),
    % Remove old clusters, add merged
    exclude(cluster_id_matches(ID1), Clusters, C1),
    exclude(cluster_id_matches(ID2), C1, C2),
    NewClusters = [cluster(MergedID, MergedMems)|C2],
    % Rebuild distance list for new cluster
    findall(dist(NewD, MinID, MaxID),
        (   member(cluster(OtherID, OtherMems), C2),
            average_linkage_dist(MergedMems, OtherMems, NewD),
            (MergedID < OtherID -> MinID = MergedID, MaxID = OtherID
            ;                      MinID = OtherID, MaxID = MergedID)
        ),
        NewDists),
    % Rebuild full distance list (remove old pairs involving ID1 or ID2)
    findall(dist(DD, A, B),
        (   pair_dist_cached(A, B, DD, Clusters),
            A \= ID1, A \= ID2, B \= ID1, B \= ID2
        ),
        FilteredOldDists),
    append(NewDists, FilteredOldDists, AllDists),
    msort(AllDists, SortedDists),
    NextID1 is MergedID + 1,
    hac_loop(NewClusters, SortedDists, NextID1, CutLevel, FinalClusters, FinalStep).
hac_loop(Clusters, [_|RestDist], NextID, CutLevel, FinalClusters, FinalStep) :-
    % The dist entry references a cluster that no longer exists; skip it.
    hac_loop(Clusters, RestDist, NextID, CutLevel, FinalClusters, FinalStep).

cluster_id_matches(ID, cluster(ID, _)).

%% pair_dist_cached(+ID1, +ID2, -D, +Clusters)
%  Look up previously computed inter-cluster distance from the pair_dist facts.
%  This is a fallback for rebuilding; we primarily work from the dist list.
pair_dist_cached(ID1, ID2, D, Clusters) :-
    member(cluster(ID1, M1), Clusters),
    member(cluster(ID2, M2), Clusters),
    ID1 < ID2,
    average_linkage_dist(M1, M2, D).

%% assign_families(+CutLevel)
%  Assigns family IDs based on current cluster_member facts.
%  At this point, cluster_member/2 reflects the final clustering state.
assign_families(_CutLevel) :-
    findall(CID, cluster_member(CID, _), AllCIDs),
    sort(AllCIDs, UniqueCIDs),
    forall(member(CID, UniqueCIDs), (
        findall(C, cluster_member(CID, C), Members),
        forall(member(M, Members),
            assertz(family_assignment(M, CID)))
    )).

%% trajectory_cluster(+CutLevel, +Context, -Clusters)
trajectory_cluster(_CutLevel, _Context, Clusters) :-
    findall(CID, cluster_member(CID, _), AllCIDs),
    sort(AllCIDs, UniqueCIDs),
    findall(cluster(CID, Members),
        (member(CID, UniqueCIDs),
         findall(C, cluster_member(CID, C), Members)),
        Clusters).

%% structural_family(+Constraint, -FamilyID)
structural_family(C, FamilyID) :-
    family_assignment(C, FamilyID).

%% cluster_members(?FamilyID, -Members)
%  When FamilyID is bound, returns its members.
%  When FamilyID is free, backtracks over each family.
cluster_members(FamilyID, Members) :-
    setof(C, cluster_member(FamilyID, C), Members).

/* ================================================================
   PHASE 4: ISOMORPHISM DETECTION
   ================================================================ */

% Categorical: Natural isomorphism test — two constraints with equivalent transformation behavior across the site
%% structural_isomorphism(+C1, +C2, -Level, -Evidence)
%  Tests whether two constraints are structurally isomorphic.
%  Level in { strict, trajectory, family, none }.
structural_isomorphism(C1, C2, Level, Evidence) :-
    C1 \= C2,
    compute_isomorphism_evidence(C1, C2, Evidence),
    classify_isomorphism_level(Evidence, Level).

compute_isomorphism_evidence(C1, C2, Evidence) :-
    % Shift match
    (   catch(logical_fingerprint:fingerprint_shift(C1, S1), _, fail),
        catch(logical_fingerprint:fingerprint_shift(C2, S2), _, fail)
    ->  (S1 = S2 -> ShiftMatch = true ; ShiftMatch = false)
    ;   ShiftMatch = unknown
    ),
    % Zone match
    (   catch(logical_fingerprint:fingerprint_zone(C1, Z1), _, fail),
        catch(logical_fingerprint:fingerprint_zone(C2, Z2), _, fail)
    ->  (Z1 = Z2 -> ZoneMatch = true ; ZoneMatch = false)
    ;   ZoneMatch = unknown
    ),
    % Void match
    (   catch(logical_fingerprint:fingerprint_voids(C1, V1), _, fail),
        catch(logical_fingerprint:fingerprint_voids(C2, V2), _, fail)
    ->  (V1 = V2 -> VoidMatch = true ; VoidMatch = false)
    ;   VoidMatch = unknown
    ),
    % Coupling band match
    config:param(trajectory_coupling_band_width, BandWidth),
    (   catch(structural_signatures:cross_index_coupling(C1, Coup1), _, fail),
        catch(structural_signatures:cross_index_coupling(C2, Coup2), _, fail)
    ->  (abs(Coup1 - Coup2) =< BandWidth -> CouplingMatch = true ; CouplingMatch = false)
    ;   CouplingMatch = unknown
    ),
    % Trajectory distance
    (   catch(get_pair_dist(C1, C2, TrajDist), _, fail)
    ->  true
    ;   TrajDist = 1.0
    ),
    % Orbit family match
    (   catch(dirac_classification:gauge_orbit(C1, O1), _, fail),
        catch(dirac_classification:gauge_orbit(C2, O2), _, fail)
    ->  findall(T, member(orbit_point(T, _), O1), Ts1),
        findall(T, member(orbit_point(T, _), O2), Ts2),
        sort(Ts1, OF1), sort(Ts2, OF2),
        (OF1 = OF2 -> OrbitMatch = true ; OrbitMatch = false)
    ;   OrbitMatch = unknown
    ),
    Evidence = isomorphism_evidence(
        shift_match(ShiftMatch),
        zone_match(ZoneMatch),
        void_match(VoidMatch),
        coupling_band_match(CouplingMatch),
        trajectory_distance(TrajDist),
        orbit_family_match(OrbitMatch)
    ).

classify_isomorphism_level(Evidence, strict) :-
    Evidence = isomorphism_evidence(
        shift_match(true), zone_match(true), void_match(true),
        coupling_band_match(true), trajectory_distance(D), _),
    D < 0.05, !.
classify_isomorphism_level(Evidence, trajectory) :-
    Evidence = isomorphism_evidence(
        shift_match(true), zone_match(true), _,
        coupling_band_match(true), trajectory_distance(D), _),
    config:param(trajectory_isomorphism_threshold, Thresh),
    D < Thresh, !.
classify_isomorphism_level(Evidence, family) :-
    Evidence = isomorphism_evidence(
        _, _, _, coupling_band_match(true), _, orbit_family_match(true)), !.
classify_isomorphism_level(_, none).

%% cross_domain_twins(+Context, +Threshold, -Pairs)
%  Finds pairs in the same structural family from different domains.
%  Domain inferred from constraint naming patterns (testset prefix).
cross_domain_twins(_Context, Threshold, Pairs) :-
    findall(twin(C1, C2, Dist, F),
        (   family_assignment(C1, F),
            family_assignment(C2, F),
            C1 @< C2,
            constraint_domain(C1, D1),
            constraint_domain(C2, D2),
            D1 \= D2,
            get_pair_dist(C1, C2, Dist),
            Dist < Threshold
        ),
        RawPairs),
    msort(RawPairs, Pairs).

%% constraint_domain(+Constraint, -Domain)
%  Infers domain from constraint naming pattern (prefix before first underscore
%  or testset filename). This is a heuristic.
constraint_domain(C, Domain) :-
    atom_string(C, Str),
    (   sub_string(Str, Before, 1, _, "_"),
        Before > 0,
        sub_string(Str, 0, Before, _, DStr),
        atom_string(Domain, DStr)
    ->  true
    ;   Domain = unknown_domain
    ).

/* ================================================================
   MAIN ENTRY POINT
   ================================================================ */

%% trajectory_run(+Context, -Summary)
%  Main entry point. Computes trajectories, distances, clustering,
%  and isomorphism detection.
trajectory_run(Context, Summary) :-
    format(user_error, '[trajectory] Starting trajectory mining run...~n', []),
    trajectory_cleanup,

    % Run multi-context MaxEnt if not already available
    ensure_maxent(Context),

    % Phase 1: Compute trajectories
    compute_all_trajectories(Context),
    findall(C, trajectory_cached(C, Context, _), AllCs),
    length(AllCs, NTrajectories),
    format(user_error, '[trajectory] Phase 1 complete: ~w trajectories.~n', [NTrajectories]),

    % Phase 2: Pairwise distances
    compute_pairwise_distances(Context),
    format(user_error, '[trajectory] Phase 2 complete: distances computed.~n', []),

    % Phase 3: Clustering
    run_hierarchical_clustering(Context),
    findall(FID, family_assignment(_, FID), AllFIDs),
    sort(AllFIDs, UniqueFIDs),
    length(UniqueFIDs, NFamilies),
    format(user_error, '[trajectory] Phase 3 complete: ~w families.~n', [NFamilies]),

    % Phase 4: Cross-domain twins
    config:param(trajectory_isomorphism_threshold, IsoThresh),
    cross_domain_twins(Context, IsoThresh, Twins),
    length(Twins, NTwins),
    format(user_error, '[trajectory] Phase 4 complete: ~w cross-domain twins.~n', [NTwins]),

    % Store run info
    get_time(Timestamp),
    assertz(trajectory_run_info(Context, NTrajectories, NFamilies, Timestamp)),

    % Compute anomalies (singletons)
    findall(C, (
        family_assignment(C, FID),
        findall(M, family_assignment(M, FID), Ms),
        length(Ms, 1)
    ), Singletons),
    length(Singletons, NSingletons),

    Summary = trajectory_summary(
        total_constraints(NTrajectories),
        families(NFamilies),
        cross_domain_twins(NTwins),
        anomalies(NSingletons)
    ),
    format(user_error, '[trajectory] Run complete: ~w constraints, ~w families, ~w twins, ~w singletons.~n',
           [NTrajectories, NFamilies, NTwins, NSingletons]).

%% ensure_maxent(+Context)
%  Runs MaxEnt if not already available. Tries multi-context first.
ensure_maxent(Context) :-
    (   catch(maxent_classifier:maxent_run_info(_, _, _), _, fail)
    ->  format(user_error, '[trajectory] MaxEnt data already available.~n', [])
    ;   format(user_error, '[trajectory] Running MaxEnt classifier...~n', []),
        standard_contexts(Contexts),
        (   catch(maxent_classifier:maxent_multi_run(Contexts, _), _, fail)
        ->  format(user_error, '[trajectory] Multi-context MaxEnt complete.~n', [])
        ;   catch(maxent_classifier:maxent_run(Context, _), _, true),
            format(user_error, '[trajectory] Single-context MaxEnt complete (degraded mode).~n', [])
        )
    ).

/* ================================================================
   SELF-TEST
   ================================================================ */

trajectory_selftest :-
    format('=== Trajectory Mining Self-Test ===~n~n'),
    covering_analysis:load_all_testsets,
    constraint_indexing:default_context(Context),

    format('Running trajectory mining...~n'),
    trajectory_run(Context, Summary),
    Summary = trajectory_summary(
        total_constraints(NTrajectories),
        families(NFamilies),
        cross_domain_twins(NTwins),
        anomalies(NSingletons)
    ),
    format('~n--- Results ---~n'),
    format('  Trajectories:    ~w~n', [NTrajectories]),
    format('  Families:        ~w~n', [NFamilies]),
    format('  Cross-domain twins: ~w~n', [NTwins]),
    format('  Singletons:      ~w~n~n', [NSingletons]),

    % Verification
    format('--- Verification ---~n'),
    (   NTrajectories > 1000
    ->  format('  [PASS] Trajectory count > 1000 (~w)~n', [NTrajectories])
    ;   format('  [WARN] Trajectory count ~w (expected > 1000)~n', [NTrajectories])
    ),
    (   NFamilies >= 5, NFamilies =< 200
    ->  format('  [PASS] Family count in range 5-200 (~w)~n', [NFamilies])
    ;   format('  [WARN] Family count ~w (expected 5-200)~n', [NFamilies])
    ),

    % Check Family A constraints
    format('~n--- Family A Validation ---~n'),
    FamilyA = [decentralized_infrastructure_rope, fair_use_doctrine,
               noethers_theorem_symmetry, reciprocity_laws_math],
    check_family_cohesion(FamilyA, Context, 'Family A'),

    % Check Family B constraints
    format('~n--- Family B Validation ---~n'),
    FamilyB = [moltbook_agent_theater, ulysses_calypso_1904],
    check_family_cohesion(FamilyB, Context, 'Family B'),

    % Check inter-family separation
    format('~n--- Cross-Family Separation ---~n'),
    (   FamilyA = [FA1|_], FamilyB = [FB1|_],
        get_pair_dist(FA1, FB1, CrossDist)
    ->  format('  d(~w, ~w) = ~4f~n', [FA1, FB1, CrossDist]),
        (   CrossDist > 0.30
        ->  format('  [PASS] Cross-family distance > 0.30~n')
        ;   format('  [NOTE] Cross-family distance ~4f (expected > 0.30)~n', [CrossDist])
        )
    ;   format('  [SKIP] Could not compute cross-family distance~n')
    ),

    format('~n=== Self-Test Complete ===~n').

check_family_cohesion(Members, Context, Label) :-
    % Check if members exist in corpus
    findall(C, (member(C, Members), trajectory_cached(C, Context, _)), Found),
    length(Found, NFound),
    length(Members, NExpected),
    format('  ~w: ~w/~w members found in corpus~n', [Label, NFound, NExpected]),
    % Check intra-family distances
    (   Found = [_,_|_]
    ->  findall(D, (
            member(C1, Found), member(C2, Found), C1 @< C2,
            get_pair_dist(C1, C2, D)
        ), Dists),
        (   Dists \= []
        ->  max_list(Dists, MaxD),
            sum_list(Dists, SumD), length(Dists, NDists),
            MeanD is SumD / NDists,
            format('  ~w: mean intra-distance = ~4f, max = ~4f~n', [Label, MeanD, MaxD]),
            (   MaxD < 0.20
            ->  format('  [PASS] All intra-distances < 0.20~n')
            ;   format('  [NOTE] Max intra-distance ~4f (target < 0.20)~n', [MaxD])
            )
        ;   format('  [SKIP] No pair distances computed~n')
        )
    ;   format('  [SKIP] Need at least 2 members for cohesion check~n')
    ),
    % Check if members share a family
    findall(FID, (member(C, Found), family_assignment(C, FID)), FIDs),
    sort(FIDs, UniqueFIDs),
    length(UniqueFIDs, NFams),
    (   NFams =:= 1
    ->  format('  [PASS] All members in same family (~w)~n', [UniqueFIDs])
    ;   format('  [NOTE] Members span ~w families: ~w~n', [NFams, UniqueFIDs])
    ).
