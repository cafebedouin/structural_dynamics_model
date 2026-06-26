% C-null harness — OQ-182 scope-setter: does the HAC family partition MEAN anything?
% Standalone, commentary-only. NO edits to context_profile_mining.pl / config.pl.
%
% Load (exactly like c1_c3_probe.pl):
%   swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
%     -l maxent_classifier.pl -l context_profile_mining.pl -l c_null_harness.pl \
%     -g "c_null_run, halt." -t "halt(1)"
%
% Mechanism correction vs the frozen "Chimera surgery map" (quantities unchanged):
%   fingerprint_shift/2 reads CONSTRAINT IDENTITY (logical_fingerprint.pl:113), not
%   trajectory_cached. So a chimera trajectory_cached + run_hierarchical_clustering/1 pins the
%   shift pre-grouping to the REAL shift boundaries regardless of sigma_shift -> toothless / false
%   PASS, and it breaks the joint control. This harness builds the shift-groups ITSELF
%   (make_groups/4, keyed on fingerprint_shift(C[sigma_shift(i)])) and reuses only
%   cluster_all_groups/2 + assign_families/1 from the engine. The per-component shuffle is a pure
%   index recombination over precomputed real component-distance matrices — no chimera trajectory.

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(random)).

:- dynamic real_pair_dist/3.       % snapshot of the real pair_dist (atoms, A @< B)
:- dynamic comp_m/4.               % comp_m(Component, PosA, PosB, Dist)  PosA < PosB
:- dynamic ci_pos/2.               % ci_pos(Atom, Position)  1-based, over the sorted index
:- dynamic c_null_seed_fact/1.

c_null_seed(20260625).

% ============================================================================
% 0. Canonical partition helper (same as c1_c3_probe.pl) + silhouette
% ============================================================================

% canonical partition from current cluster state: sorted list of sorted member-lists
current_partition(Partition) :-
    findall(FID-C, context_profile_mining:family_assignment(C, FID), Pairs0),
    keysort(Pairs0, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    findall(SMembers, (member(_-Ms, Grouped), sort(Ms, SMembers)), Fams0),
    sort(Fams0, Partition).

%% silhouette(+Partition, -MeanS, -NExcluded, -NFam, -Degenerate)
%  Reads the LIVE pair_dist facts (real or overlaid). Singletons excluded from scoring
%  (counted in NExcluded) but available as "other families" for b(c). Degenerate=true when
%  the partition has <2 families OR no constraint is scorable (no defined silhouette).
silhouette(Partition, MeanS, NExcl, NFam, Degenerate) :-
    length(Partition, NFam),
    findall(1, (member(F, Partition), length(F, 1)), Sings),
    length(Sings, NExcl),
    (   NFam < 2
    ->  Degenerate = true, MeanS = 0.0
    ;   findall(S,
            (   member(F, Partition), length(F, SzF), SzF >= 2,
                member(C, F),
                a_value(C, F, A),
                b_value(C, Partition, F, B),
                ( max(A, B) =:= 0.0 -> S = 0.0 ; S is (B - A) / max(A, B) )
            ), Ss),
        (   Ss == []
        ->  Degenerate = true, MeanS = 0.0
        ;   Degenerate = false,
            sum_list(Ss, Sum), length(Ss, M), MeanS is Sum / M
        )
    ).

a_value(C, F, A) :-
    findall(D, (member(M, F), M \== C, context_profile_mining:get_pair_dist(C, M, D)), Ds),
    ( Ds == [] -> A = 0.0 ; sum_list(Ds, S), length(Ds, N), A is S / N ).

b_value(C, Partition, F, B) :-
    findall(MeanG,
        (   member(G, Partition), G \== F,
            findall(D, (member(M, G), context_profile_mining:get_pair_dist(C, M, D)), Ds),
            Ds \== [], sum_list(Ds, S), length(Ds, N), MeanG is S / N
        ), Means),
    ( Means == [] -> B = 0.0 ; min_list(Means, B) ).

% ============================================================================
% 1. Setup — real run, index, snapshots, component matrices
% ============================================================================

%% setup_real(-IdxArr, -N, -Index, -Context, -RealPartition, -RealSil, -NTwins, -NPairs)
setup_real(IdxArr, N, Index, Context, RealPartition, RealSil, NTwins, NPairs) :-
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),
    context_profile_mining:trajectory_run(Context, Summary),
    Summary = trajectory_summary(_, _, cross_domain_twins(NTwins), _),
    % sigma domain = clustered index set, sorted (i<j <=> C[i] @< C[j])
    findall(C, context_profile_mining:family_assignment(C, _), Cs0),
    sort(Cs0, Index),
    length(Index, N),
    NPairs is N * (N - 1) // 2,
    IdxArr =.. [idx | Index],
    retractall(ci_pos(_, _)),
    forall(nth1(I, Index, Atom), assertz(ci_pos(Atom, I))),
    % snapshot the real pair_dist BEFORE any overlay
    retractall(real_pair_dist(_, _, _)),
    forall(context_profile_mining:pair_dist(A, B, D), assertz(real_pair_dist(A, B, D))),
    % real partition + real silhouette on the REAL pair_dist
    current_partition(RealPartition),
    silhouette(RealPartition, RealSil, _, _, _).

%% build_component_matrices(+IdxArr, +N, +Context)
%  Precompute the 4 symmetric component distances ONCE for a<b over real trajectories.
build_component_matrices(IdxArr, N, Context) :-
    retractall(comp_m(_, _, _, _)),
    forall(
        ( between(1, N, A), Bs is A + 1, between(Bs, N, B) ),
        ( arg(A, IdxArr, CA), arg(B, IdxArr, CB),
          context_profile_mining:trajectory_cached(CA, Context, T1),
          context_profile_mining:trajectory_cached(CB, Context, T2),
          context_profile_mining:shift_distance(T1, T2, DSh),
          context_profile_mining:metric_distance(T1, T2, DMe),
          context_profile_mining:stability_distance(T1, T2, DSt),
          context_profile_mining:pathology_distance(T1, T2, DPa),
          assertz(comp_m(shift, A, B, DSh)),
          assertz(comp_m(metric, A, B, DMe)),
          assertz(comp_m(stab, A, B, DSt)),
          assertz(comp_m(path, A, B, DPa))
        )).

%% comp_lookup(+K, +I, +J, -D) — symmetric, 0.0 on diagonal
comp_lookup(_, I, I, 0.0) :- !.
comp_lookup(K, I, J, D) :-
    ( I < J -> comp_m(K, I, J, D) ; comp_m(K, J, I, D) ).

weights(w(WSh, WMe, WSt, WPa)) :-
    config:param(trajectory_distance_shift_weight, WSh),
    config:param(trajectory_distance_metric_weight, WMe),
    config:param(trajectory_distance_stability_weight, WSt),
    config:param(trajectory_distance_pathology_weight, WPa).

% real symmetric lookup from the snapshot
rpd(A, B, D) :- ( A @< B -> real_pair_dist(A, B, D) ; A == B -> D = 0.0 ; real_pair_dist(B, A, D) ).

%% internal_check(+IdxArr, +N, +Weights, -MaxDiff)
%  Sum w_k*comp_k(a,b) must equal the engine real pair_dist for every a<b.
internal_check(IdxArr, N, w(WSh, WMe, WSt, WPa), MaxDiff) :-
    findall(Diff,
        ( between(1, N, A), Bs is A + 1, between(Bs, N, B),
          comp_lookup(shift, A, B, DSh), comp_lookup(metric, A, B, DMe),
          comp_lookup(stab, A, B, DSt), comp_lookup(path, A, B, DPa),
          Pred is WSh*DSh + WMe*DMe + WSt*DSt + WPa*DPa,
          arg(A, IdxArr, CA), arg(B, IdxArr, CB), rpd(CA, CB, Real),
          Diff is abs(Pred - Real)
        ), Diffs),
    ( Diffs == [] -> MaxDiff = 0.0 ; max_list(Diffs, MaxDiff) ).

% ============================================================================
% 2. Permutations
% ============================================================================

identity_perm(N, Arr) :- numlist(1, N, L), Arr =.. [s | L].
rand_perm(N, Arr) :- numlist(1, N, L0), random_permutation(L0, L), Arr =.. [s | L].
sig(Arr, I, P) :- arg(I, Arr, P).

% ============================================================================
% 3. Grouping + overlay + cluster_with
% ============================================================================

%% make_groups(+IdxArr, +N, +SigShift, -Groups)
%  Replicates engine group_by_shift, but keys index i's REAL constraint C[i] under
%  fingerprint_shift(C[sigma_shift(i)]). At identity this is byte-equal to group_by_shift.
make_groups(IdxArr, N, SigShift, Groups) :-
    findall(Key - CI,
        ( between(1, N, I),
          arg(I, IdxArr, CI),
          sig(SigShift, I, P), arg(P, IdxArr, CP),
          ( catch(logical_fingerprint:fingerprint_shift(CP, K0), _, fail)
            -> Key = K0 ; Key = shift(unknown, unknown, unknown, unknown) )
        ), Pairs),
    msort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    findall(shift_group(K, Ms), member(K - Ms, Grouped), Groups).

%% overlay_pair_dist(+IdxArr, +N, +Sigs, +Weights)
%  Replace pair_dist with the per-component recombined matrix. Covers EVERY i<j pair
%  (get_pair_dist silently returns 1.0 for missing pairs).
overlay_pair_dist(IdxArr, N, SigSh, SigMe, SigSt, SigPa, w(WSh, WMe, WSt, WPa)) :-
    retractall(context_profile_mining:pair_dist(_, _, _)),
    forall(
        ( between(1, N, I), Js is I + 1, between(Js, N, J) ),
        ( sig(SigSh, I, ISh), sig(SigSh, J, JSh), comp_lookup(shift, ISh, JSh, DSh),
          sig(SigMe, I, IMe), sig(SigMe, J, JMe), comp_lookup(metric, IMe, JMe, DMe),
          sig(SigSt, I, ISt), sig(SigSt, J, JSt), comp_lookup(stab, ISt, JSt, DSt),
          sig(SigPa, I, IPa), sig(SigPa, J, JPa), comp_lookup(path, IPa, JPa, DPa),
          D is WSh*DSh + WMe*DMe + WSt*DSt + WPa*DPa,
          arg(I, IdxArr, CI), arg(J, IdxArr, CJ),   % I<J => CI @< CJ (sorted index)
          assertz(context_profile_mining:pair_dist(CI, CJ, D))
        )).

%% cluster_with(+IdxArr,+N,+Sigs,+Weights, -Partition,-NFam,-Sil,-Degenerate)
cluster_with(IdxArr, N, SigSh, SigMe, SigSt, SigPa, W, Partition, NFam, Sil, Degenerate) :-
    retractall(context_profile_mining:cluster_member(_, _)),
    retractall(context_profile_mining:cluster_merge(_, _, _, _)),
    retractall(context_profile_mining:family_assignment(_, _)),
    make_groups(IdxArr, N, SigSh, Groups),
    overlay_pair_dist(IdxArr, N, SigSh, SigMe, SigSt, SigPa, W),
    context_profile_mining:cluster_all_groups(Groups, 0),
    context_profile_mining:assign_families(_),
    current_partition(Partition),
    silhouette(Partition, Sil, _, NFam, Degenerate).

% relabel a partition (atoms C[i]) by sigma: C[i] -> C[sigma(i)], then canonicalize
relabel_partition(Partition, Sig, IdxArr, Relabeled) :-
    findall(SMembers,
        ( member(F, Partition),
          findall(CP, ( member(CI, F), ci_pos(CI, I), sig(Sig, I, P), arg(P, IdxArr, CP) ), Ms),
          sort(Ms, SMembers)
        ), Fams0),
    sort(Fams0, Relabeled).

% ============================================================================
% 4. Stats helpers
% ============================================================================

mean(L, M) :- L \== [], sum_list(L, S), length(L, N), M is S / N.

median(L, Med) :-
    msort(L, S), length(S, N), N > 0,
    ( 1 is N mod 2
    ->  Mid is N // 2 + 1, nth1(Mid, S, Med)
    ;   M2 is N // 2, M1 is M2 + 1,
        nth1(M2, S, A), nth1(M1, S, B), Med is (A + B) / 2.0
    ).

sd(L, SD) :-
    mean(L, M), length(L, N), N > 0,
    sum_sq_dev(L, M, 0.0, SS),
    SD is sqrt(SS / N).

sum_sq_dev([], _, Acc, Acc).
sum_sq_dev([X | Xs], M, Acc, SS) :-
    Acc1 is Acc + (X - M)*(X - M),
    sum_sq_dev(Xs, M, Acc1, SS).

% nearest-rank 95th percentile: rank = ceil(0.95*M), clamped to [1,M]
percentile95(L, P95) :-
    msort(L, S), length(S, M), M > 0,
    R0 is ceiling(0.95 * M), ( R0 < 1 -> R = 1 ; R0 > M -> R = M ; R = R0 ),
    nth1(R, S, P95).

% ============================================================================
% 5. JSON output
% ============================================================================

write_json(Path, RealSil, N, RealNFam, NDraws, MDef, NDegen, P95, NMean, NMed, NSD, Draws) :-
    setup_call_cleanup(
        open(Path, write, Out),
        ( c_null_seed(Seed),
          current_prolog_flag(version, Ver),
          format(Out, '{~n', []),
          format(Out, '  "seed": ~w,~n', [Seed]),
          format(Out, '  "swipl_version": ~w,~n', [Ver]),
          format(Out, '  "n_index": ~w,~n', [N]),
          format(Out, '  "real_sil": ~6f,~n', [RealSil]),
          format(Out, '  "real_nfam": ~w,~n', [RealNFam]),
          format(Out, '  "null_n": ~w,~n', [NDraws]),
          format(Out, '  "null_defined_n": ~w,~n', [MDef]),
          format(Out, '  "null_degenerate_n": ~w,~n', [NDegen]),
          format(Out, '  "p95": ~6f,~n', [P95]),
          format(Out, '  "null_mean": ~6f,~n', [NMean]),
          format(Out, '  "null_median": ~6f,~n', [NMed]),
          format(Out, '  "null_sd": ~6f,~n', [NSD]),
          format(Out, '  "null_s": [', []), write_s_array(Out, Draws), format(Out, '],~n', []),
          format(Out, '  "null_nfam": [', []), write_nfam_array(Out, Draws), format(Out, ']~n', []),
          format(Out, '}~n', [])
        ),
        close(Out)).

write_s_array(_, []).
write_s_array(Out, [draw(_, S, _, Degen) | Rest]) :-
    ( Degen == true -> write(Out, 'null') ; format(Out, '~6f', [S]) ),
    ( Rest == [] -> true ; write(Out, ', ') ),
    write_s_array(Out, Rest).

write_nfam_array(_, []).
write_nfam_array(Out, [draw(_, _, NFam, _) | Rest]) :-
    format(Out, '~w', [NFam]),
    ( Rest == [] -> true ; write(Out, ', ') ),
    write_nfam_array(Out, Rest).

% ============================================================================
% 6. Main orchestration
% ============================================================================

c_null_run :-
    setup_call_cleanup(
        true,
        c_null_body,
        restore_real_pair_dist).

c_null_body :-
    format("================================================================~n", []),
    format("OQ-182 C-NULL HARNESS (testsets/ leg) — family-meaning scope-setter~n", []),
    format("================================================================~n", []),
    c_null_seed(Seed),
    current_prolog_flag(version, Ver),
    format("seed=~w  SWI-Prolog version=~w  (PRNG version-stable, not cross-version)~n", [Seed, Ver]),
    weights(W), W = w(WSh, WMe, WSt, WPa),
    config:param(trajectory_family_cut_level, Cut),
    format("weights: shift=~w metric=~w stab=~w path=~w  cut=~w~n", [WSh, WMe, WSt, WPa, Cut]),
    nl,

    % --- Setup ---
    setup_real(IdxArr, N, Index, Context, RealPartition, RealSil, NTwins, NPairs),
    length(RealPartition, RealNFam),
    format("REAL: n_index=~w  families=~w  mean_silhouette=~6f~n", [N, RealNFam, RealSil]),
    nl,

    build_component_matrices(IdxArr, N, Context),

    % --- Internal component-matrix check ---
    internal_check(IdxArr, N, W, MaxDiff),
    format("INTERNAL CHECK: max |sum_k w_k*comp_k(a,b) - engine pair_dist| = ~e~n", [MaxDiff]),
    ( MaxDiff < 1.0e-9
      -> format("  => PASS (component matrices reproduce the engine distance)~n", [])
      ;  format("  => FAIL — component matrices do NOT match engine; ABORT~n", []), throw(internal_check_failed) ),
    nl,

    % ===================== STAGE A: deterministic controls =====================
    format("---- STAGE A: deterministic controls (gating, printed BEFORE the null) ----~n", []),

    % Control 1: GROUPING-FIDELITY — pins make_groups/4 (the new code the surgery correction added)
    identity_perm(N, IdN),
    make_groups(IdxArr, N, IdN, HarnessGroups),
    context_profile_mining:group_by_shift(Index, EngineGroups),
    canon_groups(HarnessGroups, HG),
    canon_groups(EngineGroups, EG),
    ( HG == EG
      -> length(HG, NG),
         format("GROUPING-FIDELITY: PASS (make_groups@identity == engine group_by_shift: ~w shift groups, same keys+members)~n", [NG])
      ;  format("GROUPING-FIDELITY: FAIL — make_groups mis-keys; ABORT~n", []), throw(grouping_fidelity_failed) ),

    % seed the PRNG ONCE — whole stream (JOINT + tie-break + 200 draws) reproducible
    set_random(seed(Seed)),

    % Control 2: FIDELITY — cluster_with(identity x4) reproduces engine end-to-end
    cluster_with(IdxArr, N, IdN, IdN, IdN, IdN, W, P0, NFam0, Sil0, _),
    DSil0 is abs(Sil0 - RealSil),
    ( P0 == RealPartition, DSil0 < 1.0e-9
      -> format("FIDELITY: PASS (P0 == RealPartition; |S0-RealSil|=~e; families=~w)~n", [DSil0, NFam0])
      ;  format("FIDELITY: FAIL (P0==Real:~w |S0-RealSil|=~e) — harness != engine; ABORT~n",
                [P0 == RealPartition, DSil0]),
         throw(fidelity_failed) ),

    % Control 3: JOINT-TOOTHLESS — one shared sigma => relabeled real partition, silhouette identical
    rand_perm(N, SigJ),
    cluster_with(IdxArr, N, SigJ, SigJ, SigJ, SigJ, W, Pj, NFamj, Silj, _),
    DSilj is abs(Silj - RealSil),
    relabel_partition(Pj, SigJ, IdxArr, PjRelabeled),
    ( PjRelabeled == RealPartition -> RelMatch = yes ; RelMatch = no_tiebreak_boundary ),
    ( DSilj < 1.0e-9
      -> format("JOINT-TOOTHLESS: PASS (S_joint=~6f ~~= RealSil; |diff|=~e; families=~w; relabel-match=~w)~n",
                [Silj, DSilj, NFamj, RelMatch]),
         format("  => joint shuffle relabels intact vectors => the false-PASS the per-component design avoids~n", [])
      ;  format("JOINT-TOOTHLESS: FAIL (S_joint=~6f, |diff|=~e) — grouping/relabel identity broken; ABORT~n",
                [Silj, DSilj]),
         throw(joint_toothless_failed) ),
    nl,

    % --- Tie-break confirm: one fixed per-component sigma run twice => identical partition/NFam ---
    rand_perm(N, T1), rand_perm(N, T2), rand_perm(N, T3), rand_perm(N, T4),
    cluster_with(IdxArr, N, T1, T2, T3, T4, W, PtA, NFamtA, _, _),
    cluster_with(IdxArr, N, T1, T2, T3, T4, W, PtB, NFamtB, _, _),
    ( PtA == PtB, NFamtA == NFamtB
      -> format("TIE-BREAK CONFIRM: PASS (overlay regime sigma-pure; ~w families both runs)~n", [NFamtA])
      ;  format("TIE-BREAK CONFIRM: WARN (partition not sigma-pure under overlay; NFam ~w vs ~w) — known boundary, silhouette unaffected~n",
                [NFamtA, NFamtB]) ),
    nl,

    % ===================== STAGE B: per-component null =====================
    format("---- STAGE B: per-component null, N=200 ----~n", []),
    run_null_draws(IdxArr, N, W, 200, Draws),
    partition_draws(Draws, DefinedS, NFams, NDegen),
    length(DefinedS, MDef),
    length(Draws, NDraws),
    format("null draws: ~w total, ~w defined, ~w degenerate (single-family)~n", [NDraws, MDef, NDegen]),

    % stats over DEFINED draws
    mean(DefinedS, NMean), median(DefinedS, NMed), sd(DefinedS, NSD),
    percentile95(DefinedS, P95),
    min_list(DefinedS, NMin), max_list(DefinedS, NMax),
    format("null silhouette (defined): min=~6f  mean=~6f  median=~6f  max=~6f  sd=~6f~n",
           [NMin, NMean, NMed, NMax, NSD]),

    % family-count distribution (frozen reporting add — guards false-FAIL scale bias)
    min_list(NFams, FMin), max_list(NFams, FMax), mean(NFams, FMean), median(NFams, FMed),
    format("null family-count: min=~w  mean=~3f  median=~3f  max=~w   (real=~w)~n",
           [FMin, FMean, FMed, FMax, RealNFam]),
    nl,

    % --- TEETH control (printed BEFORE the verdict) ---
    ( NSD =:= 0.0 -> Gap = 0.0 ; Gap is (RealSil - NMean) / NSD ),
    format("TEETH (per-component null, read off the full distribution):~n", []),
    format("  RealSil=~6f  null_mean=~6f  null_median=~6f  standardized_gap=(RealSil-mean)/sd=~4f~n",
           [RealSil, NMean, NMed, Gap]),
    ( NMed < RealSil
      -> Teeth = pass,
         format("  => TEETH PASS (null_median < RealSil: per-component shuffle centered the null BELOW real => teeth)~n", [])
      ;  Teeth = fail,
         format("  => TEETH FAIL (null_median >= RealSil: the per-component null has NO TEETH)~n", []) ),
    nl,

    % --- Degenerate-draw flag (pre-registered 5% threshold) ---
    DegFrac is NDegen / max(1, NDraws),
    ( NDegen > 10
      -> Filtered = flagged,
         format("DEGENERATE FLAG: ~w/~w (~3f) > 5%% — verdict computed on a FILTERED null; family-count distribution is the disambiguator~n",
                [NDegen, NDraws, DegFrac])
      ;  Filtered = ok,
         format("degenerate draws: ~w/~w (~3f) <= 5%% threshold~n", [NDegen, NDraws, DegFrac]) ),
    nl,

    % --- VERDICT ---
    format("================================================================~n", []),
    format("VERDICT~n", []),
    format("  RealSil = ~6f    P95(null, per-component) = ~6f~n", [RealSil, P95]),
    ( Teeth == fail
      -> format("  TEETH-FAIL => VERDICT VOID — null had no teeth, test did not run validly.~n", []),
         format("  ACTION: REDESIGN the null, do NOT defer (broken instrument, not evidence about family meaning).~n", []),
         Verdict = void
    ;   ( RealSil > P95
          -> format("  RealSil > P95 AND TEETH passed => PASS.~n", []),
             format("  OQ-182: family product VALIDATED as meaning-bearing.~n", []),
             Verdict = pass
          ;  format("  TEETH passed but RealSil =< P95 => PERCENTILE-only FAIL.~n", []),
             format("  OQ-182: family meaning OPEN-but-promising — real signal, does not clear the 95th.~n", []),
             format("  Deferred to rebuild; shuffle test named as the closer.~n", []),
             Verdict = percentile_fail )
    ),
    ( Filtered == flagged -> format("  [verdict carries the FILTERED-NULL flag: degenerate > 5%%]~n", []) ; true ),
    nl,

    % --- Twin-vacuity parallel (frozen reframe; not a family gate) ---
    TwinFrac is NTwins / max(1, NPairs),
    format("TWIN-VACUITY PARALLEL (NOT a family gate; twin product stays OPEN regardless):~n", []),
    format("  cross_domain_twins=~w  clustered_pairs=~w  twin_fraction=~4f~n", [NTwins, NPairs, TwinFrac]),
    format("================================================================~n", []),

    % --- Write JSON distribution ---
    write_json('../audits/2026-06-25_oq182_trajectory_revive/c_null_distribution.json',
               RealSil, N, RealNFam, NDraws, MDef, NDegen, P95, NMean, NMed, NSD, Draws),
    format("wrote c_null_distribution.json (~w null draws + real)~n", [NDraws]),
    format("VERDICT_TOKEN: ~w~n", [Verdict]).

%% run_null_draws(+IdxArr,+N,+W,+NDraws, -Draws)
run_null_draws(IdxArr, N, W, NDraws, Draws) :-
    run_null_draws_(1, NDraws, IdxArr, N, W, Draws).

run_null_draws_(K, NDraws, _, _, _, []) :- K > NDraws, !.
run_null_draws_(K, NDraws, IdxArr, N, W, [draw(K, S, NFam, Degen) | Rest]) :-
    rand_perm(N, SSh), rand_perm(N, SMe), rand_perm(N, SSt), rand_perm(N, SPa),
    cluster_with(IdxArr, N, SSh, SMe, SSt, SPa, W, _, NFam, S, Degen),
    ( 0 =:= K mod 25 -> format(user_error, '[c-null] draw ~w/~w~n', [K, NDraws]) ; true ),
    K1 is K + 1,
    run_null_draws_(K1, NDraws, IdxArr, N, W, Rest).

%% partition_draws(+Draws, -DefinedS, -NFams, -NDegen)
partition_draws(Draws, DefinedS, NFams, NDegen) :-
    findall(S, member(draw(_, S, _, false), Draws), DefinedS),
    findall(F, member(draw(_, _, F, _), Draws), NFams),
    findall(1, member(draw(_, _, _, true), Draws), Degs), length(Degs, NDegen).

% canonicalize a [shift_group(Key,Members)] list to a sorted set of Key-SortedMembers
canon_groups(Groups, Canon) :-
    findall(K - SM, ( member(shift_group(K, Ms), Groups), sort(Ms, SM) ), L),
    sort(L, Canon).

%% restore_real_pair_dist — hygiene + cheap correctness witness (multiset cardinality)
restore_real_pair_dist :-
    retractall(context_profile_mining:pair_dist(_, _, _)),
    forall(real_pair_dist(A, B, D), assertz(context_profile_mining:pair_dist(A, B, D))),
    aggregate_all(count, real_pair_dist(_, _, _), NReal),
    aggregate_all(count, context_profile_mining:pair_dist(_, _, _), NLive),
    ( NReal =:= NLive
      -> format("CLEANUP: real pair_dist restored (~w facts, cardinality verified)~n", [NLive])
      ;  format("CLEANUP: WARN restore cardinality mismatch (real ~w vs live ~w)~n", [NReal, NLive]) ).
