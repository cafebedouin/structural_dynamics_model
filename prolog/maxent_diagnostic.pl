% ============================================================================
% MAXENT DIAGNOSTIC DEEP DIVE
% ============================================================================
% Standalone diagnostic script. Run from prolog/ directory:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l dirac_classification.pl -l maxent_diagnostic.pl \
%         -g "run_maxent_diagnostic, halt." 2>../outputs/maxent_diag_stderr.txt
% ============================================================================

:- use_module(covering_analysis).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(dirac_classification).
:- use_module(maxent_classifier).

:- use_module(library(lists)).
:- use_module(library(aggregate)).

/* ================================================================
   MAIN ENTRY POINT
   ================================================================ */

run_maxent_diagnostic :-
    format(user_error, '[diag] Starting MaxEnt diagnostic deep dive...~n', []),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),

    % Run MaxEnt classifier
    maxent_classifier:maxent_run(Context, Summary),
    Summary = maxent_summary(NTotal, MeanEntropy, NHighUncertainty, NHard, NSoft),

    format('=== MAXENT DIAGNOSTIC OUTPUT ===~n'),
    format('SUMMARY: NTotal=~w MeanEntropy=~6f NHighUncertainty=~w NHard=~w NSoft=~w~n',
           [NTotal, MeanEntropy, NHighUncertainty, NHard, NSoft]),

    task1_missing_constraints(Context),
    task2_per_type_entropy(Context),
    task3_hard_disagreements(Context),
    task4_non_overlapping(Context),
    task5_gaussian_profiles(Context),
    task6_cross_diagnostic(Context),

    format('~n=== END DIAGNOSTIC OUTPUT ===~n'),
    format(user_error, '[diag] Done.~n', []).

/* ================================================================
   TASK 1: ACCOUNT FOR MISSING 356 CONSTRAINTS
   ================================================================ */

task1_missing_constraints(Context) :-
    format('~n=== TASK 1: MISSING CONSTRAINTS ===~n'),

    % Count all visible constraint_claim atoms
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C), atom(C)
    ), RawVisible),
    sort(RawVisible, Visible),
    length(Visible, NVisible),
    format('VISIBLE_CLAIMS: ~w~n', [NVisible]),

    % Count all constraint_claim including non-atom/list forms
    findall(C, narrative_ontology:constraint_claim(C, _), AllClaims),
    length(AllClaims, NAllClaims),
    format('ALL_CLAIMS_RAW: ~w~n', [NAllClaims]),

    % Count list-form claims
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        is_list(C)
    ), ListClaims),
    length(ListClaims, NListClaims),
    format('LIST_FORM_CLAIMS: ~w~n', [NListClaims]),
    (   ListClaims \= []
    ->  forall(member(LC, ListClaims),
            format('  LIST_CLAIM: ~w~n', [LC]))
    ;   true
    ),

    % Count non-atom claims (excluding lists)
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        \+ atom(C)
    ), NonAtomClaims),
    length(NonAtomClaims, NNonAtom),
    format('NON_ATOM_CLAIMS: ~w~n', [NNonAtom]),

    % Count testset files
    expand_file_name('testsets/*.pl', Files),
    length(Files, NFiles),
    format('TESTSET_FILES: ~w~n', [NFiles]),

    % Determine which files' constraints are missing
    % Extract expected constraint ID from filename
    findall(ExpID-File, (
        member(File, Files),
        file_base_name(File, BaseName),
        file_name_extension(Base, _, BaseName),
        atom_string(ExpID, Base)
    ), ExpectedPairs),
    length(ExpectedPairs, NExpected),
    format('EXPECTED_IDS: ~w~n', [NExpected]),

    % Check which expected IDs are NOT in visible claims
    findall(ExpID-File, (
        member(ExpID-File, ExpectedPairs),
        \+ member(ExpID, Visible)
    ), MissingPairs),
    length(MissingPairs, NMissing),
    format('MISSING_FROM_CLAIMS: ~w~n', [NMissing]),

    % List missing constraint IDs (first 50)
    format('MISSING_IDS_SAMPLE:~n'),
    (   MissingPairs = []
    ->  format('  (none)~n')
    ;   length(MissingPairs, MLen),
        (MLen > 50 -> length(Sample, 50), append(Sample, _, MissingPairs) ; Sample = MissingPairs),
        forall(member(MID-_MFile, Sample),
            format('  ~w~n', [MID]))
    ),

    % Check which visible constraints have maxent_dist vs don't
    findall(C, maxent_classifier:maxent_dist(C, Context, _), DistConstraints),
    sort(DistConstraints, UniqueDistCs),
    length(UniqueDistCs, NWithDist),
    format('CONSTRAINTS_WITH_DIST: ~w~n', [NWithDist]),

    % Constraints visible but without distribution (silent failure in classify_one)
    findall(C, (
        member(C, Visible),
        \+ maxent_classifier:maxent_dist(C, Context, _)
    ), NoDist),
    length(NoDist, NNoDist),
    format('VISIBLE_BUT_NO_DIST: ~w~n', [NNoDist]),
    (   NoDist \= []
    ->  forall(member(ND, NoDist), (
            % Check why — does it have metrics?
            (drl_core:base_extractiveness(ND, Eps) -> true ; Eps = missing),
            (drl_core:get_raw_suppression(ND, Supp) -> true ; Supp = missing),
            (drl_core:dr_type(ND, Context, DT) -> true ; DT = no_type),
            format('  NO_DIST: ~w eps=~w supp=~w det_type=~w~n', [ND, Eps, Supp, DT])
        ))
    ;   true
    ),

    % Categorize visible constraints by deterministic type
    findall(T, (
        member(C, Visible),
        (drl_core:dr_type(C, Context, T) -> true ; T = no_dr_type)
    ), DetTypes),
    msort(DetTypes, SortedDT),
    count_occurrences(SortedDT, DTCounts),
    format('DET_TYPE_DISTRIBUTION:~n'),
    forall(member(Type-Count, DTCounts),
        format('  ~w: ~w~n', [Type, Count])),

    % Constraints claimed but with unknown/indexically_opaque det type
    findall(C-T, (
        member(C, Visible),
        drl_core:dr_type(C, Context, T),
        (T = unknown ; T = indexically_opaque)
    ), ResidualCs),
    length(ResidualCs, NResidual),
    format('RESIDUAL_TYPE_COUNT: ~w~n', [NResidual]),

    format('=== END TASK 1 ===~n').

/* ================================================================
   TASK 2: PER-TYPE ENTROPY BREAKDOWN
   ================================================================ */

task2_per_type_entropy(Context) :-
    format('~n=== TASK 2: PER-TYPE ENTROPY BREAKDOWN ===~n'),

    % Collect all constraints with distributions
    findall(row(C, HNorm, DetType), (
        maxent_classifier:maxent_dist(C, Context, _),
        maxent_classifier:maxent_entropy(C, Context, HNorm),
        (drl_core:dr_type(C, Context, DetType) -> true ; DetType = unknown)
    ), Rows),

    % Get unique types
    findall(T, member(row(_, _, T), Rows), Types),
    sort(Types, UniqueTypes),

    format('TYPE_ENTROPY_TABLE:~n'),
    format('Type|Count|Mean|Median|Min|Max|StdDev~n'),
    forall(member(Type, UniqueTypes), (
        findall(H, member(row(_, H, Type), Rows), Entropies),
        length(Entropies, Count),
        (   Count > 0
        ->  sum_list(Entropies, Sum),
            MeanH is Sum / Count,
            msort(Entropies, Sorted),
            nth0_median(Sorted, Count, MedianH),
            min_list(Entropies, MinH),
            max_list(Entropies, MaxH),
            std_dev(Entropies, MeanH, Count, StdDev),
            format('~w|~w|~6f|~6f|~6f|~6f|~6f~n',
                   [Type, Count, MeanH, MedianH, MinH, MaxH, StdDev])
        ;   true
        )
    )),
    format('=== END TASK 2 ===~n').

/* ================================================================
   TASK 3: HARD DISAGREEMENT ANALYSIS
   ================================================================ */

task3_hard_disagreements(Context) :-
    format('~n=== TASK 3: HARD DISAGREEMENTS ===~n'),

    % Collect all hard disagreements
    maxent_classifier:maxent_disagreements(Context, AllDisagreements),
    include(is_hard_d, AllDisagreements, Hards),
    length(Hards, NHard),
    format('TOTAL_HARD: ~w~n', [NHard]),

    % Cross-tabulate (DetType, ShadowType) pairs
    findall(DetType-ShadowType, member(_-hard(ShadowType, DetType), Hards), Pairs),
    msort(Pairs, SortedPairs),
    count_occurrences(SortedPairs, PairCounts),
    format('DISAGREEMENT_PAIRS:~n'),
    format('DetType->ShadowType|Count~n'),
    forall(member((DT-ST)-Count, PairCounts),
        format('~w->~w|~w~n', [DT, ST, Count])),

    % Shadow confidence for each hard disagreement
    format('HARD_DISAGREEMENT_DETAILS:~n'),
    format('Constraint|DetType|ShadowType|ShadowTopP|ShadowConf|Distribution~n'),
    forall(member(C-hard(ShadowType, DetType), Hards), (
        (   maxent_classifier:maxent_distribution(C, Context, Dist)
        ->  (member(ShadowType-ShadowP, Dist) -> true ; ShadowP = 0.0),
            maxent_classifier:maxent_entropy(C, Context, HN),
            Conf is 1.0 - HN,
            format_dist_compact(Dist, DistStr),
            format('~w|~w|~w|~6f|~6f|~w~n',
                   [C, DetType, ShadowType, ShadowP, Conf, DistStr])
        ;   format('~w|~w|~w|NA|NA|NA~n', [C, DetType, ShadowType])
        )
    )),

    % Count cluster membership
    findall(C, (
        member(C-hard(ST, DT), Hards),
        rope_cluster(DT), rope_cluster(ST)
    ), ClusterOnly),
    length(ClusterOnly, NCluster),
    findall(C, (
        member(C-hard(ST, DT), Hards),
        (\+ rope_cluster(DT) ; \+ rope_cluster(ST))
    ), InvolvesNonCluster),
    length(InvolvesNonCluster, NNonCluster),
    format('ROPE_CLUSTER_ONLY: ~w~n', [NCluster]),
    format('INVOLVES_MTN_SCAFFOLD_PITON: ~w~n', [NNonCluster]),

    % Mountain/piton disagreements — individual investigation
    format('MOUNTAIN_PITON_DISAGREEMENTS:~n'),
    forall(member(C-hard(ShadowType, DetType), Hards), (
        (   (DetType = mountain ; DetType = piton ; ShadowType = mountain ; ShadowType = piton)
        ->  (drl_core:base_extractiveness(C, Eps) -> true ; Eps = na),
            (drl_core:get_raw_suppression(C, Supp) -> true ; Supp = na),
            config:param(theater_metric_name, TN),
            (narrative_ontology:constraint_metric(C, TN, Theater) -> true ; Theater = na),
            (catch(structural_signatures:constraint_signature(C, Sig), _, Sig = na) -> true ; Sig = na),
            (maxent_classifier:maxent_distribution(C, Context, Dist2)
             -> format_dist_compact(Dist2, DS2)
             ;  DS2 = na),
            format('  MTN_PITON: ~w det=~w shadow=~w eps=~w supp=~w theater=~w sig=~w dist=~w~n',
                   [C, DetType, ShadowType, Eps, Supp, Theater, Sig, DS2])
        ;   true
        )
    )),

    % Mean shadow confidence across all hard disagreements
    findall(P, (
        member(C-hard(ST, _), Hards),
        maxent_classifier:maxent_distribution(C, Context, Dist3),
        member(ST-P, Dist3)
    ), ShadowPs),
    (   ShadowPs \= []
    ->  sum_list(ShadowPs, SumP),
        length(ShadowPs, NP),
        MeanShadowP is SumP / NP,
        format('MEAN_SHADOW_TOP_P: ~6f~n', [MeanShadowP])
    ;   format('MEAN_SHADOW_TOP_P: NA~n')
    ),

    format('=== END TASK 3 ===~n').

is_hard_d(_-hard(_, _)).
rope_cluster(rope).
rope_cluster(tangled_rope).
rope_cluster(snare).

/* ================================================================
   TASK 4: NON-OVERLAPPING POPULATION (15.8%)
   ================================================================ */

task4_non_overlapping(Context) :-
    format('~n=== TASK 4: NON-OVERLAPPING POPULATION ===~n'),

    maxent_classifier:maxent_disagreements(Context, AllDisagreements),
    include(is_hard_d, AllDisagreements, Hards),
    findall(C, member(C-hard(_, _), Hards), HardCs),

    % Split into multi-type orbit vs single-type orbit
    findall(C, (
        member(C, HardCs),
        has_multi_type_orbit(C)
    ), MultiTypeCs),
    findall(C, (
        member(C, HardCs),
        \+ has_multi_type_orbit(C)
    ), SingleTypeCs),

    length(HardCs, NHard),
    length(MultiTypeCs, NMulti),
    length(SingleTypeCs, NSingle),
    format('HARD_TOTAL: ~w~n', [NHard]),
    format('MULTI_TYPE_ORBIT: ~w~n', [NMulti]),
    format('SINGLE_TYPE_ORBIT: ~w~n', [NSingle]),
    (   NHard > 0
    ->  OverlapPct is NMulti * 100.0 / NHard,
        format('OVERLAP_PCT: ~2f~n', [OverlapPct])
    ;   true
    ),

    % Detail each single-type-orbit hard disagreement
    format('SINGLE_TYPE_DETAILS:~n'),
    format('Constraint|DetType|ShadowType|OrbitTypes|Eps|Supp|Theater|NearestBoundary|BoundaryDist~n'),
    forall(member(C, SingleTypeCs), (
        (   member(C-hard(ST, DT), Hards) -> true ; ST = na, DT = na ),
        % Get orbit
        (   catch(dirac_classification:gauge_orbit(C, OrbitPts), _, OrbitPts = [])
        ->  findall(OT, member(orbit_point(OT, _), OrbitPts), OTypes),
            sort(OTypes, UniqueOTypes),
            atomic_list_concat(UniqueOTypes, '/', OrbitStr)
        ;   OrbitStr = 'no_orbit'
        ),
        % Get metrics
        (drl_core:base_extractiveness(C, Eps) -> true ; Eps = na),
        (drl_core:get_raw_suppression(C, Supp) -> true ; Supp = na),
        config:param(theater_metric_name, TN4),
        (narrative_ontology:constraint_metric(C, TN4, Theater) -> true ; Theater = na),
        % Nearest boundary
        (   catch(nearest_boundary(C, Context, BName, BDist), _, fail)
        ->  true
        ;   BName = na, BDist = na
        ),
        format('~w|~w|~w|~w|~w|~w|~w|~w|~w~n',
               [C, DT, ST, OrbitStr, Eps, Supp, Theater, BName, BDist])
    )),

    % Inverse: multi-type orbit + LOW entropy (< 0.30)
    format('INVERSE_CHECK:~n'),
    format('Constraint|DetType|H_norm|OrbitTypes~n'),
    findall(C, (
        maxent_classifier:maxent_dist(C, Context, _),
        has_multi_type_orbit(C),
        maxent_classifier:maxent_entropy(C, Context, HN),
        HN < 0.30
    ), InverseCs),
    length(InverseCs, NInverse),
    format('INVERSE_COUNT: ~w~n', [NInverse]),
    forall(member(C, InverseCs), (
        maxent_classifier:maxent_entropy(C, Context, HN2),
        (drl_core:dr_type(C, Context, DT2) -> true ; DT2 = unknown),
        (   catch(dirac_classification:gauge_orbit(C, OPts2), _, OPts2 = [])
        ->  findall(OT2, member(orbit_point(OT2, _), OPts2), OTs2),
            sort(OTs2, UOTs2),
            atomic_list_concat(UOTs2, '/', OStr2)
        ;   OStr2 = 'no_orbit'
        ),
        format('~w|~w|~6f|~w~n', [C, DT2, HN2, OStr2])
    )),

    format('=== END TASK 4 ===~n').

has_multi_type_orbit(C) :-
    catch(dirac_classification:gauge_orbit(C, OrbitPoints), _, fail),
    findall(T, member(orbit_point(T, _), OrbitPoints), Types),
    sort(Types, UniqueTypes),
    length(UniqueTypes, N),
    N > 1.

nearest_boundary(C, Context, BestName, BestDist) :-
    findall(Dist-Name,
        catch(maxent_classifier:maxent_threshold_proximity(C, Context, Name, Dist), _, fail),
        Pairs),
    Pairs \= [],
    msort(Pairs, [BestDist-BestName|_]).

/* ================================================================
   TASK 5: GAUSSIAN PROFILE FIT
   ================================================================ */

task5_gaussian_profiles(Context) :-
    format('~n=== TASK 5: GAUSSIAN PROFILES ===~n'),

    % Report all empirical profiles
    format('EMPIRICAL_PROFILES:~n'),
    format('Type|Metric|Mu|Sigma~n'),
    forall(maxent_classifier:maxent_profile(Type, Metric, params(Mu, Sigma)),
        format('~w|~w|~6f|~6f~n', [Type, Metric, Mu, Sigma])),

    % Flag suspiciously large sigmas
    format('LARGE_SIGMA_FLAGS:~n'),
    forall((
        maxent_classifier:maxent_profile(Type, Metric, params(_Mu, Sigma)),
        Sigma > 0.25
    ), format('  LARGE_SIGMA: ~w ~w sigma=~6f~n', [Type, Metric, Sigma])),

    % Rope BaseEps distribution — check for bimodality
    format('ROPE_EPS_DISTRIBUTION:~n'),
    findall(Eps, (
        maxent_classifier:maxent_dist(C, Context, _),
        drl_core:dr_type(C, Context, rope),
        drl_core:base_extractiveness(C, Eps)
    ), RopeEpsList),
    length(RopeEpsList, NRopeEps),
    format('ROPE_EPS_COUNT: ~w~n', [NRopeEps]),
    (   RopeEpsList \= []
    ->  msort(RopeEpsList, SortedRE),
        forall(member(E, SortedRE), format('  ROPE_EPS: ~6f~n', [E])),
        % Basic bimodality check: count in [0, 0.25] vs [0.25, 0.50] vs [0.50, 1.0]
        include(below_025, RopeEpsList, Bin1),
        include(between_025_050, RopeEpsList, Bin2),
        include(above_050, RopeEpsList, Bin3),
        length(Bin1, N1), length(Bin2, N2), length(Bin3, N3),
        format('ROPE_EPS_BINS: [0,0.25]=~w [0.25,0.50]=~w [0.50,1.0]=~w~n', [N1, N2, N3])
    ;   true
    ),

    % Override-rope analysis
    format('OVERRIDE_ROPE_ANALYSIS:~n'),
    findall(C-Sig-Eps-HN, (
        maxent_classifier:maxent_dist(C, Context, _),
        drl_core:dr_type(C, Context, rope),
        catch(structural_signatures:constraint_signature(C, Sig), _, fail),
        is_override_sig(Sig),
        drl_core:base_extractiveness(C, Eps),
        maxent_classifier:maxent_entropy(C, Context, HN)
    ), OverrideRopes),
    length(OverrideRopes, NOR),
    format('OVERRIDE_ROPE_COUNT: ~w~n', [NOR]),
    forall(member(OC-OSig-OEps-OHN, OverrideRopes),
        format('  OVERRIDE_ROPE: ~w sig=~w eps=~6f hn=~6f~n', [OC, OSig, OEps, OHN])),

    % Non-override rope entropy
    findall(HN, (
        maxent_classifier:maxent_dist(C, Context, _),
        drl_core:dr_type(C, Context, rope),
        maxent_classifier:maxent_entropy(C, Context, HN),
        \+ (catch(structural_signatures:constraint_signature(C, Sig), _, fail),
            is_override_sig(Sig))
    ), NonOverrideEntropies),
    (   NonOverrideEntropies \= []
    ->  sum_list(NonOverrideEntropies, SumNO),
        length(NonOverrideEntropies, NNO),
        MeanNO is SumNO / NNO,
        format('NON_OVERRIDE_ROPE_ENTROPY_MEAN: ~6f (n=~w)~n', [MeanNO, NNO])
    ;   format('NON_OVERRIDE_ROPE_ENTROPY_MEAN: NA~n')
    ),

    % Override rope entropy
    findall(HN, (
        member(_-_-_-HN, OverrideRopes)
    ), OverrideEntropies),
    (   OverrideEntropies \= []
    ->  sum_list(OverrideEntropies, SumOE),
        length(OverrideEntropies, NOE),
        MeanOE is SumOE / NOE,
        format('OVERRIDE_ROPE_ENTROPY_MEAN: ~6f (n=~w)~n', [MeanOE, NOE])
    ;   format('OVERRIDE_ROPE_ENTROPY_MEAN: NA~n')
    ),

    % Check all types for potential bimodality (look at eps distributions)
    format('ALL_TYPE_EPS_STATS:~n'),
    forall(maxent_classifier:maxent_type(Type), (
        findall(Eps, (
            maxent_classifier:maxent_dist(C, Context, _),
            drl_core:dr_type(C, Context, Type),
            drl_core:base_extractiveness(C, Eps)
        ), TypeEpsList),
        length(TypeEpsList, NTE),
        (   NTE > 1
        ->  sum_list(TypeEpsList, SumTE),
            MeanTE is SumTE / NTE,
            std_dev(TypeEpsList, MeanTE, NTE, StdTE),
            min_list(TypeEpsList, MinTE),
            max_list(TypeEpsList, MaxTE),
            format('  ~w: n=~w mean=~6f std=~6f min=~6f max=~6f~n',
                   [Type, NTE, MeanTE, StdTE, MinTE, MaxTE])
        ;   format('  ~w: n=~w (insufficient)~n', [Type, NTE])
        )
    )),

    format('=== END TASK 5 ===~n').

is_override_sig(coupling_invariant_rope).
is_override_sig(coordination_scaffold).
is_override_sig(constructed_low_extraction).

below_025(X) :- X < 0.25.
between_025_050(X) :- X >= 0.25, X < 0.50.
above_050(X) :- X >= 0.50.

/* ================================================================
   TASK 6: CROSS-DIAGNOSTIC CORRELATION
   ================================================================ */

task6_cross_diagnostic(Context) :-
    format('~n=== TASK 6: CROSS-DIAGNOSTIC CORRELATION ===~n'),

    config:param(maxent_uncertainty_threshold, Threshold),

    % High entropy population
    findall(C, (
        maxent_classifier:maxent_dist(C, Context, _),
        maxent_classifier:maxent_entropy(C, Context, HN),
        HN > Threshold
    ), HighEntropyCs),
    length(HighEntropyCs, NHigh),
    format('HIGH_ENTROPY_COUNT: ~w (threshold=~4f)~n', [NHigh, Threshold]),

    % Low entropy population (for comparison)
    findall(C, (
        maxent_classifier:maxent_dist(C, Context, _),
        maxent_classifier:maxent_entropy(C, Context, HN),
        HN =< Threshold
    ), LowEntropyCs),
    length(LowEntropyCs, NLow),

    % Omega variables
    count_with_omega(HighEntropyCs, NOmegaHigh),
    count_with_omega(LowEntropyCs, NOmegaLow),
    (NHigh > 0 -> OmegaHighPct is NOmegaHigh * 100.0 / NHigh ; OmegaHighPct = 0.0),
    (NLow > 0 -> OmegaLowPct is NOmegaLow * 100.0 / NLow ; OmegaLowPct = 0.0),
    format('OMEGA_HIGH_ENTROPY: ~w/~w (~2f%)~n', [NOmegaHigh, NHigh, OmegaHighPct]),
    format('OMEGA_LOW_ENTROPY: ~w/~w (~2f%)~n', [NOmegaLow, NLow, OmegaLowPct]),

    % Boltzmann non-compliance
    count_boltzmann_non_compliant(HighEntropyCs, NBoltzHigh),
    count_boltzmann_non_compliant(LowEntropyCs, NBoltzLow),
    (NHigh > 0 -> BoltzHighPct is NBoltzHigh * 100.0 / NHigh ; BoltzHighPct = 0.0),
    (NLow > 0 -> BoltzLowPct is NBoltzLow * 100.0 / NLow ; BoltzLowPct = 0.0),
    format('BOLTZMANN_NC_HIGH_ENTROPY: ~w/~w (~2f%)~n', [NBoltzHigh, NHigh, BoltzHighPct]),
    format('BOLTZMANN_NC_LOW_ENTROPY: ~w/~w (~2f%)~n', [NBoltzLow, NLow, BoltzLowPct]),

    % Purity score < 0.50
    count_low_purity(HighEntropyCs, NPurityHigh),
    count_low_purity(LowEntropyCs, NPurityLow),
    (NHigh > 0 -> PurityHighPct is NPurityHigh * 100.0 / NHigh ; PurityHighPct = 0.0),
    (NLow > 0 -> PurityLowPct is NPurityLow * 100.0 / NLow ; PurityLowPct = 0.0),
    format('LOW_PURITY_HIGH_ENTROPY: ~w/~w (~2f%)~n', [NPurityHigh, NHigh, PurityHighPct]),
    format('LOW_PURITY_LOW_ENTROPY: ~w/~w (~2f%)~n', [NPurityLow, NLow, PurityLowPct]),

    % Purity score available
    count_purity_available(HighEntropyCs, NPAvailHigh),
    count_purity_available(LowEntropyCs, NPAvailLow),
    format('PURITY_AVAILABLE_HIGH: ~w/~w~n', [NPAvailHigh, NHigh]),
    format('PURITY_AVAILABLE_LOW: ~w/~w~n', [NPAvailLow, NLow]),

    % Average purity for high vs low entropy
    avg_purity_for(HighEntropyCs, AvgPHigh),
    avg_purity_for(LowEntropyCs, AvgPLow),
    format('AVG_PURITY_HIGH_ENTROPY: ~w~n', [AvgPHigh]),
    format('AVG_PURITY_LOW_ENTROPY: ~w~n', [AvgPLow]),

    % Multi-type orbit rate for high vs low
    count_multi_orbit(HighEntropyCs, NMultiHigh),
    count_multi_orbit(LowEntropyCs, NMultiLow),
    (NHigh > 0 -> MultiHighPct is NMultiHigh * 100.0 / NHigh ; MultiHighPct = 0.0),
    (NLow > 0 -> MultiLowPct is NMultiLow * 100.0 / NLow ; MultiLowPct = 0.0),
    format('MULTI_ORBIT_HIGH_ENTROPY: ~w/~w (~2f%)~n', [NMultiHigh, NHigh, MultiHighPct]),
    format('MULTI_ORBIT_LOW_ENTROPY: ~w/~w (~2f%)~n', [NMultiLow, NLow, MultiLowPct]),

    format('=== END TASK 6 ===~n').

count_with_omega(Cs, N) :-
    findall(C, (
        member(C, Cs),
        narrative_ontology:omega_variable(C, _, _)
    ), Found),
    sort(Found, Unique),
    length(Unique, N).

count_boltzmann_non_compliant(Cs, N) :-
    findall(C, (
        member(C, Cs),
        catch(structural_signatures:boltzmann_compliant(C, non_compliant(_, _)), _, fail)
    ), Found),
    sort(Found, Unique),
    length(Unique, N).

count_low_purity(Cs, N) :-
    findall(C, (
        member(C, Cs),
        catch(structural_signatures:purity_score(C, P), _, fail),
        P >= 0.0,  % Exclude -1.0 sentinel
        P < 0.50
    ), Found),
    sort(Found, Unique),
    length(Unique, N).

count_purity_available(Cs, N) :-
    findall(C, (
        member(C, Cs),
        catch(structural_signatures:purity_score(C, P), _, fail),
        P >= 0.0
    ), Found),
    sort(Found, Unique),
    length(Unique, N).

avg_purity_for(Cs, Avg) :-
    findall(P, (
        member(C, Cs),
        catch(structural_signatures:purity_score(C, P), _, fail),
        P >= 0.0
    ), Ps),
    (   Ps \= []
    ->  sum_list(Ps, Sum),
        length(Ps, N),
        Avg is Sum / N
    ;   Avg = na
    ).

count_multi_orbit(Cs, N) :-
    findall(C, (
        member(C, Cs),
        has_multi_type_orbit(C)
    ), Found),
    sort(Found, Unique),
    length(Unique, N).

/* ================================================================
   UTILITY PREDICATES
   ================================================================ */

nth0_median(Sorted, Count, Median) :-
    MidIdx is Count // 2,
    nth0(MidIdx, Sorted, Median).

std_dev(Values, Mean, Count, StdDev) :-
    foldl(sum_sq_diff_acc(Mean), Values, 0.0, SumSqDiff),
    Variance is SumSqDiff / Count,
    StdDev is sqrt(Variance).

sum_sq_diff_acc(Mean, X, Acc, NewAcc) :-
    NewAcc is Acc + (X - Mean) * (X - Mean).

count_occurrences([], []).
count_occurrences([X|Rest], [X-Count|Counts]) :-
    count_leading(X, [X|Rest], Count, Remainder),
    count_occurrences(Remainder, Counts).

count_leading(_, [], 0, []).
count_leading(X, [X|Rest], Count, Remainder) :-
    !, count_leading(X, Rest, SubCount, Remainder),
    Count is SubCount + 1.
count_leading(_, List, 0, List).

format_dist_compact(Dist, Str) :-
    findall(Atom, (
        member(Type-P, Dist),
        P > 0.01,
        format(atom(Atom), '~w:~3f', [Type, P])
    ), Parts),
    atomic_list_concat(Parts, ' ', Str).
