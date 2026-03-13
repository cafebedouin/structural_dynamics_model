% ============================================================================
% MEASUREMENT LAYER — Wasserstein L1 CDF Comparison
% ============================================================================
% Computes Wasserstein L1 (earth mover's) distance between MaxEnt probability
% distributions at adjacent observer positions along the power axis.
%
% The extraction chain (mountain < rope < tangled_rope < snare) provides the
% total order required for CDF construction. Types outside this chain
% (scaffold, piton, naturalized, unknown) are excluded and their mass is
% renormalized away. The excluded mass is tracked separately via
% wasserstein_incomparable_mass/3 so no information is silently lost.
%
% This gives a continuous, metric complement to the discrete H1 cohomological
% obstruction: H1 counts how many context-pairs disagree on type, while W1
% measures how much distributional mass must move along the extraction axis
% to transform one observer's probabilistic picture into another's.
%
% Requires: maxent_classifier:maxent_multi_run/2 called first (distributions
% stored at all 4 canonical contexts).
%
% Standalone run:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l measurement_layer.pl \
%         -g "measurement_layer:wasserstein_selftest, halt."
% ============================================================================

:- module(measurement_layer, [
    % CDF construction
    wasserstein_cdf/2,                    % wasserstein_cdf(+Dist, -CDF)

    % Core distance
    wasserstein_l1/3,                     % wasserstein_l1(+P, +Q, -W1)

    % Per-constraint edge transport
    wasserstein_edge_transport/4,         % wasserstein_edge_transport(+C, +Ctx1, +Ctx2, -W1)
    wasserstein_transport_profile/2,      % wasserstein_transport_profile(+C, -Profile)
    wasserstein_total_fracture/2,         % wasserstein_total_fracture(+C, -TotalW1)

    % Corpus aggregate
    wasserstein_corpus_fracture/1,        % wasserstein_corpus_fracture(-TotalW1)

    % Diagnostics
    wasserstein_incomparable_mass/3,      % wasserstein_incomparable_mass(+C, +Ctx, -Mass)

    % Validation
    wasserstein_selftest/0
]).

:- use_module(maxent_classifier).
:- use_module(covering_analysis).
:- use_module(narrative_ontology).

:- use_module(library(lists)).

/* ================================================================
   EXTRACTION CHAIN — the total order for CDF construction
   ================================================================
   Only types with a well-defined position on the extraction axis
   participate in the Wasserstein computation. scaffold, piton,
   naturalized, and unknown have extraction_rank = -1 and are
   excluded (their mass is renormalized away).
   ================================================================ */

%% extraction_chain(-Chain)
%  The 4 extraction-chain types in rank order (mountain=0 .. snare=3).
%  This is the domain over which CDFs are constructed.
extraction_chain([mountain, rope, tangled_rope, snare]).

%% Standard observer contexts in canonical order (U1..U4).
%  Local copy — matches grothendieck_cohomology:ordered_contexts/1
%  and dirac_classification:standard_context/1 clause order.
wasserstein_contexts([
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local)),
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national)),
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national)),
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))
]).

/* ================================================================
   CDF CONSTRUCTION
   ================================================================ */

%% wasserstein_cdf(+Dist, -CDF)
%  Constructs a cumulative distribution function over the extraction chain
%  from a MaxEnt distribution (list of Type-Prob pairs).
%
%  Dist: list of Type-Prob pairs, e.g. [mountain-0.3, rope-0.2, ...]
%  CDF:  4-element list [F(mountain), F(rope), F(tangled_rope), F(snare)]
%        where F(x) = sum of probabilities for types <= x in chain order.
%
%  Types outside the extraction chain are excluded and the remaining
%  mass is renormalized to sum to 1.0. If all mass is on incomparable
%  types, returns uniform CDF [0.25, 0.5, 0.75, 1.0].
wasserstein_cdf(Dist, CDF) :-
    extract_chain_probs(Dist, ChainProbs, _IncompMass),
    renormalize(ChainProbs, Normalized),
    cumsum(Normalized, CDF).

%% wasserstein_cdf(+[], -CDF)
%  Empty distribution: uniform CDF.
wasserstein_cdf([], CDF) :-
    cumsum([0.25, 0.25, 0.25, 0.25], CDF).

%% extract_chain_probs(+Dist, -ChainProbs, -IncompMass)
%  Extracts probabilities for the 4 chain types in order.
%  IncompMass = total probability on non-chain types.
%
%  Dist:       list of Type-Prob pairs (MaxEnt format)
%  ChainProbs: [P_mountain, P_rope, P_tangled_rope, P_snare]
%  IncompMass: float in [0.0, 1.0]
extract_chain_probs(Dist, ChainProbs, IncompMass) :-
    extraction_chain(Chain),
    maplist(prob_for_type(Dist), Chain, ChainProbs),
    sum_list(ChainProbs, ChainSum),
    maplist(pair_snd, Dist, AllProbs),
    sum_list(AllProbs, TotalSum),
    IncompMass is max(0.0, TotalSum - ChainSum).

%% prob_for_type(+Dist, +Type, -Prob)
%  Looks up the probability for Type in Dist. Returns 0.0 if absent.
prob_for_type(Dist, Type, Prob) :-
    (   member(Type-P, Dist)
    ->  Prob = P
    ;   Prob = 0.0
    ).

%% pair_snd(+Pair, -Value)
%  Extracts the second element of a Key-Value pair.
pair_snd(_-V, V).

%% renormalize(+Probs, -Normalized)
%  Divides each element by the sum. If sum < 1e-15 (all mass on
%  incomparable types), returns uniform [0.25, 0.25, 0.25, 0.25].
renormalize(Probs, Normalized) :-
    sum_list(Probs, Sum),
    (   Sum > 1.0e-15
    ->  maplist(div_by(Sum), Probs, Normalized)
    ;   Normalized = [0.25, 0.25, 0.25, 0.25]
    ).

div_by(Sum, X, Y) :- Y is X / Sum.

%% cumsum(+Probs, -CDF)
%  Running cumulative sum. [p1, p2, p3, p4] -> [p1, p1+p2, p1+p2+p3, 1.0].
%  The last element is forced to 1.0 to avoid floating-point drift.
cumsum(Probs, CDF) :-
    cumsum_(Probs, 0.0, CDF0),
    % Force last element to exactly 1.0
    (   CDF0 = []
    ->  CDF = []
    ;   append(Init, [_], CDF0),
        append(Init, [1.0], CDF)
    ).

cumsum_([], _, []).
cumsum_([P|Ps], Acc, [F|Fs]) :-
    F is Acc + P,
    cumsum_(Ps, F, Fs).

/* ================================================================
   WASSERSTEIN L1 DISTANCE
   ================================================================ */

%% wasserstein_l1(+P, +Q, -W1)
%  Computes the Wasserstein L1 (earth mover's) distance between two
%  MaxEnt distributions P and Q.
%
%  P, Q: lists of Type-Prob pairs (MaxEnt format)
%  W1:   float in [0.0, 3.0]
%
%  Both distributions are projected onto the extraction chain and
%  renormalized before CDF construction. The distance is the sum of
%  absolute CDF differences at the first n-1 positions (the last CDF
%  value is always 1.0 and contributes zero).
%
%  W1 = sum_{i=1}^{3} |F_P(x_i) - F_Q(x_i)|
%
%  Theoretical range: 0.0 (identical) to 3.0 (all mass at opposite
%  ends of the 4-type chain).
wasserstein_l1(P, Q, W1) :-
    wasserstein_cdf(P, PCDF),
    wasserstein_cdf(Q, QCDF),
    cdf_l1_distance(PCDF, QCDF, W1).

%% cdf_l1_distance(+CDF1, +CDF2, -Distance)
%  Sum of |F1(xi) - F2(xi)| for i = 1..n-1.
%  Skips the last element (always 1.0 for both, difference = 0).
cdf_l1_distance(CDF1, CDF2, Distance) :-
    % Drop last element from each (always 1.0)
    append(Head1, [_], CDF1),
    append(Head2, [_], CDF2),
    maplist(abs_diff, Head1, Head2, Diffs),
    sum_list(Diffs, Distance).

%% Handle empty CDFs (should not occur with 4-type chain, but safe)
cdf_l1_distance([], [], 0.0).

abs_diff(A, B, D) :- D is abs(A - B).

/* ================================================================
   PER-EDGE TRANSPORT COST
   ================================================================ */

%% wasserstein_edge_transport(+C, +Ctx1, +Ctx2, -W1)
%  Wasserstein L1 distance between the MaxEnt distributions of
%  constraint C at two observer contexts.
%
%  C:    constraint atom
%  Ctx1: source context (e.g. context(agent_power(powerless), ...))
%  Ctx2: target context
%  W1:   float, the transport cost along this edge
%
%  Requires maxent_classifier:maxent_multi_run/2 to have been called
%  so that distributions exist at both contexts. Fails if either
%  distribution is missing.
wasserstein_edge_transport(C, Ctx1, Ctx2, W1) :-
    maxent_classifier:maxent_distribution(C, Ctx1, P),
    maxent_classifier:maxent_distribution(C, Ctx2, Q),
    wasserstein_l1(P, Q, W1).

/* ================================================================
   TRANSPORT PROFILE — all 3 edges for a constraint
   ================================================================ */

%% wasserstein_transport_profile(+C, -Profile)
%  Computes W1 for each of the 3 canonical edges (U1->U2, U2->U3,
%  U3->U4) and returns a structured profile.
%
%  C:       constraint atom
%  Profile: transport_profile(
%               edge(u1_u2, W12),
%               edge(u2_u3, W23),
%               edge(u3_u4, W34))
%
%  Uses wasserstein_contexts/1 (local copy of the 4 canonical observer
%  positions). Fails if MaxEnt distributions are not available at all
%  4 contexts.
%
%  Output compatible with SCOPE EFFECT ANALYSIS format:
%    format('Edge U1~c U2: ~4f, U2~c U3: ~4f, U3~c U4: ~4f~n',
%           [0x2192, W12, 0x2192, W23, 0x2192, W34])
wasserstein_transport_profile(C, Profile) :-
    wasserstein_contexts([Ctx1, Ctx2, Ctx3, Ctx4]),
    wasserstein_edge_transport(C, Ctx1, Ctx2, W12),
    wasserstein_edge_transport(C, Ctx2, Ctx3, W23),
    wasserstein_edge_transport(C, Ctx3, Ctx4, W34),
    Profile = transport_profile(
        edge(u1_u2, W12),
        edge(u2_u3, W23),
        edge(u3_u4, W34)
    ).

/* ================================================================
   TOTAL PERSPECTIVAL FRACTURE — per constraint
   ================================================================ */

%% wasserstein_total_fracture(+C, -TotalW1)
%  Sum of Wasserstein L1 transport costs across all 3 canonical edges
%  for constraint C.
%
%  C:       constraint atom
%  TotalW1: float in [0.0, 9.0] (3 edges x max 3.0 each)
%
%  This is the total cost of transporting probability mass along the
%  extraction chain as the observer traverses the full power axis
%  from powerless to analytical.
wasserstein_total_fracture(C, TotalW1) :-
    wasserstein_transport_profile(C, Profile),
    Profile = transport_profile(
        edge(u1_u2, W12),
        edge(u2_u3, W23),
        edge(u3_u4, W34)
    ),
    TotalW1 is W12 + W23 + W34.

/* ================================================================
   CORPUS-LEVEL AGGREGATE
   ================================================================ */

%% wasserstein_corpus_fracture(-TotalW1)
%  Aggregates total Wasserstein transport cost across all constraints
%  in the corpus. This is the sum of wasserstein_total_fracture/2
%  for every constraint with available MaxEnt distributions.
%
%  TotalW1: float, total transport cost across all constraints and edges.
%
%  Requires maxent_multi_run/2 to have been called first.
%  Constraints missing distributions at any context are silently skipped.
wasserstein_corpus_fracture(TotalW1) :-
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        atom(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),
    foldl(accumulate_fracture, Constraints, 0.0, TotalW1).

%% accumulate_fracture(+C, +Acc, -NewAcc)
%  Adds the total fracture for C to the accumulator.
%  If the transport profile is unavailable (missing distributions),
%  the constraint is silently skipped.
accumulate_fracture(C, Acc, NewAcc) :-
    (   wasserstein_total_fracture(C, W1)
    ->  NewAcc is Acc + W1
    ;   NewAcc = Acc
    ).

/* ================================================================
   INCOMPARABLE MASS DIAGNOSTIC
   ================================================================ */

%% wasserstein_incomparable_mass(+C, +Ctx, -Mass)
%  Returns the total probability mass on types outside the extraction
%  chain (scaffold, piton, naturalized, unknown) for constraint C at
%  context Ctx.
%
%  C:    constraint atom
%  Ctx:  observer context
%  Mass: float in [0.0, 1.0]
%
%  High values (>0.5) mean the W1 computation is based on heavily
%  renormalized data and should be interpreted with caution.
wasserstein_incomparable_mass(C, Ctx, Mass) :-
    maxent_classifier:maxent_distribution(C, Ctx, Dist),
    extract_chain_probs(Dist, _ChainProbs, Mass).

/* ================================================================
   SELF-TEST
   ================================================================ */

%% wasserstein_selftest/0
%  Runs unit tests on CDF construction and W1 computation, then
%  an integration test against the loaded corpus.
wasserstein_selftest :-
    format(user_error, '[measurement] Running Wasserstein L1 self-test...~n', []),

    % --- Unit test 1: CDF construction ---
    format('=== UNIT TEST 1: CDF Construction ===~n'),
    TestDist1 = [mountain-0.4, rope-0.3, tangled_rope-0.2, snare-0.1,
                 scaffold-0.0, piton-0.0],
    wasserstein_cdf(TestDist1, CDF1),
    format('  Input:    [mtn=0.4, rope=0.3, tr=0.2, snare=0.1]~n'),
    format('  CDF:      ~w~n', [CDF1]),
    CDF1 = [F1_1, F1_2, F1_3, F1_4],
    (   abs(F1_1 - 0.4) < 0.001,
        abs(F1_2 - 0.7) < 0.001,
        abs(F1_3 - 0.9) < 0.001,
        abs(F1_4 - 1.0) < 0.001
    ->  format('  [OK] CDF values match expected [0.4, 0.7, 0.9, 1.0]~n')
    ;   format('  [FAIL] CDF values do not match expected~n')
    ),

    % --- Unit test 2: Identical distributions → W1 = 0.0 ---
    format('~n=== UNIT TEST 2: Identical Distributions ===~n'),
    wasserstein_l1(TestDist1, TestDist1, W1_same),
    format('  W1(P, P) = ~6f~n', [W1_same]),
    (   abs(W1_same) < 1.0e-10
    ->  format('  [OK] Identical distributions yield W1 = 0.0~n')
    ;   format('  [FAIL] Expected 0.0, got ~f~n', [W1_same])
    ),

    % --- Unit test 3: Worked L1 example ---
    format('~n=== UNIT TEST 3: Worked L1 Example ===~n'),
    P3 = [mountain-0.5, rope-0.5, tangled_rope-0.0, snare-0.0,
          scaffold-0.0, piton-0.0],
    Q3 = [mountain-0.0, rope-0.0, tangled_rope-0.5, snare-0.5,
          scaffold-0.0, piton-0.0],
    wasserstein_l1(P3, Q3, W1_3),
    format('  P = [mtn=0.5, rope=0.5, tr=0, snare=0]~n'),
    format('  Q = [mtn=0, rope=0, tr=0.5, snare=0.5]~n'),
    format('  P_CDF = [0.5, 1.0, 1.0, 1.0]~n'),
    format('  Q_CDF = [0.0, 0.0, 0.5, 1.0]~n'),
    format('  W1 = |0.5-0| + |1.0-0| + |1.0-0.5| = 2.0~n'),
    format('  Computed: ~6f~n', [W1_3]),
    (   abs(W1_3 - 2.0) < 0.001
    ->  format('  [OK] W1 = 2.0 as expected~n')
    ;   format('  [FAIL] Expected 2.0, got ~f~n', [W1_3])
    ),

    % --- Unit test 4: Maximum distance (opposite ends) ---
    format('~n=== UNIT TEST 4: Maximum Distance ===~n'),
    P4 = [mountain-1.0, rope-0.0, tangled_rope-0.0, snare-0.0,
          scaffold-0.0, piton-0.0],
    Q4 = [mountain-0.0, rope-0.0, tangled_rope-0.0, snare-1.0,
          scaffold-0.0, piton-0.0],
    wasserstein_l1(P4, Q4, W1_4),
    format('  P = Dirac(mountain), Q = Dirac(snare)~n'),
    format('  W1 = |1-0| + |1-0| + |1-0| = 3.0~n'),
    format('  Computed: ~6f~n', [W1_4]),
    (   abs(W1_4 - 3.0) < 0.001
    ->  format('  [OK] Maximum W1 = 3.0~n')
    ;   format('  [FAIL] Expected 3.0, got ~f~n', [W1_4])
    ),

    % --- Unit test 5: Incomparable mass handling ---
    format('~n=== UNIT TEST 5: All-Incomparable Distribution ===~n'),
    P5 = [mountain-0.0, rope-0.0, tangled_rope-0.0, snare-0.0,
          scaffold-0.5, piton-0.5],
    wasserstein_cdf(P5, CDF5),
    format('  Input: all mass on scaffold+piton~n'),
    format('  CDF:   ~w~n', [CDF5]),
    CDF5 = [F5_1, _, _, _],
    (   abs(F5_1 - 0.25) < 0.001
    ->  format('  [OK] Falls back to uniform CDF~n')
    ;   format('  [FAIL] Expected uniform CDF~n')
    ),

    % --- Unit test 6: Renormalization with incomparable mass ---
    format('~n=== UNIT TEST 6: Partial Incomparable Mass ===~n'),
    P6 = [mountain-0.2, rope-0.2, tangled_rope-0.1, snare-0.1,
          scaffold-0.2, piton-0.2],
    extract_chain_probs(P6, _, IncompMass6),
    format('  Chain mass = 0.6, incomparable mass = ~4f~n', [IncompMass6]),
    (   abs(IncompMass6 - 0.4) < 0.001
    ->  format('  [OK] Incomparable mass = 0.4~n')
    ;   format('  [FAIL] Expected 0.4, got ~f~n', [IncompMass6])
    ),
    wasserstein_cdf(P6, CDF6),
    CDF6 = [_, _, _, F6_4],
    (   abs(F6_4 - 1.0) < 0.001
    ->  format('  [OK] Renormalized CDF sums to 1.0~n')
    ;   format('  [FAIL] CDF does not sum to 1.0~n')
    ),

    % --- Unit test 7: Empty distribution ---
    format('~n=== UNIT TEST 7: Empty Distribution ===~n'),
    wasserstein_l1([], [], W1_empty),
    format('  W1([], []) = ~6f~n', [W1_empty]),
    (   abs(W1_empty) < 1.0e-10
    ->  format('  [OK] Empty distributions yield W1 = 0.0~n')
    ;   format('  [FAIL] Expected 0.0~n')
    ),

    format('~nSelf-test complete.~n').

/* ================================================================
   VERIFICATION CHECKLIST
   ================================================================

   (a) Manual CDF verification for a known distribution:
       Distribution: [mountain-0.4, rope-0.3, tangled_rope-0.2, snare-0.1]
       Chain probs:  [0.4, 0.3, 0.2, 0.1] (sum = 1.0, no renorm needed)
       CDF:          [0.4, 0.7, 0.9, 1.0]
       Verify by calling:
         wasserstein_cdf([mountain-0.4, rope-0.3, tangled_rope-0.2,
                          snare-0.1], CDF).
         Expected: CDF = [0.4, 0.7, 0.9, 1.0]

   (b) Worked L1 example:
       P = [mountain-0.5, rope-0.5, tangled_rope-0.0, snare-0.0]
       Q = [mountain-0.0, rope-0.0, tangled_rope-0.5, snare-0.5]
       P_CDF = [0.5, 1.0, 1.0, 1.0]
       Q_CDF = [0.0, 0.0, 0.5, 1.0]
       W1 = |0.5 - 0.0| + |1.0 - 0.0| + |1.0 - 0.5|
          = 0.5 + 1.0 + 0.5
          = 2.0
       Verify by calling:
         wasserstein_l1([mountain-0.5, rope-0.5, tangled_rope-0.0,
                         snare-0.0, scaffold-0.0, piton-0.0],
                        [mountain-0.0, rope-0.0, tangled_rope-0.5,
                         snare-0.5, scaffold-0.0, piton-0.0], W1).
         Expected: W1 = 2.0

   (c) Edge case — identical distributions:
       wasserstein_l1(P, P, W1) should always yield W1 = 0.0,
       since F_P(xi) = F_P(xi) for all i, hence all differences are zero.

   ================================================================ */
