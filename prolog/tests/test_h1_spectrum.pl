% ============================================================================
% TEST H1 SPECTRUM — engine-side witness for the general-n gap law (OQ-195)
% ============================================================================
% Witnesses that grothendieck_cohomology:obstruction_from_vector/3 (the pure
% OQ-51-filtered pair counter) realizes EXACTLY the spectra proven in
% docs/h1_gap_spectrum_general_n.md, at the cardinalities the stakeholder
% frame makes live (n up to 12), not just the pre-OQ-195 observer range.
%
% Three layers (plan review 2026-07-02, items 2/5/6c):
%   1. EXHAUSTIVE n=2..4: every vector over 7 real tokens + unknown.
%   2. CONSTRUCTIVE n=5..12: one vector per partition with <= 7 blocks
%      (complete for the T-bounded spectrum H(n,T=7): every such partition is
%      realized, and 7 real tokens cannot realize more blocks), asserting the
%      engine value equals the Lemma-1 formula AND the realized set per n
%      equals the Python-enumerator-verified set (cross-implementation).
%   3. OQ-51 filter at new cardinalities: unknown-padded vectors, n=8..12.
% Plus a negative control: a perturbed expectation is SHOWN to fail the
% comparator (an exhaustive enumeration with a silent generation bug is
% byte-identical to one that looked).
%
% Expected sets: proven in docs/h1_gap_spectrum_general_n.md; the n=8..12
% T-bounded sets are the enumerator's verified output
% (audits/2026-07-02_oq195_general_n_gap/enumeration_results.json).
% Run: cd prolog && swipl -g "[stack], [tests/test_h1_spectrum], run_tests, halt" -t "halt(1)"
% ============================================================================
:- module(test_h1_spectrum, []).

:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- begin_tests(h1_spectrum).

real_tokens([mountain, rope, tangled_rope, snare, scaffold, piton, naturalized]).

% Lemma 1: H1(lambda) = C(n,2) - sum_i C(lambda_i,2)
lemma1(N, Lam, H1) :-
    CN2 is N*(N-1)//2,
    foldl([P,A,B]>>(B is A + P*(P-1)//2), Lam, 0, S),
    H1 is CN2 - S.

% Proven spectra (docs/h1_gap_spectrum_general_n.md). n<=7: H(n) (T-bound
% inactive); n=8..12: H(n,T=7) (the engine-realizable spectrum).
expected_spectrum(2,  [0,1]).
expected_spectrum(3,  [0,2,3]).
expected_spectrum(4,  [0,3,4,5,6]).
expected_spectrum(5,  [0,4,6,7,8,9,10]).
expected_spectrum(6,  [0,5,8,9,11,12,13,14,15]).
expected_spectrum(7,  [0,6,10,11,12,14,15,16,17,18,19,20,21]).
expected_spectrum(8,  [0,7,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27]).
expected_spectrum(9,  [0,8,14,15,18,20,21,23,24,25,26,27,28,29,30,31,32,33,34]).
expected_spectrum(10, [0,9,16,17,21,23,24,25,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42]).
expected_spectrum(11, [0,10,18,19,24,26,27,28,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51]).
expected_spectrum(12, [0,11,20,21,27,29,30,32,35,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61]).

% ---- vector construction -------------------------------------------------
% all_vectors(+Len, -Vector): every vector over the 8-token universe.
all_vectors(0, []).
all_vectors(L, [T|Rest]) :-
    L > 0,
    real_tokens(RT),
    L1 is L - 1,
    member(T, [unknown|RT]),
    all_vectors(L1, Rest).

% partitions_le(+N, +MaxPart, +MaxParts, -Lam): descending partitions of N
% with parts <= MaxPart and at most MaxParts parts.
partitions_le(0, _, _, []).
partitions_le(N, MaxPart, MaxParts, [P|T]) :-
    N > 0, MaxParts > 0,
    UB is min(N, MaxPart),
    between(1, UB, X), P is UB + 1 - X,   % descending choice
    N1 is N - P, MP1 is MaxParts - 1,
    partitions_le(N1, P, MP1, T).

% realize(+Lam, -Vector): block i of size Lam_i filled with the i-th real token.
realize(Lam, Vector) :-
    real_tokens(RT),
    realize_(Lam, RT, Vector).
realize_([], _, []).
realize_([Sz|Rest], [Tok|Toks], Vector) :-
    length(Block, Sz), maplist(=(Tok), Block),
    realize_(Rest, Toks, Tail),
    append(Block, Tail, Vector).

engine_h1(Vector, H1) :-
    grothendieck_cohomology:obstruction_from_vector(Vector, _H0, H1).

% ---- layer 1: exhaustive n = 2..4 ------------------------------------------
% For each length, bucket every vector by real-seat count; the realized H1 set
% per real-count must equal the proven spectrum; real-count < 2 must be null.
exhaustive_bucket(Len, RealN, H1Set) :-
    findall(RC-H1,
        ( all_vectors(Len, V),
          include([T]>>(T \== unknown), V, Real),
          length(Real, RC),
          engine_h1(V, H1)
        ),
        Pairs),
    findall(H, member(RealN-H, Pairs), Hs),
    sort(Hs, H1Set).

test(exhaustive_len4_real4) :-
    exhaustive_bucket(4, 4, S), expected_spectrum(4, E), S == E.
test(exhaustive_len4_real3) :-
    exhaustive_bucket(4, 3, S), expected_spectrum(3, E), S == E.
test(exhaustive_len4_real2) :-
    exhaustive_bucket(4, 2, S), expected_spectrum(2, E), S == E.
test(exhaustive_len4_below2_null) :-
    exhaustive_bucket(4, 1, S1), S1 == [null],
    exhaustive_bucket(4, 0, S0), S0 == [null].
test(exhaustive_len3) :-
    exhaustive_bucket(3, 3, S3), expected_spectrum(3, E3), S3 == E3,
    exhaustive_bucket(3, 2, S2), expected_spectrum(2, E2), S2 == E2,
    exhaustive_bucket(3, 1, S1), S1 == [null].
test(exhaustive_len2) :-
    exhaustive_bucket(2, 2, S2), expected_spectrum(2, E2), S2 == E2,
    exhaustive_bucket(2, 1, S1), S1 == [null].

% ---- layer 2: constructive n = 5..12 ---------------------------------------
realized_spectrum(N, Set) :-
    findall(H1,
        ( partitions_le(N, N, 7, Lam),
          realize(Lam, V),
          engine_h1(V, H1),
          lemma1(N, Lam, Expect),
          ( H1 =:= Expect -> true
          ; throw(lemma1_mismatch(N, Lam, got(H1), want(Expect))) )
        ),
        Hs),
    sort(Hs, Set).

test(constructive_5_12_match_proven_spectra) :-
    forall(between(5, 12, N),
        ( realized_spectrum(N, S),
          expected_spectrum(N, E),
          ( S == E -> true ; throw(spectrum_mismatch(N, got(S), want(E))) )
        )).

test(bottom_gap_law_live_range) :-
    % Theorem A at every live cardinality: min nonzero realized H1 = n-1.
    forall(between(3, 12, N),
        ( realized_spectrum(N, [0, MinNZ|_]),
          MinNZ =:= N - 1
        )).

% ---- layer 3: OQ-51 filter at new cardinalities ----------------------------
test(unknown_padding_filters_to_real_partition) :-
    % length-12 vector, 7 real seats as (4,3) + 5 unknowns -> H1 of n=7,(4,3)=12
    realize([4,3], RealPart),
    append(RealPart, [unknown,unknown,unknown,unknown,unknown], V),
    engine_h1(V, H1), H1 =:= 12.
test(unknown_padding_large_all_null) :-
    length(V, 12), maplist(=(unknown), V),
    engine_h1(V, H), H == null.
test(unknown_padding_one_real_null) :-
    length(U, 11), maplist(=(unknown), U),
    engine_h1([snare|U], H), H == null.

% ---- negative controls ------------------------------------------------------
test(negative_control_perturbed_spectrum_fails) :-
    % A wrong expectation MUST be distinguishable: H(5) with 4 -> 3.
    realized_spectrum(5, S),
    Perturbed = [0,3,6,7,8,9,10],
    \+ S == Perturbed.
test(negative_control_bad_lemma_value_fails) :-
    realize([3,2], V),
    engine_h1(V, H1),
    H1 =:= 6,          % the true Lemma-1 value for n=5, (3,2)
    \+ H1 =:= 7.       % the planted-wrong expectation is rejected

:- end_tests(h1_spectrum).
