% ============================================================================
% READING_DIFF — the cyclopean disparity operator (OQ-59 disposition)
% ============================================================================
% Diffs two readings cell-by-cell over the closed (P,T,E,S) tuple, keyed on a
% DECLARED alignment relation. Partitions the shared structure into:
%   AgreementCells  — aligned vantages where both readings give the same type
%                     (situation-fixed = objective, asymptote territory)
%   DisparityCells  — aligned vantages where they differ
%                     (standpoint-set = the open parameters = the depth)
%   BlindSpots      — vantages one reading samples that the other does not
%                     (coverage gap)
%
% READS AUTHORED CELLS ONLY: constraint_indexing:constraint_classification/3.
% Never recomputed chi, never product_site_orbits.json (that export gives every
% reading full coverage, which makes blind-spots structurally impossible — the
% exact partial-coverage signal this operator needs). Asserts nothing.
%
% The operator is itself SEATED: the alignment key is the seat (the definition
% of "the same vantage"). It is an explicit argument — no silently-baked seat.
% kernel-AGNOSTIC: takes two arbitrary readings; does not require equal
% cs_kernel_id. Cross-kernel diffs (e.g. westphalia_ vs westphalian_) are
% first-class — agreement = invariant shared across the near-kernels.
%
% Counting semantics (DECLARED CHOICE): headline counts are over aligned
% VANTAGE-GROUPS (equivalence classes of the alignment relation), with the
% pair-level fan-out reported separately as a multiplicity. Pair-counting would
% inflate "disparity" with an artifact of the alignment relation rather than
% depth.
%
% Usage (from prolog/, testsets loaded):
%   ?- reading_diff:report_pair(R_a, R_b, all_keys).
%   ?- reading_diff:report_pair(R_a, R_b, weighted([1,1,1,1], 1)).
%   ?- reading_diff:reading_diff(R_a, R_b, exact, Agree, Disp, Blind).
%
% Standalone:
%   swipl -g "[stack], corpus_loader:load_all_testsets, [reading_diff], \
%     reading_diff:report_pair(R_a, R_b, all_keys), halt" -t "halt(1)"
% ============================================================================

:- module(reading_diff, [
    reading_diff/6,          % reading_diff(+RA,+RB,+Key, -Agree,-Disparity,-Blind) [vantage-level]
    reading_cells/2,         % reading_cells(+Reading, -Cells)
    cells_aligned/3,         % cells_aligned(+Key, +CtxA, +CtxB)
    aligned_pairs/5,         % aligned_pairs(+Key,+RA,+RB, -AgreePairs,-DispPairs) [pair-level]
    has_vantage_partition/1, % has_vantage_partition(?Key)
    per_key_regime/4,        % per_key_regime(+RA,+RB,+Key, -Regime)
    stability_verdict/3,     % stability_verdict(+RA,+RB, -Verdict)
    stability_verdict/4,     % stability_verdict(+RA,+RB,+DeclaredKeys, -Verdict)
    report_pair/3            % report_pair(+RA,+RB, +KeySpec)  (KeySpec: all_keys | Key)
]).

:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(yall)).

% ----------------------------------------------------------------------------
% Cells.  A cell is cell(Type, Context); Context is the closed 4-tuple.
% ----------------------------------------------------------------------------

%% reading_cells(+Reading, -Cells) is det.
%  The authored (P,T,E,S)->type map for one reading, deduped & sorted.
reading_cells(Reading, Cells) :-
    findall(cell(Type, Ctx),
            constraint_indexing:constraint_classification(Reading, Type, Ctx),
            Cells0),
    sort(Cells0, Cells).

ctx_args(context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S)),
         P, T, E, S).

% ----------------------------------------------------------------------------
% Alignment relations (the declared seats).
% ----------------------------------------------------------------------------

%% cells_aligned(+Key, +CtxA, +CtxB) is semidet.
cells_aligned(exact, A, B) :- A == B.
cells_aligned(fuzzy_agent_power, A, B) :-
    ctx_args(A, P, _, _, _),
    ctx_args(B, P, _, _, _).
cells_aligned(weighted(Ws, Thr), A, B) :-
    weighted_distance(Ws, A, B, D),
    D =< Thr.

weighted_distance([WP, WT, WE, WS], A, B, D) :-
    ctx_args(A, PA, TA, EA, SA),
    ctx_args(B, PB, TB, EB, SB),
    dim(PA, PB, WP, DP),
    dim(TA, TB, WT, DT),
    dim(EA, EB, WE, DE),
    dim(SA, SB, WS, DS),
    D is DP + DT + DE + DS.

dim(X, X, _, 0) :- !.
dim(_, _, W, W).

% Vantage partition: defined only for keys that are EQUIVALENCE relations.
% exact = identity; fuzzy_agent_power = same agent_power. weighted is a
% tolerance relation (reflexive, symmetric, NOT transitive) -> no clean
% partition into groups, so it is reported pair-level only.
has_vantage_partition(exact).
has_vantage_partition(fuzzy_agent_power).

vantage_key(exact, Ctx, Ctx).
vantage_key(fuzzy_agent_power, Ctx, P) :- ctx_args(Ctx, P, _, _, _).

% ----------------------------------------------------------------------------
% Vantage-level partition (the headline).
% ----------------------------------------------------------------------------

%% reading_diff(+RA, +RB, +Key, -Agreement, -Disparity, -Blind) is det.
%  Agreement = [agree(VKey, Types)]
%  Disparity = [disparity(VKey, TypesA, TypesB)]
%  Blind     = [blind(VKey, Side, Types)], Side in {a,b}
%  Types are sorted type-SETS (a reading may span several types at one vantage).
reading_diff(RA, RB, Key, Agreement, Disparity, Blind) :-
    ( has_vantage_partition(Key) -> true
    ; throw(error(domain_error(vantage_partition_key, Key),
                  context(reading_diff/6,
                          'weighted is a tolerance relation: use aligned_pairs/5 or report_pair/3')))
    ),
    reading_cells(RA, CA),
    reading_cells(RB, CB),
    vantage_typemap(Key, CA, MA),
    vantage_typemap(Key, CB, MB),
    pairs_keys(MA, KA),
    pairs_keys(MB, KB),
    append(KA, KB, K0),
    sort(K0, AllKeys),
    findall(R, ( member(V, AllKeys), classify_vantage(V, MA, MB, R) ), Results),
    include([X]>>(X = agree(_, _)), Results, Agreement),
    include([X]>>(X = disparity(_, _, _)), Results, Disparity),
    include([X]>>(X = blind(_, _, _)), Results, Blind).

%% vantage_typemap(+Key, +Cells, -Map) : Map = ordered list of VKey-SortedTypeSet
vantage_typemap(Key, Cells, Map) :-
    findall(VKey-Type,
            ( member(cell(Type, Ctx), Cells), vantage_key(Key, Ctx, VKey) ),
            Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist([K-Ts, K-Set]>>sort(Ts, Set), Grouped, Map).

classify_vantage(VKey, MA, MB, Result) :-
    ( memberchk(VKey-Ta, MA) -> HasA = true ; HasA = false ),
    ( memberchk(VKey-Tb, MB) -> HasB = true ; HasB = false ),
    ( HasA == true, HasB == true ->
        ( Ta == Tb -> Result = agree(VKey, Ta)
        ; Result = disparity(VKey, Ta, Tb)
        )
    ; HasA == true -> Result = blind(VKey, a, Ta)
    ; Result = blind(VKey, b, Tb)
    ).

% ----------------------------------------------------------------------------
% Pair-level alignment (fan-out detail + the only level defined for weighted).
% ----------------------------------------------------------------------------

%% aligned_pairs(+Key, +RA, +RB, -AgreePairs, -DispPairs) is det.
%  AgreePairs/DispPairs = [pair(cell(Ta,CtxA), cell(Tb,CtxB))] over the cross
%  product of cells that align under Key. For weighted this is the whole result;
%  for partition keys it is the fan-out multiplicity behind the vantage counts.
aligned_pairs(Key, RA, RB, AgreePairs, DispPairs) :-
    reading_cells(RA, CA),
    reading_cells(RB, CB),
    findall(pair(cell(Ta, CtxA), cell(Tb, CtxB)),
            ( member(cell(Ta, CtxA), CA),
              member(cell(Tb, CtxB), CB),
              cells_aligned(Key, CtxA, CtxB)
            ),
            AllPairs),
    include(pair_agrees, AllPairs, AgreePairs),
    exclude(pair_agrees, AllPairs, DispPairs).

pair_agrees(pair(cell(T, _), cell(T, _))).

% Cells of one reading that align to NOTHING in the other (pair-level blind).
unaligned_cells(Key, From, Other, Blind) :-
    reading_cells(From, CF),
    reading_cells(Other, CO),
    findall(cell(T, Ctx),
            ( member(cell(T, Ctx), CF),
              \+ ( member(cell(_, Ctx2), CO), cells_aligned(Key, Ctx, Ctx2) )
            ),
            Blind).

% ----------------------------------------------------------------------------
% Regime + stability verdict.
% ----------------------------------------------------------------------------

%% per_key_regime(+RA, +RB, +Key, -Regime) : binocular | undersampled (TAGGED with key by caller)
per_key_regime(RA, RB, Key, Regime) :-
    reading_diff(RA, RB, Key, _, Disparity, _),
    ( Disparity == [] -> Regime = undersampled ; Regime = binocular ).

%% stability_verdict(+RA, +RB, -Verdict) : default declared key set = the [exact ⊆ fuzzy] chain.
stability_verdict(RA, RB, Verdict) :-
    stability_verdict(RA, RB, [exact, fuzzy_agent_power], Verdict).

%% stability_verdict(+RA, +RB, +DeclaredKeys, -Verdict)
%  ORDER-INDEPENDENT (set predicate over the keys, not a sequence predicate) —
%  well-defined for ANY declared key set, chain or not:
%    robustly_binocular   <-> >=1 disparity under EVERY declared key
%    robustly_undersampled <-> 0 disparity under EVERY declared key
%    key_fragile          <-> regime flips across the set
stability_verdict(RA, RB, DeclaredKeys, Verdict) :-
    findall(N,
            ( member(K, DeclaredKeys),
              reading_diff(RA, RB, K, _, D, _),
              length(D, N)
            ),
            Counts),
    ( forall(member(C, Counts), C >= 1) -> Verdict = robustly_binocular
    ; forall(member(C, Counts), C =:= 0) -> Verdict = robustly_undersampled
    ; Verdict = key_fragile
    ).

% ----------------------------------------------------------------------------
% Reporting.
% ----------------------------------------------------------------------------

%% report_pair(+RA, +RB, +KeySpec)
%  KeySpec = all_keys  -> runs the [exact, fuzzy_agent_power] chain + stability verdict
%                          + the (chain-only) blind-monotonicity observation.
%          = weighted(Ws,Thr) -> pair-level report (no vantage partition).
%          = Key       -> single declared key.
report_pair(RA, RB, all_keys) :- !,
    report_header(RA, RB),
    report_key(RA, RB, exact),
    report_key(RA, RB, fuzzy_agent_power),
    report_stability(RA, RB, [exact, fuzzy_agent_power]),
    report_blind_chain(RA, RB).
report_pair(RA, RB, weighted(Ws, Thr)) :- !,
    report_header(RA, RB),
    report_weighted(RA, RB, weighted(Ws, Thr)).
report_pair(RA, RB, Key) :-
    report_header(RA, RB),
    report_key(RA, RB, Key).

report_header(RA, RB) :-
    format("~n================================================================~n"),
    format("READING DIFF~n  A = ~w~n  B = ~w~n", [RA, RB]),
    ( narrative_ontology:cs_kernel_id(RA, KA) -> true ; KA = '(none)' ),
    ( narrative_ontology:cs_kernel_id(RB, KB) -> true ; KB = '(none)' ),
    ( KA == KB
    -> format("  kernel: ~w (within-kernel)~n", [KA])
    ;  format("  kernels: ~w vs ~w (CROSS-KERNEL invariant probe)~n", [KA, KB])
    ),
    format("================================================================~n").

report_key(RA, RB, Key) :-
    reading_diff(RA, RB, Key, Ag, Disp, Blind),
    length(Ag, NA), length(Disp, ND), length(Blind, NB),
    aligned_pairs(Key, RA, RB, _, DispPairs),
    length(DispPairs, NFanOut),
    ( ND >= 1 -> Regime = binocular ; Regime = undersampled ),
    format("~n[key=~w]  VANTAGE-GROUP partition (headline):~n", [Key]),
    format("    agree=~w  disparity=~w  blind=~w   regime(@~w)=~w~n",
           [NA, ND, NB, Key, Regime]),
    format("    fan-out (pair-level) disparity-pairs = ~w~n", [NFanOut]),
    ( Disp == [] -> true
    ; format("    disparity vantages:~n"),
      forall(member(disparity(V, Ta, Tb), Disp),
             format("      ~w :  A=~w  vs  B=~w~n", [V, Ta, Tb]))
    ),
    ( Blind == [] -> true
    ; format("    blind vantages:~n"),
      forall(member(blind(V, Side, Ts), Blind),
             ( side_reading(Side, RA, RB, Who),
               format("      ~w :  ~w-only ~w~n", [V, Who, Ts]) ))
    ).

side_reading(a, RA, _, RA).
side_reading(b, _, RB, RB).

report_stability(RA, RB, Keys) :-
    stability_verdict(RA, RB, Keys, Verdict),
    format("~n>>> STABILITY VERDICT over ~w : ~w~n", [Keys, Verdict]).

% Chain-only observation: blind non-increasing along exact ⊆ fuzzy_agent_power.
% NOT part of the verdict; valid only because this pair of keys forms a chain.
report_blind_chain(RA, RB) :-
    reading_diff(RA, RB, exact, _, _, Be),
    reading_diff(RA, RB, fuzzy_agent_power, _, _, Bf),
    length(Be, NBe),
    length(Bf, NBf),
    ( NBf =< NBe -> Trend = "non-increasing" ; Trend = "INCREASING" ),
    format(">>> blind-count along chain [exact ⊆ fuzzy_agent_power]: ~w -> ~w (~w)~n",
           [NBe, NBf, Trend]),
    format("    (observation only — valid because these keys form a chain; not a verdict)~n").

report_weighted(RA, RB, weighted(Ws, Thr)) :-
    Key = weighted(Ws, Thr),
    aligned_pairs(Key, RA, RB, AgreePairs, DispPairs),
    length(AgreePairs, NAg),
    length(DispPairs, ND),
    unaligned_cells(Key, RA, RB, BlindA),
    unaligned_cells(Key, RB, RA, BlindB),
    length(BlindA, NBA),
    length(BlindB, NBB),
    format("~n[key=~w]  PAIR-LEVEL only (tolerance relation, no vantage partition):~n", [Key]),
    format("    agree-pairs=~w  disparity-pairs=~w  blind-cells: A-only=~w B-only=~w~n",
           [NAg, ND, NBA, NBB]),
    ( DispPairs == [] -> true
    ; format("    disparity pairs:~n"),
      forall(member(pair(cell(Ta, CtxA), cell(Tb, CtxB)), DispPairs),
             format("      A=~w@~w  vs  B=~w@~w~n", [Ta, CtxA, Tb, CtxB]))
    ).
