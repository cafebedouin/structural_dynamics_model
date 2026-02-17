% ============================================================================
% ERDOS-SELFRIDGE COVERING ANALYSIS
% ============================================================================
% Analyzes the 12-point Index Grid (4 powers x 3 scopes) for:
%   Phase 1: Grid cell redundancy — which cells always agree?
%   Phase 2: Coverage gaps — are there classification transitions between
%            grid points that the 12-cell grid misses?
%
% Based on the covering system analogy from number theory: does a finite
% set of "congruence classes" (grid cells) cover the full constraint-space?
%
% Usage:
%   cd prolog && swipl -l stack.pl -l covering_analysis.pl \
%     -g run_covering_analysis -t halt > ../outputs/covering_analysis.md
%
% Individual phases:
%   swipl -l stack.pl -l covering_analysis.pl -g run_phase1 -t halt
%   swipl -l stack.pl -l covering_analysis.pl -g run_phase2 -t halt
% ============================================================================

:- module(covering_analysis, [
    run_covering_analysis/0,
    run_phase1/0,
    run_phase2/0,
    grid_signature/2,
    pairwise_agreement/3,
    minimum_discriminating_set/1,
    classify_at_interpolated/4,
    missed_transitions/1
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(corpus_loader).
:- use_module(library(lists)).

:- dynamic cached_grid_sig/2.

%% all_corpus_constraints(-Constraints)
%  Discovers all constraint atoms with extractiveness data.
all_corpus_constraints(Constraints) :-
    config:param(extractiveness_metric_name, ExtName),
    findall(C,
        (   narrative_ontology:constraint_metric(C, ExtName, _),
            atom(C)
        ),
        CsRaw),
    sort(CsRaw, Constraints).

%% grid_cells(-Cells)
%  Returns the 12 grid cells as cell(Power, Scope) terms.
grid_cells(Cells) :-
    structural_signatures:coupling_test_powers(Powers),
    structural_signatures:coupling_test_scopes(Scopes),
    findall(cell(P, S), (member(P, Powers), member(S, Scopes)), Cells).

%% cell_label(+Cell, -Label)
%  Short text label for a grid cell.
cell_label(cell(P, S), Label) :-
    format(atom(Label), '~w/~w', [P, S]).

/* ================================================================
   PHASE 1: REDUNDANT GRID CELL DETECTION
   ================================================================ */

%% grid_signature(+Constraint, -Signature)
%  Classifies constraint at all 12 grid cells.
%  Uses structural_signatures:classify_at_context/3 (same pipeline
%  as the Boltzmann compliance engine).
%  Signature is a sorted list of classified(Power, Scope, Type).
grid_signature(C, Signature) :-
    structural_signatures:coupling_test_powers(Powers),
    structural_signatures:coupling_test_scopes(Scopes),
    findall(
        classified(P, S, Type),
        (   member(P, Powers),
            member(S, Scopes),
            structural_signatures:coupling_test_context(P, S, Ctx),
            (   structural_signatures:classify_at_context(C, Ctx, Type0)
            ->  Type = Type0
            ;   Type = unknown
            )
        ),
        Signature
    ).

%% compute_all_signatures(+Constraints)
%  Computes and caches grid signatures for all constraints.
compute_all_signatures(Constraints) :-
    retractall(cached_grid_sig(_, _)),
    structural_signatures:clear_classification_cache,
    length(Constraints, N),
    format(user_error, '[phase1] Computing grid signatures for ~w constraints...~n', [N]),
    compute_sigs_loop(Constraints, 0, N).

compute_sigs_loop([], _, _).
compute_sigs_loop([C|Cs], Done, Total) :-
    (   grid_signature(C, Sig)
    ->  assertz(cached_grid_sig(C, Sig))
    ;   true
    ),
    Done1 is Done + 1,
    (   0 =:= Done1 mod 100
    ->  format(user_error, '[phase1] ~w/~w signatures computed~n', [Done1, Total])
    ;   true
    ),
    compute_sigs_loop(Cs, Done1, Total).

%% type_at_cell(+Signature, +Power, +Scope, -Type)
%  Extracts the type from a signature at a given cell.
type_at_cell(Signature, P, S, Type) :-
    member(classified(P, S, Type), Signature), !.
type_at_cell(_, _, _, unknown).

%% pairwise_agreement(+Cell1, +Cell2, -Rate)
%  Fraction of corpus where both cells produce the same type.
pairwise_agreement(cell(P1, S1), cell(P2, S2), Rate) :-
    findall(C, cached_grid_sig(C, _), Constraints),
    length(Constraints, N),
    (   N > 0
    ->  findall(1,
            (   member(C, Constraints),
                cached_grid_sig(C, Sig),
                type_at_cell(Sig, P1, S1, T1),
                type_at_cell(Sig, P2, S2, T2),
                T1 == T2
            ),
            Agrees),
        length(Agrees, NAgree),
        Rate is NAgree / N
    ;   Rate = 1.0
    ).

%% all_cell_pairs(-Pairs)
%  Returns all C(12,2) = 66 unordered cell pairs.
all_cell_pairs(Pairs) :-
    grid_cells(Cells),
    findall(C1-C2,
        (   member(C1, Cells),
            member(C2, Cells),
            C1 @< C2
        ),
        Pairs).

%% agreement_matrix(-Matrix)
%  Full pairwise agreement matrix.
%  Matrix is a list of pair(Cell1, Cell2, Rate) terms.
agreement_matrix(Matrix) :-
    all_cell_pairs(Pairs),
    length(Pairs, NP),
    format(user_error, '[phase1] Computing ~w pairwise agreement rates...~n', [NP]),
    findall(
        pair(C1, C2, Rate),
        (   member(C1-C2, Pairs),
            pairwise_agreement(C1, C2, Rate)
        ),
        Matrix
    ).

%% disagreeing_constraints(+Cell1, +Cell2, -Constraints)
%  Returns constraints where Cell1 and Cell2 disagree.
disagreeing_constraints(cell(P1, S1), cell(P2, S2), DisagreeList) :-
    findall(
        disagree(C, T1, T2),
        (   cached_grid_sig(C, Sig),
            type_at_cell(Sig, P1, S1, T1),
            type_at_cell(Sig, P2, S2, T2),
            T1 \== T2
        ),
        DisagreeList
    ).

%% minimum_discriminating_set(-CellSet)
%  Greedy set cover: find the smallest subset of grid cells that
%  preserves all classification distinctions between constraints.
minimum_discriminating_set(CellSet) :-
    format(user_error, '[phase1] Computing minimum discriminating subset...~n', []),
    full_grid_distinct_pairs(TargetPairs),
    length(TargetPairs, NTarget),
    format(user_error, '[phase1] ~w distinct constraint pairs to cover~n', [NTarget]),
    grid_cells(AllCells),
    (   NTarget =:= 0
    ->  CellSet = []
    ;   greedy_cover(AllCells, TargetPairs, [], CellSet)
    ).

%% full_grid_distinct_pairs(-Pairs)
%  All constraint pairs with different 12-cell signatures.
full_grid_distinct_pairs(Pairs) :-
    findall(C, cached_grid_sig(C, _), CsRaw),
    sort(CsRaw, Cs),
    findall(C1-C2,
        (   member(C1, Cs), member(C2, Cs),
            C1 @< C2,
            cached_grid_sig(C1, Sig1),
            cached_grid_sig(C2, Sig2),
            Sig1 \= Sig2
        ),
        Pairs).

%% greedy_cover(+Remaining, +Uncovered, +Chosen, -Result)
%  Greedy algorithm: pick the cell covering the most uncovered pairs.
greedy_cover(_, [], Chosen, Result) :-
    reverse(Chosen, Result), !.
greedy_cover([], _, Chosen, Result) :-
    reverse(Chosen, Result), !.
greedy_cover(Remaining, Uncovered, Chosen, Result) :-
    score_all_cells(Remaining, Uncovered, Scores),
    sort(2, @>=, Scores, Sorted),
    Sorted = [score(BestCell, BestCount)|_],
    (   BestCount =:= 0
    ->  reverse(Chosen, Result)
    ;   BestCell = cell(BP, BS),
        exclude(covered_by_cell(BP, BS), Uncovered, StillUncovered),
        delete(Remaining, BestCell, RemainingNext),
        length(StillUncovered, NLeft),
        format(user_error, '[phase1]   Added ~w (~w pairs covered, ~w remaining)~n',
               [BestCell, BestCount, NLeft]),
        greedy_cover(RemainingNext, StillUncovered, [BestCell|Chosen], Result)
    ).

score_all_cells(Remaining, Uncovered, Scores) :-
    findall(
        score(Cell, Count),
        (   member(Cell, Remaining),
            Cell = cell(P, S),
            aggregate_all(count,
                (   member(C1-C2, Uncovered),
                    cached_grid_sig(C1, Sig1),
                    cached_grid_sig(C2, Sig2),
                    type_at_cell(Sig1, P, S, T1),
                    type_at_cell(Sig2, P, S, T2),
                    T1 \== T2
                ),
                Count)
        ),
        Scores
    ).

covered_by_cell(P, S, C1-C2) :-
    cached_grid_sig(C1, Sig1),
    cached_grid_sig(C2, Sig2),
    type_at_cell(Sig1, P, S, T1),
    type_at_cell(Sig2, P, S, T2),
    T1 \== T2.

/* ================================================================
   PHASE 1: REPORT GENERATION
   ================================================================ */

run_phase1 :-
    load_all_testsets,
    all_corpus_constraints(Cs),
    compute_all_signatures(Cs),
    length(Cs, NC),
    format('## Phase 1: Grid Cell Redundancy Detection~n~n'),
    report_phase1_summary(NC),
    report_agreement_matrix,
    report_tiers,
    report_minimum_subset,
    format('~n').

report_phase1_summary(NC) :-
    format('### Summary~n~n'),
    format('- **Corpus size**: ~w constraints~n', [NC]),
    format('- **Grid dimensions**: 4 powers x 3 scopes = 12 cells~n'),
    format('- **Cell pairs analyzed**: 66~n~n').

report_agreement_matrix :-
    format('### Pairwise Agreement Matrix~n~n'),
    grid_cells(Cells),
    % Header row
    format('| Cell |'),
    forall(member(C, Cells), (
        cell_short(C, Short),
        format(' ~w |', [Short])
    )),
    format('~n'),
    % Separator
    format('|------|'),
    forall(member(_, Cells), format('------|')),
    format('~n'),
    % Data rows
    forall(member(Row, Cells), (
        cell_short(Row, RowShort),
        format('| ~w |', [RowShort]),
        forall(member(Col, Cells), (
            (   Row == Col
            ->  format(' 1.000 |')
            ;   (   Row @< Col
                ->  pairwise_agreement(Row, Col, Rate)
                ;   pairwise_agreement(Col, Row, Rate)
                ),
                format(' ~3f |', [Rate])
            )
        )),
        format('~n')
    )),
    format('~n').

cell_short(cell(powerless, local), 'pw/loc') :- !.
cell_short(cell(powerless, national), 'pw/nat') :- !.
cell_short(cell(powerless, global), 'pw/glo') :- !.
cell_short(cell(moderate, local), 'mo/loc') :- !.
cell_short(cell(moderate, national), 'mo/nat') :- !.
cell_short(cell(moderate, global), 'mo/glo') :- !.
cell_short(cell(institutional, local), 'in/loc') :- !.
cell_short(cell(institutional, national), 'in/nat') :- !.
cell_short(cell(institutional, global), 'in/glo') :- !.
cell_short(cell(analytical, local), 'an/loc') :- !.
cell_short(cell(analytical, national), 'an/nat') :- !.
cell_short(cell(analytical, global), 'an/glo') :- !.
cell_short(cell(P, S), Label) :- format(atom(Label), '~w/~w', [P, S]).

report_tiers :-
    agreement_matrix(Matrix),
    % Tier A: 100% agreement
    include(is_tier_a, Matrix, TierA),
    length(TierA, NA),
    format('### Tier A: Fully Redundant Pairs (100% Agreement)~n~n'),
    format('**~w pairs found**~n~n', [NA]),
    (   TierA \= []
    ->  format('| Cell 1 | Cell 2 | Agreement |~n'),
        format('|--------|--------|-----------|~n'),
        forall(member(pair(C1, C2, Rate), TierA), (
            cell_short(C1, S1), cell_short(C2, S2),
            format('| ~w | ~w | ~3f |~n', [S1, S2, Rate])
        ))
    ;   format('No fully redundant cell pairs found.~n')
    ),
    format('~n'),
    % Tier B: >= 95% but < 100%
    include(is_tier_b, Matrix, TierB),
    length(TierB, NB),
    format('### Tier B: Nearly Redundant Pairs (>=95% Agreement)~n~n'),
    format('**~w pairs found**~n~n', [NB]),
    (   TierB \= []
    ->  format('| Cell 1 | Cell 2 | Agreement | Disagreeing Constraints |~n'),
        format('|--------|--------|-----------|------------------------|~n'),
        forall(member(pair(C1, C2, Rate), TierB), (
            cell_short(C1, S1), cell_short(C2, S2),
            disagreeing_constraints(C1, C2, Disagrees),
            findall(CA, member(disagree(CA, _, _), Disagrees), CAtoms),
            atomic_list_concat(CAtoms, ', ', DisStr),
            format('| ~w | ~w | ~3f | ~w |~n', [S1, S2, Rate, DisStr])
        ))
    ;   format('No nearly-redundant cell pairs found.~n')
    ),
    format('~n').

is_tier_a(pair(_, _, Rate)) :- Rate >= 0.9999.
is_tier_b(pair(_, _, Rate)) :- Rate >= 0.95, Rate < 0.9999.

report_minimum_subset :-
    format('### Tier C: Minimum Discriminating Subset~n~n'),
    minimum_discriminating_set(CellSet),
    length(CellSet, NMin),
    format('Greedy set cover selected **~w** cells (out of 12):~n~n', [NMin]),
    report_subset_members(CellSet, 1),
    format('~n'),
    % Unique signature count
    findall(Sig, cached_grid_sig(_, Sig), AllSigs),
    sort(AllSigs, UniqueSigs),
    length(UniqueSigs, NUnique),
    format('**~w unique grid signatures** across the corpus.~n~n', [NUnique]),
    % Embedded Prolog facts
    format('### Embedded Prolog Facts~n~n'),
    format('```prolog~n'),
    format('%% Minimum discriminating subset~n'),
    format('minimum_covering_subset(~w).~n', [CellSet]),
    format('~n%% Tier A pairs (100% agreement)~n'),
    agreement_matrix(Matrix),
    forall(
        (member(pair(C1, C2, Rate), Matrix), Rate >= 0.9999),
        format('tier_a_pair(~w, ~w).~n', [C1, C2])
    ),
    format('```~n~n').

report_subset_members([], _).
report_subset_members([Cell|Rest], N) :-
    cell_short(Cell, Short),
    format('~w. `~w`~n', [N, Short]),
    N1 is N + 1,
    report_subset_members(Rest, N1).

/* ================================================================
   PHASE 2: COVERAGE DENSITY / GAP DETECTION
   ================================================================ */

%% expanded_d_values(-DVals)
%  7 d-values: 4 canonical + 3 midpoints.
expanded_d_values(DVals) :-
    config:param(canonical_d_institutional, D_inst),
    config:param(canonical_d_moderate, D_mod),
    config:param(canonical_d_analytical, D_an),
    config:param(canonical_d_powerless, D_pw),
    Mid1 is (D_inst + D_mod) / 2,
    Mid2 is (D_mod + D_an) / 2,
    Mid3 is (D_an + D_pw) / 2,
    DVals = [D_inst, Mid1, D_mod, Mid2, D_an, Mid3, D_pw].

%% expanded_sigma_values(-SVals)
%  5 sigma values: 3 canonical + 2 intermediate.
expanded_sigma_values([0.8, 0.9, 1.0, 1.1, 1.2]).

%% coarse_d_values(-DVals)
%  The 4 d-values in the standard grid.
coarse_d_values(DVals) :-
    config:param(canonical_d_institutional, D_inst),
    config:param(canonical_d_moderate, D_mod),
    config:param(canonical_d_analytical, D_an),
    config:param(canonical_d_powerless, D_pw),
    DVals = [D_inst, D_mod, D_an, D_pw].

%% coarse_sigma_values(-SVals)
%  The 3 sigma values in the standard grid.
coarse_sigma_values([0.8, 1.0, 1.2]).

%% d_label(+D, -Label)
%  Human-readable label for a d-value.
d_label(D, Label) :-
    config:param(canonical_d_institutional, D_inst),
    config:param(canonical_d_moderate, D_mod),
    config:param(canonical_d_analytical, D_an),
    config:param(canonical_d_powerless, D_pw),
    (   abs(D - D_inst) < 0.001 -> Label = institutional
    ;   abs(D - D_mod) < 0.001  -> Label = moderate
    ;   abs(D - D_an) < 0.001   -> Label = analytical
    ;   abs(D - D_pw) < 0.001   -> Label = powerless
    ;   Mid1 is (D_inst + D_mod) / 2,
        Mid2 is (D_mod + D_an) / 2,
        Mid3 is (D_an + D_pw) / 2,
        (   abs(D - Mid1) < 0.001 -> Label = 'mid(inst-mod)'
        ;   abs(D - Mid2) < 0.001 -> Label = 'mid(mod-ana)'
        ;   abs(D - Mid3) < 0.001 -> Label = 'mid(ana-pow)'
        ;   format(atom(Label), 'd=~3f', [D])
        )
    ).

%% sigma_label(+Sigma, -Label)
%  Human-readable label for a sigma value.
sigma_label(0.8, local) :- !.
sigma_label(0.9, regional) :- !.
sigma_label(1.0, national) :- !.
sigma_label(1.1, continental) :- !.
sigma_label(1.2, global) :- !.
sigma_label(S, Label) :- format(atom(Label), 's=~2f', [S]).

%% nearest_power_level(+D, -Power)
%  Maps a d-value to the nearest canonical power atom.
%  Used to inherit time_horizon/exit_options for the immutability gate.
nearest_power_level(D, Power) :-
    findall(
        Dist-P,
        (   member(P, [powerless, moderate, institutional, analytical]),
            constraint_indexing:canonical_d_for_power(P, DP),
            Dist is abs(D - DP)
        ),
        Dists
    ),
    sort(Dists, [_-Power|_]).

%% sigma_to_scope_atom(+Sigma, -ScopeAtom)
%  Maps a sigma value to the nearest scope atom.
sigma_to_scope_atom(Sigma, ScopeAtom) :-
    findall(
        Dist-S,
        (   member(S, [local, regional, national, continental, global]),
            constraint_indexing:scope_modifier(S, SM),
            Dist is abs(Sigma - SM)
        ),
        Dists
    ),
    sort(Dists, [_-ScopeAtom|_]).

%% classify_at_interpolated(+C, +D, +Sigma, -Type)
%  Classifies constraint C at an arbitrary (D, Sigma) point.
%  Computes chi directly: chi = BaseEps * f(D) * Sigma.
%  Inherits context settings from the nearest canonical power level.
classify_at_interpolated(C, D, Sigma, Type) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    (   narrative_ontology:constraint_metric(C, ExtMetricName, BaseEps)
    ->  true
    ;   BaseEps = 0.5
    ),
    constraint_indexing:sigmoid_f(D, FD),
    Chi is BaseEps * FD * Sigma,
    config:param(suppression_metric_name, SuppMetricName),
    (   narrative_ontology:constraint_metric(C, SuppMetricName, Supp)
    ->  true
    ;   Supp = 0
    ),
    nearest_power_level(D, Power),
    sigma_to_scope_atom(Sigma, ScopeAtom),
    structural_signatures:coupling_test_context(Power, ScopeAtom, BaseCtx),
    BaseCtx = context(AP, TH, EO, _),
    Context = context(AP, TH, EO, spatial_scope(ScopeAtom)),
    (   catch(
            drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type0),
            _,
            Type0 = unknown
        )
    ->  Type = Type0
    ;   Type = unknown
    ).

%% adjacent_pairs(+List, -Pairs)
%  Returns consecutive element pairs.
adjacent_pairs([], []).
adjacent_pairs([_], []).
adjacent_pairs([A, B | Rest], [A-B | Pairs]) :-
    adjacent_pairs([B | Rest], Pairs).

%% is_coarse_d(+D)
%  True if D is a canonical grid d-value.
is_coarse_d(D) :-
    coarse_d_values(Ds),
    member(D0, Ds),
    abs(D - D0) < 0.001.

%% is_coarse_sigma(+S)
%  True if S is a canonical grid sigma value.
is_coarse_sigma(S) :-
    coarse_sigma_values(Ss),
    member(S0, Ss),
    abs(S - S0) < 0.001.

%% same_coarse_interval_d(+D1, +D2)
%  True if D1 and D2 fall within the same coarse power interval.
same_coarse_interval_d(D1, D2) :-
    coarse_d_values(CoarseDs),
    sort(CoarseDs, Sorted),
    coarse_interval_idx(D1, Sorted, I1),
    coarse_interval_idx(D2, Sorted, I2),
    I1 =:= I2.

%% same_coarse_interval_s(+S1, +S2)
%  True if S1 and S2 fall within the same coarse scope interval.
same_coarse_interval_s(S1, S2) :-
    coarse_sigma_values(CoarseSs),
    sort(CoarseSs, Sorted),
    coarse_interval_idx(S1, Sorted, I1),
    coarse_interval_idx(S2, Sorted, I2),
    I1 =:= I2.

%% coarse_interval_idx(+V, +SortedBoundaries, -Index)
%  Returns the interval index for a value given sorted boundaries.
coarse_interval_idx(V, Boundaries, Index) :-
    coarse_interval_idx_(V, Boundaries, 0, Index).

coarse_interval_idx_(_, [_], N, N) :- !.
coarse_interval_idx_(V, [Lo, Hi | _], N, N) :-
    V >= Lo - 0.001, V =< Hi + 0.001, !.
coarse_interval_idx_(V, [_ | Rest], N, Index) :-
    N1 is N + 1,
    coarse_interval_idx_(V, Rest, N1, Index).

%% missed_transitions(-Transitions)
%  Finds classification transitions between adjacent fine-grid points
%  that lie within the same coarse interval (invisible to the 12-point grid).
%  Returns list of transition(C, point(D1,S1,T1), point(D2,S2,T2)).
missed_transitions(Transitions) :-
    all_corpus_constraints(Cs),
    expanded_d_values(DVals),
    expanded_sigma_values(SVals),
    adjacent_pairs(DVals, DAdjPairs),
    adjacent_pairs(SVals, SAdjPairs),
    length(Cs, NC),
    format(user_error, '[phase2] Scanning ~w constraints for missed transitions...~n', [NC]),
    % Power-axis transitions (fixed sigma, adjacent d-values in same coarse interval)
    findall(
        transition(C, point(D1, S, T1), point(D2, S, T2)),
        (   member(C, Cs),
            member(D1-D2, DAdjPairs),
            same_coarse_interval_d(D1, D2),
            member(S, SVals),
            classify_at_interpolated(C, D1, S, T1),
            classify_at_interpolated(C, D2, S, T2),
            T1 \= T2
        ),
        PowerTransitions
    ),
    % Scope-axis transitions (fixed d, adjacent sigma values in same coarse interval)
    findall(
        transition(C, point(D, S1, T1), point(D, S2, T2)),
        (   member(C, Cs),
            member(D, DVals),
            member(S1-S2, SAdjPairs),
            same_coarse_interval_s(S1, S2),
            classify_at_interpolated(C, D, S1, T1),
            classify_at_interpolated(C, D, S2, T2),
            T1 \= T2
        ),
        ScopeTransitions
    ),
    append(PowerTransitions, ScopeTransitions, Transitions),
    length(PowerTransitions, NPT),
    length(ScopeTransitions, NST),
    format(user_error, '[phase2] Found ~w power-axis + ~w scope-axis missed transitions~n',
           [NPT, NST]).

%% transition_density(+Transitions, -DensityMap)
%  Counts missed transitions per coarse-grid interval.
transition_density(Transitions, DensityMap) :-
    coarse_d_values(CoarseDs),
    coarse_sigma_values(CoarseSs),
    sort(CoarseDs, SortedDs),
    sort(CoarseSs, SortedSs),
    adjacent_pairs(SortedDs, DIntervals),
    adjacent_pairs(SortedSs, SIntervals),
    findall(
        interval(DLo, DHi, SLo, SHi, Count),
        (   member(DLo-DHi, DIntervals),
            member(SLo-SHi, SIntervals),
            aggregate_all(count,
                (   member(transition(_, point(D, S, _), point(_, _, _)), Transitions),
                    D >= DLo - 0.001, D =< DHi + 0.001,
                    S >= SLo - 0.001, S =< SHi + 0.001
                ),
                Count)
        ),
        DensityMap
    ).

/* ================================================================
   PHASE 2: REPORT GENERATION
   ================================================================ */

run_phase2 :-
    load_all_testsets,
    all_corpus_constraints(Cs),
    % Ensure Phase 1 signatures exist (needed for reference)
    (   cached_grid_sig(_, _) -> true ; compute_all_signatures(Cs) ),
    length(Cs, NC),
    format('## Phase 2: Coverage Density / Gap Detection~n~n'),
    report_dvalue_map,
    report_phase2_transitions(NC).

report_dvalue_map :-
    format('### Expanded Grid Definition~n~n'),
    expanded_d_values(DVals),
    expanded_sigma_values(SVals),
    length(DVals, ND), length(SVals, NS),
    Total is ND * NS,
    format('- **Power axis**: ~w points (4 canonical + 3 intermediate)~n', [ND]),
    format('- **Scope axis**: ~w points (3 canonical + 2 intermediate)~n', [NS]),
    format('- **Total cells**: ~w~n~n', [Total]),
    format('### D-Value Map~n~n'),
    format('| Label | d | f(d) | Canonical? |~n'),
    format('|-------|---|------|------------|~n'),
    forall(member(D, DVals), (
        d_label(D, Label),
        constraint_indexing:sigmoid_f(D, FD),
        (is_coarse_d(D) -> Canon = 'Yes' ; Canon = 'No'),
        format('| ~w | ~4f | ~3f | ~w |~n', [Label, D, FD, Canon])
    )),
    format('~n'),
    format('### Sigma-Value Map~n~n'),
    format('| Label | sigma | Canonical? |~n'),
    format('|-------|-------|------------|~n'),
    forall(member(S, SVals), (
        sigma_label(S, Label),
        (is_coarse_sigma(S) -> Canon = 'Yes' ; Canon = 'No'),
        format('| ~w | ~2f | ~w |~n', [Label, S, Canon])
    )),
    format('~n').

report_phase2_transitions(NC) :-
    format('### Missed Transitions~n~n'),
    missed_transitions(Transitions),
    length(Transitions, NT),
    % Count by axis
    include(is_power_transition, Transitions, PTs),
    include(is_scope_transition, Transitions, STs),
    length(PTs, NPT), length(STs, NST),
    % Unique constraints
    findall(C, member(transition(C, _, _), Transitions), CsRaw),
    sort(CsRaw, UniqueCs),
    length(UniqueCs, NUniq),
    format('- **Corpus size**: ~w constraints~n', [NC]),
    format('- **Total missed transitions**: ~w~n', [NT]),
    format('- **Power-axis transitions**: ~w~n', [NPT]),
    format('- **Scope-axis transitions**: ~w~n', [NST]),
    format('- **Unique constraints affected**: ~w~n~n', [NUniq]),
    % Detail table (if not too many)
    (   NT > 0
    ->  format('### Transition Detail~n~n'),
        format('| Constraint | D1 | Sigma | Type1 | D2 | Sigma2 | Type2 | Axis |~n'),
        format('|------------|-----|-------|-------|----|--------|-------|------|~n'),
        forall(member(transition(C, point(D1, S1, T1), point(D2, S2, T2)), Transitions), (
            (   D1 =\= D2
            ->  Axis = power
            ;   Axis = scope
            ),
            format('| ~w | ~4f | ~2f | ~w | ~4f | ~2f | ~w | ~w |~n',
                   [C, D1, S1, T1, D2, S2, T2, Axis])
        )),
        format('~n')
    ;   format('**No missed transitions found.** The 12-point grid appears to cover all classification boundaries within the corpus.~n~n')
    ),
    % Transition density
    report_density_heatmap(Transitions),
    % Most affected constraints
    (   NT > 0
    ->  report_most_affected(Transitions)
    ;   true
    ),
    % Recommendation
    report_recommendation(NT, NPT, NST, NC, NUniq).

is_power_transition(transition(_, point(D1, _, _), point(D2, _, _))) :- D1 =\= D2.
is_scope_transition(transition(_, point(_, S1, _), point(_, S2, _))) :- S1 =\= S2.

report_density_heatmap(Transitions) :-
    format('### Transition Density Heatmap~n~n'),
    transition_density(Transitions, DensityMap),
    coarse_d_values(CoarseDs),
    sort(CoarseDs, SortedDs),
    adjacent_pairs(SortedDs, DIntervals),
    coarse_sigma_values(CoarseSs),
    sort(CoarseSs, SortedSs),
    adjacent_pairs(SortedSs, SIntervals),
    % Build header dynamically from actual coarse scope intervals
    format('| Power Interval |'),
    forall(member(SLo-SHi, SIntervals), (
        sigma_label(SLo, SLoLabel),
        sigma_label(SHi, SHiLabel),
        format(' ~w-~w |', [SLoLabel, SHiLabel])
    )),
    format('~n|----------------|'),
    forall(member(_, SIntervals), format('----------|')),
    format('~n'),
    forall(member(DLo-DHi, DIntervals), (
        d_label(DLo, LabelLo),
        d_label(DHi, LabelHi),
        format('| ~w - ~w |', [LabelLo, LabelHi]),
        forall(member(SLo-SHi, SIntervals), (
            (   member(interval(DLo, DHi, SLo, SHi, Count), DensityMap)
            ->  true
            ;   Count = 0
            ),
            format(' ~w |', [Count])
        )),
        format('~n')
    )),
    format('~n').

report_most_affected(Transitions) :-
    format('### Most Transition-Prone Constraints~n~n'),
    findall(C, member(transition(C, _, _), Transitions), CsRaw),
    msort(CsRaw, Sorted),
    clumped(Sorted, Clumped),
    sort(2, @>=, Clumped, ByCount),
    format('| Constraint | Transitions | Types Involved |~n'),
    format('|------------|-------------|----------------|~n'),
    report_top_affected(ByCount, 20, Transitions).

report_top_affected(_, 0, _) :- !.
report_top_affected([], _, _) :- !.
report_top_affected([C-Count|Rest], N, Transitions) :-
    findall(T,
        (   member(transition(C, point(_, _, T), _), Transitions)
        ;   member(transition(C, _, point(_, _, T)), Transitions)
        ),
        TypesRaw),
    sort(TypesRaw, Types),
    atomic_list_concat(Types, ', ', TypeStr),
    format('| ~w | ~w | ~w |~n', [C, Count, TypeStr]),
    N1 is N - 1,
    report_top_affected(Rest, N1, Transitions).

report_recommendation(NT, NPT, NST, NC, NUniq) :-
    format('### Recommendation~n~n'),
    (   NT =:= 0
    ->  format('The 4x3 Index Grid appears adequate for the current corpus. ')
    ;   true
    ),
    (   NT > 0, NPT > NST
    ->  Pct is (NPT * 100) / max(1, NT),
        format('**~1f% of missed transitions are along the power axis.** ', [Pct]),
        format('Consider adding intermediate power levels (especially between '),
        format('institutional and moderate, where the d-value gap is largest: 0.00 to 0.646).~n~n')
    ;   true
    ),
    (   NT > 0, NST >= NPT
    ->  Pct is (NST * 100) / max(1, NT),
        format('**~1f% of missed transitions are along the scope axis.** ', [Pct]),
        format('Consider adding regional (sigma=0.9) and/or continental (sigma=1.1) scope levels.~n~n')
    ;   true
    ),
    (   NT > 0
    ->  AffectedPct is (NUniq * 100) / max(1, NC),
        format('**Overall**: ~w missed transitions affecting ~w of ~w constraints (~1f%). ',
               [NT, NUniq, NC, AffectedPct]),
        (   AffectedPct < 5.0
        ->  format('This is a low rate; the grid is largely sufficient but has minor blind spots.~n~n')
        ;   AffectedPct < 20.0
        ->  format('This is a moderate rate; grid refinement would improve coverage.~n~n')
        ;   format('This is a high rate; grid refinement is recommended.~n~n')
        )
    ;   true
    ).

/* ================================================================
   MAIN ENTRY POINT
   ================================================================ */

run_covering_analysis :-
    format('# Erdos-Selfridge Covering Analysis~n~n'),
    format('*Analyzes whether the 12-point Index Grid (4 powers x 3 scopes) adequately*~n'),
    format('*covers the constraint-space, or has redundant cells and coverage gaps.*~n~n'),
    format('---~n~n'),
    run_phase1,
    format('---~n~n'),
    run_phase2,
    format('---~n~n'),
    format('*End of covering analysis*~n').
