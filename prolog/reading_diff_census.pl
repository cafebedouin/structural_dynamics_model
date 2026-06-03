% ============================================================================
% READING_DIFF_CENSUS — OQ-59 #3 : corpus-wide within-kernel twin census
% ============================================================================
% For every kernel with >=2 readings, run reading_diff over each unordered
% reading-pair and classify the pair by its stability verdict across the
% declared key chain [exact, fuzzy_agent_power]:
%   robustly_binocular   — >=1 disparity under EVERY key (genuine depth → preserve)
%   key_fragile          — regime flips across keys (depth depends on the seat)
%   robustly_undersampled — 0 disparity under every key (coverage gap, not convergence)
%
% Authored-cells-only (delegates to reading_diff/6). WITHIN-kernel pairs only —
% cross-kernel near-twins (e.g. westphalia_ vs westphalian_) are a separate probe.
%
% Run (from prolog/):
%   swipl -g "[stack], corpus_loader:load_all_testsets, [reading_diff_census], \
%     reading_diff_census:run_census, halt" -t "halt(1)"
%   % and write the full per-pair TSV:
%   swipl -g "[stack], corpus_loader:load_all_testsets, [reading_diff_census], \
%     reading_diff_census:census_to('../outputs/reading_diff_census.tsv'), halt" -t "halt(1)"
% ============================================================================

:- module(reading_diff_census, [
    run_census/0,
    census_to/1,
    census_rows/1,
    multi_kernel/2,
    reading_pair/3
]).

% reading_diff and narrative_ontology are loaded by [stack] (this module's
% precondition); all calls to them are module-qualified below, so we do not
% re-import them here (a bare use_module(reading_diff) mis-resolves under the
% stack load context). Only library deps are imported.
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(aggregate)).

%% multi_kernel(-Kernel, -Readings) : kernels with >=2 distinct readings.
%  findall + group (NOT setof with R^Module:Goal — the ^/: operator interaction
%  drops the module qualifier at runtime, giving '$bags':cs_kernel_id/2).
multi_kernel(K, Rs) :-
    findall(K0-R, narrative_ontology:cs_kernel_id(R, K0), KRs),
    keysort(KRs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    member(K-Rs0, Grouped),
    sort(Rs0, Rs),
    length(Rs, N),
    N >= 2.

%% reading_pair(-Kernel, -A, -B) : unordered within-kernel pairs (A before B).
reading_pair(K, A, B) :-
    multi_kernel(K, Rs),
    append(_, [A|Rest], Rs),
    member(B, Rest).

%% census_row(-row(Kernel,A,B, Verdict, ExactAgree,ExactDisp,ExactBlind, FuzzyDisp))
census_row(row(K, A, B, Verdict, EA, ED, EB, FD)) :-
    reading_pair(K, A, B),
    reading_diff:stability_verdict(A, B, Verdict),
    reading_diff:reading_diff(A, B, exact, Ag, Dp, Bl),
    length(Ag, EA), length(Dp, ED), length(Bl, EB),
    reading_diff:reading_diff(A, B, fuzzy_agent_power, _, FDp, _),
    length(FDp, FD).

census_rows(Rows) :- findall(R, census_row(R), Rows).

%% run_census : print the verdict distribution + headline cases to stdout.
run_census :-
    census_rows(Rows),
    length(Rows, NPairs),
    aggregate_all(count, reading_pair(_, _, _), NPairsCheck),
    aggregate_all(set(K), member(row(K, _, _, _, _, _, _, _), Rows), Kernels),
    length(Kernels, NKern),
    count_verdict(Rows, robustly_binocular, NB),
    count_verdict(Rows, key_fragile, NF),
    count_verdict(Rows, robustly_undersampled, NU),
    format("~n================ reading_diff within-kernel census ================~n"),
    format("multi-reading kernels : ~w~n", [NKern]),
    format("reading-pairs scored  : ~w  (enumerator check: ~w)~n", [NPairs, NPairsCheck]),
    format("~nSTABILITY VERDICT distribution (over [exact, fuzzy_agent_power]):~n"),
    pct(NB, NPairs, PB), pct(NF, NPairs, PF), pct(NU, NPairs, PU),
    format("  robustly_binocular    : ~w (~1f%)  — genuine depth, preserve~n", [NB, PB]),
    format("  key_fragile           : ~w (~1f%)  — depth depends on the alignment seat~n", [NF, PF]),
    format("  robustly_undersampled : ~w (~1f%)  — coverage gap, NOT convergence~n", [NU, PU]),
    format("~n-- robustly_binocular pairs (the binocular-proper set) --~n"),
    forall(( member(row(K, A, B, robustly_binocular, EA, ED, EB, FD), Rows) ),
           format("  [~w]  ~w  vs  ~w   exact ~w/~w/~w  fuzzyD ~w~n",
                  [K, A, B, EA, ED, EB, FD])),
    format("~n-- robustly_undersampled pairs (exact: 0 disparity, all coverage gap) --~n"),
    forall(( member(row(K, A, B, robustly_undersampled, EA, ED, EB, FD), Rows) ),
           format("  [~w]  ~w  vs  ~w   exact ~w/~w/~w  fuzzyD ~w~n",
                  [K, A, B, EA, ED, EB, FD])),
    format("==================================================================~n").

count_verdict(Rows, V, N) :-
    aggregate_all(count, member(row(_, _, _, V, _, _, _, _), Rows), N).

pct(_, 0, 0.0) :- !.
pct(N, D, P) :- P is 100.0 * N / D.

%% census_to(+File) : write the full per-pair TSV (header + one row per pair).
census_to(File) :-
    census_rows(Rows),
    setup_call_cleanup(
        open(File, write, S),
        ( format(S, "kernel\treading_a\treading_b\tverdict\texact_agree\texact_disp\texact_blind\tfuzzy_disp~n", []),
          forall(member(row(K, A, B, V, EA, ED, EB, FD), Rows),
                 format(S, "~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n", [K, A, B, V, EA, ED, EB, FD]))
        ),
        close(S)),
    length(Rows, N),
    format("wrote ~w rows to ~w~n", [N, File]).
