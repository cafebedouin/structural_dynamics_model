% ============================================================================
% FPN REPORT — Fixed-Point Network Iteration Analysis
% ============================================================================
% Standalone script. Run from prolog/ directory:
%   swipl -l stack.pl -l covering_analysis.pl -l fpn_report.pl \
%         -g "run_fpn_report, halt."
%
% Bulk-loads all testsets, runs the fixed-point network iterator,
% and outputs a markdown report comparing one-hop vs multi-hop
% effective purity values.
% ============================================================================

:- use_module(covering_analysis).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(drl_fpn, [fpn_run/3, fpn_intrinsic/2, fpn_ep/3]).
:- use_module(drl_purity_network, [effective_purity/4]).

:- use_module(library(lists)).

%% run_fpn_report
%  Main entry point. Loads corpus, runs FPN, outputs markdown.
run_fpn_report :-
    format(user_error, '[fpn] Starting fixed-point network analysis...~n', []),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),

    % Discover constraints
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),
    length(Constraints, NTotal),
    format(user_error, '[fpn] Found ~w constraints.~n', [NTotal]),

    % Run FPN iterator
    format(user_error, '[fpn] Running fixed-point iteration...~n', []),
    drl_fpn:fpn_run(Constraints, Context, Result),
    Result = fpn_result(Iterations, MaxDelta, Converged, _NConstraints),
    format(user_error, '[fpn] Converged=~w after ~w iterations (maxDelta=~6f).~n',
           [Converged, Iterations, MaxDelta]),

    % Collect comparison data: one-hop EP vs fixed-point EP
    findall(row(C, IP, OneHopEP, FpnEP, Type),
        (   member(C, Constraints),
            fpn_intrinsic_safe(C, IP),
            IP >= 0.0,
            one_hop_ep_safe(C, Context, OneHopEP),
            fpn_ep_safe(C, Context, FpnEP),
            type_safe(C, Context, Type)
        ),
        Rows),
    length(Rows, NCompared),
    format(user_error, '[fpn] Compared ~w constraints.~n', [NCompared]),

    % Classify zone migrations
    findall(row(C, IP, OH, FP, T),
        (member(row(C, IP, OH, FP, T), Rows), purity_zone(OH, Z1), purity_zone(FP, Z2), Z1 \= Z2),
        Migrations),
    length(Migrations, NMigrations),

    % Find significant movers (delta > 0.01)
    findall(row(C, IP, OH, FP, T),
        (member(row(C, IP, OH, FP, T), Rows), Diff is OH - FP, Diff > 0.01),
        SignificantMovers),
    length(SignificantMovers, NSignificant),

    % Write markdown report to stdout
    format('<!-- FPN_REPORT_START -->~n'),
    format('# Fixed-Point Network Iteration Report~n~n'),
    format('*Generated: corpus-wide multi-hop purity propagation analysis via drl_fpn:fpn_run/3*~n~n'),

    report_convergence(Iterations, MaxDelta, Converged, NTotal, NCompared),
    report_summary_stats(Rows, NSignificant, NMigrations),
    report_zone_migrations(Migrations),
    report_significant_movers(SignificantMovers),
    report_type_breakdown(Rows),

    format('---~n'),
    format('*End of FPN report*~n'),
    format(user_error, '[fpn] Done.~n', []).

/* ================================================================
   SAFE ACCESSORS
   ================================================================ */

fpn_intrinsic_safe(C, IP) :-
    (   drl_fpn:fpn_intrinsic(C, IP) -> true ; IP = -1.0 ).

one_hop_ep_safe(C, Context, EP) :-
    (   catch(drl_purity_network:effective_purity(C, Context, EP, _), _, fail)
    ->  true
    ;   EP = -1.0
    ).

fpn_ep_safe(C, Context, EP) :-
    (   drl_fpn:fpn_ep(C, Context, EP) -> true ; EP = -1.0 ).

type_safe(C, Context, Type) :-
    (   drl_core:dr_type(C, Context, Type) -> true ; Type = unknown ).

/* ================================================================
   PURITY ZONES
   ================================================================ */

purity_zone(EP, sound)     :- EP >= 0.70, !.
purity_zone(EP, contested) :- EP >= 0.50, !.
purity_zone(EP, degraded)  :- EP >= 0.30, !.
purity_zone(_,  critical).

/* ================================================================
   REPORT SECTIONS
   ================================================================ */

report_convergence(Iterations, MaxDelta, Converged, NTotal, NCompared) :-
    format('## Convergence Metadata~n~n'),
    format('| Property | Value |~n'),
    format('|----------|-------|~n'),
    format('| **Constraints in corpus** | ~w |~n', [NTotal]),
    format('| **Constraints compared** | ~w |~n', [NCompared]),
    format('| **Iterations to convergence** | ~w |~n', [Iterations]),
    format('| **Final max residual** | ~6f |~n', [MaxDelta]),
    (   Converged = true
    ->  format('| **Converged** | Yes |~n')
    ;   format('| **Converged** | **NO** (hit iteration cap) |~n')
    ),
    config:param(fpn_epsilon, Eps),
    config:param(fpn_max_iterations, MaxIter),
    format('| **Epsilon** | ~f |~n', [Eps]),
    format('| **Max iterations** | ~w |~n~n', [MaxIter]).

report_summary_stats(Rows, NSignificant, NMigrations) :-
    format('## Summary~n~n'),
    length(Rows, N),
    (   N > 0
    ->  findall(D, (member(row(_, _, OH, FP, _), Rows), D is OH - FP), Deltas),
        max_list(Deltas, MaxShift),
        sum_list(Deltas, SumD),
        AvgShift is SumD / N,
        format('| Metric | Value |~n'),
        format('|--------|-------|~n'),
        format('| **Constraints with significant shift (>0.01)** | ~w |~n', [NSignificant]),
        format('| **Zone migrations** | ~w |~n', [NMigrations]),
        format('| **Max EP shift** | ~6f |~n', [MaxShift]),
        format('| **Average EP shift** | ~6f |~n~n', [AvgShift])
    ;   format('No constraints with valid purity data.~n~n')
    ).

report_zone_migrations([]) :-
    format('## Zone Migrations~n~n'),
    format('No constraints changed purity zone under multi-hop propagation.~n~n').
report_zone_migrations(Migrations) :-
    Migrations \= [],
    format('## Zone Migrations~n~n'),
    format('Constraints whose purity zone changed from one-hop to fixed-point:~n~n'),
    format('| Constraint | Type | One-Hop EP | Zone | FPN EP | Zone | Shift |~n'),
    format('|------------|------|-----------|------|--------|------|-------|~n'),
    forall(member(row(C, _IP, OH, FP, T), Migrations), (
        purity_zone(OH, Z1),
        purity_zone(FP, Z2),
        Shift is OH - FP,
        format('| ~w | ~w | ~4f | ~w | ~4f | ~w | ~4f |~n',
               [C, T, OH, Z1, FP, Z2, Shift])
    )),
    format('~n').

report_significant_movers([]) :-
    format('## Significant Movers (shift > 0.01)~n~n'),
    format('No constraints shifted by more than 0.01.~n~n').
report_significant_movers(Movers) :-
    Movers \= [],
    format('## Significant Movers (shift > 0.01)~n~n'),
    % Sort by shift descending
    findall(Shift-row(C, IP, OH, FP, T),
        (member(row(C, IP, OH, FP, T), Movers), Shift is OH - FP),
        ShiftPairs),
    msort(ShiftPairs, Sorted),
    reverse(Sorted, Desc),
    format('| Constraint | Type | Intrinsic | One-Hop EP | FPN EP | Shift |~n'),
    format('|------------|------|-----------|-----------|--------|-------|~n'),
    forall(member(S-row(C, IP, OH, FP, T), Desc), (
        format('| ~w | ~w | ~4f | ~4f | ~4f | ~4f |~n',
               [C, T, IP, OH, FP, S])
    )),
    format('~n').

report_type_breakdown(Rows) :-
    format('## Type Breakdown~n~n'),
    format('Average EP shift by constraint type:~n~n'),
    format('| Type | Count | Avg One-Hop EP | Avg FPN EP | Avg Shift |~n'),
    format('|------|-------|---------------|------------|-----------|~n'),
    findall(T, member(row(_, _, _, _, T), Rows), Types),
    sort(Types, UniqueTypes),
    forall(member(Type, UniqueTypes), (
        findall(OH-FP,
            member(row(_, _, OH, FP, Type), Rows),
            Pairs),
        length(Pairs, Count),
        (   Count > 0
        ->  findall(O, member(O-_, Pairs), OHs),
            findall(F, member(_-F, Pairs), FPs),
            sum_list(OHs, SumOH), AvgOH is SumOH / Count,
            sum_list(FPs, SumFP), AvgFP is SumFP / Count,
            AvgShift is AvgOH - AvgFP,
            format('| ~w | ~w | ~4f | ~4f | ~4f |~n',
                   [Type, Count, AvgOH, AvgFP, AvgShift])
        ;   true
        )
    )),
    format('~n').
