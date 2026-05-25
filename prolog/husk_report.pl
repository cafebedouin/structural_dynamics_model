% ============================================================================
% HUSK REPORT — Temporal EP Decay Analysis
% ============================================================================
% Standalone script. Run from prolog/ directory:
%   swipl -l stack.pl -l husk_report.pl -g "run_husk_report, halt."
%
% Bulk-loads all testsets, computes husk_series in the powerless canonical
% context for each constraint, and writes outputs/husk_data.json.
%
% Context: powerless canonical (local scope, trapped exit, biographical horizon)
% — this is the victim's perspective where tangled_rope extraction is most
% visible and type stability holds across the ε-series.
% ============================================================================

:- use_module(drl_composition, [husk_series/3, husk_exists/3]).
:- use_module(boltzmann_compliance, [boltzmann_floor_for/2]).
:- use_module(logical_fingerprint, [known_constraint/1]).

:- use_module(library(lists)).

%% powerless_husk_context(-Context)
%  The canonical powerless observer context for husk analysis.
powerless_husk_context(
    context(agent_power(powerless), time_horizon(biographical),
            exit_options(trapped), spatial_scope(local))).

%% run_husk_report
%  Entry point. Loads corpus, computes husk metrics, writes husk_data.json
%  and emits husk_report.md markdown to stdout.
run_husk_report :-
    format(user_error, '[husk] Starting husk analysis...~n', []),
    corpus_loader:load_all_testsets,
    powerless_husk_context(Context),

    findall(C, (known_constraint(C), atom(C)), RawCs),
    sort(RawCs, Constraints),
    length(Constraints, N),
    format(user_error, '[husk] Processing ~w constraints...~n', [N]),

    % Collect all husk data in one pass (used by both JSON and markdown).
    findall(husk_row(C, HuskExists, EpSeries, TypeStable, StableType, EP0, EPLast, NativeFloor),
        (   member(C, Constraints),
            husk_entry_data(C, Context, HuskExists, EpSeries, TypeStable, StableType,
                            EP0, EPLast, NativeFloor)
        ),
        Rows),

    setup_call_cleanup(
        open('../outputs/husk_data.json', write, JStream),
        write_husk_json_rows(JStream, Rows),
        close(JStream)
    ),
    format(user_error, '[husk] Wrote husk_data.json~n', []),

    write_husk_markdown(Rows, N),
    format(user_error, '[husk] Done.~n', []).

%% write_husk_json_rows(+Stream, +Rows)
write_husk_json_rows(Stream, Rows) :-
    format(Stream, '{~n', []),
    write_husk_json_rows_(Stream, Rows),
    format(Stream, '}~n', []).

write_husk_json_rows_(_, []).
write_husk_json_rows_(Stream, [Row]) :-
    !,
    write_husk_json_row(Stream, Row, false).
write_husk_json_rows_(Stream, [Row|Rest]) :-
    write_husk_json_row(Stream, Row, true),
    write_husk_json_rows_(Stream, Rest).

%% write_husk_json_row(+Stream, +Row, +Comma)
write_husk_json_row(Stream, husk_row(C, HuskExists, EpSeries, TypeStable, StableType,
                                      EP0, EPLast, NativeFloor), Comma) :-
    format(Stream, '  "~w": {~n', [C]),
    write_bool(Stream, '    "husk_exists"', HuskExists), format(Stream, ',~n', []),
    write_ep_series(Stream, EpSeries),
    write_nullable_bool(Stream, '    "type_stable"', TypeStable), format(Stream, ',~n', []),
    write_nullable_atom(Stream, '    "stable_type"', StableType), format(Stream, ',~n', []),
    write_nullable_float(Stream, '    "ep_t0"', EP0), format(Stream, ',~n', []),
    write_nullable_float(Stream, '    "ep_tlast"', EPLast), format(Stream, ',~n', []),
    write_nullable_float(Stream, '    "native_floor"', NativeFloor), format(Stream, '~n', []),
    format(Stream, '  }', []),
    (Comma == true -> format(Stream, ',~n', []) ; format(Stream, '~n', [])).

/* ================================================================
   MARKDOWN REPORT (stdout)
   ================================================================ */

%% write_husk_markdown(+Rows, +NTotal)
%  Emits the full husk_report.md to stdout.
write_husk_markdown(Rows, NTotal) :-
    format('<!-- HUSK_REPORT_START -->~n'),
    format('# Husk Signature Report~n~n'),
    format('*Temporal EP decay analysis — powerless canonical context*~n~n'),

    % Summary counts
    include([husk_row(_, true, _, _, _, _, _, _)]>>true, Rows, ExistsRows),
    include([husk_row(_, _, [], _, _, _, _, _)]>>true, Rows, NoSeriesRows),
    length(ExistsRows, NExists),
    length(NoSeriesRows, NNoSeries),
    NHasSeries is NTotal - NNoSeries,

    format('## Summary~n~n'),
    format('| Metric | Value |~n'),
    format('|--------|-------|~n'),
    format('| **Total constraints** | ~w |~n', [NTotal]),
    format('| **With measurement series** | ~w |~n', [NHasSeries]),
    format('| **husk_exists = true** | ~w |~n', [NExists]),
    format('| **No series (graceful null)** | ~w |~n~n', [NNoSeries]),

    % Type breakdown among husk_exists=true
    write_husk_type_breakdown(ExistsRows),

    % EP decay magnitude among husk_exists=true
    write_husk_ep_stats(ExistsRows),

    % Husk constraints table (sorted by EP fall magnitude)
    write_husk_table(ExistsRows),

    format('---~n'),
    format('*Context: powerless canonical (local scope, trapped exit, biographical horizon)*~n'),
    format('*Floor source: `boltzmann_floor_for/2` from config.pl*~n').

%% write_husk_type_breakdown(+ExistsRows)
write_husk_type_breakdown([]) :- !.
write_husk_type_breakdown(Rows) :-
    findall(T, member(husk_row(_, _, _, _, T, _, _, _), Rows), Types),
    msort(Types, Sorted),
    format('## Stable Type Breakdown (husk_exists = true)~n~n'),
    format('| Stable Type | Count |~n'),
    format('|-------------|-------|~n'),
    aggregate_types(Sorted, Counts),
    forall(member(T-N, Counts), format('| ~w | ~w |~n', [T, N])),
    format('~n').

aggregate_types([], []).
aggregate_types([T|Rest], [T-N|Counts]) :-
    partition(=(T), Rest, Same, Other),
    length(Same, K), N is K + 1,
    aggregate_types(Other, Counts).

%% write_husk_ep_stats(+ExistsRows)
write_husk_ep_stats([]) :- !.
write_husk_ep_stats(Rows) :-
    findall(Delta, (
        member(husk_row(_, _, _, _, _, EP0, EPLast, _), Rows),
        EP0 \= null, EPLast \= null,
        Delta is EP0 - EPLast
    ), Deltas),
    (   Deltas \= []
    ->  max_list(Deltas, MaxDelta),
        sum_list(Deltas, SumDelta),
        length(Deltas, ND),
        AvgDelta is SumDelta / ND,
        format('## EP Decay Statistics (husk_exists = true)~n~n'),
        format('| Metric | Value |~n'),
        format('|--------|-------|~n'),
        format('| **Max EP fall (EP₀ − EP_last)** | ~4f |~n', [MaxDelta]),
        format('| **Mean EP fall** | ~4f |~n~n', [AvgDelta])
    ;   true
    ).

%% write_husk_table(+ExistsRows)
%  Sorted by EP fall descending; top 100 shown to keep report tractable.
write_husk_table([]) :- !.
write_husk_table(Rows) :-
    findall(Delta-husk_row(C, HE, EpS, TS, ST, EP0, EPL, NF),
        (   member(husk_row(C, HE, EpS, TS, ST, EP0, EPL, NF), Rows),
            EP0 \= null, EPL \= null,
            Delta is EP0 - EPL
        ),
        Pairs),
    msort(Pairs, Sorted),
    reverse(Sorted, Desc),
    length(Desc, NAll),
    (NAll > 100 -> length(Shown, 100), append(Shown, _, Desc) ; Shown = Desc),
    length(Shown, NShown),
    format('## Husk Constraints (sorted by EP fall, top ~w of ~w)~n~n', [NShown, NAll]),
    format('| Constraint | Stable Type | EP₀ | EP_last | EP fall | Native floor | Series pts |~n'),
    format('|------------|-------------|-----|---------|---------|--------------|------------|~n'),
    forall(member(Delta-husk_row(C, _, EpSeries, _, ST, EP0, EPL, NF), Shown), (
        length(EpSeries, Npts),
        format('| ~w | ~w | ~4f | ~4f | ~4f | ~4f | ~w |~n',
               [C, ST, EP0, EPL, Delta, NF, Npts])
    )),
    format('~n').

%% husk_entry_data(+C, +Context, -HuskExists, -EpSeries, -TypeStable,
%%                 -StableType, -EP0, -EPLast, -NativeFloor)
%  Computes all husk metrics for a single constraint.
%  Degrades gracefully when no base_extractiveness measurement series exists.
husk_entry_data(C, Context, HuskExists, EpSeries, TypeStable, StableType,
                EP0, EPLast, NativeFloor) :-
    (   catch(husk_series(C, Context, Series), _, fail),
        Series \= []
    ->  husk_exists(C, Context, HuskExists),
        extract_ep_series(Series, EpSeries),
        extract_types(Series, Types),
        sort(Types, UniqueTypes),
        (UniqueTypes = [OneType] -> TypeStable = true, StableType = OneType
                                  ; TypeStable = false, StableType = null),
        Series = [husk_pt(_, _, EP0)|_],
        last(Series, husk_pt(_, _, EPLast))
    ;   HuskExists = false,
        EpSeries = [],
        TypeStable = null,
        StableType = null,
        EP0 = null,
        EPLast = null
    ),
    (   catch(boltzmann_floor_for(C, NativeFloor), _, fail)
    ->  true
    ;   NativeFloor = null
    ).

extract_ep_series([], []).
extract_ep_series([husk_pt(T, _, EP)|Rest], [ep(T, EP)|ERest]) :-
    extract_ep_series(Rest, ERest).

extract_types([], []).
extract_types([husk_pt(_, Ty, _)|Rest], [Ty|TRest]) :-
    extract_types(Rest, TRest).

/* ================================================================
   JSON WRITE HELPERS
   ================================================================ */

write_bool(Stream, Key, true)  :- format(Stream, '~w: true',  [Key]).
write_bool(Stream, Key, false) :- format(Stream, '~w: false', [Key]).

write_nullable_bool(Stream, Key, true)  :- format(Stream, '~w: true',  [Key]).
write_nullable_bool(Stream, Key, false) :- format(Stream, '~w: false', [Key]).
write_nullable_bool(Stream, Key, null)  :- format(Stream, '~w: null',  [Key]).

write_nullable_atom(Stream, Key, null) :-
    !,
    format(Stream, '~w: null', [Key]).
write_nullable_atom(Stream, Key, Val) :-
    format(Stream, '~w: "~w"', [Key, Val]).

write_nullable_float(Stream, Key, null) :-
    !,
    format(Stream, '~w: null', [Key]).
write_nullable_float(Stream, Key, Val) :-
    format(Stream, '~w: ~4f', [Key, Val]).

write_ep_series(Stream, []) :-
    format(Stream, '    "ep_native_series": [],~n', []).
write_ep_series(Stream, EpSeries) :-
    EpSeries \= [],
    format(Stream, '    "ep_native_series": [', []),
    write_ep_points(Stream, EpSeries),
    format(Stream, '],~n', []).

write_ep_points(_, []).
write_ep_points(Stream, [ep(T, EP)]) :-
    !,
    format(Stream, '{"t": ~w, "ep": ~4f}', [T, EP]).
write_ep_points(Stream, [ep(T, EP)|Rest]) :-
    format(Stream, '{"t": ~w, "ep": ~4f}, ', [T, EP]),
    write_ep_points(Stream, Rest).
