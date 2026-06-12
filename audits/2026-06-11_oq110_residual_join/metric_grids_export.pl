/* OQ-110 §1.2 — per-constraint metric time-grids for the OQ-105 cross-read.

   Emits, for every corpus_loader:corpus_constraint/1: the union time grid
   (temporal_residual:constraint_time_set/2) and the per-metric authored
   times for suppression_requirement / base_extractiveness / theater_ratio,
   plus suppression scalar presence and the static marker. The join script
   derives grid-misalignment rows (series exists, time absent) from these —
   the same condition that sets SuppBacked=false in drl_composition.pl:219-224.

   Run from prolog/:
     swipl -g "consult('../audits/2026-06-11_oq110_residual_join/metric_grids_export.pl'), run, halt" -t "halt(1)"
*/

:- [stack].
:- corpus_loader:ensure_corpus_loaded.

run :-
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    open('../outputs/oq110_metric_grids.json', write, S),
    format(S, '{~n', []),
    write_grids(S, Cs),
    format(S, '}~n', []),
    close(S),
    length(Cs, N),
    format("emitted ~w constraint grids to outputs/oq110_metric_grids.json~n", [N]).

write_grids(_, []).
write_grids(S, [C]) :- !, write_one_grid(S, C), format(S, '~n', []).
write_grids(S, [C | R]) :- write_one_grid(S, C), format(S, ',~n', []), write_grids(S, R).

write_one_grid(S, C) :-
    temporal_residual:constraint_time_set(C, Times),
    metric_times(C, suppression_requirement, SuppTs),
    metric_times(C, base_extractiveness, EpsTs),
    metric_times(C, theater_ratio, TheaterTs),
    (   narrative_ontology:constraint_metric(C, suppression_requirement, _)
    ->  SS = true ; SS = false ),
    (   narrative_ontology:suppression_profile(C, static)
    ->  SP = true ; SP = false ),
    format(S, '"~w": {"times": ~w, "supp_times": ~w, "eps_times": ~w, "theater_times": ~w, "supp_scalar": ~w, "supp_static_marker": ~w}',
           [C, Times, SuppTs, EpsTs, TheaterTs, SS, SP]).

metric_times(C, Metric, Ts) :-
    findall(T, narrative_ontology:measurement(_, C, Metric, T, _), Ts0),
    sort(Ts0, Ts).
