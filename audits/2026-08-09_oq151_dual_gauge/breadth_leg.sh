#!/bin/bash
# OQ-151 Commit-1 breadth witness: per-leg empty_chair histogram + suites.
# Usage: breadth_leg.sh <corpus_path_relative_to_prolog>
# Serialized by the caller — never concurrent with a pipeline run.
LEG="$1"
cd /home/scott/bin/structural_dynamics_model/prolog || exit 2
swipl -l stack.pl -l reading_registry.pl -l commentary_census.pl -g "
retractall(config:param(corpus_path, _)),
asserta(config:param(corpus_path, '$LEG')),
cache_registry:clear_all_caches,
corpus_loader:load_all_testsets,
aggregate_all(count, corpus_loader:corpus_constraint(_), N),
format('LEG ~w N=~w~n', ['$LEG', N]),
findall(B, ( corpus_loader:corpus_constraint(C),
             commentary_census:commentary_cell(empty_chair, C, B) ), Bs),
length(Bs, NB),
format('LEG ~w cells=~w~n', ['$LEG', NB]),
sort(Bs, Us),
forall(member(U, Us),
       ( aggregate_all(count, member(U, Bs), K),
         format('HIST ~w ~w ~w~n', ['$LEG', U, K]) )),
[tests/test_empty_chair],
run_tests(empty_chair),
[tests/test_reading_totality],
run_tests(reading_totality),
halt" -t "halt(1)" 2>&1
