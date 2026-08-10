#!/bin/bash
# OQ-151 per-item verification: re-derive both gauges + empty_chair_state in
# swipl for a list of constraint ids on one leg.
# Usage: per_item_offdiag.sh <leg> <cell_label> <id1> [id2 ...]
LEG="$1"; CELL="$2"; shift 2
IDS=$(printf "%s," "$@" | sed 's/,$//')
cd /home/scott/bin/structural_dynamics_model/prolog || exit 2
swipl -l stack.pl -l reading_registry.pl -l commentary_census.pl -g "
retractall(config:param(corpus_path, _)),
asserta(config:param(corpus_path, '$LEG')),
cache_registry:clear_all_caches,
corpus_loader:load_all_testsets,
forall(member(C, [$IDS]),
       ( ( grothendieck_cohomology:cohomological_obstruction(C, PH0, PH1)
         -> true ; PH0 = null, PH1 = null ),
         ( stakeholder_seats:stakeholder_obstruction(C, SH0, SH1, NS, NR)
         -> true ; SH0 = err, SH1 = err, NS = err, NR = err ),
         stakeholder_seats:empty_chair_state(C, ECS),
         stakeholder_seats:consensus_provenance(C, V),
         format('ITEM leg=~w cell=~w id=~w power_h=(~w,~w) seat_h=(~w,~w,ns=~w,nr=~w)~n  empty_chair=~w~n  consensus=~w~n',
                ['$LEG', '$CELL', C, PH0, PH1, SH0, SH1, NS, NR, ECS, V]) )),
halt" -t "halt(1)" 2>&1 | grep -Ev "^(Warning|====|   |%|\[)" | grep -v "^$"
