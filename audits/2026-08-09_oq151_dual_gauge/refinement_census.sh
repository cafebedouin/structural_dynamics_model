#!/bin/bash
# OQ-151 refinement census per leg: empty_chair_state histogram + the
# exhaustive 8-token partition of the mcc candidate set + Sigma identity +
# expected-zero cells. Per-item MCC lines printed for the per-item log.
LEG="$1"
cd /home/scott/bin/structural_dynamics_model/prolog || exit 2
swipl -l stack.pl -l reading_registry.pl -l commentary_census.pl -g "
retractall(config:param(corpus_path, _)),
asserta(config:param(corpus_path, '$LEG')),
cache_registry:clear_all_caches,
corpus_loader:load_all_testsets,
aggregate_all(count, corpus_loader:corpus_constraint(_), N),
format('CENSUS leg=~w n=~w~n', ['$LEG', N]),
% full histogram
findall(B, ( corpus_loader:corpus_constraint(C),
             commentary_census:commentary_cell(empty_chair, C, B) ), Bs),
sort(Bs, Us),
forall(member(U, Us),
       ( aggregate_all(count, member(U, Bs), K),
         format('HIST ~w ~w~n', [U, K]) )),
% mcc candidate set + its 8-token partition
findall(C-B2, ( corpus_loader:corpus_constraint(C),
                stakeholder_seats:consensus_provenance(C, V),
                ( V = manufactured_consensus_candidate(_)
                ; V = manufactured_consensus_candidate_untypeable(_) ),
                commentary_census:commentary_cell(empty_chair, C, B2) ), MccPairs),
length(MccPairs, NMcc),
format('MCC_N ~w~n', [NMcc]),
findall(B3, member(_-B3, MccPairs), MccBs),
forall(member(Tok, [empty_chair_dissent, empty_chair_dissent_untypeable,
                    excluded_untyped, excluded_concurs, excluded_concurs_untypeable,
                    included_plural, included_insufficient, no_excluded_seat]),
       ( aggregate_all(count, member(Tok, MccBs), K3),
         format('MCC_PARTITION ~w ~w~n', [Tok, K3]) )),
aggregate_all(sum(K4),
    ( member(Tok4, [empty_chair_dissent, empty_chair_dissent_untypeable,
                    excluded_untyped, excluded_concurs, excluded_concurs_untypeable,
                    included_plural, included_insufficient, no_excluded_seat]),
      aggregate_all(count, member(Tok4, MccBs), K4) ), Sum),
( Sum =:= NMcc -> SigTok = ok ; SigTok = violation ),
format('SIGMA_IDENTITY sum=~w mcc_n=~w ~w~n', [Sum, NMcc, SigTok]),
% expected-zero alarm cells on the mcc set
forall(member(Z, [included_plural, included_insufficient, no_excluded_seat]),
       ( aggregate_all(count, member(Z, MccBs), KZ),
         ( KZ =:= 0 -> format('EXPECTED_ZERO ~w 0 ok~n', [Z])
         ; format('EXPECTED_ZERO ~w ~w PATTERN2_FORK_ALARM~n', [Z, KZ]) ) )),
% per-item lines for every dissent member (full state term)
forall(( member(C5-B5, MccPairs),
         memberchk(B5, [empty_chair_dissent, empty_chair_dissent_untypeable]) ),
       ( stakeholder_seats:empty_chair_state(C5, S5),
         stakeholder_seats:consensus_provenance(C5, V5),
         stakeholder_seats:stakeholder_obstruction(C5, H0, H1, NS, NR),
         format('DISSENT_ITEM ~w state=~w consensus=~w h=(~w,~w,~w,~w)~n',
                [C5, S5, V5, H0, H1, NS, NR]) )),
halt" -t "halt(1)" 2>&1
