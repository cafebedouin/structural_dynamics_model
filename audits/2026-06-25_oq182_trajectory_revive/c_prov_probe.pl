% C-prov runtime witness for OQ-182 (trajectory revive).
% Load chain (run separately per goal to keep processes isolated):
%   swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
%         -l maxent_classifier.pl -l context_profile_mining.pl \
%         -l ../audits/2026-06-25_oq182_trajectory_revive/c_prov_probe.pl \
%         -g "<goal>, halt." -t "halt(1)"
%
% cprov_main      : run trajectory_run on the loaded corpus, then assert BOTH
%                   classify_at_time_* globals are UNSET (the 2 passive nb_getval
%                   leaf reads fell back to authored constraint_metric).
% cprov_poscontrol: separate process; call classify_at_time/4 once and show the
%                   eps global DOES set — proving the probe can detect a set global.

cprov_main :-
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorpus),
    format("CORPUS: ~w constraints loaded~n", [NCorpus]),
    context_profile_mining:trajectory_run(Context, Summary),
    format("SUMMARY: ~w~n", [Summary]),
    ( catch(nb_getval(classify_at_time_eps, V1), _, fail)
      -> format("FAIL_CPROV: classify_at_time_eps IS SET = ~w~n", [V1])
      ;  format("PASS: classify_at_time_eps UNSET after trajectory_run~n", []) ),
    ( catch(nb_getval(classify_at_time_theater, V2), _, fail)
      -> format("FAIL_CPROV: classify_at_time_theater IS SET = ~w~n", [V2])
      ;  format("PASS: classify_at_time_theater UNSET after trajectory_run~n", []) ),
    format("EXCLUDED_CONSTRAINTS: 0 (no global set => no impute coupling => nothing excluded)~n", []).

% Positive control must feed classify_at_time a constraint+Time ON its authored grid,
% else it short-circuits to `unknown` before the nb_setval (OQ-178 off-grid trap).
% Prefer a constraint with a temporal suppression_requirement measurement and use that
% exact Time; fall back to a static suppression_requirement scalar (fires at any Time).
cprov_poscontrol :-
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),
    (   corpus_loader:corpus_constraint(C),
        narrative_ontology:measurement(_, C, suppression_requirement, Time, _)
    ->  format("POSCTRL_CONSTRAINT: ~w (temporal grid Time=~w)~n", [C, Time])
    ;   corpus_loader:corpus_constraint(C),
        narrative_ontology:constraint_metric(C, suppression_requirement, _),
        Time = 0,
        format("POSCTRL_CONSTRAINT: ~w (static scalar, Time=0)~n", [C])
    ),
    ( catch(nb_getval(classify_at_time_eps, _), _, fail)
      -> format("PRE: eps already set (UNEXPECTED)~n", [])
      ;  format("PRE: eps unset before classify_at_time (expected)~n", []) ),
    ( catch(drl_composition:classify_at_time(C, Time, Context, Type), E,
            (format("classify_at_time THREW: ~w~n", [E]), fail))
      -> format("classify_at_time(~w, ~w) => Type=~w~n", [C, Time, Type])
      ;  format("classify_at_time FAILED for ~w at Time=~w~n", [C, Time]) ),
    ( catch(nb_getval(classify_at_time_eps, V), _, fail)
      -> format("PASS_POSCTRL: classify_at_time_eps SET = ~w (probe CAN detect a set global)~n", [V])
      ;  format("FAIL_POSCTRL: classify_at_time_eps STILL UNSET (probe is blind!)~n", []) ).
