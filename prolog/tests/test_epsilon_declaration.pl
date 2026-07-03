% ============================================================================
% test_epsilon_declaration.pl — OQ-205 ε-declaration fail-fast gate suite.
%
% Run by run_pipeline.py's _prolog_epsilon_declaration_gate() as a sequential
% fail-fast step beside the OQ-137 reading-totality gate — this suite is the
% ENFORCEMENT for the spec §3 fail-closed provenance rule (the
% data_validation checkers it consumes are WARN-only at the orchestrator's
% _prolog_validation step, run_pipeline.py:422-438).
%
% GATE-RED conditions:
%   (a) any three-site drift (data_validation:epsilon_provenance_drift/2 —
%       the SAME checker validate_all consumes; no fork);
%   (b) an emission-totality breach: an epsilon_provenance fact on a story
%       with NO resolvable ε (orphan provenance — an emission bug), or a
%       census bucket that fails to sum to the corpus (a constraint whose ε
%       authoring form escapes every bucket definition);
%   (c) fixture-control failure (Control P, tests/fixtures/eps_controls/ —
%       run as a SECOND swipl inside the gate; see the gate docstring).
% NOT gate-red: missing provenance on pre-build stories (the loud-null
% stratum, warning-grade by operator ruling 2026-07-03).
%
% VACUITY GUARDS (Pattern 5): on the pre-build corpus the drift check's
% domain is EMPTY (no story carries epsilon_provenance), so the planted
% in-memory controls below are what prove this gate WOULD fire — a clean run
% without them would be indistinguishable from a gate that never looked.
%
% Run: cd prolog && swipl -g "[stack], [data_validation], \
%   corpus_loader:load_all_testsets, [tests/test_epsilon_declaration], \
%   run_tests(epsilon_declaration), halt" -t "halt(1)"
% ============================================================================

% --- ε-site probes (the three-site fork, spec §3) ---------------------------
eps_site_metric(C) :- narrative_ontology:constraint_metric(C, extractiveness, _).
eps_site_direct(C) :- drl_core:base_extractiveness(C, _).
eps_site_priors(C) :- domain_priors:base_extractiveness(C, _).
eps_resolvable(C)  :-
    ( eps_site_metric(C) ; eps_site_direct(C) ; eps_site_priors(C) ), !.

% --- planted-control carrier (setup picks a real with-ε corpus story) ------
:- dynamic tst_eps_carrier/1.

setup_eps_declaration :-
    corpus_loader:ensure_corpus_loaded,
    retractall(tst_eps_carrier(_)),
    once(( corpus_loader:corpus_constraint(C), eps_site_metric(C) )),
    assertz(tst_eps_carrier(C)).

cleanup_eps_declaration :-
    ( tst_eps_carrier(C)
    -> retractall(narrative_ontology:epsilon_provenance(C, _, _, _, _))
    ;  true ),
    retractall(narrative_ontology:epsilon_provenance(tst_eps_orphan_ctl, _, _, _, _)),
    retractall(tst_eps_carrier(_)).

:- begin_tests(epsilon_declaration,
               [setup(setup_eps_declaration), cleanup(cleanup_eps_declaration)]).

% --- vacuity guards ----------------------------------------------------------
test(corpus_loaded_nonempty) :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    N > 0.

test(carrier_selected) :-
    tst_eps_carrier(_).

% --- census partition: provenance / loud-null / no-ε, printed and summing ---
% The printed counts are the standing census surface; the sum catches a
% constraint escaping every bucket (each bucket has an independent witness:
% the fact, the data_validation checker, the three-site probe).
test(census_partition_sums_to_corpus) :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    findall(C1, ( corpus_loader:corpus_constraint(C1),
                  narrative_ontology:epsilon_provenance(C1, _, _, _, _) ), Ps0),
    sort(Ps0, Ps), length(Ps, NProv),
    findall(C2, ( corpus_loader:corpus_constraint(C2),
                  data_validation:missing_epsilon_provenance(C2) ), Ls0),
    sort(Ls0, Ls), length(Ls, NLoud),
    findall(C3, ( corpus_loader:corpus_constraint(C3),
                  \+ narrative_ontology:epsilon_provenance(C3, _, _, _, _),
                  \+ eps_resolvable(C3) ), Ns0),
    sort(Ns0, Ns), length(Ns, NNo),
    print_message(informational,
        format('epsilon_declaration census: n=~w provenanced=~w loud_null=~w no_epsilon=~w',
               [N, NProv, NLoud, NNo])),
    Sum is NProv + NLoud + NNo,
    (   Sum =:= N
    ->  true
    ;   print_message(error,
            format('epsilon_declaration: census buckets sum ~w != corpus ~w',
                   [Sum, N])),
        fail
    ).

% --- (a) no three-site drift on the loaded corpus ---------------------------
test(no_epsilon_provenance_drift) :-
    findall(C-D, ( corpus_loader:corpus_constraint(C),
                   data_validation:epsilon_provenance_drift(C, D) ), Vs),
    (   Vs == []
    ->  true
    ;   print_message(error,
            format('epsilon_declaration: three-site drift at ~w', [Vs])),
        fail
    ).

% --- (b) no orphan provenance (fact without a resolvable ε = emission bug) --
test(no_orphan_provenance) :-
    findall(C, ( narrative_ontology:epsilon_provenance(C, _, _, _, _),
                 \+ eps_resolvable(C) ), Os),
    (   Os == []
    ->  true
    ;   print_message(error,
            format('epsilon_declaration: orphan epsilon_provenance at ~w', [Os])),
        fail
    ).

% --- POSITIVE CONTROLS (the non-vacuity of (a) and the census) --------------
% planted drift IS caught, at the planted constraint
test(control_planted_drift_caught, [
        setup(( tst_eps_carrier(C),
                once(narrative_ontology:constraint_metric(C, extractiveness, E)),
                Bad is E + 0.2,
                assertz(narrative_ontology:epsilon_provenance(C, Bad, tst, tst, direct)) )),
        cleanup(( tst_eps_carrier(C),
                  retractall(narrative_ontology:epsilon_provenance(C, _, _, _, _)) ))]) :-
    tst_eps_carrier(C),
    data_validation:epsilon_provenance_drift(C, _).

% two-sided: an equal-value plant is flag-free AND leaves the loud-null stratum
test(control_planted_clean_flagfree, [
        setup(( tst_eps_carrier(C),
                once(narrative_ontology:constraint_metric(C, extractiveness, E)),
                assertz(narrative_ontology:epsilon_provenance(C, E, tst, tst, direct)) )),
        cleanup(( tst_eps_carrier(C),
                  retractall(narrative_ontology:epsilon_provenance(C, _, _, _, _)) ))]) :-
    tst_eps_carrier(C),
    \+ data_validation:epsilon_provenance_drift(C, _),
    \+ data_validation:missing_epsilon_provenance(C).

% planted orphan IS caught by the (b) check body
test(control_planted_orphan_caught, [
        setup(assertz(narrative_ontology:epsilon_provenance(
                          tst_eps_orphan_ctl, 0.5, tst, tst, direct))),
        cleanup(retractall(narrative_ontology:epsilon_provenance(
                               tst_eps_orphan_ctl, _, _, _, _)))]) :-
    findall(C, ( narrative_ontology:epsilon_provenance(C, _, _, _, _),
                 \+ eps_resolvable(C) ), Os),
    Os == [tst_eps_orphan_ctl].

% the carrier (pre-build, no provenance) is counted AT itself in the census
test(control_loud_null_at_carrier) :-
    tst_eps_carrier(C),
    once(data_validation:missing_epsilon_provenance(C)).

:- end_tests(epsilon_declaration).
