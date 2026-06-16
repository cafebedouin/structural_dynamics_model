% ============================================================================
% test_seat_totality.pl — OQ-121 totalization of the partial-silent commentary
% predicates consensus_provenance/2 and seat_perceived_vs_real/4.
%
% Locks the never-fail convention (the constraint_signature/2 + q6_cell/2
% discipline): a commentary reading must return an EXPLICIT token rather than
% fail silently, so an aggregate read site can distinguish the boundary cases a
% bare failure used to swallow.
%
%   consensus_provenance/2 : TOTAL over ALL constraints. no_agent_seats (no
%     non-excluded agent seat — out-of-domain) and seats_untyped (seats present,
%     none typed — absence) are explicit, distinct from the genuine verdicts.
%   seat_perceived_vs_real/4 : TOTAL over EXISTING (C,Name) seats; Computed =
%     untyped when the per-seat type cannot be derived (explicit absence). A
%     NON-existent seat correctly has no reading (that is the domain, not silence).
%
% Run: cd prolog && swipl -g "[stack], corpus_loader:load_all_testsets, \
%   [tests/test_seat_totality], run_tests(seat_totality), halt" -t "halt(1)"
% ============================================================================

:- multifile
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2.

fixture_id(tst_no_seats).
fixture_id(tst_excluded_only).
fixture_id(tst_agent_seat).

setup_fixtures :-
    % a constraint with metrics but NO stakeholder seats at all
    assertz(narrative_ontology:constraint_metric(tst_no_seats, extractiveness, 0.5)),
    % a constraint whose only seats are EXCLUDED (no non-excluded agent seat)
    assertz(narrative_ontology:constraint_stakeholder(tst_excluded_only, ghost, excluded,
        institutional, generational, arbitrage, national)),
    % a constraint with one real agent seat (for the seat-level total test)
    assertz(narrative_ontology:constraint_stakeholder(tst_agent_seat, boss, agenda_setter,
        institutional, generational, arbitrage, national)).

teardown_fixtures :-
    forall(fixture_id(C),
           ( retractall(narrative_ontology:constraint_metric(C, _, _)),
             retractall(narrative_ontology:constraint_stakeholder(C, _, _, _, _, _, _)) )).

:- begin_tests(seat_totality, [setup(setup_fixtures), cleanup(teardown_fixtures)]).

% --- consensus_provenance/2 is TOTAL over the whole corpus -------------------
test(consensus_total_over_corpus) :-
    corpus_loader:ensure_corpus_loaded,
    forall(corpus_loader:corpus_constraint(C),
           stakeholder_seats:consensus_provenance(C, _)).

% exactly one solution per constraint (deterministic verdict)
test(consensus_deterministic) :-
    findall(V, stakeholder_seats:consensus_provenance(tst_agent_seat, V), Vs),
    Vs = [_].

% boundary: no non-excluded agent seat -> explicit no_agent_seats, NOT a failure
test(consensus_no_seats_is_explicit) :-
    stakeholder_seats:consensus_provenance(tst_no_seats, no_agent_seats).
test(consensus_excluded_only_is_no_agent_seats) :-
    stakeholder_seats:consensus_provenance(tst_excluded_only, no_agent_seats).

% a real agent seat -> a genuine verdict, never the boundary token
test(consensus_agent_seat_genuine_verdict) :-
    stakeholder_seats:consensus_provenance(tst_agent_seat, V),
    V \== no_agent_seats,
    V \== seats_untyped.

% --- seat_perceived_vs_real/4 is TOTAL over EXISTING seats -------------------
test(seat_total_over_existing_seats) :-
    corpus_loader:ensure_corpus_loaded,
    forall(( corpus_loader:corpus_constraint(C),
             narrative_ontology:constraint_stakeholder(C, N, _, _, _, _, _) ),
           stakeholder_seats:seat_perceived_vs_real(C, N, _, _)).

% the fixture agent seat resolves (Perceived bound, Computed bound)
test(seat_fixture_resolves) :-
    stakeholder_seats:seat_perceived_vs_real(tst_agent_seat, boss, Perceived, Computed),
    memberchk(Perceived, [immutable, changeable]),
    nonvar(Computed).

% domain boundary preserved: a NON-existent seat has no reading (not silence-bug)
test(seat_nonexistent_no_reading) :-
    \+ stakeholder_seats:seat_perceived_vs_real(no_such_c, no_such_n, _, _).

:- end_tests(seat_totality).
