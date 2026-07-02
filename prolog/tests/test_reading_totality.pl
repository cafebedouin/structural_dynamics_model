% ============================================================================
% test_reading_totality.pl — OQ-137 registry-driven totality suite.
%
% Generalizes tests/test_seat_totality.pl (kept as-is: per-predicate boundary
% fixtures live there) into a REGISTRY-DRIVEN check: every
% reading_registry:aggregatable_reading/3 entry classed total_on_domain is
% proven EXACTLY-ONE-SOLUTION over its declared domain — totality (never fails
% on-domain) and determinism (never over-counts in an aggregate) in one pass.
%
% TWO POSITIVE CONTROLS (mandatory — without (b), "found no silent failures"
% is an unfalsified diagnostic):
%   (a) the known-total family passes (the registry seed: q6_crosscheck/3,
%       extraction_state/2, consensus_provenance/2, seat_perceived_vs_real/4,
%       constraint_signature/2);
%   (b) a deliberately-silent test-local stub (tst_silent_reading/2, which
%       FAILS on one in-domain member) is FLAGGED by the same checker — and
%       flagged AT the planted hole, not merely nonempty.
%
% Vacuity guards (Pattern 5): the registry must be non-empty, the corpus must
% be loaded (N>0), and the census source enumeration must be non-empty —
% each forall here would pass vacuously on an empty table.
%
% ANTI-FORK BRIDGE (Pattern 2): every commentary_census:commentary_source/1
% has a census_source_backing/2 entry that is registered total_on_domain, so
% a census source can never silently escape the totality registry.
%
% Run: cd prolog && swipl -g "[stack], [reading_registry], [commentary_census], \
%   corpus_loader:load_all_testsets, [tests/test_reading_totality], \
%   run_tests(reading_totality), halt" -t "halt(1)"
% ============================================================================

% ----------------------------------------------------------------------------
% Generic checker — entry_violations(+Module:Pred/Arity, +DomainSpec, -Violations)
%   Violations = [KeyArgs-SolutionCount, ...] for every domain key whose
%   solution count ≠ 1. [] iff the reading is total AND deterministic on the
%   declared domain.
% ----------------------------------------------------------------------------
entry_violations(M:P/A, DomainSpec, Violations) :-
    findall(Key-Cnt,
            ( reading_registry:reading_domain_key(DomainSpec, Key),
              reading_solution_count(M, P, A, Key, Cnt),
              Cnt =\= 1 ),
            Violations).

reading_solution_count(M, P, A, KeyArgs, Cnt) :-
    length(KeyArgs, K),
    Extra is A - K,
    length(Outs, Extra),
    append(KeyArgs, Outs, Args),
    Goal =.. [P|Args],
    findall(x, M:Goal, Xs),
    length(Xs, Cnt).

% ----------------------------------------------------------------------------
% Deliberately-silent stub (positive control (b)) — total everywhere EXCEPT
% the planted hole (asserted in setup as the first corpus constraint), i.e.
% exactly the silently-failing-defect shape the suite exists to catch. It is
% NOT in the global registry (else control (a) would fail); the control test
% hands its entry term to the checker directly.
% ----------------------------------------------------------------------------
:- dynamic tst_silent_hole/1.

tst_silent_reading(C, tst_ok) :-
    \+ tst_silent_hole(C).

setup_reading_totality :-
    corpus_loader:ensure_corpus_loaded,
    retractall(tst_silent_hole(_)),
    once(corpus_loader:corpus_constraint(H)),
    assertz(tst_silent_hole(H)).

cleanup_reading_totality :-
    retractall(tst_silent_hole(_)).

:- begin_tests(reading_totality,
               [setup(setup_reading_totality), cleanup(cleanup_reading_totality)]).

% --- vacuity guards (each forall below is vacuous on an empty table) --------
test(corpus_loaded_nonempty) :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    N > 0.

test(registry_nonempty) :-
    aggregate_all(count,
                  reading_registry:aggregatable_reading(_, _, total_on_domain), N),
    N >= 5.

test(census_sources_nonempty) :-
    aggregate_all(count, commentary_census:commentary_source(_), N),
    N >= 2.

% --- (a) every registered total_on_domain reading is exactly-one on-domain --
test(registry_totality_all_entries) :-
    forall(reading_registry:aggregatable_reading(MPA, Dom, total_on_domain),
           ( entry_violations(MPA, Dom, Vs),
             (   Vs == []
             ->  true
             ;   print_message(error,
                     format('reading_totality: ~w violates exactly-one on ~w at ~w',
                            [MPA, Dom, Vs])),
                 fail
             ) )).

% --- (b) the checker FLAGS a planted silent failure, at the planted hole ----
test(control_silent_stub_flagged_at_hole) :-
    tst_silent_hole(H),
    entry_violations(user:tst_silent_reading/2, corpus_constraint, Vs),
    Vs == [[H]-0].

% ...and the same stub with the hole retracted passes — the flag above is the
% HOLE being seen, not the checker rejecting stubs wholesale (two-sided).
test(control_silent_stub_clean_when_hole_closed, [
        setup(( tst_silent_hole(H0), retractall(tst_silent_hole(_)),
                assertz(tst_saved_hole(H0)) )),
        cleanup(( tst_saved_hole(H0), retractall(tst_saved_hole(_)),
                  assertz(tst_silent_hole(H0)) ))]) :-
    entry_violations(user:tst_silent_reading/2, corpus_constraint, Vs),
    Vs == [].

% --- anti-fork bridge: census sources can never escape the registry ---------
test(census_sources_all_registered) :-
    forall(commentary_census:commentary_source(S),
           ( reading_registry:census_source_backing(S, MPA),
             reading_registry:aggregatable_reading(MPA, _, total_on_domain) )).

% --- partial_by_design entries carry a stated reason (declared, not silent) --
test(partial_by_design_reasons_stated) :-
    forall(reading_registry:aggregatable_reading(_, _, partial_by_design(R)),
           ( atom(R), R \== '' )).

% --- boundary: seat domain off-domain silence is the domain, not a defect ---
test(seat_offdomain_has_no_reading) :-
    \+ stakeholder_seats:seat_perceived_vs_real(tst_rt_no_such_c, tst_rt_nobody, _, _).

% --- boundary: constraint_signature's honest-abstain fallback is reachable ---
% (an id with NO authored metrics reaches the explicit `unknown` token — the
% never-fail template the registry class asserts; signature_detection.pl:136)
test(signature_unknown_fallback_reachable) :-
    signature_detection:constraint_signature(tst_rt_unauthored, unknown).

:- end_tests(reading_totality).

:- dynamic tst_saved_hole/1.
