% ============================================================================
% test_empty_chair.pl — OQ-151 empty-chair detector suite.
%
% Sibling of tests/test_h1_stakeholder_spectrum.pl: proves the typed
% refinement of the mcc candidate set (stakeholder_seats:empty_chair_state/2)
% — the 8-token partition, dissent-wins multi-chair semantics, the
% excluded_untyped fail-open (the 4/5-false-positive trap of the retired
% probe_mc_cases prototype, which never is_real_type-filtered the chair set),
% the anti-fork mirror with consensus_provenance/2's Excl list, and live
% refinement coherence against (consensus verdict × stakeholder H¹).
%
% FIXTURE RECIPES (probed this session, 2026-08-09 — χ-config-sensitive, so
% every fixture VERIFIES its precondition in-test; a config change that moves
% a recipe's type fails loudly at the precondition, not silently downstream):
%   metrics_hi (eps .8/supp .7):
%     agenda_setter/institutional/generational/arbitrage/national -> rope
%     victim/powerless/generational/arbitrage/national            -> snare
%     excluded/institutional/generational/arbitrage/national      -> snare
%     excluded/not_a_power/...                                    -> unknown
%     excluded + stakeholder_d_override 0.09 (the boss d)         -> rope
%   metrics_lo (eps .2/supp .1):
%     agenda_setter/institutional/generational/arbitrage/national -> rope
%     excluded/institutional/generational/arbitrage/national      -> rope
%
% D7 (cache discipline): every fixture-mutating test clears
% cache_registry:clear_all_caches in BOTH setup and cleanup.
%
% Run: cd prolog && swipl -g "[stack], [tests/test_empty_chair], \
%   run_tests(empty_chair), halt" -t "halt(1)"
% (scoped run_tests — a bare run_tests also sweeps story-embedded plunit
% units inside prolog/testsets/*.pl, which carry pre-existing unrelated
% failures; see the Step-0 baseline note, 2026-08-09.)
% ============================================================================

:- multifile
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2.

:- begin_tests(empty_chair).

% ---- fixtures ----------------------------------------------------------------
fixture_metrics_hi(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.8)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.7)).
fixture_metrics_lo(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.2)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.1)).
fixture_rope_seat(C, N) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, agenda_setter,
        institutional, generational, arbitrage, national)).
fixture_snare_seat(C, N) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, victim,
        powerless, generational, arbitrage, national)).
fixture_unknown_agent(C, N) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, payer,
        not_a_power, generational, arbitrage, national)).
fixture_chair(C, N) :-                       % type follows the metric level
    assertz(narrative_ontology:constraint_stakeholder(C, N, excluded,
        institutional, generational, arbitrage, national)).
fixture_unknown_chair(C, N) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, excluded,
        not_a_power, generational, arbitrage, national)).
% Concurring chair in a metrics_hi story: the room's own d (0.09, probed
% 2026-08-09) via the designed per-(C,Name) probe surface — verified in-test
% against the ACTUAL boss d, not the memorized constant.
fixture_override_chair(C, N) :-
    fixture_chair(C, N).                     % override asserted in the test body

teardown_fixture(C) :-
    retractall(narrative_ontology:constraint_metric(C, _, _)),
    retractall(narrative_ontology:constraint_stakeholder(C, _, _, _, _, _, _)),
    retractall(narrative_ontology:stakeholder_non_agent(C, _)),
    retractall(stakeholder_seats:stakeholder_d_override(C, _, _)),
    cache_registry:clear_all_caches.         % D7: clear on teardown too

% real_chair_type(+C, +N, -T): precondition helper — chair derives a REAL type.
real_chair_type(C, N, T) :-
    stakeholder_seats:dr_type_for_stakeholder(C, N, T),
    grothendieck_cohomology:is_real_type(T).

% ---- 1. vacuity guards (Pattern 5) ------------------------------------------
test(vacuity_guards) :-
    corpus_loader:ensure_corpus_loaded,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    NC > 0,
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          stakeholder_seats:excluded_seat_names(C, Ns), Ns \= [] ),
        NChair),
    NChair > 0.

% ---- 2. positive control: typed dissenting chair fires ----------------------
% 2 rope seats + naturally-snare excluded chair -> empty_chair_dissent, and
% the coherence triple: consensus = mcc, obstruction = (1, 0, 2, 2).
test(dissent_positive_control,
     [ setup(( fixture_metrics_hi(tec_dissent),
               fixture_rope_seat(tec_dissent, boss),
               fixture_rope_seat(tec_dissent, boss2),
               fixture_chair(tec_dissent, chair),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_dissent)) ]) :-
    % preconditions: room unanimous rope, chair derives a DIFFERENT real type
    stakeholder_seats:dr_type_for_stakeholder(tec_dissent, boss, rope),
    real_chair_type(tec_dissent, chair, CT),
    CT \== rope,
    stakeholder_seats:empty_chair_state(tec_dissent,
        empty_chair_dissent(rope, [CT], [chair])),
    stakeholder_seats:consensus_provenance(tec_dissent,
        manufactured_consensus_candidate([chair])),
    stakeholder_seats:stakeholder_obstruction(tec_dissent, 1, 0, 2, 2).

% ---- 3. false-positive regression (the 4/5 trap — load-bearing) -------------
% An excluded seat deriving LITERAL `unknown` must land in excluded_untyped.
% The state is bound ONCE and checked by FUNCTOR EQUALITY: the predicate is
% total-deterministic, so functor equality excludes BOTH dissent functors — a
% bare \+ empty_chair_dissent(_,_,_) would miss the _untypeable variant, the
% MORE likely false-positive route.
test(untyped_chair_never_dissents,
     [ setup(( fixture_metrics_hi(tec_untyped),
               fixture_rope_seat(tec_untyped, boss),
               fixture_rope_seat(tec_untyped, boss2),
               fixture_unknown_chair(tec_untyped, ghost),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_untyped)) ]) :-
    % precondition: the chair derives the LITERAL unknown token (verified)
    stakeholder_seats:dr_type_for_stakeholder(tec_untyped, ghost, unknown),
    stakeholder_seats:empty_chair_state(tec_untyped, State),
    functor(State, F, _),
    F == excluded_untyped,
    State = excluded_untyped(ExNames),
    ExNames == [ghost].

% ---- 4. negative control: concurring chair does not fire --------------------
test(concurring_chair_negative_control,
     [ setup(( fixture_metrics_lo(tec_concur),
               fixture_rope_seat(tec_concur, boss),
               fixture_rope_seat(tec_concur, boss2),
               fixture_chair(tec_concur, chair),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_concur)) ]) :-
    % preconditions: room rope, chair derives the SAME real type
    stakeholder_seats:dr_type_for_stakeholder(tec_concur, boss, rope),
    real_chair_type(tec_concur, chair, rope),
    stakeholder_seats:empty_chair_state(tec_concur, excluded_concurs(rope)).

% ---- 5. included_plural: the room already disagrees -> no fire --------------
test(included_plural_no_fire,
     [ setup(( fixture_metrics_hi(tec_plural),
               fixture_rope_seat(tec_plural, boss),
               fixture_snare_seat(tec_plural, worker),
               fixture_chair(tec_plural, chair),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_plural)) ]) :-
    % preconditions: two DISTINCT real types in the room
    stakeholder_seats:dr_type_for_stakeholder(tec_plural, boss, T1),
    stakeholder_seats:dr_type_for_stakeholder(tec_plural, worker, T2),
    T1 \== T2,
    grothendieck_cohomology:is_real_type(T1),
    grothendieck_cohomology:is_real_type(T2),
    msort([T1, T2], Ts),
    stakeholder_seats:empty_chair_state(tec_plural, included_plural(Ts)).

% ---- 6. out-of-domain fail-open: no chair -> no_excluded_seat ---------------
test(no_chair_fail_open,
     [ setup(( fixture_metrics_hi(tec_nochair),
               fixture_rope_seat(tec_nochair, boss),
               fixture_rope_seat(tec_nochair, boss2),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_nochair)) ]) :-
    stakeholder_seats:empty_chair_state(tec_nochair, no_excluded_seat),
    % zero-fact story: same token (fail-open, never an error)
    stakeholder_seats:empty_chair_state(tec_no_such_story, no_excluded_seat).

% ---- 7. included_insufficient boundary: 1 real seat + TYPED chair -----------
% The room checks precede chair typing (order is load-bearing): a typed,
% even dissenting-looking chair beside an insufficient room reports the ROOM.
test(included_insufficient_boundary,
     [ setup(( fixture_metrics_hi(tec_insuf),
               fixture_rope_seat(tec_insuf, boss),
               fixture_chair(tec_insuf, chair),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_insuf)) ]) :-
    % preconditions: exactly 1 real included seat; chair IS typed
    stakeholder_seats:stakeholder_obstruction(tec_insuf, _, _, 1, 1),
    real_chair_type(tec_insuf, chair, _),
    stakeholder_seats:empty_chair_state(tec_insuf, included_insufficient).

% ---- 8a. dissent _untypeable: unknown agent seat beside case 2 --------------
test(dissent_untypeable_variant,
     [ setup(( fixture_metrics_hi(tec_dis_u),
               fixture_rope_seat(tec_dis_u, boss),
               fixture_rope_seat(tec_dis_u, boss2),
               fixture_unknown_agent(tec_dis_u, vp),
               fixture_chair(tec_dis_u, chair),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_dis_u)) ]) :-
    stakeholder_seats:dr_type_for_stakeholder(tec_dis_u, vp, unknown),
    real_chair_type(tec_dis_u, chair, CT),
    CT \== rope,
    stakeholder_seats:empty_chair_state(tec_dis_u,
        empty_chair_dissent_untypeable(rope, [CT], [chair])),
    stakeholder_seats:consensus_provenance(tec_dis_u,
        manufactured_consensus_candidate_untypeable([chair])).

% ---- 8b. concurs _untypeable: unknown agent seat beside case 4 --------------
test(concurs_untypeable_variant,
     [ setup(( fixture_metrics_lo(tec_con_u),
               fixture_rope_seat(tec_con_u, boss),
               fixture_rope_seat(tec_con_u, boss2),
               fixture_unknown_agent(tec_con_u, vp),
               fixture_chair(tec_con_u, chair),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_con_u)) ]) :-
    stakeholder_seats:dr_type_for_stakeholder(tec_con_u, vp, unknown),
    real_chair_type(tec_con_u, chair, rope),
    stakeholder_seats:empty_chair_state(tec_con_u,
        excluded_concurs_untypeable(rope)).

% ---- 8c. partial chair set: typed chair drives, NOT excluded_untyped --------
% One unknown-deriving chair beside one typed chair — untyped chairs are
% DROPPED from the dissent computation; the typed one drives the verdict
% (amendment 3: excluded_untyped fires iff NO chair derives real).
test(partial_chair_typed_drives,
     [ setup(( fixture_metrics_hi(tec_partial),
               fixture_rope_seat(tec_partial, boss),
               fixture_rope_seat(tec_partial, boss2),
               fixture_unknown_chair(tec_partial, ghost),
               fixture_chair(tec_partial, chair),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_partial)) ]) :-
    stakeholder_seats:dr_type_for_stakeholder(tec_partial, ghost, unknown),
    real_chair_type(tec_partial, chair, CT),
    CT \== rope,
    stakeholder_seats:empty_chair_state(tec_partial, State),
    State = empty_chair_dissent(rope, [CT], TypedNames),
    TypedNames == [chair].                   % ghost dropped, never excluded_untyped

% ---- 9. mcc_excl_mirror: anti-fork pin against consensus_provenance ---------
% forall corpus C: every mcc verdict's Excl list == excluded_seat_names/2
% EXACTLY (same findall, same order — the mirror is a pin, not an extraction;
% consensus_provenance/2 is frozen under the OQ-217 biconditional).
test(mcc_excl_mirror) :-
    corpus_loader:ensure_corpus_loaded,
    % vacuity guard: the corpus must carry mcc verdicts for this to pin
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C0),
          stakeholder_seats:consensus_provenance(C0, V0),
          ( V0 = manufactured_consensus_candidate(_)
          ; V0 = manufactured_consensus_candidate_untypeable(_) ) ),
        NMcc),
    NMcc > 0,
    forall(( corpus_loader:corpus_constraint(C),
             stakeholder_seats:consensus_provenance(C, V),
             ( V = manufactured_consensus_candidate(Excl)
             ; V = manufactured_consensus_candidate_untypeable(Excl) ) ),
           ( stakeholder_seats:excluded_seat_names(C, Ns),
             (   Ns == Excl
             ->  true
             ;   throw(mcc_excl_fork(C, mcc(Excl), mirror(Ns)))
             ) )).

% ---- 10/11. live refinement coherence + negative control on the checker -----
% (empty_chair functor × consensus functor × H¹ stratum) must land in the
% explicit allowed-triple table on every corpus constraint; the checker throws
% the offender. The negative control then removes each LIVE triple from the
% table and proves the same checker throws — discriminating, not vacuous.
%
% Full 16-row table (dispatch-order derivation in empty_chair_state/2's
% header): no_excluded_seat pairs with every non-mcc consensus verdict; the
% chair-present states refine EXACTLY the mcc split (plain <-> plain,
% _untypeable <-> _untypeable); room-insufficiency covers the three null-H¹
% consensus verdicts; included_plural is the plural cell.
allowed_triple(no_excluded_seat,       unanimous_no_excluded_seats,      zero).
allowed_triple(no_excluded_seat,       unanimous_with_untypeable_seats,  zero).
allowed_triple(no_excluded_seat,       plural,                           pos).
allowed_triple(no_excluded_seat,       insufficient_real_seats,          null).
allowed_triple(no_excluded_seat,       seats_untyped,                    null).
allowed_triple(no_excluded_seat,       no_agent_seats,                   null).
allowed_triple(included_insufficient,  insufficient_real_seats,          null).
allowed_triple(included_insufficient,  seats_untyped,                    null).
allowed_triple(included_insufficient,  no_agent_seats,                   null).
allowed_triple(included_plural,        plural,                           pos).
allowed_triple(excluded_untyped,       manufactured_consensus_candidate,            zero).
allowed_triple(excluded_untyped,       manufactured_consensus_candidate_untypeable, zero).
allowed_triple(excluded_concurs,       manufactured_consensus_candidate,            zero).
allowed_triple(excluded_concurs_untypeable, manufactured_consensus_candidate_untypeable, zero).
allowed_triple(empty_chair_dissent,    manufactured_consensus_candidate,            zero).
allowed_triple(empty_chair_dissent_untypeable, manufactured_consensus_candidate_untypeable, zero).

triple_table(Table) :- findall(t(E, V, S), allowed_triple(E, V, S), Table).

h1_stratum(H1, S) :-
    (   H1 == null -> S = null
    ;   H1 =:= 0   -> S = zero
    ;   S = pos
    ).

live_triple(C, EF, CF, S) :-
    stakeholder_seats:empty_chair_state(C, E),  functor(E, EF, _),
    stakeholder_seats:consensus_provenance(C, V), functor(V, CF, _),
    stakeholder_seats:stakeholder_obstruction(C, _, H1, _, _),
    h1_stratum(H1, S).

check_triple_in_table(C, EF, CF, S, Table) :-
    (   memberchk(t(EF, CF, S), Table)
    ->  true
    ;   throw(refinement_incoherence(C, empty_chair(EF), consensus(CF), h1(S)))
    ).

test(live_refinement_coherence) :-
    corpus_loader:ensure_corpus_loaded,
    triple_table(Table),
    forall(corpus_loader:corpus_constraint(C),
           ( live_triple(C, EF, CF, S),
             check_triple_in_table(C, EF, CF, S, Table) )).

test(negative_control_refinement_checker) :-
    corpus_loader:ensure_corpus_loaded,
    triple_table(Table),
    findall(t(EF, CF, S),
            ( corpus_loader:corpus_constraint(C), live_triple(C, EF, CF, S) ),
            Ls0),
    sort(Ls0, LiveTriples),
    LiveTriples \== [],                       % control must have something to flag
    forall(member(t(EF, CF, S), LiveTriples),
           ( selectchk(t(EF, CF, S), Table, Perturbed),
             once(( corpus_loader:corpus_constraint(C2),
                    live_triple(C2, EF, CF, S) )),
             catch(( check_triple_in_table(C2, EF, CF, S, Perturbed),
                     throw(checker_blind(t(EF, CF, S)))
                   ),
                   refinement_incoherence(_, _, _, _),
                   true) )).

% ---- 12. mixed chairs: dissent wins, note-2 asymmetry pinned ----------------
% Two TYPED chairs, one concurring (room-d override via the designed probe
% surface) + one dissenting -> empty_chair_dissent (dissent wins), BOTH typed
% names in AllTypedExNames, ONLY the dissenting type in DissentTypes.
test(mixed_chairs_dissent_wins,
     [ setup(( fixture_metrics_hi(tec_mixed),
               fixture_rope_seat(tec_mixed, boss),
               fixture_rope_seat(tec_mixed, boss2),
               fixture_override_chair(tec_mixed, chair_conc),
               fixture_chair(tec_mixed, chair_diss),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tec_mixed)) ]) :-
    % give the concurring chair the ROOM's own d (verified, not memorized)
    stakeholder_seats:derive_directionality_for_stakeholder(tec_mixed, boss, DB),
    assertz(stakeholder_seats:stakeholder_d_override(tec_mixed, chair_conc, DB)),
    cache_registry:clear_all_caches,
    % preconditions: chair_conc concurs (rope), chair_diss dissents (real, non-rope)
    real_chair_type(tec_mixed, chair_conc, rope),
    real_chair_type(tec_mixed, chair_diss, CT),
    CT \== rope,
    stakeholder_seats:empty_chair_state(tec_mixed,
        empty_chair_dissent(rope, DissentTypes, AllTypedExNames)),
    DissentTypes == [CT],                     % dissent-filtered: concurring type absent
    msort(AllTypedExNames, Sorted),
    Sorted == [chair_conc, chair_diss].       % ... but BOTH typed names present

:- end_tests(empty_chair).
