% ============================================================================
% test_h1_stakeholder_spectrum.pl — OQ-207 stakeholder-frame H¹ suite.
%
% Corpus-loaded sibling of the deliberately-pure tests/test_h1_spectrum.pl:
% that suite proves the kernel (obstruction_from_vector/3) realizes the proven
% general-n spectra on SYNTHETIC vectors; this one proves the stakeholder
% WIRING (stakeholder_seats:stakeholder_obstruction/5) — live-corpus spectrum
% validity, coherence with consensus_provenance/2 (EXACT biconditional since
% OQ-217; the pre-tightening D4 divergence cells (a)/(b)/mixed are pinned as
% insufficiency/annotated-unanimity fixtures), and the boundary cases.
%
% The expected-spectrum table is REUSED module-qualified from the sibling
% (plunit_h1_spectrum:expected_spectrum/2) — never forked (Pattern 2).
%
% TWO ABSENCE TOKENS (D2 — never unify): `untyped` is the census-facing token
% (seat_perceived_vs_real/4); `unknown` is the kernel-facing token
% (stakeholder_type_vector/2, filtered by is_real_type/1). An `untyped` leaking
% into the vector would be counted as a REAL DISAGREEING TYPE (is_real_type
% tests \== unknown only) and silently inflate H¹ — test no_untyped_in_vector
% is the positive control on the actual failure path.
%
% D7 (cache discipline): every fixture-mutating test clears
% cache_registry:clear_all_caches in BOTH setup and cleanup — a fixture
% reading a value memoized under the prior fact base fails silently as a
% plausible number.
%
% Run: cd prolog && swipl -g "[stack], [tests/test_h1_spectrum], \
%   [tests/test_h1_stakeholder_spectrum], run_tests, halt" -t "halt(1)"
% ============================================================================

:- ensure_loaded(test_h1_spectrum).   % the sibling's expected_spectrum/2 table

:- multifile
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2.

:- begin_tests(h1_stakeholder_spectrum).

% ---- shared checker (tests 2 and 8 use the SAME comparator) -----------------
% check_h1_in_table(+C, +H1, +NReal, +Table): Table is a list of NReal-Spectrum
% pairs; H1 must be a member of NReal's spectrum. Throws the offender.
check_h1_in_table(C, H1, NReal, Table) :-
    (   NReal > 12
    ->  throw(spectrum_table_exhausted(C, n_real(NReal)))   % table-extension signal
    ;   memberchk(NReal-Spec, Table),
        (   memberchk(H1, Spec)
        ->  true
        ;   throw(spectrum_violation(C, h1(H1), n_real(NReal), expected(Spec)))
        )
    ).

true_table(Table) :-
    findall(N-S, plunit_h1_spectrum:expected_spectrum(N, S), Table).

% live_numbered(-C, -H1, -NReal): corpus constraints whose stakeholder H1 is a
% number (the determinable stratum).
live_numbered(C, H1, NReal) :-
    corpus_loader:corpus_constraint(C),
    stakeholder_seats:stakeholder_obstruction(C, _H0, H1, _NS, NReal),
    number(H1).

% ---- 1. vacuity guards (Pattern 5) ------------------------------------------
% Every corpus forall below is vacuously green on an empty corpus, a seatless
% corpus, or an all-null H1 column — establish the data exists first.
test(vacuity_guards) :-
    corpus_loader:ensure_corpus_loaded,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    NC > 0,
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          stakeholder_seats:stakeholder_obstruction(C, _, _, NS, _), NS > 0 ),
        NSeated),
    NSeated > 0,
    aggregate_all(count, live_numbered(_, _, _), NNum),
    NNum > 0.

% ---- 2. live spectrum validity ----------------------------------------------
% Every determinable H1 on the corpus: 2 =< NReal (number(H1) <-> >=2 real
% seats, the OQ-51 rule two-sided), NReal =< 12 (throw above — the table needs
% extending, not the corpus fixing), H1 in the proven spectrum H(NReal).
test(live_spectrum_validity) :-
    true_table(Table),
    forall(live_numbered(C, H1, NReal),
           (   NReal >= 2
           ->  check_h1_in_table(C, H1, NReal, Table)
           ;   throw(numbered_h1_below_two_real(C, h1(H1), n_real(NReal)))
           )),
    % the null direction of the biconditional: H1 = null <-> NReal < 2
    forall(( corpus_loader:corpus_constraint(C2),
             stakeholder_seats:stakeholder_obstruction(C2, _, H1b, _, NR2),
             H1b == null ),
           (   NR2 < 2
           ->  true
           ;   throw(null_h1_with_enough_real_seats(C2, n_real(NR2)))
           )).

% ---- 3. mcc biconditional coherence (D4 case table, OQ-217 exact) -----------
% consensus_provenance/2 and stakeholder_obstruction/5 must relate EXACTLY as
% the case table permits, per corpus constraint, both directions (the verdicts
% are exhaustive and each names its exact obstruction constraint; any pair
% outside the table throws the offender). Since OQ-217 (operator ruling
% 2026-07-12) both sides compute over the SAME is_real_type/1 domain, so the
% biconditional is EXACT — the former divergence cells (a) (unanimity of one),
% (b) (all-`unknown` unanimity, the fired D4 kill condition) and mixed
% (plural([T,unknown])) all route to insufficient_real_seats / annotated
% unanimity and no longer exist as verdict-vs-H¹ daylight.
test(mcc_biconditional_coherence) :-
    forall(corpus_loader:corpus_constraint(C),
           ( stakeholder_seats:consensus_provenance(C, V),
             stakeholder_seats:stakeholder_obstruction(C, H0, H1, NS, NR),
             (   coherence_case(V, H0, H1, NS, NR)
             ->  true
             ;   throw(mcc_incoherence(C, verdict(V), h0(H0), h1(H1),
                                       n_seats(NS), n_real(NR)))
             ) )).

coherence_case(no_agent_seats, H0, H1, NS, NR) :-
    NS == 0, NR == 0, H0 == null, H1 == null.
coherence_case(seats_untyped, H0, H1, NS, NR) :-
    NS >= 1, NR == 0, H0 == null, H1 == null.
coherence_case(insufficient_real_seats, H0, H1, NS, NR) :-
    NS >= 1, NR < 2, H0 == null, H1 == null.
coherence_case(unanimous_no_excluded_seats, H0, H1, NS, NR) :-
    NR >= 2, NR == NS, H0 == 1, H1 == 0.
coherence_case(unanimous_with_untypeable_seats, H0, H1, NS, NR) :-
    NR >= 2, NR < NS, H0 == 1, H1 == 0.
coherence_case(manufactured_consensus_candidate(_), H0, H1, NS, NR) :-
    NR >= 2, NR == NS, H0 == 1, H1 == 0.
coherence_case(manufactured_consensus_candidate_untypeable(_), H0, H1, NS, NR) :-
    NR >= 2, NR < NS, H0 == 1, H1 == 0.
coherence_case(plural(Us), H0, H1, NS, NR) :-
    NR >= 2,
    \+ memberchk(unknown, Us),                 % OQ-217: unknown never in the term
    length(Us, K), K >= 2,
    H0 == 0, integer(H1), H1 > 0.              % genuine plurality <-> obstruction

% ---- fixtures ----------------------------------------------------------------
% Recipes witnessed by probe 2026-07-12 (scratchpad probe_fixture_reachability):
%   REAL seat  : eps 0.8 + supp 0.7 metrics, (agenda_setter, institutional,
%                generational, arbitrage, national) -> a real type (rope).
%   FAIL seat  : (not_a_role, not_a_power) -> d-derivation fails (role_base_d
%                and canonical_d_for_power both miss) -> the failure path.
%   UNKNOWN seat: valid role + not_a_power -> derivation succeeds, context
%                invalid -> dr_type_with_d fallback = literal `unknown`.
fixture_metrics(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.8)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.7)).
fixture_real_seat(C, N) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, agenda_setter,
        institutional, generational, arbitrage, national)).
fixture_fail_seat(C, N) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, not_a_role,
        not_a_power, generational, arbitrage, national)).
fixture_unknown_seat(C, N, Role) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, Role,
        not_a_power, generational, arbitrage, national)).
fixture_excluded_seat(C, N) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, excluded,
        institutional, generational, arbitrage, national)).

teardown_fixture(C) :-
    retractall(narrative_ontology:constraint_metric(C, _, _)),
    retractall(narrative_ontology:constraint_stakeholder(C, _, _, _, _, _, _)),
    retractall(narrative_ontology:stakeholder_non_agent(C, _)),
    cache_registry:clear_all_caches.            % D7: clear on teardown too

% ---- 4. no `untyped` in the vector (positive control on the failure path) ---
test(no_untyped_in_vector,
     [ setup(( fixture_fail_seat(tsh_untyped, phantom),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_untyped)) ]) :-
    % control precondition: the derivation ACTUALLY fails for this seat
    \+ stakeholder_seats:dr_type_for_stakeholder(tsh_untyped, phantom, _),
    % census-facing surface types it `untyped` ...
    stakeholder_seats:seat_perceived_vs_real(tsh_untyped, phantom, _, untyped),
    % ... kernel-facing vector maps the SAME failed seat to `unknown`
    stakeholder_seats:stakeholder_type_vector(tsh_untyped, Vector),
    Vector == [unknown],
    \+ memberchk(untyped, Vector).

% ---- 5. zero-seat story reads null ------------------------------------------
test(zero_seat_reads_null,
     [ setup(( fixture_metrics(tsh_zero_seat),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_zero_seat)) ]) :-
    stakeholder_seats:stakeholder_obstruction(tsh_zero_seat, H0, H1, NS, NR),
    H0 == null, H1 == null, NS == 0, NR == 0,
    stakeholder_seats:consensus_provenance(tsh_zero_seat, no_agent_seats).

% ---- 6. former cell (a): 2 agent seats / 1 typeable -> insufficiency --------
% H¹-boundary probe (operator rider, 2026-07-12): single real seat MUST read
% (null, null) on the H side — the exact biconditional's insufficiency row is
% asserted here at its boundary, not just claimed at the header.
test(single_real_seat_insufficient,
     [ setup(( fixture_metrics(tsh_cell_a),
               fixture_real_seat(tsh_cell_a, boss),
               fixture_fail_seat(tsh_cell_a, phantom),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_cell_a)) ]) :-
    % fixture precondition: the typeable seat derives a REAL type
    stakeholder_seats:dr_type_for_stakeholder(tsh_cell_a, boss, T),
    grothendieck_cohomology:is_real_type(T),
    stakeholder_seats:stakeholder_obstruction(tsh_cell_a, H0, H1, NS, NR),
    H0 == null, H1 == null, NS == 2, NR == 1,
    % OQ-217: "unanimity of one" is gone — the insufficiency token, coherent
    stakeholder_seats:consensus_provenance(tsh_cell_a, insufficient_real_seats),
    coherence_case(insufficient_real_seats, H0, H1, NS, NR).

% ---- 7. former cell (b): all seats derive `unknown` -> insufficiency --------
% H¹-boundary probe (operator rider, 2026-07-12): all-derived-unknown MUST
% read (null, null) on the H side. The excluded [ghost] seat stays in the
% fixture to prove the mcc flag can no longer fire over an all-untypeable
% seat set (the OQ-207 D4 kill-condition defect, removed at source).
test(all_unknown_insufficient,
     [ setup(( fixture_unknown_seat(tsh_cell_b, vp1, agenda_setter),
               fixture_unknown_seat(tsh_cell_b, vp2, payer),
               fixture_excluded_seat(tsh_cell_b, ghost),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_cell_b)) ]) :-
    % fixture precondition: both seats derive the LITERAL unknown type
    stakeholder_seats:dr_type_for_stakeholder(tsh_cell_b, vp1, unknown),
    stakeholder_seats:dr_type_for_stakeholder(tsh_cell_b, vp2, unknown),
    stakeholder_seats:stakeholder_obstruction(tsh_cell_b, H0, H1, NS, NR),
    H0 == null, H1 == null, NS == 2, NR == 0,
    % OQ-217: absence is never read as (manufactured) agreement
    stakeholder_seats:consensus_provenance(tsh_cell_b, insufficient_real_seats),
    coherence_case(insufficient_real_seats, H0, H1, NS, NR).

% ---- 7b. former mixed cell, NR = 1: real type beside unknown -> insufficiency
% Pre-OQ-217 this read plural([rope,unknown]) — absence as disagreement, the
% dual of cell (b). Now: <2 real seats -> insufficiency, coherent with H null.
test(mixed_single_real_insufficient,
     [ setup(( fixture_metrics(tsh_mixed),
               fixture_real_seat(tsh_mixed, boss),
               fixture_unknown_seat(tsh_mixed, vp, payer),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_mixed)) ]) :-
    stakeholder_seats:stakeholder_obstruction(tsh_mixed, H0, H1, NS, NR),
    H0 == null, H1 == null, NS == 2, NR == 1,
    stakeholder_seats:consensus_provenance(tsh_mixed, insufficient_real_seats),
    coherence_case(insufficient_real_seats, H0, H1, NS, NR).

% ---- 7c. annotated unanimity: 2 reals agree + unknown seat ------------------
% The OQ-217 option-3 token: >=2 real seats agreeing while an untypeable seat
% sits in the room -> unanimous_with_untypeable_seats (the caveat rides in the
% token, not side-band counts), H = (1, 0) — the exact biconditional's
% unanimity row with NReal < NSeats.
test(unanimous_with_untypeable,
     [ setup(( fixture_metrics(tsh_annot),
               fixture_real_seat(tsh_annot, boss),
               fixture_real_seat(tsh_annot, boss2),
               fixture_unknown_seat(tsh_annot, vp, payer),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_annot)) ]) :-
    stakeholder_seats:stakeholder_obstruction(tsh_annot, H0, H1, NS, NR),
    H0 == 1, H1 == 0, NS == 3, NR == 2,
    stakeholder_seats:consensus_provenance(tsh_annot,
        unanimous_with_untypeable_seats),
    coherence_case(unanimous_with_untypeable_seats, H0, H1, NS, NR).

% ---- 7d. plural is real-only: 2 distinct reals + unknown seat ---------------
% Plurality is EXISTENTIAL (witnessed by >=2 distinct real types), so it needs
% no annotation and the unknown token never appears in the term. Fixture
% recipe for the second real type: victim/powerless -> snare (probe witnessed
% 2026-07-12, sibling of the boss rope recipe).
fixture_snare_seat(C, N) :-
    assertz(narrative_ontology:constraint_stakeholder(C, N, victim,
        powerless, generational, arbitrage, national)).
test(plural_excludes_unknown_token,
     [ setup(( fixture_metrics(tsh_realplural),
               fixture_real_seat(tsh_realplural, boss),
               fixture_snare_seat(tsh_realplural, worker),
               fixture_unknown_seat(tsh_realplural, vp, payer),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_realplural)) ]) :-
    % fixture precondition: the two real seats derive DISTINCT real types
    stakeholder_seats:dr_type_for_stakeholder(tsh_realplural, boss, T1),
    stakeholder_seats:dr_type_for_stakeholder(tsh_realplural, worker, T2),
    T1 \== T2,
    grothendieck_cohomology:is_real_type(T1),
    grothendieck_cohomology:is_real_type(T2),
    stakeholder_seats:consensus_provenance(tsh_realplural, plural(Us)),
    \+ memberchk(unknown, Us),
    msort([T1, T2], SortedTs), Us == SortedTs,
    stakeholder_seats:stakeholder_obstruction(tsh_realplural, H0, H1, NS, NR),
    H0 == 0, integer(H1), H1 > 0, NS == 3, NR == 2,
    coherence_case(plural(Us), H0, H1, NS, NR).

% ---- 7e. mcc still fires on a genuine all-real unanimity + excluded seat ----
% The tightening must not have thrown out the flag itself: 2 real agreeing
% seats, no unknowns, non-empty excluded set -> mcc(Excl) exactly as before.
test(mcc_still_fires_all_real,
     [ setup(( fixture_metrics(tsh_mcc),
               fixture_real_seat(tsh_mcc, boss),
               fixture_real_seat(tsh_mcc, boss2),
               fixture_excluded_seat(tsh_mcc, ghost),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_mcc)) ]) :-
    stakeholder_seats:consensus_provenance(tsh_mcc,
        manufactured_consensus_candidate([ghost])),
    stakeholder_seats:stakeholder_obstruction(tsh_mcc, H0, H1, NS, NR),
    H0 == 1, H1 == 0, NS == 2, NR == 2,
    coherence_case(manufactured_consensus_candidate([ghost]), H0, H1, NS, NR).

% ---- 7f. mcc_untypeable reachability (ruled in session 2026-07-12) ----------
% Predicted-zero live population; the token exists for case-analysis totality
% and this fixture proves the cell is REACHABLE (a token no input can produce
% would be a phantom row in the coherence table).
test(mcc_untypeable_reachable,
     [ setup(( fixture_metrics(tsh_mccu),
               fixture_real_seat(tsh_mccu, boss),
               fixture_real_seat(tsh_mccu, boss2),
               fixture_unknown_seat(tsh_mccu, vp, payer),
               fixture_excluded_seat(tsh_mccu, ghost),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_mccu)) ]) :-
    stakeholder_seats:consensus_provenance(tsh_mccu,
        manufactured_consensus_candidate_untypeable([ghost])),
    stakeholder_seats:stakeholder_obstruction(tsh_mccu, H0, H1, NS, NR),
    H0 == 1, H1 == 0, NS == 3, NR == 2,
    coherence_case(manufactured_consensus_candidate_untypeable([ghost]),
                   H0, H1, NS, NR).

% ---- 8. negative control: perturbed expected set rejected -------------------
% The SAME comparator (check_h1_in_table/4) must FLAG every live determinable
% H1 when its observed value is removed from the expected set — proving the
% live_spectrum_validity pass is a discriminating check, not a vacuous
% memberchk over an unbound value or an all-accepting table.
test(negative_control_perturbed_spectrum) :-
    true_table(Table),
    aggregate_all(count, live_numbered(_, _, _), NNum),
    NNum > 0,                                   % control must have something to flag
    forall(live_numbered(C, H1, NReal),
           ( memberchk(NReal-Spec, Table),
             selectchk(H1, Spec, PerturbedSpec),
             catch(( check_h1_in_table(C, H1, NReal, [NReal-PerturbedSpec]),
                     throw(comparator_blind(C, h1(H1), n_real(NReal)))
                   ),
                   spectrum_violation(_, _, _, _),
                   true) )).

% ---- 9. agent-seat domain matches consensus exactly -------------------------
% Excluded and non-agent seats are excluded from the vector domain EXACTLY as
% consensus_provenance excludes them (structurally shared via
% stakeholder_agent_seats/2; this pins the behavior against a refactor fork).
test(agent_seat_domain_matches_consensus,
     [ setup(( fixture_metrics(tsh_domain),
               fixture_real_seat(tsh_domain, boss),
               fixture_excluded_seat(tsh_domain, ghost),
               assertz(narrative_ontology:constraint_stakeholder(tsh_domain,
                   robot, observer, institutional, generational, arbitrage,
                   national)),
               assertz(narrative_ontology:stakeholder_non_agent(tsh_domain, robot)),
               cache_registry:clear_all_caches )),
       cleanup(teardown_fixture(tsh_domain)) ]) :-
    stakeholder_seats:stakeholder_agent_seats(tsh_domain, Ns),
    Ns == [boss],
    stakeholder_seats:stakeholder_obstruction(tsh_domain, _, _, 1, _),
    % excluded-only counterpart: empty domain <-> no_agent_seats
    stakeholder_seats:stakeholder_agent_seats(tsh_domain_absent, []),
    stakeholder_seats:consensus_provenance(tsh_domain_absent, no_agent_seats).

:- end_tests(h1_stakeholder_spectrum).
