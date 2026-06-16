% ============================================================================
% test_commentary_census.pl — OQ-134/OQ-121 corpus-wide commentary-grade census.
%
% Freezes the census invariants:
%   - sum invariant Σ buckets == n_corpus per source, AND n_corpus > 0 (the bare
%     0==0 would clear vacuously if the corpus failed to load — n>0 closes that).
%   - q6_unclassified and q6_signature_unknown are DISTINCT histogram keys
%     (never collapsed); side-absent precedence routes contested×unknown to
%     q6_signature_unknown, not contested_open / q6_unclassified.
%   - OQ-121 totalization: extraction_state/2 is a TOTAL function — every
%     constraint reaches exactly one of {out_of_domain, extraction_clear,
%     extraction_unnameable, extraction_fired(_)}; extraction_reading/2 fires
%     ONLY on extraction_fired (contract preserved). Per-state positive controls.
%   - domain split: n_in_domain = n_corpus − Σ out-of-domain; coverage is
%     DOMAIN-relative; prevalence is a distinct quantity.
%   - per-cell POSITIVE CONTROLS: in-test fixtures asserted into each target cell
%     land in the right bucket — proves commentary_cell FIRES, not just returns 0.
%
% DERIVE-DON'T-STUB: every fixture asserts only INPUTS (metrics / status / victim /
% stakeholder). The signature is READ from drl_core:dr_type/2; preconditions check
% the derived type so a calibration drift fails VISIBLY rather than on a stub.
%
% Fixtures are NOT corpus_constraint/1 facts, so they do NOT perturb the census
% denominator — the sum-invariant tests run over the real loaded corpus.
%
% Run: cd prolog && swipl -g "[stack], corpus_loader:load_all_testsets, \
%   [commentary_census], [tests/test_commentary_census], run_tests(commentary_census), halt" -t "halt(1)"
% ============================================================================

:- multifile
    narrative_ontology:constraint_metric/3,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_stakeholder/7.

% Shared snare recipe (measured: ε=0.82 / supp=0.91 + structure -> snare;
% requires_active_enforcement is NOT needed and is STATIC after corpus load).
snare_metrics(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.82)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.91)),
    assertz(narrative_ontology:constraint_metric(C, theater_ratio, 0.30)),
    assertz(narrative_ontology:constraint_metric(C, accessibility_collapse, 0.70)),
    assertz(narrative_ontology:constraint_metric(C, resistance, 0.75)).

beneficiary_seat(C, Name) :-
    assertz(narrative_ontology:constraint_stakeholder(C, Name, agenda_setter,
        institutional, generational, arbitrage, national)).

fixture_id(tcc_dead_snare).
fixture_id(tcc_live_snare).
fixture_id(tcc_contested_open).
fixture_id(tcc_sig_unknown).
fixture_id(tcc_unmeasured).
fixture_id(tcc_ext_ood).
fixture_id(tcc_ext_clear).
fixture_id(tcc_ext_unnameable).
fixture_id(tcc_ext_fired).

setup_fixtures :-
    % --- q6 fixtures ---
    snare_metrics(tcc_dead_snare),
    assertz(narrative_ontology:founding_problem_status(tcc_dead_snare, dead)),
    snare_metrics(tcc_live_snare),
    assertz(narrative_ontology:founding_problem_status(tcc_live_snare, live)),
    snare_metrics(tcc_contested_open),
    assertz(narrative_ontology:founding_problem_status(tcc_contested_open, contested)),
    assertz(narrative_ontology:founding_problem_status(tcc_sig_unknown, contested)),
    snare_metrics(tcc_unmeasured),
    % --- extraction_state fixtures (OQ-121) ---
    % out_of_domain: no metrics -> dr_type unknown -> non-extractive.
    true,
    % clear: extractive + AUTHORED victim.
    snare_metrics(tcc_ext_clear),
    assertz(narrative_ontology:constraint_victim(tcc_ext_clear, captured_party)),
    % unnameable: extractive, no victim, NO beneficiary seat.
    snare_metrics(tcc_ext_unnameable),
    % fired: extractive, no victim, two beneficiary-side seats.
    snare_metrics(tcc_ext_fired),
    beneficiary_seat(tcc_ext_fired, cartel_a),
    beneficiary_seat(tcc_ext_fired, cartel_b).

teardown_fixtures :-
    forall(fixture_id(C),
           ( retractall(narrative_ontology:constraint_metric(C, _, _)),
             retractall(narrative_ontology:founding_problem_status(C, _)),
             retractall(narrative_ontology:constraint_victim(C, _)),
             retractall(narrative_ontology:constraint_stakeholder(C, _, _, _, _, _, _)) )).

% Σ of the Count-Bucket pairs.
sum_pairs([], 0).
sum_pairs([Count-_|T], S) :- sum_pairs(T, S0), S is S0 + Count.

:- begin_tests(commentary_census, [setup(setup_fixtures), cleanup(teardown_fixtures)]).

% --- Sum invariant + n>0 (both sources) --------------------------------------
test(q6_sum_invariant_and_nonzero) :-
    commentary_census:commentary_census(q6, census(q6, N, NID, Pairs, _, _, _)),
    N > 0,
    NID =:= N,                              % q6 universal domain
    sum_pairs(Pairs, Sum),
    Sum =:= N.

test(extraction_sum_invariant_and_nonzero) :-
    commentary_census:commentary_census(extraction_reading,
        census(extraction_reading, N, NID, Pairs, _, Oods, _)),
    N > 0,
    sum_pairs(Pairs, Sum),
    Sum =:= N,
    Oods = [_|_],                           % extraction HAS an out-of-domain bucket
    NID < N.                                % domain is a strict subset of the corpus

% --- Domain split consistency: n_in_domain = n_corpus − Σ out-of-domain -------
test(extraction_in_domain_split) :-
    commentary_census:commentary_census(extraction_reading,
        census(extraction_reading, N, NID, Pairs, _, Oods, _)),
    findall(Cnt, (member(OB, Oods), member(Cnt-OB, Pairs)), OodCnts),
    sum_list(OodCnts, NOod),
    NID =:= N - NOod.

% --- Distinct keys, never collapsed ------------------------------------------
test(unclassified_and_signature_unknown_are_distinct_atoms) :-
    q6_signature_unknown \== q6_unclassified.

% --- q6 per-cell positive controls -------------------------------------------
test(dead_snare_precondition_type) :- drl_core:dr_type(tcc_dead_snare, snare).
test(dead_snare_cell) :-
    commentary_census:commentary_cell(q6, tcc_dead_snare, dead_claim_vs_snare_present).
test(live_snare_precondition_type) :- drl_core:dr_type(tcc_live_snare, snare).
test(live_snare_cell) :-
    commentary_census:commentary_cell(q6, tcc_live_snare, live_claim_vs_snare_present).
test(contested_open_precondition_type) :- drl_core:dr_type(tcc_contested_open, snare).
test(contested_open_cell) :-
    commentary_census:commentary_cell(q6, tcc_contested_open, contested_open).
test(sig_unknown_precondition_type) :- drl_core:dr_type(tcc_sig_unknown, unknown).
test(sig_unknown_cell) :-
    commentary_census:commentary_cell(q6, tcc_sig_unknown, q6_signature_unknown).
test(sig_unknown_not_unclassified) :-
    commentary_census:commentary_cell(q6, tcc_sig_unknown, B),
    B \== q6_unclassified, B \== contested_open.
test(unmeasured_cell) :-
    commentary_census:commentary_cell(q6, tcc_unmeasured, q6_unmeasured).
test(commentary_cell_q6_deterministic) :-
    findall(B, commentary_census:commentary_cell(q6, tcc_dead_snare, B), Bs),
    Bs == [dead_claim_vs_snare_present].

% --- OQ-121 totalization: extraction_state per-state positive controls --------
test(ext_ood_precondition) :-
    drl_core:dr_type(tcc_ext_ood, T), \+ stakeholder_seats:extractive_type(T).
test(ext_ood_state) :-
    stakeholder_seats:extraction_state(tcc_ext_ood, out_of_domain).
test(ext_clear_precondition) :-
    drl_core:dr_type(tcc_ext_clear, T), stakeholder_seats:extractive_type(T),
    narrative_ontology:constraint_victim(tcc_ext_clear, _).
test(ext_clear_state) :-
    stakeholder_seats:extraction_state(tcc_ext_clear, extraction_clear).
test(ext_unnameable_precondition) :-
    drl_core:dr_type(tcc_ext_unnameable, T), stakeholder_seats:extractive_type(T),
    \+ narrative_ontology:constraint_victim(tcc_ext_unnameable, _).
test(ext_unnameable_state) :-
    stakeholder_seats:extraction_state(tcc_ext_unnameable, extraction_unnameable).
test(ext_fired_state) :-
    stakeholder_seats:extraction_state(tcc_ext_fired, extraction_fired(Es)),
    Es == [cartel_a, cartel_b].

% extraction_reading/2 contract: fires ONLY on the fired state ----------------
test(ext_reading_fires_on_fired) :-
    stakeholder_seats:extraction_reading(tcc_ext_fired,
        extraction([cartel_a, cartel_b], cost_bearer_unnamed)).
test(ext_reading_silent_on_clear)      :- \+ stakeholder_seats:extraction_reading(tcc_ext_clear, _).
test(ext_reading_silent_on_unnameable) :- \+ stakeholder_seats:extraction_reading(tcc_ext_unnameable, _).
test(ext_reading_silent_on_ood)        :- \+ stakeholder_seats:extraction_reading(tcc_ext_ood, _).

% commentary_cell maps each state to its bucket, deterministically ------------
test(ext_cell_ood) :-
    commentary_census:commentary_cell(extraction_reading, tcc_ext_ood, extraction_out_of_domain).
test(ext_cell_clear) :-
    commentary_census:commentary_cell(extraction_reading, tcc_ext_clear, extraction_clear).
test(ext_cell_unnameable) :-
    commentary_census:commentary_cell(extraction_reading, tcc_ext_unnameable, extraction_unnameable).
test(ext_cell_fired) :-
    commentary_census:commentary_cell(extraction_reading, tcc_ext_fired, extraction_blindspot_fired).
test(ext_cell_deterministic) :-
    findall(B, commentary_census:commentary_cell(extraction_reading, tcc_ext_fired, B), Bs),
    Bs == [extraction_blindspot_fired].

% --- Declarations (Pattern-6 honesty) ----------------------------------------
test(q6_absence_buckets_declared) :-
    commentary_census:commentary_absence_bucket(q6, q6_unmeasured),
    commentary_census:commentary_absence_bucket(q6, q6_signature_unknown).
test(q6_unclassified_is_not_absence) :-
    \+ commentary_census:commentary_absence_bucket(q6, q6_unclassified).
test(q6_coverage_decidable) :-
    commentary_census:commentary_coverage_decidable(q6).
test(q6_has_no_out_of_domain_bucket) :-
    \+ commentary_census:commentary_out_of_domain_bucket(q6, _).
test(extraction_out_of_domain_declared) :-
    commentary_census:commentary_out_of_domain_bucket(extraction_reading, extraction_out_of_domain).
test(extraction_no_absence_bucket) :-
    \+ commentary_census:commentary_absence_bucket(extraction_reading, _).
test(extraction_coverage_decidable) :-          % OQ-121: now ruled (was N/A)
    commentary_census:commentary_coverage_decidable(extraction_reading).
test(extraction_prevalence_bucket_declared) :-
    commentary_census:commentary_prevalence_bucket(extraction_reading, extraction_blindspot_fired).

% --- Generic source: extraction buckets ⊆ the four total states --------------
test(extraction_buckets_are_known_states) :-
    commentary_census:commentary_census(extraction_reading,
        census(extraction_reading, _, _, Pairs, _, _, _)),
    forall(member(_-B, Pairs),
           memberchk(B, [extraction_out_of_domain, extraction_clear,
                         extraction_unnameable, extraction_blindspot_fired])).

:- end_tests(commentary_census).
