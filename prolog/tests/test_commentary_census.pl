% ============================================================================
% test_commentary_census.pl — OQ-134 corpus-wide commentary-grade census.
%
% Freezes the census invariants:
%   - sum invariant Σ buckets == n_corpus per source, AND n_corpus > 0 (the bare
%     0==0 would clear vacuously if the corpus failed to load — n>0 closes that).
%   - q6_unclassified and q6_signature_unknown are DISTINCT histogram keys
%     (never collapsed); the side-absent precedence routes contested×unknown to
%     q6_signature_unknown, not contested_open / q6_unclassified.
%   - per-cell POSITIVE CONTROLS: in-test fixtures asserted into each target cell
%     land in the right bucket — proves commentary_cell FIRES, not just returns 0.
%   - absence-bucket + coverage-decidability declarations (Pattern-6 honesty).
%   - generic source: extraction_reading buckets (fired/silent) sum to n_corpus.
%
% DERIVE-DON'T-STUB (mirrors test_oq86_extraction_commentary): every q6 named-cell
% fixture asserts only INPUTS (metrics + founding_problem_status). The signature
% is READ from drl_core:dr_type/2; a precondition test checks the derived type so
% a calibration drift fails VISIBLY rather than passing on a stubbed cell.
%
% Fixtures are NOT corpus_constraint/1 facts, so they do NOT perturb the census
% denominator — the sum-invariant tests run over the real loaded corpus.
%
% Run: cd prolog && swipl -g "[stack], corpus_loader:load_all_testsets, \
%   [commentary_census], [tests/test_commentary_census], run_tests, halt" -t "halt(1)"
% ============================================================================

:- multifile
    narrative_ontology:constraint_metric/3,
    narrative_ontology:founding_problem_status/2.

% Shared snare recipe (measured this session: ε=0.82 / supp=0.91 + structure -> snare;
% requires_active_enforcement is NOT needed for the derivation — verified — and is a
% STATIC procedure once corpus testsets load, so it cannot be asserted at runtime).
snare_metrics(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.82)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.91)),
    assertz(narrative_ontology:constraint_metric(C, theater_ratio, 0.30)),
    assertz(narrative_ontology:constraint_metric(C, accessibility_collapse, 0.70)),
    assertz(narrative_ontology:constraint_metric(C, resistance, 0.75)).

% Fixture id list (for clean teardown).
fixture_id(tcc_dead_snare).
fixture_id(tcc_live_snare).
fixture_id(tcc_contested_open).
fixture_id(tcc_sig_unknown).
fixture_id(tcc_unmeasured).

setup_fixtures :-
    % dead × snare -> dead_claim_vs_snare_present
    snare_metrics(tcc_dead_snare),
    assertz(narrative_ontology:founding_problem_status(tcc_dead_snare, dead)),
    % live × snare -> live_claim_vs_snare_present
    snare_metrics(tcc_live_snare),
    assertz(narrative_ontology:founding_problem_status(tcc_live_snare, live)),
    % contested × snare (non-unknown) -> contested_open
    snare_metrics(tcc_contested_open),
    assertz(narrative_ontology:founding_problem_status(tcc_contested_open, contested)),
    % contested × NO metrics (dr_type unknown) -> q6_signature_unknown (side-absent wins)
    assertz(narrative_ontology:founding_problem_status(tcc_sig_unknown, contested)),
    % metrics but NO founding_problem_status -> q6_unmeasured (authored side absent)
    snare_metrics(tcc_unmeasured).

teardown_fixtures :-
    forall(fixture_id(C),
           ( retractall(narrative_ontology:constraint_metric(C, _, _)),
             retractall(narrative_ontology:founding_problem_status(C, _)) )).

% Σ of the Count-Bucket pairs.
sum_pairs([], 0).
sum_pairs([Count-_|T], S) :- sum_pairs(T, S0), S is S0 + Count.

:- begin_tests(commentary_census, [setup(setup_fixtures), cleanup(teardown_fixtures)]).

% --- Sum invariant + n>0 (both sources) --------------------------------------
test(q6_sum_invariant_and_nonzero) :-
    commentary_census:commentary_census(q6, census(q6, N, Pairs, _)),
    N > 0,                                  % corpus actually loaded (not 0==0)
    sum_pairs(Pairs, Sum),
    Sum =:= N.

test(extraction_sum_invariant_and_nonzero) :-
    commentary_census:commentary_census(extraction_reading,
        census(extraction_reading, N, Pairs, _)),
    N > 0,
    sum_pairs(Pairs, Sum),
    Sum =:= N.

% --- Distinct keys, never collapsed ------------------------------------------
test(unclassified_and_signature_unknown_are_distinct_atoms) :-
    q6_signature_unknown \== q6_unclassified.

% --- Per-cell positive controls: each fixture lands in the right bucket -------
% Preconditions read the derived type so a calibration drift fails visibly.
test(dead_snare_precondition_type) :-
    drl_core:dr_type(tcc_dead_snare, snare).
test(dead_snare_cell) :-
    commentary_census:commentary_cell(q6, tcc_dead_snare, dead_claim_vs_snare_present).

test(live_snare_precondition_type) :-
    drl_core:dr_type(tcc_live_snare, snare).
test(live_snare_cell) :-
    commentary_census:commentary_cell(q6, tcc_live_snare, live_claim_vs_snare_present).

test(contested_open_precondition_type) :-
    drl_core:dr_type(tcc_contested_open, snare).
test(contested_open_cell) :-
    commentary_census:commentary_cell(q6, tcc_contested_open, contested_open).

% contested × unknown -> q6_signature_unknown (precedence: side-absent dominates),
% and decidedly NOT q6_unclassified — the distinct-key guarantee, exercised.
test(sig_unknown_precondition_type) :-
    drl_core:dr_type(tcc_sig_unknown, unknown).
test(sig_unknown_cell) :-
    commentary_census:commentary_cell(q6, tcc_sig_unknown, q6_signature_unknown).
test(sig_unknown_not_unclassified) :-
    commentary_census:commentary_cell(q6, tcc_sig_unknown, B),
    B \== q6_unclassified,
    B \== contested_open.

% authored side absent -> q6_unmeasured.
test(unmeasured_cell) :-
    commentary_census:commentary_cell(q6, tcc_unmeasured, q6_unmeasured).

% --- commentary_cell is deterministic (exactly one bucket per constraint) ----
test(commentary_cell_q6_deterministic) :-
    findall(B, commentary_census:commentary_cell(q6, tcc_dead_snare, B), Bs),
    Bs == [dead_claim_vs_snare_present].

% --- Absence-bucket + coverage-decidability declarations (Pattern-6 honesty) --
test(q6_absence_buckets_declared) :-
    commentary_census:commentary_absence_bucket(q6, q6_unmeasured),
    commentary_census:commentary_absence_bucket(q6, q6_signature_unknown).
test(q6_unclassified_is_not_absence) :-
    \+ commentary_census:commentary_absence_bucket(q6, q6_unclassified).
test(q6_coverage_decidable) :-
    commentary_census:commentary_coverage_decidable(q6).
test(extraction_no_absence_bucket) :-
    \+ commentary_census:commentary_absence_bucket(extraction_reading, _).
test(extraction_coverage_not_decidable) :-
    \+ commentary_census:commentary_coverage_decidable(extraction_reading).

% --- Generic source: extraction_reading bivalues to exactly its two keys ------
test(extraction_buckets_are_fired_or_silent) :-
    commentary_census:commentary_census(extraction_reading,
        census(extraction_reading, _, Pairs, _)),
    forall(member(_-B, Pairs),
           memberchk(B, [extraction_blindspot_fired, extraction_silent])).

:- end_tests(commentary_census).
