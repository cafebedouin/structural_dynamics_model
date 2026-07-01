% ============================================================================
% test_gap_operability.pl — OQ-197 three-valued gap operability regression tests.
%
% Guards the fix for the Build-Discipline Pattern-6 collapse in the gap detector:
% measured-no-gap and didn't-look both used to emit a success-shaped empty. The
% fix adds gap_status/2 returning exactly one of gap(...) / no_gap /
% undetermined(Reason), with gap_coverage/1 lifted from the >=1-seat PROXY to the
% operability PRECONDITION (>=2 seats spanning >=2 distinct power positions).
%
% Two-sided by construction — the operability logic must both FLAG the
% insufficient cases (no_seats / single_seat / single_power_position) AND CLEAR
% the genuinely-comparable ones (no_gap). A fix that made everything read
% undetermined would pass an absence-only control vacuously; the no_gap cases
% below are the negative control that forbids that.
%
% Run: cd prolog && swipl -g "[stack], [tests/test_gap_operability], run_tests, halt" -t "halt(1)"
% ============================================================================

:- corpus_loader:ensure_corpus_loaded.

:- begin_tests(gap_operability).

% ----------------------------------------------------------------------------
% (a) Pure operability logic — nonfire_reason/2 on hand-built seat lists.
%     Deterministic, corpus-independent. Positive AND negative sides together.
% ----------------------------------------------------------------------------

% no operable seats at all -> undetermined(no_seats) (the kernel_v1 absence flavor)
test(nonfire_no_seats) :-
    report_generator:nonfire_reason([], undetermined(no_seats)).

% exactly one operable seat -> can't compare -> undetermined(single_seat)
test(nonfire_single_seat) :-
    report_generator:nonfire_reason([reading(0.9, powerless, scaffold, a)],
                                    undetermined(single_seat)).

% >=2 seats but all at ONE power position -> no gradient -> undetermined
% (the twins' present-but-insufficient flavor: seats present, not spanning power)
test(nonfire_single_power_position) :-
    report_generator:nonfire_reason(
        [reading(0.9, powerless, scaffold, a), reading(0.9, powerless, snare, b)],
        undetermined(single_power_position)).

% NEGATIVE CONTROL: >=2 seats at >=2 distinct power positions, types AGREE ->
% genuinely comparable, no gap -> no_gap (NOT undetermined). This forbids the
% everything-undetermined degenerate fix.
test(nonfire_no_gap_when_comparable_and_agree) :-
    report_generator:nonfire_reason(
        [reading(0.9, powerless, scaffold, a), reading(0.1, institutional, scaffold, b)],
        no_gap).

% nonfire_reason is total & deterministic across the four shapes.
test(nonfire_total_deterministic) :-
    forall(member(Rs, [[],
                       [reading(0.9,powerless,scaffold,a)],
                       [reading(0.9,powerless,scaffold,a),reading(0.9,powerless,snare,b)],
                       [reading(0.9,powerless,scaffold,a),reading(0.1,institutional,scaffold,b)]]),
           ( findall(S, report_generator:nonfire_reason(Rs, S), Ss),
             Ss = [_] )).

% ----------------------------------------------------------------------------
% (b) Contract integrity — gap_status/2 is TOTAL and DETERMINISTIC on the live
%     corpus (exactly one status per constraint), and gap_coverage/1 agrees with
%     it (covered iff not undetermined).
% ----------------------------------------------------------------------------

test(gap_status_total_deterministic) :-
    forall(corpus_loader:corpus_constraint(C),
           ( findall(S, report_generator:gap_status(C, S), Ss),
             Ss = [_] )).

test(gap_coverage_agrees_with_status) :-
    forall(corpus_loader:corpus_constraint(C),
           ( report_generator:gap_status(C, S),
             ( report_generator:gap_coverage(C)
             -> S \= undetermined(_)
             ;  S = undetermined(_) ) )).

% firing consumers still see exactly the gap(...) statuses (behaviour preserved):
% a constraint fires detect_gap_pattern iff gap_status is gap(...).
test(detect_matches_status_gap) :-
    forall(corpus_loader:corpus_constraint(C),
           ( ( report_generator:detect_gap_pattern(C, G)
             -> report_generator:gap_status(C, G)
             ;  ( report_generator:gap_status(C, S2), S2 \= gap(_,_,_) ) ) )).

% ----------------------------------------------------------------------------
% (c) Corpus regression control — dataset_recycling_amplification has stakeholder
%     seats at >=2 power positions all computing scaffold; it must read no_gap
%     (comparable, agree), NOT undetermined and NOT a spurious gap. Skips cleanly
%     if the constraint is absent from the loaded corpus.
% ----------------------------------------------------------------------------

test(dataset_recycling_reads_no_gap) :-
    ( corpus_loader:corpus_constraint(dataset_recycling_amplification)
    -> report_generator:gap_status(dataset_recycling_amplification, no_gap)
    ;  true ).

:- end_tests(gap_operability).
