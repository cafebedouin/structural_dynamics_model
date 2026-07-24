% ============================================================================
% test_purity_absence_class.pl — OQ-61 Q3 classifier boundary control.
%
% purity_absence_class/2 (json_report.pl) partitions every constraint into
% {scored, gate_fail, no_data, malformed}. malformed is a FAIL-CLOSED
% guard-class, NOT a fifth vocabulary token — the emit block halts on it. This
% suite is the permanent boundary control for that partition.
%
% Layers:
%   (a) Pure value classifier (purity_absence_classify/2) — corpus-independent,
%       covers all four classes INCLUDING malformed (which cannot arise through
%       the real clamped purity_score/2, so only the pure seam can witness it).
%   (b) Real fetch path (purity_absence_class/2) over in-memory probes:
%       gate-passing bare -> no_data; <MinN classifications -> gate_fail; the
%       golden corpus constraint -> scored.
%   (c) Partition + token-move: purity_absence_counts sums to n_total; moving
%       ONE token (gate_fail -> no_data, by adding classifications) moves the
%       matching addend — the sum-invariant alone would miss a misfile.
%   (d) purity_score determinism: exactly one solution (once/1 in the classifier
%       would otherwise mask a multi-solution regression).
%
% Run (needs the PIPELINE load chain — purity_absence_class lives in json_report,
% which [stack] does not load):
%   cd prolog && swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%     -l dirac_classification.pl -l diagnostic_summary.pl -l post_synthesis.pl \
%     -l json_report.pl -l giant_component_analysis.pl \
%     -g "[tests/test_purity_absence_class], run_tests(purity_absence_class), halt" \
%     -t "halt(1)"
% ============================================================================

:- corpus_loader:ensure_corpus_loaded.

% Gate-PASSING bare template (3 authored classifications, no grid/coupling/
% extraction/coordination data) — purity_score is `unknown` (no_data).
oq61_assert_nodata(C) :-
    retractall(constraint_indexing:constraint_classification(C, _, _)),
    Ctx1 = context(agent_power(powerless), time_horizon(generational), exit_options(trapped), spatial_scope(national)),
    Ctx2 = context(agent_power(moderate), time_horizon(generational), exit_options(mobile), spatial_scope(national)),
    Ctx3 = context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx1)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx2)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx3)),
    cache_registry:clear_all_caches.

% Gate-FAILING template (1 classification, < boltzmann_min_classifications=3):
% epistemic_access_check fails -> purity_score = -1.0 sentinel (gate_fail).
oq61_assert_gatefail(C) :-
    retractall(constraint_indexing:constraint_classification(C, _, _)),
    Ctx1 = context(agent_power(powerless), time_horizon(generational), exit_options(trapped), spatial_scope(national)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx1)),
    cache_registry:clear_all_caches.

% Promote a gate_fail probe to gate-passing (add 2 classifications) — the token
% move for layer (c). No other data -> lands in no_data.
oq61_promote_to_nodata(C) :-
    Ctx2 = context(agent_power(moderate), time_horizon(generational), exit_options(mobile), spatial_scope(national)),
    Ctx3 = context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx2)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx3)),
    cache_registry:clear_all_caches.

oq61_retract_probe(C) :-
    retractall(constraint_indexing:constraint_classification(C, _, _)),
    cache_registry:clear_all_caches.

% ----------------------------------------------------------------------------
:- begin_tests(purity_absence_class).

% (a) Pure value classifier — every class, incl. malformed (both signs).
test(classify_scored_interior)    :- json_report:purity_absence_classify(0.85, C), C == scored.
test(classify_scored_low_band)    :- json_report:purity_absence_classify(0.35, C), C == scored.
test(classify_scored_lower_bound) :- json_report:purity_absence_classify(0.0, C),  C == scored.
test(classify_scored_upper_bound) :- json_report:purity_absence_classify(1.0, C),  C == scored.
test(classify_gate_fail)          :- json_report:purity_absence_classify(-1.0, C), C == gate_fail.
test(classify_no_data)            :- json_report:purity_absence_classify(unknown, C), C == no_data.
test(classify_malformed_high)     :- json_report:purity_absence_classify(1.5, C),  C == malformed.
test(classify_malformed_negative) :- json_report:purity_absence_classify(-0.5, C), C == malformed.
% -0.5 is out of range and NOT the -1.0 sentinel -> malformed, never gate_fail.
test(classify_malformed_not_gate_fail) :-
    json_report:purity_absence_classify(-0.5, C), C \== gate_fail.

% (b) Real fetch path over in-memory probes.
test(fetch_no_data, [
        setup(oq61_assert_nodata(oq61_p_nodata)),
        cleanup(oq61_retract_probe(oq61_p_nodata))
    ]) :-
    json_report:purity_absence_class(oq61_p_nodata, C), C == no_data.

test(fetch_gate_fail, [
        setup(oq61_assert_gatefail(oq61_p_gatefail)),
        cleanup(oq61_retract_probe(oq61_p_gatefail))
    ]) :-
    json_report:purity_absence_class(oq61_p_gatefail, C), C == gate_fail.

% Golden corpus constraint is scored (contaminated band 0.354 — also the
% off-diagonal cell for the Q2 tab: a scored constraint below its type band).
test(fetch_scored_golden) :-
    json_report:purity_absence_class(alignment_constraint_narrowing, C),
    C == scored.

% (d) purity_score determinism: exactly one solution.
test(purity_score_deterministic) :-
    findall(P, purity_scoring:purity_score(alignment_constraint_narrowing, P), Ps),
    Ps = [Single], number(Single).

% (c) Partition + token-move. Set = {gate_fail, no_data, golden(scored)}.
test(counts_partition_and_token_move, [
        setup((
            oq61_assert_gatefail(oq61_p_move),
            oq61_assert_nodata(oq61_p_nodata2)
        )),
        cleanup((
            oq61_retract_probe(oq61_p_move),
            oq61_retract_probe(oq61_p_nodata2)
        ))
    ]) :-
    Set = [oq61_p_move, oq61_p_nodata2, alignment_constraint_narrowing],
    % before: one of each, no malformed; sum closes.
    json_report:purity_absence_counts(Set, S0, G0, D0, M0),
    S0 =:= 1, G0 =:= 1, D0 =:= 1, M0 =:= 0,
    S0 + G0 + D0 =:= 3,
    % move ONE token: promote the gate_fail probe to gate-passing -> no_data.
    oq61_promote_to_nodata(oq61_p_move),
    json_report:purity_absence_counts(Set, S1, G1, D1, M1),
    % the gate_fail addend fell and the no_data addend rose by the same 1;
    % scored is untouched; sum still closes; still no malformed.
    S1 =:= 1, G1 =:= 0, D1 =:= 2, M1 =:= 0,
    G1 =:= G0 - 1, D1 =:= D0 + 1, S1 =:= S0,
    S1 + G1 + D1 =:= 3.

:- end_tests(purity_absence_class).
