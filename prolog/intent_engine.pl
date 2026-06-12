:- module(intent_engine, [
	classify_interval/3,
	analyze_intent/1
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(coercion_projection). % Math Provider
:- use_module(pattern_analysis).        % State Provider

/* ================================================================
   1. MAIN ENTRY
   ================================================================ */

classify_interval(IntervalID, Pattern, Confidence) :-
    % Ensure the analysis service has populated the dynamic facts
    pattern_analysis:analyze_interval(IntervalID),
    pattern_analysis:interval_preliminary_pattern(IntervalID, Prelim),
    % Gradient-fact guard kept on retire: classify_interval must still FAIL
    % (-> analyze_intent's analysis-failed report) when no gradient fact
    % exists for the interval — pre-OQ-106 control flow, preserved.
    pattern_analysis:interval_system_gradient(IntervalID, coercion, _Gsys),
    pattern_analysis:interval_data_completeness(IntervalID, DataScore),
    classify_non_intent(Prelim, Pattern),
    fallback_confidence(DataScore, Confidence).

/* ================================================================
   2. STRUCTURAL COERCIVE INTENT — RETIRED (OQ-106, operator ruling
   2026-06-12). The top verdict never fired in the construct's
   history and could not: threshold range-dead by arithmetic
   (system_gradient_strong_threshold 1.00 strict vs 0.98 max
   reachable G_sys) and Conditions 2-4 read intent_* tables with no
   producer anywhere (GAP-08). Piton's designed/decayed axis is
   carried by the capture gate (capture-as-design, ruled with this
   retire), not by this verdict. The capability (interval-level
   intent conjunction) stays a declared absence in design_gaps.md
   GAP-08; revival = fresh preregistration (schema -> producer ->
   threshold re-derivation -> wiring). Witness for the deletion:
   audits/2026-06-12_oq106_retire/.
   ================================================================ */

/* ================================================================
   3. HELPERS & UTILS
   ================================================================ */

classify_non_intent(Prelim, Pattern) :-
    (Prelim = increasing_coercion -> Pattern = increasing_coercion
    ; Prelim = decreasing_coercion -> Pattern = decreasing_coercion
    % OQ-93 coverage-carrying read: an OPEN grid track stays OPEN here —
    % mapping open(...) to stable would re-create the success-shaped
    % default this migration killed (absence must not read as a verdict).
    ; Prelim = open(Why) -> Pattern = open(Why)
    ; Pattern = stable).

fallback_confidence(DataScore, Conf) :-
    config:param(data_high_threshold, DH),
    (DataScore >= DH -> Conf = high ; Conf = low).

%% analyze_intent(+IntervalID)
%  Harness wrapper to satisfy test_harness.pl.
analyze_intent(IntervalID) :-
    (   classify_interval(IntervalID, Pattern, Confidence)
    ->  (   Pattern = open(Why)
        ->  % OQ-93 coverage-carrying read: grid absent or below the system
            % verdict's named levels — the grid track reports OPEN, never a
            % default-shaped stable.
            format('  [INTENT] Result: OPEN (~w) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93)', [Why])
        ;   format('  [INTENT] Result: ~w (Confidence: ~w)', [Pattern, Confidence])
        ),
        % OQ-93: this verdict's gradient + completeness inputs come from the
        % leveled grid — print the actual diet so the verdict cannot read as
        % evidence-fed when it is not.
        (   catch(data_repair:grid_provenance(IntervalID, prov(A, I, P, _Abs, Total)), _, fail)
        ->  format(' [grid diet: authored ~w/~w, injected ~w, imputed ~w — OQ-93]~n', [A, Total, I, P])
        ;   nl
        )
    ;   format('  [INTENT] Analysis failed for ~w~n', [IntervalID])
    ).
