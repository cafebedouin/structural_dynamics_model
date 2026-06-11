:- module(intent_engine, [
	classify_interval/3,
	analyze_intent/1
]).

:- use_module(library(lists)).        % Required for sum_list/2
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
    pattern_analysis:interval_system_gradient(IntervalID, coercion, Gsys),
    pattern_analysis:interval_data_completeness(IntervalID, DataScore),
    
    collect_intent_evidence(IntervalID, Evidence),
    (   structural_coercive_intent(IntervalID, Prelim, Gsys, Evidence)
    ->  Pattern = structural_coercive_intent,
        refine_confidence(Evidence, DataScore, Confidence)
    ;   classify_non_intent(Prelim, Pattern),
        fallback_confidence(DataScore, Confidence)
    ).

/* ================================================================
   2. STRUCTURAL COERCIVE INTENT (4 CONDITIONS)
   ================================================================ */

structural_coercive_intent(_IntervalID, Prelim, Gsys, Evidence) :-
    % Condition 1: Strong Positive Gradient
    Prelim = increasing_coercion,
    config:param(system_gradient_strong_threshold, StrongThr),
    Gsys > StrongThr,

    % Condition 2: Alternatives Rejected
    member(viable(_System, Alt), Evidence),
    member(rejected(_System2, Alt), Evidence),

    % Condition 3: Beneficiary Asymmetry
    findall((Class, Delta), member(power(Class, Delta), Evidence), PCs),
    PCs \= [],
    max_by_value((MainBeneficiary, DeltaMain), PCs),
    config:param(beneficiary_gain_min, GainMin),
    DeltaMain >= GainMin,

    % Condition 4: Suppression/Resistance Alignment
    config:param(structural_suppression_min, SMin),
    config:param(structural_resistance_min, RMin),
    findall(ValS, member(supp(MainBeneficiary, structural, ValS), Evidence), Ss),
    findall(ValR, member(resist(MainBeneficiary, structural, ValR), Evidence), Rs),
    Ss \= [], Rs \= [],
    average_list(Ss, AvgS), AvgS >= SMin,
    average_list(Rs, AvgR), AvgR >= RMin.

/* ================================================================
   3. HELPERS & UTILS
   ================================================================ */

collect_intent_evidence(IntervalID, Evidence) :-
    findall(viable(S,A), intent_viable_alternative(IntervalID, S, A), VAs),
    findall(rejected(S,A), intent_alternative_rejected(IntervalID, S, A), RAs),
    findall(power(C,D), intent_power_change(IntervalID, C, D), PCs),
    findall(supp(C,L,V), intent_suppression_level(IntervalID, C, L, V), Supps),
    findall(resist(C,L,V), intent_resistance_level(IntervalID, C, L, V), Ress),
    append([VAs, RAs, PCs, Supps, Ress], Evidence).

classify_non_intent(Prelim, Pattern) :-
    (Prelim = increasing_coercion -> Pattern = increasing_coercion
    ; Prelim = decreasing_coercion -> Pattern = decreasing_coercion
    % OQ-93 coverage-carrying read: an OPEN grid track stays OPEN here —
    % mapping open(...) to stable would re-create the success-shaped
    % default this migration killed (absence must not read as a verdict).
    ; Prelim = open(Why) -> Pattern = open(Why)
    ; Pattern = stable).

refine_confidence(Evidence, DataScore, Conf) :-
    length(Evidence, NEv),
    config:param(data_high_threshold, DH),
    (DataScore >= DH, NEv >= 5 -> Conf = high ; Conf = medium).

fallback_confidence(DataScore, Conf) :-
    config:param(data_high_threshold, DH),
    (DataScore >= DH -> Conf = high ; Conf = low).

% Local helper predicates (not exported)
% FIXED: Renamed to avoid conflicts with library predicates

% Safe average that handles empty lists
average_list([], 0).
average_list(List, Avg) :- 
    List \= [],
    sum_list(List, Sum), 
    length(List, N), 
    Avg is Sum / N.

% Find tuple with maximum second element
% FIXED: Renamed from max_member to avoid conflict with library(lists)
max_by_value((C, D), List) :- 
    member((C, D), List), 
    \+ (member((_, D2), List), D2 > D).

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
