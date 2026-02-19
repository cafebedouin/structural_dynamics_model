% ============================================================================
% QUANTUM COMPLEXITY VERIFICATION REPORT
% ============================================================================
% Runs four testable predictions from the quantum complexity architecture
% evaluation against the full corpus. Reports results honestly — failed
% predictions are more interesting than confirmed ones.
%
% Standalone run:
%   cd prolog && swipl -l stack.pl -l covering_analysis.pl \
%     -l dirac_classification.pl -l grothendieck_cohomology.pl \
%     -l maxent_classifier.pl -l quantum_verification_report.pl \
%     -g "run_quantum_verification, halt."
% ============================================================================

:- module(quantum_verification_report, [
    run_quantum_verification/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(covering_analysis).
:- use_module(dirac_classification).
:- use_module(grothendieck_cohomology).
:- use_module(maxent_classifier).
:- use_module(corpus_loader).

:- use_module(library(lists)).

% ============================================================================
% STANDARD CONTEXTS (local copy — codebase convention)
% ============================================================================

standard_context(context(agent_power(powerless),
                        time_horizon(biographical),
                        exit_options(trapped),
                        spatial_scope(local))).

standard_context(context(agent_power(moderate),
                        time_horizon(biographical),
                        exit_options(mobile),
                        spatial_scope(national))).

standard_context(context(agent_power(institutional),
                        time_horizon(generational),
                        exit_options(arbitrage),
                        spatial_scope(national))).

standard_context(context(agent_power(analytical),
                        time_horizon(civilizational),
                        exit_options(analytical),
                        spatial_scope(global))).

% ============================================================================
% MAIN ENTRY POINT
% ============================================================================

run_quantum_verification :-
    format(user_error, '[qv] Loading corpus...~n', []),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),

    % Run MaxEnt (needed for queries 1 and 4)
    format(user_error, '[qv] Running classical MaxEnt...~n', []),
    maxent_classifier:maxent_run(Ctx, _),
    format(user_error, '[qv] Running indexed MaxEnt...~n', []),
    maxent_classifier:maxent_indexed_run(Ctx, _),

    % Pre-compute cohomology for the corpus
    format(user_error, '[qv] Computing cohomology...~n', []),
    grothendieck_cohomology:corpus_cohomology(_),

    format('<!-- QUANTUM_VERIFICATION_START -->~n'),
    format('# Quantum Complexity Verification Report~n~n'),

    query_maxent_divergence_vs_h1(Ctx),
    query_hub_conflict_vs_h1,
    query_restricted_vs_gauge_fixed,
    query_mountain_stability(Ctx),

    format('~n---~n~nEnd of report.~n').

% ============================================================================
% QUERY 1: MaxEnt Divergence vs H^1 Clustering
% ============================================================================
% Prediction: Constraints where classical and indexed MaxEnt diverge
% (divergence > 0.05) should have H^1 > 0.

query_maxent_divergence_vs_h1(Ctx) :-
    format('## Query 1: MaxEnt Divergence vs H^1 Clustering~n~n'),
    format('**Prediction:** High MaxEnt divergence (>0.05) clusters at H^1 > 0.~n~n'),

    % Get all divergent constraints
    maxent_classifier:maxent_indexing_divergences(Ctx, 0.05, Divergent),
    length(Divergent, NDivergent),
    format('### Raw Results~n~n'),
    format('Constraints with divergence > 0.05: **~w**~n~n', [NDivergent]),

    % For each, look up H^1
    findall(C-Div-H1, (
        member(C-Div, Divergent),
        grothendieck_cohomology:cohomological_obstruction(C, _, H1)
    ), DivH1Pairs),

    % Report each
    format('| Constraint | Divergence | H^1 |~n', []),
    format('|---|---|---|~n', []),
    sort(2, @>=, DivH1Pairs, SortedPairs),
    forall(member(C-Div-H1, SortedPairs),
           format('| ~w | ~4f | ~w |~n', [C, Div, H1])),
    format('~n', []),

    % Count with H^1 > 0
    include(has_h1_positive, DivH1Pairs, PositiveH1),
    length(PositiveH1, NPositive),
    (   NDivergent > 0
    ->  FracPositive is NPositive / NDivergent * 100
    ;   FracPositive = 0.0
    ),
    format('High-divergence with H^1 > 0: **~w/~w (~1f%)**~n~n', [NPositive, NDivergent, FracPositive]),

    % Reverse direction: what fraction of H^1 > 0 constraints have high divergence?
    findall(C2, (
        grothendieck_cohomology:cached_obstruction(C2, 0, H1_2),
        H1_2 > 0
    ), AllH1Positive),
    length(AllH1Positive, NH1Positive),
    findall(C3, (
        member(C3, AllH1Positive),
        maxent_classifier:maxent_indexing_divergence(C3, Ctx, D3),
        D3 > 0.05
    ), H1WithHighDiv),
    length(H1WithHighDiv, NH1WithHighDiv),
    (   NH1Positive > 0
    ->  FracReverse is NH1WithHighDiv / NH1Positive * 100
    ;   FracReverse = 0.0
    ),
    format('**Reverse direction:** H^1 > 0 constraints with high divergence: **~w/~w (~1f%)**~n~n',
           [NH1WithHighDiv, NH1Positive, FracReverse]),

    % H^1 distribution of divergent constraints
    format('H^1 distribution of high-divergence constraints:~n', []),
    forall(member(V, [0, 1, 2, 3, 4, 5, 6]), (
        include(has_h1_value(V), DivH1Pairs, AtV),
        length(AtV, NAtV),
        (NAtV > 0 -> format('  H^1=~w: ~w~n', [V, NAtV]) ; true)
    )),
    format('~n', []),

    % Interpretation
    format('### Prediction Outcome~n~n'),
    (   NDivergent =:= 0
    ->  format('**Cannot evaluate** — no constraints with divergence > 0.05.~n~n')
    ;   FracPositive >= 75.0
    ->  format('**Confirmed.** ~1f% of high-divergence constraints have H^1 > 0.~n~n', [FracPositive])
    ;   FracPositive >= 50.0
    ->  format('**Partially confirmed.** ~1f% of high-divergence constraints have H^1 > 0 (predicted >= 75%).~n~n', [FracPositive])
    ;   format('**Disconfirmed.** Only ~1f% of high-divergence constraints have H^1 > 0.~n~n', [FracPositive])
    ),

    % Reverse interpretation
    (   NH1Positive > 0
    ->  (   FracReverse < 15.0
        ->  format('**Reverse finding:** Only ~1f% of H^1 > 0 constraints show high MaxEnt divergence. ',
                    [FracReverse]),
            format('H^1 detects a broader class of observer-dependence than what MaxEnt captures. ',  []),
            format('The cohomological formalism sees structure that the classical oracle misses.~n~n', [])
        ;   format('**Reverse finding:** ~1f% of H^1 > 0 constraints show high MaxEnt divergence.~n~n',
                    [FracReverse])
        )
    ;   true
    ),
    format('---~n~n').

has_h1_positive(_-_-H1) :- H1 > 0.
has_h1_value(V, _-_-H1) :- H1 =:= V.

% ============================================================================
% QUERY 2: Hub-Conflict Constraints and H^1 Bands
% ============================================================================
% Prediction: Constraints where Hub 1 (sigmoid) and Hub 2 (immutability)
% give conflicting signals should cluster in higher H^1 bands.
%
% Hub conflict: constraint where the orbit contains mountain at one context
% and non-mountain at another, AND the metrics (BaseEps, Supp) are
% mountain-compatible — meaning the non-mountain classification is driven
% by Hub 2 (immutability = rope), not by metric thresholds.

query_hub_conflict_vs_h1 :-
    format('## Query 2: Hub-Conflict Constraints and H^1 Bands~n~n'),
    format('**Prediction:** Hub-conflict constraints cluster at H^1 >= 4.~n~n'),

    covering_analysis:all_corpus_constraints(Constraints),

    % Type A: Constraint IS classified as mountain at context Ctx,
    % but Chi at that context exceeds snare_chi_floor (0.66) — Hub 1 sigmoid
    % pushes extraction into snare territory while Hub 2 locks it as mountain.
    config:param(snare_chi_floor, SnareChi),
    findall(C-Ctx-Chi, (
        member(C, Constraints),
        standard_context(Ctx),
        drl_core:dr_type(C, Ctx, mountain),
        constraint_indexing:extractiveness_for_agent(C, Ctx, Chi),
        Chi > SnareChi
    ), TypeAConflicts),
    sort(1, @<, TypeAConflicts, TypeASorted),
    remove_dup_constraints(TypeASorted, TypeAUnique),
    length(TypeAUnique, NTypeA),

    format('### Type A Conflicts~n'),
    format('Classified as mountain but Chi > ~4f (snare-level extraction)~n~n', [SnareChi]),
    format('Found: **~w** constraints~n~n', [NTypeA]),

    (   NTypeA > 0
    ->  format('| Constraint | Context Power | Chi | H^1 |~n', []),
        format('|---|---|---|---|~n', []),
        forall(member(C1-Ctx1-Chi1, TypeAUnique), (
            Ctx1 = context(agent_power(P1), _, _, _),
            grothendieck_cohomology:cohomological_obstruction(C1, _, H1_1),
            format('| ~w | ~w | ~4f | ~w |~n', [C1, P1, Chi1, H1_1])
        )),
        format('~n', [])
    ;   true
    ),

    % Type B: Orbit-based — constraint classified as mountain at one context
    % but NOT mountain at another, where metrics are mountain-compatible.
    % This catches where Hub 2 (immutability flip) is the sole driver.
    config:param(mountain_suppression_ceiling, SuppCeil),
    config:param(mountain_extractiveness_max, MaxX),
    findall(C, (
        member(C, Constraints),
        % Must have mountain at one context
        standard_context(MtnCtx),
        drl_core:dr_type(C, MtnCtx, mountain),
        % Must have non-mountain at another
        standard_context(OtherCtx),
        OtherCtx \= MtnCtx,
        drl_core:dr_type(C, OtherCtx, OtherType),
        OtherType \= mountain,
        % Metrics are mountain-compatible (structural, not per-context)
        drl_core:base_extractiveness(C, BaseEps),
        BaseEps =< MaxX,
        drl_core:get_raw_suppression(C, Supp),
        Supp =< SuppCeil,
        drl_core:emerges_naturally(C)
    ), TypeBRaw),
    sort(TypeBRaw, TypeBUnique),
    length(TypeBUnique, NTypeB),

    format('### Type B Conflicts~n'),
    format('Mountain at one context, non-mountain at another, with mountain-compatible metrics~n'),
    format('(Hub 2 immutability flip is the sole driver of classification change)~n~n'),
    format('Found: **~w** constraints~n~n', [NTypeB]),

    (   NTypeB > 0
    ->  format('| Constraint | Mountain Context | Other Context | Other Type | H^1 |~n', []),
        format('|---|---|---|---|---|~n', []),
        forall(member(CB, TypeBUnique), (
            standard_context(MCtx),
            drl_core:dr_type(CB, MCtx, mountain),
            standard_context(OCtx), OCtx \= MCtx,
            drl_core:dr_type(CB, OCtx, OT), OT \= mountain, !,
            MCtx = context(agent_power(MP), _, _, _),
            OCtx = context(agent_power(OP), _, _, _),
            grothendieck_cohomology:cohomological_obstruction(CB, _, H1B),
            format('| ~w | ~w | ~w | ~w | ~w |~n', [CB, MP, OP, OT, H1B])
        )),
        format('~n', [])
    ;   true
    ),

    % Combined hub-conflict H^1 distribution
    append(TypeAUnique, TypeBUnique, AllConflictsRaw0),
    maplist(extract_constraint_id, AllConflictsRaw0, AllConflictIDs0),
    sort(AllConflictIDs0, AllConflictIDs),
    length(AllConflictIDs, NAllConflicts),

    format('### Combined Hub-Conflict H^1 Distribution~n~n'),
    format('Total unique hub-conflict constraints: **~w**~n~n', [NAllConflicts]),

    forall(member(V, [0, 1, 2, 3, 4, 5, 6]), (
        findall(CC, (
            member(CC, AllConflictIDs),
            grothendieck_cohomology:cohomological_obstruction(CC, _, V)
        ), AtV),
        length(AtV, NAtV),
        (NAtV > 0 -> format('  H^1=~w: ~w constraints~n', [V, NAtV]) ; true)
    )),
    format('~n', []),

    % Interpretation
    format('### Prediction Outcome~n~n'),
    findall(CC2, (
        member(CC2, AllConflictIDs),
        grothendieck_cohomology:cohomological_obstruction(CC2, _, H1CC),
        H1CC >= 4
    ), HighH1Conflicts),
    length(HighH1Conflicts, NHighH1),
    (   NAllConflicts > 0
    ->  FracHigh is NHighH1 / NAllConflicts * 100,
        (   FracHigh >= 50.0
        ->  format('**Confirmed.** ~1f% of hub-conflict constraints have H^1 >= 4.~n~n', [FracHigh])
        ;   FracHigh >= 25.0
        ->  format('**Partially confirmed.** ~1f% of hub-conflict constraints have H^1 >= 4.~n~n', [FracHigh])
        ;   format('**Disconfirmed.** Only ~1f% of hub-conflict constraints have H^1 >= 4.~n~n', [FracHigh])
        )
    ;   format('**Cannot evaluate** — no hub-conflict constraints found.~n~n', [])
    ),
    format('---~n~n').

% Helper: extract constraint ID from various tuple forms
extract_constraint_id(C-_-_, C).
extract_constraint_id(C, C) :- atom(C).

% Remove duplicate constraints (keep first occurrence with its context/chi)
remove_dup_constraints([], []).
remove_dup_constraints([C-Ctx-Chi|Rest], [C-Ctx-Chi|Filtered]) :-
    exclude(same_constraint(C), Rest, RestFiltered),
    remove_dup_constraints(RestFiltered, Filtered).
same_constraint(C, C-_-_).

% ============================================================================
% QUERY 3: Restricted-View Divergence vs gauge_fixed
% ============================================================================
% Prediction: Restricted-view divergence overlaps >= 70% with gauge_fixed.

query_restricted_vs_gauge_fixed :-
    format('## Query 3: Restricted-View Divergence vs gauge_fixed~n~n'),
    format('**Prediction:** >= 70% overlap between restricted-view divergence and gauge_fixed.~n~n'),

    covering_analysis:all_corpus_constraints(Constraints),
    length(Constraints, NConstraints),

    % For each constraint × standard context, compute:
    %   - dr_type full classification
    %   - classify_from_restricted result
    %   - gauge_fixed status
    findall(result(C, Ctx, Power, FullType, RestType, GF), (
        member(C, Constraints),
        standard_context(Ctx),
        Ctx = context(agent_power(Power), _, _, _),
        drl_core:dr_type(C, Ctx, FullType),
        constraint_indexing:classify_from_restricted(C, Ctx, RestType),
        (   dirac_classification:gauge_fixed(C, Ctx, GFVal)
        ->  GF = GFVal
        ;   GF = false
        )
    ), AllResults),
    length(AllResults, NTotal),

    format('### Raw Counts~n~n'),
    format('Total (Constraint, Context) pairs evaluated: **~w** (~w constraints x 4 contexts)~n~n',
           [NTotal, NConstraints]),

    % Count indeterminate by context
    format('**Restricted classification outcomes by context:**~n~n'),
    format('| Context | Indeterminate | Definite (non-indet) | Differs from full |~n', []),
    format('|---|---|---|---|~n', []),
    forall(member(P, [powerless, moderate, institutional, analytical]), (
        include(result_at_power(P), AllResults, AtP),
        length(AtP, NAtP),
        include(result_indeterminate, AtP, IndetAtP),
        length(IndetAtP, NIndet),
        NDefinite is NAtP - NIndet,
        include(result_definite_different, AtP, DiffAtP),
        length(DiffAtP, NDiff),
        format('| ~w | ~w | ~w | ~w |~n', [P, NIndet, NDefinite, NDiff])
    )),
    format('~n', []),

    % Set A: restricted ≠ full AND restricted ≠ indeterminate
    include(result_definite_different, AllResults, SetA),
    length(SetA, NA),

    % Set B: gauge_fixed = true
    include(result_gauge_fixed, AllResults, SetB),
    length(SetB, NB),

    % Intersection: A ∩ B
    include(result_definite_different, SetB, AintersectB),
    length(AintersectB, NAB),

    format('### Overlap Analysis~n~n'),
    format('- Set A (restricted differs from full, non-indeterminate): **~w** pairs~n', [NA]),
    format('- Set B (gauge_fixed = true): **~w** pairs~n', [NB]),
    format('- A ∩ B: **~w** pairs~n~n', [NAB]),

    (   NA > 0
    ->  OverlapAB is NAB / NA * 100,
        format('- Fraction of A that is gauge_fixed: **~1f%** (~w/~w)~n', [OverlapAB, NAB, NA])
    ;   OverlapAB = 0.0,
        format('- Fraction of A that is gauge_fixed: N/A (A is empty)~n', [])
    ),
    (   NB > 0
    ->  OverlapBA is NAB / NB * 100,
        format('- Fraction of gauge_fixed that differs in restricted: **~1f%** (~w/~w)~n~n', [OverlapBA, NAB, NB])
    ;   OverlapBA = 0.0,
        format('- Fraction of gauge_fixed that differs in restricted: N/A (B is empty)~n~n', [])
    ),

    % Sample mismatches: in A but not B, and in B but not A
    include(result_not_gauge_fixed, SetA, AnotB),
    length(AnotB, NAnotB),
    include(result_definite_same, SetB, BnotA),
    length(BnotA, NBnotA),

    format('### Mismatch Analysis~n~n'),
    format('- In A but not B (restricted differs, not gauge_fixed): **~w**~n', [NAnotB]),
    (   NAnotB > 0
    ->  format('  Interpretation: accessibility table is more restrictive than gauge_fixed.~n'),
        format('  These are cases where restricted data produces a wrong answer even though~n'),
        format('  the full classifier does not identify gauge-dependence.~n~n'),
        % Show up to 5 examples
        format('  Sample (up to 5):~n'),
        take_n(5, AnotB, SampleAnotB),
        forall(member(result(SC, _, SP, SFull, SRest, _), SampleAnotB),
               format('    ~w at ~w: full=~w, restricted=~w~n', [SC, SP, SFull, SRest])),
        format('~n')
    ;   true
    ),
    format('- In B but not A (gauge_fixed, but restricted agrees): **~w**~n', [NBnotA]),
    (   NBnotA > 0
    ->  format('  Interpretation: gauge_fixed detects frame-dependence that does not~n'),
        format('  affect the restricted classification outcome.~n~n')
    ;   true
    ),

    % Prediction outcome
    format('~n### Prediction Outcome~n~n'),
    (   NA =:= 0, NB =:= 0
    ->  format('**Cannot evaluate** — both sets are empty.~n~n')
    ;   NA =:= 0
    ->  format('**Cannot evaluate** — no non-indeterminate restricted divergences.~n~n')
    ;   OverlapAB >= 70.0, OverlapBA >= 70.0
    ->  format('**Confirmed.** Bidirectional overlap >= 70%.~n~n')
    ;   (OverlapAB >= 70.0 ; OverlapBA >= 70.0)
    ->  format('**Partially confirmed.** One direction >= 70%: A→B=~1f%, B→A=~1f%.~n~n', [OverlapAB, OverlapBA])
    ;   format('**Disconfirmed.** Bidirectional overlap below 70%: A→B=~1f%, B→A=~1f%.~n~n', [OverlapAB, OverlapBA])
    ),
    format('---~n~n').

% Result selectors
result_at_power(P, result(_, _, P, _, _, _)).
result_indeterminate(result(_, _, _, _, indeterminate, _)).
result_definite_different(result(_, _, _, Full, Rest, _)) :-
    Rest \= indeterminate, Rest \= Full.
result_definite_same(result(_, _, _, Full, Rest, _)) :-
    Rest \= indeterminate, Rest = Full.
result_gauge_fixed(result(_, _, _, _, _, true)).
result_not_gauge_fixed(result(_, _, _, _, _, false)).

% Take first N elements
take_n(_, [], []) :- !.
take_n(0, _, []) :- !.
take_n(N, [H|T], [H|R]) :- N > 0, N1 is N - 1, take_n(N1, T, R).

% ============================================================================
% QUERY 4: Mountain Stability Under Indexing
% ============================================================================
% Prediction: Genuine mountains (H^0, base_extractiveness < 0.05) should
% show near-zero MaxEnt divergence between classical and indexed runs.

query_mountain_stability(Ctx) :-
    format('## Query 4: Mountain Stability Under Indexing~n~n'),
    format('**Prediction:** Genuine mountains (H^0, epsilon < 0.05) have divergence < 0.01.~n~n'),

    % Find all constraints preserved as mountain across all contexts
    covering_analysis:all_corpus_constraints(Constraints),
    findall(C, (
        member(C, Constraints),
        dirac_classification:preserved_under_context_shift(C, preserved(mountain))
    ), PreservedMountains),
    length(PreservedMountains, NPres),
    format('Constraints preserved as mountain (H^0): **~w**~n', [NPres]),

    % Filter to base_extractiveness < 0.05
    findall(C-BE, (
        member(C, PreservedMountains),
        drl_core:base_extractiveness(C, BE),
        BE < 0.05
    ), GenuineMountains),
    length(GenuineMountains, NGenuine),
    format('Genuine mountains (epsilon < 0.05): **~w**~n~n', [NGenuine]),

    (   NGenuine =:= 0
    ->  format('**Cannot evaluate** — no genuine mountains with epsilon < 0.05 found.~n'),
        format('Expanding to epsilon < 0.15 (mountain_extractiveness_max = 0.25):~n~n'),
        findall(C2-BE2, (
            member(C2, PreservedMountains),
            drl_core:base_extractiveness(C2, BE2),
            BE2 < 0.15
        ), ExpandedMountains),
        length(ExpandedMountains, NExpanded),
        format('Mountains with epsilon < 0.15: **~w**~n~n', [NExpanded]),
        report_mountain_divergences(ExpandedMountains, Ctx)
    ;   report_mountain_divergences(GenuineMountains, Ctx)
    ),
    format('~n---~n~n').

report_mountain_divergences(Mountains, Ctx) :-
    length(Mountains, N),
    (   N =:= 0
    ->  format('No mountains to analyze.~n~n')
    ;   findall(C-BE-Div, (
            member(C-BE, Mountains),
            (   maxent_classifier:maxent_indexing_divergence(C, Ctx, Div)
            ->  true
            ;   Div = -1.0  % No MaxEnt data
            )
        ), MountainDivs),

        % Filter out those without MaxEnt data
        include(has_valid_div, MountainDivs, ValidDivs),
        length(ValidDivs, NValid),

        (   NValid =:= 0
        ->  format('No mountains with both classical and indexed MaxEnt data.~n~n')
        ;   % Compute stats
            findall(D, member(_-_-D, ValidDivs), DivList),
            max_list(DivList, MaxDiv),
            sum_list(DivList, SumDiv),
            MeanDiv is SumDiv / NValid,

            format('### Results~n~n'),
            format('Mountains with MaxEnt data: **~w**~n', [NValid]),
            format('Maximum divergence: **~4f**~n', [MaxDiv]),
            format('Mean divergence: **~4f**~n~n', [MeanDiv]),

            % Report any with divergence > 0.02
            findall(C2-BE2-Div2, (
                member(C2-BE2-Div2, ValidDivs),
                Div2 > 0.02
            ), HighDivMountains),
            length(HighDivMountains, NHighDiv),
            (   NHighDiv > 0
            ->  format('**Anomalous mountains** (divergence > 0.02): ~w~n~n', [NHighDiv]),
                format('| Constraint | Base Eps | Divergence |~n', []),
                format('|---|---|---|~n', []),
                forall(member(CA-BA-DA, HighDivMountains),
                       format('| ~w | ~4f | ~4f |~n', [CA, BA, DA])),
                format('~n', [])
            ;   format('No anomalous mountains (all divergence <= 0.02).~n~n', [])
            ),

            % Full table for all mountains
            format('### Full Table~n~n'),
            format('| Constraint | Base Eps | Divergence |~n', []),
            format('|---|---|---|~n', []),
            sort(2, @>=, ValidDivs, SortedDivs),
            forall(member(CM-BM-DM, SortedDivs),
                   format('| ~w | ~4f | ~4f |~n', [CM, BM, DM])),
            format('~n', []),

            % Prediction outcome
            format('### Prediction Outcome~n~n'),
            (   MaxDiv < 0.01
            ->  format('**Confirmed.** All genuine mountains have divergence < 0.01.~n~n')
            ;   MaxDiv < 0.02
            ->  format('**Mostly confirmed.** Max divergence ~4f (< 0.02, slightly above 0.01 threshold).~n~n', [MaxDiv])
            ;   format('**Disconfirmed.** Max divergence ~4f exceeds 0.02. Investigating sigmoid behavior at low epsilon.~n~n', [MaxDiv])
            )
        )
    ).

has_valid_div(_-_-Div) :- Div >= 0.0.
