% ============================================================================
% GAP DIAGNOSTIC: Institutional-to-Moderate Transition Analysis
% ============================================================================
% Investigates the d=0.000→0.646 gap where 4,023 of 4,231 missed
% transitions occur. Characterizes constraints that classify as
% `unknown` at the midpoint d=0.323 and determines whether the gap
% is well-behaved, structured, or genuinely ambiguous.
%
% Usage:
%   cd prolog && swipl -l stack.pl -l covering_analysis.pl \
%     -l gap_diagnostic.pl -g run_gap_diagnostic -t halt \
%     > ../outputs/gap_diagnostic.md
% ============================================================================

:- module(gap_diagnostic, [
    run_gap_diagnostic/0,
    gap_group/2,
    unknown_band/3,
    gap_transition/3
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(covering_analysis).
:- use_module(library(lists)).

:- dynamic gap_group/2.          % gap_group(Constraint, group_a|group_b|group_c)
:- dynamic unknown_band/3.       % unknown_band(Constraint, D_enter, D_exit)
:- dynamic gap_transition/3.     % gap_transition(Constraint, InstType, ModType)
:- dynamic gap_profile/6.        % gap_profile(C, BaseEps, Chi323, Supp, InstType, ModType)
:- dynamic gap_structural/5.     % gap_structural(C, HasCoord, HasAsymm, HasEnforce, Emerges)

% d-values for the analysis
midpoint_d(0.323).
institutional_d(D) :- config:param(canonical_d_institutional, D).
moderate_d(D) :- config:param(canonical_d_moderate, D).

% Canonical scope sigmas for the analysis
analysis_sigmas([0.8, 1.0, 1.2]).

/* ================================================================
   STEP 1: CATALOG THE GAP POPULATION
   ================================================================ */

%% classify_gap_groups(+Constraints)
%  Partitions all constraints into groups A, B, C based on behavior
%  at d=0.323. Grouping is per-constraint (a constraint is in Group A
%  if it's unknown at d=0.323 for ANY canonical scope level).
classify_gap_groups(Constraints) :-
    retractall(gap_group(_, _)),
    retractall(gap_profile(_, _, _, _, _, _)),
    retractall(gap_structural(_, _, _, _, _)),
    retractall(gap_transition(_, _, _)),
    retractall(unknown_band(_, _, _)),
    midpoint_d(MidD),
    institutional_d(D_inst),
    moderate_d(D_mod),
    analysis_sigmas(Sigmas),
    forall(member(C, Constraints), (
        classify_constraint_group(C, MidD, D_inst, D_mod, Sigmas)
    )).

classify_constraint_group(C, MidD, D_inst, D_mod, Sigmas) :-
    % Classify at midpoint for all scope levels
    findall(T,
        (   member(S, Sigmas),
            covering_analysis:classify_at_interpolated(C, MidD, S, T)
        ),
        MidTypes
    ),
    % Classify at institutional for all scope levels
    findall(T,
        (   member(S, Sigmas),
            covering_analysis:classify_at_interpolated(C, D_inst, S, T)
        ),
        InstTypes
    ),
    % Classify at moderate for all scope levels
    findall(T,
        (   member(S, Sigmas),
            covering_analysis:classify_at_interpolated(C, D_mod, S, T)
        ),
        ModTypes
    ),
    % Determine group
    (   member(unknown, MidTypes)
    ->  assertz(gap_group(C, group_a)),
        % Cache profile for Group A analysis
        cache_gap_profile(C, MidD, InstTypes, ModTypes)
    ;   % Check if mid-types differ from BOTH neighbors
        (   midtype_differs_from_both(MidTypes, InstTypes, ModTypes)
        ->  assertz(gap_group(C, group_b)),
            cache_transition(C, InstTypes, ModTypes)
        ;   assertz(gap_group(C, group_c)),
            cache_transition(C, InstTypes, ModTypes)
        )
    ),
    !.
classify_constraint_group(C, _, _, _, _) :-
    assertz(gap_group(C, group_c)).

%% midtype_differs_from_both(+MidTypes, +InstTypes, +ModTypes)
%  True if the midpoint types are different from both canonical neighbors.
%  Uses the "primary" type (national scope, sigma=1.0, which is the 2nd element).
midtype_differs_from_both(MidTypes, InstTypes, ModTypes) :-
    nth1(2, MidTypes, MidPrimary),
    nth1(2, InstTypes, InstPrimary),
    nth1(2, ModTypes, ModPrimary),
    MidPrimary \== InstPrimary,
    MidPrimary \== ModPrimary.

%% cache_gap_profile(+C, +MidD, +InstTypes, +ModTypes)
%  Caches metric profile and structural flags for Group A constraints.
cache_gap_profile(C, MidD, InstTypes, ModTypes) :-
    % Get base metrics
    config:param(extractiveness_metric_name, ExtName),
    (   narrative_ontology:constraint_metric(C, ExtName, BaseEps)
    ->  true
    ;   BaseEps = 0.5
    ),
    constraint_indexing:sigmoid_f(MidD, FD),
    Chi323 is BaseEps * FD * 1.0,  % at national scope (sigma=1.0)
    config:param(suppression_metric_name, SuppName),
    (   narrative_ontology:constraint_metric(C, SuppName, Supp)
    ->  true
    ;   Supp = 0
    ),
    % Canonical types (use national scope = 2nd element)
    nth1(2, InstTypes, InstType),
    nth1(2, ModTypes, ModType),
    assertz(gap_profile(C, BaseEps, Chi323, Supp, InstType, ModType)),
    assertz(gap_transition(C, InstType, ModType)),
    % Structural flags
    (   narrative_ontology:has_coordination_function(C)
    ->  HasCoord = true ; HasCoord = false
    ),
    (   narrative_ontology:has_asymmetric_extraction(C)
    ->  HasAsymm = true ; HasAsymm = false
    ),
    (   drl_core:requires_active_enforcement(C)
    ->  HasEnforce = true ; HasEnforce = false
    ),
    (   drl_core:emerges_naturally(C)
    ->  Emerges = true ; Emerges = false
    ),
    assertz(gap_structural(C, HasCoord, HasAsymm, HasEnforce, Emerges)).

cache_transition(C, InstTypes, ModTypes) :-
    nth1(2, InstTypes, InstType),
    nth1(2, ModTypes, ModType),
    assertz(gap_transition(C, InstType, ModType)).

/* ================================================================
   STEP 2: PROFILE GROUP A — THRESHOLD ANALYSIS
   ================================================================ */

%% threshold_proximity(+C, -Proximities)
%  For a Group A constraint, identifies which classification thresholds
%  its chi at d=0.323 is closest to but hasn't crossed.
threshold_proximity(C, Proximities) :-
    gap_profile(C, BaseEps, Chi323, Supp, _, _),
    gap_structural(C, HasCoord, _, HasEnforce, Emerges),
    findall(
        prox(ThreshName, Distance, Direction),
        (   threshold_boundary(ThreshName, ThreshVal, MetricUsed),
            (   MetricUsed = chi -> Val = Chi323
            ;   MetricUsed = epsilon -> Val = BaseEps
            ;   MetricUsed = suppression -> Val = Supp
            ),
            Distance is abs(Val - ThreshVal),
            (   Val < ThreshVal -> Direction = below
            ;   Direction = above
            )
        ),
        AllProx
    ),
    sort(AllProx, Proximities),
    % Also determine the specific gate that blocks classification
    determine_blocking_gate(C, BaseEps, Chi323, Supp, HasCoord, HasEnforce, Emerges).

%% threshold_boundary(+Name, +Value, +Metric)
threshold_boundary(rope_chi_ceiling, V, chi) :- config:param(rope_chi_ceiling, V).
threshold_boundary(rope_epsilon_ceiling, V, epsilon) :- config:param(rope_epsilon_ceiling, V).
threshold_boundary(scaffold_extraction_ceil, V, chi) :- config:param(scaffold_extraction_ceil, V).
threshold_boundary(tangled_rope_chi_floor, V, chi) :- config:param(tangled_rope_chi_floor, V).
threshold_boundary(tangled_rope_epsilon_floor, V, epsilon) :- config:param(tangled_rope_epsilon_floor, V).
threshold_boundary(tangled_rope_suppression_floor, V, suppression) :- config:param(tangled_rope_suppression_floor, V).
threshold_boundary(snare_chi_floor, V, chi) :- config:param(snare_chi_floor, V).
threshold_boundary(piton_extraction_ceiling, V, chi) :- config:param(piton_extraction_ceiling, V).

%% determine_blocking_gate(+C, +BaseEps, +Chi, +Supp, +HasCoord, +HasEnforce, +Emerges)
%  Identifies exactly WHY this constraint falls to unknown.
:- dynamic blocking_gate/2.

determine_blocking_gate(C, BaseEps, Chi323, _Supp, HasCoord, HasEnforce, Emerges) :-
    retractall(blocking_gate(C, _)),
    % Check each clause in classifier order
    % Mountain: already checked by classifier — Supp or BaseEps or immutability
    % Snare: Chi too low
    (   Chi323 < 0.66
    ->  assertz(blocking_gate(C, snare_chi_too_low))
    ;   true
    ),
    % Scaffold gates
    (   Chi323 =< 0.30
    ->  (   HasCoord = false
        ->  assertz(blocking_gate(C, scaffold_no_coordination))
        ;   true
        )
    ;   assertz(blocking_gate(C, scaffold_chi_too_high))
    ),
    % Rope gates (the critical one)
    (   Chi323 =< 0.35
    ->  (   BaseEps > 0.45, Chi323 > 0
        ->  assertz(blocking_gate(C, rope_epsilon_too_high))
        ;   true
        ),
        (   Emerges = false
        ->  true  % immutability gate may also matter but context-dependent
        ;   true
        )
    ;   assertz(blocking_gate(C, rope_chi_too_high))
    ),
    % Tangled rope: Chi too low
    (   Chi323 < 0.40
    ->  assertz(blocking_gate(C, tangled_rope_chi_too_low))
    ;   true
    ),
    % Tangled rope structural gates
    (   HasCoord = false
    ->  assertz(blocking_gate(C, tangled_rope_no_coordination))
    ;   true
    ),
    (   HasEnforce = false
    ->  assertz(blocking_gate(C, tangled_rope_no_enforcement))
    ;   true
    ).

/* ================================================================
   STEP 2.4: BINARY SEARCH FOR UNKNOWN-BAND BOUNDARIES
   ================================================================ */

%% compute_unknown_bands(+GroupAConstraints)
%  For each Group A constraint, finds the d-range where it's unknown.
compute_unknown_bands(GroupAConstraints) :-
    retractall(unknown_band(_, _, _)),
    institutional_d(D_inst),
    moderate_d(D_mod),
    midpoint_d(MidD),
    length(GroupAConstraints, N),
    format(user_error, '[gap] Computing unknown bands for ~w Group A constraints...~n', [N]),
    forall(member(C, GroupAConstraints), (
        % Search downward from midpoint toward institutional
        binary_search_boundary(C, D_inst, MidD, 1.0, down, D_enter),
        % Search upward from midpoint toward moderate
        binary_search_boundary(C, MidD, D_mod, 1.0, up, D_exit),
        assertz(unknown_band(C, D_enter, D_exit))
    )).

%% binary_search_boundary(+C, +Lo, +Hi, +Sigma, +Direction, -Boundary)
%  Finds the d-value where classification transitions into/out of unknown.
%  Direction = down: searching from Hi downward (finding where unknown starts)
%  Direction = up: searching from Lo upward (finding where unknown ends)
%  Tolerance: 0.005
binary_search_boundary(C, Lo, Hi, Sigma, Direction, Boundary) :-
    Width is Hi - Lo,
    (   Width < 0.005
    ->  (   Direction = down -> Boundary = Lo ; Boundary = Hi )
    ;   Mid is (Lo + Hi) / 2,
        covering_analysis:classify_at_interpolated(C, Mid, Sigma, Type),
        (   Direction = down
        ->  (   Type == unknown
            ->  % Unknown extends below Mid — search lower half
                binary_search_boundary(C, Lo, Mid, Sigma, down, Boundary)
            ;   % Not unknown — boundary is between Mid and Hi
                binary_search_boundary(C, Mid, Hi, Sigma, down, Boundary)
            )
        ;   % Direction = up
            (   Type == unknown
            ->  % Unknown extends above Mid — search upper half
                binary_search_boundary(C, Mid, Hi, Sigma, up, Boundary)
            ;   % Not unknown — boundary is between Lo and Mid
                binary_search_boundary(C, Lo, Mid, Sigma, up, Boundary)
            )
        )
    ).

/* ================================================================
   STEP 3: PATTERN ANALYSIS
   ================================================================ */

%% transition_pair_analysis(-PairFreqs)
%  Counts frequency of (institutional_type → moderate_type) pairs
%  among Group A constraints.
transition_pair_analysis(PairFreqs) :-
    findall(InstType-ModType,
        (   gap_group(C, group_a),
            gap_transition(C, InstType, ModType)
        ),
        Pairs),
    msort(Pairs, Sorted),
    clumped(Sorted, PairFreqs).

%% threshold_cluster_analysis(-Clusters)
%  Determines which blocking gates are most common among Group A.
threshold_cluster_analysis(Clusters) :-
    findall(Gate,
        (   gap_group(C, group_a),
            blocking_gate(C, Gate)
        ),
        Gates),
    msort(Gates, Sorted),
    clumped(Sorted, Clusters).

%% scope_sensitivity_at_midpoint(-Results)
%  For Group A constraints, checks if classification varies across
%  scope levels at d=0.323.
scope_sensitivity_at_midpoint(Results) :-
    midpoint_d(MidD),
    analysis_sigmas(Sigmas),
    findall(
        scope_result(C, TypesAtMid, Varies),
        (   gap_group(C, group_a),
            findall(T,
                (   member(S, Sigmas),
                    covering_analysis:classify_at_interpolated(C, MidD, S, T)
                ),
                TypesAtMid
            ),
            sort(TypesAtMid, Unique),
            length(Unique, NUniq),
            (NUniq > 1 -> Varies = yes ; Varies = no)
        ),
        Results
    ).

/* ================================================================
   REPORT GENERATION
   ================================================================ */

run_gap_diagnostic :-
    format('# Gap Diagnostic: Institutional-to-Moderate Transition~n~n'),
    format('*Characterizes the d=0.000 to d=0.646 gap where 80% of missed transitions occur.*~n~n'),
    format('---~n~n'),
    % Load corpus
    corpus_loader:load_all_testsets,
    covering_analysis:all_corpus_constraints(Cs),
    length(Cs, NC),
    format(user_error, '[gap] Analyzing ~w constraints...~n', [NC]),
    % Step 1: Classify groups
    classify_gap_groups(Cs),
    report_group_sizes(NC),
    % Step 2: Profile Group A
    findall(C, gap_group(C, group_a), GroupA),
    length(GroupA, NA),
    (   NA > 0
    ->  report_group_a_profile(GroupA),
        % Step 2.3: Threshold proximity
        forall(member(C, GroupA), (
            threshold_proximity(C, _)
        )),
        report_threshold_proximity,
        % Step 2.4: Unknown band widths
        compute_unknown_bands(GroupA),
        report_unknown_bands,
        % Step 3: Pattern analysis
        report_transition_pairs,
        report_threshold_clusters,
        report_scope_sensitivity,
        % Step 4: Recommendation
        report_recommendation_gap(NC, NA)
    ;   format('No Group A constraints found. The gap may not produce unknown classifications.~n~n')
    ),
    % Embedded Prolog facts
    report_prolog_facts,
    format('---~n~n'),
    format('*End of gap diagnostic*~n').

/* ── Section reporters ──────────────────────────────────────────── */

report_group_sizes(NC) :-
    format('## Step 1: Gap Population Catalog~n~n'),
    findall(C, gap_group(C, group_a), GA), length(GA, NA),
    findall(C, gap_group(C, group_b), GB), length(GB, NB),
    findall(C, gap_group(C, group_c), GC), length(GC, NCC),
    Total is NA + NB + NCC,
    PctA is (NA * 100) / max(1, NC),
    PctB is (NB * 100) / max(1, NC),
    PctC is (NCC * 100) / max(1, NC),
    format('Grouping is per-constraint: a constraint is in Group A if it classifies as '),
    format('`unknown` at d=0.323 for ANY of the three canonical scope levels (local/national/global).~n~n'),
    format('| Group | Description | Count | Pct |~n'),
    format('|-------|-------------|-------|-----|~n'),
    format('| A | Unknown at d=0.323 | ~w | ~1f% |~n', [NA, PctA]),
    format('| B | Classified but differs from both neighbors | ~w | ~1f% |~n', [NB, PctB]),
    format('| C | Same as one canonical neighbor | ~w | ~1f% |~n', [NCC, PctC]),
    format('| **Total** | | **~w** | |~n~n', [Total]),
    (   Total =\= NC
    ->  format('*Note: Total (~w) differs from corpus (~w) due to classification errors.*~n~n', [Total, NC])
    ;   true
    ).

report_group_a_profile(GroupA) :-
    format('## Step 2: Group A Profile (Unknown at d=0.323)~n~n'),
    length(GroupA, NA),
    format('**~w constraints** classify as `unknown` at the gap midpoint.~n~n', [NA]),
    % Metric distributions
    findall(E, gap_profile(_, E, _, _, _, _), Epsilons),
    findall(Chi, gap_profile(_, _, Chi, _, _, _), Chis),
    findall(S, gap_profile(_, _, _, S, _, _), Supps),
    format('### Metric Distributions (at d=0.323, sigma=1.0)~n~n'),
    format('| Metric | Min | Q1 | Median | Q3 | Max |~n'),
    format('|--------|-----|----|----|----|----|~n'),
    distribution_row('epsilon (base)', Epsilons),
    distribution_row('chi (effective)', Chis),
    distribution_row('suppression', Supps),
    format('~n'),
    % Structural flags summary
    findall(1, gap_structural(_, true, _, _, _), CoordYes), length(CoordYes, NCoord),
    findall(1, gap_structural(_, _, true, _, _), AsymmYes), length(AsymmYes, NAsymm),
    findall(1, gap_structural(_, _, _, true, _), EnforceYes), length(EnforceYes, NEnforce),
    findall(1, gap_structural(_, _, _, _, true), EmergesYes), length(EmergesYes, NEmerges),
    format('### Structural Flags~n~n'),
    format('| Flag | Present | Absent |~n'),
    format('|------|---------|--------|~n'),
    NCoordNo is NA - NCoord,
    NAsymmNo is NA - NAsymm,
    NEnforceNo is NA - NEnforce,
    NEmergesNo is NA - NEmerges,
    format('| has_coordination_function | ~w | ~w |~n', [NCoord, NCoordNo]),
    format('| has_asymmetric_extraction | ~w | ~w |~n', [NAsymm, NAsymmNo]),
    format('| requires_active_enforcement | ~w | ~w |~n', [NEnforce, NEnforceNo]),
    format('| emerges_naturally | ~w | ~w |~n~n', [NEmerges, NEmergesNo]),
    % Canonical classification pairs
    format('### Canonical Classifications (Departure -> Destination)~n~n'),
    format('| Constraint | epsilon | chi@0.323 | supp | Institutional | Moderate |~n'),
    format('|------------|---------|-----------|------|---------------|----------|~n'),
    forall(gap_profile(C, E, Chi, Supp, InstT, ModT), (
        format('| ~w | ~3f | ~3f | ~3f | ~w | ~w |~n', [C, E, Chi, Supp, InstT, ModT])
    )),
    format('~n').

%% distribution_row(+Label, +Values)
%  Outputs a table row with min, Q1, median, Q3, max.
distribution_row(Label, Values) :-
    (   Values \= []
    ->  msort(Values, Sorted),
        length(Sorted, N),
        nth1(1, Sorted, Min),
        last(Sorted, Max),
        Q1Idx is max(1, round(N * 0.25)),
        MedIdx is max(1, round(N * 0.50)),
        Q3Idx is max(1, round(N * 0.75)),
        nth1(Q1Idx, Sorted, Q1),
        nth1(MedIdx, Sorted, Med),
        nth1(Q3Idx, Sorted, Q3),
        format('| ~w | ~3f | ~3f | ~3f | ~3f | ~3f |~n', [Label, Min, Q1, Med, Q3, Max])
    ;   format('| ~w | - | - | - | - | - |~n', [Label])
    ).

report_threshold_proximity :-
    format('## Step 2.3: Threshold Proximity Analysis~n~n'),
    format('For each Group A constraint, the blocking gate(s) that prevent classification:~n~n'),
    % Summary: most common blocking gates
    threshold_cluster_analysis(Clusters),
    sort(2, @>=, Clusters, SortedClusters),
    format('### Blocking Gate Frequency~n~n'),
    format('| Blocking Gate | Count | Description |~n'),
    format('|---------------|-------|-------------|~n'),
    forall(member(Gate-Count, SortedClusters), (
        gate_description(Gate, Desc),
        format('| ~w | ~w | ~w |~n', [Gate, Count, Desc])
    )),
    format('~n').

gate_description(rope_epsilon_too_high, 'BaseEps > 0.45, blocking rope (chi passes)') :- !.
gate_description(rope_chi_too_high, 'Chi > 0.35, blocking rope') :- !.
gate_description(scaffold_no_coordination, 'No has_coordination_function, blocking scaffold') :- !.
gate_description(scaffold_chi_too_high, 'Chi > 0.30, blocking scaffold') :- !.
gate_description(tangled_rope_chi_too_low, 'Chi < 0.40, blocking tangled_rope') :- !.
gate_description(tangled_rope_no_coordination, 'No has_coordination_function, blocking tangled_rope') :- !.
gate_description(tangled_rope_no_enforcement, 'No requires_active_enforcement, blocking tangled_rope') :- !.
gate_description(snare_chi_too_low, 'Chi < 0.66, blocking snare') :- !.
gate_description(Gate, 'See classify_from_metrics/6') :- format(atom(_), '~w', [Gate]).

report_unknown_bands :-
    format('## Step 2.4: Unknown-Band Width Distribution~n~n'),
    findall(Width,
        (   unknown_band(_, D_enter, D_exit),
            Width is D_exit - D_enter
        ),
        Widths),
    (   Widths \= []
    ->  msort(Widths, Sorted),
        length(Sorted, N),
        nth1(1, Sorted, Min),
        last(Sorted, Max),
        Q1Idx is max(1, round(N * 0.25)),
        MedIdx is max(1, round(N * 0.50)),
        Q3Idx is max(1, round(N * 0.75)),
        nth1(Q1Idx, Sorted, Q1),
        nth1(MedIdx, Sorted, Med),
        nth1(Q3Idx, Sorted, Q3),
        sumlist(Widths, SumW),
        MeanW is SumW / N,
        format('| Statistic | d-width |~n'),
        format('|-----------|---------|~n'),
        format('| N | ~w |~n', [N]),
        format('| Min | ~4f |~n', [Min]),
        format('| Q1 | ~4f |~n', [Q1]),
        format('| Median | ~4f |~n', [Med]),
        format('| Q3 | ~4f |~n', [Q3]),
        format('| Max | ~4f |~n', [Max]),
        format('| Mean | ~4f |~n~n', [MeanW]),
        % Histogram by width bins
        format('### Band Width Histogram~n~n'),
        format('| Width Range | Count |~n'),
        format('|-------------|-------|~n'),
        count_in_range(Sorted, 0.0, 0.1, N1),
        count_in_range(Sorted, 0.1, 0.2, N2),
        count_in_range(Sorted, 0.2, 0.3, N3),
        count_in_range(Sorted, 0.3, 0.4, N4),
        count_in_range(Sorted, 0.4, 0.5, N5),
        count_in_range(Sorted, 0.5, 0.7, N6),
        format('| 0.00 - 0.10 | ~w |~n', [N1]),
        format('| 0.10 - 0.20 | ~w |~n', [N2]),
        format('| 0.20 - 0.30 | ~w |~n', [N3]),
        format('| 0.30 - 0.40 | ~w |~n', [N4]),
        format('| 0.40 - 0.50 | ~w |~n', [N5]),
        format('| 0.50+ | ~w |~n~n', [N6]),
        % Detail table: per-constraint bands
        format('### Per-Constraint Unknown Bands~n~n'),
        format('| Constraint | D_enter | D_exit | Width |~n'),
        format('|------------|---------|--------|-------|~n'),
        forall(unknown_band(C, DE, DX), (
            W is DX - DE,
            format('| ~w | ~4f | ~4f | ~4f |~n', [C, DE, DX, W])
        )),
        format('~n')
    ;   format('No unknown bands found.~n~n')
    ).

count_in_range(Sorted, Lo, Hi, Count) :-
    include(in_range(Lo, Hi), Sorted, Matching),
    length(Matching, Count).

in_range(Lo, Hi, V) :- V >= Lo, V < Hi.

report_transition_pairs :-
    format('## Step 3.1: Transition Pair Analysis~n~n'),
    transition_pair_analysis(PairFreqs),
    sort(2, @>=, PairFreqs, SortedPairs),
    findall(C, gap_group(C, group_a), GA), length(GA, NA),
    format('How Group A constraints transition from institutional to moderate classification:~n~n'),
    format('| Institutional Type | Moderate Type | Count | Pct |~n'),
    format('|-------------------|---------------|-------|-----|~n'),
    forall(member((InstT-ModT)-Count, SortedPairs), (
        Pct is (Count * 100) / max(1, NA),
        format('| ~w | ~w | ~w | ~1f% |~n', [InstT, ModT, Count, Pct])
    )),
    format('~n'),
    % Homogeneity assessment
    (   SortedPairs = [(DomInstT-DomModT)-DomCount | _]
    ->  DomPct is (DomCount * 100) / max(1, NA),
        format('**Dominant pattern**: ~w -> ~w (~1f% of Group A)~n~n',
               [DomInstT, DomModT, DomPct]),
        (   DomPct >= 90.0
        ->  format('*The gap is highly homogeneous — one dominant transition pattern.*~n~n')
        ;   DomPct >= 60.0
        ->  format('*The gap is moderately homogeneous — one primary pattern with notable minorities.*~n~n')
        ;   format('*The gap is heterogeneous — no single dominant pattern.*~n~n')
        )
    ;   format('No transition pairs found.~n~n')
    ).

report_threshold_clusters :-
    format('## Step 3.2: Threshold Clustering~n~n'),
    threshold_cluster_analysis(_Clusters),
    findall(C, gap_group(C, group_a), GA), length(GA, NA),
    format('Do Group A constraints cluster near the same threshold boundary?~n~n'),
    % Identify the PRIMARY blocking reason per constraint
    findall(PrimaryGate,
        (   gap_group(C, group_a),
            primary_blocking_gate(C, PrimaryGate)
        ),
        PrimaryGates),
    msort(PrimaryGates, PGSorted),
    clumped(PGSorted, PGClusters),
    sort(2, @>=, PGClusters, PGSortedClusters),
    format('### Primary Blocking Gate per Constraint~n~n'),
    format('The single most specific gate preventing classification:~n~n'),
    format('| Primary Gate | Count | Pct |~n'),
    format('|-------------|-------|-----|~n'),
    forall(member(Gate-Count, PGSortedClusters), (
        Pct is (Count * 100) / max(1, NA),
        format('| ~w | ~w | ~1f% |~n', [Gate, Count, Pct])
    )),
    format('~n'),
    (   PGSortedClusters = [TopGate-TopCount | _]
    ->  TopPct is (TopCount * 100) / max(1, NA),
        (   TopPct >= 80.0
        ->  format('**Single dominant cluster**: ~w accounts for ~1f% of Group A. ',
                   [TopGate, TopPct]),
            format('The gap has one well-defined cause.~n~n')
        ;   TopPct >= 50.0
        ->  format('**Primary cluster with minorities**: ~w accounts for ~1f% of Group A. ',
                   [TopGate, TopPct]),
            format('The gap has a primary cause with secondary sub-populations.~n~n')
        ;   format('**No dominant cluster**: The gap contains distinct sub-populations with different blocking gates.~n~n')
        )
    ;   true
    ).

%% primary_blocking_gate(+C, -Gate)
%  Returns the most specific blocking gate for a constraint.
%  Priority: rope_epsilon_too_high > scaffold_no_coordination > others
primary_blocking_gate(C, Gate) :-
    (   blocking_gate(C, rope_epsilon_too_high)
    ->  Gate = rope_epsilon_too_high
    ;   blocking_gate(C, scaffold_no_coordination)
    ->  Gate = scaffold_no_coordination
    ;   blocking_gate(C, rope_chi_too_high)
    ->  Gate = rope_chi_too_high
    ;   blocking_gate(C, tangled_rope_no_coordination)
    ->  Gate = tangled_rope_no_coordination
    ;   blocking_gate(C, tangled_rope_no_enforcement)
    ->  Gate = tangled_rope_no_enforcement
    ;   Gate = other
    ).

report_scope_sensitivity :-
    format('## Step 3.3: Scope Sensitivity at d=0.323~n~n'),
    scope_sensitivity_at_midpoint(Results),
    include(scope_varies, Results, Varying),
    include(scope_uniform, Results, Uniform),
    length(Varying, NVary),
    length(Uniform, NUniform),
    length(Results, NTotal),
    format('At the institutional power level (d=0.0), all three scope cells are identical '),
    format('(100% agreement from Phase 1). Does scope start to matter at d=0.323?~n~n'),
    format('- **Scope-invariant**: ~w constraints (~1f%)~n',
           [NUniform, (NUniform * 100) / max(1, NTotal)]),
    format('- **Scope-sensitive**: ~w constraints (~1f%)~n~n',
           [NVary, (NVary * 100) / max(1, NTotal)]),
    (   NVary > 0
    ->  format('### Scope-Sensitive Constraints at d=0.323~n~n'),
        format('| Constraint | local (0.8) | national (1.0) | global (1.2) |~n'),
        format('|------------|-------------|----------------|--------------|~n'),
        forall(member(scope_result(C, [TL, TN, TG], yes), Results), (
            format('| ~w | ~w | ~w | ~w |~n', [C, TL, TN, TG])
        )),
        format('~n'),
        format('Scope sensitivity at d=0.323 indicates richer structure than the '),
        format('institutional region. The gap is not merely a uniform dead zone.~n~n')
    ;   format('The gap is scope-invariant, matching the institutional pattern. '),
        format('Scope plays no role at this d-value.~n~n')
    ).

scope_varies(scope_result(_, _, yes)).
scope_uniform(scope_result(_, _, no)).

/* ── Step 4: Recommendation ─────────────────────────────────────── */

report_recommendation_gap(NC, NA) :-
    format('## Step 4: Recommendation~n~n'),
    % Gather evidence
    transition_pair_analysis(PairFreqs),
    sort(2, @>=, PairFreqs, SortedPairs),
    (   SortedPairs = [(_-_)-DomCount | _]
    ->  DomPct is (DomCount * 100) / max(1, NA)
    ;   DomPct = 0
    ),
    findall(PG, (gap_group(C, group_a), primary_blocking_gate(C, PG)), PGs),
    msort(PGs, PGSorted), clumped(PGSorted, PGClusters),
    sort(2, @>=, PGClusters, PGSortedClusters),
    (   PGSortedClusters = [TopGate-TopCount | _]
    ->  TopPct is (TopCount * 100) / max(1, NA)
    ;   TopGate = none, TopPct = 0
    ),
    scope_sensitivity_at_midpoint(ScopeResults),
    include(scope_varies, ScopeResults, Varying),
    length(Varying, NVary),
    % Unknown band statistics
    findall(W, (unknown_band(_, DE, DX), W is DX - DE), Widths),
    (   Widths \= []
    ->  msort(Widths, WSorted),
        length(WSorted, NW),
        MedIdx is max(1, round(NW * 0.50)),
        nth1(MedIdx, WSorted, MedianWidth),
        sumlist(Widths, SumW),
        MeanWidth is SumW / NW
    ;   MedianWidth = 0, MeanWidth = 0
    ),
    % Decision logic
    AffectedPct is (NA * 100) / max(1, NC),
    format('### Evidence Summary~n~n'),
    format('- Group A size: ~w constraints (~1f% of corpus)~n', [NA, AffectedPct]),
    format('- Dominant transition pattern: ~1f% homogeneity~n', [DomPct]),
    format('- Primary blocking gate: ~w (~1f%)~n', [TopGate, TopPct]),
    format('- Scope sensitivity at d=0.323: ~w constraints show variance~n', [NVary]),
    format('- Unknown band: median width ~4f, mean width ~4f~n~n', [MedianWidth, MeanWidth]),
    % Recommendation
    (   DomPct >= 80.0, TopPct >= 70.0
    ->  % Well-behaved gap
        format('### Verdict: Gap is Well-Behaved~n~n'),
        format('Group A is homogeneous: one dominant transition pattern (~1f%) ', [DomPct]),
        format('and one primary blocking gate (~w, ~1f%).~n~n', [TopGate, TopPct]),
        optimal_grid_point(OptD),
        format('**Recommendation**: The gap can be addressed with a single grid point '),
        format('at **d=~4f** (the centroid of the unknown band). ', [OptD]),
        format('This would split the institutional→moderate interval into two sub-intervals, '),
        format('resolving the majority of missed transitions.~n~n'),
        format('Alternatively, if adding a grid point is not desired, consider whether '),
        format('`classify_from_metrics/6` should handle the low-chi, high-epsilon region '),
        format('explicitly (currently it falls to `unknown`).~n~n')
    ;   DomPct >= 50.0
    ->  % Structured but complex
        format('### Verdict: Gap is Structured but Complex~n~n'),
        format('Group A has a primary pattern (~1f%) with notable sub-populations.~n~n', [DomPct]),
        % Find cluster boundaries
        optimal_grid_point(OptD),
        format('**Recommendation**: Consider adding 1-2 grid points:~n'),
        format('1. Primary point at **d=~4f** (centroid of the unknown band)~n', [OptD]),
        format('2. If sub-populations have distinct band locations, add a second point '),
        format('at the sub-population boundary.~n~n'),
        format('Also investigate whether `classify_from_metrics/6` needs additional '),
        format('threshold logic for the low-chi region between rope and tangled_rope.~n~n')
    ;   % Genuinely ambiguous
        format('### Verdict: Gap is Genuinely Ambiguous~n~n'),
        format('Group A is heterogeneous (dominant pattern only ~1f%, ', [DomPct]),
        format('top blocking gate only ~1f%).~n~n', [TopPct]),
        format('**Recommendation**: Adding grid points will not resolve this gap, because '),
        format('the classifier itself does not know what to do with constraints in this '),
        format('region. Investigate whether `classify_from_metrics/6` needs a new classification '),
        format('type or additional threshold logic for the low-chi, moderate-epsilon zone '),
        format('(the region between rope ceiling and tangled_rope floor).~n~n')
    ).

%% optimal_grid_point(-D)
%  Computes the centroid of the unknown band across Group A constraints.
optimal_grid_point(OptD) :-
    findall(Mid,
        (   unknown_band(_, DE, DX),
            Mid is (DE + DX) / 2
        ),
        Mids),
    (   Mids \= []
    ->  sumlist(Mids, Sum),
        length(Mids, N),
        OptD is Sum / N
    ;   midpoint_d(OptD)  % fallback
    ).

/* ── Prolog Facts Output ────────────────────────────────────────── */

report_prolog_facts :-
    format('## Embedded Prolog Facts~n~n'),
    format('```prolog~n'),
    format('%% Gap group membership~n'),
    forall(gap_group(C, G), format('gap_group(~w, ~w).~n', [C, G])),
    format('~n%% Unknown bands (Group A only)~n'),
    forall(unknown_band(C, DE, DX), format('unknown_band(~w, ~4f, ~4f).~n', [C, DE, DX])),
    format('~n%% Transition pairs~n'),
    forall(gap_transition(C, IT, MT), format('gap_transition(~w, ~w, ~w).~n', [C, IT, MT])),
    format('```~n~n').
