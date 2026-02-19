% ============================================================================
% MAXENT REPORT — Maximum Entropy Shadow Classifier Analysis
% ============================================================================
% Standalone script. Run from prolog/ directory:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l maxent_report.pl -g "run_maxent_report, halt."
%
% Bulk-loads all testsets, runs the MaxEnt shadow classifier, and outputs
% a markdown report with entropy analysis, disagreement detection,
% threshold proximity, and cross-diagnostic correlation.
%
% Pattern follows fpn_report.pl exactly.
% ============================================================================

:- use_module(covering_analysis).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(signature_detection, [constraint_signature/2]).
:- use_module(purity_scoring, [purity_score/2]).
:- use_module(dirac_classification).
:- use_module(maxent_classifier).

:- use_module(library(lists)).

%% run_maxent_report
%  Main entry point. Loads corpus, runs MaxEnt, outputs markdown.
run_maxent_report :-
    format(user_error, '[maxent_report] Starting MaxEnt shadow classifier analysis...~n', []),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),

    % Run MaxEnt classifier
    maxent_classifier:maxent_run(Context, Summary),
    Summary = maxent_summary(NTotal, MeanEntropy, NHighUncertainty, NHard, NSoft),

    % Collect all constraints
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C), atom(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),

    % Collect distributions and entropy data
    findall(row(C, HNorm, TopType, DetType, Dist),
        (   member(C, Constraints),
            maxent_classifier:maxent_entropy(C, Context, HNorm),
            maxent_classifier:maxent_top_type(C, Context, TopType),
            (drl_core:dr_type(C, Context, DetType) -> true ; DetType = unknown),
            maxent_classifier:maxent_distribution(C, Context, Dist)
        ),
        Rows),

    % Collect disagreements
    maxent_classifier:maxent_disagreements(Context, AllDisagreements),

    % Write markdown report to stdout
    format('<!-- MAXENT_REPORT_START -->~n'),
    format('# Maximum Entropy Shadow Classifier Report~n~n'),
    format('*Generated: corpus-wide MaxEnt probability analysis via maxent_classifier:maxent_run/2*~n~n'),

    report_summary(NTotal, MeanEntropy, NHighUncertainty, NHard, NSoft, Rows),
    report_high_uncertainty(Rows, Context),
    report_disagreements(AllDisagreements, Context),
    report_threshold_proximity(Constraints, Context),
    report_type_entropy_breakdown(Rows),
    report_override_rope_analysis(Constraints, Context),
    report_dirac_cross_validation(AllDisagreements, Context),
    report_cross_diagnostic_correlation(Constraints, Context),

    format('---~n'),
    format('*End of MaxEnt report*~n'),
    format(user_error, '[maxent_report] Done.~n', []).

/* ================================================================
   SECTION 1: SUMMARY
   ================================================================ */

report_summary(NTotal, MeanEntropy, NHighUncertainty, NHard, NSoft, Rows) :-
    format('## Summary~n~n'),
    format('| Property | Value |~n'),
    format('|----------|-------|~n'),
    format('| **Constraints analyzed** | ~w |~n', [NTotal]),
    format('| **Mean normalized entropy** | ~4f |~n', [MeanEntropy]),

    % Median entropy
    findall(H, member(row(_, H, _, _, _), Rows), Entropies),
    (   Entropies \= []
    ->  msort(Entropies, Sorted),
        length(Sorted, Len),
        MidIdx is Len // 2,
        nth0(MidIdx, Sorted, MedianEntropy)
    ;   MedianEntropy = 0.0
    ),
    format('| **Median normalized entropy** | ~4f |~n', [MedianEntropy]),

    config:param(maxent_uncertainty_threshold, Threshold),
    (   NTotal > 0
    ->  PctFlagged is NHighUncertainty * 100.0 / NTotal
    ;   PctFlagged = 0.0
    ),
    format('| **High uncertainty constraints** | ~w (~1f%) |~n', [NHighUncertainty, PctFlagged]),
    format('| **Hard disagreements** | ~w |~n', [NHard]),
    format('| **Soft disagreements** | ~w |~n', [NSoft]),
    format('| **Uncertainty threshold (H_norm)** | ~4f |~n~n', [Threshold]).

/* ================================================================
   SECTION 2: HIGH UNCERTAINTY CONSTRAINTS
   ================================================================ */

report_high_uncertainty(Rows, _Context) :-
    config:param(maxent_uncertainty_threshold, Threshold),
    findall(H-row(C, H, TopType, DetType, Dist),
        (   member(row(C, H, TopType, DetType, Dist), Rows),
            H > Threshold
        ),
        Flagged),
    format('## High Uncertainty Constraints (H_norm > ~4f)~n~n', [Threshold]),
    (   Flagged = []
    ->  format('No constraints exceed the uncertainty threshold.~n~n')
    ;   msort(Flagged, SortedAsc),
        reverse(SortedAsc, Sorted),
        format('| Constraint | Det Type | Shadow Top | H_norm | Confidence | Top P |~n'),
        format('|------------|----------|-----------|--------|------------|-------|~n'),
        forall(member(_-row(C, H, TopType, DetType, Dist), Sorted), (
            Conf is 1.0 - H,
            best_prob(Dist, TopP),
            format('| ~w | ~w | ~w | ~4f | ~4f | ~4f |~n',
                   [C, DetType, TopType, H, Conf, TopP])
        )),
        format('~n')
    ).

best_prob(Dist, TopP) :-
    maplist(maxent_classifier:pair_snd, Dist, Probs),
    max_list(Probs, TopP).

/* ================================================================
   SECTION 3: DISAGREEMENTS
   ================================================================ */

report_disagreements(AllDisagreements, Context) :-
    format('## Disagreements~n~n'),

    % Hard disagreements
    include(is_hard, AllDisagreements, Hards),
    format('### Hard Disagreements (shadow top-type != deterministic type)~n~n'),
    (   Hards = []
    ->  format('No hard disagreements found.~n~n')
    ;   format('| Constraint | Det Type | Shadow Top | Distribution |~n'),
        format('|------------|----------|-----------|--------------|~n'),
        forall(member(C-hard(ShadowType, DetType), Hards), (
            format_dist_summary(C, Context, DistStr),
            format('| ~w | ~w | ~w | ~w |~n', [C, DetType, ShadowType, DistStr])
        )),
        format('~n')
    ),

    % Soft disagreements
    include(is_soft, AllDisagreements, Softs),
    format('### Soft Disagreements (same top-type but P < 0.50)~n~n'),
    (   Softs = []
    ->  format('No soft disagreements found.~n~n')
    ;   format('| Constraint | Type | P(type) |~n'),
        format('|------------|------|---------|~n'),
        forall(member(C-soft(_, DetType, DetProb), Softs), (
            format('| ~w | ~w | ~4f |~n', [C, DetType, DetProb])
        )),
        format('~n')
    ),

    % Entropy flags (not hard or soft)
    include(is_entropy_flag, AllDisagreements, EFlags),
    length(EFlags, NEFlags),
    format('### Entropy Flags (H_norm > threshold, no type disagreement)~n~n'),
    format('~w constraints have high entropy but agree on type.~n~n', [NEFlags]).

is_hard(_-hard(_, _)).
is_soft(_-soft(_, _, _)).
is_entropy_flag(_-entropy_flag(_)).

format_dist_summary(C, Context, DistStr) :-
    (   maxent_classifier:maxent_distribution(C, Context, Dist)
    ->  findall(Atom,
            (member(Type-P, Dist), P > 0.05,
             format(atom(Atom), '~w:~2f', [Type, P])),
            Parts),
        atomic_list_concat(Parts, ' ', DistStr)
    ;   DistStr = '(no data)'
    ).

/* ================================================================
   SECTION 4: THRESHOLD PROXIMITY
   ================================================================ */

report_threshold_proximity(Constraints, Context) :-
    format('## Threshold Proximity Analysis~n~n'),
    format('Constraints closest to each classification boundary, ranked by distance:~n~n'),

    forall(
        (   maxent_classifier:threshold_boundary(BName, _MetricName, Thresh, TypeBelow, TypeAbove),
            ground(BName)
        ),
        report_one_boundary(BName, Thresh, TypeBelow, TypeAbove, Constraints, Context)
    ).

report_one_boundary(BName, Thresh, TypeBelow, TypeAbove, Constraints, Context) :-
    findall(Dist-C,
        (   member(C, Constraints),
            catch(maxent_classifier:maxent_threshold_proximity(C, Context, BName, Dist), _, fail),
            Dist < 0.10  % Only show constraints within 0.10 of boundary
        ),
        Pairs),
    msort(Pairs, Sorted),
    length(Sorted, N),
    format('### ~w (threshold=~4f, ~w <-> ~w)~n~n', [BName, Thresh, TypeBelow, TypeAbove]),
    (   N = 0
    ->  format('No constraints within 0.10 of this boundary.~n~n')
    ;   % Show top 10
        (N > 10 -> length(Top, 10), append(Top, _, Sorted) ; Top = Sorted),
        format('| Constraint | Distance | Det Type | H_norm |~n'),
        format('|------------|----------|----------|--------|~n'),
        forall(member(D-C, Top), (
            (drl_core:dr_type(C, Context, DT) -> true ; DT = unknown),
            (maxent_classifier:maxent_entropy(C, Context, HN) -> true ; HN = 0.0),
            format('| ~w | ~4f | ~w | ~4f |~n', [C, D, DT, HN])
        )),
        format('~n*~w constraints within 0.10 of boundary*~n~n', [N])
    ).

/* ================================================================
   SECTION 5: TYPE ENTROPY BREAKDOWN
   ================================================================ */

report_type_entropy_breakdown(Rows) :-
    format('## Type Entropy Breakdown~n~n'),
    format('Average normalized entropy by deterministic type:~n~n'),
    format('| Type | Count | Mean H_norm | Min | Max | Interpretation |~n'),
    format('|------|-------|------------|-----|-----|----------------|~n'),

    findall(T, member(row(_, _, _, T, _), Rows), Types),
    sort(Types, UniqueTypes),
    forall(member(Type, UniqueTypes), (
        findall(H, member(row(_, H, _, Type, _), Rows), Entropies),
        length(Entropies, Count),
        (   Count > 0
        ->  sum_list(Entropies, Sum),
            MeanH is Sum / Count,
            min_list(Entropies, MinH),
            max_list(Entropies, MaxH),
            entropy_interpretation(MeanH, Interp),
            format('| ~w | ~w | ~4f | ~4f | ~4f | ~w |~n',
                   [Type, Count, MeanH, MinH, MaxH, Interp])
        ;   true
        )
    )),
    format('~n').

entropy_interpretation(H, 'Low uncertainty')     :- H < 0.20, !.
entropy_interpretation(H, 'Moderate')            :- H < 0.40, !.
entropy_interpretation(H, 'High')                :- H < 0.60, !.
entropy_interpretation(_, 'Very high').

/* ================================================================
   SECTION 6: OVERRIDE-ROPE ANALYSIS
   ================================================================ */

report_override_rope_analysis(Constraints, Context) :-
    format('## Override-Rope Analysis~n~n'),
    format('Ropes classified via signature override (CI_Rope, coordination_scaffold).~n'),
    format('These constraints have metrics that do not match the rope profile, '),
    format('producing informatively high entropy.~n~n'),

    findall(C-Sig,
        (   member(C, Constraints),
            drl_core:dr_type(C, Context, rope),
            catch(signature_detection:constraint_signature(C, Sig), _, fail),
            override_rope_signature(Sig)
        ),
        OverrideRopes),

    (   OverrideRopes = []
    ->  format('No ropes classified via signature override detected.~n~n')
    ;   format('| Constraint | Signature | H_norm | P(rope) | P(top non-rope) |~n'),
        format('|------------|-----------|--------|---------|-----------------|~n'),
        forall(member(C-Sig, OverrideRopes), (
            (   maxent_classifier:maxent_entropy(C, Context, HN) -> true ; HN = 0.0 ),
            (   maxent_classifier:maxent_distribution(C, Context, Dist)
            ->  (member(rope-PRope, Dist) -> true ; PRope = 0.0),
                findall(P, (member(T-P, Dist), T \= rope), NonRopePs),
                (NonRopePs \= [] -> max_list(NonRopePs, TopNonRope) ; TopNonRope = 0.0)
            ;   PRope = 0.0, TopNonRope = 0.0
            ),
            format('| ~w | ~w | ~4f | ~4f | ~4f |~n',
                   [C, Sig, HN, PRope, TopNonRope])
        )),
        length(OverrideRopes, NOR),
        format('~n*~w override-ropes identified*~n~n', [NOR])
    ).

override_rope_signature(coupling_invariant_rope).
override_rope_signature(coordination_scaffold).
override_rope_signature(constructed_low_extraction).

/* ================================================================
   SECTION 7: DIRAC ORBIT CROSS-VALIDATION
   ================================================================ */

report_dirac_cross_validation(AllDisagreements, Context) :-
    format('## Dirac Orbit Cross-Validation~n~n'),
    format('Overlap between MaxEnt hard-disagreements and multi-type Dirac orbits.~n~n'),

    include(is_hard, AllDisagreements, Hards),
    findall(C, member(C-hard(_, _), Hards), HardConstraints),

    % Compute orbit data for hard-disagreement constraints
    findall(C-multi_type,
        (   member(C, HardConstraints),
            catch(dirac_orbit_multi_type(C, Context), _, fail)
        ),
        MultiTypeHards),
    findall(C-single_type,
        (   member(C, HardConstraints),
            \+ catch(dirac_orbit_multi_type(C, Context), _, fail)
        ),
        SingleTypeHards),

    length(HardConstraints, NHard),
    length(MultiTypeHards, NOverlap),
    length(SingleTypeHards, NNonOverlap),

    format('| Metric | Count |~n'),
    format('|--------|-------|~n'),
    format('| **Hard disagreements** | ~w |~n', [NHard]),
    format('| **Also in multi-type Dirac orbits** | ~w |~n', [NOverlap]),
    format('| **Metric-only ambiguity (single-type orbit)** | ~w |~n~n', [NNonOverlap]),

    (   NHard > 0
    ->  OverlapPct is NOverlap * 100.0 / NHard,
        (   OverlapPct > 50
        ->  format('**High overlap (~1f%)**: Two independent diagnostics (metric-space probability ', [OverlapPct]),
            format('vs indexical orbit structure) are identifying the same ambiguous population. '),
            format('Strong cross-validation.~n~n')
        ;   format('**Low overlap (~1f%)**: Metric ambiguity and indexical relativity are identifying ', [OverlapPct]),
            format('different populations. Both are real signals — orthogonal findings about '),
            format('classification architecture.~n~n')
        )
    ;   format('No hard disagreements to cross-validate.~n~n')
    ).

%% dirac_orbit_multi_type(+C, +Context)
%  Succeeds if C has a multi-type Dirac orbit (gauge-variant).
dirac_orbit_multi_type(C, _Context) :-
    dirac_classification:gauge_orbit(C, OrbitPoints),
    findall(T, member(orbit_point(T, _), OrbitPoints), Types),
    sort(Types, UniqueTypes),
    length(UniqueTypes, N),
    N > 1.

/* ================================================================
   SECTION 8: CROSS-DIAGNOSTIC CORRELATION
   ================================================================ */

report_cross_diagnostic_correlation(Constraints, Context) :-
    format('## Cross-Diagnostic Correlation~n~n'),
    format('Do high-entropy constraints also show other diagnostic signals?~n~n'),

    config:param(maxent_uncertainty_threshold, Threshold),

    % Entropy vs Purity
    findall(C-HN-Purity,
        (   member(C, Constraints),
            maxent_classifier:maxent_entropy(C, Context, HN),
            catch(purity_scoring:purity_score(C, Purity), _, fail),
            Purity >= 0.0  % Exclude sentinel -1.0
        ),
        EPRows),

    (   EPRows \= []
    ->  % Split into high/low entropy groups
        include(high_entropy_row(Threshold), EPRows, HighEntropyRows),
        exclude(high_entropy_row(Threshold), EPRows, LowEntropyRows),
        avg_purity(HighEntropyRows, AvgPurityHigh),
        avg_purity(LowEntropyRows, AvgPurityLow),
        length(HighEntropyRows, NHigh),
        length(LowEntropyRows, NLow),
        format('### Entropy vs Purity~n~n'),
        format('| Group | Count | Avg Purity |~n'),
        format('|-------|-------|------------|~n'),
        format('| High entropy (H > ~4f) | ~w | ~4f |~n', [Threshold, NHigh, AvgPurityHigh]),
        format('| Low entropy (H <= ~4f) | ~w | ~4f |~n~n', [Threshold, NLow, AvgPurityLow])
    ;   format('Insufficient purity data for correlation analysis.~n~n')
    ),

    % Entropy vs Omega variables
    findall(C-HN,
        (   member(C, Constraints),
            maxent_classifier:maxent_entropy(C, Context, HN),
            HN > Threshold,
            has_omega(C)
        ),
        OmegaHighEntropy),
    findall(C-HN,
        (   member(C, Constraints),
            maxent_classifier:maxent_entropy(C, Context, HN),
            HN > Threshold
        ),
        AllHighEntropy),
    length(OmegaHighEntropy, NOmegaHigh),
    length(AllHighEntropy, NAllHigh),
    format('### Entropy vs Omega Variables~n~n'),
    (   NAllHigh > 0
    ->  OmegaPct is NOmegaHigh * 100.0 / NAllHigh,
        format('Of ~w high-entropy constraints, ~w (~1f%) also have Omega variables.~n~n',
               [NAllHigh, NOmegaHigh, OmegaPct])
    ;   format('No high-entropy constraints to correlate with Omega variables.~n~n')
    ).

high_entropy_row(Threshold, _C-HN-_Purity) :- HN > Threshold.

avg_purity([], 0.0) :- !.
avg_purity(Rows, Avg) :-
    findall(P, member(_-_-P, Rows), Purities),
    sum_list(Purities, Sum),
    length(Purities, N),
    Avg is Sum / N.

has_omega(C) :-
    narrative_ontology:omega_variable(C, _, _), !.
