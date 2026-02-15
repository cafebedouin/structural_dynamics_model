% ============================================================================
% ABDUCTIVE REPORT — Cross-Subsystem Anomaly Synthesis Analysis
% ============================================================================
% Standalone script. Run from prolog/ directory:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l dirac_classification.pl -l abductive_engine.pl \
%         -l abductive_report.pl -g "run_abductive_report, halt."
%
% Orchestrates subsystem prerequisites (MaxEnt, optionally FPN), runs the
% abductive engine, and outputs a structured markdown report with artifact
% census, genuine findings, and investigation queue.
%
% Pattern follows maxent_report.pl exactly.
% ============================================================================

:- use_module(covering_analysis).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(dirac_classification).
:- use_module(maxent_classifier).
:- use_module(drl_lifecycle).
:- use_module(logical_fingerprint).
:- use_module(abductive_engine).

:- use_module(library(lists)).

%% run_abductive_report
%  Main entry point. Loads corpus, runs prerequisites, runs abductive
%  engine, outputs markdown report.
run_abductive_report :-
    format(user_error, '[abductive_report] Starting abductive reasoning analysis...~n', []),
    covering_analysis:load_all_testsets,
    constraint_indexing:default_context(Context),

    % Run MaxEnt classifier (prerequisite for most triggers)
    format(user_error, '[abductive_report] Running MaxEnt classifier...~n', []),
    maxent_classifier:maxent_run(Context, _MaxEntSummary),

    % Run FPN if enabled (prerequisite for Phase 3 triggers)
    (   config:param(fpn_enabled, 1)
    ->  format(user_error, '[abductive_report] Running FPN iterator...~n', []),
        catch(drl_modal_logic:fpn_run(Context, _FPNSummary), _, true)
    ;   format(user_error, '[abductive_report] FPN disabled — Phase 3 FPN triggers inactive.~n', [])
    ),

    % Run abductive engine
    format(user_error, '[abductive_report] Running abductive engine...~n', []),
    abductive_engine:abductive_run(Context, Summary),
    Summary = abductive_summary(NTotal, NGenuine, NArtifacts, SubsAvail),

    % Collect all constraints
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C), atom(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),

    % Write markdown report
    format('<!-- ABDUCTIVE_REPORT_START -->~n'),
    format('# Abductive Reasoning Report~n~n'),
    format('*Generated: cross-subsystem anomaly synthesis via abductive_engine:abductive_run/2*~n~n'),

    report_summary(NTotal, NGenuine, NArtifacts, SubsAvail, Context),
    report_artifact_census(Context),
    report_genuine_findings(Context),
    report_top_hypotheses(Context),
    report_investigation_queue(Context, Constraints),

    format('---~n'),
    format('*End of abductive report*~n'),
    format(user_error, '[abductive_report] Done.~n', []).

/* ================================================================
   SECTION 1: SUMMARY
   ================================================================ */

report_summary(NTotal, NGenuine, NArtifacts, SubsAvail, Context) :-
    format('## Summary~n~n'),
    format('| Property | Value |~n'),
    format('|----------|-------|~n'),
    format('| **Total hypotheses** | ~w |~n', [NTotal]),
    format('| **Genuine findings** | ~w |~n', [NGenuine]),
    format('| **Override artifacts** | ~w |~n', [NArtifacts]),
    format('| **Subsystems available** | ~w |~n~n', [SubsAvail]),

    % By-class breakdown
    format('### Hypothesis Counts by Class~n~n'),
    format('| Class | Count | Category |~n'),
    format('|-------|-------|----------|~n'),
    forall(
        member(Class-Category, [
            signature_override_artifact-artifact,
            deep_deception-genuine,
            metric_structural_divergence-genuine,
            confirmed_liminal-genuine,
            coverage_gap-genuine,
            accelerating_pathology-genuine,
            contamination_cascade-genuine,
            dormant_extraction-genuine
        ]),
        (   abductive_engine:abductive_by_class(Class, Context, ClassH),
            length(ClassH, NClass),
            format('| ~w | ~w | ~w |~n', [Class, NClass, Category])
        )
    ),
    format('~n'),

    % Subsystem availability notes
    (   \+ member(maxent, SubsAvail)
    ->  format('> **Note:** MaxEnt data not available — 5 trigger classes inactive (artifact, deep_deception, metric_structural_divergence, confirmed_liminal, dormant_extraction).~n~n')
    ;   true
    ),
    (   \+ member(fpn, SubsAvail)
    ->  format('> **Note:** FPN data not available — 2 trigger classes inactive (accelerating_pathology, contamination_cascade).~n~n')
    ;   true
    ).

/* ================================================================
   SECTION 2: ARTIFACT CENSUS
   ================================================================ */

report_artifact_census(Context) :-
    format('## Artifact Census~n~n'),
    format('How many MaxEnt hard disagreements are explained by known signature overrides?~n~n'),

    abductive_engine:abductive_artifacts(Context, Artifacts),
    length(Artifacts, NArtifacts),

    % Count total hard disagreements from MaxEnt
    (   catch(maxent_classifier:maxent_disagreements(Context, AllDisagreements), _, (AllDisagreements = []))
    ->  include(is_hard_disagreement, AllDisagreements, Hards),
        length(Hards, NHard)
    ;   NHard = 0
    ),

    format('| Metric | Count |~n'),
    format('|--------|-------|~n'),
    format('| **Total hard disagreements** | ~w |~n', [NHard]),
    format('| **Explained by override** | ~w |~n', [NArtifacts]),
    (   NHard > 0
    ->  PctExplained is NArtifacts * 100.0 / NHard,
        Unexplained is NHard - NArtifacts,
        format('| **Unexplained** | ~w |~n', [Unexplained]),
        format('| **Explanation rate** | ~1f% |~n~n', [PctExplained]),
        (   PctExplained > 85.0
        ->  format('The vast majority of hard disagreements are mechanistic artifacts of ')
        ;   format('A significant portion of hard disagreements are mechanistic artifacts of ')
        ),
        format('known signature override rules. '),
        format('These constraints have metrics that predict one type, '),
        format('but a structural signature unconditionally forces a different classification.~n~n')
    ;   format('~n')
    ),

    % Show artifact breakdown by signature type
    (   NArtifacts > 0
    ->  format('### Artifacts by Override Signature~n~n'),
        format('| Signature | Count |~n'),
        format('|-----------|-------|~n'),
        findall(Sig, (
            member(H, Artifacts),
            H = hypothesis(_, _, _, EL, _, _, _),
            member(evidence_line(signature, override, Sig), EL)
        ), AllSigs),
        msort(AllSigs, SortedSigs),
        clumped(SortedSigs, SigCounts),
        forall(member(Sig-Count, SigCounts),
            format('| ~w | ~w |~n', [Sig, Count])
        ),
        format('~n')
    ;   true
    ).

is_hard_disagreement(_-hard(_, _)).

/* ================================================================
   SECTION 3: GENUINE FINDINGS
   ================================================================ */

report_genuine_findings(Context) :-
    format('## Genuine Findings~n~n'),
    abductive_engine:abductive_genuine(Context, AllGenuine),
    (   AllGenuine = []
    ->  format('No genuine cross-subsystem findings detected.~n~n')
    ;   forall(
            member(Class, [deep_deception, metric_structural_divergence,
                           confirmed_liminal, coverage_gap,
                           accelerating_pathology, contamination_cascade,
                           dormant_extraction]),
            report_class_findings(Class, Context)
        )
    ).

report_class_findings(Class, Context) :-
    abductive_engine:abductive_by_class(Class, Context, ClassH),
    (   ClassH = []
    ->  true  % Skip empty classes
    ;   length(ClassH, NClass),
        format('### ~w (~w)~n~n', [Class, NClass]),
        class_description(Class, Desc),
        format('~w~n~n', [Desc]),
        format('| Constraint | Confidence | Anomaly | Key Evidence |~n'),
        format('|------------|------------|---------|--------------|~n'),
        forall(member(H, ClassH), format_hypothesis_row(H)),
        format('~n')
    ).

class_description(deep_deception,
    'Constraints with FNL signature AND high MaxEnt P(mountain). Metrically deep deception.').
class_description(metric_structural_divergence,
    'High entropy (metric boundary) but preserved single-type Dirac orbit (structurally unambiguous).').
class_description(confirmed_liminal,
    'Triple-confirmed liminality: high entropy + multi-type orbit + active drift events.').
class_description(coverage_gap,
    'Multi-type Dirac orbit but no perspectival_incoherence from dr_mismatch. Diagnostic blind spot.').
class_description(accelerating_pathology,
    'FPN zone migration + purity drift. Contamination is actively worsening.').
class_description(contamination_cascade,
    'FPN EP divergence + network drift. Active contamination propagation.').
class_description(dormant_extraction,
    'Clean metric appearance but extractive structural voids and non-trivial coupling.').

format_hypothesis_row(hypothesis(C, _Class, anomaly(AType, _Details), EL, _, Conf, _)) :-
    format_key_evidence(EL, KeyEv),
    format('| ~w | ~2f | ~w | ~w |~n', [C, Conf, AType, KeyEv]).

format_key_evidence(EvidenceLines, KeyEv) :-
    (   member(evidence_line(maxent, entropy, H), EvidenceLines),
        number(H)
    ->  format(atom(Part1), 'H=~2f', [H])
    ;   Part1 = ''
    ),
    (   member(evidence_line(signature, override, Sig), EvidenceLines)
    ->  format(atom(Part2), ' sig=~w', [Sig])
    ;   member(evidence_line(dirac, orbit_types, Types), EvidenceLines)
    ->  format(atom(Part2), ' orbit=~w', [Types])
    ;   member(evidence_line(fpn, divergence, Div), EvidenceLines),
        number(Div)
    ->  format(atom(Part2), ' div=~4f', [Div])
    ;   member(evidence_line(fingerprint, extractive_voids, Voids), EvidenceLines)
    ->  format(atom(Part2), ' voids=~w', [Voids])
    ;   Part2 = ''
    ),
    atomic_list_concat([Part1, Part2], KeyEv).

/* ================================================================
   SECTION 4: TOP HYPOTHESES
   ================================================================ */

report_top_hypotheses(Context) :-
    format('## Highest-Confidence Hypotheses (Top 20)~n~n'),
    abductive_engine:abductive_genuine(Context, AllGenuine),
    findall(Conf-H, (
        member(H, AllGenuine),
        H = hypothesis(_, _, _, _, _, Conf, _)
    ), Pairs),
    msort(Pairs, SortedAsc),
    reverse(SortedAsc, SortedDesc),
    (   length(SortedDesc, N), N > 20
    ->  length(Top20, 20), append(Top20, _, SortedDesc)
    ;   Top20 = SortedDesc
    ),
    (   Top20 = []
    ->  format('No genuine hypotheses to rank.~n~n')
    ;   format('| Rank | Constraint | Class | Confidence | Anomaly |~n'),
        format('|------|------------|-------|------------|---------|~n'),
        format_ranked_hypotheses(Top20, 1),
        format('~n')
    ).

format_ranked_hypotheses([], _).
format_ranked_hypotheses([_Conf-H | Rest], Rank) :-
    H = hypothesis(C, Class, anomaly(AType, _), _, _, Conf, _),
    format('| ~w | ~w | ~w | ~2f | ~w |~n', [Rank, C, Class, Conf, AType]),
    NextRank is Rank + 1,
    format_ranked_hypotheses(Rest, NextRank).

/* ================================================================
   SECTION 5: INVESTIGATION QUEUE
   ================================================================ */

report_investigation_queue(Context, _Constraints) :-
    format('## Investigation Queue~n~n'),
    format('Suggested next steps grouped by investigation action, sorted by priority.~n~n'),

    abductive_engine:abductive_genuine(Context, AllGenuine),
    (   AllGenuine = []
    ->  format('No investigations to suggest.~n~n')
    ;   forall(
            member(Action, [inspect_coupling, monitor_drift, inspect_orbit,
                            review_claim, cross_reference, inspect_metrics]),
            report_investigation_action(Action, AllGenuine)
        )
    ).

report_investigation_action(Action, AllGenuine) :-
    findall(Conf-C-Class, (
        member(H, AllGenuine),
        H = hypothesis(C, Class, _, _, _, Conf, investigation(Action, _))
    ), Pairs),
    (   Pairs = []
    ->  true  % Skip empty actions
    ;   msort(Pairs, SortedAsc),
        reverse(SortedAsc, SortedDesc),
        length(SortedDesc, N),
        format('### ~w (~w constraints)~n~n', [Action, N]),
        action_description(Action, ActionDesc),
        format('~w~n~n', [ActionDesc]),
        format('| Constraint | Class | Confidence |~n'),
        format('|------------|-------|------------|~n'),
        (   N > 10
        ->  length(Top10, 10), append(Top10, _, SortedDesc),
            ShowList = Top10, Remaining is N - 10
        ;   ShowList = SortedDesc, Remaining = 0
        ),
        forall(member(Conf-C-Class, ShowList),
            format('| ~w | ~w | ~2f |~n', [C, Class, Conf])
        ),
        (   Remaining > 0
        ->  format('| ... | (~w more) | |~n', [Remaining])
        ;   true
        ),
        format('~n')
    ).

action_description(inspect_coupling,
    'Examine cross-index coupling patterns and Boltzmann compliance for these constraints.').
action_description(monitor_drift,
    'Track temporal evolution of these constraints across measurement periods.').
action_description(inspect_orbit,
    'Review Dirac gauge orbit structure and perspectival variance for these constraints.').
action_description(review_claim,
    'Revisit the claimed type classification for these constraints against structural evidence.').
action_description(cross_reference,
    'Cross-reference FPN network topology with drift dynamics for these constraints.').
action_description(inspect_metrics,
    'Examine raw metric values and threshold proximity for these constraints.').
