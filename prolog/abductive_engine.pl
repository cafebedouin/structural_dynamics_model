% ============================================================================
% ABDUCTIVE REASONING ENGINE — Cross-Subsystem Anomaly Synthesis (v6.3)
% ============================================================================
% Diagnostic-only annotation layer that monitors signals from all subsystems
% (structural signatures, MaxEnt, FPN, Dirac orbits, drift detection, logical
% fingerprints) and produces structured hypotheses explaining WHY anomalies
% occur across subsystem boundaries.
%
% This module is a READ-ONLY consumer. It does NOT run MaxEnt, FPN, or any
% other subsystem — it probes their dynamic state to check whether they have
% already been run, and activates triggers only for available data.
%
% Architecture (Phase 6B decomposition):
%   abductive_helpers.pl  — shared dynamic facts, override tables, helpers
%   abductive_triggers.pl — T1-T11 trigger definitions
%   abductive_engine.pl   — runner, query API, cleanup, selftest (this file)
%
% Standalone run:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l dirac_classification.pl -l abductive_engine.pl \
%         -g "abductive_engine:abductive_selftest, halt."
% ============================================================================

:- module(abductive_engine, [
    abductive_enabled/0,
    abductive_run/2,              % abductive_run(+Context, -Summary)
    abductive_hypotheses/2,       % abductive_hypotheses(+C, -Hypotheses)
    abductive_hypotheses/3,       % abductive_hypotheses(+C, +Context, -Hypotheses)
    abductive_summary/1,          % abductive_summary(-Summary)
    abductive_by_class/3,         % abductive_by_class(+Class, +Context, -Hypotheses)
    abductive_genuine/2,          % abductive_genuine(+Context, -NonArtifacts)
    abductive_artifacts/2,        % abductive_artifacts(+Context, -Artifacts)
    abductive_cleanup/0,
    abductive_selftest/0
]).

:- use_module(abductive_helpers).
:- use_module(abductive_triggers).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(maxent_classifier).

:- use_module(library(lists)).

/* ================================================================
   CONFIGURATION
   ================================================================ */

abductive_enabled :-
    config:param(abductive_enabled, 1).

/* ================================================================
   CLEANUP
   ================================================================ */

abductive_cleanup :-
    retractall(abd_hypothesis(_, _, _)),
    retractall(abd_run_info(_, _, _)).

/* ================================================================
   ABDUCTIVE RUN — Main Entry Point
   ================================================================
   Cleans up prior state, probes subsystem availability, iterates
   all constraints through available triggers in priority order,
   and stores hypotheses as dynamic facts.

   Trigger ordering:
     1. signature_override_artifact     — first, to establish artifact filter
     2. deep_deception                  — signature + maxent
     3. metric_structural_divergence    — maxent + dirac (skips artifacts)
     4. confirmed_liminal              — maxent + dirac + drift (skips artifacts)
     5. coverage_gap                    — dirac + mismatch
     9. maxent_shadow_divergence        — maxent + signature (FCR shadow)
    10. convergent_structural_stress    — multi-signal convergence
     6. accelerating_pathology          — fpn + drift
     7. contamination_cascade           — fpn + drift
     8. dormant_extraction              — maxent + fingerprint + signature
    11. snare_leaning_tangled           — maxent psi + signature
   ================================================================ */

abductive_run(Context, Summary) :-
    abductive_cleanup,
    available_subsystems(SubsAvail),

    % Collect all constraints
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C), atom(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),

    % Phase 1: Artifact filter + signature triggers (requires maxent + signature)
    run_trigger_over_constraints(abductive_triggers:trigger_signature_override_artifact, Constraints, Context),  % T1
    run_trigger_over_constraints(abductive_triggers:trigger_deep_deception, Constraints, Context),               % T2

    % Phase 2: MaxEnt + Dirac conjunction triggers
    run_trigger_over_constraints(abductive_triggers:trigger_metric_structural_divergence, Constraints, Context), % T3
    run_trigger_over_constraints(abductive_triggers:trigger_confirmed_liminal, Constraints, Context),            % T4
    run_trigger_over_constraints(abductive_triggers:trigger_coverage_gap, Constraints, Context),                 % T5
    run_trigger_over_constraints(abductive_triggers:trigger_maxent_shadow_divergence, Constraints, Context),     % T9

    % Phase 3: Multi-subsystem synthesis + FPN + dormant
    run_trigger_over_constraints(abductive_triggers:trigger_convergent_structural_stress, Constraints, Context), % T10
    run_trigger_over_constraints(abductive_triggers:trigger_accelerating_pathology, Constraints, Context),       % T6
    run_trigger_over_constraints(abductive_triggers:trigger_contamination_cascade, Constraints, Context),        % T7
    run_trigger_over_constraints(abductive_triggers:trigger_dormant_extraction, Constraints, Context),           % T8
    run_trigger_over_constraints(abductive_triggers:trigger_snare_leaning_tangled, Constraints, Context),        % T11

    % Compute summary
    findall(H, abd_hypothesis(_, Context, H), AllHypotheses),
    length(AllHypotheses, NTotal),
    include(is_artifact_hypothesis, AllHypotheses, Artifacts),
    length(Artifacts, NArtifacts),
    NGenuine is NTotal - NArtifacts,

    get_time(Timestamp),
    assertz(abd_run_info(Context, NTotal, Timestamp)),

    Summary = abductive_summary(NTotal, NGenuine, NArtifacts, SubsAvail),

    format(user_error, '[abductive_engine] Run complete: ~w hypotheses (~w genuine, ~w artifacts), subsystems: ~w~n',
           [NTotal, NGenuine, NArtifacts, SubsAvail]).

%% run_trigger_over_constraints(+TriggerPred, +Constraints, +Context)
%  Attempts a trigger predicate on every constraint, catching failures.
run_trigger_over_constraints(TriggerPred, Constraints, Context) :-
    forall(
        member(C, Constraints),
        (   catch(
                (   call(TriggerPred, C, Context, H),
                    assertz(abd_hypothesis(C, Context, H))
                ),
                _Error,
                true  % Silently skip constraints where trigger fails
            )
        ;   true  % forall requires this for determinism
        )
    ).

is_artifact_hypothesis(hypothesis(_, signature_override_artifact, _, _, _, _, _)).

/* ================================================================
   QUERY API
   ================================================================ */

%% abductive_hypotheses(+C, -Hypotheses)
%  All hypotheses for constraint C (any context).
abductive_hypotheses(C, Hypotheses) :-
    findall(H, abd_hypothesis(C, _, H), Hypotheses).

%% abductive_hypotheses(+C, +Context, -Hypotheses)
%  Hypotheses for constraint C in a specific context.
abductive_hypotheses(C, Context, Hypotheses) :-
    findall(H, abd_hypothesis(C, Context, H), Hypotheses).

%% abductive_summary(-Summary)
%  Returns the summary from the most recent run.
abductive_summary(Summary) :-
    abd_run_info(Context, NTotal, _Timestamp),
    findall(H, abd_hypothesis(_, Context, H), AllH),
    include(is_artifact_hypothesis, AllH, Artifacts),
    length(Artifacts, NArtifacts),
    NGenuine is NTotal - NArtifacts,
    available_subsystems(SubsAvail),
    Summary = abductive_summary(NTotal, NGenuine, NArtifacts, SubsAvail).

%% abductive_by_class(+Class, +Context, -Hypotheses)
%  All hypotheses of a given class.
abductive_by_class(Class, Context, Hypotheses) :-
    findall(H,
        (   abd_hypothesis(_, Context, H),
            H = hypothesis(_, Class, _, _, _, _, _)
        ),
        Hypotheses).

%% abductive_genuine(+Context, -NonArtifacts)
%  All non-artifact hypotheses.
abductive_genuine(Context, NonArtifacts) :-
    findall(H,
        (   abd_hypothesis(_, Context, H),
            \+ is_artifact_hypothesis(H)
        ),
        NonArtifacts).

%% abductive_artifacts(+Context, -Artifacts)
%  All artifact hypotheses.
abductive_artifacts(Context, Artifacts) :-
    findall(H,
        (   abd_hypothesis(_, Context, H),
            is_artifact_hypothesis(H)
        ),
        Artifacts).

/* ================================================================
   SELFTEST
   ================================================================
   Standalone validation. Loads corpus, runs MaxEnt (needed for most
   triggers), runs abductive analysis, verifies basic properties.
   ================================================================ */

abductive_selftest :-
    format('=== Abductive Engine Selftest ===~n~n'),

    % Load corpus
    format('Loading corpus...~n'),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),

    % Run MaxEnt (prerequisite for most triggers)
    format('Running MaxEnt classifier...~n'),
    maxent_classifier:maxent_run(Context, MaxEntSummary),
    format('  MaxEnt: ~w~n', [MaxEntSummary]),

    % Run abductive engine
    format('Running abductive engine...~n'),
    abductive_run(Context, Summary),
    Summary = abductive_summary(NTotal, NGenuine, NArtifacts, SubsAvail),
    format('~n--- Results ---~n'),
    format('  Total hypotheses:  ~w~n', [NTotal]),
    format('  Genuine findings:  ~w~n', [NGenuine]),
    format('  Artifacts:         ~w~n', [NArtifacts]),
    format('  Subsystems:        ~w~n~n', [SubsAvail]),

    % Verify artifact count (most hard disagreements should be explained)
    format('--- Verification ---~n'),
    % Artifact count: covers constraints where overrides were actually applied.
    % FCR perspectival gate means not all FCR signatures produce overrides,
    % so artifact count is typically 30-80 (subset of 151 hard disagreements).
    (   NArtifacts > 20
    ->  format('  [PASS] Artifact count > 20 (~w artifacts from applied overrides)~n', [NArtifacts])
    ;   format('  [WARN] Artifact count ~w (expected > 20)~n', [NArtifacts])
    ),

    % Verify no errors (all hypotheses well-formed)
    findall(H, abd_hypothesis(_, Context, H), AllH),
    forall(
        member(H, AllH),
        (   H = hypothesis(_, _, anomaly(_, _), EL, _, Conf, investigation(_, _)),
            is_list(EL),
            number(Conf),
            Conf >= 0.0,
            Conf =< 1.0
        )
    ),
    format('  [PASS] All ~w hypotheses are well-formed~n', [NTotal]),

    % Report by class
    format('~n--- By Class ---~n'),
    forall(
        member(Class, [signature_override_artifact, deep_deception,
                       metric_structural_divergence, confirmed_liminal,
                       coverage_gap, maxent_shadow_divergence,
                       convergent_structural_stress, accelerating_pathology,
                       contamination_cascade, dormant_extraction,
                       snare_leaning_tangled]),
        (   abductive_by_class(Class, Context, ClassH),
            length(ClassH, NClass),
            format('  ~w: ~w~n', [Class, NClass])
        )
    ),

    format('~n=== Selftest Complete ===~n').
