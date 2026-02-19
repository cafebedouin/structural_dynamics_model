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
% Architecture:
%   1. abductive_cleanup/0  — retract prior run's hypotheses
%   2. abductive_run/2      — iterate all constraints through available triggers
%   3. Query API            — hypotheses, summary, by_class, genuine, artifacts
%   4. abductive_selftest/0 — standalone validation
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

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(drl_lifecycle).
:- use_module(logical_fingerprint).

:- use_module(library(lists)).

/* ================================================================
   DYNAMIC FACTS
   ================================================================ */

:- dynamic abd_hypothesis/3.    % abd_hypothesis(Constraint, Context, Hypothesis)
:- dynamic abd_run_info/3.      % abd_run_info(Context, NHypotheses, Timestamp)

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
   HELPER PREDICATES
   ================================================================ */

%% known_override_signature(?Signature)
%  Signatures that unconditionally override the metric-based type.
known_override_signature(false_natural_law).
known_override_signature(false_ci_rope).
known_override_signature(coupling_invariant_rope).
known_override_signature(natural_law).
known_override_signature(coordination_scaffold).
known_override_signature(constructed_low_extraction).
known_override_signature(constructed_high_extraction).
known_override_signature(constructed_constraint).

%% override_target(+Signature, -TargetType)
%  The type that a signature override forces.
override_target(false_natural_law,          tangled_rope).
override_target(false_ci_rope,              tangled_rope).
override_target(coupling_invariant_rope,    rope).
override_target(natural_law,                mountain).
override_target(coordination_scaffold,      rope).
override_target(constructed_low_extraction, rope).
override_target(constructed_high_extraction, tangled_rope).
override_target(constructed_constraint,     tangled_rope).

%% extractive_void(?VoidType)
%  Fingerprint voids that indicate extractive structural patterns.
extractive_void(unaccountable_extraction).
extractive_void(self_sustaining_extraction).
extractive_void(extractive_immutable).
extractive_void(coercion_without_coordination).

%% fpn_zone(+EP, -Zone)
%  Categorizes effective purity into zones (matching fpn_report.pl).
fpn_zone(EP, pure)         :- EP >= 0.80, !.
fpn_zone(EP, clean)        :- EP >= 0.60, !.
fpn_zone(EP, contaminated) :- EP >= 0.40, !.
fpn_zone(EP, compromised)  :- EP >= 0.20, !.
fpn_zone(_,  critical).

%% one_hop_zone(+C, +Context, -Zone)
%  Zone from the standard one-hop effective purity.
one_hop_zone(C, Context, Zone) :-
    catch(drl_modal_logic:effective_purity(C, Context, EP), _, fail),
    fpn_zone(EP, Zone).

%% compute_confidence(+EvidenceLines, +BaseConfidence, -Confidence)
%  Adjusts base confidence by evidence strength. More evidence lines
%  increase confidence slightly. Capped at 1.0.
compute_confidence(EvidenceLines, Base, Confidence) :-
    length(EvidenceLines, N),
    Bonus is min(0.10, N * 0.02),
    Raw is Base + Bonus,
    Confidence is min(1.0, max(0.0, Raw)).

%% subsystem_available(+Subsystem)
%  Checks whether a subsystem's data is present (has been run).
%  Does NOT check enable flags — only whether dynamic state exists.
subsystem_available(maxent) :-
    catch(maxent_classifier:maxent_run_info(_, _, _), _, fail), !.
subsystem_available(fpn) :-
    catch(drl_modal_logic:fpn_iteration_info(_, _, _, _), _, fail), !.
subsystem_available(dirac) :- !.     % Always available (computed on demand)
subsystem_available(drift) :- !.     % Always available (computed on demand)
subsystem_available(signature) :- !. % Always available (part of core pipeline)
subsystem_available(mismatch) :- !.  % Always available (part of core pipeline)
subsystem_available(fingerprint) :-  % Always available (computed on demand)
    !.

%% available_subsystems(-List)
%  Returns list of subsystem atoms that are currently available.
available_subsystems(Subs) :-
    findall(S, (
        member(S, [maxent, fpn, dirac, drift, signature, mismatch, fingerprint]),
        subsystem_available(S)
    ), Subs).

/* ================================================================
   TRIGGER 1: SIGNATURE OVERRIDE ARTIFACT
   ================================================================
   Most pragmatically valuable trigger. Explains MaxEnt hard
   disagreements as mechanistic artifacts of known signature overrides.
   Per-constraint causal attribution of disagreement to override.

   Requires: maxent + signature
   ================================================================ */

% Categorical: Naturality artifact diagnosis — explains cross-functor disagreement as known override effect
trigger_signature_override_artifact(C, Context, Hypothesis) :-
    subsystem_available(maxent),
    % MaxEnt must show a hard disagreement
    catch(maxent_classifier:maxent_disagreement(C, Context, Disagreement), _, fail),
    Disagreement = hard(ShadowType, DetType),
    % Must have a known override signature
    catch(structural_signatures:constraint_signature(C, Sig), _, fail),
    known_override_signature(Sig),
    override_target(Sig, ExpectedTarget),
    % The deterministic type must match the override target
    ExpectedTarget = DetType,
    % Collect evidence
    catch(maxent_classifier:maxent_entropy(C, Context, HNorm), _, (HNorm = 0.0)),
    catch(structural_signatures:purity_score(C, Purity), _, (Purity = -1.0)),
    EvidenceLines = [
        evidence_line(maxent, disagreement, hard(ShadowType, DetType)),
        evidence_line(signature, override, Sig),
        evidence_line(signature, override_target, ExpectedTarget),
        evidence_line(maxent, entropy, HNorm),
        evidence_line(signature, purity_score, Purity)
    ],
    Explanations = [
        explanation(
            override_mechanism,
            high,
            'MaxEnt disagrees because signature override unconditionally forces type. The disagreement is a mechanistic artifact, not a genuine ambiguity.'
        )
    ],
    compute_confidence(EvidenceLines, 0.85, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        signature_override_artifact,
        anomaly(hard_disagreement_with_override, hard(ShadowType, DetType)),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(inspect_metrics, C)
    ).

/* ================================================================
   TRIGGER 2: DEEP DECEPTION
   ================================================================
   FNL signature + MaxEnt P(mountain) > threshold.
   The constraint claims naturality, fails Boltzmann, AND the MaxEnt
   model independently assigns high probability to mountain type.

   Requires: signature + maxent
   ================================================================ */

trigger_deep_deception(C, Context, Hypothesis) :-
    subsystem_available(maxent),
    % Must have FNL signature
    catch(structural_signatures:false_natural_law(C, FNLEvidence), _, fail),
    % MaxEnt must assign high P(mountain)
    catch(maxent_classifier:maxent_distribution(C, Context, Dist), _, fail),
    member(mountain-PMountain, Dist),
    config:param(abductive_maxent_mountain_deception, MountainThresh),
    PMountain > MountainThresh,
    % Collect evidence
    catch(maxent_classifier:maxent_entropy(C, Context, HNorm), _, (HNorm = 0.0)),
    EvidenceLines = [
        evidence_line(signature, false_natural_law, FNLEvidence),
        evidence_line(maxent, p_mountain, PMountain),
        evidence_line(maxent, entropy, HNorm)
    ],
    Explanations = [
        explanation(
            metric_camouflage,
            high,
            'Constraint claims naturality and fails Boltzmann compliance, yet metric profile is so mountain-like that MaxEnt independently assigns high P(mountain). The deception is metrically deep.'
        ),
        explanation(
            superficial_mismatch,
            low,
            'FNL detection is a false positive and the constraint is genuinely mountain-like.'
        )
    ],
    compute_confidence(EvidenceLines, 0.70, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        deep_deception,
        anomaly(fnl_with_mountain_metrics, PMountain),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(inspect_coupling, C)
    ).

/* ================================================================
   TRIGGER 3: METRIC-STRUCTURAL DIVERGENCE
   ================================================================
   High MaxEnt entropy + preserved single-type Dirac orbit.
   The constraint is near a metric boundary but structurally
   unambiguous — divergence between metric and structural identity.

   Requires: maxent + dirac
   ================================================================ */

trigger_metric_structural_divergence(C, Context, Hypothesis) :-
    subsystem_available(maxent),
    % High entropy in MaxEnt
    catch(maxent_classifier:maxent_entropy(C, Context, HNorm), _, fail),
    config:param(maxent_uncertainty_threshold, EntropyThresh),
    HNorm > EntropyThresh,
    % Must NOT be a known artifact (skip if already explained by override)
    \+ abd_hypothesis(C, Context, hypothesis(_, signature_override_artifact, _, _, _, _, _)),
    % Dirac orbit must be single-type (preserved)
    catch(dirac_classification:preserved_under_context_shift(C, Result), _, fail),
    Result = preserved(PreservedType),
    % Collect evidence
    catch(maxent_classifier:maxent_top_type(C, Context, ShadowTop), _, (ShadowTop = unknown)),
    catch(drl_core:dr_type(C, Context, DetType), _, (DetType = unknown)),
    EvidenceLines = [
        evidence_line(maxent, entropy, HNorm),
        evidence_line(maxent, shadow_top, ShadowTop),
        evidence_line(dirac, orbit_class, preserved(PreservedType)),
        evidence_line(signature, det_type, DetType)
    ],
    Explanations = [
        explanation(
            metric_boundary_proximity,
            high,
            'Constraint sits near a metric classification boundary (high entropy) but Dirac orbit analysis confirms stable structural identity across all contexts. The ambiguity is metric, not structural.'
        ),
        explanation(
            genuine_ambiguity,
            low,
            'The preserved orbit misses a dimension that would reveal structural instability.'
        )
    ],
    compute_confidence(EvidenceLines, 0.65, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        metric_structural_divergence,
        anomaly(high_entropy_preserved_orbit, HNorm-PreservedType),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(inspect_metrics, C)
    ).

/* ================================================================
   TRIGGER 4: CONFIRMED LIMINAL
   ================================================================
   High MaxEnt entropy + multi-type Dirac orbit + drift events.
   Three independent signals confirming genuine structural liminality.

   Requires: maxent + dirac + drift
   ================================================================ */

trigger_confirmed_liminal(C, Context, Hypothesis) :-
    subsystem_available(maxent),
    % High entropy
    catch(maxent_classifier:maxent_entropy(C, Context, HNorm), _, fail),
    config:param(maxent_uncertainty_threshold, EntropyThresh),
    HNorm > EntropyThresh,
    % Must NOT be a known artifact
    \+ abd_hypothesis(C, Context, hypothesis(_, signature_override_artifact, _, _, _, _, _)),
    % Multi-type Dirac orbit
    catch(dirac_classification:gauge_orbit(C, OrbitPoints), _, fail),
    findall(T, member(orbit_point(T, _), OrbitPoints), Types),
    sort(Types, UniqueTypes),
    length(UniqueTypes, NTypes),
    NTypes > 1,
    % Drift events present
    catch(drl_lifecycle:scan_constraint_drift(C, DriftEvents), _, (DriftEvents = [])),
    DriftEvents \= [],
    % Collect evidence
    length(DriftEvents, NDrift),
    findall(DT, member(drift(DT, _, _), DriftEvents), DriftTypes),
    EvidenceLines = [
        evidence_line(maxent, entropy, HNorm),
        evidence_line(dirac, orbit_types, UniqueTypes),
        evidence_line(dirac, n_orbit_types, NTypes),
        evidence_line(drift, n_events, NDrift),
        evidence_line(drift, event_types, DriftTypes)
    ],
    Explanations = [
        explanation(
            genuine_liminality,
            high,
            'Three independent subsystems agree: metrics are ambiguous (high entropy), structural identity varies across contexts (multi-type orbit), and temporal dynamics are active (drift events). This constraint is genuinely in transition.'
        )
    ],
    compute_confidence(EvidenceLines, 0.75, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        confirmed_liminal,
        anomaly(triple_confirmed_liminality, NTypes-NDrift),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(monitor_drift, C)
    ).

/* ================================================================
   TRIGGER 5: COVERAGE GAP
   ================================================================
   Multi-type Dirac orbit but no perspectival_incoherence from
   dr_mismatch. Identifies the diagnostic gap: gauge_orbit checks
   all 4 standard contexts, but dr_mismatch excludes analytical
   context and uses cut after first match.

   Requires: dirac + mismatch (absence detection)
   ================================================================ */

trigger_coverage_gap(C, Context, Hypothesis) :-
    % Multi-type Dirac orbit (gauge-variant)
    catch(dirac_classification:gauge_orbit(C, OrbitPoints), _, fail),
    findall(T, member(orbit_point(T, _), OrbitPoints), Types),
    sort(Types, UniqueTypes),
    length(UniqueTypes, NTypes),
    NTypes > 1,
    % dr_mismatch does NOT fire perspectival_incoherence for this constraint
    \+ (catch(drl_core:dr_mismatch(C, _, perspectival_incoherence, _), _, fail)),
    % Collect evidence
    catch(drl_core:dr_type(C, Context, DetType), _, (DetType = unknown)),
    EvidenceLines = [
        evidence_line(dirac, orbit_types, UniqueTypes),
        evidence_line(dirac, n_orbit_types, NTypes),
        evidence_line(mismatch, perspectival_incoherence, absent),
        evidence_line(signature, det_type, DetType)
    ],
    Explanations = [
        explanation(
            dr_mismatch_blind_spot,
            high,
            'gauge_orbit/2 detects context-variant classification across all 4 standard contexts, but dr_mismatch/4 did not fire perspectival_incoherence. This is a known coverage gap: dr_mismatch uses cut after first match and excludes analytical context.'
        ),
        explanation(
            non_standard_variance,
            medium,
            'The orbit variance occurs only between analytical and non-analytical contexts, which dr_mismatch intentionally excludes.'
        )
    ],
    compute_confidence(EvidenceLines, 0.60, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        coverage_gap,
        anomaly(orbit_without_mismatch, UniqueTypes),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(inspect_orbit, C)
    ).

/* ================================================================
   TRIGGER 9: MAXENT SHADOW DIVERGENCE
   ================================================================
   MaxEnt strongly favors a type different from the constraint's
   signature override target. Catches FCR-gated constraints where
   both dr_type and MaxEnt agree (so T1 doesn't fire) but MaxEnt
   diverges from the intended override target.

   Requires: maxent + signature
   ================================================================ */

trigger_maxent_shadow_divergence(C, Context, Hypothesis) :-
    subsystem_available(maxent),
    % Must have a known override signature with a target
    catch(structural_signatures:constraint_signature(C, Sig), _, fail),
    known_override_signature(Sig),
    override_target(Sig, OverrideTarget),
    % MaxEnt top type must differ from override target
    catch(maxent_classifier:maxent_top_type(C, Context, MaxEntTop), _, fail),
    MaxEntTop \== OverrideTarget,
    % MaxEnt must be confident in its top type
    catch(maxent_classifier:maxent_distribution(C, Context, Dist), _, fail),
    member(MaxEntTop-PTop, Dist),
    config:param(abductive_shadow_divergence_threshold, ShadowThresh),
    PTop > ShadowThresh,
    % Must NOT already be explained by T1 artifact
    \+ abd_hypothesis(C, Context, hypothesis(_, signature_override_artifact, _, _, _, _, _)),
    % Collect evidence
    catch(maxent_classifier:maxent_entropy(C, Context, HNorm), _, (HNorm = 0.0)),
    catch(drl_core:dr_type(C, Context, DetType), _, (DetType = unknown)),
    EvidenceLines = [
        evidence_line(maxent, top_type, MaxEntTop),
        evidence_line(maxent, top_prob, PTop),
        evidence_line(signature, override, Sig),
        evidence_line(signature, override_target, OverrideTarget),
        evidence_line(maxent, entropy, HNorm),
        evidence_line(signature, det_type, DetType)
    ],
    Explanations = [
        explanation(
            shadow_override_tension,
            high,
            'MaxEnt strongly favors a type that differs from the signature override target. The override intent (based on structural signature) is contradicted by the metric-based probability distribution. This suggests the FCR gate deferred the override, leaving the metric type dominant.'
        ),
        explanation(
            signature_miscalibration,
            medium,
            'The override target may be incorrect for this constraint, or the FCR perspectival gate is correctly deferring because the constraint is genuinely the metric type.'
        )
    ],
    compute_confidence(EvidenceLines, 0.75, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        maxent_shadow_divergence,
        anomaly(shadow_override_tension, MaxEntTop-OverrideTarget),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(inspect_classification, C)
    ).

/* ================================================================
   TRIGGER 10: CONVERGENT STRUCTURAL STRESS
   ================================================================
   3+ independent stress indicators converge on the same constraint.
   Catches FCR-gated constraints that are invisible to entropy-based
   triggers (T3, T4) because their entropy is low (metrically confident
   but structurally stressed).

   Requires: signature (+ optional maxent, drift)
   ================================================================ */

%% stress_indicator(+C, +Context, -Indicator)
%  Individual stress signal checks. Each succeeding clause is independent.
stress_indicator(C, _Context, has_false_signature) :-
    catch(structural_signatures:constraint_signature(C, Sig), _, fail),
    member(Sig, [false_ci_rope, false_natural_law]).

stress_indicator(C, _Context, low_purity) :-
    catch(structural_signatures:purity_score(C, P), _, fail),
    P >= 0.0,
    config:param(abductive_stress_purity_threshold, PT),
    P < PT.

stress_indicator(C, _Context, has_drift) :-
    catch(drl_lifecycle:scan_constraint_drift(C, DriftEvents), _, fail),
    config:param(abductive_stress_drift_mode, DM),
    stress_drift_satisfied(DM, DriftEvents).

stress_indicator(C, _Context, high_coupling) :-
    catch(structural_signatures:cross_index_coupling(C, Coupling), _, fail),
    config:param(abductive_stress_coupling_threshold, CT),
    Coupling > CT.

stress_indicator(C, Context, elevated_entropy) :-
    subsystem_available(maxent),
    catch(maxent_classifier:maxent_entropy(C, Context, H), _, fail),
    config:param(abductive_stress_entropy_threshold, ET),
    H > ET.

%% stress_drift_satisfied(+DriftMode, +DriftEvents)
%  Dispatches drift check based on config param abductive_stress_drift_mode.
stress_drift_satisfied(any, Events) :-
    Events = [_|_].
stress_drift_satisfied(critical, Events) :-
    member(drift(_, _, critical), Events), !.
stress_drift_satisfied(count_2plus, Events) :-
    length(Events, N),
    N >= 2.

%% stress_indicator_count(+C, +Context, -Count, -Indicators)
%  Counts how many independent stress indicators fire for constraint C.
stress_indicator_count(C, Context, Count, Indicators) :-
    findall(Ind, stress_indicator(C, Context, Ind), Indicators),
    length(Indicators, Count).

trigger_convergent_structural_stress(C, Context, Hypothesis) :-
    % Count stress signals
    stress_indicator_count(C, Context, NSignals, Indicators),
    config:param(abductive_stress_convergence_min, MinSignals),
    NSignals >= MinSignals,
    % Collect evidence
    catch(structural_signatures:purity_score(C, Purity), _, (Purity = -1.0)),
    catch(structural_signatures:cross_index_coupling(C, Coupling), _, (Coupling = -1.0)),
    (   catch(drl_lifecycle:scan_constraint_drift(C, DriftEvs), _, fail)
    ->  length(DriftEvs, NDrift)
    ;   NDrift = 0
    ),
    catch(drl_core:dr_type(C, Context, DetType), _, (DetType = unknown)),
    EvidenceLines = [
        evidence_line(multi, stress_signals, Indicators),
        evidence_line(multi, n_signals, NSignals),
        evidence_line(signature, purity_score, Purity),
        evidence_line(signature, coupling, Coupling),
        evidence_line(drift, n_events, NDrift),
        evidence_line(signature, det_type, DetType)
    ],
    Explanations = [
        explanation(
            multi_signal_convergence,
            high,
            'Multiple independent stress indicators converge on this constraint. Each signal alone might be within normal bounds, but their convergence suggests genuine structural stress that no single-subsystem trigger would detect.'
        ),
        explanation(
            coincidental_convergence,
            low,
            'The stress signals may be correlated rather than independent, inflating the apparent convergence count.'
        )
    ],
    % Confidence scales with signal count: 0.45 + NSignals * 0.06
    BaseConf is 0.45 + NSignals * 0.06,
    compute_confidence(EvidenceLines, BaseConf, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        convergent_structural_stress,
        anomaly(multi_signal_convergence, NSignals),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(review_claim, C)
    ).

/* ================================================================
   TRIGGER 6: ACCELERATING PATHOLOGY
   ================================================================
   FPN zone migration + purity_drift event detected.
   Static equilibrium shows contamination AND temporal dynamics
   confirm it is actively worsening.

   Requires: fpn + drift
   ================================================================ */

trigger_accelerating_pathology(C, Context, Hypothesis) :-
    subsystem_available(fpn),
    % FPN shows zone migration (FPN zone differs from one-hop zone)
    catch(drl_modal_logic:fpn_ep(C, Context, FPNEP), _, fail),
    fpn_zone(FPNEP, FPNZone),
    one_hop_zone(C, Context, OneHopZone),
    FPNZone \= OneHopZone,
    % Purity drift event detected
    catch(drl_lifecycle:drift_event(C, purity_drift, PurityEvidence), _, fail),
    % Collect evidence
    catch(structural_signatures:purity_score(C, Purity), _, (Purity = -1.0)),
    EvidenceLines = [
        evidence_line(fpn, fpn_ep, FPNEP),
        evidence_line(fpn, fpn_zone, FPNZone),
        evidence_line(fpn, one_hop_zone, OneHopZone),
        evidence_line(drift, purity_drift, PurityEvidence),
        evidence_line(signature, purity_score, Purity)
    ],
    Explanations = [
        explanation(
            active_degradation,
            high,
            'FPN equilibrium analysis shows zone migration (multi-hop contamination worse than one-hop) AND purity drift confirms temporal degradation. The pathology is accelerating.'
        ),
        explanation(
            equilibrium_artifact,
            low,
            'FPN zone migration may be a convergence artifact if iteration count was low.'
        )
    ],
    compute_confidence(EvidenceLines, 0.70, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        accelerating_pathology,
        anomaly(zone_migration_with_drift, FPNZone-OneHopZone),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(monitor_drift, C)
    ).

/* ================================================================
   TRIGGER 7: CONTAMINATION CASCADE
   ================================================================
   FPN EP divergence > threshold + network_drift detected.
   Distinguishes theoretical vulnerability from ongoing propagation.

   Requires: fpn + drift
   ================================================================ */

trigger_contamination_cascade(C, Context, Hypothesis) :-
    subsystem_available(fpn),
    % FPN EP divergence exceeds threshold
    catch(drl_modal_logic:fpn_ep(C, Context, FPNEP), _, fail),
    catch(drl_modal_logic:effective_purity(C, Context, OneHopEP), _, fail),
    Divergence is abs(OneHopEP - FPNEP),
    config:param(abductive_fpn_divergence_threshold, DivThresh),
    Divergence > DivThresh,
    % Network drift detected for this constraint
    catch(drl_lifecycle:detect_network_drift(C, Context, NetworkEvidence), _, fail),
    % Collect evidence
    EvidenceLines = [
        evidence_line(fpn, fpn_ep, FPNEP),
        evidence_line(fpn, one_hop_ep, OneHopEP),
        evidence_line(fpn, divergence, Divergence),
        evidence_line(drift, network_drift, NetworkEvidence)
    ],
    Explanations = [
        explanation(
            active_propagation,
            high,
            'Multi-hop FPN equilibrium diverges from one-hop purity AND network drift detection confirms active contamination propagation. This is not just structural vulnerability — contamination is spreading.'
        ),
        explanation(
            static_vulnerability,
            medium,
            'The divergence may reflect static network topology rather than active propagation. Network drift evidence strengthens the active interpretation.'
        )
    ],
    compute_confidence(EvidenceLines, 0.65, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        contamination_cascade,
        anomaly(fpn_divergence_with_network_drift, Divergence),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(cross_reference, C)
    ).

/* ================================================================
   TRIGGER 8: DORMANT EXTRACTION
   ================================================================
   Low MaxEnt entropy + clean type + extractive fingerprint voids
   + coupling above threshold. Looks clean but has structural
   indicators of hidden extraction.

   Requires: maxent + fingerprint + signature
   ================================================================ */

trigger_dormant_extraction(C, Context, Hypothesis) :-
    subsystem_available(maxent),
    % Low entropy (constraint looks unambiguous)
    catch(maxent_classifier:maxent_entropy(C, Context, HNorm), _, fail),
    config:param(abductive_dormant_entropy_ceiling, EntropyCeiling),
    HNorm =< EntropyCeiling,
    % Classified as a "clean" type (rope or mountain)
    catch(drl_core:dr_type(C, Context, DetType), _, fail),
    member(DetType, [rope, mountain]),
    % Has extractive fingerprint voids
    catch(logical_fingerprint:fingerprint_voids(C, Voids), _, fail),
    Voids \= [],
    include(is_extractive_void, Voids, ExtractiveVoids),
    ExtractiveVoids \= [],
    % Coupling above a meaningful threshold
    catch(structural_signatures:cross_index_coupling(C, Coupling), _, fail),
    Coupling > 0.10,
    % Collect evidence
    EvidenceLines = [
        evidence_line(maxent, entropy, HNorm),
        evidence_line(signature, det_type, DetType),
        evidence_line(fingerprint, extractive_voids, ExtractiveVoids),
        evidence_line(signature, coupling, Coupling)
    ],
    length(ExtractiveVoids, NVoids),
    Explanations = [
        explanation(
            hidden_extraction,
            high,
            'Constraint appears metrically clean (low entropy, clean type) but structural analysis reveals extractive voids and non-trivial coupling. The extraction is dormant or naturalized.'
        ),
        explanation(
            benign_coupling,
            low,
            'Coupling is genuine coordination, and fingerprint voids are artifacts of sparse data rather than hidden extraction.'
        )
    ],
    % Confidence scales with number of extractive voids
    BaseConf is min(0.70, 0.50 + NVoids * 0.05),
    compute_confidence(EvidenceLines, BaseConf, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        dormant_extraction,
        anomaly(clean_appearance_extractive_structure, DetType-ExtractiveVoids),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(review_claim, C)
    ).

is_extractive_void(V) :- extractive_void(V).

/* ================================================================
   TRIGGER 11: SNARE-LEANING TANGLED
   ================================================================
   Override target is tangled_rope but MaxEnt psi (snare-lean ratio)
   exceeds threshold. Psi = P(snare) / (P(rope) + P(snare) + 0.001).
   High psi means MaxEnt sees this as overwhelmingly snare-like despite
   the tangled_rope override.

   Requires: maxent + signature
   ================================================================ */

trigger_snare_leaning_tangled(C, Context, Hypothesis) :-
    subsystem_available(maxent),
    % Override target must be tangled_rope (via signature or dr_type)
    (   catch(structural_signatures:constraint_signature(C, Sig), _, fail),
        override_target(Sig, tangled_rope)
    ->  true
    ;   catch(drl_core:dr_type(C, Context, tangled_rope), _, fail)
    ),
    % Compute psi from MaxEnt distribution
    catch(maxent_classifier:maxent_distribution(C, Context, Dist), _, fail),
    member(snare-PSnare, Dist),
    member(rope-PRope, Dist),
    % P(snare) must exceed floor (primary discriminator when P(rope)~0)
    config:param(abductive_snare_lean_psnare_floor, PSnareFloor),
    PSnare > PSnareFloor,
    % Psi ratio must also exceed threshold
    Psi is PSnare / (PRope + PSnare + 0.001),
    config:param(abductive_snare_lean_psi_threshold, PsiThresh),
    Psi > PsiThresh,
    % Collect evidence
    catch(maxent_classifier:maxent_entropy(C, Context, HNorm), _, (HNorm = 0.0)),
    catch(drl_core:dr_type(C, Context, DetType), _, (DetType = unknown)),
    EvidenceLines = [
        evidence_line(maxent, snare_psi, Psi),
        evidence_line(maxent, p_snare, PSnare),
        evidence_line(maxent, p_rope, PRope),
        evidence_line(maxent, psnare_floor, PSnareFloor),
        evidence_line(maxent, entropy, HNorm),
        evidence_line(signature, det_type, DetType)
    ],
    Explanations = [
        explanation(
            snare_dominant_tangled,
            high,
            'Despite tangled_rope classification, P(snare) exceeds the snare-lean floor and the psi ratio is extremely high. The constraint behaves overwhelmingly like a snare from a probability standpoint, with almost no rope-like characteristics in the metric distribution.'
        ),
        explanation(
            tangled_with_snare_metrics,
            medium,
            'The constraint may genuinely be tangled_rope with snare-dominant metrics due to its position in the tangled zone. The high psi reflects metric dominance, not misclassification.'
        )
    ],
    compute_confidence(EvidenceLines, 0.65, Confidence),
    config:param(abductive_confidence_floor, Floor),
    Confidence >= Floor,
    Hypothesis = hypothesis(
        C,
        snare_leaning_tangled,
        anomaly(high_snare_psi, Psi),
        EvidenceLines,
        Explanations,
        Confidence,
        investigation(inspect_decomposition, C)
    ).

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
    run_trigger_over_constraints(trigger_signature_override_artifact, Constraints, Context),  % T1
    run_trigger_over_constraints(trigger_deep_deception, Constraints, Context),               % T2

    % Phase 2: MaxEnt + Dirac conjunction triggers
    run_trigger_over_constraints(trigger_metric_structural_divergence, Constraints, Context), % T3
    run_trigger_over_constraints(trigger_confirmed_liminal, Constraints, Context),            % T4
    run_trigger_over_constraints(trigger_coverage_gap, Constraints, Context),                 % T5
    run_trigger_over_constraints(trigger_maxent_shadow_divergence, Constraints, Context),     % T9

    % Phase 3: Multi-subsystem synthesis + FPN + dormant
    run_trigger_over_constraints(trigger_convergent_structural_stress, Constraints, Context), % T10
    run_trigger_over_constraints(trigger_accelerating_pathology, Constraints, Context),       % T6
    run_trigger_over_constraints(trigger_contamination_cascade, Constraints, Context),        % T7
    run_trigger_over_constraints(trigger_dormant_extraction, Constraints, Context),           % T8
    run_trigger_over_constraints(trigger_snare_leaning_tangled, Constraints, Context),        % T11

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
