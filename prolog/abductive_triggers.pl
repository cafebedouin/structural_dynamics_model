% ============================================================================
% ABDUCTIVE TRIGGERS — Cross-Subsystem Anomaly Trigger Definitions (v6.3)
% ============================================================================
% Extracted from abductive_engine.pl (Phase 6B decomposition).
%
% Contains all 11 trigger predicates (T1-T11) and their helpers.
% Each trigger produces a hypothesis term when its conditions are met.
%
% Trigger execution order (preserved exactly from abductive_engine):
%   T1:  signature_override_artifact     — artifact filter (must be first)
%   T2:  deep_deception                  — signature + maxent
%   T3:  metric_structural_divergence    — maxent + dirac (skips T1 artifacts)
%   T4:  confirmed_liminal              — maxent + dirac + drift (skips T1)
%   T5:  coverage_gap                    — dirac + mismatch
%   T9:  maxent_shadow_divergence        — maxent + signature (FCR shadow)
%   T10: convergent_structural_stress    — multi-signal convergence
%   T6:  accelerating_pathology          — fpn + drift
%   T7:  contamination_cascade           — fpn + drift
%   T8:  dormant_extraction              — maxent + fingerprint + signature
%   T11: snare_leaning_tangled           — maxent psi + signature
%
% Imports helpers and dynamic facts from abductive_helpers.pl only.
% Does NOT import abductive_engine.pl — avoids circular dependency.
% ============================================================================

:- module(abductive_triggers, [
    trigger_signature_override_artifact/3,
    trigger_deep_deception/3,
    trigger_metric_structural_divergence/3,
    trigger_confirmed_liminal/3,
    trigger_coverage_gap/3,
    trigger_maxent_shadow_divergence/3,
    trigger_convergent_structural_stress/3,
    trigger_accelerating_pathology/3,
    trigger_contamination_cascade/3,
    trigger_dormant_extraction/3,
    trigger_snare_leaning_tangled/3
]).

:- use_module(abductive_helpers).
:- use_module(config).
:- use_module(signature_detection, [constraint_signature/2, false_natural_law/2]).
:- use_module(boltzmann_compliance, [cross_index_coupling/2]).
:- use_module(purity_scoring, [purity_score/2]).
:- use_module(drl_lifecycle).
:- use_module(grothendieck_cohomology).
:- use_module(logical_fingerprint).
:- use_module(maxent_classifier).
:- use_module(dirac_classification).
:- use_module(drl_core).

:- use_module(library(lists)).

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
    catch(signature_detection:constraint_signature(C, Sig), _, fail),
    known_override_signature(Sig),
    override_target(Sig, ExpectedTarget),
    % The deterministic type must match the override target
    ExpectedTarget = DetType,
    % Collect evidence
    catch(maxent_classifier:maxent_entropy(C, Context, HNorm), _, (HNorm = 0.0)),
    catch(purity_scoring:purity_score(C, Purity), _, (Purity = -1.0)),
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
    catch(signature_detection:false_natural_law(C, FNLEvidence), _, fail),
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
    catch(signature_detection:constraint_signature(C, Sig), _, fail),
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
    catch(signature_detection:constraint_signature(C, Sig), _, fail),
    member(Sig, [false_ci_rope, false_natural_law]).

stress_indicator(C, _Context, low_purity) :-
    catch(purity_scoring:purity_score(C, P), _, fail),
    P >= 0.0,
    config:param(abductive_stress_purity_threshold, PT),
    P < PT.

stress_indicator(C, _Context, has_drift) :-
    catch(drl_lifecycle:scan_constraint_drift(C, DriftEvents), _, fail),
    config:param(abductive_stress_drift_mode, DM),
    stress_drift_satisfied(DM, DriftEvents).

stress_indicator(C, _Context, high_coupling) :-
    catch(boltzmann_compliance:cross_index_coupling(C, Coupling), _, fail),
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

%% rare_stress_gate(+C, +Context, -Signal)
%  Succeeds if C has at least one rare anomaly signal.
%  Returns the first matching signal for evidence recording.
rare_stress_gate(C, Context, maxent_hard_disagreement) :-
    subsystem_available(maxent),
    maxent_classifier:maxent_disagreement(C, Context, hard(_, _)), !.
rare_stress_gate(C, _, h1_hub_conflict(H1)) :-
    subsystem_available(cohomology),
    grothendieck_cohomology:cohomological_obstruction(C, _, H1),
    H1 >= 4, !.

trigger_convergent_structural_stress(C, Context, Hypothesis) :-
    % Common core: N signals above minimum
    stress_indicator_count(C, Context, NSignals, Indicators),
    config:param(abductive_stress_convergence_min, MinSignals),
    NSignals >= MinSignals,
    % Rare gate: at least one anomalous signal
    rare_stress_gate(C, Context, RareSignal),
    % Collect evidence
    catch(purity_scoring:purity_score(C, Purity), _, (Purity = -1.0)),
    catch(boltzmann_compliance:cross_index_coupling(C, Coupling), _, (Coupling = -1.0)),
    (   catch(drl_lifecycle:scan_constraint_drift(C, DriftEvs), _, fail)
    ->  length(DriftEvs, NDrift)
    ;   NDrift = 0
    ),
    catch(drl_core:dr_type(C, Context, DetType), _, (DetType = unknown)),
    EvidenceLines = [
        evidence_line(multi, stress_signals, Indicators),
        evidence_line(multi, n_signals, NSignals),
        evidence_line(rare_gate, signal, RareSignal),
        evidence_line(signature, purity_score, Purity),
        evidence_line(signature, coupling, Coupling),
        evidence_line(drift, n_events, NDrift),
        evidence_line(signature, det_type, DetType)
    ],
    Explanations = [
        explanation(
            multi_signal_convergence,
            high,
            'Multiple independent stress indicators converge AND at least one rare anomaly signal is present. The common core confirms broadly stressed membership; the rare gate establishes genuine structural anomaly.'
        ),
        explanation(
            coincidental_convergence,
            low,
            'The stress signals may be correlated rather than independent, inflating the apparent convergence count.'
        )
    ],
    % Confidence: base + rare gate bonus
    RareBonus = 0.05,
    BaseConf is 0.45 + NSignals * 0.06 + RareBonus,
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
    catch(purity_scoring:purity_score(C, Purity), _, (Purity = -1.0)),
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
    catch(boltzmann_compliance:cross_index_coupling(C, Coupling), _, fail),
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
    (   catch(signature_detection:constraint_signature(C, Sig), _, fail),
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
