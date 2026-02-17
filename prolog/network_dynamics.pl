% ============================================================================
% NETWORK DYNAMICS — Network Drift Detection, Contagion, Cascades
% Split from drl_lifecycle.pl
% ============================================================================

:- module(network_dynamics, [
    detect_network_drift/3,
    network_drift_contagion/3,
    network_drift_velocity/4,
    cascade_prediction/3,
    network_stability_assessment/2,
    network_drift_severity/3
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(drl_modal_logic).
:- use_module(drift_events).

/* ================================================================
   NETWORK DRIFT DYNAMICS (v5.2)

   Type 11 — Network Drift (ND)
   A constraint whose effective purity is declining because
   contamination from drifting neighbors is increasing over time.
   This bridges Stage 8 (network topology) with the temporal
   drift engine to detect induced degradation.

   ND(C) ≡
       ∃ neighbor N of C:
           EdgeContamination(N→C) > 0
           ∧ PurityDeclineSignal(N) exists
       ∧ EffectivePurity(C) < IntrinsicPurity(C)

   Key predicates:
   A. detect_network_drift/3       — Top-level detection
   B. network_drift_contagion/3    — Identify contaminating drifting neighbors
   C. network_drift_velocity/4     — Derived EP change rate from network
   D. cascade_prediction/3         — Threshold crossing time predictions
   E. network_stability_assessment/2 — Network-wide classification
   F. network_drift_severity/3     — Severity with hub/multi-source escalation

   SHADOW MODE: Detected and logged. Does not trigger
   classification changes.

   GRACEFUL DEGRADATION: When only one constraint is loaded,
   all network drift predicates return empty/zero/stable results
   and drift_event(C, network_drift, _) fails — completely
   invisible for single-constraint workflows.
   ================================================================ */

/* ----------------------------------------------------------------
   A. detect_network_drift/3
   Top-level: detects when C's effective purity is declining
   due to contamination from drifting neighbors.
   ---------------------------------------------------------------- */

%% detect_network_drift(+C, +Context, -Evidence)
%  Succeeds if at least one neighbor is both contaminating C
%  and showing purity decline signals.
detect_network_drift(C, Context, evidence(drifting_neighbors, ContagionList, effective_purity, EP, intrinsic_purity, IP)) :-
    constraint_indexing:valid_context(Context),
    drl_modal_logic:effective_purity(C, Context, EP, _Components),
    structural_signatures:purity_score(C, IP),
    IP >= 0.0,
    network_drift_contagion(C, Context, ContagionList),
    ContagionList \= [].

/* ----------------------------------------------------------------
   B. network_drift_contagion/3
   For each neighbor, checks if it is both currently contaminating
   C (Delta > 0, EdgeContam > 0) and showing purity decline signals.
   ---------------------------------------------------------------- */

%% network_drift_contagion(+C, +Context, -ContagionList)
%  Returns [contagion(Neighbor, EdgeContam, DriftSignals), ...]
network_drift_contagion(C, Context, ContagionList) :-
    constraint_indexing:valid_context(Context),
    structural_signatures:purity_score(C, MyPurity),
    (   MyPurity < 0.0
    ->  ContagionList = []
    ;   drl_modal_logic:constraint_neighbors(C, Context, Neighbors),
        findall(
            contagion(Other, EdgeContam, DriftSignals),
            (   member(neighbor(Other, EdgeStrength, _Src), Neighbors),
                compute_neighbor_contamination(MyPurity, Other, EdgeStrength, Context, EdgeContam),
                EdgeContam > 0.0,
                drift_events:collect_purity_decline_signals(Other, DriftSignals),
                DriftSignals \= []
            ),
            ContagionList)
    ).

%% compute_neighbor_contamination(+MyPurity, +Other, +EdgeStrength, +Context, -EdgeContam)
%  Inline edge contamination using same formula as compute_edge_contamination/7.
compute_neighbor_contamination(MyPurity, Other, EdgeStrength, Context, EdgeContam) :-
    structural_signatures:purity_score(Other, OtherPurity),
    OtherPurity >= 0.0,
    Delta is max(0.0, MyPurity - OtherPurity),
    (   Delta > 0.0
    ->  config:param(purity_attenuation_factor, AttFactor),
        config:param(purity_contamination_cap, Cap),
        (   drl_core:dr_type(Other, Context, OtherType)
        ->  drl_modal_logic:type_contamination_strength(OtherType, TypeFactor)
        ;   TypeFactor = 0.0
        ),
        RawContam is Delta * EdgeStrength * AttFactor * TypeFactor,
        EdgeContam is min(Cap, RawContam)
    ;   EdgeContam = 0.0
    ).
compute_neighbor_contamination(_, _, _, _, 0.0).

/* ----------------------------------------------------------------
   C. network_drift_velocity/4
   Derived rate of effective purity change from network effects.
   For each contaminating neighbor with drift_velocity on
   base_extractiveness > 0, computes sensitivity and contribution.
   ---------------------------------------------------------------- */

%% network_drift_velocity(+C, +Context, -Velocity, -Contributors)
%  Velocity = sum of contributions (positive = degrading).
%  Contributors = [contributor(Neighbor, Rate, Sensitivity, Contribution), ...]
network_drift_velocity(C, Context, Velocity, Contributors) :-
    constraint_indexing:valid_context(Context),
    drl_modal_logic:constraint_neighbors(C, Context, Neighbors),
    structural_signatures:purity_score(C, MyPurity),
    (   MyPurity < 0.0
    ->  Velocity = 0.0, Contributors = []
    ;   drl_core:dr_type(C, Context, MyType),
        drl_modal_logic:type_immunity(MyType, Immunity),
        findall(
            contributor(Other, Rate, Sensitivity, Contribution),
            (   member(neighbor(Other, EdgeStrength, _Src), Neighbors),
                drift_events:drift_velocity(Other, base_extractiveness, Rate),
                Rate > 0,
                config:param(purity_attenuation_factor, AttFactor),
                (   drl_core:dr_type(Other, Context, OtherType)
                ->  drl_modal_logic:type_contamination_strength(OtherType, TypeFactor)
                ;   TypeFactor = 0.0
                ),
                Sensitivity is EdgeStrength * AttFactor * TypeFactor * Immunity,
                Contribution is Rate * Sensitivity
            ),
            Contributors),
        sum_contributions(Contributors, Velocity)
    ).

%% sum_contributions(+Contributors, -Total)
sum_contributions([], 0.0).
sum_contributions([contributor(_, _, _, C)|Rest], Total) :-
    sum_contributions(Rest, RestTotal),
    Total is C + RestTotal.

/* ----------------------------------------------------------------
   D. cascade_prediction/3
   Given current effective purity and network drift velocity,
   predicts threshold crossings.
   ---------------------------------------------------------------- */

%% cascade_prediction(+C, +Context, -Crossings)
%  Returns [crossing(ThresholdName, ThresholdValue, TimeYears), ...]
%  Only succeeds if velocity >= network_drift_velocity_threshold.
cascade_prediction(C, Context, Crossings) :-
    constraint_indexing:valid_context(Context),
    drl_modal_logic:effective_purity(C, Context, EP, _),
    EP >= 0.0,
    network_drift_velocity(C, Context, Velocity, _),
    config:param(network_drift_velocity_threshold, VelThresh),
    Velocity >= VelThresh,
    findall(
        crossing(Name, Threshold, TimeYears),
        (   cascade_threshold(Name, Threshold),
            EP > Threshold,
            TimeYears is (EP - Threshold) / Velocity
        ),
        Crossings),
    Crossings \= [].

%% cascade_threshold(?Name, ?Value)
%  Threshold lookup from config parameters.
cascade_threshold(sound_floor, V) :-
    config:param(purity_action_sound_floor, V).
cascade_threshold(escalation_floor, V) :-
    config:param(purity_action_escalation_floor, V).
cascade_threshold(degraded_floor, V) :-
    config:param(purity_action_degraded_floor, V).

/* ----------------------------------------------------------------
   E. network_stability_assessment/2
   Network-wide classification of drift status.
   ---------------------------------------------------------------- */

%% network_stability_assessment(+Context, -Assessment)
%  Assessment ∈ {stable, degrading, cascading}
network_stability_assessment(Context, Assessment) :-
    constraint_indexing:valid_context(Context),
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        detect_network_drift(C, Context, _)
    ), DriftingCs),
    length(DriftingCs, NumDrifting),
    (   NumDrifting =:= 0
    ->  Assessment = stable
    ;   % Count how many have critical or warning severity
        findall(C, (
            member(C, DriftingCs),
            network_drift_severity(C, Context, Sev),
            member(Sev, [critical, warning])
        ), SevereCs),
        length(SevereCs, NumSevere),
        config:param(network_cascade_count_threshold, CascadeThresh),
        (   NumSevere >= CascadeThresh
        ->  Assessment = cascading
        ;   Assessment = degrading
        )
    ).

/* ----------------------------------------------------------------
   F. network_drift_severity/3
   Maps effective purity zone to base severity, with hub and
   multi-source escalation.
   ---------------------------------------------------------------- */

%% network_drift_severity(+C, +Context, -Severity)
%  Severity ∈ {critical, warning, watch}
network_drift_severity(C, Context, Severity) :-
    constraint_indexing:valid_context(Context),
    drl_modal_logic:effective_purity(C, Context, EP, _),
    ep_base_severity(EP, BaseSeverity),
    % Hub escalation: C has >= hub_degree_threshold neighbors AND is drifting
    drl_modal_logic:constraint_neighbors(C, Context, Neighbors),
    length(Neighbors, Degree),
    config:param(network_hub_degree_threshold, HubThresh),
    config:param(network_drift_hub_escalation, HubEnabled),
    (   HubEnabled =:= 1, Degree >= HubThresh,
        detect_network_drift(C, Context, _)
    ->  escalate_severity(BaseSeverity, HubEscalated)
    ;   HubEscalated = BaseSeverity
    ),
    % Multi-source escalation: C has >= 2 drifting neighbors
    (   network_drift_contagion(C, Context, ContagionList),
        length(ContagionList, NumSources),
        NumSources >= 2
    ->  escalate_severity(HubEscalated, Severity)
    ;   Severity = HubEscalated
    ).

%% ep_base_severity(+EP, -Severity)
ep_base_severity(EP, critical) :- EP < 0.30, !.
ep_base_severity(EP, warning) :- EP < 0.70, !.
ep_base_severity(_, watch).

%% escalate_severity(+Base, -Escalated)
escalate_severity(watch, warning).
escalate_severity(warning, critical).
escalate_severity(critical, critical).
