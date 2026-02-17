% ============================================================================
% DRL COMPOSITION — Composition Rules, Transformation Tracking, Audit, Utilities
% Split from drl_modal_logic.pl (v4.0+)
% ============================================================================

:- module(drl_composition, [
    % Stage 1: Composition Rules (INDEXED)
    composite_type/4,
    composite_type/3,
    composition_rule/3,
    detect_extraction_dominance/2,
    detect_necessity_inheritance/2,

    % Stage 2: Transformation Tracking (Context-aware)
    constraint_history/3,
    constraint_history/2,
    transformation_detected/5,
    transformation_type/6,
    canonical_transformation/6,
    predict_transformation/3,

    % Stage 4: Audit
    possibly/1,
    necessarily/1,
    is_snare/1,
    is_mountain/1,
    is_rope/1,

    % Statistical Helpers
    monotonic_increasing/1,
    monotonic_decreasing/1,
    non_monotonic_trajectory/2
]).

:- use_module(drl_audit_core).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(config).

:- dynamic agent_index/2.
:- dynamic constraint_data/2.

% Scaled for the Powerless
is_snare(C) :-
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, powerless, Chi),
    drl_audit_core:structural_signature(Chi, Data, snare).

% Scaled for the Observer
is_mountain(C) :-
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, analytical, Chi),
    drl_audit_core:structural_signature(Chi, Data, mountain).

% Scaled for the Beneficiary
is_rope(C) :-
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, institutional, Chi),
    drl_audit_core:structural_signature(Chi, Data, rope).

/* ================================================================
   STAGE 1: COMPOSITION RULES (INDEXED)
   Modal logic for how constraints interact and compose
   ================================================================ */

%% composite_type(+C1, +C2, +Context, -ResultType)
% PRIMARY API: Determines composite type FROM A SPECIFIC CONTEXT
composite_type(C1, C2, Context, Result) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C1, Context, T1),
    drl_core:dr_type(C2, Context, T2),
    composition_rule(T1, T2, Result).

%% composite_type(+C1, +C2, -ResultType)
% BACKWARD COMPAT: Uses analytical context
composite_type(C1, C2, Result) :-
    constraint_indexing:default_context(Ctx),
    composite_type(C1, C2, Ctx, Result).

% Categorical: Binary operation on type space — NOT a lattice meet (two absorbing elements: mountain, piton)
%% composition_rule(+Type1, +Type2, -CompositeType)
% Formal modal composition rules from DR logic
% NOTE: These rules are NOT indexed - they're about logical structure
%       Context affects input types, not composition rules themselves

% Necessity Inheritance: ■ C₁ ∧ (C₁ → C₂) ⇒ ■ C₂
% If C1 is a Mountain and implies C2, then C2 is also a Mountain
composition_rule(mountain, _, mountain) :- !.
composition_rule(_, mountain, mountain) :- !.

% Extraction Dominance: ⊞C₁ ∧ ⊠ C₂ ∧ Embedded(C₂, C₁) ⇒ ⊠ (C₁ ∧ C₂)
% When a Snare is embedded in a Rope, the whole becomes extractive
composition_rule(rope, snare, snare) :- !.
composition_rule(snare, rope, snare) :- !.
composition_rule(tangled_rope, snare, snare) :- !.
composition_rule(snare, tangled_rope, snare) :- !.

% Snare Dominance: Multiple Snares compound
composition_rule(snare, snare, snare) :- !.

% Rope Composition: ⊞C₁ ∧ ⊞C₂ ∧ Compatible(C₁, C₂) ⇒ ⊞(C₁ ∧ C₂)
% Compatible Ropes can be composed into compound Ropes
composition_rule(rope, rope, rope) :- !.

% Tangled interactions
composition_rule(tangled_rope, tangled_rope, tangled_rope) :- !.
composition_rule(rope, tangled_rope, tangled_rope) :- !.
composition_rule(tangled_rope, rope, tangled_rope) :- !.

% Piton contamination
composition_rule(piton, _, piton) :- !.
composition_rule(_, piton, piton) :- !.

% Unknown fallback
composition_rule(_, _, unknown).

%% detect_extraction_dominance(+Composite, -Evidence)
% Detects when a Rope is corrupted by an embedded Snare
% NOTE: Uses default analytical context for detection
detect_extraction_dominance(Composite, Evidence) :-
    narrative_ontology:affects_constraint(Composite, Component),
    drl_core:dr_type(Component, snare),
    narrative_ontology:constraint_metric(Component, extractiveness, X),
    config:param(snare_epsilon_floor, Floor),
    X >= Floor,
    Evidence = embedded_snare(Component, X).

%% detect_necessity_inheritance(+Source, -Derived)
% Detects when a Mountain constraint logically implies another constraint
% NOTE: Uses default analytical context for detection
detect_necessity_inheritance(Source, Derived) :-
    drl_core:dr_type(Source, mountain),
    narrative_ontology:affects_constraint(Source, Derived),
    narrative_ontology:constraint_metric(Source, suppression_requirement, E_source),
    E_source =< 0.05,
    % If the derived constraint should also be a Mountain
    narrative_ontology:constraint_metric(Derived, suppression_requirement, E_derived),
    E_derived =< 0.05.

/* ================================================================
   STAGE 2: TRANSFORMATION TRACKING (CONTEXT-AWARE)
   Temporal modal logic for constraint evolution
   ================================================================ */

% Categorical: Temporal presheaf — collects presheaf evaluations along the time dimension of the site
%% constraint_history(+C, +Context, -Timeline)
% PRIMARY API: Collects constraint history FROM A SPECIFIC CONTEXT
% Same constraint may have different transformation patterns from different perspectives
constraint_history(C, Context, Timeline) :-
    constraint_indexing:valid_context(Context),
    findall(state(T, Type),
            (narrative_ontology:measurement(_, C, _, T, _),
             dr_type_at(C, T, Context, Type)),
            TimelineUnsorted),
    sort(TimelineUnsorted, Timeline).

%% constraint_history(+C, -Timeline)
% BACKWARD COMPAT: Uses analytical context
constraint_history(C, Timeline) :-
    constraint_indexing:default_context(Ctx),
    constraint_history(C, Ctx, Timeline).

%% dr_type_at(+C, +Time, +Context, -Type)
% Determines constraint type at specific time FROM SPECIFIC CONTEXT
% Delegates to drl_core:classify_from_metrics/6 (Single Source of Truth)
dr_type_at(C, Time, Context, Type) :-
    (narrative_ontology:measurement(_, C, suppression_requirement, Time, Supp) -> true ; Supp = 0.5),
    (narrative_ontology:measurement(_, C, extractiveness, Time, BaseX) -> true ; BaseX = 0.5),
    Context = context(agent_power(Power), _, _, _),
    constraint_indexing:power_modifier(Power, Modifier),
    Chi is BaseX * Modifier,
    drl_core:classify_from_metrics(C, BaseX, Chi, Supp, Context, Type).

%% transformation_detected(+C, +FromType, +ToType, -T1, -T2)
% Detects when constraint transformed from one type to another
% Uses actual measurement times (not iteration)
transformation_detected(C, From, To, T1, T2) :-
    constraint_history(C, Timeline),
    member(state(T1, From), Timeline),
    member(state(T2, To), Timeline),
    T2 > T1,
    From \= To,
    % Ensure no intermediate different type
    \+ (member(state(Tm, Mid), Timeline),
        Tm > T1, Tm < T2,
        Mid \= From, Mid \= To).

%% transformation_type(+C, +From, +To, +T1, +T2, -Label)
% Classifies the type of transformation with semantic label

transformation_type(C, rope, snare, T1, T2, capture) :-
    transformation_detected(C, rope, snare, T1, T2),
    check_capture_between(C, T1, T2).

transformation_type(C, rope, piton, T1, T2, obsolescence) :-
    transformation_detected(C, rope, piton, T1, T2),
    \+ check_capture_between(C, T1, T2).

transformation_type(C, scaffold, snare, T1, T2, calcification) :-
    transformation_detected(C, scaffold, snare, T1, T2),
    narrative_ontology:entity(C, scaffold),
    check_capture_between(C, T1, T2).

transformation_type(C, mountain, rope, T1, T2, discovery) :-
    transformation_detected(C, mountain, rope, T1, T2),
    narrative_ontology:constraint_claim(C, mountain).

transformation_type(C, mountain, snare, T1, T2, discovery) :-
    transformation_detected(C, mountain, snare, T1, T2),
    narrative_ontology:constraint_claim(C, mountain).

%% canonical_transformation(?C, ?From, ?To, -T1_earliest, -T2_latest, ?Label)
% Returns canonical (deduplicated) transformation
canonical_transformation(C, From, To, T1_earliest, T2_latest, Label) :-
    setof((T1, T2), transformation_type(C, From, To, T1, T2, Label), Pairs),
    findall(T1, member((T1, _), Pairs), T1s),
    findall(T2, member((_, T2), Pairs), T2s),
    min_list(T1s, T1_earliest),
    max_list(T2s, T2_latest).

%% check_capture_between(+C, +T1, +T2)
% Helper: detects if beneficiaries became concentrated
check_capture_between(C, T1, T2) :-
    narrative_ontology:measurement(_, C, extractiveness, T1, X1),
    narrative_ontology:measurement(_, C, extractiveness, T2, X2),
    X2 > X1,
    config:param(snare_epsilon_floor, Floor),
    X2 >= Floor.

%% predict_transformation(+C, +CurrentType, -LikelyFutureType)
%  Predicts likely future transformation based on trajectory.
%  Uses least-squares slope on time-sorted measurements (N >= 3)
%  to detect genuine trends, avoiding false positives from noise
%  or V-shaped recoveries that a first-vs-last comparison would miss.
predict_transformation(C, rope, snare) :-
    findall(T-X, narrative_ontology:measurement(_, C, extractiveness, T, X), Pairs),
    length(Pairs, N), N >= 3,
    msort(Pairs, Sorted),
    linear_slope(Sorted, Slope),
    Slope > 0,
    last(Sorted, _-X_latest),
    X_latest > 0.5,
    config:param(snare_epsilon_floor, Floor),
    X_latest < Floor.

predict_transformation(C, rope, piton) :-
    findall(T-E, narrative_ontology:measurement(_, C, suppression_requirement, T, E), Pairs),
    length(Pairs, N), N >= 3,
    msort(Pairs, Sorted),
    linear_slope(Sorted, Slope),
    Slope > 0,
    last(Sorted, _-E_latest),
    E_latest > 0.3,
    narrative_ontology:constraint_metric(C, extractiveness, X),
    X < 0.35.

predict_transformation(C, tangled_rope, snare) :-
    narrative_ontology:constraint_metric(C, extractiveness, X),
    X > 0.5.

/* ----------------------------------------------------------------
   STATISTICAL HELPERS: Slope and Monotonicity
   Used by predict_transformation and drift report.
   ---------------------------------------------------------------- */

%% linear_slope(+TimeSortedPairs, -Slope)
%  Least-squares slope from a list of T-V pairs (sorted by T).
%  Slope = (N*Sum(T*V) - Sum(T)*Sum(V)) / (N*Sum(T^2) - Sum(T)^2)
linear_slope(Pairs, Slope) :-
    length(Pairs, N),
    N >= 2,
    foldl(slope_accum, Pairs, 0.0-0.0-0.0-0.0, SumT-SumV-SumTV-SumT2),
    Denom is N * SumT2 - SumT * SumT,
    (   abs(Denom) > 1.0e-12
    ->  Slope is (N * SumTV - SumT * SumV) / Denom
    ;   Slope = 0.0
    ).

slope_accum(T-V, ST-SV-STV-ST2, NST-NSV-NSTV-NST2) :-
    NST is ST + T,
    NSV is SV + V,
    NSTV is STV + T * V,
    NST2 is ST2 + T * T.

%% monotonic_increasing(+TimeSortedPairs)
%  True if values are non-decreasing when sorted by time.
monotonic_increasing([]) :- !.
monotonic_increasing([_]) :- !.
monotonic_increasing([_-V1, T2-V2 | Rest]) :-
    V2 >= V1,
    monotonic_increasing([T2-V2 | Rest]).

%% monotonic_decreasing(+TimeSortedPairs)
%  True if values are non-increasing when sorted by time.
monotonic_decreasing([]) :- !.
monotonic_decreasing([_]) :- !.
monotonic_decreasing([_-V1, T2-V2 | Rest]) :-
    V2 =< V1,
    monotonic_decreasing([T2-V2 | Rest]).

%% non_monotonic_trajectory(+C, +Metric)
%  True if the measurement series is neither monotonically increasing
%  nor monotonically decreasing. Used by drift report.
non_monotonic_trajectory(C, Metric) :-
    findall(T-V, narrative_ontology:measurement(_, C, Metric, T, V), Pairs),
    length(Pairs, N), N >= 3,
    msort(Pairs, Sorted),
    \+ monotonic_increasing(Sorted),
    \+ monotonic_decreasing(Sorted).

% ============================================================================
% INDEXICAL PERSPECTIVE AUDIT
% ============================================================================
% Checks if two agents experience a structural conflict (Risk Gap)
% Implements Section IV-B
detect_perspectival_risk(ConstraintID, Agent1, Agent2, RiskLabel) :-
    % 1. Get the base extraction score for the constraint
    constraint_data(ConstraintID, Data),
    member(base_extractiveness(X_base), Data),

    % 2. Calculate what each agent "sees" based on their power index
    % Uses the pi scaling function from drl_audit_core
    agent_index(Agent1, context(Power1, _, _, _)),
    agent_index(Agent2, context(Power2, _, _, _)),

    drl_audit_core:effective_extraction(X_base, Power1, Chi1),
    drl_audit_core:effective_extraction(X_base, Power2, Chi2),

    % 3. Determine the structural type for each agent
    drl_audit_core:structural_signature(Chi1, Data, Type1),
    drl_audit_core:structural_signature(Chi2, Data, Type2),

    % 4. Match against the Risk Table
    drl_audit_core:omega_risk(Type1, Type2, RiskLabel, _).

% possibly(C) is true if the constraint is a Rope (changeable/contingent)
possibly(C) :-
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, rope).

% necessarily(C) is true if the constraint is a Mountain (fixed/required)
necessarily(C) :-
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, mountain).

/* ================================================================
   UTILITY PREDICATES
   ================================================================ */

%% last(+List, -Last)
% Gets the last element of a list
last([X], X) :- !.
last([_|Xs], Last) :- last(Xs, Last).

% Provide a fail-safe default so they are "defined" even if no data is loaded
agent_index(_, _) :- fail.
constraint_data(_, _) :- fail.

/* ================================================================
   VERSION & MIGRATION INFO
   ================================================================ */

/*
VERSION HISTORY:
v4.0 (2025-01-17):
  - BREAKING: Full indexical relativity integration
  - NEW: dependency_chain/5 (added Context)
  - NEW: assess_scaffold_need/3 (added Context)
  - NEW: simulate_cut/3 (added Context)
  - NEW: context_depends_critically/3
  - NEW: estimate_impact_indexed/5
  - NEW: check_all_contexts/2 (multi-perspective safety check)
  - CHANGED: All dependency analysis is context-relative
  - MAINTAIN: Backward compatibility via /4, /3, /2 versions

v3.x:
  - Non-indexed modal logic
  - Single "God's eye view" dependency chains

MIGRATION GUIDE v3.x → v4.0:
  Old: dependency_chain(source, target, impact, reason)
  New: dependency_chain(source, target, impact, reason, context)

  Old: assess_scaffold_need(snare, assessment)
  New: assess_scaffold_need(snare, context, assessment)

  CRITICAL NEW REQUIREMENT:
  Before cutting any constraint, MUST evaluate:
    check_all_contexts(constraint, Report)
  to detect if cut is safe from one context but catastrophic from another.

  This prevents "institutional Mountain collapse" - where institution
  cuts what they see as Rope, unknowingly destroying powerless Mountain.

v5.3 (2026-02-14):
  - NEW: Stage 8b — Fixed-Point Network Iterator (FPN)
  - NEW: fpn_run/2, fpn_run/3 (iterative contamination propagation)
  - NEW: fpn_effective_purity/4 (multi-hop effective purity)
  - NEW: states_equivalent/3 (convergence comparison)
  - DESIGN: Jacobi iteration with monotone non-increasing convergence
  - DESIGN: fpn_enabled=0 by default (opt-in activation)
*/
