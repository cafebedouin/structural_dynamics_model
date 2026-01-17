:- module(drl_modal_logic, [
    % Stage 1: Composition Rules
    composite_type/3,
    composition_rule/3,
    detect_extraction_dominance/2,
    detect_necessity_inheritance/2,
    
    % Stage 2: Transformation Tracking
    constraint_history/2,
    transformation_detected/5,
    transformation_type/6,
    canonical_transformation/6,  % NEW: Deduplicated transformations
    predict_transformation/3,
    
    % Stage 3: Counterfactual Reasoning
    simulate_cut/2,
    dependency_chain/4,
    infer_structural_coupling/3,
    assess_scaffold_need/2,
    counterfactual_world/3
]).

:- use_module(drl_core).
:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection).

/* ================================================================
   MODAL LOGIC EXTENSION FOR DEFERENTIAL REALISM
   
   FIXES APPLIED:
   - dependency_chain/3 → dependency_chain/4 (added Reason parameter)
   - transformation_type/5 → transformation_type/6 (added Label parameter)
   - Singleton warnings fixed by prefixing unused vars with _
   - All classify_at_time/4 clauses: _C, _E, _X prefix where unused
   - estimate_impact/4: Source NOT prefixed (used multiple times)
   - predict_transformation/3: All variables properly used
   
   NEW FEATURES:
   - Scaffold recognition in classify_at_time/4
   - Enables detection of Scaffold → Noose calcification
   - Scaffold checked BEFORE other type classifications
   
   EFFICIENCY IMPROVEMENTS:
   - constraint_history/2: Uses actual measurement times (not full intervals)
   - transformation_detected/5: Binds T1/T2 from measurements (not iteration)
   - Dramatically faster for large time ranges with sparse measurements
   
   This module implements three stages of formal modal reasoning:
   1. Composition Rules - how constraints interact
   2. Transformation Tracking - temporal evolution with labels
   3. Counterfactual Reasoning - intervention simulation
   ================================================================ */

/* ================================================================
   STAGE 1: COMPOSITION RULES
   Modal logic for how constraints interact and compose
   ================================================================ */

%% composite_type(+C1, +C2, -ResultType)
% Determines the type of a composite constraint formed from C1 and C2
composite_type(C1, C2, Result) :-
    drl_core:dr_type(C1, T1),
    drl_core:dr_type(C2, T2),
    composition_rule(T1, T2, Result).

%% composition_rule(+Type1, +Type2, -CompositeType)
% Formal modal composition rules from DR logic

% Necessity Inheritance: ■C₁ ∧ (C₁ → C₂) ⇒ ■C₂
% If C1 is a Mountain and implies C2, then C2 is also a Mountain
composition_rule(mountain, _, mountain) :- !.
composition_rule(_, mountain, mountain) :- !.

% Extraction Dominance: ⊞C₁ ∧ ⊠C₂ ∧ Embedded(C₂, C₁) ⇒ ⊠(C₁ ∧ C₂)
% When a Noose is embedded in a Rope, the whole becomes extractive
composition_rule(rope, noose, noose) :- !.
composition_rule(noose, rope, noose) :- !.
composition_rule(tangled_rope, noose, noose) :- !.
composition_rule(noose, tangled_rope, noose) :- !.

% Noose Dominance: Multiple Nooses compound
composition_rule(noose, noose, noose) :- !.

% Rope Composition: ⊞C₁ ∧ ⊞C₂ ∧ Compatible(C₁, C₂) ⇒ ⊞(C₁ ∧ C₂)
% Compatible Ropes can be composed into compound Ropes
composition_rule(rope, rope, rope) :- !.

% Tangled interactions
composition_rule(tangled_rope, tangled_rope, tangled_rope) :- !.
composition_rule(rope, tangled_rope, tangled_rope) :- !.
composition_rule(tangled_rope, rope, tangled_rope) :- !.

% Zombie contamination
composition_rule(zombie, _, zombie) :- !.
composition_rule(_, zombie, zombie) :- !.

% Unknown fallback
composition_rule(_, _, unknown).

%% detect_extraction_dominance(+Composite, -Evidence)
% Detects when a Rope is corrupted by an embedded Noose
detect_extraction_dominance(Composite, Evidence) :-
    narrative_ontology:affects_constraint(Composite, Component),
    drl_core:dr_type(Component, noose),
    narrative_ontology:constraint_metric(Component, extractiveness, X),
    X >= 0.66,
    Evidence = embedded_noose(Component, X).

%% detect_necessity_inheritance(+Source, -Derived)
% Detects when a Mountain constraint logically implies another constraint
detect_necessity_inheritance(Source, Derived) :-
    drl_core:dr_type(Source, mountain),
    narrative_ontology:affects_constraint(Source, Derived),
    narrative_ontology:constraint_metric(Source, suppression_requirement, E_source),
    E_source =< 0.05,
    % If the derived constraint should also be a Mountain
    narrative_ontology:constraint_metric(Derived, suppression_requirement, E_derived),
    E_derived =< 0.05.

/* ================================================================
   STAGE 2: TRANSFORMATION TRACKING
   Temporal modal logic for constraint evolution
   ================================================================ */

%% constraint_history(+C, -Timeline)
% Collects the complete history of a constraint's type across all time points
% FIXED: Use actual measurement times instead of iterating through intervals
constraint_history(C, Timeline) :-
    findall(state(T, Type), 
            (narrative_ontology:measurement(_, C, _, T, _),
             dr_type_at(C, T, Type)),
            TimelineUnsorted),
    sort(TimelineUnsorted, Timeline).

%% dr_type_at(+C, +Time, -Type)
% Determines constraint type at a specific time point
dr_type_at(C, Time, Type) :-
    % Get metrics at this time
    (narrative_ontology:measurement(_, C, suppression_requirement, Time, E) -> true ; E = 0.5),
    (narrative_ontology:measurement(_, C, extractiveness, Time, X) -> true ; X = 0.5),
    % Classify based on metrics at this time
    classify_at_time(C, E, X, Type).

%% classify_at_time(+C, +E, +X, -Type)
% FIXED: Added _ prefix to unused variables to suppress singleton warnings
% NEW: Recognize Scaffold type for modal tracking and calcification detection
% 
% SCAFFOLD CLASSIFICATION LOGIC:
% - Scaffolds remain classified as 'scaffold' while X <= tangled_rope threshold
% - This prevents them from being misclassified as 'rope' during normal operation
% - When X exceeds tangled_rope_extraction_ceil, calcification has occurred
% - Only then does it fall through to 'noose' classification
% - This ensures clean scaffold -> noose transformations (no intermediate rope state)
%
% Priority order ensures scaffolds are recognized first when extraction is low
classify_at_time(C, _E, X, scaffold) :-
    narrative_ontology:entity(C, scaffold),
    v3_1_config:param(tangled_rope_extraction_ceil, TangledCeil),
    X =< TangledCeil, !.  % Scaffold until extraction exceeds tangled_rope threshold

classify_at_time(_C, E, _X, mountain) :-
    v3_1_config:param(mountain_suppression_ceiling, Ceil),
    E =< Ceil, !.

classify_at_time(_C, E, X, noose) :-
    v3_1_config:param(noose_extraction_floor, XFloor),
    v3_1_config:param(noose_suppression_floor, EFloor),
    X >= XFloor, E >= EFloor, !.

classify_at_time(_C, _E, X, rope) :-
    v3_1_config:param(rope_extraction_ceiling, XCeil),
    X =< XCeil, !.

classify_at_time(_C, _E, X, tangled_rope) :-
    v3_1_config:param(rope_extraction_ceiling, RopeX),
    v3_1_config:param(tangled_rope_extraction_ceil, TangledX),
    X > RopeX, X =< TangledX, !.

classify_at_time(_, _, _, unknown).

%% transformation_detected(+C, +FromType, +ToType, +T1, +T2)
% Detects when a constraint changes type between two time points
% FIXED: Bind time points from actual measurements instead of iterating through intervals
transformation_detected(C, FromType, ToType, T1, T2) :-
    narrative_ontology:measurement(_, C, _, T1, _), % Bind T1 from actual measurements
    narrative_ontology:measurement(_, C, _, T2, _), % Bind T2 from actual measurements
    T2 > T1,
    dr_type_at(C, T1, FromType),
    dr_type_at(C, T2, ToType),
    FromType \= ToType.

%% transformation_type(+C, +FromType, +ToType, +T1, +T2, -Label)
% Classifies the type of transformation according to DR logic
% FIXED: Added 6th argument for Label to identify transformation mechanism
transformation_type(C, rope, noose, T1, T2, capture) :-
    transformation_detected(C, rope, noose, T1, T2),
    % Check for capture (beneficiary concentration)
    check_capture_between(C, T1, T2).

transformation_type(C, rope, zombie, T1, T2, obsolescence) :-
    transformation_detected(C, rope, zombie, T1, T2),
    % Check for obsolescence (lost function without capture)
    \+ check_capture_between(C, T1, T2).

transformation_type(C, scaffold, noose, T1, T2, calcification) :-
    transformation_detected(C, scaffold, noose, T1, T2),
    % Scaffold calcification: persisted past sunset with beneficiaries
    narrative_ontology:entity(C, scaffold),
    check_capture_between(C, T1, T2).

transformation_type(C, mountain, rope, T1, T2, discovery) :-
    transformation_detected(C, mountain, rope, T1, T2),
    % Discovery: claimed Mountain revealed as Rope
    narrative_ontology:constraint_claim(C, mountain).

transformation_type(C, mountain, noose, T1, T2, discovery) :-
    transformation_detected(C, mountain, noose, T1, T2),
    % Discovery: claimed Mountain revealed as Noose
    narrative_ontology:constraint_claim(C, mountain).

%% canonical_transformation(?C, ?From, ?To, -T1_earliest, -T2_latest, ?Label)
% Returns the canonical (deduplicated) transformation for a constraint
% Picks earliest start time and latest end time for each (C, From, To, Label) tuple
% This eliminates duplicate intermediate transformations
canonical_transformation(C, From, To, T1_earliest, T2_latest, Label) :-
    % Find all transformations of this type
    setof((T1, T2), transformation_type(C, From, To, T1, T2, Label), Pairs),
    % Extract all start times and end times
    findall(T1, member((T1, _), Pairs), T1s),
    findall(T2, member((_, T2), Pairs), T2s),
    % Get the earliest start and latest end
    min_list(T1s, T1_earliest),
    max_list(T2s, T2_latest).

%% check_capture_between(+C, +T1, +T2)
% Helper: detects if beneficiaries became concentrated
check_capture_between(C, T1, T2) :-
    narrative_ontology:measurement(_, C, extractiveness, T1, X1),
    narrative_ontology:measurement(_, C, extractiveness, T2, X2),
    X2 > X1,
    X2 >= 0.66.

%% predict_transformation(+C, +CurrentType, -LikelyFutureType)
% Predicts likely future transformation based on current trajectory
% FIXED: Removed unused variables to suppress singleton warnings
predict_transformation(C, rope, noose) :-
    % If extractiveness is rising and approaching noose threshold
    findall(X, narrative_ontology:measurement(_, C, extractiveness, _, X), Xs),
    length(Xs, N), N >= 2,
    last(Xs, X_latest),
    X_latest > 0.5,
    X_latest < 0.66,
    % Check if trend is upward
    Xs = [X_first|_],
    X_latest > X_first.

predict_transformation(C, rope, zombie) :-
    % If suppression is rising but extractiveness is not
    findall(E, narrative_ontology:measurement(_, C, suppression_requirement, _, E), Es),
    length(Es, N), N >= 2,
    last(Es, E_latest),
    E_latest > 0.3,
    % But extractiveness is low
    narrative_ontology:constraint_metric(C, extractiveness, X),
    X < 0.35.

predict_transformation(C, tangled_rope, noose) :-
    % Tangled ropes under stress often become nooses
    narrative_ontology:constraint_metric(C, extractiveness, X),
    X > 0.5.

/* ================================================================
   STAGE 3: COUNTERFACTUAL REASONING
   Reasoning about possible worlds after interventions
   ================================================================ */

%% simulate_cut(+Noose, -Effects)
% Simulates the effects of cutting a Noose constraint
simulate_cut(Noose, Effects) :-
    drl_core:dr_type(Noose, noose),
    findall(effect(Target, Impact, Reason),
            dependency_chain(Noose, Target, Impact, Reason),
            Effects).

dependency_chain(Source, Target, Impact, Reason) :-
    % 1. Explicit dependencies (declared in data)
    narrative_ontology:affects_constraint(Source, Target),
    estimate_impact(Source, Target, Impact, Reason).

dependency_chain(Source, Target, Impact, Reason) :-
    % 2. Inferred structural coupling (discovered by gradients)
    infer_structural_coupling(Source, Target, Strength),
    Strength > 0.85, % Threshold for high-fidelity coupling
    Impact = catastrophic,
    Reason = inferred_load_bearing_coupling.

infer_structural_coupling(C1, C2, Strength) :-
    C1 \= C2,
    % Ensure both constraints have temporal data
    findall(G1, dr_gradient_at(C1, _, G1), Gs1),
    findall(G2, dr_gradient_at(C2, _, G2), Gs2),
    length(Gs1, L), L > 1, length(Gs2, L),
    % Calculate covariance/correlation proxy
    calculate_coupling_strength(Gs1, Gs2, Strength).

dr_gradient_at(C, T, Grad) :-
    narrative_ontology:measurement(_, C, extractiveness, T, X1),
    % Find next time point
    narrative_ontology:measurement(_, C, extractiveness, T2, X2),
    T2 > T, !,
    Grad is X2 - X1.

estimate_impact(Source, Target, catastrophic, load_bearing) :-
    % Source is a high-extraction Noose
    narrative_ontology:constraint_metric(Source, extractiveness, X),
    v3_1_config:param(noose_load_bearing_threshold, T),
    X > T,
    % Target is a Rope that provides coordination
    drl_core:dr_type(Target, rope), !.

estimate_impact(Source, Target, moderate, disrupts_coordination) :-
    drl_core:dr_type(Source, rope),
    drl_core:dr_type(Target, rope), !.

estimate_impact(Source, Target, beneficial, removes_extraction) :-
    % If Source is extractive and Target is a Rope being corrupted
    drl_core:dr_type(Source, noose),
    drl_core:dr_type(Target, rope),
    !.

estimate_impact(Source, Target, moderate, disrupts_coordination) :-
    % If both are Ropes
    drl_core:dr_type(Source, rope),
    drl_core:dr_type(Target, rope),
    !.

estimate_impact(_, _, negligible, no_dependency) :- !.

%% calculate_coupling_strength(+List1, +List2, -Strength)
% Simple directional correlation check
calculate_coupling_strength([], [], 1.0).
calculate_coupling_strength([H1|T1], [H2|T2], S) :-
    ( (H1 > 0, H2 > 0) ; (H1 < 0, H2 < 0) ; (H1 == 0, H2 == 0) ),
    calculate_coupling_strength(T1, T2, SubS),
    S is 0.2 + SubS. % Bonus for matching direction
calculate_coupling_strength([_|T1], [_|T2], S) :-
    calculate_coupling_strength(T1, T2, S).

%% assess_scaffold_need(+Noose, -Assessment)
% Determines if cutting a Noose requires a Scaffold
% FIXED: Don't count the Noose itself as a scaffold (prevents self-reference)
assess_scaffold_need(Noose, Assessment) :-
    drl_core:dr_type(Noose, noose),
    simulate_cut(Noose, Effects),
    (   member(effect(_, catastrophic, load_bearing), Effects)
    ->  (   narrative_ontology:entity(Scaffold, scaffold),
            Scaffold \= Noose  % CRITICAL: Scaffold must be different from Noose being cut
        ->  Assessment = scaffold_present
        ;   Assessment = scaffold_required
        )
    ;   Assessment = no_scaffold_needed
    ).

%% counterfactual_world(+Intervention, +CurrentWorld, -FutureWorld)
% Models the state after an intervention
counterfactual_world(cut(C), current, after_cut) :-
    drl_core:dr_type(C, Type),
    format('In world after cutting ~w (~w):~n', [C, Type]),
    simulate_cut(C, Effects),
    forall(member(effect(Target, Impact, Reason), Effects),
           format('  - ~w: ~w (~w)~n', [Target, Impact, Reason])).

counterfactual_world(add_scaffold(S, For), current, with_scaffold) :-
    drl_core:dr_type(For, noose),
    format('In world with scaffold ~w for ~w:~n', [S, For]),
    format('  - Temporary support for transition~n'),
    format('  - Allows safe removal of ~w~n', [For]).

/* ================================================================
   UTILITY PREDICATES
   ================================================================ */

%% last(+List, -Last)
% Gets the last element of a list
last([X], X) :- !.
last([_|Xs], Last) :- last(Xs, Last).
