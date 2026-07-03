% CONSTRAINT CLASSIFICATION
% ============================================================================
% This module implements context-indexed constraint classification.
% Every Mountain/Rope/Snare judgment is relative to WHO, WHEN, WHERE, HOW.
%
% Integration: Load after domain_priors.pl, before report_generator.pl
% ============================================================================

:- module(constraint_indexing, [
    % Core API
    constraint_classification/3,    % New indexed classification
    constraint_claim_indexed/2,     % Backward compatible wrapper (renamed to avoid collision)
    
    % Context builders
    default_context/1,
    valid_context/1,
    discover_my_context/1,
    
    % Analysis utilities
    multi_index_report/1,
    compare_perspectives/2,
    perspective_gap/2,
    
    % Helper predicates (exposed for testing)
    effective_immutability/3,
    effective_immutability_for_context/2,
    extractiveness_for_agent/3,
    extractiveness_for_agent_d/4,   % OQ-83: chi with explicit d (per-stakeholder path)
    agent_resolved_directionality/4, % coalition-resolved d for χ AND reported d/f_d (single source)
    power_modifier/2,
    scope_modifier/2,

    % Sigmoid directionality (v5.0)
    sigmoid_f/2,
    canonical_d_for_power/2,
    derive_directionality/3,
    derive_directionality_at/4,     % Type-A snapshot floor: time-aware d (OQ-83)
    sigmoid_params/5,
    sigmoid_d1/2,
    sigmoid_d2/2,
    constraint_curvature/6,

    % Index predicates
    agent_power/1,
    time_horizon/1,
    exit_options/1,
    spatial_scope/1,

    % Observer accessibility (formal restriction operator)
    observer_accessible/3,
    feature_access/3,
    classify_from_restricted/3,

    % Configurable observer site (v6.2)
    site_contexts/1,            % → site_contexts_canonical/1 by default
    site_contexts_canonical/1,  % the 4 canonical contexts (read-only)
    site_contexts_product/1     % 156-point curated product site
]).

:- multifile constraint_classification/3.
:- dynamic constraint_classification/3.

% Directionality override: testsets can declare explicit d values (v5.0)
:- multifile directionality_override/3.
:- dynamic directionality_override/3.

% Cognitive displacement: per-position δ fact table (v6.1)
% Dynamic so sweeps can retract/assert without touching config params.
:- dynamic positional_displacement/2.
:- dynamic power_role_heuristic/4.
:- dynamic exit_modulation/2.
positional_displacement(powerless,     0.0).
positional_displacement(moderate,      0.0).
positional_displacement(powerful,      0.0).
positional_displacement(organized,     0.0).
positional_displacement(institutional, 0.0).
positional_displacement(analytical,    0.0).

% Required modules
:- use_module(constraint_data, [base_extractiveness/2, suppression_score/2]).
:- use_module(config).
:- use_module(narrative_ontology).

% ============================================================================
% INDEX ONTOLOGY
% ============================================================================

% ----------------------------------------------------------------------------
% Agent Power Levels - WHO is evaluating?
% ----------------------------------------------------------------------------

agent_power(powerless).    % Serf, prisoner, child
agent_power(moderate).     % Middle class, citizen
agent_power(powerful).     % Wealthy, connected
agent_power(organized).    % Union, movement
agent_power(institutional).           % State, corporation, church
agent_power(analytical).              % Historian, philosopher (meta-level)

% ----------------------------------------------------------------------------
% Time Horizons - WHEN/how long?
% ----------------------------------------------------------------------------

time_horizon(immediate).              % 1 year
time_horizon(biographical).           % 20-50 years (lifetime)
time_horizon(generational).          % 50-100 years
time_horizon(historical).            % 100-500 years
time_horizon(civilizational).        % 500+ years

% ----------------------------------------------------------------------------
% Exit Options - WHERE can you go?
% ----------------------------------------------------------------------------

exit_options(trapped).               % No physical/conceptual exit
exit_options(identity_locked).       % Structurally mobile but cognitively/identity-fused
exit_options(constrained).           % Exit costly but possible
exit_options(mobile).                % Can leave, alternatives visible
exit_options(arbitrage).             % Can play systems against each other
exit_options(analytical).            % Not constrained (observer stance)

% ----------------------------------------------------------------------------
% Spatial Scope - WHERE does it operate?
% ----------------------------------------------------------------------------

spatial_scope(local).                % Village, neighborhood
spatial_scope(regional).             % Province, state
spatial_scope(national).             % Country
spatial_scope(continental).          % Europe, Asia, etc
spatial_scope(global).               % Worldwide
spatial_scope(universal).            % Universal

% ============================================================================
% CONTEXT STRUCTURE
% ============================================================================

% ----------------------------------------------------------------------------
% Context Validation
% ----------------------------------------------------------------------------

% context(+AgentPower, +TimeHorizon, +ExitOptions, +SpatialScope)
% Represents the indexical parameters for classification

valid_context(context(
    agent_power(P),
    time_horizon(T),
    exit_options(E),
    spatial_scope(S)
)) :-
    agent_power(P),
    time_horizon(T),
    exit_options(E),
    spatial_scope(S).

% ----------------------------------------------------------------------------
% Default Context - Analytical "God's Eye View"
% ----------------------------------------------------------------------------

default_context(context(
    agent_power(analytical),
    time_horizon(civilizational),
    exit_options(analytical),
    spatial_scope(global)
)).

% ============================================================================
% BACKWARD COMPATIBILITY LAYER
% ============================================================================

% Indexed API wrapper - defaults to analytical perspective
% Renamed to avoid collision with narrative_ontology:constraint_claim/2
constraint_claim_indexed(Constraint, Type) :-
    default_context(Ctx),
    constraint_classification(Constraint, Type, Ctx).

% ============================================================================
% HELPER PREDICATES - EFFECTIVE IMMUTABILITY (Hub 2)
% ============================================================================
% Hub 2 of the two-hub perspectival architecture. This table is an
% independent source of classification variation — it determines mutability
% perception from TIME × EXIT, completely independent of Hub 1 (sigmoid
% power-scaling on the extraction dimension).
%
% A constraint can be classified differently across contexts purely because
% of this table, even if the sigmoid produces identical χ values. This is
% most visible at the mountain gate (requires BOTH low χ AND immutability
% = mountain) and at snare_immutability_check/1 (which crosses stalk
% boundaries by checking this table across all standard contexts).
%
% The discrete (TIME × EXIT) → {mountain, rope} lookup resists continuous
% sigmoid parameterization: temporal perception of changeability is
% fundamentally discontinuous (generational/trapped = mountain, but
% generational/constrained = rope — a single exit_options step flips it).

% Can this be changed given time horizon and exit options?
% Returns: mountain (unchangeable) or rope (changeable)

effective_immutability(immediate, trapped, mountain).
effective_immutability(immediate, identity_locked, mountain).
effective_immutability(immediate, constrained, mountain).
effective_immutability(immediate, mobile, rope).
effective_immutability(immediate, arbitrage, rope).

effective_immutability(biographical, trapped, mountain).
effective_immutability(biographical, identity_locked, rope).    % Perceptual filter, not structural immobility
effective_immutability(biographical, constrained, mountain).
effective_immutability(biographical, mobile, rope).
effective_immutability(biographical, arbitrage, rope).

effective_immutability(generational, trapped, mountain).
effective_immutability(generational, identity_locked, rope).
effective_immutability(generational, constrained, rope).
effective_immutability(generational, mobile, rope).
effective_immutability(generational, arbitrage, rope).

effective_immutability(historical, _, rope).

% Civilizational time horizon: analytical perspective can see structural reality
% Both mountain AND rope are valid perceptions from analytical - the metric gates
% determine which fires first (mountain checked before snare/rope in classification order).
% NOTE: Non-deterministic by design. Callers querying rope (snare gate) succeed via
% backtracking past the mountain clause. Callers using ->/2 see only mountain.
effective_immutability(civilizational, analytical, mountain).
effective_immutability(civilizational, analytical, rope).
% Non-analytical exit options still perceive everything as changeable (rope)
effective_immutability(civilizational, trapped, rope).
effective_immutability(civilizational, identity_locked, rope).
effective_immutability(civilizational, constrained, rope).
effective_immutability(civilizational, mobile, rope).
effective_immutability(civilizational, arbitrage, rope).

% Wrapper that takes full context
effective_immutability_for_context(
    context(_, time_horizon(T), exit_options(E), _),
    Perception
) :-
    effective_immutability(T, E, Perception).

% ============================================================================
% HELPER PREDICATES - AGENT-RELATIVE EXTRACTIVENESS
% ============================================================================

% ----------------------------------------------------------------------------
% Power Modifiers
% ----------------------------------------------------------------------------
% More powerful agents experience less extraction from same constraint
% Negative modifier = net beneficiary
% Determines how much of the base extraction is "felt" by the agent.
% Lower numbers = higher benefit/protection from the constraint.

power_modifier(powerless,     Modifier) :- config:param(power_modifier_powerless, Modifier).
power_modifier(moderate,      Modifier) :- config:param(power_modifier_moderate, Modifier).
power_modifier(powerful,      Modifier) :- config:param(power_modifier_powerful, Modifier).
power_modifier(organized,     Modifier) :- config:param(power_modifier_organized, Modifier).
power_modifier(institutional, Modifier) :- config:param(power_modifier_institutional, Modifier).
power_modifier(analytical,    Modifier) :- config:param(power_modifier_analytical, Modifier).

% ----------------------------------------------------------------------------
% Scope Modifiers (sigma)
% ----------------------------------------------------------------------------
% Larger scope = harder verification = more effective extraction.
% Formula: χ = ε × π(P) × σ(S)

scope_modifier(local,        Mod) :- config:param(scope_modifier_local, Mod).
scope_modifier(regional,     Mod) :- config:param(scope_modifier_regional, Mod).
scope_modifier(national,     Mod) :- config:param(scope_modifier_national, Mod).
scope_modifier(continental,  Mod) :- config:param(scope_modifier_continental, Mod).
scope_modifier(global,       Mod) :- config:param(scope_modifier_global, Mod).
scope_modifier(universal,    Mod) :- config:param(scope_modifier_universal, Mod).

% ============================================================================
% SIGMOID DIRECTIONALITY (v5.0)
% ============================================================================
% Continuous sigmoid function f(d) replacing discrete power_modifier dispatch.
% f(d) = L + (U - L) / (1 + e^(-k*(d - d0)))
%
% Directionality d in [0.0, 1.0]:
%   d ≈ 0.0  → institutional beneficiary (f ≈ -0.20)
%   d ≈ 0.5  → midpoint (f ≈ 0.65)
%   d ≈ 1.0  → powerless target (f ≈ 1.50)

%% sigmoid_f(+D, -F)
%  Compute the power modifier from directionality value D.
%  Dispatches to alt_sigmoid_f/3 based on config param power_function.
sigmoid_f(D, F) :-
    (   config:param(power_function, Variant)
    ->  alt_sigmoid_f(Variant, D, F)
    ;   alt_sigmoid_f(sigmoid, D, F)
    ).

%% alt_sigmoid_f(+Variant, +D, -F)
%  Alternative power transformation functions. All map D ∈ [0,1] to
%  approximately the same range [~-0.20, ~1.50] as the sigmoid baseline.
%  Declared multifile so test harnesses can add new variants without
%  modifying this file.
:- multifile alt_sigmoid_f/3.

% Default: standard logistic sigmoid
alt_sigmoid_f(sigmoid, D, F) :-
    config:param(sigmoid_lower, L),
    config:param(sigmoid_upper, U),
    config:param(sigmoid_midpoint, D0),
    config:param(sigmoid_steepness, K),
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

% Piecewise linear (sign-flip preserved): zero-crossing at d=0.10
% f(0.0)=-0.12, f(0.10)=0.00, f(0.50)=0.70, f(1.0)=1.42
alt_sigmoid_f(piecewise_linear, D, F) :-
    (   D =< 0.10
    ->  F is -0.12 + (D * 1.2)
    ;   D =< 0.50
    ->  F is 0.00 + ((D - 0.10) * 1.75)
    ;   F is 0.70 + ((D - 0.50) * 1.44)
    ).

% Piecewise linear (sign-flip removed — CONTROL): f(0)=+0.05, no negative region
% Null hypothesis: removing sign-flip should destroy presheaf structure
alt_sigmoid_f(piecewise_no_flip, D, F) :-
    F is 0.05 + (D * 1.37).

% Square root / concave (sign-flip preserved): rapid initial rise
% f(0.0)=-0.12, f(0.25)=0.65, f(0.50)=0.97, f(1.0)=1.42
alt_sigmoid_f(sqrt_flip, D, F) :-
    F is -0.12 + 1.54 * sqrt(D).

% Quadratic / convex (sign-flip preserved): slow initial rise
% f(0.0)=-0.12, f(0.25)=0.08, f(0.50)=0.27, f(1.0)=1.42
alt_sigmoid_f(quadratic_flip, D, F) :-
    F is -0.12 + 1.54 * D * D.

% Step function (extreme nonlinearity, sign-flip preserved): three discrete levels
% f<0.15=-0.12, f∈[0.15,0.85)=0.70, f≥0.85=1.42
alt_sigmoid_f(step_flip, D, F) :-
    (   D < 0.15
    ->  F is -0.12
    ;   D < 0.85
    ->  F is 0.70
    ;   F is 1.42
    ).

% Sigmoid shifted (d0=0.25): sign-flip at institutional is removed
% f(0.0)≈+0.22 (positive — no institutional sign-flip), f(0.25)≈0.65, f(1.0)≈1.49
alt_sigmoid_f(sigmoid_shifted, D, F) :-
    L is -0.20, U is 1.50, D0 is 0.25, K is 6.0,
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

%% sigmoid_params(+Variant, -L, -U, -D0, -K) is det.
%  Extract sigmoid parameters for smooth variants.
%  sigmoid_shifted uses hardcoded values matching its alt_sigmoid_f clause.
sigmoid_params(sigmoid, L, U, D0, K) :-
    config:param(sigmoid_lower, L),
    config:param(sigmoid_upper, U),
    config:param(sigmoid_midpoint, D0),
    config:param(sigmoid_steepness, K).
sigmoid_params(sigmoid_shifted, -0.20, 1.50, 0.25, 6.0).

%% sigmoid_d1(+D, -F1) is det.
%  First derivative f'(d) of the power-scaling sigmoid.
%  F1 = K*(U-L)*g*(1-g) where g = 1/(1+exp(-K*(d-D0))).
%  Fails cleanly for non-smooth variants (piecewise, step, sqrt, quadratic).
%  Cut after config lookup commits to the first (active) power_function value.
sigmoid_d1(D, F1) :-
    config:param(power_function, Variant), !,
    memberchk(Variant, [sigmoid, sigmoid_shifted]),
    sigmoid_params(Variant, L, U, D0, K),
    Exponent is -K * (D - D0),
    G is 1.0 / (1.0 + exp(Exponent)),
    F1 is K * (U - L) * G * (1.0 - G).

%% sigmoid_d2(+D, -F2) is det.
%  Second derivative f''(d) of the power-scaling sigmoid.
%  F2 = K²*(U-L)*g*(1-g)*(1-2*g). Zero at inflection point d=D0 (sigmoid_midpoint).
%  Antisymmetric around D0. Fails cleanly for non-smooth variants.
%  Cut after config lookup commits to the first (active) power_function value.
sigmoid_d2(D, F2) :-
    config:param(power_function, Variant), !,
    memberchk(Variant, [sigmoid, sigmoid_shifted]),
    sigmoid_params(Variant, L, U, D0, K),
    Exponent is -K * (D - D0),
    G is 1.0 / (1.0 + exp(Exponent)),
    F2 is K * K * (U - L) * G * (1.0 - G) * (1.0 - 2.0 * G).

%% constraint_curvature(+C, +Context, -D, -F, -F1, -F2) is det.
%  Compute directionality D, power modifier F=f(d), f'(d)=F1, f''(d)=F2
%  for constraint C at observer Context. Fails if power_function is non-smooth.
constraint_curvature(C, Context, D, F, F1, F2) :-
    derive_directionality(C, Context, D),
    sigmoid_f(D, F),
    sigmoid_d1(D, F1),
    sigmoid_d2(D, F2).

%% canonical_d_for_power(+PowerAtom, -D)
%  Map power atom to its canonical directionality value.
canonical_d_for_power(powerless,     D) :- config:param(canonical_d_powerless, D).
canonical_d_for_power(moderate,      D) :- config:param(canonical_d_moderate, D).
canonical_d_for_power(powerful,      D) :- config:param(canonical_d_powerful, D).
canonical_d_for_power(organized,     D) :- config:param(canonical_d_organized, D).
canonical_d_for_power(institutional, D) :- config:param(canonical_d_institutional, D).
canonical_d_for_power(analytical,    D) :- config:param(canonical_d_analytical, D).

%% derive_directionality(+Constraint, +Context, -D)
%  Full derivation chain for directionality:
%    1. Explicit override (directionality_override/3)
%    2. Structure-based derivation (beneficiary/victim + exit_options)
%    3. Canonical fallback (power atom -> canonical d)
derive_directionality(Constraint, Context, D) :-
    Context = context(agent_power(Power), _, _, _),
    (   directionality_override(Constraint, Power, D)
    ->  true
    ;   beneficiary_victim_directionality(Constraint, Context, D)
    ->  true
    ;   canonical_d_for_power(Power, D)
    ).

%% derive_directionality_at(+Constraint, +Context, +Time, -D)
%  Time-aware directionality (Type-A snapshot floor, OQ-83 / 2026-06-08).
%  Precedence: a future time-indexed-d source at the effective time, else the
%  static derive_directionality/3 — fail-CLOSE to real data, never fabricate d.
%
%  On the current corpus time_indexed_directionality_source/4 has NO facts, so
%  this is byte-identical to derive_directionality/3 at every Time (the
%  no-regression / fail-close witness, V2). effective_time/3 is the C2
%  (frame_policy) insertion point and MUST stay deterministic — a choice point
%  here would break the backtracking-identity the static pipeline relies on.
:- dynamic time_indexed_directionality_source/4.  % (C,Context,Time,D); empty — future C1 hook

derive_directionality_at(Constraint, Context, Time, D) :-
    effective_time(Constraint, Time, EffTime),
    (   time_indexed_directionality_source(Constraint, Context, EffTime, D0)
    ->  D = D0
    ;   derive_directionality(Constraint, Context, D)
    ).

%% effective_time(+Constraint, +Time, -EffTime) is det.
%  Default (no frame_policy authored): EffTime = Time (Living-shaped, and
%  identical to today because there is no time-indexed source to evaluate).
%  When C2 lands, an Originalist policy rebinds EffTime to interval start (t0);
%  THIS clause is the insertion point — keep C2 an insert, not a rewrite of
%  derive_directionality_at. Must remain deterministic (single solution).
effective_time(_Constraint, Time, Time).

%% beneficiary_victim_directionality(+Constraint, +Context, -D)
%  Derive directionality from constraint structure (beneficiary/victim data).
%  Only fires if the constraint has beneficiary or victim declarations.
beneficiary_victim_directionality(Constraint, Context, D) :-
    Context = context(agent_power(Power), _, exit_options(Exit), _),
    % Check if constraint has structural data.
    % OQ-63 ruled (operator, 2026-06-05): d-derivation consumes the AGENCY-FILTERED
    % view (agent_beneficiary = constraint_beneficiary minus the non_agent registry;
    % post-OQ-64 vindicated propositions never enter constraint_beneficiary at all).
    % A vindicated proposition or detector-bait entry must not feed d -> chi as if
    % it were an actor collecting from the constraint — that corruption is SILENT
    % (plausible d, plausible chi, plausible type, nothing fires). This aligns the
    % metric path with the signature path (FSM + NL gate), which already consume
    % agent_beneficiary. Cutover witnessed zero-diff on the live corpus (all-agent
    % beneficiaries); the guard exists for the first non-agent entry that arrives.
    (   narrative_ontology:agent_beneficiary(Constraint, _)
    ->  HasBeneficiaries = true
    ;   HasBeneficiaries = false
    ),
    (   narrative_ontology:constraint_victim(Constraint, _)
    ->  HasVictims = true
    ;   HasVictims = false
    ),
    % At least one must exist for structural derivation
    (HasBeneficiaries = true ; HasVictims = true),
    power_role_heuristic(Power, HasBeneficiaries, HasVictims, BaseD),
    exit_modulation(Exit, ExitMod),
    D0 is BaseD + ExitMod,
    clamp(D0, 0.0, 1.0, D).

%% power_role_heuristic(+Power, +HasBeneficiaries, +HasVictims, -BaseD)
%  Map power level + structural role to a base directionality.
%  Agents with beneficiary status at high power get low d (they benefit).
%  Agents with victim status at low power get high d (they suffer).
power_role_heuristic(powerless,     _, true,  0.85).
power_role_heuristic(powerless,     _, false, 0.90).
power_role_heuristic(moderate,      _, true,  0.70).
power_role_heuristic(moderate,      _, false, 0.65).
power_role_heuristic(powerful,      _, true,  0.50).
power_role_heuristic(powerful,      _, false, 0.46).
power_role_heuristic(organized,     _, true,  0.45).
power_role_heuristic(organized,     _, false, 0.40).
power_role_heuristic(institutional, true, _,  0.15).
power_role_heuristic(institutional, false, _, 0.10).
power_role_heuristic(analytical,    _, _,     0.72).

%% exit_modulation(+ExitOption, -Adjustment)
%  Adjust directionality based on exit options.
%  Trapped agents have higher effective directionality (more affected).
%  Agents with arbitrage have lower (can escape).
exit_modulation(trapped,         0.05).
exit_modulation(identity_locked, 0.04).
exit_modulation(constrained,     0.02).
exit_modulation(mobile,          0.00).
exit_modulation(arbitrage,      -0.03).
exit_modulation(analytical,      0.00).

%% clamp(+Value, +Min, +Max, -Clamped)
%  Clamp Value to [Min, Max].
clamp(V, Min, _, Min) :- V < Min, !.
clamp(V, _, Max, Max) :- V > Max, !.
clamp(V, _, _, V).

% ----------------------------------------------------------------------------
% Dynamic Coalition Modeling (The "Who" Extension)
% ----------------------------------------------------------------------------

%% resolve_coalition_power(+Power, +Constraint, -ResolvedPower)
%  Dynamically upgrades 'powerless' to 'organized' if a
%  critical mass of victims for a given snare-like constraint is reached.
resolve_coalition_power(powerless, Constraint, organized) :-
    % To avoid circular dependencies, we check for snare-like properties
    % (high base extraction, high suppression) instead of the final type.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    (narrative_ontology:constraint_metric(Constraint, ExtMetricName, BaseX) ->
        config:param(snare_epsilon_floor, XFloor),
        BaseX >= XFloor
    ; false),
    (narrative_ontology:constraint_metric(Constraint, SuppMetricName, S) ->
        config:param(snare_suppression_floor, SFloor),
        S >= SFloor
    ; false),
    % Check for critical mass of victims
    findall(_, narrative_ontology:constraint_victim(Constraint, _), Victims),
    length(Victims, Count),
    config:param(critical_mass_threshold, Threshold),
    Count >= Threshold,
    !.
resolve_coalition_power(Power, _, Power). % Default: power remains unchanged


% ----------------------------------------------------------------------------
% Cognitive Displacement Resolution
% ----------------------------------------------------------------------------

%% resolve_displacement(+PowerAtom, -Delta)
%  Returns cognitive displacement δ for the given power position.
%  In uniform mode: reads global param (sweepable by config_sensitivity_sweep).
%  In positional mode: reads per-position dynamic fact (sweepable by δ-sweep).
resolve_displacement(Power, Delta) :-
    config:param(cognitive_displacement_profile, Profile),
    (   Profile = positional
    ->  positional_displacement(Power, Delta)
    ;   config:param(cognitive_displacement, Delta)
    ).

% ----------------------------------------------------------------------------
% Calculate Extractiveness for Specific Agent
% ----------------------------------------------------------------------------

% Formula: χ = ε × f(d_eff) × σ(S)
% where d_eff = clamp(d + δ, 0, 1) and f is the sigmoid (v6.1)
% Refactored 2026-06-07 (OQ-83 step 3): the d-parameterized body is the single
% canonical χ computation (extractiveness_for_agent_d/4); this predicate derives
% d via the atom-keyed chain and delegates. Behavior-preserving — witnessed
% byte-identical on the A1 capture harness (canonical-4 + product-156), see
% audits/2026-06-07_stakeholder_layer_migration/.
extractiveness_for_agent(Constraint, Context, Score) :-
    agent_resolved_directionality(Constraint, Context, ResolvedContext, D),
    extractiveness_for_agent_d(Constraint, ResolvedContext, D, Score).

%% agent_resolved_directionality(+Constraint, +Context, -ResolvedContext, -D)
%  Coalition-resolve the context's power atom, then derive directionality on the
%  RESOLVED context. This is the single source for the d that χ uses, so any
%  consumer that reports d / f(d) alongside χ (json_report:write_one_perspective_chi)
%  must derive them HERE, not from the unresolved canonical atom — otherwise χ
%  (resolved d) and the reported f(d) (unresolved d) fork. Witnessed fork:
%  powerless→organized coalition gave χ from d=0.5 but reported f_d from d=0.9
%  (model_collapse_feedback, 2026-06-30). Scope/temporal/env are preserved by
%  resolution; only the power atom (hence displacement and d) changes.
agent_resolved_directionality(Constraint, Context, ResolvedContext, D) :-
    Context = context(agent_power(Power), T, E, S),
    resolve_coalition_power(Power, Constraint, ResolvedPower),
    ResolvedContext = context(agent_power(ResolvedPower), T, E, S),
    derive_directionality(Constraint, ResolvedContext, D).

%% extractiveness_for_agent_d(+Constraint, +Context, +D, -Score)
%  χ with EXPLICIT directionality (OQ-83: per-stakeholder d enters here).
%  Context supplies the power atom (displacement) and scope (σ); D replaces
%  the atom-keyed derivation. Does NOT re-run resolve_coalition_power — the
%  caller owns d and any power resolution.
extractiveness_for_agent_d(Constraint, Context, D, Score) :-
    Context = context(agent_power(Power), _, _, spatial_scope(Scope)),
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(Constraint, ExtMetricName, BaseScore),
    resolve_displacement(Power, Delta),
    D_eff is max(0.0, min(1.0, D + Delta)),
    sigmoid_f(D_eff, PowerMod),
    scope_modifier(Scope, ScopeMod),
    Score is BaseScore * PowerMod * ScopeMod.

% ============================================================================
% CANONICAL CLASSIFICATION PREDICATE
% ============================================================================

% constraint_classification(+ConstraintID, ?Type, +Context)
% This is the ONLY predicate that does real classification work.
% All other predicates delegate to this.
%
% Specific constraint rules should be added in separate files or
% in domain_priors.pl using this predicate.

% Example template (actual rules in domain_priors.pl or constraint_instances.pl):
% constraint_classification(some_constraint, Type, Context) :-
%     valid_context(Context),
%     ... classification logic using Context parameters ...

% ============================================================================
% QUERY UTILITIES
% ============================================================================
% ----------------------------------------------------------------------------
% Interactive Context Discovery
% ----------------------------------------------------------------------------

discover_my_context(Context) :-
    writeln(''),
    writeln('=== CONTEXT DISCOVERY ==='),
    writeln(''),
    writeln('What is your power level?'),
    writeln('  1. Powerless (prisoner, serf, child)'),
    writeln('  2. Moderate (middle class, citizen)'),
    writeln('  3. Powerful (wealthy, politically connected)'),
    writeln('  4. Collective (union, movement)'),
    writeln('  5. Institutional (state, corporation)'),
    writeln('  6. Analytical (observer, researcher)'),
    read(PowerChoice),
    map_power(PowerChoice, Power),
    
    writeln(''),
    writeln('What time horizon are you considering?'),
    writeln('  1. Immediate (1 year)'),
    writeln('  2. Biographical (my lifetime)'),
    writeln('  3. Generational (my children)'),
    writeln('  4. Historical (centuries)'),
    writeln('  5. Civilizational (millennia)'),
    read(TimeChoice),
    map_time(TimeChoice, Time),
    
    writeln(''),
    writeln('What exit options do you have?'),
    writeln('  1. Trapped (no exit visible)'),
    writeln('  2. Identity-locked (structurally mobile, cognitively fused)'),
    writeln('  3. Constrained (exit costly)'),
    writeln('  4. Mobile (can leave)'),
    writeln('  5. Arbitrage (can play systems)'),
    writeln('  6. Analytical (observer)'),
    read(ExitChoice),
    map_exit(ExitChoice, Exit),
    
    writeln(''),
    writeln('What spatial scope?'),
    writeln('  1. Local (village/neighborhood)'),
    writeln('  2. Regional (state/province)'),
    writeln('  3. National (country)'),
    writeln('  4. Continental (Europe, Asia, etc)'),
    writeln('  5. Global (worldwide)'),
    read(ScopeChoice),
    map_scope(ScopeChoice, Scope),
    
    Context = context(
        agent_power(Power),
        time_horizon(Time),
        exit_options(Exit),
        spatial_scope(Scope)
    ),
    writeln(''),
    format('Your context: ~w~n', [Context]).

% Mapping predicates for user input
map_power(1, powerless).
map_power(2, moderate).
map_power(3, powerful).
map_power(4, organized).
map_power(5, institutional).
map_power(6, analytical).

map_time(1, immediate).
map_time(2, biographical).
map_time(3, generational).
map_time(4, historical).
map_time(5, civilizational).

map_exit(1, trapped).
map_exit(2, identity_locked).
map_exit(3, constrained).
map_exit(4, mobile).
map_exit(5, arbitrage).
map_exit(6, analytical).

map_scope(1, local).
map_scope(2, regional).
map_scope(3, national).
map_scope(4, continental).
map_scope(5, global).

% ----------------------------------------------------------------------------
% Multi-Index Analysis
% ----------------------------------------------------------------------------

% Show how constraint classifies from different perspectives
multi_index_report(Constraint) :-
    writeln(''),
    writeln('=== MULTI-INDEX ANALYSIS ==='),
    format('Constraint: ~w~n~n', [Constraint]),
    
    findall(
        result(Type, Power, Time, Exit, Scope),
        constraint_classification(Constraint, Type, 
            context(agent_power(Power), time_horizon(Time),
                   exit_options(Exit), spatial_scope(Scope))),
        Results
    ),
    
    (Results = [] ->
        writeln('No classifications found.')
    ;
        format_multi_index_results(Results)
    ).

format_multi_index_results([]).
format_multi_index_results([result(Type, Power, Time, Exit, Scope)|Rest]) :-
    format('~w: power=~w, time=~w, exit=~w, scope=~w~n',
           [Type, Power, Time, Exit, Scope]),
    format_multi_index_results(Rest).

% ----------------------------------------------------------------------------
% Perspective Comparison
% ----------------------------------------------------------------------------

% Compare YOUR view vs ANALYTICAL view
compare_perspectives(Constraint, MyContext) :-
    writeln(''),
    writeln('=== PERSPECTIVE COMPARISON ==='),
    format('Constraint: ~w~n~n', [Constraint]),
    
    default_context(AnalyticalContext),
    
    (constraint_classification(Constraint, MyType, MyContext) ->
        format('From YOUR perspective (~w): ~w~n', [MyContext, MyType])
    ;
        writeln('No classification from your perspective')
    ),
    
    writeln(''),
    
    (constraint_classification(Constraint, AnalyticalType, AnalyticalContext) ->
        format('From ANALYTICAL perspective: ~w~n', [AnalyticalType])
    ;
        writeln('No analytical classification')
    ),
    
    writeln(''),
    
    (var(MyType) ; var(AnalyticalType) ->
        true
    ; MyType = AnalyticalType ->
        writeln('→ Perspectives AGREE')
    ;
        writeln('→ Perspectives DISAGREE - this is a site of political conflict')
    ).

% ----------------------------------------------------------------------------
% Perspective Gap Detection
% ----------------------------------------------------------------------------

% Find political flashpoints - where different perspectives classify differently
perspective_gap(Constraint, Gap) :-
    constraint_classification(Constraint, Type1, Context1),
    constraint_classification(Constraint, Type2, Context2),
    Type1 \= Type2,
    Context1 \= Context2,
    Gap = gap(Type1-Context1, Type2-Context2).

% ============================================================================
% OBSERVER ACCESSIBILITY — FORMAL RESTRICTION OPERATOR
% ============================================================================
% The DR equivalent of a partial trace: projects the full constraint record
% to what is structurally accessible from a given observer position.
%
% A powerless observer experiences extraction (χ) but cannot separate it
% into components (ε, σ, theater). They cannot identify beneficiaries,
% see alternative systems, or distinguish mountains from snares. This is
% the epistemic trap that makes snares look like "just how things are."
%
% The accessibility table formalizes what gauge_fixed/3 detects post-hoc:
% an observer in a gauge-fixed frame sees a restricted view that may lead
% to systematically different conclusions than the full-data classification.
%
% Testable prediction: the set of constraints where classify_from_restricted
% differs from dr_type/3 should match the set with gauge_fixed = true.
% ============================================================================

% ----------------------------------------------------------------------------
% Feature Accessibility Table
% ----------------------------------------------------------------------------
% Access levels:
%   full      — true value is observable and measurable
%   partial   — value is observable but with limited precision
%   felt_only — effect is experienced but cannot be quantified or separated
%   none      — feature is not accessible from this position

%% feature_access(+PowerLevel, +Feature, -Access)

% Powerless: experiences extraction as undifferentiated constraint
feature_access(powerless,     raw_extraction, none).
feature_access(powerless,     suppression,    felt_only).
feature_access(powerless,     beneficiaries,  none).
feature_access(powerless,     alternatives,   none).
feature_access(powerless,     theater_ratio,  none).
feature_access(powerless,     cross_context,  none).

% Moderate: can partially see structural features but not the full picture
feature_access(moderate,      raw_extraction, partial).
feature_access(moderate,      suppression,    partial).
feature_access(moderate,      beneficiaries,  partial).
feature_access(moderate,      alternatives,   partial).
feature_access(moderate,      theater_ratio,  partial).
feature_access(moderate,      cross_context,  none).

% Powerful: better visibility, especially of alternatives
feature_access(powerful,      raw_extraction, partial).
feature_access(powerful,      suppression,    partial).
feature_access(powerful,      beneficiaries,  partial).
feature_access(powerful,      alternatives,   full).
feature_access(powerful,      theater_ratio,  partial).
feature_access(powerful,      cross_context,  none).

% Organized: collective action reveals suppression and beneficiary structure
feature_access(organized,     raw_extraction, partial).
feature_access(organized,     suppression,    full).
feature_access(organized,     beneficiaries,  full).
feature_access(organized,     alternatives,   full).
feature_access(organized,     theater_ratio,  partial).
feature_access(organized,     cross_context,  none).

% Institutional: full structural visibility (except meta-level)
feature_access(institutional, raw_extraction, full).
feature_access(institutional, suppression,    full).
feature_access(institutional, beneficiaries,  full).
feature_access(institutional, alternatives,   full).
feature_access(institutional, theater_ratio,  full).
feature_access(institutional, cross_context,  none).

% Analytical: full visibility including cross-context (meta-level)
feature_access(analytical,    raw_extraction, full).
feature_access(analytical,    suppression,    full).
feature_access(analytical,    beneficiaries,  full).
feature_access(analytical,    alternatives,   full).
feature_access(analytical,    theater_ratio,  full).
feature_access(analytical,    cross_context,  full).

% ----------------------------------------------------------------------------
% Restriction Operator
% ----------------------------------------------------------------------------

%% observer_accessible(+Constraint, +Context, -RestrictedView)
%  Projects the full constraint record to what is accessible from Context.
%  RestrictedView = view(Chi, VisibleEps, VisibleSupp, VisibleTheater,
%                        KnownBeneficiaries, PerceivedMutability)
%
%  Chi is always accessible — it is what the observer experiences.
%  Other features are restricted per the feature_access/3 table.
observer_accessible(C, Context, RestrictedView) :-
    valid_context(Context),
    Context = context(agent_power(Power), _, _, _),
    % Chi is always accessible — experienced extraction
    (extractiveness_for_agent(C, Context, Chi0) -> Chi = Chi0 ; Chi = 0.0),
    % Restrict raw extraction
    restrict_continuous(Power, raw_extraction, C, extractiveness, Chi, VisibleEps),
    % Restrict suppression
    restrict_continuous(Power, suppression, C, suppression_raw, Chi, VisibleSupp),
    % Restrict theater ratio
    restrict_continuous(Power, theater_ratio, C, theater, 0.0, VisibleTheater),
    % Restrict beneficiary knowledge
    restrict_beneficiaries(Power, C, KnownBeneficiaries),
    % Perceived mutability (always accessible via direct experience).
    % Uses ->/2 deliberately: returns first perception (mountain before rope).
    % Dual-perception cases (civilizational/analytical) are handled by drl_core's backtracking.
    (effective_immutability_for_context(Context, Mut) -> PerceivedMutability = Mut ; PerceivedMutability = unknown),
    RestrictedView = view(Chi, VisibleEps, VisibleSupp, VisibleTheater,
                          KnownBeneficiaries, PerceivedMutability).

%% restrict_continuous(+Power, +Feature, +C, +MetricKey, +ChiFallback, -Value)
%  Applies access restriction to a continuous metric.
%  full → true value; partial → true value (imprecise); felt_only → Chi proxy; none → unknown.
restrict_continuous(Power, Feature, C, MetricKey, ChiFallback, Value) :-
    feature_access(Power, Feature, Access),
    restrict_by_access(Access, C, MetricKey, ChiFallback, Value).

restrict_by_access(full, C, MetricKey, _, Value) :-
    get_true_metric(C, MetricKey, Value), !.
restrict_by_access(partial, C, MetricKey, _, Value) :-
    get_true_metric(C, MetricKey, Value), !.
restrict_by_access(felt_only, _, _, ChiFallback, ChiFallback) :- !.
restrict_by_access(none, _, _, _, unknown).

%% get_true_metric(+C, +MetricKey, -Value)
%  OQ-205 (spec §3): absence of an authored ε reads `unknown`, never a
%  fabricated 0.0 (a mountain-shaped ε that passes every floor).
%  Consumers resolve `unknown` explicitly (resolve_for_classification/3),
%  same as the none-access path.
get_true_metric(C, extractiveness, Val) :-
    (constraint_data:base_extractiveness(C, Val) -> true ; Val = unknown).
get_true_metric(C, suppression_raw, Val) :-
    config:param(suppression_metric_name, MetricName),
    (narrative_ontology:constraint_metric(C, MetricName, Val) -> true ; Val = 0.0).
get_true_metric(C, theater, Val) :-
    config:param(theater_metric_name, TheaterName),
    (narrative_ontology:constraint_metric(C, TheaterName, Val) -> true ; Val = 0.0).

%% restrict_beneficiaries(+Power, +C, -Known)
restrict_beneficiaries(Power, C, Known) :-
    feature_access(Power, beneficiaries, Access),
    restrict_beneficiaries_by_access(Access, C, Known).

restrict_beneficiaries_by_access(full, C, Beneficiaries) :-
    findall(B, narrative_ontology:constraint_beneficiary(C, B), Beneficiaries), !.
restrict_beneficiaries_by_access(partial, C, Partial) :-
    findall(B, narrative_ontology:constraint_beneficiary(C, B), All),
    length(All, N),
    Visible is max(1, N // 2),
    length(Partial, Visible),
    append(Partial, _, All), !.
restrict_beneficiaries_by_access(_, _, []).

% ----------------------------------------------------------------------------
% Classification from Restricted Data
% ----------------------------------------------------------------------------

%% classify_from_restricted(+Constraint, +Context, -RestrictedType)
%  Attempts classification using only observer-accessible features.
%  Where features are inaccessible, uses conservative defaults that
%  conflate what the observer cannot distinguish.
%
%  The gap between RestrictedType and dr_type/3 measures the epistemic
%  cost of the observer's position. When they differ, the observer is
%  in a gauge-fixed frame (or near one).
classify_from_restricted(C, Context, RestrictedType) :-
    observer_accessible(C, Context, view(Chi, VisEps, VisSupp, VisTheater,
                                          _KnownBen, PerceivedMut)),
    resolve_for_classification(VisEps, Chi, Eps),
    resolve_for_classification(VisSupp, Chi, Supp),
    resolve_for_classification(VisTheater, 0.0, Theater),
    restricted_classify(C, Eps, Chi, Supp, Theater, PerceivedMut, RestrictedType).

%% resolve_for_classification(+MaybeValue, +Fallback, -Value)
%  Converts unknown/felt_only values to numeric for classification.
resolve_for_classification(unknown, Fallback, Fallback).
resolve_for_classification(Value, _, Value) :- number(Value).

%% restricted_classify(+C, +Eps, +Chi, +Supp, +Theater, +Mutability, -Type)
%  Simplified cascade using only the resolved restricted metrics.
restricted_classify(_C, Eps, _Chi, Supp, _Theater, mountain, mountain) :-
    Supp =< 0.05, Eps =< 0.25, !.
restricted_classify(_C, Eps, Chi, Supp, _Theater, _Mut, snare) :-
    Chi >= 0.66, Eps >= 0.46, Supp >= 0.60, !.
restricted_classify(_C, Eps, Chi, _Supp, _Theater, _Mut, rope) :-
    Chi =< 0.35, Eps =< 0.45, !.
restricted_classify(_C, Eps, Chi, _Supp, Theater, _Mut, piton) :-
    number(Theater), Theater >= 0.70,
    Chi =< 0.25, Eps > 0.10, !.
restricted_classify(_C, _Eps, _Chi, _Supp, _Theater, _Mut, indeterminate).

% ============================================================================
% SITE CONTEXTS — configurable observer site for measurement predicates (v6.2)
% ============================================================================
% DESIGN NOTE: Two distinct context predicates serve two distinct roles.
%
%   standard_context/1 (in drl_core.pl) — CLASSIFICATION reference frame.
%     Used by snare_immutability_check/1, dr_mismatch/4, cross_context_analysis/2.
%     Asks structural questions: "could someone with actual power change this?"
%     FIXED at 4 canonical contexts. Must not expand with the cohomology site
%     or the mountain/snare distinction collapses (historical/* → rope always
%     passes, making everything trivially snare-eligible).
%
%   site_contexts/1 (this predicate) — MEASUREMENT site.
%     Used by cohomological_obstruction/3, gauge_orbit/2, wasserstein_contexts/1,
%     arakelov_height_pair/3. Measures disagreement across observer positions.
%     CONFIGURABLE. Default: 4 canonical (identical outputs to pre-v6.2).
%     Product site: 156-point curated expansion.

%% site_contexts(-Contexts) is det.
%% Returns the observer site for measurement predicates.
%% Controlled by config:param(site_mode, Mode).
%% Classification predicates in drl_core.pl use standard_context/1
%% which is FIXED at 4 canonical contexts regardless of site_mode.
site_contexts(Contexts) :-
    config:param(site_mode, Mode),
    site_contexts_for_mode(Mode, Contexts).

site_contexts_for_mode(canonical, Contexts) :-
    site_contexts_canonical(Contexts).
site_contexts_for_mode(product, Contexts) :-
    site_contexts_product(Contexts).

% --- OQ-131 six-observer probe modes (additive; default canonical unperturbed) ---
%  These clauses are first-argument indexed on the mode atom and carry NO
%  catch-all, so the canonical/product branches above resolve byte-for-byte as
%  before (the default-invariance claim is then witnessed by a control run, not
%  assumed). Each six-point mode appends the new seats AFTER the canonical four,
%  so the 6 canonical observer-pairs are positionally identical to the 4-seat
%  mode and the entire 4->6 H1 delta lives in the 9 new pairs (= C(6,2)-C(4,2)).
%  The seat bundles are declared-revisable config params (config.pl), so a
%  per-seat sensitivity sweep overlays them without touching this clause.

%% site_contexts_for_mode(canonical_6, -Contexts)
%  The 4 canonical contexts, THEN the powerful and organized observer seats
%  built from observer_bundle_powerful / observer_bundle_organized = bundle(T,E,S).
site_contexts_for_mode(canonical_6, Contexts) :-
    site_contexts_canonical(Canon4),
    config:param(observer_bundle_powerful, bundle(TP, EP, SP)),
    config:param(observer_bundle_organized, bundle(TO, EO, SO)),
    append(Canon4,
        [ context(agent_power(powerful),  time_horizon(TP), exit_options(EP), spatial_scope(SP)),
          context(agent_power(organized), time_horizon(TO), exit_options(EO), spatial_scope(SO)) ],
        Contexts).

%% site_contexts_for_mode(power_only_4, -Contexts)
%% site_contexts_for_mode(power_only_6, -Contexts)
%  Single-coordinate control: every seat shares one fixed observer_baseline_tes
%  = bundle(T,E,S), so the only thing that widens 4->6 is the power vocabulary
%  (powerful/organized appended LAST, keeping the 4-seat sub-vector positional).
%  Isolates pure power-vocabulary widening from bundle-coordinate choice.
site_contexts_for_mode(power_only_4, Contexts) :-
    config:param(observer_baseline_tes, bundle(T, E, S)),
    findall(context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S)),
            member(P, [powerless, moderate, institutional, analytical]),
            Contexts).
site_contexts_for_mode(power_only_6, Contexts) :-
    config:param(observer_baseline_tes, bundle(T, E, S)),
    findall(context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S)),
            member(P, [powerless, moderate, institutional, analytical, powerful, organized]),
            Contexts).

%% site_contexts_canonical(-Contexts) is det.
%  The 4 canonical observer contexts. This predicate never changes.
site_contexts_canonical([
    context(agent_power(powerless), time_horizon(biographical),
            exit_options(trapped), spatial_scope(local)),
    context(agent_power(moderate), time_horizon(biographical),
            exit_options(mobile), spatial_scope(national)),
    context(agent_power(institutional), time_horizon(generational),
            exit_options(arbitrage), spatial_scope(national)),
    context(agent_power(analytical), time_horizon(civilizational),
            exit_options(analytical), spatial_scope(global))
]).

%% site_contexts_product(-Contexts) is det.
%  Curated 156-point product site: 4P × 3T × 5E × 3S minus 24 category-error
%  combinations where exit_options(analytical) is paired with non-civilizational
%  time horizons (the analytical stance is definitionally civilizational-scale).
%
%  Excluded power atoms: powerful, organized
%    (non-canonical; no canonical_d calibration; expand to 6P in Phase 3 if
%    power-axis decomposition shows P drives most obstruction)
%  Excluded time horizons: immediate, historical
%    (immediate: no effective_immutability entries for analytical_exit;
%     historical: wildcard → always rope, trivially passes snare gate)
%  Excluded exit options: identity_locked
%    (fine-grained mid-exit; thin corpus coverage)
%  Excluded scope values: regional, continental, universal
%    (non-canonical; scope_modifier params are less calibrated)
site_contexts_product(Contexts) :-
    findall(
        context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S)),
        (   member(P, [powerless, moderate, institutional, analytical]),
            member(T, [biographical, generational, civilizational]),
            member(E, [trapped, constrained, mobile, arbitrage, analytical]),
            member(S, [local, national, global]),
            \+ (E = analytical, T \= civilizational)
        ),
        Contexts
    ).

% ============================================================================
% INTEGRATION NOTES
% ============================================================================

% To integrate this module:
%
% 1. In drl_core.pl, add after domain_priors:
%    :- use_module(constraint_indexing).
%
% 2. In domain_priors.pl, add base_extractiveness/2 facts:
%    base_extractiveness(constraint_id, score).
%    % Score range: 0.0 (no extraction) to 1.0 (full extraction)
%
% 3. Add specific constraint rules using constraint_classification/3
%    See constraint_instances.pl for examples
%
% 4. Update report_generator.pl to optionally use indexed classification:
%    - Default: Use constraint_claim/2 (analytical view)
%    - Advanced: Accept context parameter from user
%
% 5. Test backward compatibility:
%    - Old code using constraint_claim/2 should work unchanged
%    - New code can use constraint_classification/3 explicitly
