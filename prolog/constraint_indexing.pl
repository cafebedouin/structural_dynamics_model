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
    power_modifier/2,
    scope_modifier/2,

    % Sigmoid directionality (v5.0)
    sigmoid_f/2,
    canonical_d_for_power/2,
    derive_directionality/3,

    % Index predicates
    agent_power/1,
    time_horizon/1,
    exit_options/1,
    spatial_scope/1
]).

:- multifile constraint_classification/3.
:- dynamic constraint_classification/3.

% Directionality override: testsets can declare explicit d values (v5.0)
:- multifile directionality_override/3.
:- dynamic directionality_override/3.

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
% HELPER PREDICATES - EFFECTIVE IMMUTABILITY
% ============================================================================

% Can this be changed given time horizon and exit options?
% Returns: mountain (unchangeable) or rope (changeable)

effective_immutability(immediate, trapped, mountain).
effective_immutability(immediate, constrained, mountain).
effective_immutability(immediate, mobile, rope).
effective_immutability(immediate, arbitrage, rope).

effective_immutability(biographical, trapped, mountain).
effective_immutability(biographical, constrained, mountain).
effective_immutability(biographical, mobile, rope).
effective_immutability(biographical, arbitrage, rope).

effective_immutability(generational, trapped, mountain).
effective_immutability(generational, constrained, rope).
effective_immutability(generational, mobile, rope).
effective_immutability(generational, arbitrage, rope).

effective_immutability(historical, _, rope).

% Civilizational time horizon: analytical perspective can see structural reality
% Both mountain AND rope are valid perceptions from analytical - the metric gates
% determine which fires first (mountain checked before snare/rope in classification order).
effective_immutability(civilizational, analytical, mountain).
effective_immutability(civilizational, analytical, rope).
% Non-analytical exit options still perceive everything as changeable (rope)
effective_immutability(civilizational, trapped, rope).
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
%  Compute the sigmoid power modifier from directionality value D.
sigmoid_f(D, F) :-
    config:param(sigmoid_lower, L),
    config:param(sigmoid_upper, U),
    config:param(sigmoid_midpoint, D0),
    config:param(sigmoid_steepness, K),
    Range is U - L,
    Exponent is -K * (D - D0),
    F is L + Range / (1 + exp(Exponent)).

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

%% beneficiary_victim_directionality(+Constraint, +Context, -D)
%  Derive directionality from constraint structure (beneficiary/victim data).
%  Only fires if the constraint has beneficiary or victim declarations.
beneficiary_victim_directionality(Constraint, Context, D) :-
    Context = context(agent_power(Power), _, exit_options(Exit), _),
    % Check if constraint has structural data
    (   narrative_ontology:constraint_beneficiary(Constraint, _)
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
exit_modulation(trapped,     0.05).
exit_modulation(constrained, 0.02).
exit_modulation(mobile,      0.00).
exit_modulation(arbitrage,  -0.03).
exit_modulation(analytical,  0.00).

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
% Calculate Extractiveness for Specific Agent
% ----------------------------------------------------------------------------

% Formula: χ = ε × f(d) × σ(S)
% where f(d) is the sigmoid directionality function (v5.0)
extractiveness_for_agent(Constraint, Context, Score) :-
    Context = context(agent_power(Power), _, _, spatial_scope(Scope)),
    resolve_coalition_power(Power, Constraint, ResolvedPower),
    % Build resolved context with coalition-adjusted power
    Context = context(_, T, E, S),
    ResolvedContext = context(agent_power(ResolvedPower), T, E, S),
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(Constraint, ExtMetricName, BaseScore),
    derive_directionality(Constraint, ResolvedContext, D),
    sigmoid_f(D, PowerMod),
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
    writeln('  2. Constrained (exit costly)'),
    writeln('  3. Mobile (can leave)'),
    writeln('  4. Arbitrage (can play systems)'),
    writeln('  5. Analytical (observer)'),
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
map_exit(2, constrained).
map_exit(3, mobile).
map_exit(4, arbitrage).
map_exit(5, analytical).

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
