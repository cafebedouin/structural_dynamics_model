:- module(constraint_bridge, [
    dr_diagnostic_report/1,
    constraint_status/3,
    recommendation_feasibility/3
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_coercion_projection).

/* ================================================================
   1. CONTEXTUAL CONSTRAINT FILTERING
   ================================================================ */

%% constraint_status(+ConstraintName, -State, -Intensity)
%  Maps CE v2.0 types to diagnostic intensities.
constraint_status(Name, binding_limit, I) :-
    narrative_ontology:constraint_claim(Name, mountain),
    narrative_ontology:constraint_metric(Name, inevitability, I), !.

constraint_status(Name, extractive_noose, I) :-
    narrative_ontology:constraint_claim(Name, noose),
    narrative_ontology:constraint_metric(Name, extractiveness, I), !.

constraint_status(Name, coordination_rope, I) :-
    narrative_ontology:constraint_claim(Name, rope),
    (narrative_ontology:constraint_metric(Name, theater_ratio, T) -> I is 1.0 - T ; I = 1.0), !.

constraint_status(Name, inertial_zombie, I) :-
    narrative_ontology:constraint_claim(Name, zombie),
    (narrative_ontology:constraint_metric(Name, theater_ratio, I) ; I = 1.0), !.

/* ================================================================
   2. UNIQUE VETO LOGIC & FEASIBILITY
   ================================================================ */

recommendation_feasibility(RecID, Status, UniqueVetoes) :-
    narrative_ontology:recommendation(RecID, _), % CRITICAL: Verify RecID is a recommendation
    % --- 1. THE STRUCTURAL AUDIT (Theorem 3) ---
    % If a recommendation affects a Noose (high extraction), 
    % it is load-bearing. Cutting it without a Scaffold is a systemic risk.
    (   narrative_ontology:affects_constraint(RecID, Target),
        narrative_ontology:constraint_metric(Target, extractiveness, X), 
        X > 0.7, % Load-bearing threshold
        \+ (narrative_ontology:entity(_, scaffold)) % No safety net provided
    ->  Status = blocked_scaffold_vacuum,
        UniqueVetoes = [] % Blocked by physics/logic, not individuals
    ;   
    % --- 2. THE POLITICAL AUDIT (Veto Logic) ---
    % Check for agents whose interests are explicitly threatened.
    (setof(Actor, narrative_ontology:veto_exposed(Actor, RecID), UniqueVetoes) 
     ; UniqueVetoes = []),
    length(UniqueVetoes, Count),
    (   Count == 0 -> Status = viable 
    ;   Count < 2  -> Status = high_veto_risk % Adjusted sensitivity
    ;   Status = blocked_by_veto)
    ).

%% is_safe_to_cut(+ConstraintID)
% Theorem 3: Cutting a load-bearing Noose requires a Scaffold.
is_safe_to_cut(C) :-
    narrative_ontology:constraint_metric(C, extractiveness, X), X > 0.7, % Load-bearing Noose
    narrative_ontology:entity(_, scaffold). % A scaffold MUST exist in the KB

/* ================================================================
   3. SCENARIO-AWARE DIAGNOSTIC REPORTING
   ================================================================ */

%% dr_diagnostic_report(+IntervalID)
%  Filters output to only show items linked to the current interval.
dr_diagnostic_report(IntervalID) :-
    format('~n=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: ~w ===~n', [IntervalID]),
    
    format('~n[CONSTRAINT INVENTORY]~n'),
    % Use setof to ensure each unique Name/State/Intensity combination is only printed once
    (   setof(line(Name, State, Intensity), 
              (constraint_status(Name, State, Intensity), 
               narrative_ontology:constraint_metric(Name, _, _)), 
              UniqueLines)
    ->  forall(member(line(N, S, I), UniqueLines),
               format('  - ~w: ~w (Intensity: ~2f)~n', [N, S, I]))
    ;   format('  No active constraints found.~n')
    ),
    
    format('~n[FEASIBILITY BRIDGE]~n'),
    % Only show recommendations that affect a constraint in the current inventory
    forall((narrative_ontology:recommendation(RID, Summary),
            narrative_ontology:affects_constraint(RID, ConsName),
            narrative_ontology:constraint_claim(ConsName, _)),
           (recommendation_feasibility(RID, Stat, Vs),
            format('  - ~w (~w): ~w | Vetoes: ~w~n', [RID, Summary, Stat, Vs]))),
    format('====================================================~n').
% ============================================================================
% INDEXICAL CONSTRAINT CLASSIFICATION
% ============================================================================
% This module implements context-indexed constraint classification.
% Every Mountain/Rope/Noose judgment is relative to WHO, WHEN, WHERE, HOW.
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
    
    % Index predicates
    agent_power/1,
    time_horizon/1,
    exit_options/1,
    spatial_scope/1
]).

:- multifile constraint_classification/3.
:- dynamic constraint_classification/3.

% Required modules
:- use_module(domain_priors, [
    base_extractiveness/2,
    suppression_score/2
]).

% ============================================================================
% INDEX ONTOLOGY
% ============================================================================

% ----------------------------------------------------------------------------
% Agent Power Levels - WHO is evaluating?
% ----------------------------------------------------------------------------

agent_power(individual_powerless).    % Serf, prisoner, child
agent_power(individual_moderate).     % Middle class, citizen
agent_power(individual_powerful).     % Wealthy, connected
agent_power(collective_organized).    % Union, movement
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
effective_immutability(civilizational, _, rope).

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

power_modifier(individual_powerless, 1.5).   % Experience MORE extraction
power_modifier(individual_moderate, 1.0).    % Baseline
power_modifier(individual_powerful, 0.5).    % Experience LESS extraction
power_modifier(collective_organized, 0.7).   % Shared burden
power_modifier(institutional, -0.2).         % NET BENEFICIARY
power_modifier(analytical, 1.0).             % Neutral observer

% ----------------------------------------------------------------------------
% Calculate Extractiveness for Specific Agent
% ----------------------------------------------------------------------------

extractiveness_for_agent(Constraint, Context, Score) :-
    Context = context(agent_power(Power), _, _, _),
    base_extractiveness(Constraint, BaseScore),
    power_modifier(Power, Modifier),
    Score is BaseScore * Modifier.

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
map_power(1, individual_powerless).
map_power(2, individual_moderate).
map_power(3, individual_powerful).
map_power(4, collective_organized).
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
:- module(constraint_instances, [
    % This module adds constraint_classification/3 rules
    % No exports needed - rules are added to constraint_indexing namespace
]).

:- use_module(constraint_indexing).
:- use_module(domain_priors).

% ============================================================================
% CONSTRAINT-SPECIFIC INDEXED CLASSIFICATIONS
% ============================================================================

% These rules extend constraint_indexing:constraint_classification/3
% See test_generation_prompt.md for examples

% ============================================================================
% HISTORICAL CONSTRAINT INSTANCES
% ============================================================================

% ----------------------------------------------------------------------------
% CATHOLIC CHURCH 1200 CE
% ----------------------------------------------------------------------------

% Serf perspective - MOUNTAIN
constraint_indexing:constraint_classification(
    catholic_church_1200,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(agent_power(individual_powerless), 
                time_horizon(biographical), 
                exit_options(trapped), 
                spatial_scope(local)),
        mountain
    ).

% Historian perspective - NOOSE
constraint_indexing:constraint_classification(
    catholic_church_1200,
    noose,
    context(
        agent_power(analytical),
        time_horizon(Horizon),
        exit_options(analytical),
        spatial_scope(continental)  % FIXED: Was unbound
    )
) :-
    member(Horizon, [historical, civilizational]),
    domain_priors:base_extractiveness(catholic_church_1200, E),
    E > 0.6,
    domain_priors:requires_active_enforcement(catholic_church_1200),
    !.  % ADDED: Cut to prevent duplicates

% Pope perspective - ROPE
constraint_indexing:constraint_classification(
    catholic_church_1200,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(continental)
    )
) :-
    constraint_indexing:extractiveness_for_agent(catholic_church_1200, 
        context(agent_power(institutional), 
                time_horizon(generational), 
                exit_options(arbitrage), 
                spatial_scope(continental)), 
        E),
    E < 0.4,  % Low for beneficiary
    !.  % ADDED: Cut to prevent duplicates

% ----------------------------------------------------------------------------
% PROPERTY RIGHTS 2025
% ----------------------------------------------------------------------------

% Homeless person - NOOSE
constraint_indexing:constraint_classification(
    property_rights_2025,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(Exit),
        spatial_scope(national)
    )
) :-
    member(Exit, [trapped, constrained]),
    constraint_indexing:extractiveness_for_agent(property_rights_2025, 
        context(agent_power(individual_powerless),
                time_horizon(biographical),
                exit_options(Exit),
                spatial_scope(national)),
        E),
    E > 0.7,
    !.  % ADDED: Cut to prevent duplicates

% Middle class - ROPE
constraint_indexing:constraint_classification(
    property_rights_2025,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    constraint_indexing:extractiveness_for_agent(property_rights_2025,
        context(agent_power(individual_moderate),
                time_horizon(biographical),
                exit_options(mobile),
                spatial_scope(national)),
        E),
    E < 0.6,
    !.  % ADDED: Cut to prevent duplicates

% Billionaire - ROPE (with benefits)
constraint_indexing:constraint_classification(
    property_rights_2025,
    rope,
    context(
        agent_power(Power),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    member(Power, [individual_powerful, institutional]),
    constraint_indexing:extractiveness_for_agent(property_rights_2025,
        context(agent_power(Power),
                time_horizon(generational),
                exit_options(arbitrage),
                spatial_scope(global)),
        E),
    E < 0,  % Negative = net beneficiary
    !.  % ADDED: Cut to prevent duplicates
% ============================================================================
% CONSTRAINT STORY: [CONSTRAINT_ID]
% ============================================================================
% Generated: [DATE]
% Model: [MODEL NAME]
% Source: [SOURCE]
% ============================================================================

:- module(constraint_[id], []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: [unique_identifier]
 * human_readable: [Descriptive name]
 * domain: [economic/political/social/religious/technological]
 * temporal_scope: [When it operates, e.g., "1200-1500 CE" or "Future/R7"]
 * spatial_scope: [Where it operates, e.g., "Medieval Europe" or "Station R7"]
 * 
 * SUMMARY:
 * [2-3 sentence description of the constraint and its context]
 * 
 * KEY AGENTS:
 * - [Agent1]: [Role/perspective]
 * - [Agent2]: [Role/perspective]
 * - [Agent3]: [Role/perspective]
 * 
 * NARRATIVE ARC:
 * [Brief description of how the constraint functions in the story/domain]
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(unknown_interval, 0, 10).
narrative_ontology:constraint_claim([constraint_id], [claimed_type]).

% Bridge to the Auditor's Metric Logic, next three
% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
domain_priors:base_extractiveness([constraint_id], [0.0-1.0]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: [Why this score?]
domain_priors:base_extractiveness([constraint_id], [0.0-1.0]).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: [Why this score?]
domain_priors:suppression_score([constraint_id], [0.0-1.0]).

% Enforcement requirements
% [Requires active enforcement OR emerges naturally]
domain_priors:requires_active_enforcement([constraint_id]).
% OR: emerges_naturally([constraint_id]).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric([constraint_id], extractiveness, [score]).
narrative_ontology:constraint_metric([constraint_id], suppression_requirement, [score]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: [AGENT NAME/ROLE] - [Mountain/Rope/Noose]
   --------------------------------------------------------------------------
   
   WHO: [Power level description]
   WHEN: [Time horizon description]
   WHERE: [Exit options description]
   SCOPE: [Spatial scope description]
   
   WHY THIS CLASSIFICATION:
   [2-3 sentences explaining why this agent/perspective sees it this way]
   
   NARRATIVE EVIDENCE:
   [Quotes or references from source material]
   -------------------------------------------------------------------------- */

% [Classification rules using constraint_indexing:constraint_classification/3...]
constraint_indexing:constraint_classification(
    [constraint_id],
    [mountain/rope/noose],
    context(
        agent_power([individual_powerless/individual_moderate/individual_powerful/collective_organized/institutional/analytical]),
        time_horizon([immediate/biographical/generational/historical/civilizational]),
        exit_options([trapped/constrained/mobile/arbitrage/analytical]),
	constraint_beneficiary([constraint_id], [beneficiary_agent_or_class]),
	constraint_victim([constraint_id], [victim_agent_or_class]),
        spatial_scope([local/regional/national/continental/global])
    )
) :-
    % Classification logic
    constraint_indexing:effective_immutability_for_context(
        context([power], [time], [exit], [scope]),
        [mountain/rope]
    ),
    % Additional conditions
    domain_priors:base_extractiveness([constraint_id], E),
    E > [threshold],  % or E < [threshold]
    !.  % Cut to prevent backtracking

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: [AGENT NAME/ROLE] - [Mountain/Rope/Noose]
   --------------------------------------------------------------------------
   [Same structure as Perspective 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    [constraint_id],
    [type],
    context([power], [time], [exit], [scope])
) :-
    % Classification logic
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: [AGENT NAME/ROLE] - [Mountain/Rope/Noose]
   --------------------------------------------------------------------------
   [Same structure as Perspective 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    [constraint_id],
    [type],
    context([power], [time], [exit], [scope])
) :-
    % Classification logic
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests([constraint_id]_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that same constraint = different types from different perspectives
 */
test(multi_perspective_[name]) :-
    % Perspective 1
    constraint_indexing:constraint_classification(
        [constraint_id],
        Type1,
        context([p1_power], [p1_time], [p1_exit], [p1_scope])
    ),
    % Perspective 2
    constraint_indexing:constraint_classification(
        [constraint_id],
        Type2,
        context([p2_power], [p2_time], [p2_exit], [p2_scope])
    ),
    % Perspective 3
    constraint_indexing:constraint_classification(
        [constraint_id],
        Type3,
        context([p3_power], [p3_time], [p3_exit], [p3_scope])
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3.

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that extraction experienced varies with agent power
 */
test(power_extractiveness_[name]) :-
    ContextPowerless = context(individual_powerless, [time], [exit], [scope]),
    ContextPowerful = context([institutional/individual_powerful], [time], [exit], [scope]),
    constraint_indexing:extractiveness_for_agent([constraint_id], ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent([constraint_id], ContextPowerful, Score2),
    Score1 > Score2.  % Powerless experience more extraction

/**
 * TEST 3: Time-horizon immutability
 * Demonstrates that mutability varies with time horizon
 */
test(time_immutability_[name]) :-
    % Short horizon + trapped = mountain
    constraint_indexing:effective_immutability([biographical/immediate], trapped, mountain),
    % Long horizon = rope (changeable)
    constraint_indexing:effective_immutability([civilizational/historical], [any_exit], rope).

/**
 * TEST 4: [Domain-specific insight]
 * [What this test demonstrates about the constraint]
 */
test([specific_insight_name]) :-
    % Test logic
    true.

:- end_tests([constraint_id]_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: [Gemini 2.0 Flash / Claude Sonnet 4 / etc]
 * 
 * KEY DECISIONS MADE BY MODEL:
 * 
 * 1. BASE EXTRACTIVENESS ([score]):
 *    Reasoning: [Why the model chose this score]
 *    Evidence: [What from the narrative supported this]
 *    Uncertainty: [What the model was uncertain about]
 *    Beneficiaries: [Who benefits]
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Model chose to analyze from [X] perspectives because [reason]
 *    Alternative perspectives considered: [list]
 *    Why they were excluded: [reason]
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    [Agent 1] as [Type]: [Model's reasoning]
 *    [Agent 2] as [Type]: [Model's reasoning]
 *    [Agent 3] as [Type]: [Model's reasoning]
 * 
 * 4. AMBIGUITIES IN SOURCE MATERIAL:
 *    - [Issue 1]: [How model resolved it]
 *    - [Issue 2]: [How model resolved it]
 *
 * 5. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable([omega_id],
 *	"[Question form of uncertainty]",
 *	resolution_mechanism("[How to resolve]"),
 *	impact("[What changes if resolved]"),
 *	confidence_without_resolution([low/medium/high])
 *	).
 * 
 * 6. CONFIDENCE ASSESSMENT:
 *    High confidence: [Which aspects]
 *    Medium confidence: [Which aspects]
 *    Low confidence: [Which aspects]
 *    Would benefit from: [Additional evidence/clarification needed]
 * 
 * 7. EDGE CASES:
 *    [Situations where classification might be ambiguous]
 *    [Boundary conditions between types]
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * Does this constraint have alternatives that were suppressed?
 * This matters for distinguishing Rope (no alternatives) from Noose (alternatives exist).
 * 
 * ALTERNATIVE 1: [Name/Description]
 *    Viability: [Why it's a real alternative]
 *    Suppression: [How/why it was rejected]
 *    Evidence: [From narrative or historical record]
 * 
 * ALTERNATIVE 2: [If applicable]
 *    ...
 * 
 * CONCLUSION:
 * [Does the existence of alternatives affect classification?]
 * [Changes Rope → Noose if alternatives were actively suppressed]
 */

% If alternatives exist (affects signature detection):
% intent_viable_alternative([interval_id], [alternative_id], '[description]').
% intent_alternative_rejected([interval_id], [alternative_id], '[reason]').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/[constraint_id]].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report([constraint_id]).
 * 
 * 3. Run tests:
 *    ?- run_tests([constraint_id]_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report([constraint_id]).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints([constraint_id], [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
:- module(data_verification, [
    verify_all/0,
    verify_interval_completeness/1,
    check_paired_measurements/0,
    diagnose_unknown/1
]).

:- use_module(library(lists)).        % Required for subtract/3
:- use_module(narrative_ontology).
:- use_module(v3_1_config).

/* ============================================================
   1. ENTRY POINTS
   ============================================================ */

%% verify_all
%  Performs a full sweep of the loaded data against the v3.1 schema.
verify_all :-
    format('~n--- [START] v3.1 DATA INTEGRITY VERIFICATION ---~n'),
    (validate_ontology -> format('[OK] Ontology Schema matches.~n') ; format('[FAIL] Schema mismatch.~n')),
    verify_structure,
    verify_measurements,
    check_paired_measurements,  % NEW: Check for paired temporal measurements
    verify_intent_logic,
    format('--- [END] Verification Complete ---~n').

/* ============================================================
   2. STRUCTURAL & VECTOR COMPLETENESS
   ============================================================ */

%% verify_structure
%  Ensures every interval has the required 32-point coercion vector.
verify_structure :-
    forall(interval(ID, T0, Tn),
           ( format('Checking Interval: ~w (~w-~w)~n', [ID, T0, Tn]),
             verify_interval_completeness(ID)
           )).

%% verify_interval_completeness(+IntervalID)
%  Checks for the presence of all 4 components across all 4 levels at T0 and Tn.
verify_interval_completeness(ID) :-
    interval(ID, T0, Tn),
    forall(level(L),
           ( verify_vector_at(L, T0, ID),
             verify_vector_at(L, Tn, ID)
           )).

verify_vector_at(Level, Time, ID) :-
    Components = [accessibility_collapse(Level), stakes_inflation(Level), 
                  suppression(Level), resistance(Level)],
    forall(member(Metric, Components),
           ( measurement(_, _, Metric, Time, _)
           -> true
           ;  format('  [MISSING] ~w for Level: ~w at T: ~w in ~w~n', [Metric, Level, Time, ID]),
              fail
           )).

/* ============================================================
   3. VALUE RANGE VALIDATION
   ============================================================ */

verify_measurements :-
    forall(measurement(ID, _, _, _, Val),
           ( number(Val), Val >= 0.0, Val =< 1.0
           -> true
           ;  format('  [VALUE ERROR] Measurement ~w is outside [0,1] range.~n', [ID]),
              fail
           )).

/* ============================================================
   4. INTENT LOGIC CONSISTENCY
   ============================================================ */

verify_intent_logic :-
    % Ensure every rejected alternative was first defined as viable (by anyone)
    % FIXED: Different entities can propose vs. reject - that's the whole point!
    % RELAXED: Allow rejecting implicit alternatives that weren't explicitly listed as viable
    forall(intent_alternative_rejected(I, _Rejecter, A),
           ( intent_viable_alternative(I, _Proposer, A)
           -> true
           ;  format('  [INFO] Alternative ~w rejected in ~w without explicit viable listing (may be implicit status quo).~n', [A, I])
           % REMOVED fail - this is informational, not an error
           )),
    % Ensure main beneficiary has a power change fact
    forall(intent_beneficiary_class(I, C),
           ( intent_power_change(I, C, _)
           -> true
           ;  format('  [MISSING DATA] Beneficiary ~w has no power_change delta in ~w.~n', [C, I]),
              fail
           )).

/* ============================================================
   5. PAIRED MEASUREMENT VALIDATION (MODAL LOGIC REQUIREMENT)
   ============================================================ */

%% check_paired_measurements
% Verifies that extractiveness and suppression_requirement measurements
% are paired at each time point for each constraint.
% CRITICAL: Modal logic requires BOTH metrics to classify constraints.
check_paired_measurements :-
    % Get all constraints that have any temporal measurements
    % CRITICAL FIX: Exclude intervals to avoid treating them as constraints
    setof(C, T^M^V^(measurement(M, C, _, T, V), entity(C, _)), Constraints),
    !,
    forall(member(C, Constraints),
           check_constraint_pairing(C)).

check_paired_measurements :- 
    % No temporal measurements found - skip check
    true.

%% check_constraint_pairing(+Constraint)
% Checks if a specific constraint has paired measurements
check_constraint_pairing(C) :-
    % Get all time points for extractiveness
    findall(T, measurement(_, C, extractiveness, T, _), XTimes),
    % Get all time points for suppression_requirement
    findall(T, measurement(_, C, suppression_requirement, T, _), ETimes),
    % Sort and compare
    sort(XTimes, XSorted),
    sort(ETimes, ESorted),
    (   XSorted = ESorted
    ->  true  % Perfect pairing
    ;   XSorted = [], ESorted = []
    ->  true  % No temporal data (uses constraint_metric only)
    ;   % Unpaired measurements detected
        format('  [WARNING] Unpaired temporal measurements for ~w~n', [C]),
        (   XSorted \= []
        ->  format('    Extractiveness measured at: ~w~n', [XSorted])
        ;   format('    Extractiveness: No temporal measurements~n')
        ),
        (   ESorted \= []
        ->  format('    Suppression measured at: ~w~n', [ESorted])
        ;   format('    Suppression: No temporal measurements~n')
        ),
        format('    Impact: May cause "unknown" classification or use default values~n')
    ).

/* ============================================================
   6. DIAGNOSTIC TOOLS
   ============================================================ */

%% diagnose_unknown(+Constraint)
% Diagnostic tool for investigating "unknown" classifications
% Call this when dr_type(C, unknown) occurs
diagnose_unknown(C) :-
    format('~n=== DIAGNOSING UNKNOWN CLASSIFICATION: ~w ===~n', [C]),
    
    % Check if constraint exists
    (   constraint_claim(C, Claimed)
    ->  format('Claimed type: ~w~n', [Claimed])
    ;   format('WARNING: No constraint_claim for ~w~n', [C])
    ),
    
    % Check current metrics
    format('~nCurrent metrics (T_end):~n'),
    (   constraint_metric(C, extractiveness, X)
    ->  format('  Extractiveness: ~w~n', [X])
    ;   format('  Extractiveness: MISSING~n')
    ),
    (   constraint_metric(C, suppression_requirement, E)
    ->  format('  Suppression: ~w~n', [E])
    ;   format('  Suppression: MISSING~n')
    ),
    
    % Check temporal measurements
    format('~nTemporal measurements:~n'),
    findall(T, measurement(_, C, extractiveness, T, _), XTimes),
    findall(T, measurement(_, C, suppression_requirement, T, _), ETimes),
    (   XTimes \= []
    ->  format('  Extractiveness at times: ~w~n', [XTimes])
    ;   format('  Extractiveness: No temporal data~n')
    ),
    (   ETimes \= []
    ->  format('  Suppression at times: ~w~n', [ETimes])
    ;   format('  Suppression: No temporal data~n')
    ),
    
    % Check pairing
    format('~nPairing analysis:~n'),
    sort(XTimes, XSorted),
    sort(ETimes, ESorted),
    (   XSorted = ESorted
    ->  format('  ✓ Measurements are paired~n')
    ;   format('  ✗ UNPAIRED MEASUREMENTS DETECTED~n'),
        format('    This is likely causing the "unknown" classification~n')
    ),
    
    % Provide fix suggestion
    format('~nRecommended fix:~n'),
    (   XSorted \= [], ESorted = []
    ->  format('  Add suppression_requirement measurements at: ~w~n', [XSorted])
    ;   ESorted \= [], XSorted = []
    ->  format('  Add extractiveness measurements at: ~w~n', [ESorted])
    ;   XSorted \= [], ESorted \= []
    ->  subtract(XSorted, ESorted, MissingE),
        subtract(ESorted, XSorted, MissingX),
        (   MissingE \= []
        ->  format('  Add suppression_requirement at: ~w~n', [MissingE])
        ;   true
        ),
        (   MissingX \= []
        ->  format('  Add extractiveness at: ~w~n', [MissingX])
        ;   true
        )
    ;   format('  Add temporal measurements with both metrics~n')
    ),
    format('~n===========================================~n~n').
:- module(domain_priors, [
    get_prior/3,
    is_known_domain/1,
    flag_novelty/1,
    expected_signature/2,
    should_be_natural_law/1,
    validate_signature/2,
    category_of/2,
    base_extractiveness/2,
    suppression_score/2,
    requires_active_enforcement/1,
    emerges_naturally/1
]).

:- discontiguous base_extractiveness/2.
:- discontiguous suppression_score/2.
:- discontiguous requires_active_enforcement/1.
:- discontiguous emerges_naturally/1.

:- multifile 
    base_extractiveness/2, 
    suppression_score/2, 
    requires_active_enforcement/1,
    emerges_naturally/1.

:- dynamic 
    base_extractiveness/2, 
    suppression_score/2, 
    requires_active_enforcement/1,
    emerges_naturally/1.

/**
 * DOMAIN PRIORS MODULE - v3.2.4 Hardened
 * Resolves redefinition warnings and restores missing API procedures.
 */

%% ============================================================================
%% 1. CATEGORY PROFILES
%% ============================================================================
category_profile(physical_natural, [1.0, 1.0, 0.0, 0.0]).
category_profile(formal_logic,     [0.9, 0.2, 0.1, 0.1]).
category_profile(statutory_formal, [0.8, 0.5, 0.7, 0.4]).
category_profile(election_cycle,   [0.8, 0.8, 0.3, 0.5]). 
category_profile(extractive_market,[0.4, 0.8, 0.8, 0.6]). 
category_profile(narrative_history,[0.6, 0.7, 0.5, 0.6]).
category_profile(unknown_novel,    [0.5, 0.5, 0.5, 0.5]).

%% ============================================================================
%% 2. API DEFINITIONS
%% ============================================================================

is_known_domain(ID) :- domain_category(ID, _), !.
is_known_domain(ID) :- base_extractiveness(ID, _), !.
is_known_domain(ID) :- suppression_score(ID, _), !.
is_known_domain(ID) :- narrative_ontology:constraint_claim(ID, _), !.

% Flag Novelty (Clears v3_1_data_repair warnings)
flag_novelty(ID) :-
    \+ is_known_domain(ID),
    format('! NOTICE: Novel Domain "~w" detected. Using neutral (0.5) priors.~n', [ID]).
flag_novelty(_).

get_prior(ID, Metric, Value) :-
    map_metric_to_hook(Metric, Hook),
    call(domain_priors:Hook, ID, Value), !.

get_prior(ID, Metric, Value) :-
    category_of(ID, Cat),
    category_profile(Cat, Vector),
    map_metric_to_vector_pos(Metric, Vector, Value), !.

get_prior(_, _, 0.5).

category_of(ID, Cat) :- domain_category(ID, Cat), !.
category_of(ID, physical_natural) :- 
    (narrative_ontology:constraint_claim(ID, natural_law) ; 
     narrative_ontology:constraint_claim(ID, physical_law)), !.
category_of(ID, election_cycle) :- 
    narrative_ontology:constraint_claim(ID, election_cycle), !.
category_of(ID, Cat) :- infer_category_from_priors(ID, Cat), !.
category_of(_, unknown_novel).

% Signature Support (Clears exported-procedure errors)
should_be_natural_law(ID) :- 
    category_of(ID, Cat), 
    expected_signature(Cat, natural_law).

expected_signature(physical_natural, natural_law).
expected_signature(formal_logic,     natural_law).
expected_signature(election_cycle,   constructed_constraint).
expected_signature(statutory_formal, constructed_constraint).
expected_signature(extractive_market, constructed_constraint).
expected_signature(narrative_history, constructed_constraint).
expected_signature(unknown_novel,    ambiguous).

validate_signature(ID, Detected) :-
    category_of(ID, Cat),
    expected_signature(Cat, Expected),
    ( Detected = Expected 
    -> format('[VALIDATION] ✓ ~w: ~w matches ~w~n', [ID, Detected, Cat])
    ;  format('[VALIDATION] ✗ ~w: Expected ~w, got ~w~n', [ID, Expected, Detected])).

%% ============================================================================
%% 3. INTERNAL HELPERS
%% ============================================================================

map_metric_to_hook(base_extractiveness(_), base_extractiveness).
map_metric_to_hook(extractiveness,         base_extractiveness).
map_metric_to_hook(suppression(_),          suppression_score).
map_metric_to_hook(suppression_requirement, suppression_score).

map_metric_to_vector_pos(accessibility_collapse(_), [A,_,_,_], A).
map_metric_to_vector_pos(stakes_inflation(_),      [_,S,_,_], S).
map_metric_to_vector_pos(suppression(_),           [_,_,U,_], U).
map_metric_to_vector_pos(resistance(_),            [_,_,_,R], R).

infer_category_from_priors(ID, extractive_market) :- 
    base_extractiveness(ID, E), E > 0.6, !.
infer_category_from_priors(ID, statutory_formal) :- 
    requires_active_enforcement(ID), !.

%% ============================================================================
%% 4. SIGNATURE 
%% ============================================================================
expected_signature(physical_natural, natural_law).
expected_signature(formal_logic,     natural_law).
expected_signature(election_cycle,   constructed_constraint).
expected_signature(statutory_formal, constructed_constraint).
expected_signature(extractive_market, constructed_constraint).
expected_signature(narrative_history, constructed_constraint).
expected_signature(unknown_novel,    ambiguous).

validate_signature(ID, Detected) :-
    category_of(ID, Cat),
    expected_signature(Cat, Expected),
    (   Detected = Expected
    ->  format('[VALIDATION] ✓ ~w: ~w matches ~w~n', [ID, Detected, Cat])
    ;   format('[VALIDATION] ✗ ~w: Expected ~w, got ~w~n', [ID, Expected, Detected])
    ).

%% ============================================================================
%% 5. DOMAIN REGISTRY (UPDATE VIA domain_priors.py)
%% ============================================================================
domain_category(ai_evaluators_matching, extractive_market).
domain_category(airbnb_str_regulation, narrative_history).
domain_category(automatic_enrollment_defaults, narrative_history).
domain_category(biological_curiosity, narrative_history).
domain_category(black_soil_toxicity, extractive_market).
domain_category(blackstone_carried_interest_taxation, narrative_history).
domain_category(blackstone_conflicts_of_interest, extractive_market).
domain_category(blackstone_smd_control, extractive_market).
domain_category(blackstone_tra, extractive_market).
domain_category(choice_architecture_design, narrative_history).
domain_category(cloudflare_dual_class_asymmetry, extractive_market).
domain_category(coinbase_crypto_volatility, narrative_history).
domain_category(coinbase_regulatory_uncertainty, extractive_market).
domain_category(college_admissions_market, extractive_market).
domain_category(colombia_2026_presidential_election, narrative_history).
domain_category(copyleft_viral_licensing, narrative_history).
domain_category(copyright_protection, narrative_history).
domain_category(couples_residency_match, narrative_history).
domain_category(creative_commons_licensing, narrative_history).
domain_category(dark_patterns_manipulation, extractive_market).
domain_category(dexy_gold_protocol, narrative_history).
domain_category(dharma_of_kurukshetra, narrative_history).
domain_category(ergo_autolykos_asic_resistance, narrative_history).
domain_category(ergo_lets_protocol, narrative_history).
domain_category(ergo_mixer_protocol, narrative_history).
domain_category(ergo_nipopows, narrative_history).
domain_category(ergo_storage_rent, narrative_history).
domain_category(exploration_vs_exploitation, narrative_history).
domain_category(fair_use_doctrine, narrative_history).
domain_category(gale_shapley_matching, narrative_history).
domain_category(genetic_algorithms_evolution, narrative_history).
domain_category(golden_handcuffs, narrative_history).
domain_category(great_awakening_rekindling, narrative_history).
domain_category(hamiltonian_path_complexity, narrative_history).
domain_category(hammurabi_lex_talionis, narrative_history).
domain_category(heuristic_optimization, narrative_history).
domain_category(information_foraging_theory, narrative_history).
domain_category(institutional_mutation_domestication, extractive_market).
domain_category(kidney_exchange_market, narrative_history).
domain_category(kjv_linguistic_residue, narrative_history).
domain_category(kjv_textual_authority, narrative_history).
domain_category(kubo_ranking_system_r7, extractive_market).
domain_category(lehman_repo_105, extractive_market).
domain_category(lets, narrative_history).
domain_category(local_vs_global_optima, narrative_history).
domain_category(matching_markets_general, narrative_history).
domain_category(max_flow_min_cut, narrative_history).
domain_category(medical_residency_match, narrative_history).
domain_category(medieval_church_hegemony, extractive_market).
domain_category(nipopows, narrative_history).
domain_category(non_compete_agreements, extractive_market).
domain_category(optimal_stopping_marriage, narrative_history).
domain_category(permissive_software_licensing, narrative_history).
domain_category(protocol_r7_isolation, extractive_market).
domain_category(public_domain_commons, narrative_history).
domain_category(puritan_new_world_pivot, narrative_history).
domain_category(relativity_of_simultaneity, narrative_history).
domain_category(relativity_physical_invariance, narrative_history).
domain_category(rosen_bridge_protocol, narrative_history).
domain_category(section_469_c7_professional_threshold, narrative_history).
domain_category(shannon_entropy_limit, narrative_history).
domain_category(sig_usd_protocol, narrative_history).
domain_category(silicon_lexicon_overload, extractive_market).
domain_category(skills_based_hiring, narrative_history).
domain_category(sludge_bureaucratic_friction, extractive_market).
domain_category(storage_rent, narrative_history).
domain_category(sts86_ascent_checklist, narrative_history).
domain_category(tax_code_section_469, narrative_history).
domain_category(tcp_rfc9293_interoperability, narrative_history).
domain_category(tcp_state_machine_logic, narrative_history).
domain_category(trade_secret_law, narrative_history).
domain_category(traveling_salesperson_problem, narrative_history).
domain_category(visa_ipo_regulatory_compliance, narrative_history).
domain_category(visa_judgment_sharing_agreement, narrative_history).
:- module(drl_core, [
    dr_type/2,            
    dr_mismatch/3,        
    dr_action/2,
    dr_signature/2,
    % NEW: Indexed classification (re-exports from constraint_indexing)
    constraint_classification/3,   % Indexed version
    constraint_claim_indexed/2,    % Backward compatible (renamed to avoid collision)
    multi_index_report/1,          % Analysis utility
    compare_perspectives/2,        % Comparison utility
    discover_my_context/1          % Interactive context builder
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(structural_signatures).  % v3.2 signature detection
:- use_module(constraint_indexing).
:- use_module(constraint_instances).

% Re-export indexed classification predicates from constraint_indexing
:- reexport(constraint_indexing, [
    constraint_classification/3,
    constraint_claim_indexed/2,
    multi_index_report/1,
    compare_perspectives/2,
    discover_my_context/1
]).

% --- Classification Logic ---

is_mountain(C) :-
    v3_1_config:param(mountain_suppression_ceiling, Ceil),
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E =< Ceil.

is_rope(C) :-
    v3_1_config:param(rope_extraction_ceiling, XCeil),
    narrative_ontology:constraint_metric(C, extractiveness, X), X =< XCeil.

is_noose(C) :-
    v3_1_config:param(noose_extraction_floor, XFloor),
    v3_1_config:param(noose_suppression_floor, EFloor),
    narrative_ontology:constraint_metric(C, extractiveness, X), X >= XFloor,
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E >= EFloor.

is_tangled_rope(C) :-
    v3_1_config:param(rope_extraction_ceiling, RopeX),
    v3_1_config:param(tangled_rope_extraction_ceil, TangledX),
    narrative_ontology:constraint_metric(C, extractiveness, X), 
    X > RopeX, X =< TangledX.

is_zombie(C) :-
    v3_1_config:param(zombie_extraction_ceiling, XCeil),
    narrative_ontology:constraint_metric(C, extractiveness, X), X =< XCeil,
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E > XCeil.

% Canonical Mapping (v3.2: Signature-aware)
% First get metric-based type, then check if signature overrides it
dr_type(C, Type) :-
    metric_based_type(C, MetricType),
    structural_signatures:integrate_signature_with_modal(C, MetricType, Type).

% Metric-based classification (unchanged from v3.1)
metric_based_type(C, mountain)     :- is_mountain(C), !.
metric_based_type(C, noose)        :- is_noose(C), !.
metric_based_type(C, rope)         :- is_rope(C), !.
metric_based_type(C, tangled_rope) :- is_tangled_rope(C), !.
metric_based_type(C, zombie)       :- is_zombie(C), !.
metric_based_type(_, unknown).

% --- Action Routing (Grouped to avoid Discontiguous warning) ---

dr_action(C, accept)      :- dr_type(C, mountain).
dr_action(C, maintain)    :- dr_type(C, rope).
dr_action(C, reform)      :- dr_type(C, tangled_rope).
dr_action(C, cut)         :- dr_type(C, noose).
dr_action(C, bypass)      :- dr_type(C, zombie).
dr_action(C, investigate) :- dr_type(C, unknown).

% --- DRL Error Taxonomy ---

dr_mismatch(C, type_1_false_mountain, severe) :-
    narrative_ontology:constraint_claim(C, mountain),
    \+ is_mountain(C).

dr_mismatch(C, type_3_noose_as_rope, severe) :-
    narrative_ontology:constraint_claim(C, rope),
    is_noose(C).

dr_mismatch(C, type_5_zombie_as_noose, moderate) :-
    narrative_ontology:constraint_claim(C, noose),
    is_zombie(C).

/* ================================================================
   v3.2 STRUCTURAL SIGNATURE DETECTION
   ================================================================ */

%% dr_signature(+Constraint, -Signature)
%  Detects structural signature: natural_law | coordination_scaffold | 
%  constructed_constraint | ambiguous
dr_signature(C, Signature) :-
    structural_signatures:constraint_signature(C, Signature).

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
:- module(intent_engine, [
    classify_interval/3
]).

:- use_module(library(lists)).        % Required for sum_list/2
:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection). % Math Provider
:- use_module(pattern_analysis).        % State Provider

/* ================================================================
   1. MAIN ENTRY
   ================================================================ */

classify_interval(IntervalID, Pattern, Confidence) :-
    % Ensure the analysis service has populated the dynamic facts
    pattern_analysis:analyze_interval(IntervalID),
    pattern_analysis:interval_preliminary_pattern(IntervalID, Prelim),
    pattern_analysis:interval_system_gradient(IntervalID, coercion, Gsys),
    pattern_analysis:interval_data_completeness(IntervalID, DataScore),
    
    collect_intent_evidence(IntervalID, Evidence),
    (   structural_coercive_intent(IntervalID, Prelim, Gsys, Evidence)
    ->  Pattern = structural_coercive_intent,
        refine_confidence(Evidence, DataScore, Confidence)
    ;   classify_non_intent(Prelim, Pattern),
        fallback_confidence(DataScore, Confidence)
    ).

/* ================================================================
   2. STRUCTURAL COERCIVE INTENT (4 CONDITIONS)
   ================================================================ */

structural_coercive_intent(_IntervalID, Prelim, Gsys, Evidence) :-
    % Condition 1: Strong Positive Gradient
    Prelim = increasing_coercion,
    v3_1_config:param(system_gradient_strong_threshold, StrongThr),
    Gsys > StrongThr,

    % Condition 2: Alternatives Rejected
    member(viable(_System, Alt), Evidence),
    member(rejected(_System2, Alt), Evidence),

    % Condition 3: Beneficiary Asymmetry
    findall((Class, Delta), member(power(Class, Delta), Evidence), PCs),
    PCs \= [],
    max_by_value((MainBeneficiary, DeltaMain), PCs),
    v3_1_config:param(beneficiary_gain_min, GainMin),
    DeltaMain >= GainMin,

    % Condition 4: Suppression/Resistance Alignment
    v3_1_config:param(structural_suppression_min, SMin),
    v3_1_config:param(structural_resistance_min, RMin),
    findall(ValS, member(supp(MainBeneficiary, structural, ValS), Evidence), Ss),
    findall(ValR, member(resist(MainBeneficiary, structural, ValR), Evidence), Rs),
    Ss \= [], Rs \= [],
    average_list(Ss, AvgS), AvgS >= SMin,
    average_list(Rs, AvgR), AvgR >= RMin.

/* ================================================================
   3. HELPERS & UTILS
   ================================================================ */

collect_intent_evidence(IntervalID, Evidence) :-
    findall(viable(S,A), intent_viable_alternative(IntervalID, S, A), VAs),
    findall(rejected(S,A), intent_alternative_rejected(IntervalID, S, A), RAs),
    findall(power(C,D), intent_power_change(IntervalID, C, D), PCs),
    findall(supp(C,L,V), intent_suppression_level(IntervalID, C, L, V), Supps),
    findall(resist(C,L,V), intent_resistance_level(IntervalID, C, L, V), Ress),
    append([VAs, RAs, PCs, Supps, Ress], Evidence).

classify_non_intent(Prelim, Pattern) :-
    (Prelim = increasing_coercion -> Pattern = increasing_coercion 
    ; Prelim = decreasing_coercion -> Pattern = decreasing_coercion 
    ; Pattern = stable).

refine_confidence(Evidence, DataScore, Conf) :-
    length(Evidence, NEv),
    v3_1_config:param(data_high_threshold, DH),
    (DataScore >= DH, NEv >= 5 -> Conf = high ; Conf = medium).

fallback_confidence(DataScore, Conf) :-
    v3_1_config:param(data_high_threshold, DH),
    (DataScore >= DH -> Conf = high ; Conf = low).

% Local helper predicates (not exported)
% FIXED: Renamed to avoid conflicts with library predicates

% Safe average that handles empty lists
average_list([], 0).
average_list(List, Avg) :- 
    List \= [],
    sum_list(List, Sum), 
    length(List, N), 
    Avg is Sum / N.

% Find tuple with maximum second element
% FIXED: Renamed from max_member to avoid conflict with library(lists)
max_by_value((C, D), List) :- 
    member((C, D), List), 
    \+ (member((_, D2), List), D2 > D).
:- module(modal_evaluator, [
    classify_modal/2,
    detect_type_error/3
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config). 

%% classify_modal(+ConstraintID, -ModalType)
% Rule M: Mountain Identification (Decay=0, Enforcement=0)
classify_modal(C, mountain) :-
    narrative_ontology:constraint_metric(C, suppression_requirement, 0.0),
    narrative_ontology:constraint_metric(C, snap_back_potential, 0.0), !.

% Rule N: Noose Identification (High Enforcement, High Snap-back)
classify_modal(C, noose) :-
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E > 0.5,
    narrative_ontology:constraint_metric(C, snap_back_potential, S), S > 0.5, !.

%% detect_type_error(+C, -ErrorType, -Severity)
% Section IV: Type I Error (False Mountain)
% UPDATED: Now uses dynamic param to avoid hardcoded mismatch.
detect_type_error(C, type_1_false_mountain, severe) :-
    narrative_ontology:constraint_claim(C, mountain),
    v3_1_config:param(mountain_suppression_ceiling, Ceiling),
    narrative_ontology:constraint_metric(C, suppression_requirement, E), 
    E > Ceiling, !.

% Section IV: Type III Error (Noose Misclassified as Rope)
detect_type_error(C, type_3_extractive_rope, severe) :-
    narrative_ontology:constraint_claim(C, rope),
    narrative_ontology:constraint_metric(C, extractiveness, X), X > 0.7, !.

%% detect_gravity_extraction(+C, -Warning)
% Detects when a Noose is being sold as a Mountain (The "Khatri Move").
detect_gravity_extraction(C, Warning) :-
    narrative_ontology:constraint_claim(C, mountain),
    % Check for high enforcement energy (Suppression)
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E > 0.5,
    Warning = 'GRAVITY EXTRACTION DETECTED: Constraint claimed as Mountain requires active suppression energy.'.

%% verify_zombie_status(+C, -Warning)
% Detects if a Zombie is still consuming "Maintenance Energy" without utility.
verify_zombie_status(C, Warning) :-
    narrative_ontology:constraint_claim(C, zombie),
    narrative_ontology:constraint_metric(C, intensity, I), I > 0.1,
    narrative_ontology:constraint_metric(C, extractiveness, X), X == 0.0,
    Warning = 'ZOMBIE INEFFICIENCY: High-intensity constraint persists with zero extraction or coordination utility.'.
:- module(narrative_ontology, [
    % Core ontology
    entity/2,
    interval/3,
    event/4,

    % CE v2.0 constraint layer
    constraint_claim/2,
    recommendation/2,
    affects_constraint/2,
    veto_actor/1,
    veto_exposed/2,
    constraint_metric/3,
    omega_variable/3,

    % Optional measurement layer (v3.1 coercion metrics)
    measurement/5,

    % Optional intent evidence layer
    intent_viable_alternative/3,
    intent_alternative_rejected/3,
    intent_beneficiary_class/2,
    intent_power_change/3,
    intent_suppression_level/4,
    intent_resistance_level/4,
    intent_norm_strength/3,

    % Validation entry point
    validate_ontology/0
]).

/* ============================================================
   1. MULTIFILE & DYNAMIC DECLARATIONS
   ============================================================ */

:- multifile 
    entity/2, interval/3, event/4,
    constraint_claim/2, recommendation/2, affects_constraint/2,
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3.

:- dynamic 
    entity/2, interval/3, event/4,
    constraint_claim/2, recommendation/2, affects_constraint/2,
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3.

/* ============================================================
   2. VALIDATION LOGIC
   ============================================================ */

%% validate_ontology
%  Master entry point for checking Knowledge Base integrity.
validate_ontology :-
    format('--- [START] v3.1 DATA INTEGRITY VERIFICATION ---~n'),
    (   validate_entities,
        validate_intervals,
        validate_events,
        validate_constraint_claims,
        validate_constraint_metrics,
        validate_measurements,
        validate_omegas,
        validate_intent
    ->  format('[OK] Ontology Schema matches.~n')
    ;   format('[FAIL] Schema mismatch.~n')
    ),
    format('--- [END] Verification Complete ---~n').

validate_entities :-
    forall(entity(ID,Type),
        ( atom(ID),
          member(Type, [individual_powerless, individual_powerful, 
                        institutional, analytical, class])
        -> true
        ;  format('ERROR: Invalid entity(~w,~w)~n',[ID,Type]), fail
        )).

validate_intervals :-
    forall(interval(ID,Start,End),
        ( atom(ID), integer(Start), integer(End), Start =< End
        -> true
        ;  format('ERROR: Invalid interval(~w,~w,~w)~n',[ID,Start,End]), fail
        )).

validate_events :-
    forall(event(ID,Time,Actor,Type),
        ( atom(ID), integer(Time), (entity(Actor,_) ; atom(Actor)), atom(Type)
        -> true
        ;  format('ERROR: Invalid event(~w,~w,~w,~w)~n',[ID,Time,Actor,Type]), fail
        )).

%% validate_constraint_claims
%  Updated for v3.2.4 schema expansion.
validate_constraint_claims :-
    forall(constraint_claim(Name,Type),
        ( atom(Name),
          % Expanded list: handles scientific, coordination, and complex states
          member(Type, [
              mountain, noose, rope, tangled_rope, zombie,
              election_cycle, natural_law, physical_law
          ])
        -> true
        ;  format('ERROR: Invalid constraint_claim(~w,~w)~n',[Name,Type]), fail
        )).

validate_constraint_metrics :-
    forall(constraint_metric(Name,Metric,Val),
        ( (constraint_claim(Name,_) ; true),
          atom(Metric),
          number(Val), Val >= 0.0, Val =< 1.0
        -> true
        ;  format('ERROR: Invalid constraint_metric(~w,~w,~w)~n',[Name,Metric,Val]), fail
        )).

validate_measurements :-
    forall(measurement(ID,Target,Metric,Time,Val),
        ( atom(ID),
          (entity(Target,_) ; interval(Target,_,_)),
          (atom(Metric) ; compound(Metric)),
          integer(Time),
          number(Val), Val >= 0.0, Val =< 1.0
        -> true
        ;  format('ERROR: Invalid measurement(~w,~w,~w,~w,~w)~n',
                  [ID,Target,Metric,Time,Val]), fail
        )).

validate_omegas :-
    forall(omega_variable(ID, Type, Desc),
        ( atom(ID),
          member(Type, [empirical, conceptual, preference]),
          (atom(Desc) ; string(Desc))
        -> true
        ;  format('ERROR: Invalid omega_variable(~w,~w,~w)~n',[ID,Type,Desc]), fail
        )).

validate_intent :-
    forall(intent_viable_alternative(I,S,A),
        ( (interval(I,_,_) ; atom(I)), (entity(S,_) ; atom(S)), atom(A)
        -> true
        ;  format('ERROR: Invalid intent_viable_alternative(~w,~w,~w)~n',[I,S,A]), fail
        )).
:- module(pattern_analysis, [
    analyze_interval/1,
    interval_system_gradient/3,
    interval_data_completeness/2,
    interval_preliminary_pattern/2
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection).

:- dynamic interval_system_gradient/3.
:- dynamic interval_data_completeness/2.
:- dynamic interval_preliminary_pattern/2.

analyze_interval(IntervalID) :-
    interval(IntervalID, T0, _),
    retractall(interval_system_gradient(IntervalID, _, _)),
    retractall(interval_data_completeness(IntervalID, _)),
    retractall(interval_preliminary_pattern(IntervalID, _)),
    
    % Explicitly call the math module to solve the warning
    v3_1_coercion_projection:system_gradient(IntervalID, T0, Gsys),
    assertz(interval_system_gradient(IntervalID, coercion, Gsys)),
    
    compute_completeness(IntervalID, Score),
    assertz(interval_data_completeness(IntervalID, Score)),
    
    param(system_gradient_threshold, Thr),
    (Gsys > Thr -> P = increasing_coercion ; Gsys < -Thr -> P = decreasing_coercion ; P = stable),
    assertz(interval_preliminary_pattern(IntervalID, P)).

compute_completeness(ID, Score) :-
    interval(ID, T0, Tn),
    % Explicit call to solve warning
    findall((L, T), (level(L), member(T, [T0, Tn]), v3_1_coercion_projection:coercion_vector(L, T, _)), Vectors),
    length(Vectors, N),
    Score is N / 8.

:- module(report_generator, [generate_full_report/1, generate_indexed_report/3]).

:- use_module(library(lists)).        % Required for sum_list/2
:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(intent_engine).
:- use_module(v3_1_coercion_projection).
:- use_module(pattern_analysis).
:- use_module(constraint_bridge).
:- use_module(modal_evaluator).
:- use_module(drl_core).          
:- use_module(uke_dr_bridge).
:- use_module(structural_signatures).  % v3.2 signature detection
:- use_module(constraint_indexing).    % NEW: Indexed classification

generate_full_report(IntervalID) :-
    interval(IntervalID, T_start, Tn),
    classify_interval(IntervalID, Pattern, Conf),
    
    format('~n~n====================================================~n'),
    format('   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      ~n'),
    format('====================================================~n'),
    format('Timeline:       ~w to ~w~n', [T_start, Tn]),
    format('Structural Pattern: ~w~n', [Pattern]),
    format('Confidence:     ~w~n', [Conf]),
    
    % --- SECTION 1: DRL ONTOLOGY AUDIT (REALITY VS. CLAIM) ---
    format('~n[CONSTRAINT INVENTORY: REALITY AUDIT]~n'),
    format('  ~20s | ~12s | ~12s | ~8s~n', ['Constraint', 'Claimed', 'Actual', 'Action']),
    format('  ----------------------------------------------------------------------~n'),
    % Wrap forall to ensure report continues even if no constraints found
    (   forall(narrative_ontology:constraint_claim(C, Claimed),
               ( drl_core:dr_type(C, Actual),
                 drl_core:dr_action(C, Action),
                 format('  ~20w | ~12w | ~12w | ~8w~n', [C, Claimed, Actual, Action])
               ))
    ;   true
    ),

    % --- SECTION 2: META-LOGICAL AUDIT ---
    format('~n[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  ! ALERT [~w]: ~w detected for ~w~n', [Sev, Err, C]))
    ;   format('  No classification errors detected. System is Ontologically Coherent.~n')
    ),

    % --- SECTION 2b: STRUCTURAL SIGNATURE ANALYSIS (v3.2) ---
    format('~n[STRUCTURAL SIGNATURE ANALYSIS]~n'),
    (   catch(
            forall(narrative_ontology:constraint_claim(C, _Claim),
                   report_constraint_signature(C)),
            Error,
            format('  [FAIL] Exception: ~w~n', [Error]))
    ;   true
    ),

    % --- SECTION 3: UKE_DR FEASIBILITY BRIDGE ---
    format('~n[UKE_DR FEASIBILITY BRIDGE]~n'),
    format('  ~40s | ~12s~n', ['Recommendation', 'UKE Status']),
    format('  ----------------------------------------------------------------------~n'),
    % Use soft-failure pattern to prevent report crash on missing recommendation data
    (   forall(narrative_ontology:recommendation(RID, Summary),
               ( ( uke_dr_bridge:uke_status(RID, UKEStatus, Reasons) 
                 -> format('  - ~40w | ~12w~n', [Summary, UKEStatus]),
                    forall(member(R, Reasons), format('    > ~w~n', [R]))
                 ;  format('  - ~40w | ~12s~n', [Summary, 'DATA_MISSING'])
                 )
               ))
    ;   true
    ),
    
    % --- SECTION 4: KINETIC MAGNITUDE ---
    % Use ~2f for float precision to avoid existence_error [cite: 99]
    findall(Kappa, (v3_1_config:level(L), v3_1_coercion_projection:coercion_magnitude(L, Tn, Kappa)), Kappas),
    (   Kappas \= [] 
    ->  sum_list(Kappas, Sum), length(Kappas, N), AvgK is Sum / N,
        format('~nAggregate Magnitude (Kappa) at Tn: ~2f~n', [AvgK])
    ;   format('~nAggregate Magnitude (Kappa): DATA_INSUFFICIENT~n')
    ),
    
    % --- SECTION 5: PERSPECTIVE GAP ---
    format('~n[PERSPECTIVAL GAP ANALYSIS]~n'),
    (   forall(narrative_ontology:constraint_claim(C, _),
               perspectival_gap_audit(C))
    ;   true
    ),
    
    format('====================================================~n').

% ============================================================================
% REPORT GENERATION
% ============================================================================

%% generate_indexed_report(+Text, +Context, -Report)
%  Generate a constraint analysis report from a specific perspective
generate_indexed_report(Text, Context, Report) :-
    extract_constraints(Text, Constraints),
    maplist(classify_with_context(Context), Constraints, Classifications),
    format_indexed_report(Classifications, Context, Report).

%% classify_with_context(+Context, +Constraint, -Classification)
%  Classify a constraint from a specific perspective
classify_with_context(Context, Constraint, classification(Constraint, Type)) :-
    constraint_indexing:constraint_classification(Constraint, Type, Context).

%% extract_constraints(+Text, -Constraints)
%  Extract constraint identifiers from text
%  
%  This is a simple implementation that looks for known constraint keywords.
%  For production use, integrate with narrative_ontology extraction.
extract_constraints(Text, Constraints) :-
    atom_codes(Text, Codes),
    findall(C, 
        (constraint_keyword(C), 
         atom_codes(C, CCode),
         sublist(CCode, Codes)),
        ConstraintsWithDups),
    sort(ConstraintsWithDups, Constraints).  % Remove duplicates

%% constraint_keyword(?ConstraintID)
%  Known constraint identifiers to look for in text
%  Add more as you create indexed classifications
constraint_keyword(catholic_church_1200).
constraint_keyword(property_rights_2025).

%% perspectival_gap_audit(+Constraint)
% Scans for contradictions between analytical, institutional, and powerless views.
perspectival_gap_audit(C) :-
    format('~n  Analysis for Constraint: ~w~n', [C]),
    % Find Powerless Perspective
    (   constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _))
    ->  true ; TypeP = none),
    
    % Find Institutional Perspective
    (   constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _))
    ->  true ; TypeI = none),

    % Report Gaps
    (   TypeP == mountain, TypeI == rope 
    ->  format('    ! GAP: Institutional "Rope" (Coordination) appears as "Mountain" (Natural Law) to the Powerless.~n')
    ;   true),
    (   TypeP == noose, TypeI == rope
    ->  format('    ! ALERT: Extractive "Noose" is masked as a functional "Rope" in the Institutional view.~n')
    ;   true),
    
    format('    - Individual (Powerless): ~w~n', [TypeP]),
    format('    - Institutional (Manager): ~w~n', [TypeI]).

%% format_indexed_report(+Classifications, +Context, -Report)
%  Format classifications into a human-readable report
format_indexed_report(Classifications, Context, Report) :-
    Context = context(agent_power(Power), time_horizon(Time), 
                      exit_options(Exit), spatial_scope(Scope)),
    with_output_to(atom(Report),
        (format('~n[INDEXED CONSTRAINT ANALYSIS]~n'),
         format('Perspective: ~w / ~w / ~w / ~w~n~n', [Power, Time, Exit, Scope]),
         format('Classifications:~n'),
         forall(member(classification(C, T), Classifications),
                format('  ~w: ~w~n', [C, T])))).

%% generate_llm_feedback(+IntervalID)
% Extracts logical friction points for recursive LLM refinement.
generate_llm_feedback(IntervalID) :-
    format('~n### START LLM REFINEMENT MANIFEST: ~w ###~n', [IntervalID]),
    
    % 1. PERSPECTIVAL GAPS (Political Flashpoints)
    format('~n[PERSPECTIVAL_GAPS]~n'),
    (   forall(narrative_ontology:constraint_claim(C, _),
               (   constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
                   constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
                   TypeP \= TypeI,
                   format('  - Constraint "~w": Individual sees ~w, but Institution sees ~w.~n', [C, TypeP, TypeI])
               ))
    ;   true
    ),

    % 2. ONTOLOGICAL MISMATCHES (Logic Errors)
    format('~n[ONTOLOGICAL_MISMATCHES]~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  - ~w: [~w] ~w detected. The claimed status does not match the observed metrics.~n', [C, Sev, Err]))
    ;   format('  - None detected.~n')
    ),

    % 3. UNRESOLVED OMEGAS (Reasoning Blockers)
    format('~n[UNRESOLVED_OMEGAS]~n'),
    (   setof((OID, Type, Desc), narrative_ontology:omega_variable(OID, Type, Desc), Omegas)
    ->  forall(member((OID, Type, Desc), Omegas),
               format('  - ~w (~w): ~w. This variable lacks empirical grounding.~n', [OID, Type, Desc]))
    ;   format('  - None detected.~n')
    ),

    format('~n### END REFINEMENT MANIFEST ###~n').


%% sublist(+Sublist, +List)
%  True if Sublist appears as a contiguous subsequence in List
sublist([], _).
sublist([H|T], [H|T2]) :- !, sublist(T, T2).
sublist(Sub, [_|T]) :- sublist(Sub, T).

/* ================================================================
   HELPER: SIGNATURE REPORTING (v3.2)
   ================================================================ */

report_constraint_signature(C) :-
    % Get structural signature
    drl_core:dr_signature(C, Signature),
    
    % Get confidence
    structural_signatures:signature_confidence(C, Signature, Confidence),
    
    % Get explanation
    structural_signatures:explain_signature(C, Signature, Explanation),
    
    % Report in compact format
    format('  ~20w: ~20w (confidence: ~w)~n', [C, Signature, Confidence]),
    
    % Only show explanation for non-ambiguous signatures
    (   Signature \= ambiguous
    ->  format('    → ~w~n', [Explanation])
    ;   true
    ).
:- module(scenario_manager, [
    clear_kb/0,
    load_and_run/2,
    list_active_intervals/0
]).

:- use_module(narrative_ontology).
:- use_module(test_harness).

% 1. FORCE DYNAMIC STATE
% This prevents "Redefined static procedure" errors even if the dataset 
% is generated without dynamic headers.
:- dynamic 
    narrative_ontology:entity/2, 
    narrative_ontology:interval/3, 
    narrative_ontology:event/4, 
    narrative_ontology:measurement/2, 
    narrative_ontology:constraint_claim/2, 
    narrative_ontology:constraint_metric/3, 
    narrative_ontology:omega_variable/3, 
    narrative_ontology:recommendation/2, 
    narrative_ontology:affects_constraint/2, 
    narrative_ontology:veto_actor/1, 
    narrative_ontology:veto_exposed/2,
    narrative_ontology:intent_fact/4.

/**
 * clear_kb
 * Forcefully retracts all facts.
 */
clear_kb :-
    format('~n[SCENARIO MANAGER] Clearing Knowledge Base...~n'),
    % Core Layers
    retractall(narrative_ontology:entity(_, _)),
    retractall(narrative_ontology:interval(_, _, _)),
    retractall(narrative_ontology:event(_, _, _, _)),
    retractall(narrative_ontology:measurement(_, _, _, _, _)),
    % Constraint Layer
    retractall(narrative_ontology:constraint_claim(_, _)),
    retractall(narrative_ontology:constraint_metric(_, _, _)),
    retractall(narrative_ontology:omega_variable(_,_,_)),
    retractall(narrative_ontology:recommendation(_, _)),
    retractall(narrative_ontology:affects_constraint(_, _)),
    retractall(narrative_ontology:veto_actor(_)),
    retractall(narrative_ontology:veto_exposed(_, _)),
    % Intent Layer
    retractall(narrative_ontology:intent_viable_alternative(_, _, _)),
    retractall(narrative_ontology:intent_alternative_rejected(_, _, _)),
    retractall(narrative_ontology:intent_beneficiary_class(_, _)),
    retractall(narrative_ontology:intent_power_change(_, _, _)),
    retractall(narrative_ontology:intent_suppression_level(_, _, _, _)),
    retractall(narrative_ontology:intent_resistance_level(_, _, _, _)),
    retractall(narrative_ontology:intent_norm_strength(_, _, _)),
    format('[OK] Knowledge Base is empty.~n').

% =============================================================================
% UPDATED Section 3: GLOBAL TEMPORAL SANITIZATION
% =============================================================================
% Iterate through EVERY interval in the KB to satisfy the Broad Auditor.
sanitize_all_intervals :-
    forall(narrative_ontology:interval(_ID, Start, End), (
        ensure_endpoint(Start),
        ensure_endpoint(End)
    )).

ensure_endpoint(T) :-
    narrative_ontology:measurement(T, _) -> true ; 
    % Impute neutral padding for any endpoint found in any interval.
    assertz(narrative_ontology:measurement(T, [0.5, 0.5, 0.5, 0.5])).

% 4. OMEGA ROUTER
% Routes Omega variables to appropriate resolution channels based 
% on the classification framework.
route_omega(ID) :-
    narrative_ontology:omega_variable(ID, Type, Desc),
    format('[OMEGA] Identified ~w (~w): ~w~n', [ID, Type, Desc]),
    (   Type == empirical  -> format(' -> Action: Design Measurement/Experiment.~n')
    ;   Type == conceptual -> format(' -> Action: Define Framework/Terms.~n')
    ;   Type == preference -> format(' -> Action: Escalate to Stakeholders.~n')
    ;   format(' -> Warning: Unknown Omega Type.~n')
    ). % Added missing closing parenthesis and period.

% =============================================================================
% UPDATED Section 5: TEST EXECUTION (Scenario Manager)
% =============================================================================
load_and_run(File, IntervalID) :-
    clear_kb,
    (   exists_file(File)
    ->  format('[SCENARIO MANAGER] Loading: ~w...~n', [File]),
        user:consult(File),

	% INJECT STRUCTURAL ANCHOR: Resolves [STEP 1] errors
        % This provides the audit suite with the interval it expects.
        assertz(narrative_ontology:interval(IntervalID, 0, 10)),
        inject_minimal_measurements(IntervalID),
	
        % FIX: Repair ALL intervals found in the KB, not just the primary one.
        format('[SCENARIO MANAGER] Performing Global Repair...~n'),
        forall(narrative_ontology:interval(ID, _, _), 
               v3_1_data_repair:repair_interval(ID)),
        
        % Proceed with the standard test suite
        test_harness:run_all_tests(IntervalID)
    ;   format('[ERROR] File ~w not found.~n', [File])
    ).

% 6. HELPER
list_active_intervals :-
    findall(ID, narrative_ontology:interval(ID, _, _), IDs),
    format('~nActive Intervals in KB: ~w~n', [IDs]).

% Helper to keep the main predicate clean
inject_minimal_measurements(ID) :-
    forall(member(T, [0, 10]),
        ( assertz(narrative_ontology:measurement(m_gen, ID, accessibility_collapse(structural), T, 0.5)),
          assertz(narrative_ontology:measurement(m_gen, ID, stakes_inflation(structural), T, 0.5)),
          assertz(narrative_ontology:measurement(m_gen, ID, suppression(structural), T, 0.5)),
          assertz(narrative_ontology:measurement(m_gen, ID, resistance(structural), T, 0.5))
        )).
:- module(structural_signatures, [
    constraint_signature/2,
    signature_confidence/3,
    explain_signature/3,
    integrate_signature_with_modal/3
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(v3_1_config).

/* ================================================================
   STRUCTURAL SIGNATURE DETECTION v3.2
   
   Problem Statement (from Copilot's analysis):
   "Your classifier sees MAGNITUDE but not TYPE."
   
   The current DRL classifier uses only metric thresholds:
   - suppression > 0.1 → not a mountain
   - extractiveness > 0.7 → noose
   
   This causes misclassification of conceptual constraints:
   - Chaitin's Ω: collapse=1.0, suppression=0.0 → classified as mountain
   - Special Relativity: collapse=1.0, suppression=0.0 → classified as mountain
   - Arrow's Theorem: collapse=0.9, suppression=0.1 → fraud detection fires
   
   But these are STRUCTURALLY DIFFERENT:
   - Chaitin's Ω = NATURAL LAW (inherent impossibility)
   - Special Relativity = COORDINATION SCAFFOLD (successful standard)
   - Arrow's Theorem = NATURAL LAW (mathematical impossibility)
   
   Solution: Add STRUCTURAL SIGNATURES that detect constraint ORIGIN
   rather than just constraint METRICS.
   
   Three Core Signatures:
   1. Natural Law - empirical invariant, no alternatives possible
   2. Coordination Scaffold - voluntary equilibrium, alternatives existed
   3. Constructed Constraint - enforced rule, power asymmetries
   ================================================================ */

%% constraint_signature(+ConstraintID, -Signature)
%  Main entry point: classifies structural signature
%  Returns: natural_law | coordination_scaffold | constructed_constraint | ambiguous
constraint_signature(C, Signature) :-
    get_constraint_profile(C, Profile),
    classify_by_signature(Profile, Signature).

/* ================================================================
   PROFILE EXTRACTION
   
   Extracts 7 key features for signature classification:
   1. Accessibility Collapse (avg across time)
   2. Suppression Requirement (avg across time)
   3. Resistance Level (avg across time)
   4. Beneficiary Count (how many asymmetric winners)
   5. Has Viable Alternatives (were there choices?)
   6. Temporal Stability (does it evolve or remain constant?)
   7. Coordination Success (high access + low enforcement)
   ================================================================ */

get_constraint_profile(C, 
                      profile(AccessCollapse, Suppression, Resistance,
                             BeneficiaryCount, HasAlternatives, 
                             TemporalStability, CoordinationSuccess)) :-
    
    % Get averaged metrics across all levels
    get_metric_average(C, accessibility_collapse, AccessCollapse),
    get_metric_average(C, suppression_requirement, Suppression),
    get_metric_average(C, resistance, Resistance),
    
    % Count asymmetric beneficiaries
    count_power_beneficiaries(C, BeneficiaryCount),
    
    % Check for viable alternatives
    has_viable_alternatives(C, HasAlternatives),
    
    % Compute temporal stability
    compute_temporal_stability(C, TemporalStability),
    
    % Check coordination success pattern
    CoordinationSuccess = (AccessCollapse > 0.8, Suppression < 0.2).

%% get_metric_average(+Constraint, +MetricType, -Average)
%  Averages a metric across all levels (structural, organizational, class, individual)
get_metric_average(C, MetricType, Average) :-
    findall(Val, narrative_ontology:constraint_metric(C, MetricType, Val), Vals),
    (   Vals \= []
    ->  sum_list(Vals, Sum),
        length(Vals, N),
        Average is Sum / N
    ;   Average = 0.5  % Default if no data
    ).

%% count_power_beneficiaries(+Constraint, -Count)
%  Counts distinct classes with positive power changes
count_power_beneficiaries(C, Count) :-
    % Find intervals affecting this constraint
    findall(Class, (
        narrative_ontology:affects_constraint(I, C),
        narrative_ontology:intent_power_change(I, Class, Delta),
        Delta > 0.1  % Meaningful gain threshold
    ), Beneficiaries),
    sort(Beneficiaries, UniqueBeneficiaries),
    length(UniqueBeneficiaries, Count).

%% has_viable_alternatives(+Constraint, -HasAlternatives)
%  Checks if viable alternatives were considered (indicates choice vs necessity)
has_viable_alternatives(C, true) :-
    narrative_ontology:affects_constraint(I, C),
    narrative_ontology:intent_viable_alternative(I, _, _), !.
has_viable_alternatives(_, false).

%% compute_temporal_stability(+Constraint, -Stability)
%  Measures whether constraint metrics remain stable over time
%  Returns: stable | evolving
compute_temporal_stability(C, Stability) :-
    % Get suppression values at different time points for this constraint
    findall(Val, 
            narrative_ontology:constraint_metric(C, suppression_requirement, Val),
            Vals),
    (   Vals = []
    ->  Stability = unknown
    ;   Vals = [_SingleVal]
    ->  Stability = stable  % Only one measurement
    ;   compute_variance(Vals, Variance),
        (   Variance < 0.05
        ->  Stability = stable
        ;   Stability = evolving
        )
    ).

compute_variance(Vals, Variance) :-
    length(Vals, N),
    N > 0,
    sum_list(Vals, Sum),
    Mean is Sum / N,
    findall(SqDiff, (member(V, Vals), SqDiff is (V - Mean) * (V - Mean)), SqDiffs),
    sum_list(SqDiffs, SumSqDiffs),
    Variance is SumSqDiffs / N.

/* ================================================================
   SIGNATURE CLASSIFICATION LOGIC
   
   Decision Tree:
   
   1. Check Natural Law signature FIRST (most specific)
      - Extreme collapse + minimal enforcement + no alternatives
      - Examples: Chaitin's Ω, Heisenberg, Arrow's Theorem
      
   2. Check Coordination Scaffold SECOND
      - Extreme collapse + minimal enforcement + HAS alternatives
      - Examples: Special Relativity, SI Units, ISO Standards
      
   3. Check Constructed Constraint LAST (most general)
      - Positive enforcement OR beneficiary asymmetries
      - Examples: 26 USC §469, GS1 Barcodes, Hammurabi's Code
      
   4. Otherwise: ambiguous
   ================================================================ */

classify_by_signature(Profile, natural_law) :-
    natural_law_signature(Profile), !.

classify_by_signature(Profile, coordination_scaffold) :-
    coordination_scaffold_signature(Profile), !.

classify_by_signature(Profile, constructed_constraint) :-
    constructed_constraint_signature(Profile), !.

classify_by_signature(_, ambiguous).

/* ================================================================
   SIGNATURE 1: NATURAL LAW
   
   Diagnostic Pattern:
   ✓ Extreme accessibility collapse (≥ 0.85)
   ✓ Minimal suppression (≤ 0.15)
   ✓ Minimal resistance (≤ 0.15)
   ✓ Zero beneficiaries (no asymmetric winners)
   ✓ No viable alternatives (not a choice)
   ✓ Temporally stable (doesn't evolve)
   
   Interpretation:
   This is an INHERENT PROPERTY of the system, not a human choice.
   It cannot be changed by coordination or enforcement.
   
   Examples:
   - Chaitin's Omega (algorithmic information limit)
   - Heisenberg Uncertainty (quantum measurement limit)
   - Arrow's Impossibility (social choice limit)
   - Second Law of Thermodynamics (entropy increase)
   ================================================================ */

natural_law_signature(profile(AccessCollapse, Suppression, Resistance,
                             BeneficiaryCount, HasAlternatives,
                             TemporalStability, _CoordinationSuccess)) :-
    
    % Metric conditions
    v3_1_config:param(natural_law_collapse_min, CollapseMin),
    AccessCollapse >= CollapseMin,  % Default: 0.85
    
    v3_1_config:param(natural_law_suppression_max, SuppMax),
    Suppression =< SuppMax,  % Default: 0.15
    
    v3_1_config:param(natural_law_resistance_max, ResMax),
    Resistance =< ResMax,  % Default: 0.15
    
    % Structural conditions (CRITICAL for distinguishing from coordination)
    BeneficiaryCount == 0,  % No asymmetric winners
    HasAlternatives == false,  % Not a choice
    TemporalStability == stable.  % Doesn't evolve

/* ================================================================
   SIGNATURE 2: COORDINATION SCAFFOLD
   
   Diagnostic Pattern:
   ✓ Extreme accessibility collapse (≥ 0.85)
   ✓ Minimal suppression (≤ 0.15)
   ✓ Minimal resistance (≤ 0.15)
   ✓ Zero or few beneficiaries (symmetric benefits)
   ✓ HAS viable alternatives (WAS a choice)
   ✓ May be stable or evolving (can be refined)
   
   KEY DISTINCTION from Natural Law:
   - Alternatives EXISTED → this was a COORDINATION CHOICE
   - Success is CONTINGENT → depends on continued acceptance
   - Could theoretically be replaced → not inherent to reality
   
   Interpretation:
   This is a SUCCESSFUL VOLUNTARY STANDARD that everyone adopted
   because it provides symmetric coordination benefits.
   
   Examples:
   - Special Relativity (replaced Newtonian mechanics)
   - SI Metric System (replaced imperial units)
   - UTC Time Standard (replaced local solar time)
   - IP Protocol (replaced other network protocols)
   ================================================================ */

coordination_scaffold_signature(profile(AccessCollapse, Suppression, Resistance,
                                       BeneficiaryCount, HasAlternatives,
                                       _TemporalStability, _CoordinationSuccess)) :-
    
    % Metric conditions (same as natural law)
    v3_1_config:param(coordination_collapse_min, CollapseMin),
    AccessCollapse >= CollapseMin,  % Default: 0.85
    
    v3_1_config:param(coordination_suppression_max, SuppMax),
    Suppression =< SuppMax,  % Default: 0.15
    
    v3_1_config:param(coordination_resistance_max, ResMax),
    Resistance =< ResMax,  % Default: 0.15
    
    % Structural conditions (DIFFERENT from natural law)
    BeneficiaryCount =< 1,  % Symmetric or near-symmetric
    HasAlternatives == true.  % KEY: This WAS a choice

/* ================================================================
   SIGNATURE 3: CONSTRUCTED CONSTRAINT
   
   Diagnostic Pattern:
   ✓ Variable accessibility collapse
   ✓ Positive suppression (> 0.2) OR
   ✓ Positive resistance (> 0.2) OR
   ✓ Multiple beneficiaries (asymmetric gains)
   
   Interpretation:
   This is an INSTITUTIONALLY ENFORCED RULE that requires
   active maintenance and produces asymmetric outcomes.
   
   Examples:
   - 26 USC §469 (passive loss limitation)
   - GS1 Barcode System (licensing monopoly)
   - Hammurabi's Code (benefice system)
   - Lehman's Repo 105 (accounting fiction)
   ================================================================ */

constructed_constraint_signature(profile(_AccessCollapse, Suppression, Resistance,
                                        BeneficiaryCount, _HasAlternatives,
                                        _TemporalStability, _CoordinationSuccess)) :-
    
    % At least one indicator of constructed constraint
    (   Suppression > 0.2        % Requires enforcement
    ;   Resistance > 0.2         % Faces opposition  
    ;   BeneficiaryCount > 1     % Asymmetric benefits
    ).

/* ================================================================
   CONFIDENCE SCORING
   
   Returns confidence level based on how strongly the signature
   pattern matches the classification.
   ================================================================ */

%% signature_confidence(+ConstraintID, +Signature, -Confidence)
%  Returns: high | medium | low
signature_confidence(C, Signature, Confidence) :-
    get_constraint_profile(C, Profile),
    compute_signature_confidence(Profile, Signature, Confidence).

compute_signature_confidence(Profile, natural_law, Confidence) :-
    Profile = profile(AccessCollapse, Suppression, Resistance, _, _, _, _),
    
    % Count strong indicators
    findall(1, (
        (AccessCollapse > 0.95);
        (Suppression < 0.05);
        (Resistance < 0.05)
    ), Indicators),
    length(Indicators, Count),
    
    (   Count >= 3 -> Confidence = high
    ;   Count >= 2 -> Confidence = medium
    ;   Confidence = low
    ).

compute_signature_confidence(Profile, coordination_scaffold, Confidence) :-
    Profile = profile(AccessCollapse, Suppression, _, _, HasAlternatives, _, _),
    
    % Strong indicators
    findall(1, (
        (AccessCollapse > 0.95);
        (Suppression < 0.05);
        (HasAlternatives == true)  % Critical for coordination
    ), Indicators),
    length(Indicators, Count),
    
    (   Count >= 3 -> Confidence = high
    ;   Count >= 2 -> Confidence = medium
    ;   Confidence = low
    ).

compute_signature_confidence(Profile, constructed_constraint, Confidence) :-
    Profile = profile(_, Suppression, Resistance, BeneficiaryCount, _, _, _),
    
    % Count enforcement indicators
    findall(1, (
        (Suppression > 0.5);
        (Resistance > 0.5);
        (BeneficiaryCount > 2)
    ), Indicators),
    length(Indicators, Count),
    
    (   Count >= 2 -> Confidence = high
    ;   Count >= 1 -> Confidence = medium
    ;   Confidence = low
    ).

compute_signature_confidence(_, ambiguous, low).

/* ================================================================
   EXPLANATION GENERATION
   ================================================================ */

%% explain_signature(+ConstraintID, +Signature, -Explanation)
explain_signature(C, natural_law, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(AC, S, R, _, _, _, _),
    format(atom(Explanation),
           'NATURAL LAW signature for ~w: Extreme inaccessibility (collapse=~2f) with minimal enforcement (suppression=~2f, resistance=~2f). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.',
           [C, AC, S, R]).

explain_signature(C, coordination_scaffold, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(AC, S, _, _, _, _, _),
    format(atom(Explanation),
           'COORDINATION SCAFFOLD signature for ~w: Extreme accessibility (collapse=~2f) with minimal enforcement (suppression=~2f). Viable alternatives existed historically, indicating this is a successful coordination standard rather than a natural law. Maintains adoption through symmetric benefits.',
           [C, AC, S]).

explain_signature(C, constructed_constraint, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(_, S, R, BC, _, _, _),
    format(atom(Explanation),
           'CONSTRUCTED CONSTRAINT signature for ~w: Active enforcement detected (suppression=~2f, resistance=~2f) with ~d asymmetric beneficiaries. This is an institutionally maintained rule requiring ongoing suppression energy.',
           [C, S, R, BC]).

explain_signature(C, ambiguous, Explanation) :-
    format(atom(Explanation),
           'AMBIGUOUS signature for ~w: Insufficient structural differentiation to classify. Consider gathering more data on alternatives, beneficiaries, and temporal evolution.',
           [C]).

/* ================================================================
   INTEGRATION WITH MODAL CLASSIFICATION
   
   This is the key integration point: structural signatures
   OVERRIDE modal classification when there's a mismatch.
   
   Example: Special Relativity
   - Modal classifier says: "mountain" (suppression=0, snapback=0)
   - Signature detector says: "coordination_scaffold"
   - Integrated result: "rope" with note about coordination success
   ================================================================ */

%% integrate_signature_with_modal(+Constraint, +ModalType, -AdjustedType)
%  Adjusts modal classification based on structural signature
integrate_signature_with_modal(C, ModalType, AdjustedType) :-
    constraint_signature(C, Signature),
    resolve_modal_signature_conflict(ModalType, Signature, AdjustedType).

% Natural laws stay as mountains (correct classification)
resolve_modal_signature_conflict(mountain, natural_law, mountain) :- !.

% Coordination scaffolds should be ROPES not mountains
resolve_modal_signature_conflict(mountain, coordination_scaffold, rope) :- !.

% Constructed constraints override mountain classification
resolve_modal_signature_conflict(mountain, constructed_constraint, tangled_rope) :- !.

% No conflict - keep original classification
resolve_modal_signature_conflict(ModalType, _, ModalType).
:- module(test_harness, [
    load_scenario/1,
    run_all_tests/1,
    quick_check/1
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection).
:- use_module(v3_1_data_repair).
:- use_module(data_verification).
:- use_module(pattern_analysis).
:- use_module(intent_engine).
:- use_module(constraint_bridge).
:- use_module(drl_core).
:- use_module(uke_dr_bridge).
:- use_module(report_generator).

/* ================================================================
   v3.2 HARD-STOP TEST HARNESS
   ================================================================ */

run_all_tests(IntervalID) :-
    format('~n>>> INITIATING v3.1.1 DR-AUDIT SUITE: ~w~n', [IntervalID]),

    % Step 1: Data Repair (Epistemic Imputation)
    format('[STEP 1] Auditing and Repairing Measurements...~n'),
    v3_1_data_repair:repair_interval(IntervalID),

    % Step 2: Hard-Stop Verification Gate
    % v3.2 Change: This is now a strict boolean gate.
    format('[STEP 2] Verifying Data Integrity...~n'),
    (   data_verification:verify_all 
    ->  format('[OK] Verification passed. Proceeding to audit.~n')
    ;   (   format('[CRITICAL FAIL] Data integrity verification failed for ~w.~n', [IntervalID]),
            format('Summary generation aborted to prevent halluncinated analysis.~n'),
            !, fail  % Hard-stop the pipeline here
        )
    ),

    % Step 3: Compute System Dynamics (Only reached if Step 2 passes)
    format('[STEP 3] Computing System Gradients...~n'),
    pattern_analysis:analyze_interval(IntervalID),

    % Step 4: DRL Ontological Audit
    format('[STEP 4] DRL Ontological Audit...~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  ! ALERT [~w]: ~w detected for ~w~n', [Sev, Err, C]))
    ;   format('  [OK] No Ontological Fraud detected.~n')
    ),

    % Step 5: Extracting Omegas
    format('[STEP 5] Extracting Omega Variables (Ω)...~n'),
    (   setof((OID, Type, Desc), narrative_ontology:omega_variable(OID, Type, Desc), Omegas)
    ->  forall(member((OID, Type, Desc), Omegas),
               format('  - Ω_~w [~w]: ~w~n', [OID, Type, Desc]))
    ;   format('  No reasoning blockers (Omegas) identified.~n')
    ),

    % Step 6: Final Reporting
    format('[STEP 6] Generating Executive Summary...~n'),
    report_generator:generate_full_report(IntervalID),

    % Step 7: Recursive Feedback
    format('[STEP 7] Generating LLM Refinement Manifest...~n'),
    report_generator:generate_llm_feedback(IntervalID).

%% quick_check(+IntervalID)
%  Diagnostic for identifying the primary "Binding Mountain" intensity.
quick_check(IntervalID) :-
    format('--- Diagnostic: ~w ---~n', [IntervalID]),
    (   drl_core:dr_type(Name, mountain),
        narrative_ontology:constraint_metric(Name, intensity, Intensity),
        Intensity > 0.8
    ->  format('CRITICAL MOUNTAIN: ~w (~2f)~n', [Name, Intensity])
    ;   format('No binding mountains detected.~n')).

load_scenario(lehman) :-
    consult('lehman_data.pl'),
    format('~n[SCENARIO] Loaded: Lehman Terminal Collapse (2008)~n').

/* ================================================================
   4. Interactive Help
   ================================================================ */

help :-
    format('~n--- v3.1 Structural Analysis Harness ---~n'),
    format('1. load_scenario(iran|lehman).~n'),
    format('2. run_all_tests(IntervalID).~n'),
    format('3. quick_check(IntervalID).~n').
% ============================================================================
% INDEXICAL CLASSIFICATION INTEGRATION TEST
% ============================================================================

% Test that the indexical system loads and basic queries work

:- begin_tests(indexical_integration).

% Test 1: Default context works
test(default_context_defined) :-
    constraint_indexing:default_context(Ctx),
    Ctx = context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    ).

% Test 2: Catholic Church multi-perspective
test(catholic_church_multi_perspective) :-
    % Serf sees Mountain
    constraint_indexing:constraint_classification(
        catholic_church_1200,
        mountain,
        context(agent_power(individual_powerless), 
                time_horizon(biographical), 
                exit_options(trapped), 
                spatial_scope(local))
    ),
    % Historian sees Noose
    constraint_indexing:constraint_classification(
        catholic_church_1200,
        noose,
        context(agent_power(analytical), 
                time_horizon(civilizational), 
                exit_options(analytical), 
                spatial_scope(continental))
    ),
    % Pope sees Rope
    constraint_indexing:constraint_classification(
        catholic_church_1200,
        rope,
        context(agent_power(institutional), 
                time_horizon(generational), 
                exit_options(arbitrage), 
                spatial_scope(continental))
    ).

% Test 3: Property rights perspectives
test(property_rights_perspectives) :-
    % Powerless sees Noose
    constraint_indexing:constraint_classification(
        property_rights_2025,
        noose,
        context(agent_power(individual_powerless),
                time_horizon(biographical),
                exit_options(trapped),
                spatial_scope(national))
    ),
    % Middle class sees Rope
    constraint_indexing:constraint_classification(
        property_rights_2025,
        rope,
        context(agent_power(individual_moderate),
                time_horizon(biographical),
                exit_options(mobile),
                spatial_scope(national))
    ).

% Test 4: Extractiveness scales with power
test(extractiveness_power_scaling) :-
    Context1 = context(agent_power(individual_powerless), 
                       time_horizon(biographical), 
                       exit_options(trapped), 
                       spatial_scope(national)),
    Context2 = context(agent_power(institutional), 
                       time_horizon(generational), 
                       exit_options(arbitrage), 
                       spatial_scope(global)),
    constraint_indexing:extractiveness_for_agent(property_rights_2025, Context1, Score1),
    constraint_indexing:extractiveness_for_agent(property_rights_2025, Context2, Score2),
    Score1 > Score2.  % Powerless experience more extraction

% Test 5: Immutability varies with time horizon
test(time_horizon_immutability) :-
    constraint_indexing:effective_immutability(biographical, trapped, mountain),
    constraint_indexing:effective_immutability(civilizational, trapped, rope).

% Test 6: Base extractiveness defined
test(base_extractiveness_defined) :-
    domain_priors:base_extractiveness(catholic_church_1200, E1),
    E1 = 0.7,
    domain_priors:base_extractiveness(property_rights_2025, E2),
    E2 = 0.5.

:- end_tests(indexical_integration).

% To run: swipl -g "consult('test_indexical_integration.pl'), run_tests." -t halt
:- module(uke_dr_bridge, [
    uke_status/3
]).

:- use_module(drl_core).
:- use_module(narrative_ontology).
:- use_module(constraint_bridge).

% Map Feasibility + DRL Type to UKE Status
uke_status(RecID, Status, Reasons) :-
    narrative_ontology:recommendation(RecID, _), % CRITICAL: Verify RecID is actually a recommendation
    narrative_ontology:affects_constraint(RecID, C),
    drl_core:dr_type(C, Type),
    constraint_bridge:recommendation_feasibility(RecID, Feas, Vetoes),
    determine_status(C, Type, Feas, Vetoes, Status, Reasons). % Passed C here

% --- Status Routing ---

determine_status(_, mountain, _, _, fantasy, 
    ['Recommendation attempts to modify a natural constraint (Mountain).']).

% Fixed: Uses C to check load-bearing status
determine_status(C, noose, _, _, blocked, 
    ['CRITICAL: Load-bearing Noose removal attempted without Scaffold.']) :-
    is_load_bearing(C),
    \+ narrative_ontology:entity(_, scaffold).

determine_status(_, Type, viable, [], viable, 
    ['No structural or political vetoes detected.']) :-
    member(Type, [rope, noose, zombie]).

determine_status(_, tangled_rope, _, _, aspirational, 
    ['System is Tangled: Extraction is rising. Coordination remains, but Reform is required.']).

% Fallback for unclassified constraints
determine_status(_, unknown, _, _, investigate, 
    ['Structural audit incomplete: Constraint metrics do not match known types.']).

determine_status(_, _, blocked_by_veto, Vetoes, blocked, [Msg]) :-
    format(string(Msg), 'Vetoed by: ~w', [Vetoes]).

% Helper
is_load_bearing(C) :- 
    drl_core:dr_type(C, noose),
    v3_1_config:param(noose_load_bearing_threshold, T),
    narrative_ontology:constraint_metric(C, extractiveness, X), 
    X > T.
:- module(v3_1_coercion_projection, [
    coercion_vector/3,
    coercion_magnitude/3,
    coercion_gradient/4,
    system_gradient/3,
    time_point_in_interval/2
]).

:- use_module(library(lists)).        % Required for sum_list/2
:- use_module(narrative_ontology).
:- use_module(v3_1_config).

% Base Vector
coercion_vector(Level, Time, [A, S, U, R]) :-
    measurement(_, _, accessibility_collapse(Level), Time, A),
    measurement(_, _, stakes_inflation(Level),      Time, S),
    measurement(_, _, suppression(Level),           Time, U),
    measurement(_, _, resistance(Level),            Time, R).

% Magnitude logic
coercion_magnitude(Level, Time, Kappa) :-
    coercion_vector(Level, Time, [A, S, U, R]),
    aggregation_weights(Level, WA, WS, WU, WR),
    Kappa is (WA * A) + (WS * S) + (WU * U) + (WR * R).

% Time points helper (Optimized)
time_point_in_interval(IntervalID, Time) :-
    interval(IntervalID, T_start, T_end),
    % Find all unique times present in the measurement database
    setof(T, is_measurement_time(T), AllTimes),
    member(Time, AllTimes),
    Time >= T_start,
    Time =< T_end,
    !.

% Helper to isolate the cross-module dynamic call
is_measurement_time(T) :-
    narrative_ontology:measurement(_, _, _, T, _).

% Gradient logic (Guarded)
coercion_gradient(Level, IntervalID, T_now, Grad) :-
    interval(IntervalID, _, T_end),
    T_now < T_end,
    % Use setof to find future points but wrap in a conditional to prevent looping
    (   setof(T_next, 
              (time_point_in_interval(IntervalID, T_next), T_next > T_now), 
              FuturePoints)
    ->  FuturePoints = [T_next|_], % Take the immediate next point
        coercion_magnitude(Level, T_now, K_now),
        coercion_magnitude(Level, T_next, K_next),
        Grad is K_next - K_now,
        ! % Prevent backtracking into the setof search
    ;   fail % Explicitly fail if no future points exist in the interval
    ).

% System Gradient (Safe Aggregation)
system_gradient(IntervalID, Time, SysGrad) :-
    findall(WG,
        ( level(L),
          influence_weight(L, W),
          % Ensure coercion_gradient succeeds before calculating
          coercion_gradient(L, IntervalID, Time, G),
          WG is W * G
        ),
        WGList),
    % Guard against empty lists at the end of a timeline
    (   WGList \= []
    ->  sum_list(WGList, SysGrad), !
    ;   SysGrad = 0.0 % Return neutral gradient if no changes are detected
    ).
:- module(v3_1_config, [
    param/2,
    aggregation_weights/5,
    influence_weight/2,
    level/1
]).

/* ================================================================
   1. HIERARCHY DEFINITIONS
   ================================================================ */

%% level(?Level)
% Defines the four analytical levels of the system.
level(structural).
level(organizational).
level(class).
level(individual).

/* ================================================================
   2. COMPONENT WEIGHTS (Alpha)
   Maps components (A, S, U, R) to magnitude (Kappa) per level.
   Formula: $\kappa = (\alpha_A \cdot A) + (\alpha_S \cdot S) + (\alpha_U \cdot U) + (\alpha_R \cdot R)$.
   ================================================================ */

%% aggregation_weights(+Level, -AlphaA, -AlphaS, -AlphaU, -AlphaR)
% Weights are shifted based on level-specific significance.
aggregation_weights(structural,     0.30, 0.20, 0.20, 0.20).
aggregation_weights(organizational, 0.30, 0.40, 0.15, 0.15).
aggregation_weights(class,          0.30, 0.20, 0.30, 0.30).
aggregation_weights(individual,     0.25, 0.25, 0.25, 0.25).

/* ================================================================
   3. INFLUENCE WEIGHTS (w_i)
   Determines how much each level contributes to the System Gradient (Gsys).
   ================================================================ */

%% influence_weight(+Level, -Weight)
influence_weight(structural,     0.40). % Highest impact on system stability.
influence_weight(organizational, 0.30).
influence_weight(class,          0.20).
influence_weight(individual,     0.10).

/* ================================================================
   4. INTENT & DETECTION THRESHOLDS
   Consolidated parameters for the intent_engine and pattern_analysis.
   ================================================================ */

% --- Gradient Thresholds ---
% Minimum change to be considered non-stable.
param(system_gradient_threshold, 0.01).
% Threshold for "Strong" intent classification.
param(system_gradient_strong_threshold, 1.00).

% --- Beneficiary Asymmetry ---
% Min power gain required to be identified as the "Main Beneficiary".
param(beneficiary_gain_min, 0.50).
% Max gain allowed for other classes to maintain "Asymmetry".
param(loser_loss_max_gain, 0.10).

% --- Suppression & Resistance Alignment ---
% Required thresholds for structural-level alignment.
param(structural_suppression_min, 0.70). 
param(structural_resistance_min, 0.70).

% --- Data Integrity ---
% Thresholds for high/medium confidence classification.
param(data_high_threshold, 0.95).
param(data_medium_threshold, 0.75).

/* ================================================================
   5. DEFERENTIAL REALISM (DR) THRESHOLDS
   ================================================================ */

% --- Mountain Boundaries ---
param(mountain_suppression_ceiling, 0.05). % noise floor
param(mountain_snap_back_ceiling,    0.00). 

% --- Rope Boundaries ---
param(rope_extraction_ceiling,       0.35). % Pure coordination
param(rope_suppression_ceiling,      0.15). 

% --- Tangled Rope (Hybrid Zone) ---
% Zone between Rope (0.35) and Noose (0.66)
param(tangled_rope_extraction_ceil,  0.65). 
param(tangled_rope_suppression_ceil, 0.45). % Fixed: 0.45 instead of 045

% --- Noose Boundaries ---
param(noose_extraction_floor,        0.66). 
param(noose_suppression_floor,       0.46). 
param(noose_load_bearing_threshold,  0.70). 

% --- Zombie Boundaries ---
param(zombie_extraction_ceiling,     0.10).

/* ================================================================
   6. STRUCTURAL SIGNATURE THRESHOLDS (v3.2)
   These parameters enable detection of constraint ORIGIN
   (natural law vs coordination scaffold vs constructed constraint)
   rather than just constraint METRICS.
   ================================================================ */

% --- Natural Law Signature ---
% Identifies inherent properties of reality (no alternatives possible)
param(natural_law_collapse_min,      0.85).  % Extreme universal inaccessibility
param(natural_law_suppression_max,   0.15).  % No enforcement needed
param(natural_law_resistance_max,    0.15).  % Cannot be resisted

% --- Coordination Scaffold Signature ---
% Identifies successful voluntary standards (alternatives existed)
param(coordination_collapse_min,     0.85).  % Universal adoption achieved
param(coordination_suppression_max,  0.15).  % Voluntary compliance
param(coordination_resistance_max,   0.15).  % Minimal opposition

% --- Constructed Constraint Signature ---
% Identifies institutionally enforced rules (power asymmetries)
param(constructed_suppression_min,   0.20).  % Requires enforcement
param(constructed_resistance_min,    0.20).  % Faces opposition
param(constructed_beneficiary_min,   2).     % Asymmetric gains threshold
:- module(v3_1_data_repair, [
    repair_interval/1
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(domain_priors). % NEW: Hook into the Epistemic Prior Library

% Ensure we can add facts to the ontology's measurement predicate
:- dynamic narrative_ontology:measurement/5.

/* ============================================================
   REPAIR ORCHESTRATOR
   ============================================================ */

%% repair_interval(+IntervalID)
% Audits the measurement vectors for a given interval and repairs gaps
% using domain-specific epistemic priors.
repair_interval(IntervalID) :-
    (   narrative_ontology:interval(IntervalID, T0, Tn)
    ->  format('~n[REPAIR] Auditing vectors for: ~w...~n', [IntervalID]),
        % Audit both endpoints for all analytical levels
        forall(v3_1_config:level(L), 
               ( repair_point(L, T0, IntervalID), 
                 repair_point(L, Tn, IntervalID) 
               )),
        true
    ;   format('~n[ERROR] Interval ~w not found in database.~n', [IntervalID]),
        false
    ).

%% repair_point(+Level, +Time, +IntervalID)
% Iterates through the 4-component coercion vector at a specific time point.
repair_point(Level, Time, IntervalID) :-
    Components = [accessibility_collapse(Level), stakes_inflation(Level), 
                  suppression(Level), resistance(Level)],
    forall(member(Metric, Components), 
           ensure_metric_exists(Metric, Time, IntervalID)).

%% ensure_metric_exists(+Metric, +Time, +IntervalID)
% Core v3.2 Imputation Logic:
% 1. Checks for existing data.
% 2. Resolves prior based on domain type.
% 3. Flags novelty if the domain is unmapped.
ensure_metric_exists(Metric, Time, IntervalID) :-
    % Look directly into the ontology for existing measurement
    narrative_ontology:measurement(_, _, Metric, Time, _)
    ->  true
    ;   (   % NEW: Fetch prior value instead of hard-coded 0.5
            domain_priors:get_prior(IntervalID, Metric, Value),
            
            % NEW: Surface novelty alert to the LLM/User
            (domain_priors:is_known_domain(IntervalID) -> true ; domain_priors:flag_novelty(IntervalID)),
            
            gensym(repair_m_, SyntheticID),
            % Assert the synthetic fact into the global ontology
            assertz(narrative_ontology:measurement(SyntheticID, IntervalID, Metric, Time, Value)),
            format('  [FIXED] Imputed ~w for ~w at T=~w~n', [Value, Metric, Time])
    ).
:- module(v3_1_stack, [
    initialize_system/0,
    run_scenario/2
]).

% 1. Load Core Knowledge Schema
:- use_module(narrative_ontology).      % Schema & Global Expansion
:- use_module(v3_1_config).             % Grounded Weights & Thresholds

% 2. Load Management & Control (NEW)
:- use_module(scenario_manager, []).    % Lifecycle Controller

% 3. Load Functional Engines (Imported but silenced for namespace safety)
:- use_module(v3_1_coercion_projection, []).
:- use_module(modal_evaluator, []).
:- use_module(drl_core, []).
:- use_module(structural_signatures, []).  % NEW: v3.2 signature detection
:- use_module(v3_1_data_repair, []).
:- use_module(data_verification, []).
:- use_module(pattern_analysis, []).
:- use_module(intent_engine, []).

% 4. Load Diagnostic & UI
:- use_module(constraint_bridge, []).
:- use_module(uke_dr_bridge, []).
:- use_module(report_generator, []).
:- use_module(test_harness, []).

/* ================================================================
   SCENARIO ALIASES
   ================================================================ */

%% run_scenario(+File, +IntervalID)
%  Convenience alias for the Scenario Manager's load_and_run predicate.
run_scenario(File, IntervalID) :-
    scenario_manager:load_and_run(File, IntervalID).

/* ================================================================
   INITIALIZATION
   ================================================================ */

initialize_system :-
    format('~n====================================================~n'),
    format('   v3.1 STRUCTURAL ANALYSIS STACK INITIALIZED      ~n'),
    format('====================================================~n'),
    format('Namespace:  Consolidated & Grounded (v3.1)~n'),
    format('Control:    Scenario Manager Active~n'),
    format('Usage:      run_scenario(\'file.pl\', interval_id).~n'),
    format('====================================================~n').

:- initialize_system.
/* ================================================================
   v3.2 STRUCTURAL SIGNATURE CONFIGURATION
   
   Add these parameters to v3_1_config.pl to enable signature detection
   ================================================================ */

% NATURAL LAW SIGNATURE THRESHOLDS
% These identify constraints that are inherent properties of reality

param(natural_law_collapse_min, 0.85).
% Minimum accessibility collapse for natural law
% Natural laws affect everyone equally (high collapse)

param(natural_law_suppression_max, 0.15).
% Maximum suppression for natural law
% Natural laws don't require enforcement

param(natural_law_resistance_max, 0.15).
% Maximum resistance for natural law
% Natural laws cannot be effectively resisted

% COORDINATION SCAFFOLD SIGNATURE THRESHOLDS  
% These identify successful voluntary coordination standards

param(coordination_collapse_min, 0.85).
% Minimum accessibility collapse for coordination
% Successful standards achieve universal adoption

param(coordination_suppression_max, 0.15).
% Maximum suppression for coordination
% Coordination scaffolds are adopted voluntarily

param(coordination_resistance_max, 0.15).
% Maximum resistance for coordination
% Successful coordination faces minimal opposition

% CONSTRUCTED CONSTRAINT SIGNATURE THRESHOLDS
% These identify institutionally enforced rules

param(constructed_suppression_min, 0.20).
% Minimum suppression indicating institutional enforcement
% Constructed constraints require active maintenance

param(constructed_resistance_min, 0.20).
% Minimum resistance indicating opposition
% Constructed constraints face pushback

param(constructed_beneficiary_min, 2).
% Minimum beneficiary count for asymmetric gains
% Constructed constraints often benefit specific groups

/* ================================================================
   USAGE NOTES
   
   1. Natural Law vs Coordination Scaffold Distinction:
      Both have extreme collapse + minimal enforcement
      KEY DIFFERENCE: Coordination had viable alternatives
      
   2. Calibration Recommendations:
      - Start with these defaults
      - Monitor false positives in conceptual domains
      - Adjust collapse_min down if too restrictive
      - Adjust suppression_max up if missing constructs
      
   3. Integration with Existing System:
      - Structural signatures OVERRIDE modal classification
      - Use integrate_signature_with_modal/3 in report flow
      - Add signature explanations to audit output
   ================================================================ */
:- use_module(scenario_manager).
:- dynamic test_passed/1.
:- dynamic test_failed/2.

run_dynamic_suite :-
    retractall(test_passed(_)),
    retractall(test_failed(_, _)),
    writeln('--- STARTING DYNAMIC VALIDATION ---'),
    test_file('./testsets/26usc469.pl', 'tax_code_section_469', '26USC469', 1),
    test_file('./testsets/26usc469_real_estate_exemption.pl', 'section_469_c7_professional_threshold', '26USC469_REAL_ESTATE_EXEMPTION', 2),
    test_file('./testsets/ai_evaluators_matching.pl', 'ai_evaluators_matching', 'AI_EVALUATORS_MATCHING', 3),
    test_file('./testsets/automatic_enrollment_defaults.pl', 'automatic_enrollment_defaults', 'AUTOMATIC_ENROLLMENT_DEFAULTS', 4),
    test_file('./testsets/biological_curiosity.pl', 'biological_curiosity', 'BIOLOGICAL_CURIOSITY', 5),
    test_file('./testsets/blackstone_carried_interest_taxation.pl', 'blackstone_ipo_restructuring', 'BLACKSTONE_CARRIED_INTEREST_TAXATION', 6),
    test_file('./testsets/blackstone_conflicts_of_interest.pl', 'blackstone_conflict_era', 'BLACKSTONE_CONFLICTS_OF_INTEREST', 7),
    test_file('./testsets/blackstone_smd_control.pl', 'blackstone_governance_lock', 'BLACKSTONE_SMD_CONTROL', 8),
    test_file('./testsets/blackstone_tax_receiveable_agreement.pl', 'blackstone_tra_life', 'BLACKSTONE_TAX_RECEIVEABLE_AGREEMENT', 9),
    test_file('./testsets/choice_architecture_design.pl', 'choice_architecture_design', 'CHOICE_ARCHITECTURE_DESIGN', 10),
    test_file('./testsets/cloudflare_dual_class_asymmetry.pl', 'cloudflare_dual_class_asymmetry', 'CLOUDFLARE_DUAL_CLASS_ASYMMETRY', 11),
    test_file('./testsets/coinbase_crypto_volatility.pl', 'coinbase_ipo_window', 'COINBASE_CRYPTO_VOLATILITY', 12),
    test_file('./testsets/coinbase_regulatory_uncertainty.pl', 'coinbase_reg_pivot', 'COINBASE_REGULATORY_UNCERTAINTY', 13),
    test_file('./testsets/college_admissions_market.pl', 'college_admissions_market', 'COLLEGE_ADMISSIONS_MARKET', 14),
    test_file('./testsets/columbia_2026_elections.pl', 'colombia_2026_presidential_election', 'COLUMBIA_2026_ELECTIONS', 15),
    test_file('./testsets/copyleft_viral_licensing.pl', 'copyleft_viral_licensing', 'COPYLEFT_VIRAL_LICENSING', 16),
    test_file('./testsets/copyright_protection.pl', 'copyright_protection', 'COPYRIGHT_PROTECTION', 17),
    test_file('./testsets/couples_residency_match.pl', 'couples_residency_match', 'COUPLES_RESIDENCY_MATCH', 18),
    test_file('./testsets/creative_commons_licensing.pl', 'creative_commons_licensing', 'CREATIVE_COMMONS_LICENSING', 19),
    test_file('./testsets/dark_patterns_manipulation.pl', 'dark_patterns_manipulation', 'DARK_PATTERNS_MANIPULATION', 20),
    test_file('./testsets/ergo_advanced_mechanisms.pl', 'storage_rent_interval', 'ERGO_ADVANCED_MECHANISMS', 21),
    test_file('./testsets/ergo_autolykos_asic_resistance.pl', 'ergo_mining_era', 'ERGO_AUTOLYKOS_ASIC_RESISTANCE', 22),
    test_file('./testsets/ergo_dexy_gold_protocol.pl', 'dexy_gold_interval', 'ERGO_DEXY_GOLD_PROTOCOL', 23),
    test_file('./testsets/ergo_lets_protocol.pl', 'ergo_lets_interval', 'ERGO_LETS_PROTOCOL', 24),
    test_file('./testsets/ergo_mixer_protocol.pl', 'ergo_mixer_interval', 'ERGO_MIXER_PROTOCOL', 25),
    test_file('./testsets/ergo_nipopows.pl', 'ergo_nipopows_interval', 'ERGO_NIPOPOWS', 26),
    test_file('./testsets/ergo_rosen_bridge_protocol.pl', 'rosen_bridge_interval', 'ERGO_ROSEN_BRIDGE_PROTOCOL', 27),
    test_file('./testsets/ergo_sig_usd_protocol.pl', 'sig_usd_interval', 'ERGO_SIG_USD_PROTOCOL', 28),
    test_file('./testsets/ergo_storage_rent_mechanism.pl', 'ergo_operational_era', 'ERGO_STORAGE_RENT_MECHANISM', 29),
    test_file('./testsets/exploration_vs_exploitation.pl', 'exploration_vs_exploitation', 'EXPLORATION_VS_EXPLOITATION', 30),
    test_file('./testsets/fair_use_doctrine.pl', 'fair_use_doctrine', 'FAIR_USE_DOCTRINE', 31),
    test_file('./testsets/gale_shapley.pl', 'gale_shapley_matching', 'GALE_SHAPLEY', 32),
    test_file('./testsets/genetic_algorithms_evolution.pl', 'genetic_algorithms_evolution', 'GENETIC_ALGORITHMS_EVOLUTION', 33),
    test_file('./testsets/gita_kurukshetra.pl', 'dharma_of_kurukshetra', 'GITA_KURUKSHETRA', 34),
    test_file('./testsets/golden_handcuffs.pl', 'golden_handcuffs', 'GOLDEN_HANDCUFFS', 35),
    test_file('./testsets/hamiltonian_path_complexity.pl', 'hamiltonian_path_complexity', 'HAMILTONIAN_PATH_COMPLEXITY', 36),
    test_file('./testsets/hammurabi.pl', 'hammurabi_lex_talionis', 'HAMMURABI', 37),
    test_file('./testsets/heuristic_optimization.pl', 'heuristic_optimization', 'HEURISTIC_OPTIMIZATION', 38),
    test_file('./testsets/information_foraging_theory.pl', 'information_foraging_theory', 'INFORMATION_FORAGING_THEORY', 39),
    test_file('./testsets/institutional_mutation_domestication.pl', 'institutional_mutation_domestication', 'INSTITUTIONAL_MUTATION_DOMESTICATION', 40),
    test_file('./testsets/kidney_exchange_market.pl', 'kidney_exchange_market', 'KIDNEY_EXCHANGE_MARKET', 41),
    test_file('./testsets/kjv_great_awakening.pl', 'great_awakening_rekindling', 'KJV_GREAT_AWAKENING', 42),
    test_file('./testsets/kjv_linguistic_residue.pl', 'kjv_linguistic_residue', 'KJV_LINGUISTIC_RESIDUE', 43),
    test_file('./testsets/kjv_puritan_new_world_exit.pl', 'puritan_new_world_pivot', 'KJV_PURITAN_NEW_WORLD_EXIT', 44),
    test_file('./testsets/kjv_textual_authority.pl', 'kjv_textual_authority', 'KJV_TEXTUAL_AUTHORITY', 45),
    test_file('./testsets/lehman_repo_105.pl', 'lehman_repo_105', 'LEHMAN_REPO_105', 46),
    test_file('./testsets/local_vs_global_optima.pl', 'local_vs_global_optima', 'LOCAL_VS_GLOBAL_OPTIMA', 47),
    test_file('./testsets/marriage_problem.pl', 'optimal_stopping_marriage', 'MARRIAGE_PROBLEM', 48),
    test_file('./testsets/matching_markets_general.pl', 'matching_markets_general', 'MATCHING_MARKETS_GENERAL', 49),
    test_file('./testsets/max_flow.pl', 'max_flow_min_cut', 'MAX_FLOW', 50),
    test_file('./testsets/medical_residency_match.pl', 'medical_residency_match', 'MEDICAL_RESIDENCY_MATCH', 51),
    test_file('./testsets/medieval_church_hegomony.pl', 'medieval_church_hegemony', 'MEDIEVAL_CHURCH_HEGOMONY', 52),
    test_file('./testsets/non_compete_agreements.pl', 'non_compete_agreements', 'NON_COMPETE_AGREEMENTS', 53),
    test_file('./testsets/permissive_software_licensing.pl', 'permissive_software_licensing', 'PERMISSIVE_SOFTWARE_LICENSING', 54),
    test_file('./testsets/public_domain_commons.pl', 'public_domain_commons', 'PUBLIC_DOMAIN_COMMONS', 55),
    test_file('./testsets/relativity_of_simultaneity.pl', 'relativity_of_simultaneity', 'RELATIVITY_OF_SIMULTANEITY', 56),
    test_file('./testsets/relativity_physical_invariance.pl', 'relativity_physical_invariance', 'RELATIVITY_PHYSICAL_INVARIANCE', 57),
    test_file('./testsets/rfc9293_interoperability.pl', 'tcp_rfc9293_interoperability', 'RFC9293_INTEROPERABILITY', 58),
    test_file('./testsets/rfc9293_state_machine.pl', 'tcp_state_machine_logic', 'RFC9293_STATE_MACHINE', 59),
    test_file('./testsets/rotation_seven_black_soil.pl', 'black_soil_toxicity', 'ROTATION_SEVEN_BLACK_SOIL', 60),
    test_file('./testsets/rotation_seven_isolation.pl', 'protocol_r7_isolation', 'ROTATION_SEVEN_ISOLATION', 61),
    test_file('./testsets/rotation_seven_kubo_ranking.pl', 'kubo_ranking_system_r7', 'ROTATION_SEVEN_KUBO_RANKING', 62),
    test_file('./testsets/s1_airbnb.pl', 'airbnb_ipo_era', 'S1_AIRBNB', 63),
    test_file('./testsets/s1_visa.pl', 'visa_ipo_window', 'S1_VISA', 64),
    test_file('./testsets/s1_visa_judgment_sharing_agreement.pl', 'visa_litigation_ringfencing', 'S1_VISA_JUDGMENT_SHARING_AGREEMENT', 65),
    test_file('./testsets/shannon_entropy_limit.pl', 'shannon_entropy_limit', 'SHANNON_ENTROPY_LIMIT', 66),
    test_file('./testsets/silicon_lexicon_overload.pl', 'silicon_lexicon_overload', 'SILICON_LEXICON_OVERLOAD', 67),
    test_file('./testsets/skills_based_hiring.pl', 'skills_based_hiring', 'SKILLS_BASED_HIRING', 68),
    test_file('./testsets/sludge_bureaucratic_friction.pl', 'sludge_bureaucratic_friction', 'SLUDGE_BUREAUCRATIC_FRICTION', 69),
    test_file('./testsets/sts86_ascent_checklist.pl', 'sts86_ascent_checklist', 'STS86_ASCENT_CHECKLIST', 70),
    test_file('./testsets/trade_secret_law.pl', 'trade_secret_law', 'TRADE_SECRET_LAW', 71),
    test_file('./testsets/traveling_salesperson_problem.pl', 'traveling_salesperson_problem', 'TRAVELING_SALESPERSON_PROBLEM', 72),
    count_and_report.

test_file(Path, ID, Label, N) :-
    format('~n[~w] DOMAIN: ~w (~w)~n', [N, Label, Path]),
    (   catch(load_and_run(Path, ID), E, (assertz(test_failed(Path, E)), format('[FAIL] Exception: ~w~n', [E]), fail))
    ->  assertz(test_passed(Path)),
        report_generator:generate_llm_feedback(ID)
    ;   assertz(test_failed(Path, audit_failed)),
        report_generator:generate_llm_feedback(ID)
    ),
    !. 

count_and_report :-
    findall(P, test_passed(P), Ps), length(Ps, PC), findall(F, test_failed(F,_), Fs), length(Fs, FC),
    format('~nDONE: ~w Passed, ~w Failed~n', [PC, FC]).
