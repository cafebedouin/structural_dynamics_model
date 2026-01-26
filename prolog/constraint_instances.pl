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
    snare,
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
    snare,
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
