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
