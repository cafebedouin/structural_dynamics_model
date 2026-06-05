% ============================================================================
% CONSTRAINT STORY: net_zero_stabilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_net_zero_stabilization, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: net_zero_stabilization
 *   human_readable: The Net Zero Carbon Constraint
 *   domain: scientific/political/economic
 *
 * SUMMARY:
 *   The net-zero carbon constraint represents the global effort to limit
 *   temperature rise by achieving a balance between emissions and removals of
 *   greenhouse gases. This requires a complex interplay of scientific
 *   understanding, political will, and economic feasibility. Before 2005, the
 *   scientific consensus was that global temperatures could be stabilized
 *   while still allowing for a small budget of CO2 emissions.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victims (powerless/trapped)
 *   - Developing Nations: Secondary victims (moderate/constrained)
 *   - Fossil Fuel Companies: Initial beneficiaries, some now arbitrageurs (institutional/arbitrage)
 *   - High Carbon Emitting Nations: Powerful actors, constrained by policy (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(net_zero_stabilization, 0.6).
domain_priors:suppression_score(net_zero_stabilization, 0.7).
domain_priors:theater_ratio(net_zero_stabilization, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(net_zero_stabilization, extractiveness, 0.6).
narrative_ontology:constraint_metric(net_zero_stabilization, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(net_zero_stabilization, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(net_zero_stabilization, tangled_rope).
narrative_ontology:human_readable(net_zero_stabilization, "The Net Zero Carbon Constraint").
narrative_ontology:topic_domain(net_zero_stabilization, "scientific/political/economic").

domain_priors:requires_active_enforcement(net_zero_stabilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(net_zero_stabilization, fossil_fuel_companies).
narrative_ontology:constraint_beneficiary(net_zero_stabilization, high_carbon_emitting_nations).
narrative_ontology:constraint_victim(net_zero_stabilization, future_generations).
narrative_ontology:constraint_victim(net_zero_stabilization, developing_nations).
narrative_ontology:constraint_victim(net_zero_stabilization, climate_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations are trapped by the consequences of past and present emissions, bearing the brunt of climate change impacts with limited means of escape.
constraint_indexing:constraint_classification(net_zero_stabilization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Developing nations face constraints in their development pathways due to the need to reduce emissions, while also being vulnerable to climate impacts. They benefit somewhat from technology transfer and adaptation aid, but bear a disproportionate burden.
constraint_indexing:constraint_classification(net_zero_stabilization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Fossil fuel companies initially benefited from the exploitation of carbon resources. Now, some benefit from carbon capture technologies and offsets, while experiencing arbitrage in markets and policy.
constraint_indexing:constraint_classification(net_zero_stabilization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% High carbon emitting nations initially benefited from industrialization. They are now constrained by climate policies, but also seek arbitrage through carbon markets and offsets. They wield powerful influence in climate negotiations.
constraint_indexing:constraint_classification(net_zero_stabilization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer recognizes the complex interplay of factors influencing the net-zero carbon constraint, including scientific understanding, political will, and economic feasibility.
constraint_indexing:constraint_classification(net_zero_stabilization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(net_zero_stabilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(net_zero_stabilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(net_zero_stabilization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(net_zero_stabilization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(net_zero_stabilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The constraint extracts resources and freedom of action from future generations and developing nations to benefit current generations and carbon-intensive industries. Suppression (0.7): Strong suppression arises from technological lock-in, vested interests, and the difficulty of coordinating global action. Theater Ratio (0.3): Some theatrical elements are present in climate policy, but there is also significant genuine effort toward mitigation and adaptation.
 *
 * PERSPECTIVAL GAP:
 *   Future generations see a snare, trapped by past emissions. Developing nations see a tangled rope, constrained by the need to develop sustainably while also adapting to climate impacts. Fossil fuel companies see a rope, as they adapt and seek arbitrage opportunities. High carbon emitting nations see a tangled rope, constrained by policy but also wielding power in negotiations. The analytical observer recognizes the complexity of the situation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's power, exit options, and relationship to the carbon constraint. Future generations, with no power or exit, experience maximum extraction. Fossil fuel companies, with power and arbitrage, experience benefit. Developing nations are victims due to being constrained. High carbon emitting nations are powerful, therefore see a lower impact from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The net-zero carbon constraint resolves the mandatrophy by encompassing the entire range of potential perspectives, from pure extraction to coordination. The key is to analyze each agent's position within the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_sensitivity,
    'What is the Earth''s climate sensitivity to increased CO2 concentrations?',
    'Improved climate models, paleoclimate data analysis, observations of recent warming trends.',
    'Higher climate sensitivity implies a smaller carbon budget and more stringent mitigation requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_sensitivity, empirical, 'Uncertainty in climate sensitivity').

omega_variable(
    carbon_cycle_feedbacks,
    'How will carbon cycle feedbacks (e.g., permafrost thaw, forest dieback) affect the remaining carbon budget?',
    'Improved earth system models, observations of carbon cycle dynamics, process-based understanding of feedback mechanisms.',
    'Positive feedbacks could further reduce the carbon budget, necessitating deeper and faster emissions cuts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_cycle_feedbacks, empirical, 'Uncertainty in carbon cycle feedbacks').

omega_variable(
    technological_feasibility,
    'What is the feasibility and scalability of carbon capture and storage (CCS) and other negative emissions technologies?',
    'Pilot projects, technological demonstration, economic analysis, life cycle assessments.',
    'If CCS proves infeasible or too expensive, the carbon budget will be further constrained, requiring more radical emissions reductions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_feasibility, empirical, 'Feasibility of negative emission technologies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(net_zero_stabilization, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(net__tr_t0, net_zero_stabilization, theater_ratio, 0, 0.2).
narrative_ontology:measurement(net__tr_t10, net_zero_stabilization, theater_ratio, 10, 0.3).
narrative_ontology:measurement(net__tr_t20, net_zero_stabilization, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(net__be_t0, net_zero_stabilization, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(net__be_t10, net_zero_stabilization, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(net__be_t20, net_zero_stabilization, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(net_zero_stabilization, global_infrastructure).
narrative_ontology:affects_constraint(net_zero_stabilization, global_carbon_market).
narrative_ontology:affects_constraint(net_zero_stabilization, paris_agreement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
