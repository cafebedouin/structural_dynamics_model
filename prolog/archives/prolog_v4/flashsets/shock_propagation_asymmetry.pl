% ============================================================================
% CONSTRAINT STORY: shock_propagation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shock_propagation_asymmetry, []).

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
 *   constraint_id: shock_propagation_asymmetry
 *   human_readable: The One-Way Crisis Valve
 *   domain: economic/logistical
 *
 * SUMMARY:
 *   The One-Way Crisis Valve describes a global market integration framework
 *   where positive gains are concentrated at the center (core economies,
 *   multinational corporations), while negative shocks are funneled to the
 *   periphery (periphery economies, global environmental commons). This
 *   creates a structural asymmetry where the periphery bears the costs of
 *   crises without the benefits of growth, and the core enjoys the benefits
 *   without the costs.
 *
 * KEY AGENTS:
 *   - Core Economies: Institutional/Arbitrage - Benefit from concentrating gains and externalizing shocks.
 *   - Periphery Economies: Powerless/Trapped - Absorb negative shocks without reciprocal benefit.
 *   - Multinational Corporations: Institutional/Arbitrage - Extract resources and concentrate profits, externalizing costs.
 *   - Global Environmental Commons: Powerless/Trapped - Absorbs environmental debt without recourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shock_propagation_asymmetry, 0.65).
domain_priors:suppression_score(shock_propagation_asymmetry, 0.7).
domain_priors:theater_ratio(shock_propagation_asymmetry, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shock_propagation_asymmetry, extractiveness, 0.65).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shock_propagation_asymmetry, tangled_rope).
narrative_ontology:human_readable(shock_propagation_asymmetry, "The One-Way Crisis Valve").
narrative_ontology:topic_domain(shock_propagation_asymmetry, "economic/logistical").

domain_priors:requires_active_enforcement(shock_propagation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, core_economies).
narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, multinational_corporations).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, periphery_economies).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, global_environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Periphery economies are trapped in a system where they absorb negative shocks without reciprocal benefit. Limited exit options due to debt dependencies and structural adjustment programs.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Core economies benefit from the concentration of positive gains and the externalization of negative shocks. They arbitrage the system, ensuring that they retain the advantages.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer recognizes the system as a tangled rope, where there is some coordination benefit (global trade, efficiency), but also asymmetric extraction. The global structure concentrates wealth and funnels risks.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The global environmental commons is a powerless and trapped victim of the one-way crisis valve. Environmental debt is pushed to the periphery without recourse.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Multinational corporations benefit from the ability to extract resources and concentrate profits in the core, while externalizing costs to the periphery. They arbitrage the global system to their advantage.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shock_propagation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shock_propagation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shock_propagation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The system extracts wealth and resources from the periphery, concentrates benefits in the core, and forces the periphery to absorb the costs of global crises. Suppression (0.70): High. The system actively suppresses alternatives, such as fair trade agreements, debt relief, and environmental regulations that would benefit the periphery. Theater Ratio (0.30): Low. The system is primarily functional, with limited performative elements. The main function is to concentrate gains and externalize shocks, not to create the appearance of fairness or equity.
 *
 * PERSPECTIVAL GAP:
 *   Core economies and multinational corporations see the system as a rope - a coordination mechanism that allows for global trade and economic growth. Periphery economies and the global environmental commons see the system as a snare - a trap that extracts wealth and resources while forcing them to absorb the costs of global crises. The analytical observer sees the system as a tangled rope - a hybrid of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural positions of the agents. Core economies and multinational corporations have high power and arbitrage options, so they experience low or negative effective extraction. Periphery economies and the global environmental commons have low power and trapped exit options, so they experience high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by recognizing that the system does provide some coordination benefits (global trade, efficiency). However, it also recognizes that the system is asymmetrically extractive, concentrating gains in the core and funnelling losses to the periphery. The tangled rope classification accurately captures this hybrid nature of the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_adjustment_alternatives,
    'Are there viable alternatives to structural adjustment programs that would allow periphery economies to escape dependency?',
    'Comparative analysis of alternative development models and their impact on periphery economies.',
    'If alternatives exist: the system is a tangled rope. If no alternatives: the system is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_adjustment_alternatives, empirical, 'Viability of alternatives to structural adjustment').

omega_variable(
    carbon_debt_valuation,
    'How should carbon debt be valued and redistributed to account for historical emissions by core economies?',
    'Development of a standardized methodology for valuing carbon debt and assigning responsibility.',
    'If carbon debt can be accurately valued: redistribution mechanisms can be implemented. If not: the periphery remains trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_debt_valuation, conceptual, 'Carbon debt valuation methodology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shock_propagation_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shoc_tr_t0, shock_propagation_asymmetry, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shoc_tr_t5, shock_propagation_asymmetry, theater_ratio, 5, 0.25).
narrative_ontology:measurement(shoc_tr_t10, shock_propagation_asymmetry, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(shoc_be_t0, shock_propagation_asymmetry, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(shoc_be_t5, shock_propagation_asymmetry, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(shoc_be_t10, shock_propagation_asymmetry, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shock_propagation_asymmetry, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
