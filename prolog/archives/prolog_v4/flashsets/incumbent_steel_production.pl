% ============================================================================
% CONSTRAINT STORY: incumbent_steel_production
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incumbent_steel_production, []).

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
 *   constraint_id: incumbent_steel_production
 *   human_readable: Incumbent Blast Furnace Steel Production Method
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The global steel industry is dominated by the blast furnace method, which
 *   requires high-grade iron ore and coking coal. This established method
 *   creates a constraint on the adoption of cleaner, more sustainable steel
 *   production technologies. The method benefits incumbent steel mills with
 *   established infrastructure but extracts from the environment through
 *   carbon emissions and other pollutants. This is a tangled rope, where
 *   there are benefits and harms involved.
 *
 * KEY AGENTS:
 *   - Integrated Steel Mills: Primary beneficiary (institutional/arbitrage) - benefits from established infrastructure and customer base.
 *   - Coal Mining Industry: Secondary beneficiary (powerful/constrained) - benefits from continued demand for coking coal.
 *   - Emerging Steel Technologies: Primary victim (moderate/constrained) - faces barriers to entry due to established infrastructure and economies of scale.
 *   - Environment: Ultimate victim (powerless/trapped) - suffers from carbon emissions and pollution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_steel_production, 0.6).
domain_priors:suppression_score(incumbent_steel_production, 0.7).
domain_priors:theater_ratio(incumbent_steel_production, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_steel_production, extractiveness, 0.6).
narrative_ontology:constraint_metric(incumbent_steel_production, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(incumbent_steel_production, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incumbent_steel_production, tangled_rope).
narrative_ontology:human_readable(incumbent_steel_production, "Incumbent Blast Furnace Steel Production Method").
narrative_ontology:topic_domain(incumbent_steel_production, "technological/economic").

domain_priors:requires_active_enforcement(incumbent_steel_production).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incumbent_steel_production, integrated_steel_mills).
narrative_ontology:constraint_beneficiary(incumbent_steel_production, coal_mining_industry).
narrative_ontology:constraint_victim(incumbent_steel_production, emerging_steel_technologies).
narrative_ontology:constraint_victim(incumbent_steel_production, environment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The environment is trapped by the continued reliance on blast furnace steel production due to its carbon emissions and pollution. The environment cannot exit and bears the brunt of the extractive process.
constraint_indexing:constraint_classification(incumbent_steel_production, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% Emerging steel technologies are constrained by the established infrastructure and economies of scale of blast furnace production, while also benefiting from ongoing research and development and potential future market disruption. They have some mobility, but face significant barriers to entry.
constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Integrated steel mills benefit from established infrastructure, supply chains, and customer relationships. They have arbitrage opportunities due to their global reach and influence over the steel market. This perspective experiences the constraint as coordination due to established standards and procedures.
constraint_indexing:constraint_classification(incumbent_steel_production, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The coal mining industry is constrained by the long term decline of the steel industry but benefits in the short term from the continued use of coking coal in blast furnaces. They exert pressure through powerful lobbying groups to continue support of the blast furnace methods.
constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees the historical dominance of the blast furnace method as a piton. The technology has been surpassed by emerging technologies, but it maintains its dominance through inertia and its installed base.
constraint_indexing:constraint_classification(incumbent_steel_production, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_steel_production_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incumbent_steel_production, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incumbent_steel_production, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incumbent_steel_production, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incumbent_steel_production, TR),
    TR >= 0.70.

:- end_tests(incumbent_steel_production_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The blast furnace method extracts significantly from the environment through carbon emissions and other pollutants. Suppression (0.7): The method suppresses the adoption of cleaner, more sustainable steel production technologies through its established infrastructure and economies of scale. Theater ratio (0.3): There is limited performative activity associated with the blast furnace method. Compliance with basic regulations is necessary, but the overall theater is low.
 *
 * PERSPECTIVAL GAP:
 *   The environment views the incumbent steel production as a Snare, as they cannot escape the burden of pollution and resources consumption. Integrated steel mills view the system as coordination, represented as a Rope, since it benefits them and integrates into their already established system. Emerging steel technologies view the dominance of the blast furnace method as a Tangled Rope because it impedes their growth and market opportunities, yet benefits them through ongoing R&D.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent steel mills benefit because the system is already set up. The coal industry benefits from continued demand of coking coal. Emerging technologies are stifled through dominance of blast furnaces and the environment is harmed by the large amounts of emissions. The Coal mining industry d value is higher because although powerful, in the long term the decline of steel will impact the industry heavily.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved because even though there are both negative (environmental damage, stifled innovation) and positive aspects (established economic engine, raw materials requirements), the system leans more towards extraction than coordination overall, leading to its designation as a Tangled Rope. If there were an emphasis on green tech and an emphasis on helping transition away from coal, then the system would be considered to be more coordination and classified as such.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_disruption_rate,
    'What is the rate at which emerging steel technologies will disrupt the dominance of the blast furnace method?',
    'Analysis of investment trends, research and development breakthroughs, and market adoption rates of new technologies.',
    'Higher disruption rate: the blast furnace method will decline rapidly, leading to stranded assets and job losses. Lower disruption rate: the blast furnace method will persist for longer, exacerbating environmental problems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_disruption_rate, empirical, 'Rate of technology disruption in steel production.').

omega_variable(
    policy_intervention_level,
    'What is the level of policy intervention required to accelerate the transition to cleaner steel production methods?',
    'Assessment of the effectiveness of different policy instruments, such as carbon taxes, subsidies for green technologies, and regulations on emissions.',
    'Higher intervention level: faster transition to cleaner steel production, but potential for higher costs and economic disruption. Lower intervention level: slower transition to cleaner steel production, but lower costs and less disruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intervention_level, preference, 'Level of policy intervention required to transition to cleaner steel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incumbent_steel_production, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incu_tr_t0, incumbent_steel_production, theater_ratio, 0, 0.2).
narrative_ontology:measurement(incu_tr_t10, incumbent_steel_production, theater_ratio, 10, 0.3).
narrative_ontology:measurement(incu_tr_t20, incumbent_steel_production, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(incu_be_t0, incumbent_steel_production, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(incu_be_t10, incumbent_steel_production, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(incu_be_t20, incumbent_steel_production, base_extractiveness, 20, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incumbent_steel_production, resource_allocation).
narrative_ontology:affects_constraint(incumbent_steel_production, iron_ore_supply).
narrative_ontology:affects_constraint(incumbent_steel_production, global_carbon_emissions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
