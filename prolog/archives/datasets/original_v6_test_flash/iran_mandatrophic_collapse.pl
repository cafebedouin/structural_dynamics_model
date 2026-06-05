% ============================================================================
% CONSTRAINT STORY: iran_mandatrophic_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_mandatrophic_collapse, []).

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
 *   constraint_id: iran_mandatrophic_collapse
 *   human_readable: Iranian Mandatrophy (The Water-Economic Choke)
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   Mandatrophy in Iran describes the systemic wasting away of ecological and
 *   economic resilience caused by the rigid prioritization of the
 *   "Revolutionary Mandate" (regional proxy funding, nuclear ambition, and
 *   ideological self-sufficiency) over the organic "margins" of the state
 *   (aquifers, cultural heritage, economic diversity). This leads to a
 *   self-reinforcing cycle of environmental degradation, economic decline,
 *   and political instability. The government's commitment to regional
 *   influence and nuclear program diverts resources from essential services
 *   and sustainable development, exacerbating water scarcity and economic
 *   hardship.
 *
 * KEY AGENTS:
 *   - Iranian Farmers: Primary victim (powerless/trapped) - bear the brunt of water scarcity and economic hardship.
 *   - Average Iranian Citizens: Secondary victim (moderate/constrained) - experience a mix of benefits and costs but are increasingly vulnerable.
 *   - Revolutionary Guard: Primary beneficiary (institutional/arbitrage) - control resources and maintain power through the 'Revolutionary Mandate.'
 *   - Nuclear Program Administrators: Beneficiary (institutional/constrained) - benefit from resource allocation towards their program, at the expense of other sectors.
 *   - Ministry of Agriculture: Degraded institution (institutional/constrained) - unable to effectively support Iranian farmers due to resource constraints.
 *   - Regional Water Resources: Environmental Victim (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_mandatrophic_collapse, 0.75).
domain_priors:suppression_score(iran_mandatrophic_collapse, 0.8).
domain_priors:theater_ratio(iran_mandatrophic_collapse, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, extractiveness, 0.75).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_mandatrophic_collapse, tangled_rope).
narrative_ontology:human_readable(iran_mandatrophic_collapse, "Iranian Mandatrophy (The Water-Economic Choke)").
narrative_ontology:topic_domain(iran_mandatrophic_collapse, "political/economic/technological").

domain_priors:requires_active_enforcement(iran_mandatrophic_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, revolutionary_guard).
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, nuclear_program_administrators).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, iranian_farmers).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, iranian_citizens).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, regional_water_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Iranian farmer is trapped by water scarcity, government policies favoring water-intensive industries, and lack of alternative livelihoods. They experience the mandatrophic collapse as a snare, with no exit options and increasing vulnerability.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The average Iranian citizen experiences a mix of benefits (national pride, perceived security) and costs (economic hardship, environmental degradation). They are constrained by political realities and economic opportunities but have limited exit options.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The IRGC benefits from the 'Revolutionary Mandate,' controlling resources and maintaining power. They see the system as a rope, facilitating their activities and providing them with arbitrage opportunities. The immediate time horizon reflects their focus on maintaining short-term control.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Ministry of Agriculture's original mandate to support Iranian farmers is undermined by the prioritization of the 'Revolutionary Mandate.' The Ministry becomes a piton, a degraded institution unable to fulfill its intended purpose effectively. Performs a largely performative function.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a global perspective, the analytical observer sees a tangled rope: a system where the benefits of the 'Revolutionary Mandate' are increasingly outweighed by the costs of environmental degradation and economic decline. Long time horizon as the problem unfolds over decades and potentially centuries, and the solution will also take generations.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_mandatrophic_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_mandatrophic_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_mandatrophic_collapse, TR),
    TR >= 0.70.

:- end_tests(iran_mandatrophic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. Significant resources are diverted from sustainable development to support the 'Revolutionary Mandate,' leading to severe environmental degradation and economic hardship. Suppression (0.80): High. Political repression and lack of economic opportunities limit the ability of citizens to challenge the system or seek alternative livelihoods. Theater Ratio (0.60): Moderate. While there are efforts to address environmental problems, they are often performative and insufficient to address the root causes of the mandatrophic collapse.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives illustrate the structural asymmetries of the constraint. The Iranian farmer experiences the system as a snare, while the Revolutionary Guard sees it as a rope. The average citizen is caught in a tangled web of benefits and costs. The Ministry of Agriculture is a degraded institution, and the analytical observer sees a complex system spiraling towards collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect the structural position of each agent. The Iranian farmer, with no exit options, experiences maximum extraction. The Revolutionary Guard, with control over resources, experiences minimal extraction. The average citizen experiences a mix of extraction and benefits. The Ministry of Agriculture experiences a high theater ratio, reflecting the performative nature of its activities.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the 'Revolutionary Mandate' creates a system where short-term political goals are prioritized over long-term sustainability. The system is not necessarily intended to cause harm, but its structure leads to unintended consequences. Corrective steps would involve restructuring incentives and allowing resources to flow where they're needed rather than artificially forcing them toward the 'Revolutionary Mandate'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    water_resource_regeneration_capacity,
    'What is the regeneration capacity of Iranian aquifers under current and future climate scenarios?',
    'Hydrological modeling, geological surveys, climate change projections',
    'Determines the long-term sustainability of water resources and the feasibility of alternative agricultural practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(water_resource_regeneration_capacity, empirical, 'Assessment of Iranian aquifers'' regeneration capacity').

omega_variable(
    political_regime_change_probability,
    'What is the probability of a significant political shift in Iran that could alter the prioritization of the ''Revolutionary Mandate''?',
    'Political analysis, sociological studies, scenario planning',
    'A regime change could lead to a shift in resource allocation and environmental policies, potentially mitigating the mandatrophic collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_regime_change_probability, preference, 'Analysis of political regime change probability').

omega_variable(
    economic_diversification_potential,
    'What is the potential for economic diversification in Iran away from water-intensive industries?',
    'Economic modeling, technological assessment, industry analysis',
    'Diversification could reduce pressure on water resources and create alternative livelihoods for affected populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_diversification_potential, empirical, 'Potential for economic diversification away from water-intensive industries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_mandatrophic_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_tr_t0, iran_mandatrophic_collapse, theater_ratio, 0, 0.3).
narrative_ontology:measurement(iran_tr_t10, iran_mandatrophic_collapse, theater_ratio, 10, 0.5).
narrative_ontology:measurement(iran_tr_t20, iran_mandatrophic_collapse, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(iran_be_t0, iran_mandatrophic_collapse, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(iran_be_t10, iran_mandatrophic_collapse, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(iran_be_t20, iran_mandatrophic_collapse, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_mandatrophic_collapse, resource_allocation).
narrative_ontology:affects_constraint(iran_mandatrophic_collapse, middle_east_water_wars).
narrative_ontology:affects_constraint(iran_mandatrophic_collapse, iran_nuclear_proliferation).

% DUAL FORMULATION NOTE:
% The Iranian Mandatrophic Collapse is both a consequence of and a contributing factor to regional instability and nuclear proliferation efforts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
