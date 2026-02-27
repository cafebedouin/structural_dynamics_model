% ============================================================================
% CONSTRAINT STORY: pele_microreactor_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pele_microreactor_deployment, []).

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
 *   constraint_id: pele_microreactor_deployment
 *   human_readable: Pele Mobile Micro-Reactor Deployment Protocol
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   The constraint is the system of rules, security protocols, and political
 *   agreements governing the deployment of a transportable nuclear reactor
 *   (Project Pele) to power forward military bases. This deployment aims to
 *   provide reliable energy to remote military locations, but also presents
 *   significant risks to local populations, potential adversaries, and
 *   environmental integrity. The deployment is a complex issue with varied
 *   perspectives.
 *
 * KEY AGENTS:
 *   - Forward Military Bases: Primary beneficiary (institutional/arbitrage)
 *   - Local Populations Near Deployment Sites: Primary victim (powerless/trapped)
 *   - Potential Adversaries: Secondary victim (moderate/constrained)
 *   - Environmental Integrity: Organized victim (organized/mobile)
 *   - US Military Industrial Complex: Beneficiary (institutional/arbitrage)
 *   - Analytical Observer: Assesses the geopolitical implications (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pele_microreactor_deployment, 0.55).
domain_priors:suppression_score(pele_microreactor_deployment, 0.45).
domain_priors:theater_ratio(pele_microreactor_deployment, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pele_microreactor_deployment, extractiveness, 0.55).
narrative_ontology:constraint_metric(pele_microreactor_deployment, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(pele_microreactor_deployment, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pele_microreactor_deployment, tangled_rope).
narrative_ontology:human_readable(pele_microreactor_deployment, "Pele Mobile Micro-Reactor Deployment Protocol").
narrative_ontology:topic_domain(pele_microreactor_deployment, "technological/geopolitical").

domain_priors:requires_active_enforcement(pele_microreactor_deployment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pele_microreactor_deployment, forward_military_bases).
narrative_ontology:constraint_beneficiary(pele_microreactor_deployment, us_military_industrial_complex).
narrative_ontology:constraint_victim(pele_microreactor_deployment, local_populations_near_deployment_sites).
narrative_ontology:constraint_victim(pele_microreactor_deployment, potential_adversaries).
narrative_ontology:constraint_victim(pele_microreactor_deployment, environmental_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Local populations bear the risk of accidents and environmental contamination, with limited recourse or exit options.
constraint_indexing:constraint_classification(pele_microreactor_deployment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% Adversaries are constrained by the need to react to the deployment, but can also benefit from the distraction and potential to exploit vulnerabilities.
constraint_indexing:constraint_classification(pele_microreactor_deployment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The US military industrial complex benefits from the expansion of capabilities and increased contracts.
constraint_indexing:constraint_classification(pele_microreactor_deployment, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Environmental advocacy groups can organize and protest, but are constrained by the political and security interests at stake, and the technical difficulties of monitoring. Exit options are to lobby for stronger environmental safeguards or to highlight environmental incidents when they occur. 
constraint_indexing:constraint_classification(pele_microreactor_deployment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Forward military bases benefit from reliable power supply, enabling increased operational capabilities.
constraint_indexing:constraint_classification(pele_microreactor_deployment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the deployment as a mixed blessing, with both benefits and risks for various stakeholders.
constraint_indexing:constraint_classification(pele_microreactor_deployment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pele_microreactor_deployment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pele_microreactor_deployment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pele_microreactor_deployment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pele_microreactor_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pele_microreactor_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The local populations and environment bear a substantial portion of the risk in this deployment. Suppression (0.45): Moderate. The US military will need to overcome international and local resistance to successfully deploy this technology. Theater ratio (0.30): Low. The security protocols and actual implementation of the deployment are more important than mere public perception.
 *
 * PERSPECTIVAL GAP:
 *   The deployment is viewed differently by various stakeholders. Local populations view the deployment with great trepidation, due to the potential risks. Potential adversaries view the deployment as a provocation, but also potentially a vulnerability. The US military views the deployment as a necessary step to maintain global power projection. An analytical observer would see a complex problem with significant trade-offs.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are based on each agent's relationship to the extraction flow. The local populations are the primary target, and bear the greatest risk. The US military benefits from the deployment, and are thus the primary beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling a pure coordination mechanism by acknowledging the asymmetric distribution of risks and benefits. While the US Military's intent may be coordination (reliable power), the actual implementation extracts from other stakeholders. The Tangled Rope type appropriately balances these two factors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accident_probability,
    'What is the probability of a major accident during transportation or operation?',
    'Monte Carlo simulations, stress testing, historical data from similar deployments',
    'High probability: Snare classification reinforced. Low probability: Tangled Rope classification reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accident_probability, empirical, 'Probability of major accident').

omega_variable(
    proliferation_risk,
    'What is the risk of proliferation of nuclear materials or technology?',
    'Security analysis, safeguards implementation, international agreements',
    'High risk: Snare classification reinforced. Low risk: Tangled Rope classification reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_risk, empirical, 'Risk of nuclear proliferation').

omega_variable(
    environmental_impact,
    'What is the long-term environmental impact of the deployment?',
    'Environmental impact assessments, long-term monitoring, lifecycle analysis',
    'High impact: Snare classification reinforced. Low impact: Tangled Rope classification reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_impact, empirical, 'Long-term environmental impact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pele_microreactor_deployment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pele_tr_t0, pele_microreactor_deployment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pele_tr_t5, pele_microreactor_deployment, theater_ratio, 5, 0.3).
narrative_ontology:measurement(pele_tr_t10, pele_microreactor_deployment, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(pele_be_t0, pele_microreactor_deployment, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pele_be_t5, pele_microreactor_deployment, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(pele_be_t10, pele_microreactor_deployment, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pele_microreactor_deployment, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
