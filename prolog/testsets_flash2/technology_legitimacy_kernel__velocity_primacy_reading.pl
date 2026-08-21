% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Climate Technology Legitimacy: Velocity Primacy Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the legitimacy of climate mitigation technologies
 *   based on their ability to be deployed at scale within the remaining
 *   carbon budget timeline (e.g., 2030/2050 targets). It is one reading of
 *   the broader 'technology_legitimacy_kernel'. This reading prioritizes
 *   speed and immediate impact, favoring technologies like solar and wind,
 *   while marginalizing those with longer development or deployment cycles,
 *   such as nuclear power. The classification as a Tangled Rope reflects its
 *   genuine coordination function (focusing efforts on urgent climate action)
 *   coupled with asymmetric extraction (marginalizing certain technologies
 *   and their advocates, and imposing costs on grid operators).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.65).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.7).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Climate Technology Legitimacy: Velocity Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '6b7c8802-01a5-4ba1-9386-82a5fe23c6ec').
narrative_ontology:cs_kernel_codification('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', distributed).
narrative_ontology:cs_authority_grounding('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', practice).
narrative_ontology:cs_interpretation_layer_present('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec').
narrative_ontology:cs_reading_relation('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', technology_legitimacy_kernel__reliability_primacy_reading, influences).
narrative_ontology:cs_reading_relation('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', foundational, speed_of_deployment_is_paramount).
narrative_ontology:cs_axiom_status(speed_of_deployment_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', speed_of_deployment_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', foundational, carbon_budget_is_fixed_and_urgent).
narrative_ontology:cs_axiom_status(carbon_budget_is_fixed_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', carbon_budget_is_fixed_and_urgent, empirically_contingent).
narrative_ontology:cs_reference_frame('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', urgent_carbon_budget_response).
narrative_ontology:cs_drift_state('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6b7c8802-01a5-4ba1-9386-82a5fe23c6ec', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_activists_velocity_aligned).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_intermittency_burdened).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from policy and investment prioritization due to their rapid deployment potential. Their technologies are favored by this reading, leading to increased market access and funding.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Advocate for technologies that can deliver immediate, large-scale carbon reductions, aligning with the urgency emphasized by this reading. Their policy positions gain legitimacy and influence.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_activists_velocity_aligned, beneficiary,
    organized, generational, identity_locked, global).

% Bear the cost of marginalization and reduced investment, as nuclear projects typically have long construction timelines that conflict with the 'velocity primacy' criterion. They face an uphill battle for policy support.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_advocates, payer,
    powerful, generational, constrained, national).

% Face increased operational complexity and costs due to the rapid integration of intermittent renewable sources, which are prioritized by this reading. They must invest heavily in grid modernization and storage solutions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_intermittency_burdened, payer,
    institutional, immediate, constrained, regional).

% Structurally excluded from the 'legitimate climate mitigation technology' discourse under this reading, as their technologies do not contribute to carbon reduction. They would argue for continued use of fossil fuels with carbon capture, but this reading prioritizes speed over such solutions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, fossil_fuel_lobby, excluded,
    institutional, biographical, constrained, global).

% Are responsible for setting climate policy and allocating resources. This reading provides a clear framework for their decisions, prioritizing fast-deploying technologies and shaping the national energy mix.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, policy_makers_climate_portfolios, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates policy, investment, and public discourse around a shared urgency for rapid climate action, focusing on technologies that can deliver immediate, large-scale carbon reductions within tight timelines.
% TRANSFER_FUNCTION: Transfers political capital, investment, and public support towards rapidly deployable renewable energy technologies, and away from slower-to-deploy or more complex solutions like nuclear power.
% ABSENT_VOICES: Advocates for technologies with long development cycles (e.g., advanced nuclear, fusion) or those with significant legacy costs (e.g., carbon capture and storage for fossil fuels) are marginalized. They would argue for a broader portfolio of solutions, but their concerns are subordinated to the urgency of the carbon budget.
% DISAPPEARANCE_RATIONALE: If this reading of legitimacy vanished, the criteria for climate technology funding and policy would immediately broaden. Technologies currently marginalized (e.g., nuclear) would gain renewed consideration, and the energy transition strategy would likely become more diversified, potentially slowing overall deployment but increasing reliability or reducing long-term risks.
% FOUNDING_PROBLEM: The urgent need to reduce greenhouse gas emissions within a rapidly closing carbon budget, as defined by international climate agreements and scientific consensus.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) and numerous national scientific bodies corroborate the urgency of the carbon budget and the need for rapid emissions reductions. This is widely accepted outside of specific technology advocacy groups.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the opportunity costs imposed on technologies that don't meet the velocity criterion, and the economic burden on grid operators to manage the intermittency of favored renewables. Suppression (0.7) is high because this reading actively shapes policy, funding, and public discourse, making it difficult for marginalized technologies to gain traction or for grid operators to advocate for alternative, slower-paced, but more stable, energy mixes. Theater ratio (0.2) is relatively low, as the urgency of climate change means there's genuine effort behind the policy, but some 'greenwashing' or oversimplification of complex energy challenges may occur.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable energy developers and climate activists, this constraint is a necessary Rope, coordinating urgent action. From the perspective of nuclear advocates and grid operators, it functions as a Snare, unfairly excluding viable solutions and imposing significant costs due to an overly narrow definition of 'legitimacy'. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy developers and velocity-aligned climate activists are clear beneficiaries, as their interests and technologies are prioritized. Nuclear power advocates and grid operators (burdened by intermittency) are victims, bearing the costs of this prioritization. Policy makers act as agenda-setters, implementing the framework. The fossil fuel lobby is excluded from the legitimate discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_budget_accuracy,
    'How accurate and robust are the remaining carbon budget timelines (2030/2050 targets) that ground this reading''s urgency?',
    'Ongoing climate science research, re-evaluation of emission pathways, and independent audits of carbon accounting models.',
    'If the carbon budget is found to be less urgent or more flexible, the ''velocity primacy'' criterion would weaken, potentially allowing for a broader portfolio of technologies to be considered legitimate. If found to be more urgent, the constraint would harden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_budget_accuracy, empirical, 'Uncertainty in the scientific basis for the urgency driving this reading.').

omega_variable(
    intermittency_cost_underestimation,
    'Does this reading systematically underestimate the long-term costs and technical challenges associated with managing grid intermittency from rapidly deployed renewables?',
    'Comprehensive, independent economic and engineering analyses of grid integration costs, energy storage requirements, and system reliability in high-renewable grids, compared to projections.',
    'If costs are significantly underestimated, the ''extraction'' from grid operators is higher than currently measured, potentially shifting the constraint towards a Snare for those seats. It would also strengthen the ''reliability_primacy_reading'' sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermittency_cost_underestimation, empirical, 'Potential for hidden costs of rapid renewable deployment.').

omega_variable(
    kernel_reading_divergence,
    'Is this constraint a genuine ''tangled_rope'' coordinating urgent climate action, or is it a ''snare'' for technologies that do not fit its narrow ''velocity primacy'' definition?',
    'Analysis of policy outcomes in jurisdictions that adopt this reading: if the energy mix becomes overly concentrated and grid stability suffers, it suggests a snare. If a balanced, rapid transition occurs, it supports the tangled rope framing.',
    'Reclassification to ''snare'' would indicate that the coordination function is primarily a cover for the exclusion of competing technologies, with significant negative consequences for energy diversity and resilience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Ambiguity between genuine coordination and exclusionary extraction within the ''velocity primacy'' framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'technology_legitimacy_kernel'. This 'velocity_primacy_reading' prioritizes speed of deployment, while 'reliability_primacy_reading' prioritizes grid stability and 'precautionary_reading' prioritizes bounded worst-case risks. Each reading defines a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
