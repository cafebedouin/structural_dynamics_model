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
 *   the broader 'technology_legitimacy_kernel'. This
 *   'velocity_primacy_reading' prioritizes speed and scale of deployment,
 *   favoring technologies like solar and wind, while implicitly marginalizing
 *   those with longer lead times, such as nuclear power, or those with
 *   significant unaddressed risks. The constraint operates as a Tangled Rope,
 *   coordinating rapid climate action but extracting costs from technologies
 *   and stakeholders that do not fit the 'velocity' criterion.
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
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Climate Technology Legitimacy: Velocity Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '2fe4aaf8-f44c-4ae0-a685-47f048ed1269').
narrative_ontology:cs_kernel_codification('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', formalized).
narrative_ontology:cs_authority_grounding('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', expertise).
narrative_ontology:cs_interpretation_layer_present('2fe4aaf8-f44c-4ae0-a685-47f048ed1269').
narrative_ontology:cs_reading_relation('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', foundational, speed_of_deployment_is_paramount).
narrative_ontology:cs_axiom_status(speed_of_deployment_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', speed_of_deployment_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', foundational, carbon_budget_is_fixed_and_urgent).
narrative_ontology:cs_axiom_status(carbon_budget_is_fixed_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', carbon_budget_is_fixed_and_urgent, empirically_contingent).
narrative_ontology:cs_reference_frame('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', urgent_carbon_budget_response).
narrative_ontology:cs_drift_state('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', contemporary_energy_transition, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2fe4aaf8-f44c-4ae0-a685-47f048ed1269', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_activists_velocity_focused).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from policy frameworks that prioritize rapid deployment, as their technologies (solar, wind) are well-suited for quick scaling. This reading legitimizes their solutions and attracts investment.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Advocate for immediate and rapid deployment of any technology that can meet carbon targets quickly, often prioritizing speed over other concerns. This reading aligns with their urgent calls for action.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_activists_velocity_focused, beneficiary,
    moderate, immediate, constrained, global).

% Bear the cost of this constraint as their technologies, despite low-carbon output, are typically slow to deploy due to long construction timelines and regulatory hurdles. This reading marginalizes their solutions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_advocates, payer,
    organized, generational, constrained, national).

% Face increased costs and operational challenges in managing grids with a high penetration of intermittent renewable energy sources, which are favored by this reading. They must invest in storage and transmission upgrades.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, biographical, constrained, national).

% Are responsible for setting climate policy and allocating resources. This reading provides a clear metric for technology selection, simplifying complex decisions but potentially overlooking other critical factors.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, policy_makers_climate_focused, agenda_setter,
    institutional, generational, constrained, national).

% Would argue that rapid deployment without sufficient consideration of long-term risks (e.g., waste, ecological impact) is irresponsible. Their concerns are sidelined by the urgency emphasized in this reading.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates climate mitigation efforts by providing a clear, time-bound criterion for selecting and prioritizing technologies, focusing resources on solutions that can deliver rapid emissions reductions.
% TRANSFER_FUNCTION: Transfers legitimacy, funding, and policy support towards technologies capable of rapid deployment (e.g., renewables) and away from those with longer lead times (e.g., nuclear), shifting investment and political capital.
% ABSENT_VOICES: Advocates for technologies with long-term reliability or strong precautionary principles are marginalized. They would argue for a more balanced approach that considers grid stability, energy security, and irreversible risks, but their concerns are deemed secondary to deployment speed.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the criteria for climate technology legitimacy would become much broader and more contested. Technologies currently favored would lose their primary justification, and those currently marginalized would gain a stronger footing. Policy decisions would become more complex, and investment flows would diversify significantly.
% FOUNDING_PROBLEM: The urgent need to meet global carbon budget targets (e.g., 1.5°C limit) within a rapidly closing window, requiring immediate and large-scale deployment of climate solutions.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and international bodies (e.g., IPCC) consistently corroborate the urgency of the carbon budget timeline, providing external validation for the founding problem's continued relevance. Energy economists also highlight the need for rapid transitions to avoid catastrophic climate impacts.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).

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
 *   The extractiveness (0.65) stems from the opportunity costs imposed on technologies that are effective but slow to deploy, and the additional costs borne by grid operators to integrate intermittent sources. Suppression (0.70) is high because this reading actively suppresses alternative criteria for legitimacy, such as reliability or precaution, through policy and funding mechanisms. The theater ratio (0.20) is relatively low, as the focus on rapid deployment is a genuine, if narrow, function. The metrics show a slight increase in extractiveness and suppression towards the peak of the carbon budget timeline (2030-2035), reflecting intensified pressure, then a slight decrease as some targets are met or become less urgent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable energy developers and velocity-focused climate activists, this constraint is a necessary Rope, coordinating urgent action. For nuclear power advocates and grid operators, it functions as a Snare, extracting resources and marginalizing their contributions due to the emphasis on speed. Policy makers, as agenda-setters, experience it as a Tangled Rope, balancing the coordination of rapid deployment with the political and economic costs imposed on marginalized sectors.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy developers and velocity-focused climate activists are beneficiaries, as their interests align with rapid deployment. Nuclear power advocates and grid operators are victims, bearing the costs of marginalization and grid instability, respectively. Policy makers are agenda-setters, actively enforcing this criterion. Precautionary advocates are excluded, as their concerns are not prioritized by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not yet mandatrophic, as the founding problem (urgent carbon budget targets) is still live. However, if the carbon budget targets are missed, or if the negative consequences of rapid, uncritical deployment (e.g., grid instability, unmanaged waste) become severe, the constraint could become mandatrophic. The current classification as a Tangled Rope acknowledges both its coordination function and its asymmetric extraction, preventing it from being mislabeled as a pure Rope (ignoring victims) or a pure Snare (ignoring the genuine urgency of climate action).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deployment_speed_vs_long_term_cost,
    'Does prioritizing deployment speed (velocity primacy) inadvertently increase long-term system costs or create new vulnerabilities (e.g., grid instability, supply chain dependencies) that undermine overall climate goals?',
    'Comprehensive lifecycle cost analysis and system-level resilience modeling comparing velocity-optimized pathways with more balanced approaches over a 50-100 year horizon.',
    'If long-term costs or vulnerabilities are significantly higher, the effective extractiveness of this constraint would be re-evaluated upward, potentially shifting its classification towards a Snare due to hidden future costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployment_speed_vs_long_term_cost, empirical, 'Trade-off between rapid deployment and long-term system resilience/cost-effectiveness.').

omega_variable(
    carbon_budget_timeline_flexibility,
    'Is the carbon budget timeline (e.g., 2030/2050 targets) a fixed, immutable constraint, or is there flexibility in how it is interpreted and achieved, allowing for a broader portfolio of technologies?',
    'Scientific re-evaluation of climate tipping points and socio-economic modeling of alternative mitigation pathways, alongside political negotiation on target adjustments.',
    'If the timeline is found to be more flexible, the ''velocity primacy'' reading would lose some of its foundational justification, reducing its suppressive force and potentially allowing other readings (e.g., reliability, precaution) to gain influence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_budget_timeline_flexibility, conceptual, 'The degree of rigidity in the carbon budget timeline as a policy driver.').

omega_variable(
    natural_law_vs_policy_choice,
    'Is the ''velocity primacy'' a natural consequence of the physics of climate change and carbon budgets, or is it a policy choice that prioritizes certain values (speed) over others (reliability, precaution)?',
    'Analysis of how different scientific interpretations of climate risk translate into policy recommendations, and the role of value judgments in selecting mitigation strategies.',
    'If it''s primarily a policy choice, the ''emerges_naturally'' aspect would be false, and the constraint''s legitimacy would rest more heavily on its coordination function and less on its ''inevitability'', potentially increasing its perceived extractiveness for those it marginalizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_policy_choice, conceptual, 'Whether velocity primacy is a natural law or a constructed policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t2020, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(tech_tr_t2025, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement(tech_tr_t2030, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2030, 0.2).
narrative_ontology:measurement(tech_tr_t2035, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2035, 0.22).
narrative_ontology:measurement(tech_tr_t2040, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2040, 0.2).
narrative_ontology:measurement(tech_tr_t2045, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2045, 0.18).
narrative_ontology:measurement(tech_tr_t2050, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2050, 0.15).

% Extraction over time
narrative_ontology:measurement(tech_be_t2020, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(tech_be_t2025, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement(tech_be_t2030, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2030, 0.62).
narrative_ontology:measurement(tech_be_t2035, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2035, 0.65).
narrative_ontology:measurement(tech_be_t2040, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2040, 0.67).
narrative_ontology:measurement(tech_be_t2045, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2045, 0.68).
narrative_ontology:measurement(tech_be_t2050, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2050, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t2020, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(tech_su_t2025, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2025, 0.63).
narrative_ontology:measurement(tech_su_t2030, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2030, 0.67).
narrative_ontology:measurement(tech_su_t2035, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2035, 0.7).
narrative_ontology:measurement(tech_su_t2040, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2040, 0.68).
narrative_ontology:measurement(tech_su_t2045, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2045, 0.65).
narrative_ontology:measurement(tech_su_t2050, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2050, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_investment_incentives).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_regulatory_hurdles).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'technology_legitimacy_kernel'. The other readings are 'reliability_primacy_reading' and 'precautionary_reading', each with distinct beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
