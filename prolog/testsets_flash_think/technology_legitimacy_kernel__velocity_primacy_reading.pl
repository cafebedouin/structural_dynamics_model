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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Technology Legitimacy: Velocity Primacy Reading (Climate Mitigation)
 *   domain: Energy Policy/Climate Mitigation/Technology Governance
 *
 * SUMMARY:
 *   This constraint represents the 'velocity primacy' reading of the
 *   'technology legitimacy' kernel in climate mitigation. It asserts that a
 *   technology is legitimate if and only if it can be deployed at scale
 *   within the remaining carbon budget timeline (e.g., 2030/2050 targets).
 *   This framework prioritizes speed and scale, channeling resources and
 *   policy support towards rapidly deployable solutions like renewables,
 *   while marginalizing slower technologies such as nuclear or carbon
 *   capture. The constraint functions as a Tangled Rope, coordinating efforts
 *   towards rapid decarbonization but extracting from those technologies and
 *   stakeholders who cannot meet its stringent timeline criteria.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.7).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.75).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Technology Legitimacy: Velocity Primacy Reading (Climate Mitigation)").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "Energy Policy/Climate Mitigation/Technology Governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '9452e7df-d69d-4ee0-bf7a-7483ba3313da').
narrative_ontology:cs_kernel_codification('9452e7df-d69d-4ee0-bf7a-7483ba3313da', formalized).
narrative_ontology:cs_authority_grounding('9452e7df-d69d-4ee0-bf7a-7483ba3313da', expertise).
narrative_ontology:cs_interpretation_layer_present('9452e7df-d69d-4ee0-bf7a-7483ba3313da').
narrative_ontology:cs_reading_relation('9452e7df-d69d-4ee0-bf7a-7483ba3313da', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_reading_relation('9452e7df-d69d-4ee0-bf7a-7483ba3313da', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('9452e7df-d69d-4ee0-bf7a-7483ba3313da', foundational, carbon_budget_is_binding).
narrative_ontology:cs_axiom_status(carbon_budget_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('9452e7df-d69d-4ee0-bf7a-7483ba3313da', carbon_budget_is_binding, empirically_contingent).
narrative_ontology:cs_axiom('9452e7df-d69d-4ee0-bf7a-7483ba3313da', foundational, speed_is_the_primary_metric).
narrative_ontology:cs_axiom_status(speed_is_the_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('9452e7df-d69d-4ee0-bf7a-7483ba3313da', speed_is_the_primary_metric, instrumental).
narrative_ontology:cs_reference_frame('9452e7df-d69d-4ee0-bf7a-7483ba3313da', urgent_decarbonization_imperative).
narrative_ontology:cs_drift_state('9452e7df-d69d-4ee0-bf7a-7483ba3313da', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9452e7df-d69d-4ee0-bf7a-7483ba3313da', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, policymakers_and_funders).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_energy_proponents).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, carbon_capture_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, environmental_justice_advocates).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, rapid_deployment_is_key).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_urgency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide the scientific basis for carbon budgets and timelines, which underpins the 'velocity primacy' criterion. Their models and projections define the urgency and the targets that drive this constraint.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_scientists_and_modelers, agenda_setter,
    analytical, generational, analytical, global).

% Develop and deploy technologies like solar and wind power, which are often characterized by relatively fast deployment cycles. They benefit from policies and funding streams that prioritize speed and scale within the carbon budget timeline.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Advocate for nuclear power, which offers dispatchable, low-carbon energy but typically has very long planning and construction timelines. They are marginalized or excluded by the 'velocity primacy' criterion, facing reduced funding and policy support.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_energy_proponents, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_energy_proponents, excluded).

% Work on technologies for carbon capture, utilization, and storage (CCUS), which often involve complex infrastructure projects with significant lead times. They struggle to meet the rapid deployment demands of the 'velocity primacy' reading and face similar marginalization as nuclear proponents.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, carbon_capture_developers, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, carbon_capture_developers, excluded).

% Responsible for maintaining stable and reliable electricity grids. The rapid, large-scale deployment of intermittent renewable energy sources, prioritized by 'velocity primacy', imposes significant costs and technical challenges on grid management and stability.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, immediate, constrained, national).

% Implement and fund climate mitigation strategies. They benefit from the clear, actionable framework provided by 'velocity primacy' for allocating resources and demonstrating progress towards climate targets, often gaining political capital from perceived rapid action.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, policymakers_and_funders, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, policymakers_and_funders, beneficiary).

% Monitor the social and environmental impacts of energy projects, particularly on vulnerable communities. They often bear the diffuse costs of rushed, large-scale deployments that may overlook local concerns or exacerbate existing inequalities, even if these projects contribute to rapid decarbonization.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, environmental_justice_advocates, observer,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, environmental_justice_advocates, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Focuses climate mitigation efforts, funding, and policy support on technologies that can deliver rapid, large-scale carbon reductions within critical timelines (e.g., 2030/2050 targets), thereby coordinating action towards urgent decarbonization.
% TRANSFER_FUNCTION: Transfers legitimacy, financial resources, and political capital to fast-deployable technologies (e.g., solar, wind) and away from slower ones (e.g., nuclear, large-scale carbon capture), while imposing costs and challenges on grid stability and potentially on local communities affected by rapid deployment.
% ABSENT_VOICES: Proponents of technologies with long lead times (e.g., advanced nuclear, next-generation carbon capture) who argue for their long-term necessity and unique benefits, as well as communities who bear the brunt of rushed, large-scale deployments without adequate planning or consultation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the criteria for climate technology legitimacy would broaden significantly. Funding and policy would shift to a more diverse portfolio of technologies, potentially leading to slower overall decarbonization but with different risk profiles, greater emphasis on reliability, safety, or long-term sustainability, and a reorganization of energy investment priorities.
% FOUNDING_PROBLEM: The urgent need to rapidly decarbonize global energy systems to meet critical climate targets (e.g., 1.5°C limit) within a shrinking global carbon budget, leading to a prioritization of deployment speed and scale.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC reports, national climate targets, and the broad scientific consensus on the urgency of climate action and the shrinking carbon budget corroborate the live status of this founding problem. Policymakers and climate scientists widely attest to the imperative for rapid deployment.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.7) stems from the strict 'if and only if' condition, which imposes significant costs on technologies and stakeholders that do not align with rapid deployment, regardless of other benefits. Suppression (0.75) is high because this reading actively excludes alternative criteria for legitimacy and marginalizes technologies that don't fit. The moderate theater ratio (0.4) reflects instances where projects might be rushed or framed as 'fast' for political optics, even if their true impact or integration is less straightforward. Accessibility collapse is high for slow technologies (0.8), as this framework effectively closes off their path to legitimacy and funding. Resistance (0.6) comes from proponents of marginalized technologies and those concerned about the side effects of rapid, potentially less integrated, deployment.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of 'velocity primacy' (e.g., some policymakers, renewable developers) perceive this as a necessary Rope for urgent climate action, coordinating efforts efficiently. However, those marginalized by this reading (e.g., nuclear proponents, grid operators) experience it as a Snare, extracting resources and legitimacy from their preferred solutions or imposing significant operational costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate scientists and modelers, by defining the carbon budget and timelines, act as agenda-setters. Renewable energy developers and policymakers/funders are primary beneficiaries, as their interests align with the constraint's emphasis on speed. Nuclear and carbon capture developers, along with grid operators, are victims, bearing the costs of marginalization or increased operational complexity. Environmental justice advocates act as observers, but also bear diffuse costs from potentially rushed projects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''technology_legitimacy_kernel'', or merely a policy preference within a broader, unified framework?',
    'Analysis of policy documents, funding allocations, and public discourse to identify whether ''velocity primacy'' functions as a foundational, mutually exclusive criterion for legitimacy, or as one of several negotiable priorities.',
    'If a distinct reading, it confirms the kernel decomposition. If a mere preference, the kernel itself might be a single, more complex constraint (e.g., a Tangled Rope balancing multiple criteria).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the structural distinctness of this reading within the ''technology legitimacy'' kernel.').

omega_variable(
    unintended_consequences_of_velocity,
    'Does the prioritization of deployment velocity lead to significant, unacknowledged negative externalities (e.g., grid instability, environmental justice impacts from rushed projects) that undermine overall climate mitigation goals?',
    'Empirical studies tracking the long-term system costs and social impacts of rapidly deployed technologies, compared to scenarios with more balanced criteria.',
    'If significant negative externalities are found, the effective extractiveness of this constraint would be higher than currently measured, and its coordination function would be compromised, potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unintended_consequences_of_velocity, empirical, 'Assesses whether the focus on speed creates hidden costs or compromises other climate goals.').

omega_variable(
    long_term_technology_exclusion,
    'Is the exclusion or marginalization of slower-to-deploy technologies (e.g., advanced nuclear, next-gen CCUS) a temporary measure or a permanent structural feature of this reading, potentially hindering long-term decarbonization pathways?',
    'Longitudinal analysis of policy and funding trends, and expert elicitation on the future role of these technologies under ''velocity primacy'' assumptions.',
    'If permanent, the suppression and extractiveness for these technologies are higher and more entrenched, potentially leading to a ''Piton'' classification for the excluded technologies themselves, as their function atrophies due to lack of support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_technology_exclusion, preference, 'Examines the long-term implications of prioritizing velocity on technology diversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tech_tr_t6, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(tech_tr_t18, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(tech_tr_t30, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tech_be_t6, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(tech_be_t18, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(tech_be_t30, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tech_su_t6, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(tech_su_t18, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(tech_su_t30, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'technology_legitimacy_kernel'. Other readings include 'reliability_primacy_reading' and 'precautionary_reading', each with distinct ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
