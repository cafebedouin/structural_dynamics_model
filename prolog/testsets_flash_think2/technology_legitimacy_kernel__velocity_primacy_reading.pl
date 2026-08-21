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
 *   domain: Energy Policy/Climate Mitigation/Technology Governance
 *
 * SUMMARY:
 *   This constraint represents the 'velocity primacy' reading of the broader
 *   'technology legitimacy' kernel in climate mitigation. It asserts that a
 *   technology is legitimate if and only if it can be deployed at scale
 *   within the remaining carbon budget timeline (e.g., 2030/2050 targets).
 *   This reading prioritizes speed of deployment, favoring technologies like
 *   solar and wind, while marginalizing those with longer development or
 *   construction times, such as nuclear power or carbon capture and storage.
 *   The constraint functions as a Tangled Rope, coordinating efforts around
 *   rapid deployment but extracting from technologies and stakeholders that
 *   do not meet the velocity criterion.
 *
 * KEY AGENTS:
 *   - climate_activists_velocity_focus: Agenda setter/Beneficiary (organized/constrained)
 *   - fast_deploying_renewables_industry: Beneficiary (powerful/arbitrage)
 *   - nuclear_power_advocates: Payer/Excluded (organized/constrained)
 *   - grid_operators: Payer (institutional/trapped)
 *   - carbon_capture_storage_advocates: Payer/Excluded (moderate/constrained)
 *   - policymakers_climate_mitigation: Agenda setter (institutional/constrained)
 *   - precautionary_advocates: Excluded/Observer (organized/analytical)
 *   - reliability_advocates: Excluded/Observer (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.7).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.8).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Climate Technology Legitimacy: Velocity Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "Energy Policy/Climate Mitigation/Technology Governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '540c4fbb-754f-41c5-8131-fb1782b667b5').
narrative_ontology:cs_kernel_codification('540c4fbb-754f-41c5-8131-fb1782b667b5', formalized).
narrative_ontology:cs_authority_grounding('540c4fbb-754f-41c5-8131-fb1782b667b5', expertise).
narrative_ontology:cs_interpretation_layer_present('540c4fbb-754f-41c5-8131-fb1782b667b5').
narrative_ontology:cs_reading_relation('540c4fbb-754f-41c5-8131-fb1782b667b5', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('540c4fbb-754f-41c5-8131-fb1782b667b5', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('540c4fbb-754f-41c5-8131-fb1782b667b5', foundational, speed_of_deployment_is_paramount).
narrative_ontology:cs_axiom_status(speed_of_deployment_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('540c4fbb-754f-41c5-8131-fb1782b667b5', speed_of_deployment_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('540c4fbb-754f-41c5-8131-fb1782b667b5', foundational, carbon_budget_is_fixed_deadline).
narrative_ontology:cs_axiom_status(carbon_budget_is_fixed_deadline, holdable).
narrative_ontology:cs_axiom_grounding('540c4fbb-754f-41c5-8131-fb1782b667b5', carbon_budget_is_fixed_deadline, empirically_contingent).
narrative_ontology:cs_reference_frame('540c4fbb-754f-41c5-8131-fb1782b667b5', urgent_decarbonization_pathway).
narrative_ontology:cs_drift_state('540c4fbb-754f-41c5-8131-fb1782b667b5', contemporary_climate_policy_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('540c4fbb-754f-41c5-8131-fb1782b667b5', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, fast_deploying_renewables_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_activists_velocity_focus).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, carbon_capture_storage_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for rapid, large-scale deployment of existing renewable technologies, often prioritizing speed over other considerations like long-term grid stability or novel technology development. They shape public discourse and policy priorities.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_activists_velocity_focus, agenda_setter,
    organized, biographical, constrained, global).

% Benefits from policies and funding streams aligned with rapid deployment. Their technologies (solar, wind, batteries) are favored by this legitimacy criterion, leading to increased market share and investment.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, fast_deploying_renewables_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Advocate for nuclear power as a reliable, carbon-free baseload, but face marginalization due to long construction timelines and high upfront costs, which conflict with the velocity criterion. They bear the cost of reduced policy support and investment.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_advocates, payer,
    organized, generational, constrained, national).

% Responsible for maintaining grid stability and reliability. The rapid, intermittent deployment favored by the velocity criterion creates significant challenges for grid management, requiring costly upgrades and operational adjustments.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, immediate, trapped, national).

% Advocate for CCS as a necessary technology for hard-to-abate sectors, but face skepticism and reduced funding due to its current deployment velocity and cost, which often falls outside the 'within budget timeline' criterion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, carbon_capture_storage_advocates, payer,
    moderate, generational, constrained, global).

% Translate climate goals into policy, often adopting the velocity primacy as a guiding principle for funding, subsidies, and regulatory frameworks, thereby shaping the technological landscape.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, policymakers_climate_mitigation, agenda_setter,
    institutional, biographical, constrained, national).

% Focus on bounding worst-case failure modes and legacy costs of technologies. Their concerns about novel or large-scale interventions (e.g., geoengineering, unproven CCS) are often sidelined by the urgency-driven velocity criterion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_advocates, excluded,
    organized, generational, analytical, global).

% Prioritize dispatchable, baseload-capable generation for grid stability. Their arguments for technologies like nuclear or advanced gas with CCS are often deprioritized in favor of faster, but intermittent, renewable deployment.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, reliability_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Focuses climate mitigation efforts on technologies that can deliver rapid emissions reductions at scale within the remaining carbon budget timeline, thereby coordinating investment and policy around a specific set of solutions.
% TRANSFER_FUNCTION: Transfers legitimacy, funding, and political will towards fast-deploying technologies (e.g., solar, wind, batteries) and away from slower-to-deploy but potentially valuable alternatives (e.g., nuclear, carbon capture and storage).
% ABSENT_VOICES: Proponents of technologies that are slow but reliable (e.g., nuclear) or those focused on long-term safety and reversibility (precautionary advocates) are structurally marginalized or excluded from the primary policy discourse driven by this criterion.
% DISAPPEARANCE_RATIONALE: If this legitimacy criterion vanished overnight, the criteria for climate technology legitimacy would fragment, leading to a wider array of technologies being pursued, potentially slower overall emissions reductions in the short term, but possibly more robust and diverse long-term solutions. Funding and policy would shift dramatically.
% FOUNDING_PROBLEM: The urgent need to reduce greenhouse gas emissions within a rapidly closing global carbon budget (e.g., 2030/2050 targets) to avoid catastrophic climate change impacts.
% FOUNDING_PROBLEM_CORROBORATION: The urgency and the carbon budget timeline are widely corroborated by IPCC reports, scientific consensus on climate change, and international climate agreements (e.g., Paris Agreement targets). This corroboration comes from scientific bodies and international organizations outside the direct beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.7) reflects the significant redirection of resources and political will away from slower technologies, imposing costs on their proponents and on grid operators managing intermittency. Suppression (0.8) is high because this criterion actively marginalizes alternatives through policy, funding, and public discourse. Theater ratio (0.2) is low as the debate is genuinely urgent and consequential, with real-world impacts on investment and emissions. Accessibility collapse (0.7) is high for technologies that cannot meet the velocity criterion, as policy support and market access diminish. Resistance (0.75) is substantial from advocates of marginalized technologies and those prioritizing other criteria.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of the velocity primacy (e.g., climate activists, renewables industry) experience this as a necessary coordination mechanism for urgent climate action, seeing its benefits as outweighing any costs. Conversely, advocates for nuclear or CCS, and grid operators, experience it as an extractive force that unfairly disadvantages their solutions or imposes significant operational burdens, despite their potential contributions to decarbonization.
 *
 * DIRECTIONALITY LOGIC:
 *   Fast-deploying renewables and velocity-focused climate activists are clear beneficiaries, as the constraint directs resources and legitimacy towards their preferred solutions. Nuclear power advocates, grid operators, and CCS advocates are victims, bearing the costs of marginalization, increased operational complexity, or reduced funding. Policymakers act as agenda setters, implementing policies that enforce this criterion. Precautionary and reliability advocates are excluded, as their concerns are deprioritized.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the significant extraction from marginalized technologies) or a Snare (which would ignore the genuine coordination function of focusing urgent climate action). It highlights the inherent trade-offs and asymmetric impacts of prioritizing velocity in climate mitigation, even if the underlying goal is widely shared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    velocity_primacy_kernel_reading,
    'Is this constraint a genuine, objective requirement for climate mitigation, or a specific normative reading of the ''technology legitimacy'' kernel that prioritizes velocity over other values?',
    'Comparative analysis of climate mitigation outcomes under different legitimacy criteria (e.g., velocity vs. reliability vs. precaution) over a longer time horizon (e.g., 2100), assessing trade-offs and their ethical implications.',
    'If it''s confirmed as a specific normative reading, its classification as a Tangled Rope is reinforced, highlighting the value-laden choices embedded in ''legitimacy''. If it were an objective requirement, it would lean towards Mountain, but its beneficiaries/victims contradict that.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(velocity_primacy_kernel_reading, conceptual, 'This constraint is the ''velocity primacy'' reading of the ''technology legitimacy'' kernel.').

omega_variable(
    reliability_primacy_alternative_impact,
    'How would the classification and stakeholder positions change if the ''reliability primacy'' reading of the kernel were adopted instead?',
    'Counterfactual analysis of policy and investment flows under a reliability-first framework, identifying shifts in beneficiary/victim sets and extraction patterns for technologies like nuclear and dispatchable renewables.',
    'Nuclear power advocates would likely become beneficiaries, grid operators would see reduced costs, and fast-deploying intermittent renewables might become payers or face reduced support, leading to a different constraint type (likely still Tangled Rope but with different beneficiaries/victims and a different balance of extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliability_primacy_alternative_impact, conceptual, 'Impact of adopting the ''reliability primacy'' reading.').

omega_variable(
    precautionary_reading_alternative_impact,
    'How would the classification and stakeholder positions change if the ''precautionary reading'' of the kernel were adopted instead?',
    'Counterfactual analysis of policy and investment flows under a precautionary framework, identifying shifts in beneficiary/victim sets and extraction patterns, particularly for novel or high-risk technologies.',
    'Technologies with high legacy costs or uncertain failure modes (e.g., some CCS, geoengineering) would become victims or be excluded, while established, low-risk renewables might remain beneficiaries, but with a stronger emphasis on safety and reversibility. This could lead to a different constraint type or a shift in the balance of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precautionary_reading_alternative_impact, conceptual, 'Impact of adopting the ''precautionary reading''.').

omega_variable(
    scale_definition_ambiguity,
    'What constitutes ''at scale'' deployment within the carbon budget timeline, and is this definition consistently applied across technologies and regions?',
    'Detailed, technology-specific and region-specific modeling of deployment pathways and their actual emissions reduction impact, compared against the carbon budget, alongside a review of policy definitions.',
    'Inconsistent or overly optimistic definitions of ''at scale'' could lead to misallocation of resources, underestimating the true extraction from marginalized technologies, or overstating the coordination function of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_definition_ambiguity, empirical, 'Ambiguity in defining ''at scale'' deployment and its consistent application.').


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
narrative_ontology:measurement(tech_tr_t2035, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2035, 0.2).
narrative_ontology:measurement(tech_tr_t2040, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2040, 0.2).
narrative_ontology:measurement(tech_tr_t2045, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2045, 0.2).
narrative_ontology:measurement(tech_tr_t2050, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2050, 0.2).

% Extraction over time
narrative_ontology:measurement(tech_be_t2020, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(tech_be_t2025, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2025, 0.64).
narrative_ontology:measurement(tech_be_t2030, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(tech_be_t2035, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2035, 0.7).
narrative_ontology:measurement(tech_be_t2040, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2040, 0.7).
narrative_ontology:measurement(tech_be_t2045, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2045, 0.7).
narrative_ontology:measurement(tech_be_t2050, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2050, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t2020, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(tech_su_t2025, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2025, 0.75).
narrative_ontology:measurement(tech_su_t2030, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2030, 0.8).
narrative_ontology:measurement(tech_su_t2035, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2035, 0.8).
narrative_ontology:measurement(tech_su_t2040, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2040, 0.8).
narrative_ontology:measurement(tech_su_t2045, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2045, 0.8).
narrative_ontology:measurement(tech_su_t2050, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2050, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, national_climate_targets).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_subsidies).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_regulatory_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
