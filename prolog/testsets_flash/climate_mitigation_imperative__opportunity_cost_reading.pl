% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Imperative: Opportunity Cost Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'opportunity_cost_reading' of the broader
 *   'climate_mitigation_imperative' kernel. It asserts that climate
 *   mitigation efforts must prioritize technologies that deliver the fastest
 *   carbon reduction per dollar invested. Within this framework, nuclear
 *   power, due to its high capital intensity and long deployment timelines,
 *   is seen as a net-harmful diversion of resources that could be more
 *   effectively deployed in faster-to-market renewable energy projects. This
 *   reading actively seeks to exclude or de-prioritize nuclear from
 *   mitigation portfolios.
 *
 * KEY AGENTS:
 *   - renewable_energy_advocates: Primary beneficiary (institutional/arbitrage) — benefits from capital redirection
 *   - climate_activists: Primary beneficiary (organized/mobile) — aligns with rapid decarbonization goals
 *   - nuclear_industry: Primary target (institutional/constrained) — bears exclusion and capital diversion
 *   - pro_nuclear_policymakers: Primary target (powerful/constrained) — faces political and funding challenges
 *   - energy_system_planners: Agenda setter (institutional/analytical) — tasked with optimizing mitigation strategies
 *   - fossil_fuel_industry: Excluded (institutional/trapped) — benefits from any delay in mitigation, but not directly from this constraint's internal dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.4).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Imperative: Opportunity Cost Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, 'b3317260-db7a-4989-b5e7-a78a0a21e539').
narrative_ontology:cs_kernel_codification('b3317260-db7a-4989-b5e7-a78a0a21e539', implicit).
narrative_ontology:cs_authority_grounding('b3317260-db7a-4989-b5e7-a78a0a21e539', expertise).
narrative_ontology:cs_reading_relation('b3317260-db7a-4989-b5e7-a78a0a21e539', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3317260-db7a-4989-b5e7-a78a0a21e539', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('b3317260-db7a-4989-b5e7-a78a0a21e539', foundational, mitigation_speed_is_paramount).
narrative_ontology:cs_axiom_status(mitigation_speed_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b3317260-db7a-4989-b5e7-a78a0a21e539', mitigation_speed_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('b3317260-db7a-4989-b5e7-a78a0a21e539', foundational, capital_efficiency_is_key).
narrative_ontology:cs_axiom_status(capital_efficiency_is_key, holdable).
narrative_ontology:cs_axiom_grounding('b3317260-db7a-4989-b5e7-a78a0a21e539', capital_efficiency_is_key, empirically_contingent).
narrative_ontology:cs_reference_frame('b3317260-db7a-4989-b5e7-a78a0a21e539', fastest_carbon_reduction_per_dollar).
narrative_ontology:cs_drift_state('b3317260-db7a-4989-b5e7-a78a0a21e539', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b3317260-db7a-4989-b5e7-a78a0a21e539', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_activists).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, pro_nuclear_policymakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote policies and investments that prioritize renewable energy sources, aligning with the 'fastest deployment per dollar' principle. They benefit from the redirection of capital and political will away from nuclear projects.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Advocate for urgent and effective climate action, often supporting strategies that emphasize rapid deployment of proven, cost-effective technologies. They see nuclear as a slow and expensive distraction from immediate decarbonization goals.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_activists, beneficiary,
    organized, generational, mobile, global).

% Develops, builds, and operates nuclear power plants. They face significant financial and political headwinds due to the perception of high costs and long timelines, leading to reduced investment and policy support. They argue nuclear is essential for baseload power and energy security.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% Advocate for nuclear power as a critical component of a low-carbon energy mix, citing its reliability and dispatchability. They struggle to secure funding and political consensus against the 'opportunity cost' argument, facing pressure to divert resources to renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, pro_nuclear_policymakers, payer,
    powerful, biographical, constrained, national).

% Responsible for designing and implementing national or regional energy strategies to meet climate targets. They must balance various factors, including cost, speed, reliability, and public acceptance, and are influenced by the 'opportunity cost' argument in their resource allocation decisions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, energy_system_planners, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from any delays or inefficiencies in climate mitigation efforts, regardless of the specific technology choices. While not directly involved in the nuclear vs. renewables debate, their long-term interests are served by the internal divisions within the climate mitigation community.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fossil_fuel_industry, excluded,
    institutional, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the allocation of financial and political capital towards climate mitigation strategies that yield the fastest and most cost-effective carbon reductions, thereby solving the collective action problem of rapid decarbonization.
% TRANSFER_FUNCTION: Transfers investment capital, policy support, and public attention away from nuclear power projects and towards renewable energy and other faster-deploying, lower-capital-intensity mitigation solutions.
% ABSENT_VOICES: Advocates for advanced nuclear technologies (e.g., small modular reactors) who argue that their cost and timeline profiles are fundamentally different from traditional large-scale nuclear, and that the 'opportunity cost' argument unfairly penalizes innovation. Their voices are often marginalized in the current debate focused on existing nuclear paradigms.
% DISAPPEARANCE_RATIONALE: If this 'opportunity cost' reading vanished, the discourse around nuclear power's role in climate mitigation would fundamentally shift. Capital allocation for energy projects would be re-evaluated, potentially leading to increased investment in nuclear, and a different mix of technologies in national energy portfolios. The political landscape of climate policy would be significantly altered.
% FOUNDING_PROBLEM: The urgent need for rapid and cost-effective decarbonization to avert catastrophic climate change, coupled with limited financial resources and time.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on climate change and the IPCC reports corroborate the urgency of the founding problem. Economic analyses from independent research institutions (e.g., Lazard's LCOE studies, IEA reports) corroborate the cost and deployment challenges of traditional nuclear power relative to renewables, supporting the 'opportunity cost' framing, though the interpretation of these data remains contested.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates resources towards rapid climate mitigation (a collective action problem) but does so by extracting from and suppressing nuclear development. Extractiveness (0.65) is high due to the significant capital and political resources diverted from nuclear. Suppression (0.4) is moderate, reflecting active policy and funding mechanisms that de-prioritize nuclear, but not outright bans. Theater ratio is low (0.2) as the constraint's proponents genuinely believe in its core logic. Resistance (0.7) is high from the nuclear industry and its allies. Accessibility collapse (0.3) is low because alternative energy sources are abundant, but the specific path of nuclear is constrained.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable energy advocates, this constraint is a necessary Rope, efficiently directing resources to the most impactful solutions. From the nuclear industry's perspective, it is a Snare, unfairly excluding a viable low-carbon option based on a narrow set of metrics. Energy system planners, as agenda-setters, navigate this tension, attempting to balance various objectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy advocates and climate activists are beneficiaries (d near 0.0) as the constraint directs capital and policy support towards their preferred solutions. The nuclear industry and pro-nuclear policymakers are victims (d near 1.0) as they face active exclusion and resource diversion. Energy system planners are agenda-setters, balancing competing demands. The fossil fuel industry is excluded, benefiting from any overall mitigation delay but not directly from this constraint's specific dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (rapid climate mitigation) is very much live. The contest is over the *means* to achieve that mandate, specifically whether nuclear power is an asset or an opportunity cost. The classification as Tangled Rope reflects the active coordination function alongside the asymmetric extraction from nuclear, preventing mislabeling it as a pure Snare (which would imply the coordination story is entirely cover) or a pure Rope (which would ignore the identifiable victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''opportunity_cost_reading'' of the ''climate_mitigation_imperative'' kernel, or is it a different constraint?',
    'Analysis of policy documents and advocacy statements to confirm the explicit framing of nuclear as an opportunity cost.',
    'If misidentified, the classification of nuclear as a ''victim'' and renewables as ''beneficiaries'' would be incorrect, leading to a different constraint type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as the ''opportunity_cost_reading'' of the ''climate_mitigation_imperative'' kernel.').

omega_variable(
    cost_benefit_analysis_accuracy,
    'Are the capital intensity and deployment timelines for nuclear power accurately assessed relative to other low-carbon technologies, considering full lifecycle costs and grid integration?',
    'Independent, peer-reviewed techno-economic analysis comparing levelized cost of energy (LCOE), system integration costs, and deployment speed for various energy sources under different grid scenarios.',
    'If nuclear''s costs/timelines are found to be competitive or superior in certain contexts, its ''victim'' status would be challenged, potentially shifting the constraint towards a ''rope'' or ''contested'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_analysis_accuracy, empirical, 'Accuracy of nuclear''s cost and timeline assessment.').

omega_variable(
    carbon_per_dollar_per_year_metric_validity,
    'Is ''carbon-per-dollar-per-year'' the most appropriate and comprehensive metric for evaluating climate mitigation effectiveness, or does it overlook other critical factors like grid stability, energy security, or long-term decarbonization pathways?',
    'Expert consensus on climate modeling and energy system analysis regarding the optimal set of metrics for mitigation, considering multi-objective optimization.',
    'If the metric is deemed insufficient, the constraint''s core logic would be weakened, potentially reclassifying it as a ''snare'' if it primarily serves to exclude nuclear without a robust, holistic justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_per_dollar_per_year_metric_validity, conceptual, 'Validity of the ''carbon-per-dollar-per-year'' metric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 15, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_imperative' kernel. Other readings include 'portfolio_optimization_reading' and 'systems_transition_reading', which offer different perspectives on nuclear's role in mitigation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
