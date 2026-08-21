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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Imperative (Opportunity Cost Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'opportunity cost' reading of the climate
 *   mitigation imperative, which asserts that mitigation efforts must
 *   prioritize technologies offering the fastest carbon reduction per dollar
 *   invested. Under this reading, nuclear power, despite being low-carbon, is
 *   deemed net-harmful due to its high capital intensity and long deployment
 *   timelines, which divert resources from more immediately impactful
 *   renewable energy projects. This is one reading of the broader
 *   'climate_mitigation_imperative' kernel.
 *
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
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Imperative (Opportunity Cost Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '9680cd0d-f35c-4ee8-904c-e7623391d9a4').
narrative_ontology:cs_kernel_codification('9680cd0d-f35c-4ee8-904c-e7623391d9a4', distributed).
narrative_ontology:cs_authority_grounding('9680cd0d-f35c-4ee8-904c-e7623391d9a4', expertise).
narrative_ontology:cs_interpretation_layer_present('9680cd0d-f35c-4ee8-904c-e7623391d9a4').
narrative_ontology:cs_reading_relation('9680cd0d-f35c-4ee8-904c-e7623391d9a4', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('9680cd0d-f35c-4ee8-904c-e7623391d9a4', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('9680cd0d-f35c-4ee8-904c-e7623391d9a4', foundational, carbon_reduction_rate_is_paramount).
narrative_ontology:cs_axiom_status(carbon_reduction_rate_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('9680cd0d-f35c-4ee8-904c-e7623391d9a4', carbon_reduction_rate_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('9680cd0d-f35c-4ee8-904c-e7623391d9a4', foundational, capital_intensity_is_a_mitigation_cost).
narrative_ontology:cs_axiom_status(capital_intensity_is_a_mitigation_cost, holdable).
narrative_ontology:cs_axiom_grounding('9680cd0d-f35c-4ee8-904c-e7623391d9a4', capital_intensity_is_a_mitigation_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('9680cd0d-f35c-4ee8-904c-e7623391d9a4', urgent_cost_effective_decarbonization).
narrative_ontology:cs_drift_state('9680cd0d-f35c-4ee8-904c-e7623391d9a4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9680cd0d-f35c-4ee8-904c-e7623391d9a4', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_vulnerable_communities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, pro_nuclear_policymakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the prioritization of faster, cheaper renewable deployments, aligning with their advocacy for rapid decarbonization. They actively promote policies that redirect investment away from nuclear.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Are the ultimate beneficiaries of rapid, cost-effective climate mitigation, as they face the most immediate and severe impacts of climate change. Their situation is improved by policies that maximize carbon reduction per dollar per year.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_vulnerable_communities, beneficiary,
    powerless, immediate, trapped, local).

% Bears the cost of this constraint as its projects are deemed too slow and capital-intensive to meet the 'fastest deployment per dollar' criterion. This leads to reduced investment, project cancellations, and a shrinking market share.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% Face political and budgetary pressure to defund or deprioritize nuclear projects in favor of renewables, as their policy choices are evaluated against the 'fastest mitigation' metric. Their careers and agendas are constrained by this imperative.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, pro_nuclear_policymakers, payer,
    powerful, biographical, constrained, national).

% Is excluded from the mitigation conversation entirely, as their technologies are not low-carbon. While not directly paying into this specific constraint, their exclusion is a foundational premise of the entire climate mitigation imperative.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fossil_fuel_industry, excluded,
    institutional, generational, constrained, global).

% Provide the scientific basis for the urgency of climate action and the need for rapid decarbonization, but do not directly benefit or pay from specific technology choices. Their analysis informs the 'fastest deployment' metric.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global and national investment decisions towards the most efficient and rapid deployment of carbon-reducing technologies, ensuring that limited capital and time are used to maximize climate impact.
% TRANSFER_FUNCTION: Redirects capital and political will from slower, more expensive low-carbon technologies (like nuclear) to faster, cheaper ones (like renewables), transferring investment opportunities and market share.
% ABSENT_VOICES: The fossil fuel industry is structurally excluded from the conversation about low-carbon energy choices. Advocates for long-term energy security and grid stability (who might support nuclear) are marginalized if their arguments do not align with the 'fastest deployment' metric.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, investment in nuclear power would likely surge, and the urgency of rapid renewable deployment would diminish. Climate mitigation strategies would shift towards a more diversified, potentially slower, and more capital-intensive portfolio, reorganizing global energy investment flows.
% FOUNDING_PROBLEM: The existential threat of climate change requires urgent, large-scale decarbonization, and there are finite resources (time, capital) to achieve it.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on climate change (IPCC reports, national academies) corroborates the urgency and the need for effective mitigation. Economic analyses from independent research institutions (e.g., Lazard LCOE studies) corroborate the cost-effectiveness of renewables over nuclear for rapid deployment, from outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates investment towards rapid decarbonization (a collective action problem) but does so by extracting resources and opportunities from the nuclear industry. Extractiveness is high (0.65) because nuclear projects are effectively 'taxed' by being deemed inefficient for mitigation, leading to reduced investment. Suppression (0.4) is moderate, as nuclear advocates can still argue their case, but face significant headwinds. Resistance (0.7) is high from the nuclear industry and its supporters. The metrics are projected to increase as the urgency of climate action intensifies, making the opportunity cost argument more potent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable energy advocates, this constraint is a necessary Rope, efficiently allocating resources to solve an urgent problem. From the nuclear industry's perspective, it is a Snare, unfairly excluding a viable low-carbon option based on a narrow metric. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy advocates and climate-vulnerable communities are beneficiaries, as the constraint directs resources towards their preferred outcomes. The nuclear industry and pro-nuclear policymakers are victims, as their preferred technology is deprioritized. The constraint actively enforces this prioritization through policy and funding mechanisms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opportunity_cost_metric_validity,
    'Is ''fastest deployment per dollar'' the most appropriate metric for climate mitigation, or does it overlook other critical factors like grid stability, energy security, or long-term system resilience?',
    'Comprehensive energy system modeling that integrates multiple metrics (cost, speed, reliability, security, waste management) and evaluates trade-offs across different technology portfolios.',
    'If other factors are deemed equally or more critical, the constraint''s extractiveness from nuclear might be re-evaluated as less justified, potentially shifting its classification towards a Snare or Piton from the nuclear seat. If the metric holds, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_metric_validity, conceptual, 'Whether the primary metric for mitigation is sufficiently comprehensive.').

omega_variable(
    nuclear_cost_reduction_trajectory,
    'Will advanced nuclear technologies (e.g., SMRs) achieve significant cost reductions and faster deployment timelines in the future, altering their ''opportunity cost'' profile?',
    'Empirical data from pilot projects and first-of-a-kind deployments of advanced nuclear reactors, tracking actual costs and construction times against projections.',
    'If nuclear costs fall and timelines shorten substantially, the constraint''s justification for extracting from nuclear would weaken, potentially reducing its effective extractiveness and shifting its classification towards a Rope or even a Mountain (if it becomes genuinely competitive).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_cost_reduction_trajectory, empirical, 'Future cost and deployment trajectory of nuclear power.').

omega_variable(
    political_will_vs_technical_feasibility,
    'To what extent is the ''fastest deployment'' imperative driven by genuine technical and economic feasibility, versus political expediency and the desire to avoid complex, long-term nuclear projects?',
    'Analysis of policy debates, lobbying efforts, and public discourse surrounding energy choices, distinguishing between arguments based on technical data and those based on political preferences or institutional inertia.',
    'If political expediency is a dominant factor, the constraint''s suppression of nuclear might be re-evaluated as less about objective opportunity cost and more about a constructed preference, increasing its Snare-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_vs_technical_feasibility, preference, 'Underlying drivers of the ''fastest deployment'' imperative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2030, 0.22).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2040, 0.23).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2050, 0.24).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2050, 0.7).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2030, 0.45).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2040, 0.48).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2050, 0.5).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_imperative' kernel. Its focus on opportunity cost and fastest deployment per dollar differentiates it from readings that prioritize portfolio diversity or systemic transformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
