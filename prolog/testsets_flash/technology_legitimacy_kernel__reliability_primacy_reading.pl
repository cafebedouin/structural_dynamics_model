% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Climate Technology Legitimacy: Reliability Primacy Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the legitimacy of climate mitigation technologies
 *   through the lens of grid reliability, prioritizing dispatchable,
 *   baseload-capable generation. It is one reading of the broader
 *   'technology_legitimacy_kernel' which also includes readings focused on
 *   deployment velocity and precautionary principles. This
 *   'reliability_primacy_reading' structurally favors technologies like
 *   nuclear and fossil fuels with carbon capture, while imposing significant
 *   costs and legitimacy hurdles on intermittent renewables unless paired
 *   with expensive storage. The constraint is claimed as a Tangled Rope
 *   because it genuinely coordinates grid stability but extracts from
 *   specific technology developers and ratepayers through its stringent
 *   criteria.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.6).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.7).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Climate Technology Legitimacy: Reliability Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, 'cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38').
narrative_ontology:cs_kernel_codification('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', implicit).
narrative_ontology:cs_authority_grounding('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', expertise).
narrative_ontology:cs_interpretation_layer_present('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38').
narrative_ontology:cs_reading_relation('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', foundational, grid_stability_is_paramount).
narrative_ontology:cs_axiom_status(grid_stability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', grid_stability_is_paramount, deontological).
narrative_ontology:cs_axiom('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', foundational, dispatchability_is_necessary).
narrative_ontology:cs_axiom_status(dispatchability_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', dispatchability_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', traditional_grid_stability_paradigm).
narrative_ontology:cs_drift_state('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', contemporary_energy_transition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cb7f8dbd-ee6c-4269-a051-9fe26bb2bd38', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, fossil_fuel_with_ccs_advocates).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, climate_activists_prioritizing_speed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining grid stability and reliability. They prioritize technologies that offer dispatchable, baseload power to avoid blackouts and manage load fluctuations. This reading aligns with their operational mandate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from this reading as nuclear power is inherently baseload and dispatchable. This framing legitimizes their technology for climate mitigation, attracting investment and policy support.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, beneficiary,
    organized, generational, mobile, global).

% Also benefit, as their technologies (e.g., natural gas with carbon capture) can be dispatchable and baseload. This reading provides a pathway for their continued relevance in climate policy, despite carbon emissions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, fossil_fuel_with_ccs_advocates, beneficiary,
    powerful, biographical, constrained, national).

% Bear significant costs under this reading, as their technologies (solar, wind) are not dispatchable or baseload without expensive storage solutions. This increases their cost of entry and reduces their perceived legitimacy for climate mitigation.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    moderate, biographical, constrained, regional).

% Indirectly pay for the emphasis on baseload reliability through higher electricity bills, as the grid prioritizes more expensive dispatchable options or requires costly storage for renewables. They have little direct influence on technology legitimacy criteria.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs, payer,
    powerless, immediate, trapped, local).

% Advocate for rapid deployment of all available low-carbon technologies, including intermittent renewables, to meet urgent carbon budget targets. This reading's emphasis on reliability over speed marginalizes their preferred solutions and policy approaches.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_activists_prioritizing_speed, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy and investment towards technologies that ensure continuous, stable electricity supply, preventing grid instability and blackouts during the energy transition.
% TRANSFER_FUNCTION: Transfers legitimacy, policy support, and investment from intermittent renewable technologies (without storage) to dispatchable, baseload-capable technologies like nuclear and fossil fuels with CCS, shifting costs for reliability onto ratepayers and renewable developers.
% ABSENT_VOICES: Advocates for rapid decarbonization and those prioritizing cost-effectiveness over absolute dispatchability are marginalized. They would argue for a broader portfolio of technologies and more flexible grid management, but their concerns are secondary to the reliability mandate.
% DISAPPEARANCE_RATIONALE: If this legitimacy criterion vanished, energy policy would immediately shift to prioritize other factors (e.g., speed of deployment, cost, environmental impact), leading to a rapid re-evaluation of technology portfolios, increased investment in intermittent renewables, and potentially less emphasis on traditional baseload sources.
% FOUNDING_PROBLEM: The challenge of maintaining grid stability and preventing blackouts while transitioning from a fossil-fuel-dominated energy system to a low-carbon one.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and energy security experts universally corroborate the ongoing and critical nature of grid stability. While the specific technologies to achieve it are contested, the problem itself is a live concern for all parties, including those who advocate for different solutions.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is driven by the increased costs imposed on intermittent renewables and the higher prices ratepayers may bear for 'reliable' energy sources. Suppression (0.7) is high because this reading actively marginalizes alternative technologies and policy approaches that prioritize other criteria (e.g., speed, cost, environmental risk). The 'requires_active_enforcement' flag is true because grid operators and regulators actively enforce these criteria through permitting, subsidies, and market design. The accessibility_collapse (0.4) is moderate, as alternatives are not entirely foreclosed but are made significantly more difficult and costly. Resistance (0.5) is also moderate, reflecting ongoing debates and advocacy from proponents of other technology types.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of grid operators, this is a necessary coordination mechanism to ensure energy security. From the perspective of intermittent renewable developers and climate activists, it is an extractive and suppressive mechanism that slows decarbonization and protects incumbent technologies. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid operators act as agenda-setters, benefiting from a clear mandate for stability. The nuclear industry and advocates for fossil fuels with CCS are direct beneficiaries. Intermittent renewable developers and ratepayers (who bear the costs of prioritizing reliability) are the primary payers. Climate activists prioritizing speed are excluded, as their preferred solutions are disfavored by this reading's criteria.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliability_cost_vs_benefit,
    'Is the cost imposed by prioritizing baseload reliability (e.g., on intermittent renewables and ratepayers) proportional to the actual grid stability benefits, or does it reflect an over-emphasis on traditional grid architectures?',
    'Comparative analysis of grid stability metrics and energy costs in regions adopting more flexible, distributed grid management with high renewable penetration, versus regions adhering to baseload primacy.',
    'If costs are disproportionate, the ''extractiveness'' of this reading is higher than justified by genuine coordination needs, suggesting a stronger ''snare'' component. If proportional, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_cost_vs_benefit, empirical, 'Assessing the true cost-benefit ratio of baseload reliability primacy.').

omega_variable(
    legitimacy_criteria_framing,
    'Is the ''reliability primacy'' criterion a fundamental, immutable requirement for climate technology legitimacy, or a policy choice reflecting a specific risk aversion and technological bias?',
    'Conceptual analysis of energy system resilience, comparing ''reliability'' as a static property of generation to ''resilience'' as an adaptive property of the entire grid system (including demand-side management, storage, and smart grids).',
    'If it''s a policy choice, the ''claimed_type'' as a ''tangled_rope'' is reinforced, highlighting the constructed nature of the constraint. If it''s immutable, it would lean towards a ''mountain'' (though unlikely given beneficiaries), suggesting a re-evaluation of its ''emerges_naturally'' status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_criteria_framing, conceptual, 'Distinguishing fundamental necessity from policy preference in technology legitimacy criteria.').

omega_variable(
    sibling_reading_impact_on_legitimacy,
    'How would the widespread adoption of the ''velocity_primacy_reading'' or ''precautionary_reading'' structurally alter the legitimacy of technologies favored by this ''reliability_primacy_reading''?',
    'Scenario modeling of policy shifts: if a ''velocity'' or ''precautionary'' framework became dominant, track changes in investment, permitting, and public acceptance for nuclear or CCS technologies.',
    'If a sibling reading gains dominance, technologies favored by ''reliability primacy'' would face reduced legitimacy and increased costs, shifting their position from ''beneficiary'' towards ''payer'' or ''excluded'' within the broader kernel context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_on_legitimacy, empirical, 'Cross-reading impact assessment on technology legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(tech_be_t2000, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(tech_be_t2005, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(tech_be_t2010, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(tech_be_t2015, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(tech_be_t2020, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(tech_be_t2024, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t2000, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(tech_su_t2005, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(tech_su_t2010, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(tech_su_t2015, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(tech_su_t2020, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(tech_su_t2024, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'technology_legitimacy_kernel'. Each reading defines technology legitimacy differently, leading to distinct beneficiary/victim structures and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
