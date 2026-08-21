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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Technology Legitimacy: Reliability Primacy Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'reliability primacy' reading of
 *   technology legitimacy for climate mitigation. It asserts that only
 *   technologies providing dispatchable, baseload-capable generation are
 *   truly legitimate. This framing benefits nuclear power and fossil fuels
 *   with CCS, while imposing significant costs and legitimacy hurdles on
 *   intermittent renewables. The constraint is actively enforced through grid
 *   regulations and capacity markets. This is one reading of the
 *   'technology_legitimacy_kernel', which also includes
 *   'velocity_primacy_reading' and 'precautionary_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.65).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.7).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Technology Legitimacy: Reliability Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '1f2bcb55-8903-4247-bee1-7b2b1be1ba8d').
narrative_ontology:cs_kernel_codification('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', formalized).
narrative_ontology:cs_authority_grounding('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', practice).
narrative_ontology:cs_interpretation_layer_present('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d').
narrative_ontology:cs_reading_relation('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', foundational, grid_stability_is_paramount).
narrative_ontology:cs_axiom_status(grid_stability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', grid_stability_is_paramount, instrumental).
narrative_ontology:cs_axiom('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', foundational, dispatchability_is_a_necessary_condition).
narrative_ontology:cs_axiom_status(dispatchability_is_a_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', dispatchability_is_a_necessary_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', traditional_grid_engineering_principles).
narrative_ontology:cs_drift_state('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1f2bcb55-8903-4247-bee1-7b2b1be1ba8d', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_power_advocates).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, fossil_fuel_industry_with_ccs).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, climate_activists_prioritizing_speed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from this reading as nuclear power inherently provides dispatchable, baseload generation. They gain legitimacy and funding, positioning nuclear as a primary solution for climate mitigation.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_power_advocates, beneficiary,
    organized, generational, constrained, national).

% Prioritize grid stability and reliability, aligning with this reading. They administer grid connection standards and capacity markets that favor dispatchable sources, effectively enforcing this legitimacy criterion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit by positioning their dispatchable generation (e.g., natural gas with carbon capture and storage) as legitimate under this reading, extending the lifespan of their assets despite climate concerns.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, fossil_fuel_industry_with_ccs, beneficiary,
    institutional, biographical, constrained, national).

% Bear the cost of this reading, as their technologies (solar, wind) are not inherently dispatchable or baseload. They must invest in costly storage solutions or face reduced legitimacy and market access, increasing project costs and complexity.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    moderate, biographical, constrained, regional).

% Indirectly pay for the emphasis on reliability through higher electricity bills, as the grid infrastructure and dispatchable generation favored by this reading often come with significant capital and operational costs.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs, payer,
    powerless, immediate, trapped, local).

% Are excluded from the core legitimacy framing, as their priority is rapid deployment of any low-carbon technology, even if intermittent. This reading slows down the adoption of readily available, cheaper intermittent renewables.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_activists_prioritizing_speed, excluded,
    organized, generational, constrained, global).

% Analyze the implications of this legitimacy criterion on energy markets, technology development, and climate targets. They can identify trade-offs and unintended consequences, but do not directly influence the constraint's operation.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, energy_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy and investment around the paramount goal of maintaining a stable and reliable electricity grid, ensuring continuous power supply for critical infrastructure and daily life.
% TRANSFER_FUNCTION: Transfers legitimacy, funding, and market access to dispatchable, baseload-capable generation technologies (e.g., nuclear, fossil with CCS) from intermittent renewables, which are deemed less legitimate without costly grid integration solutions. Costs are transferred to ratepayers and developers of intermittent sources.
% ABSENT_VOICES: Climate activists prioritizing rapid deployment and developers of low-cost intermittent renewables are marginalized. They would argue that the reliability standard is too stringent, delaying necessary climate action and increasing costs, but their concerns are secondary to grid stability in this framing.
% DISAPPEARANCE_RATIONALE: If this legitimacy criterion vanished, energy policy would immediately shift to prioritize other factors like cost or speed of deployment. Investment in intermittent renewables would accelerate without the same reliability burden, and dispatchable sources would lose a key competitive advantage, leading to a rapid reorganization of the energy technology landscape.
% FOUNDING_PROBLEM: The historical challenge of ensuring continuous, stable electricity supply to meet demand fluctuations, preventing blackouts and grid collapse.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and national security agencies consistently attest to the live status of grid stability as a critical problem, citing the increasing complexity of modern grids and the severe consequences of power outages. Independent engineering assessments corroborate the technical challenges of integrating high penetrations of intermittent generation without dispatchable backup.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because it forces intermittent renewables to internalize the costs of grid stability (e.g., through storage), which are then passed to ratepayers. Suppression (0.70) is high due to the institutional power of grid operators and the fossil fuel industry in shaping policy and market rules that favor dispatchable sources. Theater ratio (0.20) is moderate; while grid stability is a genuine concern, the extent to which it is used to exclude competing technologies rather than genuinely optimize the grid introduces performative elements.
 *
 * PERSPECTIVAL GAP:
 *   Grid operators perceive this as a necessary technical constraint for system integrity, while developers of intermittent renewables see it as an extractive barrier to market entry. The engine's classification will reflect this divergence, likely showing a 'tangled_rope' for payers and a 'rope' or 'scaffold' for beneficiaries, depending on the balance of coordination and extraction from each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear power advocates, grid operators, and the fossil fuel industry with CCS are beneficiaries, as this reading legitimizes their technologies and operational models. Intermittent renewable developers and ratepayers bear the costs, facing higher hurdles and expenses. Climate activists prioritizing speed are excluded, as their concerns are secondary to reliability in this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliability_cost_allocation_ambiguity,
    'Is the cost of grid reliability fairly allocated across all generation types, or does this reading disproportionately burden intermittent sources?',
    'Detailed economic modeling of grid integration costs under various policy regimes, comparing scenarios with and without a strict reliability primacy criterion.',
    'If costs are disproportionately burdened, the effective extraction from intermittent renewables is higher than currently measured, potentially reclassifying the constraint as a ''snare'' for those seats. If fairly allocated, the ''tangled_rope'' classification holds, with reliability costs being a genuine coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_cost_allocation_ambiguity, empirical, 'Uncertainty regarding the fairness of reliability cost allocation.').

omega_variable(
    technological_evolution_of_dispatchability,
    'How rapidly will energy storage and smart grid technologies evolve to make intermittent sources effectively dispatchable and baseload-capable?',
    'Ongoing technological development and market adoption rates of advanced storage and grid management systems. Regular reassessment of the technical and economic feasibility of integrating high penetrations of renewables.',
    'If storage costs drop significantly and integration improves, the ''accessibility_collapse'' for intermittent renewables would decrease, reducing their effective extraction and potentially shifting the constraint towards a ''rope'' or even ''scaffold'' as the need for strict dispatchability from generation sources diminishes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_evolution_of_dispatchability, empirical, 'Uncertainty about future technological advancements impacting dispatchability.').

omega_variable(
    framing_of_legitimacy_criteria,
    'Is the ''reliability primacy'' criterion a fundamental technical necessity for grid operation, or a policy choice influenced by incumbent interests?',
    'Comparative analysis of energy policies and grid outcomes in different jurisdictions with varying legitimacy criteria, alongside historical analysis of lobbying efforts and policy capture by different energy sectors.',
    'If primarily a policy choice, the ''extraction'' component of the constraint is higher, and its ''suppression'' is more clearly a function of political power rather than technical necessity, pushing it closer to a ''snare''. If a fundamental necessity, the coordination function is stronger, supporting the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_legitimacy_criteria, conceptual, 'Conceptual ambiguity between technical necessity and policy choice in defining legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
