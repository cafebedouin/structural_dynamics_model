% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Decarbonization via Demand Reduction and Sufficiency
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth sufficiency' reading of climate
 *   mitigation legitimacy, asserting that decarbonization fundamentally
 *   requires demand reduction, rendering large-scale energy generation
 *   expansion unnecessary. This reading challenges conventional
 *   growth-oriented climate strategies by prioritizing sufficiency and
 *   efficiency over continuous supply-side growth. It is a contested claim,
 *   with significant implications for energy policy and economic models.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.78).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.85).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Decarbonization via Demand Reduction and Sufficiency").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'd0c5a9e3-c54e-45ba-957c-214eecfad1e1').
narrative_ontology:cs_kernel_codification('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', implicit).
narrative_ontology:cs_authority_grounding('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', expertise).
narrative_ontology:cs_interpretation_layer_present('d0c5a9e3-c54e-45ba-957c-214eecfad1e1').
narrative_ontology:cs_reading_relation('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', climate_mitigation_legitimacy__renewable_primacy_reading, influences).
narrative_ontology:cs_reading_relation('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', foundational, energy_demand_has_ecological_limits).
narrative_ontology:cs_axiom_status(energy_demand_has_ecological_limits, holdable).
narrative_ontology:cs_axiom_grounding('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', energy_demand_has_ecological_limits, empirically_contingent).
narrative_ontology:cs_axiom('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', foundational, sufficiency_is_a_primary_decarbonization_strategy).
narrative_ontology:cs_axiom_status(sufficiency_is_a_primary_decarbonization_strategy, holdable).
narrative_ontology:cs_axiom_grounding('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', sufficiency_is_a_primary_decarbonization_strategy, conventional).
narrative_ontology:cs_reference_frame('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', ecological_sufficiency_paradigm).
narrative_ontology:cs_drift_state('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', contemporary_growth_paradigm, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d0c5a9e3-c54e-45ba-957c-214eecfad1e1', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_ecosystem).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_communities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, large_scale_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_oriented_economies).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, ecological_limits_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and champion policies for demand reduction and sufficiency, arguing for a fundamental shift in economic paradigms. They seek to reorient energy policy away from supply-side expansion.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates, agenda_setter,
    analytical, civilizational, mobile, global).

% Benefits from reduced resource extraction, lower emissions, and less habitat destruction associated with large-scale energy infrastructure. It is a passive beneficiary, unable to act on its own behalf.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_ecosystem, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_ecosystem).

% Inherit a more stable climate and less depleted natural resources if demand reduction is successfully implemented. They are passive beneficiaries, unable to influence current policy directly.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, future_generations, beneficiary,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_mitigation_legitimacy__degrowth_sufficiency_reading, future_generations).

% Benefit from reduced local pollution, less land use for energy projects, and increased energy autonomy through local sufficiency measures. They often advocate for smaller-scale, decentralized solutions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_communities, beneficiary,
    moderate, biographical, constrained, local).

% Faces existential threat from policies prioritizing demand reduction, as their business model relies on continuous growth in energy consumption. They actively resist such policies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_industry, payer,
    institutional, biographical, constrained, global).

% Suffers from the claim that large-scale generation expansion is unnecessary, as their projects are inherently large-scale and capital-intensive. They advocate for their role in baseload power.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry, payer,
    institutional, biographical, constrained, global).

% While supporting renewables, they are victims of the 'expansion unnecessary' clause, as their business model often relies on large-scale projects (e.g., massive solar farms, offshore wind). They would prefer a focus on large-scale deployment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, large_scale_renewable_developers, payer,
    organized, biographical, constrained, global).

% Their economic models are predicated on continuous growth in GDP and energy consumption. Demand reduction policies challenge their fundamental operating assumptions and require significant structural change.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_oriented_economies, payer,
    institutional, generational, constrained, global).

% Are tasked with implementing decarbonization strategies. They face pressure from both degrowth advocates and growth-oriented industries, making the adoption of radical demand reduction policies politically challenging.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global and national efforts towards decarbonization by prioritizing energy demand reduction and sufficiency measures, thereby making large-scale energy generation expansion redundant.
% TRANSFER_FUNCTION: Transfers investment, political capital, and social focus away from large-scale energy infrastructure projects (fossil, nuclear, and large-scale renewables) towards energy efficiency, conservation, and smaller, decentralized, sufficiency-oriented solutions. It also transfers costs to industries reliant on energy growth.
% ABSENT_VOICES: Future energy consumers who might face energy scarcity if demand reduction targets are unrealistic or poorly implemented; those who believe in technological solutions without lifestyle changes; and those who prioritize economic growth above all else.
% DISAPPEARANCE_RATIONALE: If the claim that demand reduction is *required* and large-scale expansion *unnecessary* vanished, energy policy would revert to a supply-side focus, leading to different investment patterns, continued growth in energy consumption, and potentially higher emissions. The entire climate mitigation discourse would shift.
% FOUNDING_PROBLEM: Unchecked global energy demand growth leading to unsustainable resource depletion, escalating greenhouse gas emissions, and a reliance on large, centralized, and often environmentally damaging energy infrastructure, with technological fixes alone proving insufficient to meet climate targets.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists, a segment of climate scientists (e.g., IPCC reports on consumption and lifestyle changes), and numerous environmental NGOs corroborate the problem of unsustainable demand growth and the limitations of purely supply-side solutions. This corroboration comes from outside the direct beneficiaries of demand reduction policies.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because implementing this 'requirement' would impose substantial costs on industries and economies built on energy growth. Suppression (0.85) is also high, as this reading actively seeks to suppress alternative (growth-dependent) decarbonization pathways and faces strong resistance from powerful incumbents. Resistance (0.75) is significant from industries and political factions whose interests are threatened. Theater ratio (0.20) is relatively low, as advocates of this reading are generally sincere in their pursuit of systemic change, though some 'greenwashing' of efficiency measures might occur without deeper structural shifts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates, this constraint is a necessary coordination mechanism for planetary survival. From the perspective of growth-oriented industries, it is a highly extractive and suppressive force that threatens their existence. The engine's computation of per-seat classification will highlight this divergence, showing a 'tangled_rope' for the system as a whole, but potentially a 'rope' for beneficiaries and a 'snare' for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth advocates, the global ecosystem, future generations, and local communities are the primary beneficiaries, as they gain from a more sustainable and less impactful energy system. The fossil fuel, nuclear, and large-scale renewable industries, along with growth-oriented economies, are the primary victims, as their business models are directly challenged by the premise of demand reduction and the 'unnecessary' status of expansion. Policy makers are agenda-setters, navigating these conflicting pressures.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_cost_of_sufficiency_implementation,
    'What is the actual marginal cost of implementing demand reduction and sufficiency policies at scale, and how does it compare to the economic benefits of avoided expansion and environmental damage?',
    'Comprehensive economic modeling and pilot programs demonstrating the real-world costs and benefits of large-scale demand reduction policies, including social and behavioral shifts.',
    'If costs are prohibitive or benefits are lower than projected, the legitimacy of this reading as a primary decarbonization strategy would be undermined, potentially shifting policy towards supply-side solutions. If benefits significantly outweigh costs, its adoption would be accelerated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_cost_of_sufficiency_implementation, empirical, 'Economic feasibility and net benefit of demand reduction policies.').

omega_variable(
    political_feasibility_of_demand_reduction,
    'Is demand reduction politically feasible at the scale and speed required for effective decarbonization, given entrenched economic interests and societal expectations of growth?',
    'Analysis of policy adoption rates, public acceptance, and electoral outcomes in jurisdictions attempting significant demand reduction, alongside studies of social tipping points and behavioral change.',
    'If political feasibility is low, the constraint''s effective suppression and extractiveness would be amplified for advocates, as they face insurmountable barriers. If high, the constraint could transition towards a more widely accepted coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_demand_reduction, empirical, 'Political and social viability of degrowth policies.').

omega_variable(
    kernel_reading_structural_delta,
    'How would the structural properties (extractiveness, suppression, beneficiaries, victims) of climate mitigation change if a sibling reading (e.g., baseload_necessity_reading) were adopted as the dominant framework?',
    'Comparative analysis of policy outcomes and stakeholder impacts under different dominant readings, using counterfactual modeling and historical case studies.',
    'Each sibling reading would produce a different set of beneficiaries and victims, and different levels of extraction and suppression, reflecting the shift in which industries and societal groups bear the costs and reap the benefits of decarbonization strategies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2005, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2025, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(clim_be_t2005, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(clim_be_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2015, 0.73).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2025, 0.77).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2030, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(clim_su_t2005, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(clim_su_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2020, 0.83).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2025, 0.84).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_carbon_pricing_regime).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_efficiency_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'climate_mitigation_legitimacy' kernel, each representing a distinct approach to decarbonization. This reading emphasizes demand reduction and sufficiency, contrasting with supply-side expansion or technology-neutral approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
