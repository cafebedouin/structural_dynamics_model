% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation Imperative: Portfolio Optimization Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio optimization' reading of the
 *   climate mitigation imperative, which asserts that all low-carbon energy
 *   sources, including nuclear, must be maximized to ensure reliable baseload
 *   power and achieve rapid decarbonization. It frames climate action as a
 *   technology-neutral race against time, where the primary goal is carbon
 *   intensity reduction. The constraint is claimed as a 'tangled_rope'
 *   because it genuinely coordinates climate action but also involves
 *   significant extraction from high-carbon industries and suppresses
 *   alternative decarbonization pathways.
 *
 * KEY AGENTS:
 *   - climate_mitigation_advocates: Primary agenda-setter (institutional/constrained)
 *   - nuclear_industry: Primary beneficiary (powerful/arbitrage)
 *   - grid_operators: Secondary beneficiary/agenda-setter (institutional/constrained)
 *   - fossil_fuel_industry: Primary target/payer (powerful/constrained)
 *   - renewable_only_advocates: Secondary target/payer (organized/constrained)
 *   - opportunity_cost_reading_proponents: Excluded voice (organized/constrained)
 *   - systems_transition_reading_proponents: Excluded voice (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.7).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.8).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation Imperative: Portfolio Optimization Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, 'f4542d03-eb1a-4ca1-bf95-ad9e401904fa').
narrative_ontology:cs_kernel_codification('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', implicit).
narrative_ontology:cs_authority_grounding('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', expertise).
narrative_ontology:cs_interpretation_layer_present('f4542d03-eb1a-4ca1-bf95-ad9e401904fa').
narrative_ontology:cs_reading_relation('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', foundational, carbon_intensity_is_primary_metric).
narrative_ontology:cs_axiom_status(carbon_intensity_is_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', carbon_intensity_is_primary_metric, empirically_contingent).
narrative_ontology:cs_axiom('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', foundational, grid_reliability_requires_baseload).
narrative_ontology:cs_axiom_status(grid_reliability_requires_baseload, holdable).
narrative_ontology:cs_axiom_grounding('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', grid_reliability_requires_baseload, empirically_contingent).
narrative_ontology:cs_reference_frame('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', urgent_decarbonization_imperative).
narrative_ontology:cs_drift_state('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', contemporary_energy_transition, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f4542d03-eb1a-4ca1-bf95-ad9e401904fa', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, renewable_only_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, general_public).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes policies that maximize all low-carbon energy sources, including nuclear, to achieve rapid decarbonization. They benefit from the perceived effectiveness of this comprehensive approach to climate goals.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_advocates, agenda_setter,
    institutional, generational, constrained, global).

% Receives policy support, subsidies, and regulatory pathways under this reading, positioning nuclear as a necessary component for reliable baseload power in a decarbonized grid. They benefit directly from this inclusion.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    powerful, generational, arbitrage, global).

% Advocate for the inclusion of nuclear power to ensure grid stability and reliable baseload capacity, which simplifies their operational challenges in managing intermittent renewable sources. They benefit from a more predictable energy mix.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, agenda_setter).

% Bears the primary costs of this constraint through carbon pricing, regulations, and market exclusion, as the imperative is to phase out high-carbon sources. Their business model is directly targeted.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry, payer,
    powerful, biographical, constrained, global).

% While supporting decarbonization, they bear costs by having their preferred 'renewables-only' pathways de-emphasized or by facing competition from subsidized nuclear projects, which they view as inefficient or risky. Their policy influence is diminished.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_only_advocates, payer,
    organized, biographical, constrained, global).

% Benefits from climate change mitigation and a stable energy supply. However, they may indirectly bear costs through higher energy prices (due to nuclear subsidies or carbon taxes) or through perceived risks associated with nuclear power.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, general_public, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, general_public, payer).

% Advocate for prioritizing the fastest and cheapest decarbonization pathways, often arguing against nuclear due to its high capital costs and long construction times. Their arguments are marginalized in policy discussions driven by the portfolio optimization reading.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, opportunity_cost_reading_proponents, excluded,
    organized, biographical, constrained, global).

% Advocate for decentralized, democratically controlled energy systems, viewing nuclear as perpetuating centralized, extractive power structures. Their vision for energy transition is sidelined by the technology-neutral, 'all sources' approach of this reading.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, systems_transition_reading_proponents, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global and national efforts to mitigate climate change by ensuring a stable, low-carbon energy supply through the maximal deployment of all available low-carbon technologies, including nuclear for baseload reliability.
% TRANSFER_FUNCTION: Transfers policy support, subsidies, and market access to nuclear and other low-carbon energy sectors, while imposing regulatory burdens, carbon costs, and market disadvantages on the fossil fuel industry.
% ABSENT_VOICES: Proponents of the 'opportunity_cost_reading' (who argue nuclear is too slow/expensive) and the 'systems_transition_reading' (who argue nuclear centralizes power) are structurally excluded from the dominant policy discourse, which prioritizes a technology-inclusive, baseload-focused approach to decarbonization.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, energy policy would likely revert to prioritizing cost or other factors over carbon intensity, leading to slower decarbonization, increased fossil fuel reliance, and potentially higher climate risks. The energy mix would shift away from nuclear and towards less reliable or higher-carbon alternatives.
% FOUNDING_PROBLEM: The urgent and complex challenge of mitigating climate change by rapidly decarbonizing energy systems while simultaneously ensuring grid reliability and energy security.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, national science academies, and international energy agencies consistently corroborate the urgency of climate change and the need for comprehensive decarbonization strategies, including diverse low-carbon sources.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.70) reflects the substantial costs imposed on the fossil fuel industry and the diversion of resources from other energy pathways. Suppression (0.80) is high due to active policy enforcement against high-carbon sources and the marginalization of arguments against nuclear or for renewables-only solutions. Theater ratio is low (0.20) because the constraint is driven by a genuine, urgent problem (climate change), and the actions taken are largely functional, not performative. The increasing extractiveness and suppression over time reflect the intensifying climate crisis and the hardening of policy positions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the nuclear industry and grid operators, this constraint is a necessary 'rope' for climate action and grid stability. From the perspective of the fossil fuel industry, it is a 'snare' designed to dismantle their operations. Renewable-only advocates might see it as a 'tangled_rope' that coordinates decarbonization but extracts from their preferred, potentially more agile, pathways. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry and grid operators are clear beneficiaries, receiving support and validation for their roles (low d). Climate mitigation advocates, as agenda-setters, also benefit from the policy direction aligning with their goals. The fossil fuel industry is the primary target, facing direct extraction and suppression (high d). Renewable-only advocates are also targets, as their preferred pathways are constrained or compete with subsidized nuclear (moderate-high d). The general public is a mixed beneficiary/payer, experiencing both benefits of mitigation and potential costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' by highlighting the significant extraction from fossil fuels and the suppression of alternative low-carbon strategies. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine coordination function around climate mitigation. The 'tangled_rope' classification accurately captures the hybrid nature, where a legitimate collective-action problem (climate change) is addressed through a structure that also involves asymmetric extraction and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_baseload_necessity_ambiguity,
    'Is nuclear power truly necessary for reliable baseload in a decarbonized grid, or can a combination of renewables, storage, and demand-side management achieve comparable reliability and cost-effectiveness?',
    'Long-term empirical data from grids with high renewable penetration and advanced storage solutions, coupled with independent engineering and economic modeling comparing different future energy mixes.',
    'If nuclear is not strictly necessary, the justification for its inclusion and associated subsidies under this reading weakens, potentially shifting resources to other low-carbon technologies and altering the constraint''s effective extractiveness from renewable-only advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_baseload_necessity_ambiguity, empirical, 'Empirical necessity of nuclear for grid reliability.').

omega_variable(
    portfolio_optimization_vs_opportunity_cost,
    'Does maximizing ''all'' low-carbon sources, including nuclear, represent the most efficient and rapid pathway to decarbonization, or does it divert resources (opportunity cost) from faster-deploying or cheaper alternatives?',
    'Comparative analysis of decarbonization trajectories and costs in different policy regimes, particularly those prioritizing different technology mixes, over a multi-decade period.',
    'If significant opportunity costs are demonstrated, the ''portfolio optimization'' framing''s efficiency claim is undermined, potentially leading to a re-evaluation of policy priorities and a shift in the constraint''s perceived legitimacy and beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portfolio_optimization_vs_opportunity_cost, conceptual, 'Efficiency of ''all sources'' approach versus opportunity cost.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''climate_mitigation_imperative'' kernel. What are the specific structural changes that would occur if the ''opportunity_cost_reading'' or ''systems_transition_reading'' were adopted as the dominant policy framework?',
    'Analysis of policy documents, legislative debates, and resource allocation patterns under alternative dominant readings.',
    'If an alternative reading became dominant, the beneficiary and victim sets would shift (e.g., nuclear industry becomes a victim under opportunity_cost_reading), and the constraint''s extractiveness and suppression mechanisms would reorient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Structural implications of alternative readings of the climate mitigation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2030, 0.19).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2040, 0.2).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2050, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2040, 0.69).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2050, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2030, 0.78).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2040, 0.79).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2050, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_phaseout_policies).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_subsidies).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'climate_mitigation_imperative' kernel. Each reading represents a different structural claim about how climate mitigation should be pursued, leading to different beneficiary/victim sets and classifications. They are linked to allow for analysis of their interdependencies and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
