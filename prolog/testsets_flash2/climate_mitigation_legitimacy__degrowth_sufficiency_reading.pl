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
 *   human_readable: Climate Mitigation Legitimacy: Degrowth Sufficiency Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth sufficiency' reading of climate
 *   mitigation legitimacy, asserting that decarbonization fundamentally
 *   requires demand reduction, rendering large-scale generation expansion
 *   (including both nuclear and large-scale renewables) unnecessary. This
 *   reading positions energy system downsizing as a core component of
 *   legitimate climate action. It is a contested framing, with significant
 *   resistance from pro-growth economic models and advocates of supply-side
 *   technological solutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Climate Mitigation Legitimacy: Degrowth Sufficiency Reading").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae').
narrative_ontology:cs_kernel_codification('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', distributed).
narrative_ontology:cs_authority_grounding('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', distributed).
narrative_ontology:cs_reading_relation('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', foundational, demand_reduction_is_necessary_for_decarbonization).
narrative_ontology:cs_axiom_status(demand_reduction_is_necessary_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', demand_reduction_is_necessary_for_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', foundational, infinite_growth_is_incompatible_with_planetary_boundaries).
narrative_ontology:cs_axiom_status(infinite_growth_is_incompatible_with_planetary_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', infinite_growth_is_incompatible_with_planetary_boundaries, empirically_contingent).
narrative_ontology:cs_reference_frame('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', ecological_sufficiency_paradigm).
narrative_ontology:cs_drift_state('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', contemporary_policy_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a3ed0bc0-8ac4-4080-81c7-a91f6117a0ae', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_movements).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, pro_growth_economists).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_power_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, large_scale_renewable_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and advocate for policies that prioritize demand reduction and energy system downsizing, framing these as essential for legitimate decarbonization. Their identity is deeply tied to the degrowth paradigm.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates, agenda_setter,
    organized, generational, identity_locked, global).

% Benefit from policies that decentralize energy production and reduce overall consumption, aligning with their goals of local autonomy and reduced environmental impact. They are often allied with degrowth advocates.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_movements, beneficiary,
    moderate, biographical, constrained, local).

% Face significant operational and economic challenges under demand reduction mandates, which limit their growth and require costly retooling or relocation. Their business models are predicated on energy availability and affordability.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries, payer,
    powerful, immediate, constrained, national).

% Find their foundational economic models challenged by degrowth principles, which they view as economically detrimental and socially regressive. Their professional identity is often tied to growth-oriented policy advice.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, pro_growth_economists, payer,
    institutional, generational, identity_locked, global).

% See their proposed solutions for large-scale, dispatchable, carbon-free power rendered unnecessary or actively opposed by a demand-reduction paradigm. Their projects require significant capital and long-term planning, which are undermined.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_power_advocates, payer,
    organized, generational, constrained, national).

% Experience reduced market opportunities for new, large-scale solar and wind farms if demand reduction makes such expansion unnecessary. Their business models rely on significant capital investment in new generation capacity.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, large_scale_renewable_developers, payer,
    powerful, biographical, constrained, regional).

% Observe and model the efficacy of various mitigation pathways, including demand reduction. They assess the scientific validity and potential impacts of degrowth strategies on climate targets, without advocating for specific economic models.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal efforts towards decarbonization by prioritizing a specific pathway: reducing overall energy demand to alleviate the need for massive new generation infrastructure, thereby simplifying grid management and resource allocation.
% TRANSFER_FUNCTION: Transfers societal resources and political capital away from large-scale energy infrastructure projects (both fossil and new clean generation) towards conservation, efficiency, and localized energy solutions. It also transfers the burden of adaptation to lower energy consumption onto energy-intensive sectors and consumers.
% ABSENT_VOICES: Future generations, whose energy needs and technological capabilities are being implicitly constrained by current demand reduction targets, are absent from the immediate policy discourse. Their potential for technological solutions to energy supply is foreclosed by this reading.
% DISAPPEARANCE_RATIONALE: If the legitimacy of demand reduction as a primary decarbonization strategy vanished, the policy landscape would immediately shift towards aggressive expansion of either nuclear or large-scale renewables, with significant capital reallocation and a renewed focus on energy abundance rather than sufficiency.
% FOUNDING_PROBLEM: The perceived unsustainability of infinite growth on a finite planet, coupled with the urgency of climate change, leading to the conclusion that technological supply-side solutions alone are insufficient and that fundamental societal demand must be addressed.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists and some environmental scientists corroborate the foundational problem of planetary boundaries and the limits to growth. However, mainstream economists and energy policy experts often contest the necessity of demand reduction over technological innovation, leading to a 'contested' status for the problem's framing.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it offers a coordination function (a clear pathway for decarbonization) but involves significant asymmetric extraction. Extraction is high (0.65) due to the economic and social costs imposed on industries and populations accustomed to growth-oriented energy policies. Suppression (0.70) is also high, as this reading actively seeks to suppress alternative, supply-side-focused decarbonization strategies through policy and narrative. Resistance is substantial (0.75) from those whose interests or ideologies are challenged. Theater ratio is low (0.20) because the advocacy for demand reduction is genuine and directly functional to its goals, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between those who see demand reduction as an ethical imperative and a necessary condition for ecological sustainability (beneficiaries), and those who view it as an economic impediment or an unnecessary restriction on human flourishing (victims). The former experience the constraint as a legitimate coordination mechanism; the latter experience it as an extractive and suppressive force.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth advocates and local resilience movements are beneficiaries, as the constraint aligns with their core values and empowers their policy agendas. Energy-intensive industries, pro-growth economists, nuclear power advocates, and large-scale renewable developers are victims, as their interests are directly curtailed or rendered obsolete by this reading's policy implications. Climate scientists act as observers, analyzing the implications without direct benefit or cost from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by highlighting the dual nature of the constraint. While it offers a coordination function for climate action, its high extractiveness and suppression, coupled with active enforcement against alternatives, indicate it is not a pure Rope. The 'live' status of the founding problem (unsustainability of growth) suggests it is not a Piton, but the 'contested' status of the problem's framing points to the ongoing struggle over its legitimacy and necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_impact_of_degrowth,
    'What are the full economic and social costs of implementing demand reduction policies at the scale required for decarbonization, and how are these distributed across different societal groups?',
    'Comprehensive, independent macroeconomic modeling that includes social welfare impacts, employment shifts, and equity considerations, beyond purely environmental metrics.',
    'If costs are found to be prohibitive or inequitably distributed, the legitimacy of this reading as a primary decarbonization pathway would be severely undermined, potentially shifting policy towards supply-side solutions. If costs are manageable and equitable, its legitimacy would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_degrowth, empirical, 'Uncertainty regarding the real-world economic and social feasibility of large-scale demand reduction.').

omega_variable(
    technological_potential_underestimation,
    'Does this reading systematically underestimate the future potential of technological innovation (e.g., advanced nuclear, breakthrough energy storage, carbon capture) to achieve decarbonization without significant demand reduction?',
    'Long-term, multi-scenario energy system modeling that incorporates optimistic but plausible technological development trajectories, compared against demand-reduction-focused scenarios.',
    'If technological solutions prove more effective and less costly than assumed, the ''unnecessary'' claim regarding generation expansion would be challenged, weakening the constraint''s legitimacy. If technological progress remains insufficient, the demand reduction argument is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_potential_underestimation, empirical, 'Uncertainty about the future role of technology in decarbonization, which this reading downplays.').

omega_variable(
    legitimacy_of_growth_paradigm,
    'Is economic growth inherently incompatible with ecological sustainability, or can ''green growth'' decouple economic activity from environmental impact sufficiently to achieve climate goals?',
    'Long-term empirical observation of decoupling trends in various economies, combined with conceptual analysis of the ''growth'' concept itself and its relationship to resource consumption and waste generation.',
    'If green growth proves viable, the foundational axiom of this reading (growth-sustainability incompatibility) is challenged, potentially shifting the entire climate mitigation discourse. If decoupling is insufficient, the degrowth argument gains stronger normative grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_growth_paradigm, conceptual, 'Fundamental conceptual disagreement on the compatibility of economic growth and environmental sustainability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2025, 0.17).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2030, 0.18).
narrative_ontology:measurement(clim_tr_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2035, 0.19).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2040, 0.2).
narrative_ontology:measurement(clim_tr_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2045, 0.2).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2050, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2030, 0.61).
narrative_ontology:measurement(clim_be_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2035, 0.63).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2040, 0.64).
narrative_ontology:measurement(clim_be_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2045, 0.65).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2050, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2025, 0.63).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2030, 0.66).
narrative_ontology:measurement(clim_su_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2035, 0.68).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2040, 0.69).
narrative_ontology:measurement(clim_su_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2045, 0.7).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2050, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'climate_mitigation_legitimacy' kernel, each representing a distinct approach to achieving decarbonization. This 'degrowth_sufficiency_reading' emphasizes demand reduction, influencing and being influenced by other readings that prioritize different technological or economic pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
