% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy for Climate Mitigation
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'renewable primacy' reading of climate
 *   mitigation legitimacy, asserting that renewables plus storage offer the
 *   fastest and cheapest path to full decarbonization, thereby marginalizing
 *   other energy technologies like nuclear. It functions as a Tangled Rope,
 *   coordinating investment towards renewables while extracting from
 *   competing technologies by diverting capital and political support. The
 *   constraint is actively enforced through policy and public discourse,
 *   suppressing alternatives.
 *
 * KEY AGENTS:
 *   - renewable_energy_developers: Primary beneficiary (organized/mobile)
 *   - distributed_energy_proponents: Secondary beneficiary (moderate/constrained)
 *   - nuclear_industry: Primary payer (institutional/constrained)
 *   - fossil_fuel_lobby: Primary payer (institutional/trapped)
 *   - grid_operators_with_legacy_infrastructure: Payer (institutional/constrained)
 *   - climate_activists: Agenda setter (organized/mobile)
 *   - policy_makers: Agenda setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy for Climate Mitigation").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '140f80d2-780a-4603-9232-337d5a93080e').
narrative_ontology:cs_kernel_codification('140f80d2-780a-4603-9232-337d5a93080e', distributed).
narrative_ontology:cs_authority_grounding('140f80d2-780a-4603-9232-337d5a93080e', expertise).
narrative_ontology:cs_interpretation_layer_present('140f80d2-780a-4603-9232-337d5a93080e').
narrative_ontology:cs_reading_relation('140f80d2-780a-4603-9232-337d5a93080e', climate_mitigation_legitimacy__baseload_necessity_reading, influences).
narrative_ontology:cs_reading_relation('140f80d2-780a-4603-9232-337d5a93080e', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('140f80d2-780a-4603-9232-337d5a93080e', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('140f80d2-780a-4603-9232-337d5a93080e', foundational, renewables_are_fastest_cheapest_decarbonization).
narrative_ontology:cs_axiom_status(renewables_are_fastest_cheapest_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('140f80d2-780a-4603-9232-337d5a93080e', renewables_are_fastest_cheapest_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('140f80d2-780a-4603-9232-337d5a93080e', secondary, distributed_generation_is_resilient).
narrative_ontology:cs_axiom_status(distributed_generation_is_resilient, holdable).
narrative_ontology:cs_axiom_grounding('140f80d2-780a-4603-9232-337d5a93080e', distributed_generation_is_resilient, empirically_contingent).
narrative_ontology:cs_reference_frame('140f80d2-780a-4603-9232-337d5a93080e', rapid_cost_effective_decarbonization).
narrative_ontology:cs_drift_state('140f80d2-780a-4603-9232-337d5a93080e', contemporary_grid_integration_challenges, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('140f80d2-780a-4603-9232-337d5a93080e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_proponents).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_lobby).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_with_legacy_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from policies prioritizing renewable deployment and associated grid infrastructure. Advocates for accelerated transition and favorable regulatory environments for solar, wind, and battery storage projects.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Advocates for decentralized energy systems, microgrids, and local ownership of generation assets. Benefits from policies that reduce barriers to distributed renewable deployment and grid integration.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_proponents, beneficiary,
    moderate, biographical, constrained, local).

% Faces reduced investment and political support due to the prioritization of renewables. Argues for its role in baseload power and energy security. Its long capital cycles are seen as a liability by this reading.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% Directly challenged by any decarbonization strategy. This reading's emphasis on rapid, cost-effective renewable deployment further marginalizes fossil fuels, accelerating their decline as a primary energy source.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_lobby, payer,
    institutional, generational, trapped, global).

% Faces significant challenges and costs in adapting existing grid infrastructure to high penetrations of intermittent renewables and distributed generation. Requires substantial investment in modernization and flexibility.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_with_legacy_infrastructure, payer,
    institutional, generational, constrained, national).

% Actively promotes the renewable primacy narrative, influencing public opinion and policy. Their advocacy drives the political will for rapid renewable deployment and divestment from other energy sources.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_activists, agenda_setter,
    organized, generational, mobile, global).

% Implement policies based on this reading, directing subsidies, regulations, and research funding towards renewables and storage. They balance economic, political, and technical considerations, often under pressure from various lobbies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates investment and policy towards a specific technological pathway (renewables + storage) to achieve the collective action problem of climate change mitigation, by asserting its superior speed and cost-effectiveness.
% TRANSFER_FUNCTION: Transfers capital and political will from nuclear and fossil fuel sectors towards renewable energy development and grid modernization, based on the claim of faster and cheaper decarbonization.
% ABSENT_VOICES: Proponents of nuclear power (baseload_necessity_reading) and those advocating for a broader, technology-neutral approach (portfolio_pragmatism_reading) are marginalized in policy discussions dominated by this reading. They would argue for a more diversified energy mix to ensure reliability and manage transition risks.
% DISAPPEARANCE_RATIONALE: If the belief in renewable primacy vanished, energy policy would likely revert to a more diversified, technology-neutral approach, potentially re-evaluating nuclear power and other dispatchable sources. Investment flows would shift, and the pace and pathway of decarbonization efforts would fundamentally change.
% FOUNDING_PROBLEM: The urgent need for rapid and cost-effective decarbonization to address climate change, coupled with concerns about the long lead times and high capital costs of nuclear power.
% FOUNDING_PROBLEM_CORROBORATION: Independent energy economists and climate scientists corroborate the urgency of decarbonization and the declining costs of renewables. However, the 'faster and cheaper' claim for full decarbonization is contested by some energy system modelers and nuclear advocates, who point to grid stability challenges and system integration costs.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading actively reallocates significant capital and political resources away from nuclear and fossil fuels, imposing costs on those sectors. Suppression is also high, as the narrative actively discredits and marginalizes alternative decarbonization pathways, limiting their access to funding and public acceptance. The theater ratio is moderate, reflecting that while genuine decarbonization efforts are underway, some of the 'faster and cheaper' rhetoric serves to justify the exclusion of competitors rather than purely reflecting objective analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable developers and climate activists, this constraint is a necessary and efficient coordination mechanism for climate action. From the nuclear industry's perspective, it is an extractive snare that unfairly disadvantages a viable decarbonization technology. Policy makers experience it as a complex balancing act, navigating these competing claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers and distributed energy proponents are clear beneficiaries, as the constraint directs resources and policy in their favor. The nuclear industry, fossil fuel lobby, and legacy grid operators are payers, bearing the costs of redirected investment and infrastructure changes. Climate activists and policy makers act as agenda setters, actively shaping and enforcing the constraint. The 'faster and cheaper' claim is the core mechanism for justifying this redirection.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (simple coordination) by highlighting the active suppression of alternatives and the identifiable victims. It also avoids mislabeling as a Snare by acknowledging the genuine coordination function of directing climate action. The 'tangled' aspect captures the dual nature of solving a collective problem while imposing asymmetric costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_and_speed_corroboration,
    'Is the claim that ''renewables plus storage can achieve full decarbonization faster and cheaper than nuclear'' robustly corroborated by independent, comprehensive energy system modeling that accounts for all system costs (e.g., grid integration, long-duration storage, transmission upgrades)?',
    'Consensus among multiple independent, peer-reviewed energy system models that use consistent assumptions and account for full system costs, or a large-scale real-world demonstration project achieving full decarbonization with renewables+storage at lower cost/speed than nuclear alternatives.',
    'If corroborated, the constraint moves closer to a Rope, as its coordination function is genuinely optimal. If disproven, the extractive component (from nuclear) becomes more salient, pushing it towards a Snare, as the ''faster and cheaper'' justification becomes a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_and_speed_corroboration, empirical, 'Empirical validity of the ''faster and cheaper'' claim for full decarbonization.').

omega_variable(
    baseload_necessity_vs_flexibility,
    'Is dispatchable baseload power (e.g., nuclear) fundamentally necessary for grid stability at high decarbonization levels, or can grid flexibility (e.g., storage, demand response, transmission) fully substitute for it?',
    'Large-scale grid simulations and real-world operational experience from regions with very high renewable penetration demonstrating stable, reliable operation without baseload nuclear, or evidence of insurmountable technical barriers to such operation.',
    'If baseload is found to be necessary, the ''renewable primacy'' reading forecloses a critical component of grid stability, making its suppression of nuclear more problematic and pushing it towards a Snare. If flexibility fully substitutes, the constraint''s claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_vs_flexibility, empirical, 'Technical necessity of baseload power in a decarbonized grid.').

omega_variable(
    capital_sink_or_diversification,
    'Is investment in nuclear power a ''capital sink'' that delays renewable deployment, or a necessary diversification that hedges against risks in renewable deployment (e.g., supply chain, land use, social acceptance)?',
    'Comparative analysis of capital allocation and deployment timelines in different energy policy regimes (e.g., those prioritizing nuclear vs. those prioritizing renewables) and assessment of risk profiles for each technology.',
    'If nuclear is a capital sink, the constraint''s extractive effect on nuclear is justified by its positive impact on overall decarbonization speed. If it''s a necessary hedge, the constraint''s suppression of nuclear is detrimental to overall climate goals, making it more extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_sink_or_diversification, conceptual, 'Role of nuclear investment in overall decarbonization strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(clim_tr_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2025, 0.2).
narrative_ontology:measurement_basis(clim_tr_t2025, projected).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2030, 0.2).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(clim_be_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2025, 0.63).
narrative_ontology:measurement_basis(clim_be_t2025, projected).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement_basis(clim_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(clim_su_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement_basis(clim_su_t2025, projected).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement_basis(clim_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_legitimacy' kernel. Its claims about speed and cost-effectiveness directly influence the legitimacy and resource allocation for other decarbonization pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
