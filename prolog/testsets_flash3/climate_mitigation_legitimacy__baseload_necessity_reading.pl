% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Power Necessity for Decarbonization (Baseload Necessity Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'baseload necessity' reading of climate
 *   mitigation legitimacy, asserting that reliable decarbonization requires
 *   dispatchable baseload power that renewables alone cannot provide at
 *   scale. This reading prioritizes grid stability and reliability, often
 *   leading to policy support for nuclear power and fossil fuels with carbon
 *   capture and storage (CCS). It is one reading of the broader
 *   'climate_mitigation_legitimacy' kernel, alongside
 *   'renewable_primacy_reading', 'portfolio_pragmatism_reading', and
 *   'degrowth_sufficiency_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.45).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.6).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Power Necessity for Decarbonization (Baseload Necessity Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '88048afb-cb19-4785-aa21-4150a4022eff').
narrative_ontology:cs_kernel_codification('88048afb-cb19-4785-aa21-4150a4022eff', formalized).
narrative_ontology:cs_authority_grounding('88048afb-cb19-4785-aa21-4150a4022eff', expertise).
narrative_ontology:cs_interpretation_layer_present('88048afb-cb19-4785-aa21-4150a4022eff').
narrative_ontology:cs_reading_relation('88048afb-cb19-4785-aa21-4150a4022eff', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('88048afb-cb19-4785-aa21-4150a4022eff', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_reading_relation('88048afb-cb19-4785-aa21-4150a4022eff', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('88048afb-cb19-4785-aa21-4150a4022eff', foundational, dispatchable_power_is_non_negotiable).
narrative_ontology:cs_axiom_status(dispatchable_power_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('88048afb-cb19-4785-aa21-4150a4022eff', dispatchable_power_is_non_negotiable, empirically_contingent).
narrative_ontology:cs_axiom('88048afb-cb19-4785-aa21-4150a4022eff', foundational, renewables_alone_cannot_meet_scale_and_reliability).
narrative_ontology:cs_axiom_status(renewables_alone_cannot_meet_scale_and_reliability, holdable).
narrative_ontology:cs_axiom_grounding('88048afb-cb19-4785-aa21-4150a4022eff', renewables_alone_cannot_meet_scale_and_reliability, empirically_contingent).
narrative_ontology:cs_reference_frame('88048afb-cb19-4785-aa21-4150a4022eff', traditional_grid_reliability_paradigm).
narrative_ontology:cs_drift_state('88048afb-cb19-4785-aa21-4150a4022eff', contemporary_renewable_advances_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('88048afb-cb19-4785-aa21-4150a4022eff', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_industry_with_ccs).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, taxpayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, environmental_justice_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from policies that prioritize dispatchable baseload, receiving subsidies and long-term contracts for new plant construction and operation. Their continued existence is framed as essential for grid stability during decarbonization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry, beneficiary,
    institutional, generational, constrained, national).

% Seeks to extend the life of existing assets by integrating Carbon Capture and Storage (CCS) technology, positioning itself as a provider of dispatchable baseload. Benefits from policy support for CCS as a 'decarbonization' solution.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_industry_with_ccs, beneficiary,
    institutional, biographical, constrained, national).

% Responsible for maintaining grid stability and reliability. They advocate for dispatchable baseload capacity to manage intermittency from renewables, influencing policy towards technologies that provide this stability.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, agenda_setter,
    institutional, immediate, constrained, regional).

% Bear the cost of policy frameworks that divert investment from pure renewable deployment to baseload alternatives. They argue that grid modernization and storage solutions can address intermittency without relying on nuclear or fossil fuels.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_advocates, payer,
    organized, generational, constrained, global).

% Fund subsidies and long-term contracts for baseload power sources through taxes, often without direct input on energy policy choices. Their costs are diffuse but significant.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Often bear the disproportionate environmental and health burdens of large-scale baseload power generation (e.g., nuclear waste storage, fossil fuel emissions even with CCS), with limited political power to influence siting or policy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, environmental_justice_communities, payer,
    powerless, generational, trapped, local).

% Provide scientific consensus on climate change and decarbonization targets, but do not directly adjudicate specific technology pathways. Their findings are interpreted by various stakeholders to support different energy policies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national energy policy to ensure a stable and reliable electricity supply during the transition to a low-carbon economy, by prioritizing dispatchable baseload power sources.
% TRANSFER_FUNCTION: Transfers public funds (subsidies, guaranteed contracts) and regulatory certainty to industries capable of providing dispatchable baseload power (nuclear, fossil with CCS), at the expense of taxpayers and potentially faster renewable deployment.
% ABSENT_VOICES: Advocates for decentralized energy systems, microgrids, and community-owned renewables are often marginalized in national policy debates focused on large-scale baseload solutions. They would argue for greater energy democracy and resilience.
% DISAPPEARANCE_RATIONALE: If the perceived necessity of dispatchable baseload vanished overnight, energy policy would rapidly shift towards accelerated renewable deployment and storage solutions. Investment in nuclear and CCS would cease, and grid planning would fundamentally reorganize around distributed generation and demand-side management.
% FOUNDING_PROBLEM: The problem of ensuring continuous, reliable electricity supply while transitioning away from fossil fuels, given the intermittency of primary renewable sources like solar and wind.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and national energy agencies consistently attest to the live status of the problem, citing technical challenges of grid integration for high penetrations of intermittent renewables. This is corroborated by engineering studies on grid stability, though the *solution* (baseload vs. storage/flexibility) is contested.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).
:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the public subsidies and long-term contracts directed towards baseload providers, which are seen as necessary costs by proponents but as rent-seeking by opponents. Suppression (0.60) arises from the institutional inertia and regulatory frameworks that favor established baseload technologies, making it difficult for purely renewable pathways to gain equivalent policy support. Theater ratio (0.20) is moderate; while the need for grid stability is real, the extent to which it *requires* specific baseload technologies versus flexible grid solutions is debated. The metrics are projected to increase as the perceived necessity of baseload hardens in policy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of grid operators and baseload industries, this constraint is a necessary coordination mechanism for a stable energy transition. From the perspective of renewable advocates and environmental justice communities, it is an extractive mechanism that entrenches incumbent technologies and imposes costs on society, while suppressing more sustainable alternatives. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear and fossil fuel (with CCS) industries are clear beneficiaries, receiving policy support and investment. Grid operators, while serving a coordination function, also benefit from a simpler grid management problem. Renewable energy advocates, taxpayers, and environmental justice communities bear the costs, either through diverted investment, direct subsidies, or environmental burdens. The 'baseload necessity' framing itself acts as a constraint on alternative, potentially cheaper or more equitable, decarbonization pathways.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (reliable decarbonization) is still live, but its *method* (baseload necessity) is contested. The classification as a Tangled Rope reflects that there is a genuine coordination problem (grid stability), but it is coupled with asymmetric extraction benefiting specific industries. This prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination challenge).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity_empirical_validity,
    'To what extent is the claim that renewables cannot provide dispatchable baseload power at scale empirically valid, considering advancements in storage and grid management technologies?',
    'Long-term empirical data from grids with high renewable penetration and advanced storage/demand-side management, coupled with independent engineering assessments of future technological potential.',
    'If renewables plus storage are proven capable, the ''baseload necessity'' claim''s empirical grounding weakens, shifting the constraint towards a Snare by exposing its coordination story as cover for extraction. If the claim holds, the constraint''s coordination function is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_validity, empirical, 'Empirical validity of the baseload necessity claim.').

omega_variable(
    cost_benefit_analysis_of_baseload_alternatives,
    'What is the true societal cost-benefit of prioritizing dispatchable baseload (e.g., nuclear, CCS) versus investing in advanced grid flexibility, storage, and demand-side management for decarbonization?',
    'Comprehensive, independent economic and social cost-benefit analyses that internalize externalities (e.g., nuclear waste, CCS leakage risk, environmental justice impacts) and compare across technology pathways.',
    'If baseload alternatives are found to be significantly more cost-effective or equitable, the constraint''s legitimacy as a coordination mechanism erodes, increasing its effective extractiveness and potentially reclassifying it as a Snare. If baseload is genuinely optimal, its Rope-like qualities are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_analysis_of_baseload_alternatives, preference, 'Societal cost-benefit of baseload vs. flexibility alternatives.').

omega_variable(
    framing_of_grid_reliability_risk,
    'Is the emphasis on ''baseload necessity'' a genuine technical requirement for grid reliability, or a framing device used by incumbent industries to maintain market share and policy support?',
    'Analysis of lobbying efforts, policy language, and media narratives from baseload industries versus independent grid modeling and academic studies on reliability solutions.',
    'If primarily a framing device, the constraint''s theater ratio would be higher, and its suppression of renewable alternatives would be seen as more deliberate, pushing it towards a Snare. If a genuine technical requirement, the coordination function is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_grid_reliability_risk, conceptual, 'Technical requirement vs. industry framing of grid reliability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2030, 0.25).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2040, 0.3).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2050, 0.35).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2030, 0.52).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2040, 0.58).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2050, 0.65).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2030, 0.68).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2040, 0.75).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2050, 0.8).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_investment_policy).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_waste_disposal_regulations).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, carbon_capture_subsidies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
