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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy in Decarbonization Policy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'renewable primacy' reading within the
 *   broader 'climate_mitigation_legitimacy' kernel. It asserts that a
 *   strategy focused solely on renewables plus storage is the fastest and
 *   cheapest path to full decarbonization, implicitly framing nuclear power
 *   as an unnecessary and costly diversion. This reading drives policy
 *   decisions that prioritize renewable deployment and disincentivize nuclear
 *   investment, creating a structural advantage for renewable developers and
 *   a disadvantage for the nuclear industry.
 *
 * KEY AGENTS:
 *   - renewable_energy_developers: Primary beneficiary (institutional/arbitrage) — benefits from policy prioritization and capital allocation.
 *   - battery_storage_manufacturers: Primary beneficiary (institutional/arbitrage) — benefits from increased demand for storage solutions.
 *   - environmental_advocacy_groups: Beneficiary (organized/mobile) — aligns with their policy goals and receives political capital.
 *   - nuclear_power_industry: Primary victim (institutional/constrained) — faces policy disincentives, reduced investment, and public skepticism.
 *   - fossil_fuel_industry: Victim (institutional/trapped) — faces accelerated phase-out due to renewable expansion.
 *   - traditional_grid_operators: Victim (institutional/constrained) — faces challenges adapting to decentralized, intermittent generation.
 *   - policy_makers: Agenda setter (institutional/mobile) — enacts policies reflecting this reading, allocating subsidies and regulatory support.
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
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy in Decarbonization Policy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '34d9720e-8146-4bc4-8fd0-00c31ef913ab').
narrative_ontology:cs_kernel_codification('34d9720e-8146-4bc4-8fd0-00c31ef913ab', distributed).
narrative_ontology:cs_authority_grounding('34d9720e-8146-4bc4-8fd0-00c31ef913ab', expertise).
narrative_ontology:cs_interpretation_layer_present('34d9720e-8146-4bc4-8fd0-00c31ef913ab').
narrative_ontology:cs_reading_relation('34d9720e-8146-4bc4-8fd0-00c31ef913ab', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('34d9720e-8146-4bc4-8fd0-00c31ef913ab', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_reading_relation('34d9720e-8146-4bc4-8fd0-00c31ef913ab', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('34d9720e-8146-4bc4-8fd0-00c31ef913ab', foundational, renewable_cost_declines_indefinite).
narrative_ontology:cs_axiom_status(renewable_cost_declines_indefinite, holdable).
narrative_ontology:cs_axiom_grounding('34d9720e-8146-4bc4-8fd0-00c31ef913ab', renewable_cost_declines_indefinite, empirically_contingent).
narrative_ontology:cs_axiom('34d9720e-8146-4bc4-8fd0-00c31ef913ab', foundational, nuclear_inherently_slow_expensive).
narrative_ontology:cs_axiom_status(nuclear_inherently_slow_expensive, holdable).
narrative_ontology:cs_axiom_grounding('34d9720e-8146-4bc4-8fd0-00c31ef913ab', nuclear_inherently_slow_expensive, empirically_contingent).
narrative_ontology:cs_reference_frame('34d9720e-8146-4bc4-8fd0-00c31ef913ab', rapid_cost_effective_decarbonization_via_renewables).
narrative_ontology:cs_drift_state('34d9720e-8146-4bc4-8fd0-00c31ef913ab', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('34d9720e-8146-4bc4-8fd0-00c31ef913ab', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_power_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, traditional_grid_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Companies developing and deploying solar, wind, and geothermal projects. They benefit from policy support, subsidies, and public perception that prioritizes their technologies, leading to increased investment and market share.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers, beneficiary,
    institutional, biographical, arbitrage, global).

% Companies producing grid-scale and distributed battery storage solutions. They see massive market growth and R&D funding channeled to their sector as a direct consequence of policies favoring intermittent renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Organizations advocating for climate action and renewable energy. This reading aligns with their policy goals, enhancing their political influence and fundraising capabilities by promoting a clear, 'clean' energy pathway.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, environmental_advocacy_groups, beneficiary,
    organized, generational, mobile, global).

% Companies involved in designing, building, and operating nuclear power plants. They face policy disincentives, reduced access to capital, and public skepticism, leading to project cancellations and a shrinking market share, despite their low-carbon output.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_power_industry, payer,
    institutional, generational, constrained, national).

% Companies involved in the extraction, processing, and burning of coal, oil, and natural gas. This constraint accelerates the phase-out of their core business, leading to stranded assets and declining profitability, with limited options for transition.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_industry, payer,
    institutional, biographical, trapped, global).

% Utilities and independent system operators responsible for maintaining grid stability and reliability. They face significant technical and financial challenges in integrating high levels of intermittent renewables, requiring costly upgrades and new operational paradigms.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, traditional_grid_operators, payer,
    institutional, generational, constrained, national).

% Government officials and legislative bodies responsible for setting energy policy and allocating public funds. They actively promote and enforce policies that align with the renewable primacy reading, influencing market signals and regulatory frameworks.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% Academics and researchers analyzing the costs, benefits, and feasibility of different energy pathways. They provide independent analysis that can either corroborate or challenge the claims of renewable primacy, influencing public and policy discourse.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate national and international efforts towards rapid decarbonization by focusing resources and policy on a specific, perceived optimal technological pathway (renewables plus storage).
% TRANSFER_FUNCTION: Transfers capital, policy support, and public legitimacy from nuclear and fossil fuel industries to renewable energy developers and battery manufacturers, based on the claim of superior speed and cost-effectiveness for decarbonization.
% ABSENT_VOICES: Proponents of nuclear power, who argue for its baseload capacity and energy density, are often marginalized in policy discussions dominated by the renewable primacy narrative. Their arguments for a technology-neutral approach or nuclear's role in deep decarbonization are suppressed by the framing of nuclear as 'too slow' or 'too expensive'.
% DISAPPEARANCE_RATIONALE: If the belief in renewable primacy vanished overnight, the energy policy landscape would fundamentally shift. Investment would diversify, nuclear projects might be re-evaluated, and the rapid, focused deployment of renewables would lose its primary ideological driver, leading to a more varied and potentially slower decarbonization pathway.
% FOUNDING_PROBLEM: The urgent need for rapid and cost-effective decarbonization to address climate change, coupled with the perceived slowness and high cost of traditional nuclear power deployment.
% FOUNDING_PROBLEM_CORROBORATION: The urgency of climate change is widely corroborated by scientific consensus (IPCC, national academies). The perceived cost and speed advantages of renewables are corroborated by numerous economic analyses and market trends from independent energy economists and international agencies (e.g., IRENA, Lazard LCOE reports), though these are often contested by nuclear industry advocates and some energy system modelers.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates rapid decarbonization efforts (a collective action problem) while simultaneously extracting from and suppressing alternative energy technologies, particularly nuclear. Extractiveness (0.65) is high because capital and policy support are diverted from nuclear, creating a cost for that industry. Suppression (0.70) is high due to active policy mechanisms (e.g., exclusion from clean energy standards, reduced R&D funding) that limit nuclear's market access and public legitimacy. Theater ratio (0.20) is low, as the policy actions are genuinely aimed at decarbonization, even if the specific pathway is contested. The increasing extractiveness and suppression over time reflect the hardening of policy positions and the growing momentum behind the renewable-only narrative.
 *
 * PERSPECTIVAL GAP:
 *   Policy makers and renewable developers experience this as a Rope, a necessary coordination mechanism to address climate change efficiently. The nuclear industry and traditional grid operators experience it as a Snare, as it actively undermines their viability and forces costly adaptations. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers and battery manufacturers are clear beneficiaries (d=0.0-0.1) as the constraint directly channels resources and policy support to them. Environmental advocacy groups are also beneficiaries (d=0.1-0.2) as it aligns with their mission. The nuclear power industry is a primary target (d=0.9-1.0) as the constraint actively disfavors it. The fossil fuel industry is also a target (d=0.8-0.9) due to the accelerated transition. Traditional grid operators are targets (d=0.7-0.8) due to the disruptive changes required. Policy makers, as agenda setters, are near symmetric (d=0.4-0.5) as they balance various interests while pursuing decarbonization goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the extraction from nuclear) or a pure Snare (ignoring the genuine coordination function for decarbonization). The 'tangled_rope' classification highlights that the coordination for rapid decarbonization is achieved through a mechanism that also creates asymmetric costs and suppresses alternatives, which is precisely the dynamic the framework is designed to detect. The 'contested' status of the founding problem further supports this, indicating that the original problem (fragmented decarbonization efforts) is now being addressed in a way that creates new forms of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_cost_trajectory_uncertainty,
    'Will the cost trajectories of renewables+storage continue to outpace nuclear, or will nuclear costs decline faster than projected?',
    'Longitudinal empirical data on levelized cost of energy (LCOE) for new build nuclear vs. renewables+storage over the next decade, including grid integration costs.',
    'If nuclear costs decline faster, the ''cheaper'' claim of renewable primacy weakens, potentially shifting policy towards a more diversified portfolio. If renewable costs continue to fall, the primacy claim strengthens, leading to further policy support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_cost_trajectory_uncertainty, empirical, 'Uncertainty in future cost competitiveness between renewable+storage and nuclear technologies.').

omega_variable(
    grid_stability_at_high_penetration,
    'Can grid infrastructure reliably integrate very high penetrations of intermittent renewables with current storage technologies without significant stability issues or prohibitive costs?',
    'Pilot projects and full-scale deployments in regions aiming for 80-100% renewable grids, with detailed analysis of grid stability, curtailment rates, and system-wide costs.',
    'If grid stability proves challenging or prohibitively expensive, the ''faster'' claim of renewable primacy is undermined, potentially re-legitimizing dispatchable baseload sources like nuclear. If successful, it reinforces the primacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_at_high_penetration, empirical, 'Uncertainty regarding the technical feasibility and cost of 100% renewable grid stability.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine empirical claim about energy economics, or a normative commitment within the ''climate_mitigation_legitimacy'' kernel, specifically the ''renewable_primacy_reading''?',
    'Analysis of policy advocacy: if the claim persists despite contradictory empirical evidence, it functions as a normative commitment. If it adapts to new data, it''s an empirical claim.',
    'If primarily a normative commitment, its persistence is less about empirical truth and more about identity and values, making it more resistant to empirical challenge. If empirical, it''s more susceptible to data-driven policy shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''renewable_primacy_reading'' of the ''climate_mitigation_legitimacy'' kernel, asserting that renewables plus storage are the fastest and cheapest path to full decarbonization, implicitly foreclosing nuclear as a primary solution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, energy_grid_stability_standards).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_waste_disposal_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
