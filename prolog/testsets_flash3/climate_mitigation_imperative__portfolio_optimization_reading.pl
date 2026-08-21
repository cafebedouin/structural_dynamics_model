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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation Imperative: Portfolio Optimization Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio optimization' reading of the
 *   climate mitigation imperative, which asserts that all low-carbon energy
 *   sources, including nuclear, must be maximized to achieve climate goals
 *   and ensure grid reliability. It positions nuclear power as a necessary
 *   component for baseload generation. This reading is distinct from others
 *   that prioritize cost-effectiveness (opportunity_cost_reading) or systemic
 *   transformation (systems_transition_reading).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.45).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.3).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation Imperative: Portfolio Optimization Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '5fb6b15e-5d3b-42bd-8a77-55d7f82841ab').
narrative_ontology:cs_kernel_codification('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', implicit).
narrative_ontology:cs_authority_grounding('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', expertise).
narrative_ontology:cs_interpretation_layer_present('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab').
narrative_ontology:cs_reading_relation('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', foundational, all_low_carbon_sources_are_necessary).
narrative_ontology:cs_axiom_status(all_low_carbon_sources_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', all_low_carbon_sources_are_necessary, empirically_contingent).
narrative_ontology:cs_axiom('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', foundational, nuclear_provides_essential_baseload).
narrative_ontology:cs_axiom_status(nuclear_provides_essential_baseload, holdable).
narrative_ontology:cs_axiom_grounding('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', nuclear_provides_essential_baseload, empirically_contingent).
narrative_ontology:cs_reference_frame('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', technology_neutral_decarbonization).
narrative_ontology:cs_drift_state('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5fb6b15e-5d3b-42bd-8a77-55d7f82841ab', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, climate_scientists).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, renewable_only_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives policy support, subsidies, and R&D funding under the premise that nuclear power is a necessary low-carbon baseload source for climate mitigation. Benefits from inclusion in 'all-of-the-above' energy strategies.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    organized, generational, constrained, global).

% Benefit from the stability and reliability that nuclear baseload provides to the electrical grid, especially as intermittent renewables grow. They advocate for a diverse energy portfolio to maintain grid resilience.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, beneficiary,
    institutional, biographical, constrained, national).

% Advocate for rapid decarbonization based on scientific consensus regarding climate change. From this reading, they see nuclear as a proven, scalable low-carbon technology that must be part of the solution to meet emissions targets.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_scientists, beneficiary,
    analytical, civilizational, analytical, universal).

% Faces increasing regulatory pressure, carbon pricing, and divestment as policies prioritize low-carbon sources. This constraint directly targets their market share and long-term viability.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry, payer,
    institutional, biographical, constrained, global).

% Bear the cost of policy and public attention diverted to nuclear, which they argue is too slow, expensive, and risky compared to renewables. They see nuclear as competing for finite resources and political will.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_only_advocates, payer,
    organized, generational, constrained, global).

% Responsible for designing and implementing climate mitigation strategies. This reading guides them to support a diverse portfolio of low-carbon technologies, including nuclear, through legislation, subsidies, and regulatory frameworks.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, policymakers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national and global efforts to decarbonize energy systems by identifying and promoting all available low-carbon technologies, ensuring grid stability while reducing emissions.
% TRANSFER_FUNCTION: Directs public and private investment, R&D funding, and regulatory support towards nuclear power and other low-carbon sources, shifting resources away from fossil fuels.
% ABSENT_VOICES: Communities disproportionately affected by nuclear waste storage or mining, and those advocating for purely decentralized energy systems, are often marginalized in high-level policy discussions focused on national grid stability and emissions targets.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, the policy landscape would fragment. Nuclear power would lose its 'necessary' status, likely leading to reduced investment and a slower phase-out of fossil fuels, as the unified strategic rationale for a diverse low-carbon portfolio would be lost.
% FOUNDING_PROBLEM: The urgent need to mitigate climate change by rapidly reducing greenhouse gas emissions while maintaining energy security and grid reliability.
% FOUNDING_PROBLEM_CORROBORATION: International climate bodies (e.g., IPCC, IEA) and national energy agencies consistently corroborate the live status of the climate mitigation problem and the need for diverse low-carbon solutions, including nuclear, to meet targets. This is attested by scientific reports and energy outlooks from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).
:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the redirection of resources and the imposition of costs on fossil fuels and, indirectly, on renewable-only strategies. Suppression (0.30) is moderate, as it involves policy frameworks that disincentivize high-carbon sources and promote low-carbon alternatives, but does not fully eliminate dissent or alternative approaches. Theater ratio (0.10) is low, as the core function of decarbonization is genuinely pursued, though the specific 'necessity' of nuclear is debated. The metrics are projected to increase slightly as the imperative hardens and opposition to fossil fuels intensifies.
 *
 * PERSPECTIVAL GAP:
 *   While policymakers and nuclear proponents see this as a pragmatic, necessary coordination, renewable-only advocates perceive it as an extractive diversion of resources and political capital from faster, cheaper solutions. The engine's classification will highlight this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry, grid operators, and climate scientists (from this reading's perspective) are beneficiaries, receiving support and validation. The fossil fuel industry is a clear victim, facing direct policy pressure. Renewable-only advocates are also victims, as their preferred strategy is diluted by the inclusion of nuclear, which they view as a less efficient or desirable path. Policymakers act as agenda-setters, implementing the portfolio optimization strategy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_cost_effectiveness,
    'Is nuclear power truly cost-effective and timely enough to contribute meaningfully to climate mitigation targets compared to alternative low-carbon sources?',
    'Comparative lifecycle cost analysis, including construction time, operational costs, and waste disposal, against a portfolio of renewables + storage, under various grid integration scenarios.',
    'If nuclear is found to be significantly less cost-effective or slower to deploy, the ''necessity'' claim of this reading would be weakened, potentially shifting policy support towards other low-carbon options and reducing the extractiveness from renewable-only advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_cost_effectiveness, empirical, 'Empirical uncertainty regarding nuclear''s economic and temporal viability for climate goals.').

omega_variable(
    grid_reliability_alternatives,
    'Are there alternative, non-nuclear solutions for baseload power and grid reliability that could achieve similar outcomes without the associated costs and risks of nuclear?',
    'Technological advancements in energy storage, smart grid management, and long-distance transmission, coupled with modeling of future energy systems without nuclear.',
    'If robust, cost-effective alternatives emerge, the ''necessity'' of nuclear for baseload would be conceptually challenged, potentially leading to a re-evaluation of this reading''s core premise and a reduction in support for the nuclear industry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_reliability_alternatives, conceptual, 'Conceptual uncertainty about the uniqueness of nuclear''s contribution to grid reliability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2030, 0.12).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2040, 0.13).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2050, 0.14).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2030, 0.48).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2040, 0.5).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2050, 0.52).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2030, 0.33).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2040, 0.35).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2050, 0.37).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_mitigation_imperative' kernel. This 'portfolio_optimization_reading' emphasizes maximizing all low-carbon sources, including nuclear, for climate goals and grid stability. It is linked to the 'opportunity_cost_reading' (nuclear is too expensive/slow) and the 'systems_transition_reading' (nuclear perpetuates centralization) as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
