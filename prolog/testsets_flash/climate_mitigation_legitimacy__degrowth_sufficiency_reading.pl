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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth Sufficiency Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth sufficiency' reading of climate
 *   mitigation legitimacy, asserting that decarbonization fundamentally
 *   requires demand reduction, thereby rendering large-scale generation
 *   expansion (including both nuclear and renewables) unnecessary. It frames
 *   the problem as one of overconsumption and systemic growth dependence,
 *   rather than solely a technological challenge. This reading places both
 *   nuclear and large-scale renewable energy development in the 'victim'
 *   category, as their expansion is seen as perpetuating unsustainable growth
 *   paradigms.
 *
 * KEY AGENTS:
 *   - degrowth_advocates: Primary beneficiary (institutional/arbitrage) — promotes a systemic shift that benefits their worldview.
 *   - local_resilience_movements: Secondary beneficiary (organized/mobile) — aligns with their goals of localized, low-consumption economies.
 *   - nuclear_industry: Primary victim (institutional/constrained) — faces direct opposition to its expansion plans.
 *   - renewable_energy_developers: Secondary victim (organized/constrained) — their large-scale projects are deemed unnecessary or counterproductive.
 *   - economic_growth_advocates: Victim (institutional/constrained) — their core premise of continuous growth is challenged.
 *   - climate_scientists: Observer (analytical/analytical) — provide data on climate targets and energy system models, but do not endorse specific policy pathways.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.45).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth Sufficiency Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '2d56c4d0-70c9-40bd-a3d3-86493a7e30d6').
narrative_ontology:cs_kernel_codification('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', distributed).
narrative_ontology:cs_authority_grounding('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', diffuse_epistemic).
narrative_ontology:cs_reading_relation('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', foundational, demand_reduction_is_necessary_for_decarbonization).
narrative_ontology:cs_axiom_status(demand_reduction_is_necessary_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', demand_reduction_is_necessary_for_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', foundational, large_scale_generation_expansion_is_unnecessary).
narrative_ontology:cs_axiom_status(large_scale_generation_expansion_is_unnecessary, holdable).
narrative_ontology:cs_axiom_grounding('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', large_scale_generation_expansion_is_unnecessary, empirically_contingent).
narrative_ontology:cs_reference_frame('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', sufficiency_based_decarbonization).
narrative_ontology:cs_drift_state('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('2d56c4d0-70c9-40bd-a3d3-86493a7e30d6', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_movements).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, economic_growth_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal efforts towards decarbonization by focusing on demand reduction and sufficiency, aiming to avoid the need for massive, potentially disruptive, energy infrastructure expansion.
% TRANSFER_FUNCTION: Transfers societal resources and political will away from large-scale energy generation projects (nuclear, large renewables) and towards policies promoting energy efficiency, reduced consumption, and localized energy systems. It also transfers the burden of climate action from technological innovation to behavioral and systemic change.
% ABSENT_VOICES: The voices of those who believe in technological solutions for decarbonization without fundamental lifestyle changes, or those whose livelihoods depend on the growth of the energy sector, are often marginalized or dismissed as 'growth-addicted' within this framework. They would argue for the necessity of large-scale energy projects and the economic benefits of growth.
% DISAPPEARANCE_RATIONALE: If this reading of climate mitigation legitimacy disappeared, the policy landscape would immediately shift towards prioritizing large-scale energy generation (renewables, nuclear, or a mix) as the primary decarbonization strategy. Investment flows, research priorities, and public discourse would reorient around supply-side solutions, and demand reduction would likely become a secondary, efficiency-focused goal rather than a foundational principle.
% FOUNDING_PROBLEM: The founding problem was the perceived unsustainability of continuous economic growth and its inherent conflict with ecological limits, particularly in the context of climate change. It sought to address the root causes of emissions beyond just technological fixes.
% FOUNDING_PROBLEM_CORROBORATION: The problem of unsustainable growth and its ecological impact is widely attested by ecological economists, environmental scientists, and various international reports (e.g., IPCC reports on consumption patterns). While the 'necessity' of degrowth as a solution is contested, the underlying problem it addresses is corroborated by sources outside its direct beneficiaries.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it offers a genuine coordination function (a coherent pathway to decarbonization via demand reduction) but simultaneously extracts from and suppresses alternative, growth-oriented energy strategies. Extractiveness (0.65) is high because it demands significant societal restructuring and economic contraction from those committed to growth. Suppression (0.45) is moderate but active, as it requires policy and cultural shifts to actively disincentivize consumption and large-scale energy projects. The resistance (0.70) is high due to strong opposition from industries and political factions committed to economic growth and technological solutions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates, this is a necessary and beneficial reorientation (beneficiary seat). From the perspective of the nuclear and large-scale renewable industries, it is an extractive and suppressive ideology that undermines their legitimate role in decarbonization (victim seats). The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth advocates and local resilience movements are beneficiaries (low d) as the constraint aligns with and legitimizes their core tenets. The nuclear industry, renewable energy developers, and economic growth advocates are victims (high d) as the constraint directly targets their operational models and foundational assumptions, making their expansion unnecessary or undesirable. The constraint subsidizes the degrowth agenda by framing it as a necessary condition for climate action.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (decarbonization) is very much live. However, the 'necessity' of demand reduction is contested. If future evidence shows that decarbonization can be achieved without significant demand reduction (e.g., through technological breakthroughs), the 'demand reduction is necessary' component of this constraint would become a Snare, persisting only to benefit degrowth advocates by suppressing alternatives, rather than solving a genuine coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_empirical_feasibility,
    'Is large-scale demand reduction empirically achievable within the necessary timeframe to meet climate targets without severe economic disruption?',
    'Empirical studies on demand elasticity, behavioral change, and economic modeling of degrowth scenarios; pilot programs demonstrating sustained, voluntary demand reduction.',
    'If empirically feasible, this reading gains significant legitimacy, potentially shifting policy towards sufficiency. If not, its claims become more extractive, demanding sacrifices without clear pathways, and it would be reclassified as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_empirical_feasibility, empirical, 'The empirical feasibility of degrowth as a primary decarbonization strategy.').

omega_variable(
    climate_mitigation_legitimacy_kernel_reading,
    'Is this constraint a genuine requirement for decarbonization, or a policy preference framed as a necessity?',
    'Analysis of the underlying climate science and energy system models to determine if demand reduction is a necessary condition for decarbonization, or merely one possible pathway among others.',
    'If a necessary condition, the constraint is closer to a Mountain; if a policy preference, it is a constructed constraint (Tangled Rope or Snare) reflecting the values of its beneficiaries. This reading (degrowth_sufficiency_reading) is one of four competing interpretations of the climate_mitigation_legitimacy kernel. Sibling readings (baseload_necessity_reading, renewable_primacy_reading, portfolio_pragmatism_reading) would prioritize different energy sources and scales of deployment, making both nuclear and renewables beneficiaries rather than victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_mitigation_legitimacy_kernel_reading, conceptual, 'This constraint is the ''degrowth_sufficiency_reading'' of the ''climate_mitigation_legitimacy'' kernel. Sibling readings would shift the beneficiary/victim sets and the core claims about necessary energy technologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 15, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_legitimacy' kernel. Its core premise (demand reduction is necessary) directly influences the legitimacy and resource allocation for other energy technology pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
