% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Climate Adaptation Priority Doctrine
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the policy doctrine that accepts 2-3°C global
 *   warming as inevitable and prioritizes investment in adaptation and
 *   resilience over costly, rapid prevention (mitigation). It is a specific
 *   reading of the broader 'climate_response_obligation' kernel, which
 *   encompasses various approaches to addressing climate change. This reading
 *   shifts the burden of climate impacts to future generations and
 *   climate-vulnerable regions, while protecting current economic interests
 *   and high-carbon industries. The claimed type is 'tangled_rope' because it
 *   offers a coordination function (managing inevitable change) but with
 *   significant asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.82).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.78).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.82).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Climate Adaptation Priority Doctrine").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '38d67139-5f80-41b2-8190-4e2c707b3d54').
narrative_ontology:cs_kernel_codification('38d67139-5f80-41b2-8190-4e2c707b3d54', formalized).
narrative_ontology:cs_authority_grounding('38d67139-5f80-41b2-8190-4e2c707b3d54', extraction).
narrative_ontology:cs_interpretation_layer_present('38d67139-5f80-41b2-8190-4e2c707b3d54').
narrative_ontology:cs_reading_relation('38d67139-5f80-41b2-8190-4e2c707b3d54', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('38d67139-5f80-41b2-8190-4e2c707b3d54', climate_response_obligation__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('38d67139-5f80-41b2-8190-4e2c707b3d54', foundational, economic_growth_is_paramount).
narrative_ontology:cs_axiom_status(economic_growth_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('38d67139-5f80-41b2-8190-4e2c707b3d54', economic_growth_is_paramount, instrumental).
narrative_ontology:cs_axiom('38d67139-5f80-41b2-8190-4e2c707b3d54', foundational, adaptation_is_cost_effective_response).
narrative_ontology:cs_axiom_status(adaptation_is_cost_effective_response, holdable).
narrative_ontology:cs_axiom_grounding('38d67139-5f80-41b2-8190-4e2c707b3d54', adaptation_is_cost_effective_response, empirically_contingent).
narrative_ontology:cs_reference_frame('38d67139-5f80-41b2-8190-4e2c707b3d54', economic_pragmatism_framework).
narrative_ontology:cs_drift_state('38d67139-5f80-41b2-8190-4e2c707b3d54', contemporary_climate_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('38d67139-5f80-41b2-8190-4e2c707b3d54', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, high_carbon_consumers).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_industry).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, climate_vulnerable_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prioritizes economic stability and avoids costly immediate decarbonization, shifting resources to protect existing assets and infrastructure from climate impacts. Benefits from continued high-carbon economic activity.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_wealthy_nations, agenda_setter,
    institutional, biographical, arbitrage, global).

% Protected from rapid transition costs and regulatory burdens associated with aggressive mitigation, allowing continued operation and profitability. Actively lobbies for adaptation-focused policies.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_fuel_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Avoids immediate lifestyle changes, carbon taxes, and other costs associated with rapid decarbonization, maintaining current consumption patterns.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, high_carbon_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Profits from increased investment in resilience infrastructure, disaster preparedness, climate engineering, and other adaptation technologies and services.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, adaptation_industry, beneficiary,
    organized, biographical, mobile, global).

% Inherits a world with significantly higher global temperatures (2-3°C warming), increased climate impacts, and greater adaptation costs, having been denied the benefits of early prevention.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, universal).

% Bears a disproportionate share of climate change impacts (e.g., extreme weather, sea-level rise, resource scarcity) with fewer resources for adaptation, often relying on insufficient international aid.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_nations, payer,
    powerless, generational, trapped, global).

% Faces displacement, livelihood destruction, and increased mortality from climate impacts, with limited capacity to adapt or relocate.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_vulnerable_communities, payer,
    powerless, immediate, trapped, local).

% Advocates for stronger mitigation and systemic change, but their calls are often marginalized or dismissed as economically unfeasible within the dominant policy discourse.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_activists, excluded,
    organized, generational, constrained, global).

% Argues for rapid decarbonization and intergenerational justice, seeing adaptation-only as a moral hazard that locks in future suffering. Their policy proposals are deprioritized.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, mitigation_advocates, excluded,
    organized, generational, constrained, global).

% Argues for fundamental systemic change to reduce material throughput, viewing both adaptation and mitigation within a growth paradigm as insufficient. Their perspective is largely outside mainstream policy discussions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, degrowth_advocates, excluded,
    organized, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global and national efforts around managing the *effects* of climate change, rather than preventing it, by focusing resources on resilience, protective measures, and technological solutions to cope with 2-3°C warming.
% TRANSFER_FUNCTION: Transfers the burden of climate change from current high-emitting economies and generations to future generations and climate-vulnerable regions, while channeling significant investment into adaptation technologies and infrastructure, often benefiting specific industries.
% ABSENT_VOICES: Future generations, non-human species, and the most climate-vulnerable communities (often in the Global South) are largely absent from the decision-making processes that prioritize adaptation over prevention. Their interests are represented by advocates, but they lack direct agency and their concerns are systematically deprioritized.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, the global climate policy landscape would immediately shift towards more aggressive mitigation targets and intergenerational equity considerations, requiring a massive reallocation of capital, a re-evaluation of economic models, and a fundamental change in international relations regarding climate responsibility.
% FOUNDING_PROBLEM: The perceived high economic and social cost of rapid decarbonization, the political difficulty of achieving global consensus on stringent mitigation targets, and the desire to protect existing economic structures and consumption patterns.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., some economists, industry lobbyists, certain political factions) argue that mitigation is too costly and politically infeasible, making adaptation the only pragmatic path. Critics (e.g., climate scientists, ethicists, Global South representatives, climate activists) attest that the founding problem is often reframed to avoid responsibility, not that mitigation is impossible or that adaptation is sufficient; legislative-hearing testimony, scientific consensus reports, and independent economic analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the doctrine systematically externalizes the costs of climate change onto future generations and vulnerable populations, allowing current beneficiaries to avoid transition costs. Suppression is also high (0.78) as it actively marginalizes and suppresses alternative policy pathways like aggressive mitigation or degrowth through political discourse, economic arguments, and resource allocation. The theater ratio is moderate (0.25); while genuine adaptation efforts exist, some are performative or insufficient, serving to justify inaction on prevention. Accessibility collapse is moderate (0.60) as alternatives are presented as economically or politically unfeasible, though not entirely impossible. Resistance is high (0.70) from those who bear the costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of current wealthy nations and industries, this doctrine is a pragmatic and economically rational approach to an inevitable problem, a form of necessary coordination. From the perspective of future generations and vulnerable communities, it is a deeply unjust and extractive mechanism that prioritizes short-term gain over long-term survival and equity. The engine's classification will highlight this structural divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Current wealthy nations, the fossil fuel industry, high-carbon consumers, and the adaptation industry are the primary beneficiaries, avoiding immediate costs and profiting from the chosen response. Future generations, Global South nations, and climate-vulnerable communities are the primary targets/victims, bearing the disproportionate impacts and costs of a warmer world. Climate activists and mitigation/degrowth advocates are largely excluded from the policy-setting process, their alternatives suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a case of mandatrophy, as the 'mandate' to respond to climate change is very much alive. Instead, the classification helps to identify how the *interpretation* of that mandate has been shaped to serve specific interests, preventing the mislabeling of a highly extractive and suppressive policy as mere 'pragmatic coordination' or an 'inevitable' response. It reveals the active choices and power dynamics behind the framing of climate action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''adaptation_priority'' reading of the ''climate_response_obligation'' kernel?',
    'Analysis of policy documents, political discourse, and resource allocation patterns to confirm the explicit or implicit prioritization of adaptation over mitigation.',
    'If misidentified, the analysis of inter-reading relations and axiom contradictions would be flawed, leading to incorrect conclusions about the contest over climate policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading of the climate response kernel.').

omega_variable(
    structural_delta_assessment,
    'How precisely does this ''adaptation_priority'' reading shift costs and benefits compared to ''mitigation_priority'' or ''degrowth_reading''?',
    'Detailed economic modeling and ethical analysis comparing the distributional impacts of different climate policy pathways across generations and regions.',
    'More precise quantification of the structural delta would refine the extractiveness and suppression metrics, potentially altering the magnitude of the computed per-seat classifications for beneficiaries and victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_assessment, empirical, 'Quantifies the cost/benefit shift inherent in this reading.').

omega_variable(
    adaptation_efficacy_vs_cost,
    'Is the current level of investment in adaptation truly sufficient and cost-effective to manage 2-3°C warming, or is it a false economy that will lead to greater future costs?',
    'Long-term empirical data on climate impacts, the effectiveness of adaptation measures, and the actual costs incurred by vulnerable populations, compared against initial projections.',
    'If adaptation proves insufficient or more costly than projected, the extractiveness metric would be even higher, and the ''claimed_type'' might shift further towards ''snare'' as the coordination function becomes increasingly theatrical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_efficacy_vs_cost, empirical, 'Assesses the real-world effectiveness and cost-efficiency of adaptation strategies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_response_obligation__adaptation_priority, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(clim_tr_t1995, climate_response_obligation__adaptation_priority, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(clim_tr_t2000, climate_response_obligation__adaptation_priority, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(clim_tr_t2005, climate_response_obligation__adaptation_priority, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(clim_tr_t2010, climate_response_obligation__adaptation_priority, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__adaptation_priority, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__adaptation_priority, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_response_obligation__adaptation_priority, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(clim_be_t1995, climate_response_obligation__adaptation_priority, base_extractiveness, 1995, 0.69).
narrative_ontology:measurement(clim_be_t2000, climate_response_obligation__adaptation_priority, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(clim_be_t2005, climate_response_obligation__adaptation_priority, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement(clim_be_t2010, climate_response_obligation__adaptation_priority, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__adaptation_priority, base_extractiveness, 2015, 0.81).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__adaptation_priority, base_extractiveness, 2020, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_response_obligation__adaptation_priority, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(clim_su_t1995, climate_response_obligation__adaptation_priority, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(clim_su_t2000, climate_response_obligation__adaptation_priority, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(clim_su_t2005, climate_response_obligation__adaptation_priority, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(clim_su_t2010, climate_response_obligation__adaptation_priority, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__adaptation_priority, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__adaptation_priority, suppression_requirement, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_subsidies).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, carbon_emissions_targets).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, international_climate_aid).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings (adaptation_priority, mitigation_priority, degrowth_reading) of the 'climate_response_obligation' kernel. Each reading represents a distinct structural approach to climate policy with different beneficiaries, victims, and underlying assumptions. They are linked to show the contested nature of climate action.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
