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
 *   human_readable: Climate Policy: Adaptation Priority (2-3°C Warming Accepted)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents a specific reading of global climate response
 *   obligations, prioritizing adaptation to an accepted 2-3°C warming over
 *   costly prevention (mitigation). It frames this approach as pragmatic and
 *   economically responsible, while implicitly shifting the burden of climate
 *   impacts to future generations and climate-vulnerable regions. This story
 *   instantiates the 'adaptation_priority' reading of the
 *   'climate_response_obligation' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.78).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.65).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Climate Policy: Adaptation Priority (2-3°C Warming Accepted)").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, 'db7cb80a-385d-4129-92c9-d006e340da9d').
narrative_ontology:cs_kernel_codification('db7cb80a-385d-4129-92c9-d006e340da9d', formalized).
narrative_ontology:cs_authority_grounding('db7cb80a-385d-4129-92c9-d006e340da9d', extraction).
narrative_ontology:cs_interpretation_layer_present('db7cb80a-385d-4129-92c9-d006e340da9d').
narrative_ontology:cs_reading_relation('db7cb80a-385d-4129-92c9-d006e340da9d', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('db7cb80a-385d-4129-92c9-d006e340da9d', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('db7cb80a-385d-4129-92c9-d006e340da9d', foundational, economic_growth_is_paramount).
narrative_ontology:cs_axiom_status(economic_growth_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('db7cb80a-385d-4129-92c9-d006e340da9d', economic_growth_is_paramount, instrumental).
narrative_ontology:cs_axiom('db7cb80a-385d-4129-92c9-d006e340da9d', foundational, climate_impacts_are_manageable_through_technology_and_investment).
narrative_ontology:cs_axiom_status(climate_impacts_are_manageable_through_technology_and_investment, holdable).
narrative_ontology:cs_axiom_grounding('db7cb80a-385d-4129-92c9-d006e340da9d', climate_impacts_are_manageable_through_technology_and_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('db7cb80a-385d-4129-92c9-d006e340da9d', status_quo_economic_growth_paradigm).
narrative_ontology:cs_drift_state('db7cb80a-385d-4129-92c9-d006e340da9d', contemporary_climate_crisis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('db7cb80a-385d-4129-92c9-d006e340da9d', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, high_carbon_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, climate_vulnerable_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prioritizes economic stability and avoids costly, immediate transitions to decarbonization. Invests in domestic resilience measures, shifting the burden of unmitigated warming to others.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_wealthy_nations, agenda_setter,
    institutional, generational, arbitrage, global).

% Protected from rapid decarbonization policies, allowing continued operation and profitability. Benefits from the framing that prevention is too costly.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_fuel_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Maintain current lifestyles and consumption patterns, avoiding immediate behavioral changes or significant carbon taxes that would be required by aggressive mitigation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, high_carbon_consumers, beneficiary,
    moderate, biographical, mobile, global).

% Bear the brunt of unmitigated climate impacts, inheriting a more dangerous and resource-constrained world due to insufficient prevention efforts by prior generations.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Face disproportionate climate impacts with limited capacity for self-funded resilience. Receive insufficient adaptation aid, and their calls for climate justice and mitigation are often marginalized.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_nations, payer,
    organized, generational, constrained, global).

% Directly suffer from extreme weather events, sea-level rise, and resource scarcity. Have limited means to adapt, relocate, or influence policy decisions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_vulnerable_communities, payer,
    powerless, immediate, trapped, local).

% Advocate for rapid decarbonization and systemic change to prevent warming, but their policy proposals are deemed too costly or politically infeasible by dominant actors.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, mitigation_advocates, excluded,
    organized, biographical, constrained, global).

% Propose fundamental economic restructuring to reduce material throughput and stay within planetary boundaries, but their ideas are dismissed as radical and unrealistic by the prevailing economic paradigm.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, degrowth_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global and national efforts around managing the *consequences* of climate change through resilience investments, rather than preventing it, thereby allowing continued economic growth in developed nations.
% TRANSFER_FUNCTION: Transfers the primary burden of climate change impacts and long-term costs from current generations and high-emitting industries to future generations and climate-vulnerable regions.
% ABSENT_VOICES: Future generations, who bear the greatest costs, have no direct voice. Global South nations and climate-vulnerable communities are present but often outvoted or under-resourced, their concerns marginalized by the dominant economic powers. Advocates for aggressive mitigation and degrowth are sidelined.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the global climate policy landscape would shift dramatically towards more aggressive mitigation and potentially degrowth strategies, reallocating resources and responsibilities, and fundamentally altering economic priorities and investment flows.
% FOUNDING_PROBLEM: The perceived high economic and political cost of rapid decarbonization and systemic change, coupled with the inertia of existing economic systems and the desire to maintain current living standards.
% FOUNDING_PROBLEM_CORROBORATION: Economic models from institutions aligned with current growth paradigms, political statements from powerful nations, and industry lobbying groups corroborate the high cost of mitigation. Independent scientific bodies and Global South representatives contest this framing, arguing the costs of inaction are far higher and that mitigation is economically viable.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high and increasing because the policy choice to accept significant warming imposes severe, uncompensated costs on victims. Suppression is moderate-high as it requires actively sidelining alternative, more aggressive mitigation or degrowth policies. Theater ratio is moderate-low; adaptation investments are real, but the framing can be performative in avoiding deeper systemic change. Resistance is high from those who bear the costs or advocate for different approaches.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries perceive this as a pragmatic, cost-effective, and politically feasible response to an 'inevitable' challenge. Victims, however, experience it as an unjust imposition of costs, a failure of intergenerational and global equity, and a catastrophic abdication of responsibility. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Current wealthy nations, the fossil fuel industry, and high-carbon consumers are beneficiaries, avoiding immediate economic disruption and maintaining existing systems. Future generations, Global South nations, and climate-vulnerable communities are victims, bearing the disproportionate impacts of a warmer world without adequate preventative action. Mitigation and degrowth advocates are excluded, their policy alternatives suppressed by the dominant narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is an active policy choice, not a degraded one. Its mandate is to manage climate impacts while preserving existing economic structures, which is a live and contested function, not an atrophied one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''adaptation_priority'' reading of the ''climate_response_obligation'' kernel?',
    'Analysis of policy documents, political discourse, and resource allocation patterns to confirm the explicit or implicit acceptance of 2-3°C warming and the prioritization of adaptation over prevention.',
    'If misidentified, the entire structural analysis of this constraint''s relationship to other climate policy readings would be incorrect, leading to false conclusions about inter-reading dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed within the climate response kernel.').

omega_variable(
    inevitability_of_warming_ambiguity,
    'Is the ''inevitability'' of 2-3°C warming a natural scientific fact, or a political construct used to justify inaction on mitigation?',
    'Expert scientific consensus on climate tipping points and remaining carbon budgets, contrasted with political feasibility assessments and economic modeling assumptions.',
    'If politically constructed, the constraint''s extractiveness and suppression would be higher, as the ''inevitability'' narrative serves to legitimize the transfer of costs to victims and suppress alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_of_warming_ambiguity, empirical, 'Distinguishes between scientific inevitability and political framing of climate outcomes.').

omega_variable(
    cost_benefit_of_adaptation_vs_mitigation,
    'What is the true long-term cost-benefit ratio of prioritizing adaptation over aggressive mitigation, considering all externalities and intergenerational equity?',
    'Comprehensive economic modeling that fully internalizes climate damages, ecosystem services, and social costs across generations, not just short-term economic impacts.',
    'If mitigation proves significantly more cost-effective in the long run, the ''costly prevention'' argument supporting this constraint would be undermined, reclassifying it as more purely extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_benefit_of_adaptation_vs_mitigation, empirical, 'Evaluates the economic rationale for adaptation priority against full-cost accounting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__adaptation_priority, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2025, climate_response_obligation__adaptation_priority, theater_ratio, 2025, 0.21).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__adaptation_priority, theater_ratio, 2030, 0.22).
narrative_ontology:measurement(clim_tr_t2035, climate_response_obligation__adaptation_priority, theater_ratio, 2035, 0.23).
narrative_ontology:measurement(clim_tr_t2040, climate_response_obligation__adaptation_priority, theater_ratio, 2040, 0.24).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__adaptation_priority, theater_ratio, 2050, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__adaptation_priority, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(clim_be_t2025, climate_response_obligation__adaptation_priority, base_extractiveness, 2025, 0.7).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__adaptation_priority, base_extractiveness, 2030, 0.74).
narrative_ontology:measurement(clim_be_t2035, climate_response_obligation__adaptation_priority, base_extractiveness, 2035, 0.76).
narrative_ontology:measurement(clim_be_t2040, climate_response_obligation__adaptation_priority, base_extractiveness, 2040, 0.77).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__adaptation_priority, base_extractiveness, 2050, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__adaptation_priority, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(clim_su_t2025, climate_response_obligation__adaptation_priority, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__adaptation_priority, suppression_requirement, 2030, 0.61).
narrative_ontology:measurement(clim_su_t2035, climate_response_obligation__adaptation_priority, suppression_requirement, 2035, 0.63).
narrative_ontology:measurement(clim_su_t2040, climate_response_obligation__adaptation_priority, suppression_requirement, 2040, 0.64).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__adaptation_priority, suppression_requirement, 2050, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_subsidies).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, international_climate_finance).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, global_carbon_markets).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
