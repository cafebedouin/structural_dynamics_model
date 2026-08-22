% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Climate Response: Mitigation Priority
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint defines a 'legitimate' climate response as one that
 *   prioritizes emissions reduction through technological innovation and
 *   carbon pricing, aiming to preserve economic growth while decoupling it
 *   from emissions. It is a dominant framing in international climate policy
 *   and national economic strategies. The constraint coordinates global
 *   efforts towards a specific set of solutions but extracts from future
 *   generations if the promised decoupling fails, and from carbon-intensive
 *   industries and low-income consumers in the present.
 *
 * KEY AGENTS:
 *   - current_economic_system: Primary beneficiary (institutional/arbitrage) — maintains growth trajectory.
 *   - technological_innovators: Primary beneficiary (organized/arbitrage) — profits from developing mitigation solutions.
 *   - future_generations: Primary victim (powerless/trapped) — bears the consequences if mitigation fails.
 *   - carbon_intensive_industries: Payer (powerful/constrained) — bears costs of carbon pricing and regulation.
 *   - low_income_consumers: Payer (powerless/constrained) — bears indirect costs of carbon pricing.
 *   - climate_scientists: Observer (analytical/analytical) — assesses feasibility and impacts of mitigation strategies.
 *   - international_policy_makers: Agenda setter (institutional/constrained) — designs and implements global climate agreements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.65).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.4).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Climate Response: Mitigation Priority").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '23105b4c-9a91-4fda-87ec-0cd7983f4727').
narrative_ontology:cs_kernel_codification('23105b4c-9a91-4fda-87ec-0cd7983f4727', formalized).
narrative_ontology:cs_authority_grounding('23105b4c-9a91-4fda-87ec-0cd7983f4727', lineage).
narrative_ontology:cs_interpretation_layer_present('23105b4c-9a91-4fda-87ec-0cd7983f4727').
narrative_ontology:cs_reading_relation('23105b4c-9a91-4fda-87ec-0cd7983f4727', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('23105b4c-9a91-4fda-87ec-0cd7983f4727', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('23105b4c-9a91-4fda-87ec-0cd7983f4727', foundational, economic_growth_is_necessary).
narrative_ontology:cs_axiom_status(economic_growth_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('23105b4c-9a91-4fda-87ec-0cd7983f4727', economic_growth_is_necessary, instrumental).
narrative_ontology:cs_axiom('23105b4c-9a91-4fda-87ec-0cd7983f4727', foundational, technological_innovation_is_primary_solution).
narrative_ontology:cs_axiom_status(technological_innovation_is_primary_solution, holdable).
narrative_ontology:cs_axiom_grounding('23105b4c-9a91-4fda-87ec-0cd7983f4727', technological_innovation_is_primary_solution, empirically_contingent).
narrative_ontology:cs_reference_frame('23105b4c-9a91-4fda-87ec-0cd7983f4727', sustainable_development_paradigm).
narrative_ontology:cs_drift_state('23105b4c-9a91-4fda-87ec-0cd7983f4727', contemporary_climate_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23105b4c-9a91-4fda-87ec-0cd7983f4727', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, current_economic_system).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, technological_innovators).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, low_income_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the framing that allows continued growth, albeit 'decoupled', avoiding radical systemic change. It is the primary recipient of the coordination function's stability.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, current_economic_system, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__mitigation_priority, current_economic_system).

% Profits from the emphasis on technological solutions, receiving funding and market opportunities for developing renewables, carbon capture, and other mitigation technologies.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, technological_innovators, beneficiary,
    organized, biographical, arbitrage, global).

% Bears the ultimate risk and consequences if the promised decoupling and technological solutions fail to deliver sufficient emissions reductions, inheriting a degraded planet.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Faces costs from carbon pricing, regulations, and pressure to transition away from fossil fuels. Their business models are directly challenged by the mitigation priority.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Bears indirect costs of carbon pricing through higher energy and goods prices, often with limited capacity to absorb these costs or switch to more expensive green alternatives.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, low_income_consumers, payer,
    powerless, immediate, constrained, local).

% Provides the scientific basis for understanding climate change and assessing the feasibility and effectiveness of mitigation strategies. Their role is to inform, not to set policy directly.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_scientists, observer,
    analytical, generational, analytical, global).

% Responsible for negotiating and implementing international climate agreements and national policies that embody the mitigation priority. They balance various stakeholder interests and political pressures.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, international_policy_makers, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global and national efforts to reduce greenhouse gas emissions by focusing on technological innovation and market-based mechanisms like carbon pricing, aiming for a unified approach to climate action.
% TRANSFER_FUNCTION: Transfers economic costs and risks from the current economic system (by preserving its growth model) to future generations (by relying on future technological solutions) and to specific industries/consumers (through carbon pricing).
% ABSENT_VOICES: Advocates for 'degrowth_transformation' are largely excluded from mainstream policy discussions, as their proposals fundamentally challenge the economic growth imperative central to this constraint. Indigenous communities and vulnerable populations, often bearing the brunt of climate impacts, have limited voice in setting the technological and economic priorities.
% DISAPPEARANCE_RATIONALE: If this framing of 'legitimate' climate response vanished, global climate policy would fragment, leading to a scramble for alternative approaches. Resource allocation would shift dramatically, potentially towards adaptation or more radical economic restructuring, and the current beneficiaries would lose their privileged position.
% FOUNDING_PROBLEM: The problem of how to address climate change without undermining the global economic system and the promise of continued prosperity, particularly for developing nations.
% FOUNDING_PROBLEM_CORROBORATION: International bodies like the IPCC and many national governments attest that the problem of balancing climate action with economic development is still live. Critics (e.g., degrowth advocates, some climate justice groups) corroborate the existence of the problem but contest the 'mitigation_priority' framing as a legitimate solution, arguing it perpetuates the problem.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a 'tangled_rope' because it genuinely coordinates global action on climate change (benefiting the current economic system and innovators) but also involves significant extraction (from future generations, carbon-intensive industries, and low-income consumers). Extractiveness is high (0.65) due to the intergenerational transfer of risk and the direct costs imposed on certain sectors. Suppression (0.40) is moderate, as alternative approaches (like degrowth) are actively marginalized but not entirely eliminated. Theater ratio (0.20) reflects some performative aspects, where commitments to 'decoupling' may mask insufficient action. The increasing extractiveness and suppression over time reflect the growing urgency of the climate crisis and the hardening of this particular policy pathway.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'current_economic_system' and 'technological_innovators', this is a necessary 'rope' for sustainable development. From 'future_generations' and 'degrowth_advocates', it appears more like a 'snare' that prioritizes short-term economic interests over long-term planetary health. The engine's classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   'Current_economic_system' and 'technological_innovators' are beneficiaries (low d) as the constraint allows them to continue operating and innovating within a familiar paradigm. 'Future_generations' are clear victims (high d) as they bear the ultimate risk of failure. 'Carbon_intensive_industries' and 'low_income_consumers' are payers (high d) due to direct and indirect costs. 'Climate_scientists' are analytical observers (d=0.5). 'International_policy_makers' are agenda setters, balancing coordination and extraction (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to achieve emissions reduction while preserving growth. If decoupling proves empirically impossible, the mandate would be resolved as 'dead', but the constraint might persist due to institutional inertia and the beneficiaries' continued interest in maintaining the growth paradigm. This would shift its classification towards a 'piton' or 'snare', as its coordination function atrophies while extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility,
    'Is sustained economic growth truly decouplable from emissions at the required speed and scale, or is this an empirically contingent claim that might fail?',
    'Empirical observation of global GDP growth vs. emissions trajectory over the next 10-20 years, assessed against IPCC 1.5C pathways.',
    'If decoupling fails, the mitigation_priority reading shifts from a ''tangled_rope'' (coordination with extraction) to a ''snare'' (pure extraction from future generations), as its core promise of preserving growth without catastrophic warming is broken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility, empirical, 'Uncertainty regarding the empirical feasibility of decoupling economic growth from emissions.').

omega_variable(
    technological_dependency_risk,
    'Does the reliance on future technological innovation (e.g., carbon capture, advanced renewables) introduce unacceptable intergenerational risk, effectively externalizing current inaction onto future capabilities?',
    'Assessment of technological readiness levels (TRL) and deployment rates of critical mitigation technologies against required scale-up for 1.5C targets.',
    'If technological dependency is deemed too risky, the ethical justification for the mitigation_priority reading weakens, potentially shifting its classification towards a ''snare'' due to the implicit extraction from future generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_dependency_risk, conceptual, 'Risk of over-reliance on unproven or unscaled future technologies for emissions reduction.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''mitigation_priority'' reading of the ''climate_response_legitimacy'' kernel. What would change if a sibling reading were adopted?',
    'Analysis of policy shifts and resource allocation under alternative framings (e.g., ''adaptation_priority'' would reallocate funds to resilience, ''degrowth_transformation'' would challenge the growth imperative itself).',
    'Adopting ''adaptation_priority'' would shift focus from global emissions to local vulnerability, potentially reducing the ''future_generations'' victim set but increasing costs for ''vulnerable_communities''. Adopting ''degrowth_transformation'' would fundamentally alter the ''current_economic_system'' beneficiary, likely reclassifying the constraint entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Documents this constraint as a specific reading of the climate response kernel and its implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__mitigation_priority, theater_ratio, 5, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__mitigation_priority, theater_ratio, 10, 0.2).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__mitigation_priority, theater_ratio, 15, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__mitigation_priority, theater_ratio, 20, 0.28).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__mitigation_priority, theater_ratio, 25, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__mitigation_priority, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__mitigation_priority, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__mitigation_priority, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__mitigation_priority, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__mitigation_priority, base_extractiveness, 25, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__mitigation_priority, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__mitigation_priority, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__mitigation_priority, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__mitigation_priority, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__mitigation_priority, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
