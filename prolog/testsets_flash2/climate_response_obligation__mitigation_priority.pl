% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Climate Response Obligation: Mitigation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'mitigation priority' reading of the
 *   broader climate response obligation. It asserts that intergenerational
 *   justice demands rapid decarbonization to minimize future warming, placing
 *   the primary burden of transition costs on current high-emitting entities
 *   and nations. While framed as a 'tangled_rope' due to its genuine
 *   coordination function (global climate stability) and clear asymmetric
 *   extraction (costs on current emitters, benefits to future generations),
 *   its implementation faces significant resistance and political theater.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.65).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.4).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Climate Response Obligation: Mitigation Priority Reading").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '88336cd5-cd13-4e6a-87a0-7f728c71f4eb').
narrative_ontology:cs_kernel_codification('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', formalized).
narrative_ontology:cs_authority_grounding('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', expertise).
narrative_ontology:cs_interpretation_layer_present('88336cd5-cd13-4e6a-87a0-7f728c71f4eb').
narrative_ontology:cs_reading_relation('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', foundational, intergenerational_equity_demands_prevention).
narrative_ontology:cs_axiom_status(intergenerational_equity_demands_prevention, holdable).
narrative_ontology:cs_axiom_grounding('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', intergenerational_equity_demands_prevention, deontological).
narrative_ontology:cs_axiom('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', foundational, scientific_consensus_on_warming_risk_is_actionable).
narrative_ontology:cs_axiom_status(scientific_consensus_on_warming_risk_is_actionable, holdable).
narrative_ontology:cs_axiom_grounding('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', scientific_consensus_on_warming_risk_is_actionable, empirically_contingent).
narrative_ontology:cs_reference_frame('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', precautionary_principle_framework).
narrative_ontology:cs_drift_state('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', contemporary_political_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('88336cd5-cd13-4e6a-87a0-7f728c71f4eb', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, high_emitting_nations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, carbon_intensive_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary beneficiaries of minimized warming and stable climate systems. They bear no costs in the present but are entirely dependent on current actions for their future well-being. Their 'power' is purely moral and ethical, not material.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% Benefit from reduced climate impacts, as they are disproportionately vulnerable to warming despite historically low emissions. They advocate for rapid decarbonization and climate finance from developed nations. They bear some transition costs but less than the Global North.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_nations, beneficiary,
    organized, generational, constrained, global).

% Bear significant costs through stranded assets, reduced demand, and regulatory burdens from decarbonization policies. They actively resist mitigation efforts and seek to delay the transition, as their business model is directly threatened.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_industries, payer,
    institutional, biographical, constrained, global).

% Primarily developed nations with high historical and per-capita emissions. They are obligated to undertake rapid decarbonization, incurring substantial economic restructuring costs. They face political resistance from carbon-intensive sectors and consumers.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, high_emitting_nations, payer,
    institutional, biographical, constrained, global).

% Individuals and households in developed nations who rely on fossil fuels for transport, heating, and goods. They bear costs through higher energy prices, taxes, and lifestyle changes required for decarbonization. Their resistance is often expressed through political channels.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, carbon_intensive_consumers, payer,
    moderate, immediate, constrained, national).

% Provide the scientific basis for understanding climate change and the urgency of mitigation. They are not directly beneficiaries or victims but their findings underpin the moral and policy arguments for this constraint. Their role is to inform, not to implement.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to reduce greenhouse gas emissions, requiring nations and industries to align their economic activities with a shared goal of limiting global warming to prevent catastrophic future impacts.
% TRANSFER_FUNCTION: Transfers economic costs and technological innovation burdens from future generations and vulnerable nations to current high-emitting nations and industries, in exchange for a more stable climate future.
% ABSENT_VOICES: Future generations, by definition, cannot speak for themselves; their interests are represented by advocates. Non-human species and ecosystems, which bear the brunt of climate change, are also absent from direct negotiation, their interests represented by environmental organizations.
% DISAPPEARANCE_RATIONALE: If the obligation to prioritize mitigation vanished, global emissions would likely accelerate, leading to more severe and rapid climate change. This would fundamentally alter ecosystems, human societies, and economic structures, necessitating massive, reactive adaptation efforts and causing widespread suffering.
% FOUNDING_PROBLEM: The existential threat of anthropogenic climate change, driven by greenhouse gas emissions, which poses severe and irreversible risks to human civilization and natural systems, particularly for future generations.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, national academies of science, and a global consensus among climate scientists corroborate the live status of the founding problem. International agreements like the Paris Agreement also reflect this shared understanding, from outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the substantial economic restructuring and investment required from current generations and carbon-intensive industries. Suppression (0.40) is moderate; while there are international agreements and national policies, enforcement is often weak, and resistance from vested interests is high. Theater ratio (0.20) indicates that some climate action is performative, but there's also genuine effort. Accessibility collapse (0.70) is high because the scientific consensus makes the 'alternative' of inaction increasingly untenable, though political will remains a barrier. Resistance (0.80) is very high due to the direct economic costs and lifestyle changes imposed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of future generations, this is a moral imperative (a Mountain of ethics). From the perspective of fossil fuel industries, it's a Snare designed to destroy their business. The engine's classification as 'tangled_rope' captures the hybrid nature: a genuine coordination problem (climate stability) with clear, asymmetric extraction (transition costs).
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and Global South nations are clear beneficiaries (d near 0.0), as they gain a more stable climate with minimal direct cost. Fossil fuel industries, high-emitting nations, and carbon-intensive consumers are targets (d near 1.0), bearing the costs of decarbonization. The constraint subsidizes the future by extracting from the present.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_discount_rate,
    'What is the appropriate discount rate for future harms and benefits in climate policy, and how does it affect the perceived urgency and cost-benefit of mitigation?',
    'Philosophical and economic consensus on intergenerational ethics, potentially informed by empirical studies of societal time preferences.',
    'A high discount rate would reduce the perceived value of future benefits, making mitigation appear less urgent and more extractive, potentially shifting the classification towards Snare. A low or zero discount rate would amplify the urgency and justify higher present costs, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, conceptual, 'The ethical and economic valuation of future climate impacts.').

omega_variable(
    burden_sharing_equity,
    'What constitutes an equitable distribution of mitigation burdens between historically high-emitting nations and developing nations, given their differing capacities and vulnerabilities?',
    'International negotiations and agreements that establish a globally accepted framework for ''common but differentiated responsibilities and respective capabilities'' (CBDR-RC).',
    'If the burden-sharing mechanism is perceived as inequitable, resistance from developing nations could increase, raising the suppression metric and potentially shifting the constraint towards a Snare for them. If equitable, it reinforces the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_sharing_equity, preference, 'Fairness of mitigation burden distribution.').

omega_variable(
    technological_breakthrough_impact,
    'To what extent will unforeseen technological breakthroughs (e.g., cheap carbon capture, fusion energy) reduce the cost and perceived extractiveness of rapid decarbonization?',
    'Empirical observation of technological development trajectories and their deployment costs over time.',
    'Significant breakthroughs could drastically lower the extractiveness for current generations, potentially shifting the constraint towards a Rope by reducing the ''payer'' burden. Lack of breakthroughs would maintain or increase extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_breakthrough_impact, empirical, 'Impact of future technology on mitigation costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.13).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.16).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.18).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__mitigation_priority, theater_ratio, 40, 0.19).
narrative_ontology:measurement(clim_tr_t50, climate_response_obligation__mitigation_priority, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__mitigation_priority, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(clim_be_t50, climate_response_obligation__mitigation_priority, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__mitigation_priority, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(clim_su_t50, climate_response_obligation__mitigation_priority, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, global_carbon_pricing).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, renewable_energy_subsidies).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_fuel_divestment).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_response_obligation' kernel, focusing on mitigation. It is linked to other readings (adaptation_priority, degrowth_reading) which represent alternative approaches to the same overarching problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
