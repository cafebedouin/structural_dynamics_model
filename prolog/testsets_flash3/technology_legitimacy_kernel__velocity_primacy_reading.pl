% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Climate Technology Legitimacy: Velocity Primacy Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'velocity primacy' reading of climate
 *   technology legitimacy, asserting that only technologies deployable at
 *   scale within the remaining carbon budget timeline (e.g., 2030/2050
 *   targets) are legitimate for mitigation. This reading prioritizes speed
 *   and immediate impact, often favoring rapidly deployable renewables while
 *   marginalizing technologies with longer lead times like nuclear. The
 *   constraint is claimed as a Tangled Rope because it genuinely coordinates
 *   urgent climate action but also extracts from and suppresses alternatives
 *   that do not meet its strict temporal criteria.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.65).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.7).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Climate Technology Legitimacy: Velocity Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'b3be31a9-fb2d-4b32-b6fd-95881f6fb979').
narrative_ontology:cs_kernel_codification('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', distributed).
narrative_ontology:cs_authority_grounding('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', practice).
narrative_ontology:cs_interpretation_layer_present('b3be31a9-fb2d-4b32-b6fd-95881f6fb979').
narrative_ontology:cs_reading_relation('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', technology_legitimacy_kernel__reliability_primacy_reading, influences).
narrative_ontology:cs_reading_relation('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', foundational, speed_is_paramount_for_carbon_budget).
narrative_ontology:cs_axiom_status(speed_is_paramount_for_carbon_budget, holdable).
narrative_ontology:cs_axiom_grounding('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', speed_is_paramount_for_carbon_budget, empirically_contingent).
narrative_ontology:cs_axiom('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', secondary, existing_technologies_sufficient_for_speed).
narrative_ontology:cs_axiom_status(existing_technologies_sufficient_for_speed, holdable).
narrative_ontology:cs_axiom_grounding('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', existing_technologies_sufficient_for_speed, empirically_contingent).
narrative_ontology:cs_reference_frame('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', urgent_carbon_budget_response).
narrative_ontology:cs_drift_state('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b3be31a9-fb2d-4b32-b6fd-95881f6fb979', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_activists_velocity_aligned).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, policy_makers_short_term_targets).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_energy_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_intermittency_challenge).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, long_term_infrastructure_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from policy and investment prioritization due to their rapid deployment potential. Their technologies (solar, wind) are favored by this reading's emphasis on speed, leading to easier market access and subsidies.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers, beneficiary,
    organized, immediate, mobile, global).

% Advocate for immediate, large-scale deployment of existing, fast-to-build technologies to meet urgent carbon budget deadlines. This reading aligns with their core demands, amplifying their influence in policy debates.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_activists_velocity_aligned, beneficiary,
    moderate, biographical, constrained, global).

% Are incentivized to adopt this reading to demonstrate progress towards politically salient 2030/2050 climate targets. They shape regulations and funding to favor technologies that promise quick wins, often at the expense of longer-term, more complex solutions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, policy_makers_short_term_targets, agenda_setter,
    institutional, immediate, constrained, national).

% Face significant hurdles and marginalization under this reading due to the long lead times for nuclear plant construction. They argue for the baseload, carbon-free benefits of nuclear, but are often excluded from 'fast deployment' policy discussions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_energy_advocates, payer,
    organized, generational, constrained, national).

% Bear the increasing costs and technical challenges of integrating a high proportion of intermittent renewable energy sources. This reading prioritizes rapid deployment without fully accounting for grid stability requirements, forcing operators to adapt or face reliability issues.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_intermittency_challenge, payer,
    institutional, immediate, trapped, regional).

% Are tasked with designing resilient energy systems for decades to come. This reading's short-term focus can lead to underinvestment in foundational, long-lifecycle infrastructure (like advanced nuclear or grid modernization) that doesn't meet immediate deployment speed criteria.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, long_term_infrastructure_planners, payer,
    moderate, civilizational, constrained, national).

% Would argue that rapid deployment without sufficient consideration of worst-case failure modes (e.g., untested geoengineering, unproven carbon capture at scale) is irresponsible. Their concerns about long-term risks are often sidelined by the urgency of the velocity argument.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_advocates, excluded,
    moderate, generational, identity_locked, global).

% Would emphasize the need for dispatchable, baseload power to ensure grid stability and energy security. Their arguments for technologies like nuclear or advanced fossil fuels with CCS are often dismissed by the velocity reading as too slow to deploy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, reliability_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates policy, investment, and public discourse around technologies that can deliver rapid, large-scale carbon reductions within tight deadlines, creating a unified focus for climate action.
% TRANSFER_FUNCTION: Transfers political capital, R&D funding, and public legitimacy towards rapidly deployable renewable technologies (solar, wind) and away from slower-to-build, capital-intensive options (nuclear, large-scale CCS).
% ABSENT_VOICES: Advocates for technologies with long lead times (e.g., advanced nuclear, fusion) or those with significant unknown risks (e.g., some geoengineering proposals) are marginalized. Their concerns about long-term reliability or worst-case scenarios are often not part of the 'velocity-first' conversation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the criteria for climate technology legitimacy would immediately broaden. Technologies currently sidelined (e.g., nuclear) would regain policy traction, investment flows would diversify, and the urgency of 'speed at all costs' would be balanced by other considerations like reliability and long-term risk, fundamentally altering climate policy and energy markets.
% FOUNDING_PROBLEM: The existential threat of climate change, driven by accumulating greenhouse gas emissions, necessitates urgent and effective mitigation strategies to stay within a rapidly diminishing carbon budget.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on climate change and the IPCC reports consistently corroborate the urgency of the carbon budget problem. International climate agreements and national emissions targets further attest to the live status of this foundational challenge, from sources outside the direct beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the opportunity costs imposed on technologies and approaches that are effective but slower to deploy, diverting resources and political will. Suppression (0.70) is high because this reading actively marginalizes or excludes alternatives that don't fit the 'velocity' criterion, requiring active enforcement in policy and funding decisions. The theater ratio (0.20) is moderate; while there's genuine urgency, some 'fast deployment' claims may overstate actual impact or ignore downstream challenges (like grid integration).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable developers, this is a Rope, coordinating necessary action. From nuclear advocates or grid operators, it's a Snare, extracting from their interests and suppressing viable alternatives. The engine's per-seat classification will reflect these divergences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy developers and aligned climate activists are clear beneficiaries, as their interests align with rapid deployment. Policy makers focused on short-term targets also benefit from adopting this reading. Conversely, nuclear energy advocates, grid operators facing intermittency challenges, and long-term infrastructure planners are victims, bearing the costs of this narrow focus. Precautionary and reliability advocates are excluded, their concerns not fully integrated into this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_budget_realism,
    'Is the remaining carbon budget timeline (e.g., 2030/2050 targets) a fixed, unchangeable constraint, or is it subject to political negotiation and scientific re-evaluation?',
    'Analysis of IPCC reports on carbon budget uncertainty, political science research on climate target setting, and historical precedent for target revisions.',
    'If the budget is more flexible, the ''velocity primacy'' argument weakens, potentially allowing for a broader range of legitimate technologies. If it''s truly fixed, the constraint''s urgency is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_budget_realism, empirical, 'Uncertainty regarding the rigidity of the carbon budget timeline.').

omega_variable(
    grid_integration_cost_underestimation,
    'Does the ''velocity primacy'' reading systematically underestimate the long-term costs and technical challenges of integrating rapidly deployed intermittent renewables into existing grid infrastructure?',
    'Comprehensive, independent economic and engineering studies comparing projected grid integration costs with actual costs in high-renewable grids, accounting for storage and transmission upgrades.',
    'If costs are significantly underestimated, the net benefit of ''velocity primacy'' is reduced, potentially shifting legitimacy towards technologies that offer more stable power, even if slower to deploy. This would increase the effective extraction from grid operators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_integration_cost_underestimation, empirical, 'Potential underestimation of grid integration costs for fast-deployed renewables.').

omega_variable(
    legitimacy_framing_contest,
    'Is the ''velocity primacy'' framing a genuine, structurally necessary criterion for climate technology legitimacy, or is it a strategic framing deployed by specific interest groups to advance their preferred technologies?',
    'Discourse analysis of policy debates, funding allocations, and lobbying efforts by various energy sectors, alongside a structural analysis of the actual physical constraints of climate mitigation.',
    'If it''s primarily a strategic framing, the constraint''s ''coordination'' function is weaker, and its ''extraction'' from sidelined technologies is more pronounced, pushing it closer to a Snare. If it''s a genuine structural necessity, its coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_framing_contest, conceptual, 'Ambiguity between genuine necessity and strategic framing in the ''velocity primacy'' argument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'technology_legitimacy_kernel'. It focuses on deployment velocity within carbon budget timelines, influencing policy and investment decisions in climate mitigation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
