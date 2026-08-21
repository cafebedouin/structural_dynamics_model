% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response: Adaptation Priority Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'adaptation priority' reading of
 *   legitimate climate response, which accepts a certain warming trajectory
 *   and focuses on protecting vulnerable populations through resilience and
 *   adaptive capacity. It is one reading of the broader
 *   'climate_response_legitimacy' kernel. The framework is presented as a
 *   pragmatic and ethical response to unavoidable climate impacts, but it
 *   structurally benefits wealthy nations by allowing them to preserve their
 *   development models while deferring the more disruptive costs of
 *   aggressive mitigation. Vulnerable populations bear immediate costs, and
 *   future generations face compounded impacts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.72).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response: Adaptation Priority Framework").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, 'af7d721d-9045-49a3-8eac-6dff37af21d3').
narrative_ontology:cs_kernel_codification('af7d721d-9045-49a3-8eac-6dff37af21d3', formalized).
narrative_ontology:cs_authority_grounding('af7d721d-9045-49a3-8eac-6dff37af21d3', extraction).
narrative_ontology:cs_interpretation_layer_present('af7d721d-9045-49a3-8eac-6dff37af21d3').
narrative_ontology:cs_reading_relation('af7d721d-9045-49a3-8eac-6dff37af21d3', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('af7d721d-9045-49a3-8eac-6dff37af21d3', climate_response_legitimacy__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('af7d721d-9045-49a3-8eac-6dff37af21d3', foundational, unavoidable_warming_acceptance).
narrative_ontology:cs_axiom_status(unavoidable_warming_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('af7d721d-9045-49a3-8eac-6dff37af21d3', unavoidable_warming_acceptance, empirically_contingent).
narrative_ontology:cs_axiom('af7d721d-9045-49a3-8eac-6dff37af21d3', foundational, vulnerable_first_principle).
narrative_ontology:cs_axiom_status(vulnerable_first_principle, holdable).
narrative_ontology:cs_axiom_grounding('af7d721d-9045-49a3-8eac-6dff37af21d3', vulnerable_first_principle, deontological).
narrative_ontology:cs_reference_frame('af7d721d-9045-49a3-8eac-6dff37af21d3', pragmatic_impact_management).
narrative_ontology:cs_drift_state('af7d721d-9045-49a3-8eac-6dff37af21d3', contemporary_climate_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af7d721d-9045-49a3-8eac-6dff37af21d3', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_industry_consultants).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, vulnerable_nations_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors set the global climate policy agenda, advocating for adaptation as the primary response. They benefit by preserving their existing economic development models and deferring the more disruptive costs of aggressive mitigation, while gaining influence through adaptation funding mechanisms.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations_industries, agenda_setter,
    institutional, generational, arbitrage, global).

% These populations bear the immediate and escalating impacts of climate change, facing an 'adaptation deficit' where funding and capacity fall short of needs. They are nominally prioritized for protection but often receive insufficient resources, forcing them to pay with lives, livelihoods, and lost development.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, vulnerable_nations_populations, payer,
    organized, immediate, constrained, global).

% This sector profits from the increased demand for resilience infrastructure, climate risk assessments, and adaptive capacity building. They receive significant funding flows for projects and expertise, aligning their interests with the adaptation-first approach.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_industry_consultants, beneficiary,
    moderate, biographical, mobile, global).

% These are the ultimate bearers of compounded climate impacts due to deferred aggressive mitigation. While current policies aim to protect them through adaptation, the acceptance of a higher warming trajectory means they inherit a more unstable and resource-constrained world.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, identity_locked, universal).

% These groups argue for a primary focus on rapid, deep emissions reductions. Within the adaptation-priority framework, their calls for more aggressive mitigation are often sidelined or framed as economically unfeasible, effectively excluding their preferred solutions from the core policy response.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, mitigation_advocates, excluded,
    organized, generational, constrained, global).

% These groups advocate for systemic economic transformation to reduce resource consumption and emissions. Their proposals directly challenge the 'preserve development model' aspect of the adaptation-priority framework, leading to their exclusion from mainstream policy discussions.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, degrowth_advocates, excluded,
    organized, generational, constrained, global).

% These experts provide the foundational data on warming trajectories and impacts. While their science informs the need for adaptation, their warnings about the long-term consequences of insufficient mitigation are often selectively interpreted or downplayed within this framework.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_scientists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global efforts to build resilience and adaptive capacity in vulnerable regions, managing the unavoidable impacts of climate change and protecting populations already at risk.
% TRANSFER_FUNCTION: Transfers financial resources, technological expertise, and policy focus from wealthier nations to vulnerable ones for adaptation, while implicitly transferring the burden of unmitigated warming impacts to vulnerable populations and future generations.
% ABSENT_VOICES: Advocates for aggressive mitigation and degrowth are largely absent from the core decision-making, as their proposals challenge the underlying premise of accepting the warming trajectory and preserving existing economic models. They would argue for a more fundamental shift in global priorities.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, there would be a significant vacuum in global climate policy. Without a coordinated adaptation strategy, vulnerable populations would face immediate and catastrophic consequences, leading to widespread humanitarian crises and geopolitical instability. The global response would either collapse into uncoordinated, localized efforts or be forced to rapidly pivot to a more aggressive mitigation-focused approach.
% FOUNDING_PROBLEM: The recognition that some level of warming is unavoidable due to past emissions, and that vulnerable populations are already experiencing severe impacts, requiring immediate protective action and resource allocation.
% FOUNDING_PROBLEM_CORROBORATION: IPCC reports, humanitarian organizations, and scientific consensus corroborate the reality of unavoidable warming and the disproportionate impact on vulnerable regions, validating the initial problem statement. However, the adequacy of the response is contested.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.72, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.72) reflects the significant 'adaptation deficit' borne by vulnerable nations and the deferred costs to future generations, while wealthy nations avoid immediate economic disruption. Suppression (0.68) is high due to the active marginalization of alternative climate strategies (e.g., aggressive mitigation, degrowth) that would challenge the status quo. The theater ratio (0.45) indicates a substantial performative aspect, where adaptation efforts are highlighted to justify continued high-carbon development, even as the actual funding often falls short of needs. The metrics show a trend of increasing extractiveness and suppression over time, suggesting a hardening of this policy stance as climate impacts worsen.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of wealthy nations and the adaptation industry, this framework is a necessary and ethical coordination mechanism. From the perspective of vulnerable nations and future generations, it functions as an extractive mechanism that shifts burdens and perpetuates existing inequalities. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and industries are clear beneficiaries (low d) as they maintain their economic models and gain influence. The adaptation industry also benefits directly from funding flows. Vulnerable nations and future generations are the primary targets (high d), bearing the direct and deferred costs of climate impacts and insufficient adaptation. Mitigation and degrowth advocates are structurally excluded, their perspectives suppressed by the dominant narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_funding_adequacy,
    'Is the promised adaptation funding actually sufficient to meet the needs of vulnerable populations, or is it a fraction of the true cost, perpetuating the ''adaptation deficit''?',
    'Independent audits of adaptation finance flows versus assessed needs, and empirical studies on the effectiveness and reach of adaptation projects in vulnerable regions.',
    'If funding is found to be grossly inadequate, it would strengthen the classification towards Snare, highlighting the performative nature of the ''protection'' claim. If funding is largely adequate and effective, it would support the coordination function, moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_funding_adequacy, empirical, 'Whether adaptation funding genuinely meets the needs of vulnerable populations.').

omega_variable(
    intergenerational_equity_tradeoff,
    'Is accepting the warming trajectory a legitimate intergenerational tradeoff, or an unjust deferral of costs and risks to future generations?',
    'Ethical and philosophical analysis of intergenerational justice, potentially informed by economic modeling of long-term climate damages and adaptation costs.',
    'If deemed an unjust deferral, it would amplify the extractiveness metric, particularly for the ''future_generations'' seat. If framed as a legitimate, unavoidable tradeoff, it would temper the perceived extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_equity_tradeoff, conceptual, 'Ethical legitimacy of deferring mitigation costs to future generations.').

omega_variable(
    development_model_legitimacy,
    'Is preserving the wealthy nations'' development model a necessary condition for adaptation funding and global stability, or an extractive component of the climate response that perpetuates inequality?',
    'Comparative analysis of economic models that decouple development from high emissions, and political economy studies on the power dynamics influencing climate policy choices.',
    'If preserving the development model is found to be an extractive choice, it would further solidify the Tangled Rope classification and increase the effective extraction for wealthy nations. If it''s deemed a necessary pragmatic compromise, it would slightly reduce the perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_model_legitimacy, conceptual, 'Whether preserving wealthy nations'' development model is a necessary or extractive component.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_response_legitimacy__adaptation_priority, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(clim_tr_t2010, climate_response_legitimacy__adaptation_priority, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(clim_tr_t2020, climate_response_legitimacy__adaptation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2030, climate_response_legitimacy__adaptation_priority, theater_ratio, 2030, 0.45).
narrative_ontology:measurement(clim_tr_t2040, climate_response_legitimacy__adaptation_priority, theater_ratio, 2040, 0.48).
narrative_ontology:measurement(clim_tr_t2050, climate_response_legitimacy__adaptation_priority, theater_ratio, 2050, 0.5).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(clim_be_t2010, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(clim_be_t2020, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(clim_be_t2030, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2030, 0.72).
narrative_ontology:measurement(clim_be_t2040, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2040, 0.75).
narrative_ontology:measurement(clim_be_t2050, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2050, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(clim_su_t2010, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(clim_su_t2020, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2030, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2030, 0.68).
narrative_ontology:measurement(clim_su_t2040, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2040, 0.7).
narrative_ontology:measurement(clim_su_t2050, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2050, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'climate_response_legitimacy' kernel. Each reading represents a different structural approach to climate policy, with different beneficiaries, victims, and underlying assumptions about economic and ethical priorities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
