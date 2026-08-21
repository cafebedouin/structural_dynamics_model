% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy: Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes a specific reading of 'performance legitimacy'
 *   where the state's authority is primarily grounded in its ability to
 *   deliver tangible improvements in citizens' daily lives, such as
 *   employment, healthcare, education, and elderly care. This reading
 *   prioritizes social welfare and consumption support, often at the expense
 *   of capital-intensive industrial expansion or traditional infrastructure
 *   spending. It functions as a Tangled Rope because it genuinely coordinates
 *   resource allocation for social good (beneficiaries) but also extracts
 *   from other sectors (victims) through active state enforcement and
 *   redirection of investment, requiring continuous political will to
 *   maintain.
 *
 * KEY AGENTS:
 *   - state_apparatus: Agenda setter (institutional/constrained)
 *   - citizens_receiving_services: Primary beneficiary (organized/constrained)
 *   - capital_intensive_industrial_expansion: Primary payer (powerful/constrained)
 *   - private_capital_investors: Payer (powerful/arbitrage)
 *   - technocratic_planners: Excluded (institutional/identity_locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.45).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.6).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy: Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '4401bafb-23a2-4c14-b77a-0b0a84826599').
narrative_ontology:cs_kernel_codification('4401bafb-23a2-4c14-b77a-0b0a84826599', implicit).
narrative_ontology:cs_authority_grounding('4401bafb-23a2-4c14-b77a-0b0a84826599', practice).
narrative_ontology:cs_interpretation_layer_present('4401bafb-23a2-4c14-b77a-0b0a84826599').
narrative_ontology:cs_reading_relation('4401bafb-23a2-4c14-b77a-0b0a84826599', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('4401bafb-23a2-4c14-b77a-0b0a84826599', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('4401bafb-23a2-4c14-b77a-0b0a84826599', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('4401bafb-23a2-4c14-b77a-0b0a84826599', foundational, social_stability_through_welfare).
narrative_ontology:cs_axiom_status(social_stability_through_welfare, holdable).
narrative_ontology:cs_axiom_grounding('4401bafb-23a2-4c14-b77a-0b0a84826599', social_stability_through_welfare, instrumental).
narrative_ontology:cs_axiom('4401bafb-23a2-4c14-b77a-0b0a84826599', foundational, direct_citizen_experience_as_metric).
narrative_ontology:cs_axiom_status(direct_citizen_experience_as_metric, holdable).
narrative_ontology:cs_axiom_grounding('4401bafb-23a2-4c14-b77a-0b0a84826599', direct_citizen_experience_as_metric, empirically_contingent).
narrative_ontology:cs_reference_frame('4401bafb-23a2-4c14-b77a-0b0a84826599', state_as_welfare_provider).
narrative_ontology:cs_drift_state('4401bafb-23a2-4c14-b77a-0b0a84826599', contemporary_global_challenges, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4401bafb-23a2-4c14-b77a-0b0a84826599', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, citizens_receiving_services).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumption).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_expansion).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_spending).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, private_capital_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that prioritizes and allocates resources towards social welfare programs, employment initiatives, healthcare, education, and elderly care. It actively enforces policies that support redistribution and consumption over unchecked industrial growth, seeking to maintain social stability and public support.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from improved access to essential services, social safety nets, and employment opportunities. Their support for the state's legitimacy is contingent on the continued delivery and quality of these tangible improvements in their daily lives.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, citizens_receiving_services, beneficiary,
    organized, biographical, constrained, local).

% Benefit from increased demand and state investment in social services, leading to job creation and economic activity within these sectors. They are aligned with policies that prioritize consumption and social welfare.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sectors, beneficiary,
    moderate, biographical, mobile, regional).

% Represents the aggregate economic activity driven by individual and family spending, which is boosted by policies that enhance disposable income and access to services. While not an 'agent' in the traditional sense, it is a key economic outcome and beneficiary of this reading's priorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, household_consumption, beneficiary,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__livelihood_security_reading, household_consumption).

% Represents the economic activities and sectors focused on large-scale industrial projects and heavy manufacturing. These sectors bear costs through reduced state investment, diverted resources, and potentially higher taxes or regulations designed to fund social programs. Their growth is constrained by the prioritization of livelihood security.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_expansion, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_expansion).

% Represents the allocation of funds by local authorities towards large-scale infrastructure projects (e.g., roads, bridges, industrial parks). Under this reading, these areas receive less funding as resources are redirected to social services and direct citizen benefits, leading to slower development in traditional infrastructure.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_spending, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_spending).

% Face a less favorable investment climate for large-scale, long-term industrial projects due to state prioritization of social spending and consumption. They may seek opportunities in other economies or shift investments towards service sectors, but their traditional avenues for high returns are constrained.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, private_capital_investors, payer,
    powerful, biographical, arbitrage, global).

% Advocate for long-term, efficiency-driven, and often capital-intensive development strategies. Under this reading, their influence is diminished, and their preferred metrics (e.g., industrial output, R&D investment) are secondary to direct livelihood improvements. They are excluded from the primary agenda-setting for resource allocation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, technocratic_planners, excluded,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state resources and policy efforts to directly address citizens' immediate needs for employment, healthcare, education, and elderly care, ensuring a baseline of social welfare and stability.
% TRANSFER_FUNCTION: Transfers public funds, policy attention, and administrative capacity from capital-intensive industrial development and large-scale infrastructure projects to social service provision, consumption support, and direct citizen benefits.
% ABSENT_VOICES: Advocates for 'high-quality development' (sustainability, innovation) and 'quantitative growth' (GDP targets) are marginalized, as their metrics and priorities are secondary to direct livelihood improvements. Technocratic planners and industrial lobbies would argue for different investment priorities.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the state's legitimacy would immediately erode as social services faltered and citizens' daily lives deteriorated. Resource allocation would likely revert to growth-centric or industrial policies, leading to social unrest and a fundamental shift in the state's social contract.
% FOUNDING_PROBLEM: The state faced a challenge of maintaining social stability and public trust amidst economic transitions, requiring a direct demonstration of its capacity to improve citizens' daily lives.
% FOUNDING_PROBLEM_CORROBORATION: Public opinion surveys consistently show high citizen demand for improved social services and livelihood security. International development organizations and independent social scientists corroborate the ongoing importance of these factors for state legitimacy in the region, from outside the state apparatus itself.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).
:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the redirection of resources away from certain sectors, which bear costs. Suppression (0.60) is significant because the state actively enforces this prioritization, limiting alternative development paths and investment choices. The theater ratio (0.20) is low, indicating that the state's efforts are genuinely directed towards service delivery, with less performative overhead. The claimed type is Tangled Rope, as it combines a genuine coordination function (social welfare) with asymmetric extraction (from industrial/infrastructure sectors) enforced by the state.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and citizens receiving services would experience this as a legitimate and beneficial coordination mechanism. In contrast, capital-intensive industries, local governments focused on infrastructure, and private investors would perceive it as an extractive constraint, limiting their growth and investment opportunities. Technocratic planners, whose legitimacy often derives from 'rational' economic growth, would find their influence suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus (agenda_setter) is a beneficiary in terms of legitimacy and social stability, hence a lower directionality. Citizens and service sectors are direct beneficiaries, also with low directionality. Capital-intensive industrial expansion, local government infrastructure spending, and private capital investors are targets, experiencing higher directionality due to resource redirection and constrained investment options. Technocratic planners are excluded, their d value reflecting their marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine social coordination as pure extraction. While there are clear victims, the constraint's primary mandate is to deliver tangible social benefits, which is a live problem. The 'tangled' aspect captures the inherent trade-offs and the active enforcement required to maintain this specific balance of priorities, rather than dismissing it as a simple snare or an unproblematic rope. The 'live' status of the founding problem, combined with the 'world_rearranges' disappearance verdict, indicates that the constraint's mandate is still relevant and its function is critical for the state's stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_diversion_efficiency,
    'Is the redirection of resources from industrial investment to social services efficient and effective in achieving livelihood security, or does it lead to unintended economic stagnation?',
    'Longitudinal economic studies comparing social welfare outcomes with overall economic growth and industrial diversification under this policy regime versus alternative regimes.',
    'If inefficient, the ''extraction'' from industrial sectors might be higher than intended, leading to a re-evaluation of the constraint''s overall benefit-cost ratio and potentially a reclassification towards Snare if the social benefits are not realized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_efficiency, empirical, 'Assesses the actual economic impact and trade-offs of prioritizing livelihood security.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s legitimacy primarily derived from livelihood security, or is it also significantly dependent on other factors like national strength, technological advancement, or political stability?',
    'Comparative political science research and public opinion analysis across different contexts and over time, isolating the relative weight of various legitimacy sources.',
    'If other factors are equally or more dominant, this ''livelihood_security_reading'' might be a partial or secondary constraint, influencing the overall ''performance_legitimacy'' kernel but not fully defining it, potentially shifting its classification within a broader system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Examines the multi-faceted nature of state legitimacy beyond just livelihood security.').

omega_variable(
    kernel_reading_divergence,
    'How do the structural implications of this ''livelihood_security_reading'' diverge from other readings of ''performance_legitimacy'' (e.g., ''quantitative_growth_reading'', ''qualitative_development_reading'', ''techno_nationalist_reading'')?',
    'Comparative analysis of policy documents, resource allocation patterns, and public discourse under different dominant readings of performance legitimacy.',
    'If the divergence is significant, it confirms the need for separate constraint stories for each reading. If minimal, it suggests the readings might converge on a single underlying constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Documents the distinct structural consequences of this specific reading of performance legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__livelihood_security_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__livelihood_security_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__livelihood_security_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__livelihood_security_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__livelihood_security_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__livelihood_security_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel, focusing on livelihood security. Other readings (quantitative_growth_reading, qualitative_development_reading, techno_nationalist_reading) represent distinct constraints with different structural properties and beneficiaries/victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
