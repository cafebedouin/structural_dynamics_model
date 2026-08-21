% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy: Qualitative Development Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes the 'qualitative development' reading of
 *   performance legitimacy, where the state's right to rule is justified by
 *   its ability to achieve structural transformation towards innovation,
 *   sustainability, and efficiency, rather than raw GDP growth. This involves
 *   prioritizing high-tech sectors and state-backed innovation ecosystems,
 *   while traditional manufacturing and property-dependent local governments
 *   bear the costs of restructuring. The constraint is classified as a
 *   Tangled Rope due to its genuine coordination function (directing national
 *   development) coupled with significant asymmetric extraction from
 *   deprioritized sectors and regions, requiring active enforcement to
 *   maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.65).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.7).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy: Qualitative Development Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '6534462c-0b8f-4aa0-8ed2-7ea919eb59da').
narrative_ontology:cs_kernel_codification('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', formalized).
narrative_ontology:cs_authority_grounding('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', lineage).
narrative_ontology:cs_interpretation_layer_present('6534462c-0b8f-4aa0-8ed2-7ea919eb59da').
narrative_ontology:cs_reading_relation('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', foundational, innovation_driven_development_is_supreme).
narrative_ontology:cs_axiom_status(innovation_driven_development_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', innovation_driven_development_is_supreme, instrumental).
narrative_ontology:cs_axiom('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', foundational, sustainability_and_efficiency_are_non_negotiable).
narrative_ontology:cs_axiom_status(sustainability_and_efficiency_are_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', sustainability_and_efficiency_are_non_negotiable, empirically_contingent).
narrative_ontology:cs_reference_frame('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', post_growth_paradigm_shift).
narrative_ontology:cs_drift_state('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6534462c-0b8f-4aa0-8ed2-7ea919eb59da', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, central_planning_agencies).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, labor_intensive_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies define and enforce the 'high-quality development' agenda, prioritizing innovation, sustainability, and efficiency. They direct state resources, set industrial policy, and evaluate regional performance based on these metrics. Their legitimacy is tied to achieving these structural transformations.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_planning_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive significant state support, subsidies, and preferential policies (e.g., tax breaks, R&D funding, market access) under the 'high-quality development' framework. They benefit from the shift away from raw growth metrics and the focus on innovation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    powerful, biographical, mobile, global).

% Includes state-owned venture capital funds, research institutions, and technology parks that are prioritized for investment and policy support. They are key implementers of the qualitative development agenda and benefit from its resource allocation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    organized, generational, constrained, national).

% Face pressure to upgrade, consolidate, or shut down if they do not meet new efficiency and environmental standards. They bear the costs of industrial restructuring and reduced state support, often leading to job losses and economic disruption.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_sectors, payer,
    moderate, biographical, constrained, regional).

% Historically relied on land sales and real estate development for revenue. The shift away from raw growth and property-led investment under 'high-quality development' severely constrains their fiscal capacity and development models, forcing them to find new revenue streams or face central government penalties.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    organized, immediate, trapped, local).

% Often part of traditional manufacturing, these industries are deprioritized in favor of high-tech sectors. They face declining demand, increased regulatory burdens, and reduced access to capital, leading to closures and unemployment for their workforce.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, labor_intensive_industries, payer,
    powerless, biographical, trapped, local).

% Monitor the policy shift for investment opportunities in favored sectors (e.g., green tech, AI) and risks in deprioritized areas. Their capital flows can either reinforce or challenge the state's development agenda.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, international_investors, observer,
    powerful, immediate, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national economic policy and resource allocation towards a unified vision of advanced, sustainable, and innovation-driven development, moving away from fragmented, growth-at-all-costs approaches.
% TRANSFER_FUNCTION: Transfers state resources, policy support, and legitimacy from traditional, high-growth-at-any-cost sectors and local governments to high-tech, innovation-driven industries and central planning agencies.
% ABSENT_VOICES: Local populations affected by industrial restructuring and job losses in traditional sectors, as well as local entrepreneurs whose businesses are not aligned with the 'high-quality' agenda, are often excluded from the policy-making process. They would advocate for more balanced growth and social safety nets.
% DISAPPEARANCE_RATIONALE: If this qualitative development reading of performance legitimacy vanished, central planning agencies would lose their guiding framework, high-tech sectors would lose preferential support, and traditional industries might see a resurgence of growth-focused policies. The entire national development strategy would need to be re-articulated, leading to significant economic and political reorganization.
% FOUNDING_PROBLEM: The previous model of quantitative, high-speed growth led to severe environmental degradation, resource depletion, overcapacity in traditional industries, and an unsustainable debt burden for local governments, threatening long-term stability and international competitiveness.
% FOUNDING_PROBLEM_CORROBORATION: Central planning agencies and state media consistently highlight the ongoing challenges of environmental pollution, industrial overcapacity, and the need for technological upgrading. Independent environmental NGOs and international economic organizations also corroborate the severity of the problems caused by the previous growth model, supporting the necessity of a qualitative shift.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the shift in development priorities reallocates resources and opportunities, creating clear winners and losers. Suppression (0.70) is substantial as the state actively enforces industrial policies, environmental regulations, and fiscal discipline to compel compliance with the new agenda, often overriding local interests. Theater ratio (0.40) reflects that while genuine efforts are made towards innovation and sustainability, some 'green' or 'high-tech' projects may be performative, serving to signal compliance with the central agenda rather than achieving true transformation. Accessibility collapse (0.60) is moderate, as alternatives for traditional industries are constrained but not entirely eliminated (e.g., they can attempt to upgrade or relocate). Resistance (0.45) is present from affected local governments and industries, but often suppressed or co-opted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of central planning agencies, this is a necessary and beneficial coordination mechanism for national long-term development. From the perspective of traditional industries and local governments, it is an extractive policy that imposes significant costs and limits their autonomy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Central planning agencies and the high-tech/innovation ecosystem are clear beneficiaries, receiving resources and legitimacy from this reading. Traditional manufacturing, labor-intensive industries, and property-dependent local governments are victims, bearing the costs of restructuring and deprioritization. International investors act as observers, arbitraging the policy shifts.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare by acknowledging the genuine coordination problem of transitioning a large economy towards a new development model. However, it also highlights the extractive and suppressive elements, preventing it from being mislabeled as a pure Rope. The 'contested' status of the founding problem indicates that while the initial problems (environmental degradation, overcapacity) are still live, the current solution's fairness and efficacy are debated, suggesting potential for mandatrophy if the costs continue to outweigh perceived benefits for victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_restructuring,
    'What is the full social and economic cost of industrial restructuring and deprioritization for traditional sectors and local populations, and how is it distributed?',
    'Independent, granular economic and social impact assessments, including longitudinal studies of affected regions and displaced workers, not solely relying on official statistics.',
    'If the true costs are significantly higher and more concentrated than acknowledged, the constraint''s effective extractiveness and suppression would be re-evaluated upwards, potentially shifting its classification closer to a Snare for victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_restructuring, empirical, 'Assessing the unacknowledged costs borne by victims of the development shift.').

omega_variable(
    sustainability_vs_innovation_tradeoff,
    'Are the goals of sustainability and innovation genuinely aligned within this development model, or do they present unacknowledged tradeoffs that lead to performative compliance in one area to achieve targets in another?',
    'Detailed audits of ''green'' and ''high-tech'' projects, assessing their actual environmental impact and innovation output versus their reported metrics and resource consumption.',
    'If significant tradeoffs or performative compliance are found, the theater_ratio would increase, and the coordination function''s efficacy would be questioned, potentially weakening the ''rope'' aspect of the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_vs_innovation_tradeoff, empirical, 'Examining the internal consistency and genuine implementation of sustainability and innovation goals.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s legitimacy truly derived from achieving ''high-quality development'', or is this a post-hoc rationalization for maintaining control and directing resources to favored sectors?',
    'Longitudinal analysis of public opinion data, protest movements, and elite discourse, particularly during periods of economic slowdown or policy failure, to gauge the actual public acceptance of the ''qualitative development'' narrative as a source of legitimacy.',
    'If the narrative is found to be primarily a rationalization, the constraint''s extractiveness would be seen as more fundamental, and its coordination function as more of a cover, pushing it closer to a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Distinguishing genuine legitimacy from rationalized control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__qualitative_development_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__qualitative_development_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__qualitative_development_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__qualitative_development_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__qualitative_development_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__qualitative_development_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__qualitative_development_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__qualitative_development_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__qualitative_development_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel, focusing on 'qualitative development'. It influences and coexists with other readings of the same kernel, as the state navigates multiple, sometimes conflicting, legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
