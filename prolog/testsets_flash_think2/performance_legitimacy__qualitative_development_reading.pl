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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   This constraint describes the national development strategy of a
 *   state-capitalist economy, where legitimacy is increasingly derived from
 *   achieving 'high-quality development' goals (innovation, sustainability,
 *   efficiency) rather than raw GDP growth. This reading of the
 *   'performance_legitimacy' kernel involves active state intervention to
 *   reallocate resources, creating beneficiaries in high-tech sectors and
 *   victims in traditional industries and local governments. The constraint
 *   is claimed as a 'rope' by its proponents (a necessary coordination for
 *   national upgrading) but operates with significant extraction and
 *   suppression, making it a 'tangled_rope' in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.7).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.75).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy: Qualitative Development Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '0ebf1064-3078-4f86-8a32-40b06fb48e94').
narrative_ontology:cs_kernel_codification('0ebf1064-3078-4f86-8a32-40b06fb48e94', formalized).
narrative_ontology:cs_authority_grounding('0ebf1064-3078-4f86-8a32-40b06fb48e94', extraction).
narrative_ontology:cs_interpretation_layer_present('0ebf1064-3078-4f86-8a32-40b06fb48e94').
narrative_ontology:cs_reading_relation('0ebf1064-3078-4f86-8a32-40b06fb48e94', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('0ebf1064-3078-4f86-8a32-40b06fb48e94', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ebf1064-3078-4f86-8a32-40b06fb48e94', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('0ebf1064-3078-4f86-8a32-40b06fb48e94', foundational, innovation_driven_development_is_superior).
narrative_ontology:cs_axiom_status(innovation_driven_development_is_superior, holdable).
narrative_ontology:cs_axiom_grounding('0ebf1064-3078-4f86-8a32-40b06fb48e94', innovation_driven_development_is_superior, instrumental).
narrative_ontology:cs_axiom('0ebf1064-3078-4f86-8a32-40b06fb48e94', foundational, environmental_sustainability_is_non_negotiable).
narrative_ontology:cs_axiom_status(environmental_sustainability_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('0ebf1064-3078-4f86-8a32-40b06fb48e94', environmental_sustainability_is_non_negotiable, conventional).
narrative_ontology:cs_reference_frame('0ebf1064-3078-4f86-8a32-40b06fb48e94', sustainable_innovation_driven_economy).
narrative_ontology:cs_drift_state('0ebf1064-3078-4f86-8a32-40b06fb48e94', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0ebf1064-3078-4f86-8a32-40b06fb48e94', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_development_planners).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, citizens_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, citizens_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the national development strategy, prioritizing innovation, sustainability, and efficiency. Directs state capital, talent, and policy support towards favored sectors. Benefits from the enhanced legitimacy derived from achieving 'high-quality development' goals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_development_planners, agenda_setter,
    institutional, generational, constrained, national).

% Receives significant state investment, subsidies, and policy support. Benefits from a favorable regulatory environment and access to skilled labor and venture capital. Their growth is a key metric for 'qualitative development'.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    organized, biographical, mobile, national).

% Comprises state-owned enterprises, research institutions, and venture funds that directly implement the qualitative development strategy. Benefits from directed funding and policy mandates, acting as a key engine for industrial upgrading.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    institutional, generational, constrained, national).

% Faces increased environmental regulations, reduced state support, and pressure to upgrade or relocate. Bears the costs of economic restructuring, including job losses and reduced profitability, as resources are reallocated away from them.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing, payer,
    moderate, immediate, constrained, regional).

% Historically reliant on land sales and taxes from traditional industries for revenue. Experiences fiscal pressure as traditional sectors decline and new, high-tech industries may generate less immediate, property-related revenue. Must adapt to new development priorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    organized, biographical, constrained, local).

% Experiences short-term disruption from economic restructuring, including job displacement and social safety net strain. Expected to be long-term beneficiaries of a cleaner environment, higher-quality goods, and a more prosperous, innovative economy, but these benefits are diffuse and delayed.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, citizens_at_large, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, citizens_at_large, beneficiary).

% Analyzes the effectiveness and implications of the qualitative development strategy, assessing its impact on global trade, climate goals, and human rights. Provides external commentary and potential pressure for policy adjustments.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, state_development_planners).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national resources, policy, and industrial efforts to shift the economy from raw, extensive growth to intensive, high-value, and sustainable development, addressing environmental limits and innovation gaps.
% TRANSFER_FUNCTION: Transfers capital, talent, and policy support from traditional, often polluting or low-efficiency, industries and regions to high-tech, green, and innovative sectors, while also transferring social costs (e.g., job displacement) to traditional sectors and the general populace.
% ABSENT_VOICES: Small and medium-sized enterprises in traditional sectors, displaced workers, and communities heavily reliant on industries being phased out. They would advocate for more gradual transitions, stronger social safety nets, and continued support for their existing livelihoods.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the state's legitimacy framework would collapse, leading to a rapid re-prioritization of raw GDP growth, a resurgence of traditional industries, and a likely increase in environmental degradation, as the central directive for 'high-quality development' would be absent.
% FOUNDING_PROBLEM: The nation faced unsustainable environmental degradation, diminishing returns from raw growth, and the risk of falling into the 'middle-income trap' due to a lack of innovation and industrial upgrading.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic think tanks, environmental scientists, and international development organizations corroborate the existence and severity of the founding problems, supporting the strategic shift towards qualitative development, even while critiquing its implementation costs.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.70) is high due to the forced reallocation of resources and the imposition of costs on traditional sectors. Suppression (0.75) is substantial, reflecting the state's active role in directing economic transformation and limiting alternatives for non-compliant actors. The theater ratio (0.40) indicates that while genuine efforts are made towards innovation and sustainability, there's also a performative aspect to maintaining the 'high-quality development' narrative, sometimes masking the underlying costs and coercive elements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state planners and high-tech beneficiaries, this constraint is a necessary, forward-looking coordination mechanism for national prosperity. From the perspective of traditional industries and local governments, it is an extractive force that imposes significant costs and limits their autonomy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State development planners, high-tech sectors, and the state-backed innovation ecosystem are clear beneficiaries, receiving directed resources and policy support. Traditional manufacturing, property-dependent local governments, and citizens at large (due to short-term disruption) are the primary payers/victims, bearing the costs of restructuring. The state's institutional power and control over resources enable this asymmetric flow.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    short_term_cost_vs_long_term_benefit,
    'Is the long-term promise of ''high-quality development'' sufficient to justify the short-to-medium-term social and economic costs borne by traditional sectors and the general populace?',
    'Longitudinal studies tracking social welfare, employment, and income inequality alongside innovation and environmental metrics over several decades. Public opinion surveys on perceived quality of life and economic security.',
    'If long-term benefits do not materialize or are unevenly distributed, the constraint''s legitimacy (and thus its effective suppression) would erode, potentially leading to increased resistance and reclassification towards a Snare. If benefits are clear and widespread, it strengthens the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(short_term_cost_vs_long_term_benefit, empirical, 'Trade-off between immediate costs and future benefits of qualitative development.').

omega_variable(
    innovation_as_extraction_cover,
    'To what extent is the ''innovation'' and ''sustainability'' narrative a genuine strategic shift versus a new justification for state-led resource allocation that primarily benefits politically connected sectors?',
    'Audits of state investment funds, analysis of venture capital allocation patterns, and independent assessments of the actual environmental and technological impact of favored projects, compared to their stated goals.',
    'If the narrative primarily serves as cover for rent-seeking by favored entities, the constraint''s true extractiveness is higher, and its coordination function is weaker, pushing it closer to a Snare. If the innovation is genuine, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_as_extraction_cover, empirical, 'Distinguishing genuine innovation from rent-seeking under the guise of qualitative development.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine instantiation of ''qualitative development'' as a distinct legitimacy claim, or is it primarily a re-framing of ''techno-nationalism'' or ''quantitative growth'' with new rhetoric?',
    'Comparative analysis of policy documents, resource allocation, and public discourse across different periods and against the core tenets of sibling readings. Examination of whether policy choices genuinely prioritize sustainability over strategic industry dominance, or efficiency over raw output.',
    'If it''s primarily a re-framing, its distinctness as a reading is weaker, and its classification might be better understood as a variant of a sibling constraint. If distinct, it reinforces the current classification and the integrity of the kernel decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity of this reading''s distinctness from sibling performance legitimacy claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__qualitative_development_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__qualitative_development_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__qualitative_development_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__qualitative_development_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__qualitative_development_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__qualitative_development_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__qualitative_development_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__qualitative_development_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__qualitative_development_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel, focusing on qualitative development. Its distinct ε value and stakeholder structure differentiate it from other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
