% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Approach to Software Source Status
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'utilitarian hybrid' reading of software
 *   source status, which posits that software licensing decisions (open vs.
 *   proprietary) should be made to maximize aggregate welfare, acknowledging
 *   that different contexts may warrant different approaches. It rejects
 *   categorical ideological stances in favor of pragmatic optimization. This
 *   reading is one of several competing interpretations of the
 *   'software_source_status' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.3).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.2).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Approach to Software Source Status").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '97099bbe-e6b8-428b-a2d4-ce2403a4085b').
narrative_ontology:cs_kernel_codification('97099bbe-e6b8-428b-a2d4-ce2403a4085b', distributed).
narrative_ontology:cs_authority_grounding('97099bbe-e6b8-428b-a2d4-ce2403a4085b', expertise).
narrative_ontology:cs_interpretation_layer_present('97099bbe-e6b8-428b-a2d4-ce2403a4085b').
narrative_ontology:cs_reading_relation('97099bbe-e6b8-428b-a2d4-ce2403a4085b', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('97099bbe-e6b8-428b-a2d4-ce2403a4085b', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('97099bbe-e6b8-428b-a2d4-ce2403a4085b', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('97099bbe-e6b8-428b-a2d4-ce2403a4085b', foundational, aggregate_welfare_maximization_is_primary).
narrative_ontology:cs_axiom_status(aggregate_welfare_maximization_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('97099bbe-e6b8-428b-a2d4-ce2403a4085b', aggregate_welfare_maximization_is_primary, instrumental).
narrative_ontology:cs_axiom('97099bbe-e6b8-428b-a2d4-ce2403a4085b', foundational, context_determines_optimal_licensing).
narrative_ontology:cs_axiom_status(context_determines_optimal_licensing, holdable).
narrative_ontology:cs_axiom_grounding('97099bbe-e6b8-428b-a2d4-ce2403a4085b', context_determines_optimal_licensing, empirically_contingent).
narrative_ontology:cs_reference_frame('97099bbe-e6b8-428b-a2d4-ce2403a4085b', optimal_hybrid_ecosystem).
narrative_ontology:cs_drift_state('97099bbe-e6b8-428b-a2d4-ce2403a4085b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('97099bbe-e6b8-428b-a2d4-ce2403a4085b', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, innovation_ecosystem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a diverse software ecosystem where the best tool for a given job, whether open or proprietary, is available. Their welfare is maximized by the optimal choice of licensing model for each specific software component.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the flexibility to choose licensing models that best suit their project goals, funding models, and target audience, leading to greater overall productivity and innovation. They are not forced into a single ideological stance.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_developers, beneficiary,
    moderate, biographical, mobile, global).

% Thrives on a pragmatic approach that allows both open-source collaboration for foundational infrastructure and proprietary development for specialized applications, leading to greater overall technological progress and economic value.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, innovation_ecosystem, beneficiary,
    institutional, generational, analytical, global).

% Would argue against any proprietary software, viewing it as inherently extractive or unethical. This reading, however, prioritizes aggregate welfare over a categorical freedom imperative, thus excluding their purist stance from the core decision-making framework.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, ideological_advocates_for_pure_open_source, excluded,
    moderate, generational, identity_locked, global).

% Would argue for absolute property rights over software, potentially hindering collaboration and access. This reading balances property rights against the broader societal benefit, thus excluding their maximalist stance.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, ideological_advocates_for_pure_proprietary_rights, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the selection of software licensing models to optimize for aggregate societal welfare, allowing for a hybrid ecosystem where both open and proprietary models are deployed where they are most effective.
% TRANSFER_FUNCTION: Facilitates the transfer of value (utility, innovation, economic growth) to the broadest possible set of stakeholders by promoting context-appropriate licensing, rather than a categorical transfer to any single group.
% ABSENT_VOICES: Purist advocates for either exclusively open-source or exclusively proprietary models are marginalized, as their categorical positions are deemed suboptimal for aggregate welfare. They would argue for their respective ideological purity over pragmatic optimization.
% DISAPPEARANCE_RATIONALE: If this pragmatic, utilitarian approach vanished, the software ecosystem would likely polarize, leading to less optimal licensing choices, reduced innovation, and decreased aggregate welfare as ideological purity replaced contextual optimization.
% FOUNDING_PROBLEM: The original problem was how to best organize software development and distribution to maximize its benefit to society, given the diverse nature of software projects and their applications.
% FOUNDING_PROBLEM_CORROBORATION: Economists, technology policy analysts, and many pragmatic software industry leaders (outside of purist ideological camps) corroborate that optimizing software licensing for aggregate welfare remains a live and complex problem, requiring continuous re-evaluation rather than dogmatic adherence to a single model.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because this approach aims to minimize rent-seeking by promoting optimal licensing, not by enforcing a single model. Suppression is also low (0.2) as it encourages flexibility rather than coercing adherence to a specific licensing type. Theater ratio is low (0.1) because the stated goal of maximizing welfare is genuinely pursued through flexible policy, not merely performed. Accessibility collapse is moderate (0.4) as it acknowledges that some proprietary solutions might be optimal in certain niches, thus limiting universal access, but overall aims for broad availability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries, this constraint is a flexible framework for optimal resource allocation. From the perspective of the excluded ideological advocates, it might be seen as a compromise that dilutes their core principles, but it does not actively extract from them.
 *
 * DIRECTIONALITY LOGIC:
 *   Software users, developers, and the broader innovation ecosystem are all beneficiaries, as the constraint's goal is to maximize their collective welfare. There are no direct 'victims' in this reading, as it seeks to avoid creating categorical losers. Ideological purists (for either open or proprietary models) are 'excluded' as their rigid stances are not aligned with the utilitarian goal.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively resists mandatrophy by continuously re-evaluating licensing strategies against the live problem of maximizing aggregate welfare. Its flexibility prevents it from becoming an inert or purely performative structure, as it adapts to changing technological and economic contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_welfare_measurement,
    'How is ''aggregate welfare'' precisely defined and measured in the context of software licensing, and is this measurement robust against manipulation or capture by specific interests?',
    'Development of standardized, independently verifiable metrics for societal benefit from software, including economic impact, innovation rates, accessibility, and user freedom, with transparent methodologies.',
    'If aggregate welfare is ill-defined or easily manipulated, this reading could inadvertently become a cover for extractive practices favoring powerful actors; if robustly defined, it strengthens the constraint''s legitimacy as a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_welfare_measurement, empirical, 'Ambiguity in defining and measuring ''aggregate welfare'' in software.').

omega_variable(
    context_dependency_boundaries,
    'What are the clear, objective criteria for determining when an ''open'' model is optimal versus a ''proprietary'' model, and how are these criteria applied consistently across diverse software contexts?',
    'Establishment of a framework of contextual factors (e.g., infrastructure vs. application, security criticality, market maturity, funding model) and their weighting, with case studies and expert consensus.',
    'Without clear criteria, the ''context-dependent optimization'' could devolve into arbitrary decisions or be swayed by lobbying, leading to suboptimal outcomes and increased extractiveness; clear criteria reinforce its ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_dependency_boundaries, conceptual, 'Ambiguity in defining contextual boundaries for optimal licensing models.').

omega_variable(
    ideological_capture_risk,
    'Despite its stated utilitarian goal, is this hybrid reading susceptible to capture by either the ''pure open source'' or ''pure proprietary rights'' ideologies, leading to a de facto bias?',
    'Longitudinal analysis of licensing trends and policy decisions, examining whether outcomes consistently align with aggregate welfare maximization or show persistent skew towards one ideological pole.',
    'If captured, the constraint''s actual operation would drift towards a ''tangled_rope'' or ''snare'' for the disfavored side, despite its stated ''rope'' intent; if resilient, it confirms its coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_capture_risk, empirical, 'Risk of ideological capture despite utilitarian framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1990, software_source_status__utilitarian_hybrid_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1990, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(soft_be_t2000, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(soft_be_t2010, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(soft_be_t2024, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1990, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(soft_su_t2000, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(soft_su_t2010, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(soft_su_t2024, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__utilitarian_hybrid_reading, 0.15).

% DUAL FORMULATION NOTE:
% This constraint is the 'utilitarian_hybrid_reading' of the 'software_source_status' kernel, which also includes 'freedom_imperative_reading', 'pragmatic_development_reading', and 'property_rights_reading'. Each reading represents a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
