% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Software Control Legitimacy: Pragmatic Openness Reading
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic openness' reading of the
 *   'software_control_legitimacy' kernel. It posits that software control is
 *   primarily a development methodology choice, where both open source and
 *   proprietary models are legitimate alternatives. The focus is on producing
 *   better software through peer review, collaboration, and appropriate
 *   investment, rather than on ideological purity. This reading accepts the
 *   coexistence of diverse models and seeks to optimize for quality and
 *   innovation across the ecosystem.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.1).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Software Control Legitimacy: Pragmatic Openness Reading").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, '2973d50e-49ba-49d8-bc32-b0037dbf9ea4').
narrative_ontology:cs_kernel_codification('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', distributed).
narrative_ontology:cs_authority_grounding('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', expertise).
narrative_ontology:cs_interpretation_layer_present('2973d50e-49ba-49d8-bc32-b0037dbf9ea4').
narrative_ontology:cs_reading_relation('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', foundational, software_quality_is_primary_driver).
narrative_ontology:cs_axiom_status(software_quality_is_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', software_quality_is_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', foundational, diverse_development_models_can_coexist).
narrative_ontology:cs_axiom_status(diverse_development_models_can_coexist, holdable).
narrative_ontology:cs_axiom_grounding('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', diverse_development_models_can_coexist, conventional).
narrative_ontology:cs_reference_frame('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', quality_driven_software_engineering).
narrative_ontology:cs_drift_state('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2973d50e-49ba-49d8-bc32-b0037dbf9ea4', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to choose between open source and proprietary development models based on project needs and quality goals. They can leverage peer review and collaboration in open source or proprietary investment in closed source, optimizing for software quality and innovation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, developers, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from a diverse software ecosystem where both open source and proprietary models contribute to a wide range of high-quality, reliable software. Their choices are expanded by the coexistence of different development philosophies.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, users, beneficiary,
    moderate, biographical, mobile, global).

% Advocate for open source as a valid and often superior development methodology, emphasizing collaboration and peer review. They promote the pragmatic benefits of openness without necessarily condemning proprietary alternatives as illegitimate.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_foundations, agenda_setter,
    organized, generational, mobile, global).

% Develop and distribute software under proprietary licenses, emphasizing investment protection and commercial sustainability. They assert the legitimacy of their model as a viable alternative for innovation and quality, coexisting with open source.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_firms, agenda_setter,
    institutional, generational, mobile, global).

% Evaluates and discusses the merits of different software development methodologies, contributing to best practices and empirical understanding of what produces 'better software'. They seek evidence-based conclusions rather than ideological adherence.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_engineering_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To foster an environment where diverse software development methodologies (open source and proprietary) can be evaluated and chosen based on their effectiveness in producing high-quality, reliable software.
% TRANSFER_FUNCTION: Knowledge, best practices, and innovation flow between different development models, contributing to overall software quality and ecosystem health.
% ABSENT_VOICES: Those who hold absolutist positions regarding software freedom (e.g., 'freedom_imperative_reading') or intellectual property rights (e.g., 'property_rights_reading'), as this reading seeks a pragmatic coexistence rather than ideological victory. They would argue for the ethical supremacy of one model over the other.
% DISAPPEARANCE_RATIONALE: Without this pragmatic mediating view, the debate over software control would likely devolve into more polarized and less productive ideological conflicts between proponents of absolute freedom and absolute property rights, hindering collaborative efforts towards quality.
% FOUNDING_PROBLEM: How to reconcile the perceived benefits and drawbacks of open source and proprietary software models to ensure continuous innovation and quality in the software industry.
% FOUNDING_PROBLEM_CORROBORATION: Empirical studies on software quality, developer productivity, and market dynamics; ongoing discussions within the software engineering community and industry bodies; and the continued existence of successful projects under both models.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect this reading's non-coercive, choice-oriented nature. It does not extract from or suppress any particular model, but rather facilitates a pragmatic evaluation. The low theater ratio (0.05) indicates a genuine focus on functional outcomes (quality software). Accessibility collapse is low (0.20) because alternatives are explicitly recognized and available. Resistance is low (0.10) as this reading aims to mediate, not to instigate conflict.
 *
 * PERSPECTIVAL GAP:
 *   This reading stands in contrast to more ideologically driven perspectives, such as the 'freedom_imperative_reading' (which would see proprietary software as inherently extractive) or the 'property_rights_reading' (which might view open source as undermining legitimate investment). This pragmatic view aims to bridge these gaps by focusing on shared goals of quality and innovation.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and users are identified as beneficiaries because they gain from the flexibility and quality outcomes promoted by this pragmatic view. There are no victims, as this reading legitimizes both open source and proprietary approaches, avoiding the extraction inherent in absolutist positions. Agenda-setters (open source foundations, proprietary firms) operate to promote their respective models within this framework of legitimate coexistence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''pragmatic_openness_reading'' of the ''software_control_legitimacy'' kernel?',
    'Comparison with canonical texts and statements from proponents of pragmatic approaches to software development.',
    'If misidentified, the classification of this constraint would be inaccurate, potentially leading to incorrect inferences about its relationship to sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verification of the specific kernel reading being instantiated.').

omega_variable(
    definition_of_better_software,
    'Is ''better software'' defined purely by technical merit (performance, security) or does it include ethical/social dimensions (freedom, accessibility)?',
    'Consensus within the software engineering community or user advocacy groups, or formalization of software quality metrics to include non-technical aspects.',
    'If ethical/social dimensions are prioritized, the ''freedom_imperative_reading'' might gain more legitimacy, potentially increasing perceived extraction from proprietary models within a broader ethical framework. If purely technical, this reading''s neutrality is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_better_software, empirical, 'Ambiguity in the definition of ''better software'' and its implications for methodology choice.').

omega_variable(
    balance_of_collaboration_and_investment,
    'What is the optimal balance between open collaboration (open source) and proprietary investment (proprietary models) for long-term software ecosystem health and innovation?',
    'Longitudinal economic studies, case studies of successful and failing software projects, and empirical research on innovation models.',
    'If open collaboration is shown to be overwhelmingly superior for all aspects, the ''property_rights_reading'' might be seen as more extractive. If proprietary investment is crucial for certain types of innovation, this reading''s legitimacy is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balance_of_collaboration_and_investment, empirical, 'Uncertainty regarding the ideal mix of open and proprietary models for optimal software outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, information_standard).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel, each representing a distinct structural claim about software control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
