% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text (Corporate Moat Reading)
 *   domain: Software Licensing / Intellectual Property / Technology Governance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.65).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.45).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text (Corporate Moat Reading)").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "Software Licensing / Intellectual Property / Technology Governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '76247644-6542-46d5-b744-a716fd64f706').
narrative_ontology:cs_kernel_codification('76247644-6542-46d5-b744-a716fd64f706', fixed_text).
narrative_ontology:cs_authority_grounding('76247644-6542-46d5-b744-a716fd64f706', practice).
narrative_ontology:cs_interpretation_layer_present('76247644-6542-46d5-b744-a716fd64f706').
narrative_ontology:cs_reading_relation('76247644-6542-46d5-b744-a716fd64f706', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('76247644-6542-46d5-b744-a716fd64f706', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('76247644-6542-46d5-b744-a716fd64f706', foundational, unrestricted_commercial_use_is_primary_freedom).
narrative_ontology:cs_axiom_status(unrestricted_commercial_use_is_primary_freedom, holdable).
narrative_ontology:cs_axiom_grounding('76247644-6542-46d5-b744-a716fd64f706', unrestricted_commercial_use_is_primary_freedom, instrumental).
narrative_ontology:cs_axiom('76247644-6542-46d5-b744-a716fd64f706', secondary, reciprocity_is_optional_not_structural).
narrative_ontology:cs_axiom_status(reciprocity_is_optional_not_structural, holdable).
narrative_ontology:cs_axiom_grounding('76247644-6542-46d5-b744-a716fd64f706', reciprocity_is_optional_not_structural, conventional).
narrative_ontology:cs_reference_frame('76247644-6542-46d5-b744-a716fd64f706', unfettered_commercial_integration).
narrative_ontology:cs_drift_state('76247644-6542-46d5-b744-a716fd64f706', contemporary_open_source_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('76247644-6542-46d5-b744-a716fd64f706', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, copyleft_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leverage permissive open-source licenses to integrate code into proprietary products without reciprocal contribution. They actively defend this right through legal teams and lobbying, effectively maintaining a 'moat' around their derivatives. They benefit from reduced R&D costs and accelerated product development.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, agenda_setter,
    institutional, generational, arbitrage, global).

% Contribute code under permissive licenses, often hoping for broad adoption and community growth. They bear the cost of uncompensated value transfer when their work is incorporated into proprietary products without reciprocal contributions, making project sustainability challenging.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    moderate, biographical, constrained, global).

% Actively campaign for licenses that require reciprocity (e.g., GPL). They are structurally excluded from the direct benefits of permissive licensing and bear the cost of its widespread adoption, which they view as enabling exploitation and undermining the commons. Their efforts to promote alternatives are often suppressed by the dominant narrative of 'unfettered freedom'.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, copyleft_advocates, excluded).

% Analyze the legal and economic implications of different licensing models, including the long-term effects of permissive licenses on innovation, competition, and the sustainability of open-source projects. They often provide critical perspectives on the power dynamics at play.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates broad adoption and integration of software components across diverse projects, including proprietary ones, by minimizing legal friction and maximizing compatibility.
% TRANSFER_FUNCTION: Transfers the value of open-source contributions (code, effort, innovation) from individual maintainers and the broader commons to enterprise corporations, who can then build proprietary derivative products without a reciprocal obligation.
% ABSENT_VOICES: Copyleft advocates and those who prioritize reciprocal contributions or the long-term health of the software commons are often marginalized in discussions that frame permissive licensing solely as 'freedom' or 'openness'. Their arguments for structural reciprocity are suppressed by the dominant narrative.
% DISAPPEARANCE_RATIONALE: If permissive licenses vanished overnight, the global software ecosystem would undergo a massive reorganization. Many proprietary products built on permissively licensed open-source components would become legally untenable, forcing a shift towards copyleft compliance, proprietary-from-scratch development, or significant re-licensing efforts. The flow of innovation and value would be fundamentally altered.
% FOUNDING_PROBLEM: To maximize software reuse and interoperability by removing legal barriers to integration into diverse projects, including proprietary ones, thereby accelerating innovation and adoption.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., some large tech companies, certain open-source foundations) attest that permissive licenses continue to solve the problem of maximizing adoption and integration. Critics (e.g., Free Software Foundation, some independent developers, legal scholars focused on reciprocity) argue that while adoption is high, the founding problem has been co-opted, and the arrangement now primarily serves corporate enclosure, not universal freedom. Independent economic analyses often highlight the asymmetric value transfer.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_definition_ambiguity,
    'Is the ''freedom'' promoted by permissive licenses primarily the freedom to integrate code into any project (including proprietary ones), or the freedom to ensure all derivatives remain open and reciprocal?',
    'Conceptual analysis of foundational open-source philosophy texts and legal precedents, alongside empirical studies of developer motivations and community outcomes.',
    'If ''freedom'' is primarily about reciprocity, the constraint''s extractiveness is higher than measured, as it fails to deliver on its core promise. If it''s about unrestricted integration, the measured extractiveness is a necessary cost of that freedom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''freedom'' in open-source licensing.').

omega_variable(
    value_transfer_quantification,
    'What is the quantifiable economic value transferred from individual maintainers to enterprise corporations through permissive licensing without direct compensation or reciprocal contribution?',
    'Comprehensive economic modeling and empirical studies of open-source project dependencies, corporate product portfolios, and developer compensation structures.',
    'A high quantifiable value transfer would strongly corroborate the high extractiveness and Snare classification. A low transfer would challenge the extractiveness metric, potentially shifting the classification towards a Tangled Rope or even Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_transfer_quantification, empirical, 'Quantification of uncompensated value transfer in permissive licensing.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of reciprocal contributions structural (due to ecosystem dominance) or internalized (developers choosing permissive licenses despite concerns)?',
    'Surveys and qualitative studies of developer motivations and perceived constraints, alongside analysis of funding models for copyleft projects. If suppression persists after structural barriers are reduced, it suggests internalization.',
    'If internalized, the effective suppression is higher, as developers self-limit. If purely structural, removing corporate dominance could quickly shift licensing practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of reciprocal licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t2000, permissive_license_text__corporate_moat_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(perm_tr_t2005, permissive_license_text__corporate_moat_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(perm_tr_t2010, permissive_license_text__corporate_moat_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(perm_tr_t2015, permissive_license_text__corporate_moat_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(perm_tr_t2020, permissive_license_text__corporate_moat_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(perm_tr_t2025, permissive_license_text__corporate_moat_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(perm_be_t2000, permissive_license_text__corporate_moat_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(perm_be_t2005, permissive_license_text__corporate_moat_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(perm_be_t2010, permissive_license_text__corporate_moat_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(perm_be_t2015, permissive_license_text__corporate_moat_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(perm_be_t2020, permissive_license_text__corporate_moat_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(perm_be_t2025, permissive_license_text__corporate_moat_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t2000, permissive_license_text__corporate_moat_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(perm_su_t2005, permissive_license_text__corporate_moat_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(perm_su_t2010, permissive_license_text__corporate_moat_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(perm_su_t2015, permissive_license_text__corporate_moat_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(perm_su_t2020, permissive_license_text__corporate_moat_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(perm_su_t2025, permissive_license_text__corporate_moat_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'permissive_license_text' kernel. It focuses on the extractive outcomes for corporations, while 'commons_coordination_reading' emphasizes universal implementation freedom, and 'copyleft_counterfactual_reading' critiques the lack of reciprocity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
