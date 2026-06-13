% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Gendered Category Membership (Biological Sex Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint defines gendered category membership (specifically
 *   'woman') based exclusively on immutable biological markers such as
 *   chromosomes and reproductive anatomy at birth. It actively excludes
 *   transgender women from these categories and spaces, and its persistence
 *   relies on strong social and, increasingly, legal enforcement. The
 *   constraint is presented as a natural, immutable truth, but its high
 *   extractiveness and suppression reveal it as a constructed snare that
 *   benefits specific groups while harming others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.85).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.9).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, snare).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Gendered Category Membership (Biological Sex Reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '5aa5b3f4-6385-40b6-9ae5-9e2319b205f3').
narrative_ontology:cs_kernel_codification('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', implicit).
narrative_ontology:cs_authority_grounding('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', lineage).
narrative_ontology:cs_interpretation_layer_present('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3').
narrative_ontology:cs_reading_relation('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', secondary, gender_follows_sex).
narrative_ontology:cs_axiom_status(gender_follows_sex, holdable).
narrative_ontology:cs_axiom_grounding('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', gender_follows_sex, conventional).
narrative_ontology:cs_reference_frame('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', pre_gender_identity_recognition_era).
narrative_ontology:cs_drift_state('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', contemporary_gender_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5aa5b3f4-6385-40b6-9ae5-9e2319b205f3', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cisgender_women_advocates).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, biological_essentialists).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, gender_non_conforming_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively define and defend 'woman' as a category exclusively based on biological sex assigned at birth, advocating for policies that exclude transgender women from sex-segregated spaces and categories. They believe this preserves the integrity and safety of cisgender women's spaces.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cisgender_women_advocates, agenda_setter,
    organized, generational, mobile, global).

% Are denied recognition and access to categories and spaces aligned with their gender identity, based on biological markers. This leads to social exclusion, discrimination, and psychological distress. Their identity is locked, making 'exit' from their gender identity impossible.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% Benefit from the reification of a binary, immutable biological sex as the foundational determinant of social categories. Their worldview is reinforced, and their academic or political positions gain legitimacy within this framework.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, biological_essentialists, beneficiary,
    institutional, civilizational, arbitrage, universal).

% Experience pressure to conform to rigid sex-based categories, even if they do not identify as transgender. Their expressions of gender outside the binary are often policed or invalidated, leading to social friction and limited self-expression.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_non_conforming_individuals, payer,
    moderate, biographical, constrained, local).

% Monitor and advocate against discrimination based on gender identity. They analyze the impact of biological sex-based definitions on marginalized groups and challenge policies that restrict rights or access based on these definitions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, human_rights_organizations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social understanding and legal frameworks around a clear, binary definition of sex for the purpose of maintaining sex-segregated categories and spaces, particularly for cisgender women.
% TRANSFER_FUNCTION: Transfers social recognition, access to specific categories and spaces, and a sense of ontological security from transgender women and gender non-conforming individuals to cisgender women and biological essentialists.
% ABSENT_VOICES: Intersex individuals, whose biological markers do not fit a simple binary, are often overlooked in these discussions, and their experiences challenge the immutability of binary biological sex as a foundational category. They would argue for more nuanced understandings of sex and gender.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, social categories and legal definitions of gender would immediately shift to accommodate gender identity and broader understandings of sex. Sex-segregated spaces would need to redefine their criteria, and the social status of transgender individuals would be significantly altered, leading to a major reorganization of social norms and legal protections.
% FOUNDING_PROBLEM: The perceived need to protect and define the category of 'woman' based on immutable biological characteristics, often framed as a response to perceived threats to cisgender women's rights, safety, and identity.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for this reading attest that the problem is live, citing ongoing debates about women's sports, bathrooms, and legal definitions. Human rights organizations and transgender advocates contest this, arguing the 'problem' is a manufactured moral panic, but acknowledge the political salience of the debate.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because it denies fundamental identity and social recognition to transgender women, imposing significant social and psychological costs. Suppression (0.90) is also very high, as it requires active policing of boundaries, legal challenges to gender recognition, and social pressure to enforce a rigid binary. The theater ratio (0.10) is low, indicating that the enforcement is largely functional in achieving its exclusionary goals, rather than merely performative. Resistance (0.70) is substantial, reflecting the ongoing activism and advocacy by transgender and human rights groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of cisgender women advocates, this constraint is a necessary protection of their rights and spaces, potentially even a 'mountain' of biological reality. From the perspective of transgender women, it is a 'snare' designed to exclude and harm them. The engine's classification will reflect the latter due to the high extractiveness and suppression, despite the claimed naturalness.
 *
 * DIRECTIONALITY LOGIC:
 *   Cisgender women advocates and biological essentialists are beneficiaries (d near 0.0-0.2), gaining social and ontological security, and validation of their worldview. Transgender women and gender non-conforming individuals are clear targets (d near 0.9-1.0), bearing the brunt of exclusion and identity invalidation. Human rights organizations act as observers, analyzing the impact without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_immutability_vs_social_construction,
    'Is the grounding of gendered categories in immutable biological markers a natural law or a social construct actively maintained for specific beneficiaries?',
    'Cross-cultural anthropological studies of gender systems, historical analysis of sex definitions, and scientific consensus on the interplay of biology and environment in sex development. If the ''immutability'' is found to be culturally and historically contingent, it points to social construction.',
    'If a social construct, the constraint''s ''mountain'' claim is false, and its high extractiveness is revealed as a feature of a ''snare'' or ''tangled_rope''. If genuinely immutable, the extractiveness might be re-evaluated as an unavoidable consequence of a natural boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_immutability_vs_social_construction, conceptual, 'Ambiguity between natural law and social construction for biological sex definitions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, policy exclusion) or internalized (psychological impact, self-censorship) for transgender individuals?',
    'Post-policy-change studies: if suppression persists after legal and policy barriers are removed, it indicates a significant internalized component. If it rapidly diminishes, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after formal barriers are removed. This would deepen the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gendered category exclusion.').

omega_variable(
    category_dilution_empirical_basis,
    'Is there empirical evidence that the inclusion of transgender women in ''woman'' categories actually dilutes or harms the rights and safety of cisgender women?',
    'Longitudinal studies of sex-segregated spaces (e.g., sports, shelters) in jurisdictions with inclusive gender recognition policies, comparing outcomes to jurisdictions with exclusionary policies. Absence of harm would undermine a key justification for the constraint.',
    'If no empirical harm is found, the primary justification for the constraint''s exclusionary function collapses, revealing the ''coordination'' aspect as a cover for pure extraction and identity suppression. This would strengthen the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_dilution_empirical_basis, empirical, 'Empirical basis for claims of category dilution or harm to cisgender women.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1990, gendered_category_membership__biological_sex_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(gend_tr_t2000, gendered_category_membership__biological_sex_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__biological_sex_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(gend_tr_t2020, gendered_category_membership__biological_sex_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(gend_tr_t2024, gendered_category_membership__biological_sex_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gend_be_t1990, gendered_category_membership__biological_sex_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(gend_be_t2000, gendered_category_membership__biological_sex_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__biological_sex_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(gend_be_t2020, gendered_category_membership__biological_sex_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(gend_be_t2024, gendered_category_membership__biological_sex_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1990, gendered_category_membership__biological_sex_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(gend_su_t2000, gendered_category_membership__biological_sex_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__biological_sex_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(gend_su_t2020, gendered_category_membership__biological_sex_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(gend_su_t2024, gendered_category_membership__biological_sex_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
