% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gendered Category Membership (Gender Identity Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint describes the social and legal framework where membership
 *   in gendered categories (e.g., 'woman', 'man') is primarily determined by
 *   an individual's subjective gender identity and self-declaration. It is a
 *   reading of the broader 'gendered_category_membership' kernel, which is
 *   contested by alternative readings based on biological sex or social role.
 *   This reading prioritizes the inclusion and recognition of transgender
 *   individuals, often leading to the redefinition of sex-segregated spaces
 *   as gender-segregated. The structural delta is that trans women are
 *   included in the 'woman' category via self-ID, and cis women who resist
 *   this inclusion are often positioned as perpetrators of exclusion.
 *
 * KEY AGENTS:
 *   - transgender_individuals: Primary beneficiary (moderate/identity_locked) — gain recognition and inclusion.
 *   - gender_identity_advocates: Agenda setter (organized/mobile) — promote and enforce this reading.
 *   - gender_critical_feminists: Primary payer (organized/constrained) — bear costs of redefined categories, face ostracization.
 *   - cisgender_women_in_sex_segregated_spaces: Payer (powerless/constrained) — experience direct impact of redefinition.
 *   - legal_systems: Agenda setter (institutional/constrained) — codify and enforce this reading.
 *   - analytical_observers: Observer (analytical/analytical) — analyze structural implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.45).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.6).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gendered Category Membership (Gender Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, 'a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e').
narrative_ontology:cs_kernel_codification('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', formalized).
narrative_ontology:cs_authority_grounding('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', lineage).
narrative_ontology:cs_interpretation_layer_present('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e').
narrative_ontology:cs_reading_relation('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', foundational, gender_identity_is_self_declared).
narrative_ontology:cs_axiom_status(gender_identity_is_self_declared, holdable).
narrative_ontology:cs_axiom_grounding('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', gender_identity_is_self_declared, deontological).
narrative_ontology:cs_axiom('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', foundational, inclusion_of_transgender_individuals_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(inclusion_of_transgender_individuals_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', inclusion_of_transgender_individuals_is_a_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', gender_identity_as_primary_determinant).
narrative_ontology:cs_drift_state('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a89b2f9b-e4c5-41d5-8b1f-fd7e6e5a3b9e', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, gender_critical_feminists).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cisgender_women_in_sex_segregated_spaces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition and inclusion in gendered categories aligned with their self-declared identity, which is crucial for their well-being and social integration. Exit from this framework would mean denying their identity.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce the gender identity reading through legal, social, and institutional channels. They benefit from the expansion of this framework and the validation of their ideological stance.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).

% Bear the cost of losing sex-based categories and protections, particularly in areas like women's sports, changing rooms, and shelters. They face social and professional ostracization for resisting the gender identity reading.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_critical_feminists, payer,
    organized, generational, constrained, global).

% Experience the direct impact of sex-segregated spaces becoming gender-segregated, leading to perceived loss of privacy, safety, and specific protections. Their concerns are often dismissed or reframed as transphobic.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cisgender_women_in_sex_segregated_spaces, payer,
    powerless, immediate, constrained, local).

% Are increasingly tasked with codifying and enforcing gender identity as the primary basis for gendered categories, often through anti-discrimination laws. They navigate conflicting claims and societal pressures.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% Attempt to understand the structural implications, ethical dilemmas, and social consequences of grounding category membership in subjective identity, without direct participation or benefit.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social recognition and inclusion for individuals whose gender identity differs from their sex assigned at birth, ensuring their access to spaces and categories aligned with their identity.
% TRANSFER_FUNCTION: Transfers social recognition, access to gendered spaces, and definitional authority over 'woman' from biological sex as a primary criterion to subjective gender identity, from gender-critical perspectives to gender identity advocates.
% ABSENT_VOICES: Children and adolescents, particularly girls, whose developing sense of sex-based boundaries and safety in single-sex spaces is impacted, often lack a voice in policy debates. Their concerns are mediated through adult advocates, often with conflicting interests.
% DISAPPEARANCE_RATIONALE: If the gender identity reading vanished overnight, the social and legal landscape around gender would undergo significant rearrangement. Transgender individuals would lose legal and social recognition of their gender identity, leading to widespread distress and discrimination. Sex-based categories would reassert primacy, altering access to spaces and services. The current political and social conflicts would shift dramatically.
% FOUNDING_PROBLEM: The historical and ongoing marginalization, discrimination, and lack of recognition for transgender individuals, leading to significant psychological distress and social exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Transgender individuals and their advocates attest to the live problem of discrimination and lack of recognition. Medical and psychological associations, as well as human rights organizations, corroborate the need for gender identity recognition to address mental health disparities and ensure human rights, from outside the immediate beneficiary group.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).
:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs borne by those who lose sex-based categories and the resources expended in enforcing this framework. Suppression (0.6) is significant, as resistance to this reading often results in social and professional penalties, limiting open debate and alternative viewpoints. Theater ratio (0.2) is low, as the enforcement and advocacy are genuinely aimed at achieving the stated goals of recognition and inclusion, though some performative aspects exist in public discourse. Accessibility collapse (0.4) is moderate, as alternatives (e.g., maintaining sex-based categories) are still conceptually available but increasingly difficult to enact without significant social cost. Resistance (0.7) is high, indicating ongoing and vocal opposition from various groups, particularly gender-critical feminists.
 *
 * PERSPECTIVAL GAP:
 *   Transgender individuals and gender identity advocates experience this as a necessary and beneficial coordination mechanism for social inclusion and human rights. Conversely, gender-critical feminists and many cisgender women experience it as an extractive and suppressive force that erodes sex-based rights and protections. The legal systems navigate these conflicting perspectives, often attempting to balance competing rights, but ultimately enforcing the dominant legal interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals are clear beneficiaries (d=0.0-0.1) as the constraint directly affirms their identity and grants access. Gender identity advocates also benefit (d=0.1-0.2) from the validation and expansion of their framework. Gender-critical feminists and cisgender women in sex-segregated spaces are targets (d=0.8-1.0) as they bear the costs of category redefinition and face suppression for their resistance. Legal systems act as agenda-setters (d=0.3-0.4), enforcing the framework while also being constrained by existing legal precedents and public pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (to ensure recognition and inclusion for transgender individuals) is actively pursued and contested. The classification as a Tangled Rope reflects the genuine coordination function (inclusion) intertwined with asymmetric extraction (costs to those who lose sex-based categories) and active enforcement against resistance. It prevents mislabeling as a Snare by acknowledging the real coordination problem it addresses, while also preventing mislabeling as a Rope by highlighting the significant extraction and suppression involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_gender_identity,
    'Is gender identity an inherent, immutable aspect of self, or a socially constructed and fluid phenomenon?',
    'Further research in neuroscience, psychology, and sociology on the origins and stability of gender identity, alongside philosophical analysis of selfhood.',
    'If inherent and immutable, the constraint''s ''naturalness'' increases, potentially shifting its classification towards a Mountain for transgender individuals. If primarily constructed, its status as a human-made construct becomes clearer, reinforcing its Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_gender_identity, empirical, 'Ambiguity regarding the ontological status of gender identity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, institutional policies) or internalized (social pressure, fear of ostracization)?',
    'Post-exit suppression trajectory: if resistance to the gender identity reading persists after formal legal/institutional barriers are removed, reclassify as partially internalized. Longitudinal studies on the psychological impact of social pressure.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would amplify the extractive nature for those who dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting views.').

omega_variable(
    competing_rights_frameworks,
    'How should the rights of transgender individuals to identity recognition be balanced against the rights of cisgender women to sex-based protections and single-sex spaces?',
    'Development of legal frameworks that explicitly address and reconcile these competing claims, potentially through carve-outs or alternative provisions for specific contexts. Societal consensus on the hierarchy or scope of these rights.',
    'The current classification as Tangled Rope reflects this tension. A clear resolution could shift it towards a Rope (if a mutually beneficial coordination is found) or a Snare (if one set of rights is systematically overridden without genuine coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_rights_frameworks, preference, 'Irreducible conflict between competing rights claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1990, gendered_category_membership__gender_identity_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(gend_tr_t2000, gendered_category_membership__gender_identity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__gender_identity_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(gend_tr_t2015, gendered_category_membership__gender_identity_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(gend_tr_t2020, gendered_category_membership__gender_identity_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(gend_tr_t2024, gendered_category_membership__gender_identity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gend_be_t1990, gendered_category_membership__gender_identity_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(gend_be_t2000, gendered_category_membership__gender_identity_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__gender_identity_reading, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(gend_be_t2015, gendered_category_membership__gender_identity_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(gend_be_t2020, gendered_category_membership__gender_identity_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(gend_be_t2024, gendered_category_membership__gender_identity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1990, gendered_category_membership__gender_identity_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(gend_su_t2000, gendered_category_membership__gender_identity_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__gender_identity_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(gend_su_t2015, gendered_category_membership__gender_identity_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(gend_su_t2020, gendered_category_membership__gender_identity_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(gend_su_t2024, gendered_category_membership__gender_identity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, womens_sports_eligibility_rules).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, sex_segregated_spaces_policies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gendered_category_membership' kernel. Its structural properties and classification are distinct from the 'biological_sex_reading' and 'social_role_reading', which are modeled as separate constraints due to differing epsilon values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
