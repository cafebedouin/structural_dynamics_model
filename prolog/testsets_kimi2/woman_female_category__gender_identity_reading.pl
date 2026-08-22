% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Identity-Based Category Membership for Woman/Female
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint story instantiates the gender_identity_reading of the
 *   woman_female_category kernel: the claim that membership in the categories
 *   'woman' and 'female' is determined solely by internal self-identification
 *   with the gender category, independent of biological sex. Under this
 *   reading, the constraint operates as a legal and social coordination
 *   mechanism that reassigns categorical membership from biological indices
 *   to subjective identity claims. It is actively enforced through
 *   anti-discrimination law, identity document issuance, and social sanction
 *   against misgendering or sex-based exclusion. The arrangement extracts
 *   dignity, safety, and definitional stability from sex-category-reliant
 *   women and from trans women whose presence in female spaces becomes
 *   politicized, while conferring recognition and material access on
 *   transgender individuals seeking identity-based protections. The
 *   claim/metric gap is deliberate: the reading is claimed as a scaffold or
 *   rope by its advocates (a progressive coordination mechanism), while the
 *   authored metrics describe substantially extractive, actively enforced
 *   operation with high resistance and asymmetric cost distribution â the
 *   engine measures that divergence.
 *
 * KEY AGENTS:
 *   - transgender_individuals_seekers: Primary beneficiary (moderate/constrained) â gain recognition and access through self-ID.
 *   - women_sex_based_rights_claimants: Primary payer (organized/constrained) â lose sex-based boundaries and protections.
 *   - trans_women_female_space_users: Secondary payer with beneficiary crossover (powerless/trapped) â gain access but face politicized dignity harms.
 *   - state_legal_administrators: Agenda setter (institutional/constrained) â administers the legal category.
 *   - gender_identity_advocacy_organizations: Secondary beneficiary (organized/mobile) â politically sustains the framework.
 *   - sex_based_feminist_advocates: Excluded voice (organized/constrained) â objects but is kept out of legitimizing conversations.
 *   - human_rights_observers: Analytical observer (institutional/analytical) â monitors both recognition gains and sex-rights erosion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.72).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.65).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity-Based Category Membership for Woman/Female").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '8e9cf67e-4729-48d4-ae18-2dade07c2d8f').
narrative_ontology:cs_kernel_codification('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', formalized).
narrative_ontology:cs_authority_grounding('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', lineage).
narrative_ontology:cs_interpretation_layer_present('8e9cf67e-4729-48d4-ae18-2dade07c2d8f').
narrative_ontology:cs_reading_relation('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', foundational, self_identification_determines_membership).
narrative_ontology:cs_axiom_status(self_identification_determines_membership, holdable).
narrative_ontology:cs_axiom_grounding('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', self_identification_determines_membership, deontological).
narrative_ontology:cs_axiom('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', foundational, biological_sex_irrelevant_to_social_category).
narrative_ontology:cs_axiom_status(biological_sex_irrelevant_to_social_category, holdable).
narrative_ontology:cs_axiom_grounding('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', biological_sex_irrelevant_to_social_category, conventional).
narrative_ontology:cs_reference_frame('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', self_determination_framework).
narrative_ontology:cs_drift_state('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', contemporary_policy_contestation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8e9cf67e-4729-48d4-ae18-2dade07c2d8f', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals_seekers).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, women_sex_based_rights_claimants).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, trans_women_female_space_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, trans_women_female_space_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal and social recognition of their gender identity through self-declaration, gaining access to identity documents, gender-segregated services, and protections against misgendering.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals_seekers, beneficiary,
    moderate, biographical, constrained, national).

% Assert that female category membership requires biological sex boundaries for safety, fairness, and material rights; experience the self-ID rule as dissolving sex-based protections in shelters, prisons, sports, and data collection.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, women_sex_based_rights_claimants, payer,
    organized, biographical, constrained, national).

% Access female-only spaces under self-ID frameworks but face heightened scrutiny, invalidation, and safety risks when presence in those spaces becomes politicized; experience dignity harms from constant contestation of their right to be there.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, trans_women_female_space_users, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, trans_women_female_space_users, beneficiary).

% Administer legal gender recognition systems, issue amended identity documents, and adjudicate discrimination claims under self-ID statutes; define the evidentiary and procedural requirements for category membership.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, state_legal_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Organize political and legal campaigns to entrench self-ID frameworks, provide resources for gender recognition claims, and defend the constraint against rollback or sex-based exemptions.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Argue that female category membership must track biological sex to protect women's rights; structurally excluded from policy consultations that adopt self-ID frameworks, or dismissed as outside the bounds of legitimate discourse when they object.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_based_feminist_advocates, excluded,
    organized, biographical, constrained, national).

% Monitor state compliance with international human rights standards regarding gender identity recognition; document both advances in self-ID access and concerns about impacts on sex-based rights.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, human_rights_observers, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and legal recognition of gender identity without requiring medical diagnosis, surgery, or sterilization; provides a unified categorical framework for identity documents, anti-discrimination protections, and access to gender-segregated services.
% TRANSFER_FUNCTION: Moves categorical membership, legal status, and access to female-only spaces and services from biological-sex-based determinants to self-declared identity; transfers recognition and bureaucratic access to self-identifiers while imposing boundary-loss and dignity costs on those who rely on sex-based categorization.
% ABSENT_VOICES: Sex-based feminist advocates who argue for biological sex boundaries in law and policy are frequently excluded from consultations or delegitimized; medical professionals who maintain that sex is clinically relevant are increasingly sidelined in policy discourse.
% DISAPPEARANCE_RATIONALE: If self-ID as the criterion for woman/female membership vanished overnight, legal gender recognition would revert to medical or biological gatekeeping, access to female prisons, shelters, and sports would be re-biologized, identity documents would be reissued on anatomical/chromosomal criteria, and the social ontology of 'woman' would shift back toward sex-based indexing.
% FOUNDING_PROBLEM: Legal gender recognition historically required medical diagnosis of gender dysphoria, sterilization, and surgical intervention, creating bureaucratic violence and indignity for transgender people seeking to live in their identified gender.
% FOUNDING_PROBLEM_CORROBORATION: Trans rights organizations and international human rights bodies attest the founding problem of medical gatekeeping. Sex-based feminist advocates, some detransitioners, and independent medical ethicists attest the problem is either solved or that the remedy has overexpanded; peer-reviewed longitudinal research on desistance and post-transition regret provides external corroboration that the problem's scope and the solution's proportionality are disputed.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.72) because the constraint reallocates categorical membership and access to sex-segregated provisions from one group to another, generating concentrated dignity and safety costs for payers. Suppression is substantial (0.65) because the constraint's persistence depends on legal penalties for exclusion, institutional policies enforcing self-ID, and social stigma against sex-based objections. Theater ratio is moderate (0.27) because while the coordination function (recognition, reduced gatekeeping) is genuine, a growing share of enforcement activity serves boundary dissolution rather than the originally articulated problem of medical gatekeeping. Accessibility collapse (0.45) is moderate: sex-based alternatives still exist in many jurisdictions and discourses but are increasingly delegitimized. Resistance (0.70) is high due to organized feminist opposition, medical professional dissent, and legislative backlash.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (state administrators, trans seekers, advocacy orgs) experience the constraint as a necessary correction to oppressive medical gatekeeping and a dignity-affirming coordination mechanism. The payer seats (sex-based rights claimants, trans women in contested spaces) experience it as an imposed redefinition that dissolves material protections and politicizes their presence. The engine computes this divergence from the structural asymmetry in exit options (beneficiaries mobile/constrained, payers trapped/constrained) and the directional flow of costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (transgender_individuals_seekers, gender_identity_advocacy_organizations) receive low directionality: the constraint subsidizes their access to recognition and legal status, so effective extraction is damped or inverted. Payers (women_sex_based_rights_claimants, trans_women_female_space_users) receive high directionality: the constraint extracts definitional stability, safety assurances, and dignity from them. The trans_women_female_space_users seat is structurally complex: they are nominally beneficiaries of the rule (it grants access) but pay its dignity costs when enforcement politicizes their presence, justifying a secondary_role of beneficiary with primary role payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â medical gatekeeping and bureaucratic indignity in gender recognition â is substantially solved in jurisdictions with self-ID, yet the arrangement persists and expands into new domains (sports, prisons, shelters) beyond the original recognition context. This mismatch between founding purpose and current scope prevents mislabeling the constraint as a pure scaffold; the absence of a sunset clause and the expansion into contested domains indicate tangled rope rather than transitional coordination. The theater ratio captures the growing share of performative boundary-dissolution that no longer tracks the original coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trans_women_victimization_mechanism,
    'Are trans women in female-only spaces harmed by the constraint''s structural design, by incomplete enforcement of the self-ID rule, or by external backlash independent of the constraint?',
    'Comparative analysis across jurisdictions with full self-ID enforcement versus partial recognition; measure safety, dignity, and integration outcomes for trans women in female-only spaces.',
    'If harm persists under full enforcement, the constraint structurally extracts from a beneficiary group, complicating directionality. If harm is enforcement-gap dependent, extraction is from incomplete implementation rather than the constraint itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trans_women_victimization_mechanism, empirical, 'Ambiguity about whether trans women victims are harmed by the constraint or its absence.').

omega_variable(
    category_naturalness,
    'Is the woman/female category a natural kind that self-ID incorrectly dissolves, or a socially constructed category that self-ID legitimately reconfigures?',
    'Cross-cultural and historical analysis of gender category stability; empirical study of whether self-ID frameworks produce categorical collapse or stable social coordination.',
    'If the category is a natural kind, the constraint is a false summit presenting constructed rule as recognition of reality. If socially constructed, the constraint is a legitimate coordination mechanism whose extraction is the price of updating the category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_naturalness, conceptual, 'Whether the category is natural or constructed, with implications for mountain versus tangled rope classification.').

omega_variable(
    suppression_ambiguity,
    'Is the suppression of sex-based alternative categorizations achieved primarily through structural enforcement (legal penalties, institutional exclusion) or through internalized normative pressure (social stigma, professional sanction for dissent)?',
    'Track suppression persistence after legal rollback: if sex-based categorization returns quickly after repeal, suppression was primarily structural; if stigma persists, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure, and the constraint''s extraction is deeper than the legal framework alone suggests. This would push classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__gender_identity_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__gender_identity_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__gender_identity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__gender_identity_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.27).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(woma_be_t4, woman_female_category__gender_identity_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(woma_be_t8, woman_female_category__gender_identity_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(woma_be_t12, woman_female_category__gender_identity_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(woma_be_t16, woman_female_category__gender_identity_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.72).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(woman_female_category__gender_identity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the woman_female_category kernel, decomposed from the colloquial label 'woman/female' which conflates biological sex, gender identity, and hybrid contextual criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
