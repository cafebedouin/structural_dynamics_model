% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__identity_reading, []).

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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Gender Category Membership by Self-Identification (Identity Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint defines category membership, particularly for 'woman',
 *   based on an individual's subjective gender identity
 *   (self-identification). It is one reading of the broader
 *   'sex_gender_category' kernel. This reading includes trans women in the
 *   'woman' category, expanding the victim set to include trans women
 *   experiencing misogyny, but also leads to cis women losing exclusive claim
 *   to sex-based protections. While boundary enforcement costs are low due to
 *   self-declaration, there is high conflict over access to single-sex
 *   spaces. The constraint is claimed as a Tangled Rope, reflecting its dual
 *   function of coordinating identity recognition while extracting from
 *   cisgender women's sex-based rights.
 *
 * KEY AGENTS:
 *   - transgender_individuals: Primary beneficiary (identity_locked/global) — gains recognition and inclusion.
 *   - identity_advocates: Agenda setter (organized/global) — promotes and defends self-identification.
 *   - cisgender_women: Primary payer (powerful/global) — experiences redefinition of 'woman' and loss of exclusive sex-based protections.
 *   - gender_critical_feminists: Payer (moderate/national) — actively resists redefinition, faces social penalties.
 *   - legal_systems: Agenda setter (institutional/national) — codifies and enforces self-identification.
 *   - public_institutions: Payer (institutional/local) — implements policies, manages conflicts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.65).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.7).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Gender Category Membership by Self-Identification (Identity Reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '54ea55c8-bf4e-4da0-86e7-753c1ffc2280').
narrative_ontology:cs_kernel_codification('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', formalized).
narrative_ontology:cs_authority_grounding('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', lineage).
narrative_ontology:cs_interpretation_layer_present('54ea55c8-bf4e-4da0-86e7-753c1ffc2280').
narrative_ontology:cs_reading_relation('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', foundational, gender_identity_is_primary_determinant_of_gender).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_gender, holdable).
narrative_ontology:cs_axiom_grounding('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', gender_identity_is_primary_determinant_of_gender, deontological).
narrative_ontology:cs_axiom('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', foundational, self_identification_is_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(self_identification_is_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', self_identification_is_sufficient_for_category_membership, conventional).
narrative_ontology:cs_reference_frame('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', identity_affirmation_framework).
narrative_ontology:cs_drift_state('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', contemporary_social_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('54ea55c8-bf4e-4da0-86e7-753c1ffc2280', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, identity_advocates).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cisgender_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, gender_critical_feminists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, public_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition and inclusion in categories aligning with their gender identity, reducing social dysphoria and discrimination. Their identity is central to their self-concept, making 'exit' from this framework unthinkable.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and defend self-identification as the primary determinant of gender category. They shape policy and public discourse, benefiting from the expansion of identity-based rights and recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, identity_advocates, agenda_setter,
    organized, generational, constrained, global).

% Experience a redefinition of 'woman' that includes individuals assigned male at birth, leading to concerns about sex-based rights, single-sex spaces, and data collection. Their ability to assert sex-based boundaries is constrained by legal and social pressure.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cisgender_women, payer,
    powerful, biographical, constrained, global).

% Actively resist the redefinition of sex-based categories, arguing it erodes protections for biological women. They face social ostracization and professional penalties for their views, making their 'exit' from the prevailing discourse costly.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, payer,
    moderate, generational, constrained, national).

% Are tasked with codifying and enforcing self-identification principles, often through anti-discrimination laws. They mediate conflicts arising from competing interpretations of sex and gender, incurring administrative and social costs.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Implement policies based on self-identification, such as access to single-sex spaces (bathrooms, changing rooms, shelters). They bear the costs of managing conflicts, adapting facilities, and navigating public controversy.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, public_institutions, payer,
    institutional, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social and legal recognition of gender identity, ensuring individuals are affirmed in their self-declared gender and reducing discrimination against transgender people.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to gendered spaces, and definitional authority over 'woman' from cisgender women to transgender individuals, particularly trans women.
% ABSENT_VOICES: Children and vulnerable women in single-sex spaces, whose safety and privacy concerns are often dismissed or framed as transphobic. Their voices are marginalized in policy debates, often through accusations of bigotry.
% DISAPPEARANCE_RATIONALE: If self-identification as the sole determinant of gender category vanished overnight, legal frameworks would revert to sex-based definitions, single-sex spaces would be re-evaluated, and the social recognition of transgender identities would be significantly altered, leading to a major reorganization of identity politics and legal classification.
% FOUNDING_PROBLEM: Transgender individuals faced systemic discrimination and lack of recognition for their gender identity, leading to social exclusion, violence, and mental health crises.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy groups and human rights organizations attest to the ongoing problem of discrimination. While some gender-critical groups dispute the scope or nature of the problem, the existence of discrimination against transgender individuals is widely corroborated by independent research and international bodies.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because cisgender women's sex-based rights and protections are diluted or redefined, and their ability to assert sex-based boundaries is curtailed. Suppression (0.70) is high due to social and legal pressures against challenging self-identification, often through accusations of bigotry, which limits open debate and alternative framings. Theater ratio (0.20) is relatively low, as the constraint's primary function is active redefinition and enforcement, not mere performance. Accessibility collapse (0.40) is moderate; while direct alternatives to self-ID are suppressed, resistance and alternative framings persist. Resistance (0.75) is high, reflecting significant ongoing social and political conflict.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals and identity advocates, this constraint is a necessary coordination mechanism for affirming identity and combating discrimination. From the perspective of cisgender women and gender-critical feminists, it is an extractive mechanism that redefines fundamental categories and erodes sex-based rights. The engine's classification will reflect this divergence based on the structural positions of beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals are full beneficiaries (d=0.0) as the constraint directly affirms their identity. Identity advocates are also beneficiaries (d=0.1) as they gain influence and achieve their policy goals. Cisgender women are targets (d=0.9) as they bear the costs of category redefinition and loss of exclusive protections. Gender-critical feminists are also targets (d=0.9) due to the direct impact on their advocacy and the social costs they incur. Legal systems and public institutions are agenda-setters and payers, experiencing both coordination benefits and the costs of managing conflict (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates identity recognition for one group (transgender individuals) while simultaneously extracting from another (cisgender women's sex-based rights) through the same structure. It requires active enforcement to maintain, as evidenced by ongoing legal and social disputes. The classification prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impact_on_sex_based_rights,
    'To what extent does self-identification as the sole determinant of gender category genuinely erode or merely redefine sex-based rights and protections for cisgender women?',
    'Longitudinal studies on the efficacy of sex-based protections in jurisdictions with self-ID laws, comparing outcomes in areas like domestic violence shelters, sports, and health data collection.',
    'If erosion is empirically demonstrated, the extractiveness for cisgender women is higher than currently estimated; if it''s primarily redefinition without functional loss, extractiveness is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_sex_based_rights, empirical, 'Assessing the functional impact of self-ID on sex-based protections.').

omega_variable(
    social_cohesion_vs_identity_affirmation,
    'Is the conflict arising from this constraint an unavoidable consequence of identity affirmation, or does it stem from a failure to balance competing group rights and needs?',
    'Comparative analysis of policy approaches in different jurisdictions, examining whether alternative models (e.g., medical gatekeeping, separate but equal provisions) achieve identity affirmation with less social conflict.',
    'If the conflict is primarily due to an imbalance, the constraint''s suppression and extractiveness could be reduced by alternative policy designs; if unavoidable, the current metrics reflect an inherent trade-off.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_cohesion_vs_identity_affirmation, preference, 'Evaluating the necessity and manageability of social conflict in identity politics.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (e.g., social penalties for gender-critical views) structural (e.g., legal restrictions on speech) or internalized (e.g., self-censorship due to fear of social ostracism)?',
    'Analysis of legal cases involving ''hate speech'' or ''misgendering'' alongside surveys on self-censorship and social pressure among those holding gender-critical views. If suppression persists after legal barriers are removed, it is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. If primarily structural, legal reforms could more directly alleviate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__identity_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__identity_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__identity_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__identity_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__identity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__identity_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__identity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__identity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, single_sex_spaces_access).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, gender_data_collection_standards).

% DUAL FORMULATION NOTE:
% This constraint is the 'identity_reading' of the 'sex_gender_category' kernel, which also has 'biology_reading' and 'hybrid_reading' siblings. Each reading represents a distinct structural constraint with different ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
