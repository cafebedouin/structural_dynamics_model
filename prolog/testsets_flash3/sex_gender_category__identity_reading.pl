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
 *   human_readable: Category Membership by Subjective Gender Identity (Identity Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint defines category membership, specifically for 'woman',
 *   based on subjective gender identity (self-identification). It is one
 *   reading of the broader 'sex_gender_category' kernel. This reading
 *   includes trans women in the 'woman' category, expanding the victim set to
 *   include trans women experiencing misogyny, but also means cis women lose
 *   exclusive claim to sex-based protections. While boundary enforcement
 *   costs are low due to self-declaration, there is high conflict over space
 *   access and the definition of 'woman'. The claimed type is 'tangled_rope'
 *   because it genuinely coordinates social inclusion for trans women while
 *   simultaneously extracting from cis women by redefining their sex-based
 *   category.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.65).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.7).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Category Membership by Subjective Gender Identity (Identity Reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '97e2a512-74e9-468f-91f8-9434b678ef4c').
narrative_ontology:cs_kernel_codification('97e2a512-74e9-468f-91f8-9434b678ef4c', distributed).
narrative_ontology:cs_authority_grounding('97e2a512-74e9-468f-91f8-9434b678ef4c', practice).
narrative_ontology:cs_interpretation_layer_present('97e2a512-74e9-468f-91f8-9434b678ef4c').
narrative_ontology:cs_reading_relation('97e2a512-74e9-468f-91f8-9434b678ef4c', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('97e2a512-74e9-468f-91f8-9434b678ef4c', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('97e2a512-74e9-468f-91f8-9434b678ef4c', foundational, gender_identity_is_primary_determinant_of_category).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_category, holdable).
narrative_ontology:cs_axiom_grounding('97e2a512-74e9-468f-91f8-9434b678ef4c', gender_identity_is_primary_determinant_of_category, deontological).
narrative_ontology:cs_axiom('97e2a512-74e9-468f-91f8-9434b678ef4c', foundational, self_identification_is_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(self_identification_is_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('97e2a512-74e9-468f-91f8-9434b678ef4c', self_identification_is_sufficient_for_category_membership, conventional).
narrative_ontology:cs_reference_frame('97e2a512-74e9-468f-91f8-9434b678ef4c', gender_identity_affirming_framework).
narrative_ontology:cs_drift_state('97e2a512-74e9-468f-91f8-9434b678ef4c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('97e2a512-74e9-468f-91f8-9434b678ef4c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women).
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

% Gain access to 'woman'-designated spaces and categories, affirming their gender identity. They experience misogyny and seek inclusion in protections for women. Their identity is central to their self-concept, making exit from this framework unthinkable.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, global).

% Lose exclusive claim to sex-based protections and spaces (e.g., changing rooms, shelters, sports categories) that were historically established for biological women. They bear the cost of redefining 'woman' to include trans women, leading to perceived loss of safety, privacy, and fairness in sports. Their options are to accept the redefinition, or to organize resistance.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women, payer,
    organized, generational, constrained, global).

% Actively promote and enforce the self-identification principle, shaping legal and social norms. They benefit from the expansion of gender identity rights and the validation of their ideological framework. They have significant influence in policy-making bodies and cultural institutions.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_identity_advocates, agenda_setter,
    institutional, generational, mobile, global).

% Actively resist the self-identification principle, arguing it erodes women's sex-based rights and protections. They face social and professional ostracization for their views. Their options are to continue organizing and advocating for sex-based rights, or to disengage from the political process.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, payer,
    organized, generational, constrained, national).

% Are tasked with codifying and enforcing laws based on this reading, leading to complex legal challenges regarding sex-based rights and non-discrimination. They mediate conflicts between competing rights claims and bear the cost of legal ambiguity and social unrest.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% Implement policies based on self-identification, often facing backlash from various groups. They bear the operational costs of adapting facilities, policies, and language, and managing internal and external conflicts. Their options are to comply with legal mandates or face litigation.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, public_institutions, payer,
    institutional, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social recognition and legal inclusion for individuals whose gender identity differs from their sex assigned at birth, by aligning social categories with subjective identity.
% TRANSFER_FUNCTION: Transfers social and legal recognition of 'woman' status from being exclusively tied to biological sex to being inclusive of subjective gender identity. This transfers access to sex-segregated spaces and resources.
% ABSENT_VOICES: Children and vulnerable adults, whose safety and privacy concerns in sex-segregated spaces are often represented by others, would likely object if they could articulate their interests directly. Their voices are mediated through parents, advocates, or institutions.
% DISAPPEARANCE_RATIONALE: If the principle of self-identification for category membership vanished overnight, legal frameworks would revert to sex-based definitions, trans women would lose access to 'woman'-designated spaces, and the social and political landscape around gender identity would fundamentally shift, leading to widespread reorganization of rights claims and social norms.
% FOUNDING_PROBLEM: The historical exclusion and discrimination faced by transgender individuals, particularly trans women, from social and legal categories that align with their gender identity.
% FOUNDING_PROBLEM_CORROBORATION: Transgender individuals and their advocates attest that the problem of exclusion and discrimination remains live. Sociological studies on transphobia and gender-based violence corroborate the ongoing challenges faced by trans women. Gender-critical feminists contest the framing of the problem as one of 'exclusion' from the category 'woman', arguing it is a redefinition of 'woman' itself.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) due to the perceived loss of exclusive sex-based protections and resources for cis women. Suppression is also high (0.70) because active social and legal enforcement mechanisms are required to maintain the self-identification principle against resistance from gender-critical groups, often involving 'no debate' stances and deplatforming. Theater ratio is moderate (0.40) as some efforts to 'educate' and 'sensitize' are performative, masking the underlying power dynamics and suppression of dissenting views. Accessibility collapse is moderate (0.45) as alternatives (e.g., single-sex spaces defined by biology) are actively suppressed but not entirely eliminated, leading to ongoing contestation. Resistance is high (0.80) from cis women and gender-critical feminists who actively challenge this definition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of trans women and gender identity advocates, this constraint is a necessary coordination mechanism for inclusion and human rights. From the perspective of cis women and gender-critical feminists, it is an extractive snare that undermines their sex-based rights. The engine's classification as 'tangled_rope' reflects this dual function of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and gender identity advocates are beneficiaries, gaining social recognition and legal inclusion. Cis women and gender-critical feminists are payers, bearing the costs of redefined categories and loss of exclusive protections. Legal systems and public institutions act as agenda-setters, implementing and enforcing the policies, often at the cost of managing social conflict.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_woman_ambiguity,
    'Is ''woman'' a category defined by biological sex, subjective gender identity, or a combination?',
    'Conceptual clarity through philosophical and legal consensus, or a societal shift in understanding that resolves the definitional conflict.',
    'If ''woman'' is definitively understood as a biological category, this constraint would be reclassified as a snare for cis women; if it is definitively understood as a gender identity, the extraction from cis women would be re-evaluated as a necessary cost of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_woman_ambiguity, conceptual, 'Ambiguity in the foundational definition of ''woman'' and its implications for category membership.').

omega_variable(
    safety_privacy_fairness_empirical_impact,
    'What is the empirical impact of self-identification policies on the safety, privacy, and fairness for cis women in sex-segregated spaces and sports?',
    'Longitudinal empirical studies and data collection on incidents in mixed-sex spaces, and performance outcomes in sports categories, disaggregated by sex and gender identity.',
    'Robust evidence of significant negative impacts would increase the measured extractiveness and suppression for cis women, potentially reclassifying the constraint closer to a snare. Lack of such evidence would reduce the perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_privacy_fairness_empirical_impact, empirical, 'Empirical evidence regarding the consequences of self-identification for cis women''s rights.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gender-critical views structural (e.g., legal restrictions, institutional policies) or internalized (e.g., self-censorship due to social pressure)?',
    'Analysis of legal challenges to free speech, institutional policies on ''hate speech'' vs. ''misgendering'', and surveys on self-censorship among dissenting groups. If suppression persists after formal barriers are removed, it is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as dissenting voices carry the suppression with them. If purely structural, removing formal barriers would significantly reduce suppression.',
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
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__identity_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__identity_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.4).

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
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, single_sex_spaces_policy).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, womens_sports_regulations).

% DUAL FORMULATION NOTE:
% This constraint is the 'identity_reading' of the 'sex_gender_category' kernel. It is linked to the 'biology_reading' and 'hybrid_reading' as alternative interpretations of the same core concept, each with distinct structural implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
