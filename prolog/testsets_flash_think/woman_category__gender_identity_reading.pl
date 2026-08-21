% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Woman Category: Gender Identity Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint defines 'woman' based on internal gender identity,
 *   irrespective of assigned sex at birth. It is one reading of the contested
 *   'woman_category' kernel. While it aims to coordinate inclusion and
 *   self-determination for transgender women, its implementation often leads
 *   to asymmetric extraction from cisgender women, particularly in contexts
 *   like sports and sex-segregated spaces, where access rights collide with
 *   sex-based protections. The constraint is actively enforced through policy
 *   and social pressure, suppressing alternative definitions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.75).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.8).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Woman Category: Gender Identity Reading").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '34e4beab-074c-404d-8f8b-075cc8163510').
narrative_ontology:cs_kernel_codification('34e4beab-074c-404d-8f8b-075cc8163510', formalized).
narrative_ontology:cs_authority_grounding('34e4beab-074c-404d-8f8b-075cc8163510', practice).
narrative_ontology:cs_interpretation_layer_present('34e4beab-074c-404d-8f8b-075cc8163510').
narrative_ontology:cs_reading_relation('34e4beab-074c-404d-8f8b-075cc8163510', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('34e4beab-074c-404d-8f8b-075cc8163510', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('34e4beab-074c-404d-8f8b-075cc8163510', foundational, gender_identity_is_determinative).
narrative_ontology:cs_axiom_status(gender_identity_is_determinative, holdable).
narrative_ontology:cs_axiom_grounding('34e4beab-074c-404d-8f8b-075cc8163510', gender_identity_is_determinative, deontological).
narrative_ontology:cs_axiom('34e4beab-074c-404d-8f8b-075cc8163510', foundational, self_identification_is_primary).
narrative_ontology:cs_axiom_status(self_identification_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('34e4beab-074c-404d-8f8b-075cc8163510', self_identification_is_primary, deontological).
narrative_ontology:cs_reference_frame('34e4beab-074c-404d-8f8b-075cc8163510', gender_identity_as_primary_determinant).
narrative_ontology:cs_drift_state('34e4beab-074c-404d-8f8b-075cc8163510', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('34e4beab-074c-404d-8f8b-075cc8163510', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, institutional_bodies_adopting_policy).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, cisgender_women_advocating_sex_based_rights).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, women_in_sex_segregated_spaces).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_athletes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal and social recognition as women, allowing access to spaces, services, and categories aligned with their gender identity. Their ability to exit this framework without losing recognition is constrained by the prevailing social and legal norms.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, constrained, global).

% Actively promote and defend the definition of 'woman' based on gender identity, influencing policy and public discourse. They benefit from the expansion of this framework and its adoption by institutions.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).

% Implement and enforce policies that define 'woman' by gender identity, often in response to advocacy and legal challenges. They bear the administrative costs and political friction of implementation but gain perceived alignment with progressive values.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, institutional_bodies_adopting_policy, agenda_setter,
    institutional, biographical, constrained, national).

% Experience the erosion of sex-based categories and protections, particularly in areas like single-sex spaces, sports, and data collection. They actively resist this definition but are often marginalized or excluded from policy discussions.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, cisgender_women_advocating_sex_based_rights, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, cisgender_women_advocating_sex_based_rights, excluded).

% Find their expectations of single-sex spaces (e.g., changing rooms, shelters) altered by the inclusion of transgender women. Their options are limited to adapting, avoiding, or privately resisting, often without institutional support.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, women_in_sex_segregated_spaces, payer,
    powerless, immediate, constrained, local).

% Compete in sports categories that include transgender women, leading to concerns about fairness and competitive equity due to biological differences. Their ability to exit competitive sport or demand sex-segregated categories is highly constrained by governing bodies.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_athletes, payer,
    powerless, biographical, constrained, national).

% Are not directly centered by this reading, which focuses on identity rather than biological variation. Their experiences may or may not align with the binary gender identity framework, making them observers of its application.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, intersex_individuals, observer,
    powerless, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure that individuals who identify as women are recognized as such in social, legal, and political contexts, fostering inclusion and self-determination for transgender women.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to certain spaces, and eligibility for categories from a definition primarily based on sex assigned at birth to one based on internal gender identity. This transfer is from cisgender women (in terms of exclusive access/definition) to transgender women (in terms of inclusion).
% ABSENT_VOICES: Advocates for sex-based definitions of 'woman' are frequently excluded from policy-making and public discourse, often labeled as discriminatory or transphobic, preventing their concerns about sex-based rights from being fully addressed.
% DISAPPEARANCE_RATIONALE: If the gender-identity-based definition of 'woman' vanished overnight, legal and social frameworks would largely revert to sex-based definitions. This would cause significant disruption and misrecognition for transgender women, while also leading to a re-evaluation of sex-segregated spaces and policies, and a re-assertion of sex-based rights for cisgender women.
% FOUNDING_PROBLEM: The historical exclusion, misrecognition, and discrimination faced by transgender women within the category of 'woman', leading to a lack of legal and social protections aligned with their self-identified gender.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy groups, human rights organizations, and many medical and psychological associations corroborate the ongoing problem of misrecognition and discrimination against transgender individuals. Critics (e.g., sex-based rights advocates) contest the framing of the problem, arguing that the proposed solution creates new harms for cisgender women, but do not deny the historical exclusion of transgender people.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant costs borne by cisgender women in areas where sex-based protections are diminished or redefined, such as competitive sports and single-sex spaces. Suppression (0.80) is high due to active efforts to delegitimize or silence dissenting views that prioritize sex-based definitions. The low theater ratio (0.15) indicates that the constraint is genuinely and actively enforced, not merely performative. Resistance (0.70) is substantial, reflecting ongoing advocacy and legal challenges from groups prioritizing sex-based rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender women and their advocates, this constraint is a necessary coordination mechanism for inclusion and human rights. From the perspective of many cisgender women, particularly those advocating for sex-based rights, the same constraint operates as a snare, extracting protections and eroding categories vital for their safety and equity. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women and gender identity advocates are clear beneficiaries, gaining recognition and access. Institutional bodies adopting these policies also benefit from perceived inclusivity. Cisgender women advocating for sex-based rights, women in sex-segregated spaces, and female athletes are victims, bearing the costs of redefined categories and diminished protections. Intersex individuals are observers, as their unique biological realities are not the primary focus of this identity-based definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impact_on_sex_based_rights,
    'What is the actual, measurable impact of defining ''woman'' by gender identity on sex-based rights and protections for cisgender women, particularly in areas like safety, privacy, and competitive fairness?',
    'Longitudinal studies on crime rates in mixed-sex spaces, analysis of competitive outcomes in women''s sports, and surveys on women''s perceptions of safety and privacy in redefined single-sex spaces.',
    'If significant negative impacts are empirically demonstrated, the constraint''s extractiveness and suppression for cisgender women would be further validated, potentially shifting its classification towards a Snare for those seats. If impacts are negligible, the coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_sex_based_rights, empirical, 'Empirical assessment of the collision between gender identity rights and sex-based rights.').

omega_variable(
    conceptual_coherence_of_category,
    'Is the category ''woman'', when defined solely by gender identity, conceptually coherent and functionally robust across all contexts (e.g., medical, legal, social, sports) without reference to biological sex?',
    'Philosophical and legal analysis of definitional paradoxes, and practical observation of how the definition functions in contexts where material sex differences are relevant. This is a conceptual question that empirical data can inform but not fully resolve.',
    'If conceptual incoherence is widely acknowledged, the constraint''s legitimacy and long-term stability would be undermined, potentially increasing its theater_ratio as enforcement becomes more performative. If coherence is established, its foundational stability would increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_coherence_of_category, conceptual, 'Conceptual robustness of an identity-only definition of ''woman''.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of sex-based rights advocacy primarily structural (e.g., legal frameworks, institutional policies) or internalized (e.g., social stigma, fear of reprisal)?',
    'Analysis of public discourse trends, institutional responses to dissent, and surveys of individuals'' willingness to express sex-based rights views in different contexts. If suppression persists after structural barriers are removed, it indicates internalized suppression.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would amplify the effective extraction for those targeted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for sex-based rights advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2000, woman_category__gender_identity_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(woma_tr_t2005, woman_category__gender_identity_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(woma_tr_t2010, woman_category__gender_identity_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(woma_tr_t2015, woman_category__gender_identity_reading, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(woma_tr_t2020, woman_category__gender_identity_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(woma_tr_t2025, woman_category__gender_identity_reading, theater_ratio, 2025, 0.15).
narrative_ontology:measurement(woma_tr_t2030, woman_category__gender_identity_reading, theater_ratio, 2030, 0.15).

% Extraction over time
narrative_ontology:measurement(woma_be_t2000, woman_category__gender_identity_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(woma_be_t2005, woman_category__gender_identity_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(woma_be_t2010, woman_category__gender_identity_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(woma_be_t2015, woman_category__gender_identity_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(woma_be_t2020, woman_category__gender_identity_reading, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement(woma_be_t2025, woman_category__gender_identity_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement(woma_be_t2030, woman_category__gender_identity_reading, base_extractiveness, 2030, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2000, woman_category__gender_identity_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(woma_su_t2005, woman_category__gender_identity_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(woma_su_t2010, woman_category__gender_identity_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(woma_su_t2015, woman_category__gender_identity_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(woma_su_t2020, woman_category__gender_identity_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(woma_su_t2025, woman_category__gender_identity_reading, suppression_requirement, 2025, 0.79).
narrative_ontology:measurement(woma_su_t2030, woman_category__gender_identity_reading, suppression_requirement, 2030, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sex_based_protections_policy).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, womens_sports_eligibility).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_category' kernel, each representing a distinct structural claim about the definition of 'woman'. They are linked to capture their interdependencies and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
