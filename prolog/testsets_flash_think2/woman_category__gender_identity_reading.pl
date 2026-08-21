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
 *   human_readable: Category 'Woman' Defined by Gender Identity
 *   domain: social_policy/political_philosophy/law
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.75).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.7).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Category 'Woman' Defined by Gender Identity").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "social_policy/political_philosophy/law").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, 'a52f3c0f-9993-4be3-b108-132f46cd08ae').
narrative_ontology:cs_kernel_codification('a52f3c0f-9993-4be3-b108-132f46cd08ae', formalized).
narrative_ontology:cs_authority_grounding('a52f3c0f-9993-4be3-b108-132f46cd08ae', practice).
narrative_ontology:cs_interpretation_layer_present('a52f3c0f-9993-4be3-b108-132f46cd08ae').
narrative_ontology:cs_reading_relation('a52f3c0f-9993-4be3-b108-132f46cd08ae', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('a52f3c0f-9993-4be3-b108-132f46cd08ae', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('a52f3c0f-9993-4be3-b108-132f46cd08ae', foundational, gender_identity_is_primary_determinant_of_womanhood).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('a52f3c0f-9993-4be3-b108-132f46cd08ae', gender_identity_is_primary_determinant_of_womanhood, deontological).
narrative_ontology:cs_axiom('a52f3c0f-9993-4be3-b108-132f46cd08ae', secondary, sex_assigned_at_birth_is_irrelevant_to_womanhood_category).
narrative_ontology:cs_axiom_status(sex_assigned_at_birth_is_irrelevant_to_womanhood_category, holdable).
narrative_ontology:cs_axiom_grounding('a52f3c0f-9993-4be3-b108-132f46cd08ae', sex_assigned_at_birth_is_irrelevant_to_womanhood_category, conventional).
narrative_ontology:cs_reference_frame('a52f3c0f-9993-4be3-b108-132f46cd08ae', self_identification_as_woman).
narrative_ontology:cs_drift_state('a52f3c0f-9993-4be3-b108-132f46cd08ae', contemporary_legal_and_social_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a52f3c0f-9993-4be3-b108-132f46cd08ae', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, cisgender_women_seeking_sex_based_protections).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_biology_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and codify the definition of 'woman' based on gender identity, advocating for legal and social recognition of transgender women as women. They shape policy and public discourse.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from legal and social recognition as women, gaining access to spaces, services, and identity documents aligned with their gender identity. Their ability to live authentically is tied to this definition.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    organized, biographical, constrained, global).

% Bear costs through the perceived dilution or loss of sex-based protections in areas like sports, single-sex spaces (e.g., changing rooms, shelters), and data collection, where their biological sex is deemed irrelevant to their category membership. They face social and institutional pressure when advocating for sex-based rights.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, cisgender_women_seeking_sex_based_protections, payer,
    organized, biographical, constrained, global).

% Advocate for a definition of 'woman' based on biological sex. They are often excluded from policy-making processes and public discourse, facing accusations of transphobia, and their concerns about sex-based rights are frequently dismissed.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_biology_advocates, excluded,
    moderate, generational, constrained, global).

% Are responsible for enacting and enforcing laws and policies that define 'woman' and determine access to rights and services. They navigate competing claims and political pressures, often codifying the gender identity definition into law.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Are affected by definitions of sex and gender, but their specific biological variations are often overlooked or not fully accommodated by either the gender identity or strict sex-biology definitions. They observe the debate from a position of vulnerability.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, intersex_individuals, observer,
    powerless, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent framework for legal and social recognition of gender identity, aiming to ensure transgender women are included in the category 'woman' for rights and access, thereby reducing discrimination and promoting social cohesion around gender identity.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to spaces/resources, and definitional authority from a sex-based understanding of 'woman' to a gender-identity-based understanding. This reallocates rights and protections based on self-identification.
% ABSENT_VOICES: Those who advocate for sex-based rights and protections for cisgender women, particularly in contexts like sports or single-sex spaces, are often framed as discriminatory and excluded from policy-making conversations. Their concerns are frequently marginalized or dismissed.
% DISAPPEARANCE_RATIONALE: If this definition vanished overnight, legal and social frameworks would revert to sex-based definitions, significantly altering the rights and recognition of transgender women, and re-establishing sex-based protections. The social and legal landscape would undergo a profound reorganization.
% FOUNDING_PROBLEM: The historical exclusion and discrimination faced by transgender individuals, particularly transgender women, from social and legal recognition as their affirmed gender, leading to systemic marginalization and denial of rights.
% FOUNDING_PROBLEM_CORROBORATION: Transgender rights organizations, human rights bodies, and many legal scholars corroborate the ongoing problem of discrimination and the need for gender identity recognition. Independent sociological studies also document the challenges faced by transgender individuals.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_definition_coherence,
    'Is the definition of ''woman'' based solely on gender identity internally coherent and universally applicable across all social and legal contexts without contradiction?',
    'Analysis of legal challenges and policy outcomes in diverse contexts (e.g., sports, prisons, healthcare) where the definition is applied. Contradictions or unresolvable conflicts would indicate incoherence.',
    'If incoherent, the constraint''s legitimacy and stability would be undermined, potentially leading to its reclassification as a Snare due to its reliance on active suppression of contradictory evidence or claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_definition_coherence, conceptual, 'Coherence of gender identity definition across contexts.').

omega_variable(
    impact_on_sex_based_protections,
    'Does the gender identity definition of ''woman'' genuinely enhance overall inclusion and safety, or does it dilute sex-based protections for cisgender women, particularly in vulnerable contexts?',
    'Empirical studies on safety outcomes in single-sex spaces, fairness in women''s sports, and the efficacy of data collection for sex-disaggregated analysis, comparing outcomes before and after implementation of the gender identity definition.',
    'If empirical evidence shows significant dilution of sex-based protections without commensurate overall safety/inclusion gains, the constraint''s extractiveness would be re-evaluated upward, strengthening its Snare-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_sex_based_protections, empirical, 'Trade-off between gender identity inclusion and sex-based protections.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative definitions of ''woman'' primarily structural (legal mandates, institutional policies) or internalized (social pressure, fear of being labeled bigoted)?',
    'Post-policy-change analysis: if resistance to alternative definitions persists even after legal mandates are relaxed, it suggests a significant internalized component. Surveys of public and professional opinion on self-censorship.',
    'If internalized suppression is dominant, the constraint''s effective suppression is higher than the structural measure suggests, making exit from the dominant narrative more difficult and amplifying its extractive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative definitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2010, woman_category__gender_identity_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(woma_tr_t2013, woman_category__gender_identity_reading, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(woma_tr_t2016, woman_category__gender_identity_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(woma_tr_t2019, woman_category__gender_identity_reading, theater_ratio, 2019, 0.15).
narrative_ontology:measurement(woma_tr_t2022, woman_category__gender_identity_reading, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(woma_tr_t2025, woman_category__gender_identity_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(woma_be_t2010, woman_category__gender_identity_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(woma_be_t2013, woman_category__gender_identity_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement(woma_be_t2016, woman_category__gender_identity_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(woma_be_t2019, woman_category__gender_identity_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(woma_be_t2022, woman_category__gender_identity_reading, base_extractiveness, 2022, 0.73).
narrative_ontology:measurement(woma_be_t2025, woman_category__gender_identity_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2010, woman_category__gender_identity_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(woma_su_t2013, woman_category__gender_identity_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement(woma_su_t2016, woman_category__gender_identity_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(woma_su_t2019, woman_category__gender_identity_reading, suppression_requirement, 2019, 0.65).
narrative_ontology:measurement(woma_su_t2022, woman_category__gender_identity_reading, suppression_requirement, 2022, 0.68).
narrative_ontology:measurement(woma_su_t2025, woman_category__gender_identity_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, women_sports_eligibility).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, single_sex_spaces_access).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'woman_category' kernel. Sibling readings include 'sex_biology_reading' and 'intersex_accommodation_reading', which offer alternative definitions of 'woman'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
