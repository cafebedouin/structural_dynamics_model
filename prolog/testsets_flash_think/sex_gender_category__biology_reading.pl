% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Biological Sex as Primary Category for Gender/Sex Classification
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint represents the 'biology_reading' of the contested
 *   'sex_gender_category' kernel. It asserts that membership in sex/gender
 *   categories (e.g., 'woman', 'man') is determined solely by immutable
 *   reproductive biology (chromosomes, anatomy at birth). This reading
 *   actively excludes trans women from the 'woman' category and defines cis
 *   women as the sole victim set for sex-based harms, leading to high
 *   boundary enforcement costs and the forced binary classification of
 *   intersex individuals. The constraint is claimed as a Tangled Rope,
 *   reflecting its purported coordination function (protecting cis women)
 *   alongside its asymmetric extraction and active enforcement against those
 *   who do not fit the biological definition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.75).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.8).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Biological Sex as Primary Category for Gender/Sex Classification").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '5aa53c6d-ba05-43c3-88b6-c5d98064ec92').
narrative_ontology:cs_kernel_codification('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', formalized).
narrative_ontology:cs_authority_grounding('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', lineage).
narrative_ontology:cs_interpretation_layer_present('5aa53c6d-ba05-43c3-88b6-c5d98064ec92').
narrative_ontology:cs_reading_relation('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', foundational, sex_is_binary_and_immutable_at_birth).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', sex_is_binary_and_immutable_at_birth, empirically_contingent).
narrative_ontology:cs_axiom('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', foundational, sex_is_the_basis_for_women_s_rights).
narrative_ontology:cs_axiom_status(sex_is_the_basis_for_women_s_rights, holdable).
narrative_ontology:cs_axiom_grounding('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', sex_is_the_basis_for_women_s_rights, deontological).
narrative_ontology:cs_reference_frame('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', binary_reproductive_sex_framework).
narrative_ontology:cs_drift_state('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', contemporary_identity_politics_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5aa53c6d-ba05-43c3-88b6-c5d98064ec92', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, gender_critical_advocates).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_men).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, cis_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and enforce this biological definition of sex, viewing it as foundational for women's rights, safety, and the integrity of sex-segregated spaces. They invest significant political and social capital in maintaining these boundaries.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_critical_advocates, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from sex-based protections and categories defined by this reading, which are intended to address harms specific to biological females. Some may also experience social pressure to conform to rigid gender roles or bear the costs of internal policing of these categories.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, cis_women, payer).

% Are explicitly excluded from the 'woman' category under this reading, leading to denial of access to women's spaces, services, and legal recognition. They face significant discrimination and invalidation of their identity.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, trapped, global).

% Are often forced into a binary male/female classification that may not align with their biological reality or identity, frequently leading to non-consensual medical interventions and social marginalization.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, global).

% Are denied recognition of their male identity based on their birth sex, impacting legal documents, social acceptance, and access to male-specific resources or spaces.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_men, payer,
    powerless, biographical, identity_locked, global).

% Are often tasked with codifying and enforcing sex classifications in law (e.g., birth certificates, sports categories), balancing historical precedents with evolving social understandings. They are a primary site of contestation and enforcement.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% Historically and currently define and diagnose sex based on biological markers. Their practices can reinforce binary classifications and influence legal and social recognition, sometimes against the wishes or realities of intersex individuals.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_professionals, agenda_setter,
    institutional, biographical, constrained, global).

% Monitor and challenge the exclusionary aspects of this biological reading, advocating for broader, more inclusive definitions of gender and sex that respect individual autonomy and human dignity.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish clear, immutable categories for human reproduction, social roles, and legal rights based on observable biological differences, primarily to protect cis women from male-pattern violence and discrimination, and to ensure fairness in sex-segregated activities.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to sex-segregated spaces/resources, and political power to those who fit the biological definition of their sex, from those who do not (e.g., trans women, intersex individuals).
% ABSENT_VOICES: Transgender individuals (especially trans women), intersex individuals, and their allies are often excluded from the foundational discussions that establish these categories, or their perspectives are dismissed as irrelevant or threatening to the established order.
% DISAPPEARANCE_RATIONALE: If biological sex as the primary category for gender/sex classification vanished overnight, legal systems, social norms, and identity frameworks would undergo a profound reorganization. Sex-segregated spaces, sports, and legal protections would need redefinition, leading to significant social and political upheaval as new frameworks are negotiated.
% FOUNDING_PROBLEM: To establish clear, immutable categories for human reproduction, social roles, and legal rights based on observable biological differences, particularly to protect women from male-pattern violence and discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Gender-critical advocates and some conservative legal scholars attest the problem is live and essential for women's rights. Transgender advocates, intersex advocates, and many human rights organizations attest that while some aspects of sex-based protection remain relevant, the rigid, exclusionary application of this reading creates more harm than it solves; independent sociological and medical research also challenges the strict binary and immutability claims.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) due to the denial of recognition, rights, and access to resources for trans and intersex individuals. Suppression is also high (0.80) as this reading requires active social, legal, and institutional enforcement to maintain its boundaries against competing claims and lived realities. The theater ratio is moderate (0.40); while there is genuine belief in the biological basis, a significant portion of the enforcement activity is performative, aimed at defending the category's exclusivity rather than solely serving its stated protective function. Accessibility collapse is high for those excluded, as this framework offers no alternative path to category membership. Resistance is substantial (0.70) from affected communities and human rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries (cis women, gender-critical advocates), this constraint is a necessary coordination mechanism for protecting sex-based rights and ensuring safety. From the perspective of victims (trans women, intersex individuals, trans men), it is a deeply extractive and suppressive mechanism that denies their identity and rights. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women and gender-critical advocates are structural beneficiaries, gaining clarity in sex-based protections and ideological coherence. Trans women, intersex individuals, and trans men are targets, bearing the costs of exclusion, misgendering, and forced medicalization. Legal systems and medical professionals act as agenda-setters, codifying and enforcing these classifications. Human rights advocates serve as observers, challenging the constraint's exclusionary impacts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_vs_social_construction,
    'Is the ''sex'' category, as defined by this reading, a purely biological fact or a socially constructed classification system built upon biological differences?',
    'Analysis of historical and cross-cultural variations in sex classification, and the role of social norms and power structures in defining and enforcing categories beyond mere biological observation.',
    'If primarily a social construct, the constraint''s ''emerges_naturally'' claim is further undermined, strengthening its classification as a constructed, actively enforced constraint rather than a natural limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_vs_social_construction, conceptual, 'Ambiguity between biological fact and social construction in sex classification.').

omega_variable(
    intersex_binary_enforcement_impact,
    'What are the full social, psychological, and medical costs imposed on intersex individuals by the enforcement of a strict biological sex binary?',
    'Longitudinal studies of intersex individuals'' experiences with medical interventions, legal classifications, and social acceptance, compared to outcomes in contexts with more fluid or non-binary recognition.',
    'Higher documented costs would increase the measured extractiveness and suppression, particularly for the ''intersex_individuals'' victim group, potentially shifting the constraint towards a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_binary_enforcement_impact, empirical, 'Costs of binary enforcement on intersex individuals.').

omega_variable(
    protection_vs_exclusion_necessity,
    'Is the exclusion of trans women from the ''woman'' category, as defined by this reading, a necessary condition for the protection of cis women''s rights and safety, or does it primarily constitute an unnecessary harm?',
    'Empirical studies on the impact of inclusive vs. exclusive policies on women''s safety and rights in various contexts, alongside ethical analysis of competing claims to identity and protection.',
    'If exclusion is found to be unnecessary or counterproductive to cis women''s safety, the ''coordination_function'' aspect of the constraint would be significantly weakened, pushing it closer to a pure Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_vs_exclusion_necessity, preference, 'Whether exclusion is necessary for protection or primarily harmful.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t2000, sex_gender_category__biology_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(sex__tr_t2005, sex_gender_category__biology_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(sex__tr_t2010, sex_gender_category__biology_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(sex__tr_t2015, sex_gender_category__biology_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(sex__tr_t2020, sex_gender_category__biology_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(sex__tr_t2025, sex_gender_category__biology_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(sex__be_t2000, sex_gender_category__biology_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(sex__be_t2005, sex_gender_category__biology_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(sex__be_t2010, sex_gender_category__biology_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(sex__be_t2015, sex_gender_category__biology_reading, base_extractiveness, 2015, 0.71).
narrative_ontology:measurement(sex__be_t2020, sex_gender_category__biology_reading, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement(sex__be_t2025, sex_gender_category__biology_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t2000, sex_gender_category__biology_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(sex__su_t2005, sex_gender_category__biology_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(sex__su_t2010, sex_gender_category__biology_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(sex__su_t2015, sex_gender_category__biology_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(sex__su_t2020, sex_gender_category__biology_reading, suppression_requirement, 2020, 0.79).
narrative_ontology:measurement(sex__su_t2025, sex_gender_category__biology_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'biology_reading' of the 'sex_gender_category' kernel, which also includes 'identity_reading' and 'hybrid_reading' as sibling constraints. Each reading instantiates a distinct constraint with its own ε value and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
