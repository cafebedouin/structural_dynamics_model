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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Category Membership by Subjective Gender Identity (Identity Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint describes the social and legal framework where an
 *   individual's gender category membership, particularly for 'woman', is
 *   determined by their subjective gender identity (self-identification). It
 *   is a reading of the broader 'sex_gender_category' kernel, emphasizing
 *   inclusivity and recognition of transgender identities. While it aims to
 *   coordinate social recognition, it generates significant conflict and
 *   perceived extraction for groups advocating for sex-based categories.
 *
 * KEY AGENTS:
 *   - Trans women: Primary beneficiaries, gaining recognition and inclusion.
 *   - Gender identity advocates: Agenda-setters, actively promoting and defending this framework.
 *   - Cis women seeking sex-based protections: Primary payers, experiencing a perceived loss of exclusive access to sex-segregated spaces and protections.
 *   - Gender critical feminists: Payers/Excluded, actively resisting this framework and facing social pressure.
 *   - Legal systems & Social institutions: Agenda-setters, interpreting and implementing policies based on this definition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.65).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.55).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Category Membership by Subjective Gender Identity (Identity Reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, 'ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9').
narrative_ontology:cs_kernel_codification('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', formalized).
narrative_ontology:cs_authority_grounding('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', practice).
narrative_ontology:cs_interpretation_layer_present('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9').
narrative_ontology:cs_reading_relation('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', foundational, gender_identity_is_self_determined).
narrative_ontology:cs_axiom_status(gender_identity_is_self_determined, holdable).
narrative_ontology:cs_axiom_grounding('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', gender_identity_is_self_determined, deontological).
narrative_ontology:cs_axiom('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', foundational, trans_women_are_women).
narrative_ontology:cs_axiom_status(trans_women_are_women, holdable).
narrative_ontology:cs_axiom_grounding('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', trans_women_are_women, deontological).
narrative_ontology:cs_reference_frame('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', gender_identity_as_primary_category_marker).
narrative_ontology:cs_drift_state('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ecc5b51b-c644-41ee-8cb0-d0ae25bde5e9', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women_seeking_sex_based_protections).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, gender_critical_feminists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain social and legal recognition in their affirmed gender, which is crucial for their well-being and safety. They benefit from inclusion in categories like 'woman' but may still face discrimination.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, constrained, global).

% Actively promote and defend the principle of self-identification for gender category membership. They benefit from the expansion of gender identity rights and the adoption of this framework in law and policy.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, gender_identity_advocates, beneficiary).

% Experience a perceived loss of exclusive claim to sex-based protections and spaces (e.g., changing rooms, sports categories) as the 'woman' category expands to include trans women. They bear the cost of navigating these changed social and legal boundaries.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women_seeking_sex_based_protections, payer,
    organized, biographical, constrained, national).

% Actively resist the identity-based definition of 'woman' and advocate for sex-based categories, particularly for women's rights and protections. They face social and institutional pressure, often being excluded from mainstream discourse for their views.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, gender_critical_feminists, excluded).

% Are tasked with interpreting and implementing laws that define gender and sex, often balancing competing rights and definitions. They enforce the legal recognition of gender identity based on self-identification in many jurisdictions.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, legal_systems, agenda_setter,
    institutional, generational, analytical, national).

% Implement policies and practices based on the identity-based definition of gender, affecting access to spaces, services, and data collection. They navigate public opinion and legal mandates.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, social_institutions, agenda_setter,
    institutional, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, gender_identity_advocates).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a consistent framework for gender category membership based on self-identification, ensuring recognition and inclusion for transgender individuals across social and legal contexts.
% TRANSFER_FUNCTION: Transfers the authority to define 'woman' from a purely sex-based understanding to one inclusive of gender identity, expanding social and legal recognition for trans women, while shifting the burden of adaptation to cis women and institutions previously operating on sex-based definitions.
% ABSENT_VOICES: Those who prioritize sex-based rights and protections for cisgender women, arguing that an identity-based definition undermines these. They are often excluded from policy-making and public discourse on gender identity.
% DISAPPEARANCE_RATIONALE: If the principle of self-identification for gender category membership vanished, legal and social frameworks would revert to sex-based definitions, profoundly altering the rights and recognition of transgender individuals and sparking intense social and political reorganization around gender categories.
% FOUNDING_PROBLEM: The historical and ongoing exclusion, discrimination, and misgendering of transgender individuals, particularly trans women, within social and legal categories that rigidly define gender based on assigned sex at birth.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy organizations, human rights bodies, and numerous academic studies consistently corroborate the ongoing problem of discrimination and the need for legal and social recognition of gender identity. This is attested by international human rights frameworks and legislative efforts in many countries.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.65) is moderate-high, reflecting the significant perceived costs borne by cis women who feel their sex-based protections are diluted, and the high social conflict generated. Suppression (0.55) is moderate, as while legal enforcement of self-ID is increasing, social pressure and exclusion of dissenting views are also significant. Resistance (0.8) is high, indicating ongoing and vocal opposition from various groups. Theater ratio (0.15) is low, as the commitment to self-identification is genuinely held and enacted, not merely performative. The increasing trend in extractiveness and suppression reflects the intensifying nature of the 'culture war' around these definitions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of trans women and gender identity advocates, this constraint is a beneficial coordination mechanism that rectifies historical injustices and ensures dignity. From the perspective of cis women seeking sex-based protections and gender-critical feminists, it is an extractive mechanism that undermines their rights and safety by redefining fundamental categories. The engine's classification as a Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and gender identity advocates are clear beneficiaries, as the constraint directly serves their interests in recognition and inclusion. Cis women seeking sex-based protections and gender-critical feminists are targets/payers, as they bear the costs of category redefinition and face social/institutional pressure for dissent. Legal systems and social institutions act as agenda-setters, enforcing the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Rope (ignoring the extraction from cis women) or a pure Snare (ignoring the genuine coordination function of recognition for trans women). It acknowledges both the legitimate coordination problem of ensuring transgender inclusion and the asymmetric extraction experienced by other groups through the redefinition of categories. The high resistance and contested founding problem status further highlight the ongoing tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_boundary_ambiguity,
    'Is the ''woman'' category fundamentally defined by sex (biology) or by gender identity (self-identification)?',
    'Legal precedent from higher courts or international human rights bodies explicitly adjudicating the primary definitional basis for legal categories, or a broad societal consensus shift.',
    'If resolved towards a sex-based definition, this reading''s foundational axioms would be overridden, leading to a reclassification towards a Snare for its continued enforcement. If resolved firmly towards identity, the resistance and extractiveness for opposing groups might diminish over time, moving it closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_boundary_ambiguity, conceptual, 'The core conceptual ambiguity regarding the definition of gender categories.').

omega_variable(
    impact_on_sex_based_protections,
    'To what extent does the inclusion of trans women in ''woman'' categories dilute or enhance sex-based protections for cisgender women?',
    'Empirical studies on the efficacy of single-sex spaces, sports, and data collection in jurisdictions that have adopted self-ID, compared to those that maintain sex-based definitions.',
    'If empirical evidence strongly shows dilution of protections, the extractiveness for cis women would be further amplified, potentially pushing the constraint closer to a Snare. If protections are maintained or enhanced, the extractiveness would be damped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_sex_based_protections, empirical, 'Empirical impact of identity-based categories on sex-based protections.').

omega_variable(
    social_cost_of_dissent,
    'What is the actual social and professional cost borne by individuals who publicly dissent from the identity-based definition of gender?',
    'Sociological studies, surveys, and legal analyses of cases involving individuals who have faced professional or social repercussions for expressing gender-critical views.',
    'If the costs are found to be severe and widespread, the suppression metric would be higher, reinforcing the Tangled Rope or pushing it towards a Snare. If dissent is largely tolerated, suppression would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_of_dissent, empirical, 'Measurement of social and professional costs for dissenting from identity-based gender definitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t2000, sex_gender_category__identity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(sex__tr_t2006, sex_gender_category__identity_reading, theater_ratio, 2006, 0.12).
narrative_ontology:measurement(sex__tr_t2012, sex_gender_category__identity_reading, theater_ratio, 2012, 0.13).
narrative_ontology:measurement(sex__tr_t2018, sex_gender_category__identity_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(sex__tr_t2024, sex_gender_category__identity_reading, theater_ratio, 2024, 0.15).
narrative_ontology:measurement(sex__tr_t2030, sex_gender_category__identity_reading, theater_ratio, 2030, 0.16).

% Extraction over time
narrative_ontology:measurement(sex__be_t2000, sex_gender_category__identity_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(sex__be_t2006, sex_gender_category__identity_reading, base_extractiveness, 2006, 0.52).
narrative_ontology:measurement(sex__be_t2012, sex_gender_category__identity_reading, base_extractiveness, 2012, 0.58).
narrative_ontology:measurement(sex__be_t2018, sex_gender_category__identity_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(sex__be_t2024, sex_gender_category__identity_reading, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement(sex__be_t2030, sex_gender_category__identity_reading, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t2000, sex_gender_category__identity_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(sex__su_t2006, sex_gender_category__identity_reading, suppression_requirement, 2006, 0.42).
narrative_ontology:measurement(sex__su_t2012, sex_gender_category__identity_reading, suppression_requirement, 2012, 0.48).
narrative_ontology:measurement(sex__su_t2018, sex_gender_category__identity_reading, suppression_requirement, 2018, 0.53).
narrative_ontology:measurement(sex__su_t2024, sex_gender_category__identity_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement(sex__su_t2030, sex_gender_category__identity_reading, suppression_requirement, 2030, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'sex_gender_category' kernel. Each reading presents a different structural claim about category membership, leading to different ε values and classifications. This 'identity_reading' focuses on subjective gender identity as the primary determinant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
