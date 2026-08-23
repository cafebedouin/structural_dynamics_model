% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Woman Category: Biological Sex Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the sex_biology_reading of the contested
 *   woman_category kernel. It defines 'woman' as an adult human female with
 *   XX chromosomes and typical female reproductive anatomy, and is codified
 *   in law, sports eligibility rules, and medical data systems. The reading
 *   produces a high-extraction constraint on transgender women (categorical
 *   exclusion from sex-segregated protections) and intersex people (forced
 *   binary categorization), while also subjecting cisgender women to
 *   biological verification. Sibling readings include gender_identity_reading
 *   and intersex_accommodation_reading.
 *
 * KEY AGENTS:
 *   - sex_policy_administrators: Agenda-setter (institutional/analytical) â codifies and enforces the biological definition.
 *   - cisgender_women: Dual-positioned beneficiary/payer (organized/constrained) â gain access to sex-segregated protections but bear verification costs.
 *   - transgender_women: Primary target (powerless/identity_locked) â excluded from legal recognition and sex-segregated spaces.
 *   - intersex_people: Target (powerless/trapped) â forced into binary categories or left in legal ambiguity.
 *   - gender_identity_advocates: Excluded voice (organized/constrained) â argues for self-identification criteria but is absent from codification processes.
 *   - human_rights_monitor: Observer (institutional/analytical) â documents exclusion and categorization harms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.78).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.8).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Woman Category: Biological Sex Reading").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'ea694e0e-9c60-4136-a9fe-a168e5aed894').
narrative_ontology:cs_kernel_codification('ea694e0e-9c60-4136-a9fe-a168e5aed894', fixed_text).
narrative_ontology:cs_authority_grounding('ea694e0e-9c60-4136-a9fe-a168e5aed894', expertise).
narrative_ontology:cs_interpretation_layer_present('ea694e0e-9c60-4136-a9fe-a168e5aed894').
narrative_ontology:cs_reading_relation('ea694e0e-9c60-4136-a9fe-a168e5aed894', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('ea694e0e-9c60-4136-a9fe-a168e5aed894', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('ea694e0e-9c60-4136-a9fe-a168e5aed894', foundational, reproductive_anatomy_defines_womanhood).
narrative_ontology:cs_axiom_status(reproductive_anatomy_defines_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('ea694e0e-9c60-4136-a9fe-a168e5aed894', reproductive_anatomy_defines_womanhood, empirically_contingent).
narrative_ontology:cs_axiom('ea694e0e-9c60-4136-a9fe-a168e5aed894', foundational, binary_sex_sufficient_for_law).
narrative_ontology:cs_axiom_status(binary_sex_sufficient_for_law, holdable).
narrative_ontology:cs_axiom_grounding('ea694e0e-9c60-4136-a9fe-a168e5aed894', binary_sex_sufficient_for_law, conventional).
narrative_ontology:cs_reference_frame('ea694e0e-9c60-4136-a9fe-a168e5aed894', binary_biological_typicality).
narrative_ontology:cs_drift_state('ea694e0e-9c60-4136-a9fe-a168e5aed894', contemporary_gender_policy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea694e0e-9c60-4136-a9fe-a168e5aed894', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_policy_administrators).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, cisgender_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce statutes, medical guidelines, and sports eligibility rules that codify 'female' as typical XX chromosomes and female reproductive anatomy. They gain administrative clarity, enforcement authority, and a bright-line tool for sex-segregated governance.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_policy_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Receive access to sex-segregated shelters, sports categories, and data protections under the biological definition, but bear the costs of biological verification, sex testing, and the reduction of their legal status to reproductive anatomy.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, cisgender_women, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, cisgender_women, payer).

% Are categorically excluded from legal recognition as women and from sex-segregated protections and spaces on the basis of chromosomal and anatomical criteria. Their gender identity is locked out of the category definition, and they cannot alter their biology to satisfy the rule.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, identity_locked, national).

% Have sex characteristics that do not fit typical male or female definitions. They are forced into binary categories through medical or legal coercion, subjected to normalizing interventions, or left in ambiguous legal limbo under the 'typical case' framing.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people, payer,
    powerless, biographical, trapped, national).

% Argue for self-identification and gender-identity-based legal categories. They are structurally excluded from legislative and medical policy-making bodies that codify biological definitions, and their alternative framework is suppressed from the policy conversation.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Documents the exclusion of transgender women and the harms to intersex people resulting from binary biological categorization in law, medicine, and sports. Produces reports that contest the naturalness and necessity of the strict biological definition.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, human_rights_monitor, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line biological criterion for sorting individuals into sex categories to administer sex-segregated protections, sports competitions, medical research protocols, and demographic data collection.
% TRANSFER_FUNCTION: Moves legal recognition and access to sex-segregated spaces and services away from individuals whose chromosomes or anatomy do not match the typical female profile; moves the burden of biological verification onto all individuals categorized under the rule.
% ABSENT_VOICES: Transgender women and gender identity advocates who would argue for self-identification criteria are excluded from the policy rooms where biological definitions are codified. Intersex advocacy groups noting the non-binary spectrum of sex characteristics are likewise marginalized from medical standard-setting.
% DISAPPEARANCE_RATIONALE: If the biological definition vanished overnight, sex-segregated services, sports categories, and data systems would lose their current sorting key. Administrators would need to adopt alternative criteria (such as gender identity or individualized assessment) or desegregate entirely. The current pattern of inclusion, exclusion, and verification would dissolve.
% FOUNDING_PROBLEM: The historical and ongoing need to administer sex-segregated spaces, services, and data in a way that is administratively simple, ostensibly objective, and aligned with observable biological traits.
% FOUNDING_PROBLEM_CORROBORATION: Feminist legal scholars and medical professionals attest to the historical need for sex-segregated protections. Human rights organizations, intersex advocates, and transgender rights movements attest that the binary biological framing is now contested and causes documented exclusion and harm. No neutral party unanimously corroborates the current strict framing.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint categorically denies recognition and access to transgender women and forces intersex people into binary categories, extracting legal personhood and spatial access. Suppression (0.80) is high because persistence depends on active enforcement: legal statutes, medical gatekeeping, sports testing panels, and data systems that reject non-conforming classification. Theater ratio (0.45) reflects that some enforcement is functional (medical data integrity) while an increasing share is performative (sex-testing rituals that rarely change outcomes but signal categorical exclusion). Accessibility collapse (0.70) is high because once the biological definition is institutionalized, gender-identity-based alternatives become administratively inaccessible. Resistance (0.75) is high due to sustained legal and social contestation by transgender rights movements, intersex advocates, and allied institutions. Temporal measurements show extraction and suppression rising together as the definition has become more contested and more actively policed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (policy administrators) experiences the constraint as necessary coordination for sex-segregated governance; the payer seats (transgender women, intersex people) experience it as enforced categorical exclusion. Cisgender women experience both sides simultaneously. The engine computes this divergence from the structural asymmetry in exit options and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Sex_policy_administrators sit near the beneficiary end (low d): they gain administrative clarity and authority from the bright-line rule. Cisgender_women are dual-positioned: they receive coordination benefits (access to segregated protections) but also bear extraction costs (verification, reduction to anatomy), placing them near the middle but slightly toward beneficiary due to their primary role assignment. Transgender_women sit near the full-target end (high d): they bear categorical exclusion with identity_locked exit options. Intersex_people also sit near the full-target end: their biology traps them in a binary they do not fit. Gender_identity_advocates are excluded from the constraint's operation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by preserving the genuine coordination function it serves: sex-segregated shelters, sports categories, and medical data collection do require some sorting mechanism. However, the asymmetric extraction â categorical exclusion of transgender women and forced binary assignment of intersex people â means it is not a pure Rope. The active enforcement requirement and presence of both beneficiaries and victims place it in Tangled Rope. If the coordination function were abandoned and only the exclusion remained, it would degrade toward Snare; if the extraction were removed and only a voluntary sorting tool remained, it would approach Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the sex_biology_reading of the woman_category kernel; how would the structural classification change if the gender_identity_reading or intersex_accommodation_reading were adopted instead?',
    'Comparative analysis of sibling constraint stories and their stakeholder directionalities within the same kernel family.',
    'Adopting a sibling reading would restructure the beneficiary and victim sets entirely and would likely produce a different constraint type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural position within the contested woman_category kernel.').

omega_variable(
    intersex_categorization_ambiguity,
    'How does the typical case framing handle intersex variations that do not fit binary male or female profiles â forced assignment, exclusion, or medical intervention?',
    'Empirical review of medical protocols and legal categorization outcomes for intersex individuals under this definition.',
    'If forced binary assignment is the dominant outcome, extraction from intersex people is higher than the base metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_categorization_ambiguity, empirical, 'Ambiguity in intersex categorization under binary biological definition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (laws, medical testing, sports eligibility panels) or internalized (self-policing of gender presentation to avoid scrutiny)?',
    'Post-exit suppression trajectory: if individuals continue to avoid spaces or modify behavior after legal barriers are removed, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    biological_definition_natural_kind,
    'Is the XX-chromosome and female-anatomy definition a discovered natural kind or a constructed legal instrument?',
    'Philosophical and biological review of whether typical female anatomy functions as a descriptive or normative criterion in practice.',
    'If constructed rather than discovered, the constraint''s legitimacy claim weakens and its extraction profile becomes more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_definition_natural_kind, conceptual, 'Natural kind status of biological sex definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(woma_tr_t6, woman_category__sex_biology_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(woma_tr_t12, woman_category__sex_biology_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(woma_tr_t18, woman_category__sex_biology_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(woma_tr_t24, woman_category__sex_biology_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(woma_tr_t30, woman_category__sex_biology_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(woma_be_t6, woman_category__sex_biology_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(woma_be_t12, woman_category__sex_biology_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(woma_be_t18, woman_category__sex_biology_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(woma_be_t24, woman_category__sex_biology_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(woma_be_t30, woman_category__sex_biology_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(woma_su_t6, woman_category__sex_biology_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(woma_su_t12, woman_category__sex_biology_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(woma_su_t18, woman_category__sex_biology_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(woma_su_t24, woman_category__sex_biology_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(woma_su_t30, woman_category__sex_biology_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
