% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Gender Category Membership by Subjective Gender Identity (Self-ID)
 *   domain: social/legal/identity
 *
 * SUMMARY:
 *   This constraint instantiates the identity reading of the contested
 *   sex_gender_category kernel: legal and social category membership (woman,
 *   man) is determined by subjective gender identity (self-identification)
 *   rather than immutable reproductive biology or medical gatekeeping. The
 *   reading includes trans women in the 'woman' category, expands the
 *   recognized victim set of misogyny to include trans women, and strips cis
 *   women of exclusive claims to sex-based protections, generating high
 *   conflict over space access. Sibling readings are biology_reading
 *   (immutable biology determines membership) and hybrid_reading (medical
 *   gatekeeping plus social transition).
 *
 * KEY AGENTS:
 *   - Transgender women: Primary beneficiaries â gain categorical recognition and protections (moderate power, constrained exit).
 *   - Cis women: Primary payers â lose exclusive sex-based protections and face space-access conflict (organized, constrained exit).
 *   - Single-sex service providers: Secondary payers â must restructure admission criteria or face sanction (moderate power, constrained exit).
 *   - State legal apparatus: Agenda-setter â codifies and enforces self-ID frameworks (institutional, analytical exit).
 *   - LGBTQ advocacy organizations: Beneficiaries â collect institutional legitimacy and funding (organized, mobile exit).
 *   - Gender-critical feminists: Excluded â framework treats their sex-based analysis as discriminatory (organized, constrained exit).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.55).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.7).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Gender Category Membership by Subjective Gender Identity (Self-ID)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social/legal/identity").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '9d0177f6-28fa-4fe6-8088-569058017137').
narrative_ontology:cs_kernel_codification('9d0177f6-28fa-4fe6-8088-569058017137', formalized).
narrative_ontology:cs_authority_grounding('9d0177f6-28fa-4fe6-8088-569058017137', lineage).
narrative_ontology:cs_interpretation_layer_present('9d0177f6-28fa-4fe6-8088-569058017137').
narrative_ontology:cs_reading_relation('9d0177f6-28fa-4fe6-8088-569058017137', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('9d0177f6-28fa-4fe6-8088-569058017137', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('9d0177f6-28fa-4fe6-8088-569058017137', foundational, trans_women_are_women).
narrative_ontology:cs_axiom_status(trans_women_are_women, holdable).
narrative_ontology:cs_axiom_grounding('9d0177f6-28fa-4fe6-8088-569058017137', trans_women_are_women, deontological).
narrative_ontology:cs_axiom('9d0177f6-28fa-4fe6-8088-569058017137', foundational, self_identification_requires_no_external_validation).
narrative_ontology:cs_axiom_status(self_identification_requires_no_external_validation, holdable).
narrative_ontology:cs_axiom_grounding('9d0177f6-28fa-4fe6-8088-569058017137', self_identification_requires_no_external_validation, deontological).
narrative_ontology:cs_axiom('9d0177f6-28fa-4fe6-8088-569058017137', secondary, medical_gatekeeping_causes_bureaucratic_violence).
narrative_ontology:cs_axiom_status(medical_gatekeeping_causes_bureaucratic_violence, holdable).
narrative_ontology:cs_axiom_grounding('9d0177f6-28fa-4fe6-8088-569058017137', medical_gatekeeping_causes_bureaucratic_violence, instrumental).
narrative_ontology:cs_reference_frame('9d0177f6-28fa-4fe6-8088-569058017137', subjective_identity_framework).
narrative_ontology:cs_drift_state('9d0177f6-28fa-4fe6-8088-569058017137', contemporary_backlash_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9d0177f6-28fa-4fe6-8088-569058017137', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, lgbtq_advocacy_orgs).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, single_sex_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal and social recognition as women through self-declaration, accessing sex-based protections, spaces, and bureaucratic categories previously reserved for cis women. Their safety and recognition depend on the constraint's enforcement against biology-based exclusion.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, transgender_women, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from legal gender recognition systems that rely solely on self-identification rather than medical or surgical gatekeeping, reducing bureaucratic violence and enabling access to gendered services and documentation.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Organizations that campaigned for self-ID frameworks; gain institutional standing, funding streams, and policy influence when legal systems adopt identity-based classification. They do not administer the constraint but collect legitimacy and resources from its operation.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, lgbtq_advocacy_orgs, beneficiary,
    organized, generational, mobile, global).

% Lose exclusive categorical claim to sex-based protections and single-sex spaces (shelters, prisons, sports, shortlists). Experience conflict over space access and the legal delegitimization of biology-based boundary claims; cannot exit the sex classification system.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women, payer,
    organized, biographical, constrained, national).

% Women's refuges, prisons, sports governing bodies, and healthcare providers that must redesign admission criteria around self-declared identity rather than biology; face legal jeopardy, funding loss, and social sanction for maintaining sex-based boundaries.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, single_sex_service_providers, payer,
    moderate, biographical, constrained, national).

% Legislatures, courts, and administrative agencies that codify and enforce self-ID gender recognition; mediate between identity-based and sex-based rights claims through anti-discrimination law and human rights frameworks.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for sex-based rights who argue that self-ID erases the material category of sex; their analytical framework is treated as discriminatory in policy spaces that have adopted the identity reading, effectively excluding them from legislative and institutional consultation.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, excluded,
    organized, biographical, constrained, national).

narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and legal recognition for transgender individuals by removing medical gatekeeping, allowing self-declaration to align legal sex category with lived gender identity, and reducing bureaucratic barriers to protection.
% TRANSFER_FUNCTION: Moves categorical membership, legal protections, and space-access entitlements from a biology-based criterion to a subjective identity-based criterion; transfers authority to define category boundaries from medical professionals and the state to the individual declarant.
% ABSENT_VOICES: Gender-critical feminists and biology-reading advocates are structurally excluded from policy formulation in jurisdictions that have adopted pure self-ID; their objections are classified as discriminatory rather than competing rights claims, removing them from the conversation.
% DISAPPEARANCE_RATIONALE: If legal gender categories based on self-ID vanished overnight, transgender individuals would lose current recognition and protections, single-sex services would revert to biological admission criteria, legal documents would require remapping, and advocacy organizations would pivot to defending the lost framework.
% FOUNDING_PROBLEM: The medical gatekeeping model for gender recognition was experienced as demeaning, pathologizing, and exclusionary by transgender individuals; legal sex categories that did not match lived identity produced bureaucratic violence, denial of services, and lack of legal protection.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by international human rights bodies (e.g., Amnesty International, UN treaty bodies) and medical ethicists from outside the directly benefiting advocacy sector; contested by criminologists, feminist legal scholars, and some clinicians who argue the self-ID solution has outrun the founding problem and now undermines sex-based rights.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.55) is moderate because the constraint redistributes categorical membership and space access asymmetrically: trans women gain inclusion while cis women lose exclusivity, but the transfer is legal/social rather than directly material. Suppression (0.70) is high because the framework requires active enforcement â legal penalties for misgendering or exclusion, social norms that delegitimize biology-based classification, and institutional no-platforming of dissent. Theater ratio (0.40) reflects significant performative activity (allyship signaling, institutional declarations) alongside genuine legal reclassification. Accessibility collapse (0.60) is substantial because once self-ID is codified, biology-based alternatives become legally and socially inaccessible in those jurisdictions. Resistance (0.75) is very high due to organized gender-critical movements, legal challenges, and political backlash. The measurement grid shares time points so temporal analysis samples every metric at the same moments.
 *
 * PERSPECTIVAL GAP:
 *   The transgender beneficiary seat experiences the constraint as recognition, harm reduction, and liberation from medical gatekeeping; the cis women payer seat experiences the identical legal structure as the dissolution of a material category that organized their safety and political rights. The state agenda-setter seat experiences a manageable administrative update that nonetheless generates outsized political conflict. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women and the broader transgender beneficiary set sit near the low-d (subsidy) end: the constraint actively subsidizes their recognition and access. LGBTQ advocacy organizations also sit near the beneficiary end, collecting institutional legitimacy. Cis women and single-sex service providers sit near the high-d (target) end: the constraint extracts their prior categorical exclusivity and imposes restructuring costs. The state legal apparatus sits near symmetric (administering redistribution without bearing concentrated costs or gains). Gender-critical feminists are excluded from the conversation rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â medical gatekeeping as demeaning and exclusionary â is genuinely live for transgender claimants. However, the mandate has expanded from reducing bureaucratic harm to full categorical inclusion with zero external validation, creating asymmetric costs (lost single-sex spaces, delegitimized biology-based analysis) that prevent the constraint from reading as pure coordination (rope). The mandatrophy is not that the founding problem is dead, but that the solution's scope has outrun the problem, layering extraction onto coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'How does the identity_reading''s classification change if the biology_reading or hybrid_reading is adopted as the operative framework?',
    'Comparative jurisdiction analysis: measure the structural variables (beneficiary/victim sets, extraction, suppression) in jurisdictions operating under each reading to see whether the identity reading genuinely produces a unique constraint profile or merely shifts victim identities.',
    'If the victim set simply inverts between readings rather than changing shape, the kernel is a zero-sum redistribution contest; if the identity reading uniquely reduces total extraction, it functions as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committee-frame omega documenting this constraint as one reading of a contested kernel with siblings biology_reading and hybrid_reading.').

omega_variable(
    space_access_conflict_empirics,
    'Does self-ID-based category assignment in practice produce measurable increases in harm or privacy loss in single-sex spaces, or is the conflict primarily theoretical and symbolic?',
    'Empirical longitudinal studies of prisons, shelters, and sporting categories in self-ID jurisdictions, measuring incident rates, privacy complaints, and retention outcomes against biology-based jurisdictions.',
    'If harm increases are measurable and concentrated on cis women, the extraction profile is higher than symbolic; if no measurable increase, the conflict is performative and theater_ratio should be adjusted upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(space_access_conflict_empirics, empirical, 'Whether space-access conflict under self-ID translates into material harms or remains symbolic.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of biology-based and gender-critical dissent structural (legal penalty, employment loss, no-platforming) or internalized (self-censorship due to social stigma and fear of ostracization)?',
    'Post-exit suppression trajectory: survey gender-critical feminists and clinicians who have left institutional roles or jurisdictions with self-ID frameworks; if they resume public dissent after exit, suppression was primarily structural; if silence persists, it was partially internalized.',
    'If internalized, effective suppression exceeds the structural metric and the constraint operates partly through cognitive capture; if purely structural, resistance is higher than measured because the structural metric captures only external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of dissent against identity-based classification.').

omega_variable(
    founding_problem_scope_creep,
    'Has the self-ID arrangement expanded beyond the founding problem of demeaning medical gatekeeping into a broader categorical redefinition that no longer tracks the original harm?',
    'Historical comparison of original activist demands (streamlined legal gender recognition) against current institutional requirements (full categorical inclusion in all sex-based provisions, elimination of biological language) in the same jurisdictions.',
    'If the current arrangement substantially outruns the original problem, mandatrophy is present and the constraint risks piton-like inertia; if the scope matches the original problem, the coordination function remains primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_scope_creep, conceptual, 'Whether the constraint''s mandate has expanded beyond its founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgc_identity_tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sgc_identity_tr_t4, sex_gender_category__identity_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(sgc_identity_tr_t8, sex_gender_category__identity_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(sgc_identity_tr_t12, sex_gender_category__identity_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(sgc_identity_tr_t16, sex_gender_category__identity_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(sgc_identity_tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(sgc_identity_be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(sgc_identity_be_t4, sex_gender_category__identity_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(sgc_identity_be_t8, sex_gender_category__identity_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(sgc_identity_be_t12, sex_gender_category__identity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(sgc_identity_be_t16, sex_gender_category__identity_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(sgc_identity_be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(sgc_identity_su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sgc_identity_su_t4, sex_gender_category__identity_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(sgc_identity_su_t8, sex_gender_category__identity_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(sgc_identity_su_t12, sex_gender_category__identity_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(sgc_identity_su_t16, sex_gender_category__identity_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(sgc_identity_su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'sex/gender category' conflates three structurally distinct constraints (identity, biology, hybrid readings) with different epsilon values, victim/beneficiary structures, and enforcement requirements. They form a constraint family linked by shared kernel but mutually exclusive operative rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
