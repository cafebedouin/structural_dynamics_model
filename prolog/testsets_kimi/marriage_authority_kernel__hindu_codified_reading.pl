% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Codified Marriage Authority (Hindu Marriage Act 1955 / Civil Courts)
 *   domain: comparative law / constitutional pluralism / religious governance
 *
 * SUMMARY:
 *   In post-colonial India, the Hindu Marriage Act 1955 codified and reformed
 *   the diverse customary and scriptural laws governing Hindu family
 *   relations, centralizing authority in state civil courts. This constraint
 *   story captures the hindu_codified_reading of the
 *   marriage_authority_kernel: the claim that for Indians classified as
 *   Hindu, marriage and family law authority derives from this statute as
 *   interpreted by the civil judiciary, not from religious custom, canonical
 *   texts, or secular individual rights. The reading coordinates the Hindu
 *   community under a uniform legal umbrellaâabolishing polygamy, providing
 *   judicial divorce, and establishing maintenance obligationsâwhile
 *   simultaneously extracting sub-community autonomy and retaining moderate
 *   gender asymmetries that fall short of secular constitutional standards.
 *   It is actively enforced by courts that refuse to recognize rival
 *   customary authorities on these questions.
 *
 * KEY AGENTS:
 *   - Civil judiciary (institutional/agenda_setter): gains monopoly jurisdiction over Hindu family disputes and develops binding precedent.
 *   - Union legislature (institutional/agenda_setter): enacted the codifying statute and retains amendment power; maintains the personal-law framework against UCC pressure.
 *   - Hindu reform constituency (organized/beneficiary): sought and gained statutory modernization and uniformity.
 *   - Hindu women statutory beneficiaries (moderate/beneficiary): gained partial rights relative to pre-codified custom but remain below secular equity.
 *   - Hindu customary autonomy groups (moderate/payer): lost customary self-governance to the statutory template.
 *   - Hindu women residual inequity bearers (powerless/payer): bear remaining statutory and interpretive gender asymmetries with identity-locked exit.
 *   - Uniform civil code advocates (organized/excluded): argue for abolition of all personal laws in favor of secular individual rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.58).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Codified Marriage Authority (Hindu Marriage Act 1955 / Civil Courts)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative law / constitutional pluralism / religious governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, '05e4d69b-6c82-40dc-9465-b3dcab3c4c59').
narrative_ontology:cs_kernel_codification('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', formalized).
narrative_ontology:cs_authority_grounding('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', lineage).
narrative_ontology:cs_interpretation_layer_present('05e4d69b-6c82-40dc-9465-b3dcab3c4c59').
narrative_ontology:cs_reading_relation('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', marriage_authority_kernel__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', foundational, hindu_marriage_authority_derives_from_codified_statute).
narrative_ontology:cs_axiom_status(hindu_marriage_authority_derives_from_codified_statute, holdable).
narrative_ontology:cs_axiom_grounding('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', hindu_marriage_authority_derives_from_codified_statute, conventional).
narrative_ontology:cs_axiom('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', foundational, state_judiciary_supreme_interpreter_of_hindu_law).
narrative_ontology:cs_axiom_status(state_judiciary_supreme_interpreter_of_hindu_law, holdable).
narrative_ontology:cs_axiom_grounding('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', state_judiciary_supreme_interpreter_of_hindu_law, conventional).
narrative_ontology:cs_reference_frame('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', hindu_marriage_act_1955_framework).
narrative_ontology:cs_drift_state('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', contemporary_post_2005_amendment, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('05e4d69b-6c82-40dc-9465-b3dcab3c4c59', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_reform_constituency).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_women_statutory_beneficiaries).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_customary_autonomy_groups).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_residual_inequity_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and adjudicates the Hindu Marriage Act 1955, developing binding precedents on marriage validity, divorce, maintenance, and ancillary relief. Exercises state-monopoly jurisdiction over Hindu family disputes, displacing customary and religious authorities. Gains institutional authority, precedent-setting power, and case volume from this personal-law jurisdiction.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Enacted the Hindu Marriage Act 1955 and retains plenary authority to amend or repeal it. Defines statutory Hindu identity and the uniform legal template for Hindu marriage, divorce, and maintenance. Could theoretically replace the Act with a uniform civil code under Article 44 but has maintained the personal-law framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, union_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Progressive Hindu organizations and modernizing elites who campaigned for statutory abolition of child marriage, polygamy, and unilateral divorce. Benefit from codified legal certainty, enforceable monogamy, and the symbolic legitimation of a reformed Hindu family law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_reform_constituency, beneficiary,
    organized, generational, constrained, national).

% Hindu women who gained enforceable rights under the Act, including judicial divorce, maintenance, and monogamy for husbands. Their situation improved relative to pre-codified customary law but remains below the gender-equity standard of secular constitutional law and the Special Marriage Act.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_statutory_beneficiaries, beneficiary,
    moderate, biographical, constrained, national).

% Sub-communities, including tribal and lower-caste groups, whose distinct customary marriage, divorce, and inheritance practices were overridden by the Act's uniform statutory template. Forced to litigate in civil courts using rules that do not reflect their lived norms and social organization.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_customary_autonomy_groups, payer,
    moderate, generational, constrained, regional).

% Hindu women who bear remaining statutory and interpretive asymmetries, including limited matrimonial property claims, exposure to restitution-of-conjugal-rights decrees, and structural judicial biases. Their personal identity as Hindu women locks them into the personal-law system; individual exit via the Special Marriage Act is socially stigmatized and practically inaccessible for many.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_residual_inequity_bearers, payer,
    powerless, biographical, identity_locked, national).

% Secular reformers and constitutional lawyers who argue that all personal laws, including the Hindu codified framework, should be replaced by a gender-just uniform civil code grounded in individual rights rather than religious identity. Structurally excluded from the Hindu Marriage Act framework, but their litigation and public advocacy generate constitutional pressure on the system.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, uniform_civil_code_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform statutory framework for Hindu marriage, divorce, and maintenance, replacing diverse and often discriminatory customary practices with codified rules and a single state-backed adjudication forum in civil courts.
% TRANSFER_FUNCTION: Transfers interpretive and enforcement authority over Hindu family relations from customary and religious institutions to state civil courts; transfers partial gender-equity benefits to Hindu women while retaining certain patriarchal asymmetries; transfers sub-community autonomy to a majoritarian statutory template.
% ABSENT_VOICES: Uniform civil code advocates argue the personal-law system should be abolished in favor of secular individual rights. Customary religious authorities and caste panchayats were displaced by the Act and lack formal standing under it. Muslim, Christian, and Parsi personal-law communities operate in parallel but serve as comparative reference points in reform debates.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act and its court interpretation vanished, Hindu family relations would revert to uncodified customary law and a constitutional vacuum. Marriage validity, divorce grounds, and maintenance claims would become deeply uncertain. The civil judiciary would lose a major personal-law jurisdiction, reform beneficiaries would lose statutory protections, and customary groups would regain institutional autonomy.
% FOUNDING_PROBLEM: Pre-independence Hindu family law was fragmented across scriptural texts, regional customs, and caste practices, with widespread child marriage, polygamy, and the denial of divorce and property rights to women. The post-colonial state sought to modernize, unify, and reform Hindu family relations through legislative codification.
% FOUNDING_PROBLEM_CORROBORATION: The reform constituency and women's organizations attest the problem is partially live because gender inequities persist. Legal historians and subordinate customary groups attest the codification imposed a Brahminical-majoritarian template that did not reflect actual Hindu diversity. Parliamentary debates from the 1950s corroborate the modernization intent, while contemporary feminist legal scholars from outside the direct beneficiary set corroborate the incomplete and uneven nature of the reform.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate because the constraint delivers genuine coordination benefitsâuniformity, legal certainty, and partial gender reformâwhile still extracting autonomy from customary groups and retaining gender asymmetries. Suppression (0.58) reflects the active judicial and administrative suppression of rival customary authorities and the lack of viable intra-community alternatives to civil court adjudication. Theater ratio (0.30) captures the performative dimension of state courts applying a 'Hindu' template that often does not reflect the lived diversity of the community. Accessibility collapse (0.65) is substantial because once a litigant is classified as Hindu, the civil court framework becomes the only state-recognized forum. Resistance (0.45) is moderate, manifesting in UCC advocacy, feminist litigation, and sub-community non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the civil judiciary and reform constituency, the constraint appears as legitimate coordination: a modernizing legal framework that replaced chaotic and oppressive customs with rational rules. From customary autonomy groups and residual inequity bearers, it appears as state extraction: a majoritarian template that suppresses plural identities and preserves patriarchal elements under the guise of reform. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The civil judiciary and reform constituency are structural beneficiaries: they gain jurisdiction and policy victories, so their directionality sits near the beneficiary end (low d, damped effective extraction). Hindu women statutory beneficiaries also sit on the beneficiary side, though their constrained exit keeps d from the extreme. Customary autonomy groups and residual inequity bearers are structural victims: they lose autonomy and bear asymmetries with limited or identity-locked exit, placing their d near the target end (high effective extraction). The uniform civil code advocates are excluded, with no directional flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it has a clear, partially live founding problem (fragmented and discriminatory pre-independence Hindu custom) that is corroborated by historical record and parliamentary intent. However, the founding problem is contested: feminist scholars and customary groups argue the codification solved some problems while creating others. The constraint is not a piton because it has concentrated beneficiaries (courts, reform constituency) and concentrated victims (customary groups, inequity bearers), and it is not primarily theatrical. It is not a snare because the coordination functionâuniform dispute resolution and partial reformâis real and not merely cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint one legitimate reading among plural personal-law frameworks, or an interim step toward a uniform secular civil code?',
    'Comparative corpus analysis across the marriage_authority_kernel constraint family; constitutional adjudication on Articles 25 and 44; political determination by the Union Legislature.',
    'If the secular civil reading is adopted as uniform law, this constraint dissolves. If constitutional pluralism is irreducible, this reading remains a stable coordinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: this constraint is the hindu_codified_reading of a contested kernel.').

omega_variable(
    gender_equity_gap,
    'Does the remaining gender inequity in the Hindu Marriage Act reflect a transitional residue amenable to further amendment, or a structural feature of codified personal law?',
    'Legislative amendment history tracking compared against persistent judicial resistance to full gender parity in matrimonial property and conjugal rights.',
    'If transitional, further reform reduces extractiveness and moves the constraint toward rope. If structural, the constraint remains tangled rope with locked-in asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_gap, empirical, 'Whether gender inequity is transitional or structural in codified Hindu law.').

omega_variable(
    customary_autonomy_vs_modernization,
    'Is the override of subordinate Hindu customs an extractive imposition of a majoritarian statutory template, or a legitimate coordination function that protects vulnerable members of those communities?',
    'Empirical study of litigation outcomes for tribal and lower-caste groups under the Act versus their pre-codified customary dispute resolution.',
    'If the override primarily extracts autonomy without protective benefit, the victim set expands and the constraint leans toward snare. If protective, the coordination function is genuine and the classification holds as tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_autonomy_vs_modernization, empirical, 'Whether customary override is extractive or protective coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t14, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(marr_tr_t28, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 28, 0.21).
narrative_ontology:measurement(marr_tr_t42, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 42, 0.25).
narrative_ontology:measurement(marr_tr_t56, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 56, 0.28).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 70, 0.3).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t14, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 14, 0.33).
narrative_ontology:measurement(marr_be_t28, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 28, 0.36).
narrative_ontology:measurement(marr_be_t42, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 42, 0.38).
narrative_ontology:measurement(marr_be_t56, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 56, 0.4).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(marr_su_t14, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(marr_su_t28, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 28, 0.54).
narrative_ontology:measurement(marr_su_t42, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 42, 0.56).
narrative_ontology:measurement(marr_su_t56, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 56, 0.57).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the marriage_authority_kernel family. The natural-language label 'marriage/family law authority in India' conflates multiple structurally distinct readings (Hindu codified, Muslim Shariat, Christian canonical, Parsi communal, secular civil). Each reading carries its own epsilon, beneficiary/victim structure, and authority grounding, linked through the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
