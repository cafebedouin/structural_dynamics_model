% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Muslim Shariat Reading of Marriage as Civil Contract
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint models the 'Muslim Shariat Reading' of marriage as a
 *   civil contract (nikah) governed by Quranic injunctions and Hadith, as
 *   applied in various jurisdictions. It is one reading of the broader
 *   'family_law_authority' kernel. Key structural deltas for this reading
 *   include contractual dissolution (talaq), permission for polygyny, the
 *   mahr (dower) obligation, and historically, gender-asymmetric divorce
 *   access. While presented as a coordination mechanism for family life, its
 *   operation often involves significant extraction, particularly from female
 *   spouses, due to interpretive traditions and enforcement mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.65).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.75).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Shariat Reading of Marriage as Civil Contract").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'c88f45cc-6c83-499e-9364-921ff2e4128a').
narrative_ontology:cs_kernel_codification('c88f45cc-6c83-499e-9364-921ff2e4128a', fixed_text).
narrative_ontology:cs_authority_grounding('c88f45cc-6c83-499e-9364-921ff2e4128a', lineage).
narrative_ontology:cs_interpretation_layer_present('c88f45cc-6c83-499e-9364-921ff2e4128a').
narrative_ontology:cs_reading_relation('c88f45cc-6c83-499e-9364-921ff2e4128a', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('c88f45cc-6c83-499e-9364-921ff2e4128a', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c88f45cc-6c83-499e-9364-921ff2e4128a', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('c88f45cc-6c83-499e-9364-921ff2e4128a', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('c88f45cc-6c83-499e-9364-921ff2e4128a', foundational, marriage_as_civil_contract_under_sharia).
narrative_ontology:cs_axiom_status(marriage_as_civil_contract_under_sharia, holdable).
narrative_ontology:cs_axiom_grounding('c88f45cc-6c83-499e-9364-921ff2e4128a', marriage_as_civil_contract_under_sharia, conventional).
narrative_ontology:cs_axiom('c88f45cc-6c83-499e-9364-921ff2e4128a', foundational, divine_revelation_as_primary_source_of_law).
narrative_ontology:cs_axiom_status(divine_revelation_as_primary_source_of_law, holdable).
narrative_ontology:cs_axiom_grounding('c88f45cc-6c83-499e-9364-921ff2e4128a', divine_revelation_as_primary_source_of_law, theological).
narrative_ontology:cs_reference_frame('c88f45cc-6c83-499e-9364-921ff2e4128a', classical_islamic_jurisprudence).
narrative_ontology:cs_drift_state('c88f45cc-6c83-499e-9364-921ff2e4128a', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c88f45cc-6c83-499e-9364-921ff2e4128a', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_spouses).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, religious_scholars_and_courts).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, female_spouses).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, children_of_divorce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from easier divorce access (historically, unilateral talaq), permission for polygyny, and the mahr (dower) obligation from the wife's family. They are the primary agents of contractual dissolution.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_spouses, beneficiary,
    powerful, biographical, mobile, local).

% Historically faced gender-asymmetric divorce access, requiring judicial intervention (khula) or husband's consent. They are subject to polygyny and often bear the social and economic costs of divorce more heavily. Identity is often tied to marital status and community norms.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, female_spouses, payer,
    powerless, biographical, identity_locked, local).

% Interpret and apply Quranic injunctions and Hadith to marriage and divorce cases. They administer the legal framework, issue fatwas, and preside over family courts, deriving authority from religious texts and tradition. They benefit from the perpetuation of their interpretive authority.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_scholars_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Bear the social, emotional, and economic consequences of marital dissolution, often with limited agency in custody or financial arrangements. Their long-term well-being is directly impacted by the constraint's operation.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, children_of_divorce, payer,
    powerless, generational, trapped, local).

% In many Muslim-majority countries, secular legal systems coexist with or incorporate elements of Sharia family law. They observe and sometimes intervene to harmonize religious and civil codes, particularly regarding women's rights and child welfare.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% Advocate for reforms to Sharia-based family laws to ensure gender equality in marriage, divorce, and inheritance. They challenge interpretations that disadvantage women and children, but often face resistance from religious establishments and conservative social norms.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_s_rights_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for legitimate marital unions, defining rights and obligations of spouses, and providing a structured process for dissolution, thereby coordinating social and familial relations within an Islamic legal and ethical context.
% TRANSFER_FUNCTION: Transfers authority over marital dissolution and family structure to male spouses and religious courts, while transferring social and economic burdens, particularly in divorce, to female spouses and children. It also transfers legitimacy from religious texts to the legal framework.
% ABSENT_VOICES: Women's rights advocates and secular legal reformers, who would argue for interpretations and legal frameworks that ensure full gender equality and protect vulnerable parties, are often marginalized or excluded from the primary interpretive and adjudicative processes.
% DISAPPEARANCE_RATIONALE: If this reading of marriage law vanished, the social and legal fabric of many Muslim-majority societies would undergo profound reorganization. Marital legitimacy, inheritance, and family structures would be thrown into disarray, necessitating new legal and social conventions.
% FOUNDING_PROBLEM: To provide a divinely sanctioned and socially ordered framework for family life, ensuring legitimate procreation, mutual rights and obligations, and a mechanism for resolving marital disputes and dissolution within an Islamic ethical system.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and conservative communities attest that the founding problem of maintaining an Islamic social order for family life remains live. Secular legal scholars and women's rights advocates, while acknowledging the historical problem, argue that the current interpretation of the solution has created new problems of gender inequality and injustice, making the status 'contested' from their perspective.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial due to the gender-asymmetric rights and obligations, particularly in divorce and polygyny, which historically favored male spouses. Suppression (0.75) is high, maintained by religious and social norms, community pressure, and the authority of religious courts, which limit exit options for female spouses. Theater ratio (0.20) is relatively low, as the religious and legal functions are genuinely performed, though the 'coordination' narrative often masks the extractive elements. The slight decrease in extractiveness and suppression over time reflects ongoing legal reforms in some jurisdictions (e.g., bans on triple talaq, increased judicial oversight for women's divorce).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male spouses and religious authorities, this system provides a stable, divinely sanctioned framework for family life. From the perspective of female spouses and women's rights advocates, it is an extractive system that perpetuates gender inequality under the guise of religious law. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Male spouses are primary beneficiaries (lower d) due to their greater rights in dissolution and polygyny. Female spouses are primary targets (higher d) due to limited exit options and greater burdens. Religious scholars and courts are agenda-setters, benefiting from their interpretive authority. Children are victims, bearing the costs of dissolution. Secular legal systems are observers, while women's rights advocates are excluded voices.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_flexibility_vs_rigidity,
    'To what extent is the ''Muslim Shariat Reading'' of marriage flexible to modern interpretations that promote gender equality, versus rigidly bound by historical interpretations?',
    'Analysis of judicial precedents and legislative reforms in various Muslim-majority countries over time, specifically tracking changes in women''s divorce rights, child custody, and polygyny regulations.',
    'If highly flexible, the constraint could evolve towards a Rope or Scaffold, reducing extraction. If rigid, it remains a Tangled Rope or Snare, requiring external pressure for change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_vs_rigidity, empirical, 'The degree to which Sharia family law can adapt to contemporary gender equality norms.').

omega_variable(
    cultural_vs_theological_grounding,
    'Is the gender asymmetry in this reading primarily a theological imperative, or a reflection of historical cultural practices that have been integrated into religious law?',
    'Comparative theological scholarship examining diverse Islamic legal schools and historical contexts, alongside sociological studies of contemporary practice versus textual injunctions.',
    'If primarily cultural, reforms might be easier to implement without challenging core theological tenets. If theological, resistance to reform would be stronger, potentially leading to deeper societal rifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_vs_theological_grounding, conceptual, 'Distinguishing between theological and cultural roots of gender asymmetry in Sharia family law.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression on female spouses structural (legal barriers, economic dependency) or internalized (social stigma, religious identity fusion)?',
    'Post-divorce trajectory studies for women who exit the marriage: if suppression persists (e.g., social ostracization, economic hardship) even after legal dissolution, it indicates a strong internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making true ''exit'' more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for female spouses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__muslim_shariat_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__muslim_shariat_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__muslim_shariat_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(fami_tr_t2010, family_law_authority__muslim_shariat_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__muslim_shariat_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__muslim_shariat_reading, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement(fami_be_t1950, family_law_authority__muslim_shariat_reading, base_extractiveness, 1950, 0.72).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__muslim_shariat_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(fami_be_t2010, family_law_authority__muslim_shariat_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__muslim_shariat_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__muslim_shariat_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(fami_su_t1950, family_law_authority__muslim_shariat_reading, suppression_requirement, 1950, 0.82).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__muslim_shariat_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(fami_su_t2010, family_law_authority__muslim_shariat_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__muslim_shariat_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on the Muslim Shariat interpretation. Other readings (Hindu Dharmashastra, Christian Canonical, Parsi Zoroastrian, Secular Contractual) are distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
