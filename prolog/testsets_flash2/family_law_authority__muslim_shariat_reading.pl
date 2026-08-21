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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint describes the Muslim Shariat reading of marriage as a
 *   civil contract (nikah), governed by Quranic injunctions and Hadith. It is
 *   characterized by contractual dissolution (talaq), permission for
 *   polygyny, the obligation of mahr (dower), and historically
 *   gender-asymmetric divorce access (e.g., pre-2019 triple talaq bans). This
 *   reading is one of several competing interpretations of family law
 *   authority, each instantiating a distinct constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.65).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.7).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Shariat Reading of Marriage as Civil Contract").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, '380bba7c-f37f-438c-bd36-285568d55306').
narrative_ontology:cs_kernel_codification('380bba7c-f37f-438c-bd36-285568d55306', fixed_text).
narrative_ontology:cs_authority_grounding('380bba7c-f37f-438c-bd36-285568d55306', lineage).
narrative_ontology:cs_interpretation_layer_present('380bba7c-f37f-438c-bd36-285568d55306').
narrative_ontology:cs_reading_relation('380bba7c-f37f-438c-bd36-285568d55306', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('380bba7c-f37f-438c-bd36-285568d55306', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('380bba7c-f37f-438c-bd36-285568d55306', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('380bba7c-f37f-438c-bd36-285568d55306', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('380bba7c-f37f-438c-bd36-285568d55306', foundational, marriage_as_civil_contract_under_divine_law).
narrative_ontology:cs_axiom_status(marriage_as_civil_contract_under_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('380bba7c-f37f-438c-bd36-285568d55306', marriage_as_civil_contract_under_divine_law, theological).
narrative_ontology:cs_axiom('380bba7c-f37f-438c-bd36-285568d55306', foundational, gender_differentiated_marital_rights).
narrative_ontology:cs_axiom_status(gender_differentiated_marital_rights, holdable).
narrative_ontology:cs_axiom_grounding('380bba7c-f37f-438c-bd36-285568d55306', gender_differentiated_marital_rights, conventional).
narrative_ontology:cs_reference_frame('380bba7c-f37f-438c-bd36-285568d55306', classical_shariat_family_law).
narrative_ontology:cs_drift_state('380bba7c-f37f-438c-bd36-285568d55306', contemporary_reforms_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('380bba7c-f37f-438c-bd36-285568d55306', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, husbands_in_polygynous_marriages).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_spouses_in_divorce).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, female_spouses_in_divorce).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_seeking_equal_marital_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Quranic injunctions and Hadith, adjudicating marital disputes, divorces (talaq), and dower (mahr) obligations. They maintain the traditional gender-asymmetric framework, often resisting reforms for equal rights.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_religious_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the permission of polygyny and historically easier access to divorce (talaq). Their marital and familial authority is structurally reinforced by this reading of the law.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, husbands_in_polygynous_marriages, beneficiary,
    moderate, biographical, mobile, local).

% Historically benefited from unilateral divorce (triple talaq, prior to bans in many jurisdictions) and generally face fewer legal hurdles in dissolving marriages compared to women.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_spouses_in_divorce, beneficiary,
    moderate, immediate, mobile, local).

% Historically faced significant barriers to initiating divorce (khula) and often had limited recourse against unilateral talaq. They bear the social and economic costs of gender-asymmetric dissolution processes.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, female_spouses_in_divorce, payer,
    powerless, immediate, constrained, local).

% Advocate for reforms to achieve gender equality in marriage and divorce, challenging traditional interpretations. They are often identity-locked by their religious and cultural affiliation, making exit from the system difficult.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_seeking_equal_marital_rights, payer,
    organized, generational, identity_locked, national).

% In many Muslim-majority countries, secular courts coexist with or oversee religious family law, sometimes introducing reforms (e.g., banning triple talaq) or offering alternative legal avenues. They observe and sometimes intervene in the application of Shariat law.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for marital unions, defining rights and obligations, inheritance, and dissolution procedures according to Islamic principles, providing social order and legitimacy for families within the Muslim community.
% TRANSFER_FUNCTION: Transfers marital authority and certain rights (e.g., polygyny, ease of divorce) to male spouses, while obligating them to provide mahr (dower) and maintenance. It transfers social and economic burdens of divorce disproportionately to female spouses.
% ABSENT_VOICES: Progressive Islamic scholars and women's rights advocates who argue for egalitarian interpretations of Islamic texts are often marginalized or excluded from traditional religious authority structures, despite their growing influence in public discourse.
% DISAPPEARANCE_RATIONALE: If this reading of marriage law vanished, the social and legal fabric of Muslim communities would undergo profound reorganization. Marital contracts, inheritance, and family structures would lose their traditional religious grounding, leading to a vacuum that would be filled by secular law or alternative religious interpretations, fundamentally altering societal norms.
% FOUNDING_PROBLEM: To provide a divinely sanctioned and comprehensive legal framework for family life, ensuring social stability, moral conduct, and the orderly transfer of property and lineage within the early Muslim community.
% FOUNDING_PROBLEM_CORROBORATION: Muslim religious authorities and many adherents attest that the founding problem of providing a divinely guided framework for family life remains live and essential for maintaining Islamic identity and social order. Secular legal scholars and women's rights groups, while acknowledging the historical context, argue that the specific interpretations have become problematic and require reform, but do not deny the original intent to establish order.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope due to its genuine coordination function (providing a framework for family life) coupled with significant asymmetric extraction, particularly from female spouses. Extractiveness (0.65) is driven by gender-asymmetric rights in divorce and polygyny. Suppression (0.70) is maintained by religious authority and social norms, limiting alternatives for those within the community. The theater ratio (0.20) reflects some performative aspects in maintaining traditional interpretations despite evolving social realities, but the core function remains active. The slight dip in extractiveness and theater ratio towards the end of the interval reflects reforms in some jurisdictions (e.g., triple talaq bans) and growing resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of traditional religious authorities and many male adherents, this constraint is a legitimate and divinely ordained framework for social order (a Rope or even a Mountain). From the perspective of female spouses and women's rights advocates, it operates as a substantially extractive Snare or Tangled Rope, perpetuating inequality. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Muslim religious authorities act as agenda-setters, benefiting from maintaining their interpretive authority. Male spouses, particularly in polygynous marriages or divorce proceedings, are beneficiaries due to the structural advantages afforded to them. Female spouses and women seeking equal rights are victims, bearing the costs of gender inequality and constrained exit options. Secular legal systems act as observers, sometimes intervening to moderate the constraint's extractive aspects.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide a stable family framework is still live, but its specific interpretations have led to significant extraction. The classification as Tangled Rope prevents mislabeling it as pure extraction (Snare), acknowledging its coordination function, while highlighting the asymmetric costs. The ongoing resistance and reforms indicate that the mandate is contested, not fully atrophied, but its implementation is under scrutiny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equality_interpretation_ambiguity,
    'Is the gender asymmetry inherent to Islamic family law, or is it a product of specific historical interpretations that can be reformed within an Islamic framework?',
    'Comparative analysis of diverse Islamic legal reforms and interpretations across different jurisdictions, particularly those that have successfully implemented more egalitarian family laws while remaining within Shariat principles.',
    'If reform is possible, the constraint''s extractiveness could be significantly reduced without abandoning its coordination function, potentially reclassifying it closer to a Rope. If asymmetry is deemed inherent, the Tangled Rope classification would be more robust, highlighting the fundamental tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_interpretation_ambiguity, conceptual, 'Ambiguity regarding the reformability of gender-asymmetric provisions within Islamic family law.').

omega_variable(
    secular_law_influence_magnitude,
    'To what extent do secular legal systems and international human rights norms genuinely mitigate the extractive aspects of this reading, versus merely coexisting with them?',
    'Empirical study of legal outcomes in mixed-jurisdiction countries, comparing divorce rates, property division, and women''s access to justice under Shariat courts versus secular courts, and the impact of specific legislative reforms.',
    'If secular influence is strong, the effective extractiveness for victims might be lower than the base measure suggests, as alternatives exist. If weak, the base extractiveness and suppression are more accurate, indicating limited real-world mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_law_influence_magnitude, empirical, 'The actual impact of secular legal frameworks on the operation of Shariat family law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__muslim_shariat_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__muslim_shariat_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__muslim_shariat_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__muslim_shariat_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__muslim_shariat_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fami_be_t10, family_law_authority__muslim_shariat_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(fami_be_t20, family_law_authority__muslim_shariat_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(fami_be_t30, family_law_authority__muslim_shariat_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fami_be_t40, family_law_authority__muslim_shariat_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(fami_be_t50, family_law_authority__muslim_shariat_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fami_su_t10, family_law_authority__muslim_shariat_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fami_su_t20, family_law_authority__muslim_shariat_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fami_su_t30, family_law_authority__muslim_shariat_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(fami_su_t40, family_law_authority__muslim_shariat_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(fami_su_t50, family_law_authority__muslim_shariat_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on the Muslim Shariat interpretation. It coexists with and influences other religious and secular readings of family law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
