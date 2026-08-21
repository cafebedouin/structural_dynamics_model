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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Muslim Marriage (Nikah) under Sharia Law
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes marriage (Nikah) as a civil contract within
 *   Islamic tradition, governed by Quranic injunctions and Hadith, as
 *   interpreted and enforced by religious scholars and courts. It is one
 *   reading of the broader 'family_law_authority' kernel. Key structural
 *   features include the mahr (dower) obligation, historical
 *   gender-asymmetric divorce access (e.g., triple talaq, largely banned or
 *   reformed in many jurisdictions post-2019), and the permissibility of
 *   polygyny for men in some interpretations. The constraint is claimed as a
 *   'tangled_rope' due to its genuine coordination function for family
 *   structure alongside asymmetric extraction, particularly from female
 *   spouses.
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
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Marriage (Nikah) under Sharia Law").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'fb1912db-2fc5-4526-8636-f12503282265').
narrative_ontology:cs_kernel_codification('fb1912db-2fc5-4526-8636-f12503282265', fixed_text).
narrative_ontology:cs_authority_grounding('fb1912db-2fc5-4526-8636-f12503282265', lineage).
narrative_ontology:cs_interpretation_layer_present('fb1912db-2fc5-4526-8636-f12503282265').
narrative_ontology:cs_reading_relation('fb1912db-2fc5-4526-8636-f12503282265', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb1912db-2fc5-4526-8636-f12503282265', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb1912db-2fc5-4526-8636-f12503282265', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb1912db-2fc5-4526-8636-f12503282265', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('fb1912db-2fc5-4526-8636-f12503282265', foundational, marriage_as_civil_contract_divinely_sanctioned).
narrative_ontology:cs_axiom_status(marriage_as_civil_contract_divinely_sanctioned, holdable).
narrative_ontology:cs_axiom_grounding('fb1912db-2fc5-4526-8636-f12503282265', marriage_as_civil_contract_divinely_sanctioned, theological).
narrative_ontology:cs_axiom('fb1912db-2fc5-4526-8636-f12503282265', foundational, male_guardianship_and_polygyny_permissible).
narrative_ontology:cs_axiom_status(male_guardianship_and_polygyny_permissible, holdable).
narrative_ontology:cs_axiom_grounding('fb1912db-2fc5-4526-8636-f12503282265', male_guardianship_and_polygyny_permissible, theological).
narrative_ontology:cs_reference_frame('fb1912db-2fc5-4526-8636-f12503282265', classical_islamic_jurisprudence).
narrative_ontology:cs_drift_state('fb1912db-2fc5-4526-8636-f12503282265', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fb1912db-2fc5-4526-8636-f12503282265', '').
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

% Benefit from the right to polygyny (in some interpretations) and historically easier access to divorce (talaq). Bear the obligation of mahr (dower) and financial support. Their exit options are constrained by social norms and legal frameworks, but generally more favorable than for female spouses.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_spouses, beneficiary,
    moderate, biographical, constrained, local).

% Historically faced gender-asymmetric divorce access (e.g., triple talaq before bans), limited rights to initiate divorce (khula often requires male consent or judicial process), and social pressure to maintain marriage. Receive mahr, but its value may diminish over time. Their identity is often deeply tied to marital status within the community, making exit difficult.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, female_spouses, payer,
    powerless, biographical, identity_locked, local).

% Interpret and apply Quranic injunctions and hadith, adjudicating marriage and divorce cases. They maintain the legitimacy of the system and benefit from their authoritative role. Their power is constrained by state law in many jurisdictions, but they retain significant social and religious authority.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_scholars_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Bear the social and economic consequences of marital dissolution, particularly in contexts where support and custody arrangements are inconsistent or favor one parent. They have no agency in the constraint's operation or dissolution.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, children_of_divorce, payer,
    powerless, generational, trapped, local).

% In many countries, secular legal systems coexist with or partially integrate Sharia family law. They observe, regulate, and sometimes override aspects of religious law, particularly concerning gender equality and child welfare. Their role is to ensure compliance with broader constitutional principles.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for legitimate marital unions, defining rights and obligations for spouses, ensuring lineage, and providing a social structure for family formation within Islamic communities.
% TRANSFER_FUNCTION: Transfers social status, economic obligations (mahr, maintenance), and rights (divorce, polygyny) between spouses, primarily from male to female (mahr) and from female to male (easier divorce access historically).
% ABSENT_VOICES: Feminist legal scholars advocating for gender-egalitarian interpretations of Islamic law, and individuals seeking purely secular marriage contracts, are often marginalized or excluded from traditional religious legal discourse. They would argue for reforms to ensure equal rights and protections.
% DISAPPEARANCE_RATIONALE: If the framework of Nikah and its associated Sharia injunctions vanished, the social and legal structure of marriage for millions of Muslims globally would collapse. Family formation, inheritance, and personal status would require entirely new legal and social frameworks, leading to widespread societal reorganization.
% FOUNDING_PROBLEM: To provide a divinely sanctioned and socially ordered framework for family life, sexual relations, and the upbringing of children, ensuring stability and moral conduct within the early Muslim community.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and community leaders universally attest that the founding problem of establishing a moral and stable family unit remains live and central to Islamic society. Secular legal scholars and human rights advocates, while acknowledging the historical context, often contest the contemporary application of certain aspects, particularly regarding gender equality, but do not deny the original intent of social ordering.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) stems from the historical and ongoing gender asymmetries in rights and obligations, particularly concerning divorce and spousal authority, which often favor male spouses. Suppression (0.70) is high due to the combination of religious authority, social pressure, and legal frameworks that limit alternatives or make exit difficult for female spouses, often reinforced by identity-lock mechanisms. The theater ratio (0.20) is relatively low, indicating that the system is largely functional in its stated purpose of regulating marriage, though some aspects may be performative in maintaining traditional power structures. The temporal measurements show relative stability, reflecting the enduring nature of religious legal systems, with minor fluctuations due to ongoing reforms and contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male spouses and religious authorities, the system provides a divinely ordained and stable framework for family life, with costs seen as necessary for social order. From the perspective of female spouses, the same system can be experienced as highly extractive and suppressive, limiting their autonomy and exit options. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Male spouses are beneficiaries due to their historically greater rights and flexibility within the marital contract. Female spouses are payers, bearing the costs of gender asymmetry and constrained exit options. Religious scholars and courts act as agenda-setters, interpreting and enforcing the rules, benefiting from their institutional authority. Children of divorce are payers, experiencing the downstream consequences of the system's operation without agency. Secular legal systems act as observers, influencing the constraint's application through state law and human rights frameworks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equality_reforms_impact,
    'To what extent have contemporary legal reforms (e.g., bans on triple talaq, enhanced khula rights) genuinely reduced extractiveness and suppression for female spouses, versus merely shifting the mechanisms of control?',
    'Empirical studies on divorce rates, post-divorce economic status of women, and access to judicial remedies in jurisdictions with reforms, compared to those without.',
    'If reforms show significant reduction in extraction and suppression, the constraint''s classification for female spouses would shift towards a more balanced ''rope'' or ''scaffold'' (if transitional). If control mechanisms have merely shifted, the ''tangled_rope'' or ''snare'' classification would persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equality_reforms_impact, empirical, 'Assessing the real-world impact of legal reforms on gender equality within Sharia family law.').

omega_variable(
    identity_lock_vs_structural_suppression,
    'What proportion of female spouses'' constrained exit options is due to internalized identity-lock (social stigma, religious belief) versus structural barriers (lack of economic independence, legal hurdles)?',
    'Qualitative sociological research and comparative legal analysis across different socio-economic contexts, examining post-exit trajectories and perceived barriers.',
    'If identity-lock is a dominant factor, the effective suppression is higher and more persistent, as it travels with the individual. If structural barriers are primary, legal and economic reforms would have a more direct and immediate impact on exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_suppression, empirical, 'Distinguishing internalized vs. structural components of suppression for female spouses.').

omega_variable(
    secular_vs_religious_authority_primacy,
    'In jurisdictions with dual legal systems, which authority (secular state law or religious court) holds ultimate primacy in adjudicating family law, and how does this affect the constraint''s enforcement and extractiveness?',
    'Analysis of case law, constitutional provisions, and the practical enforcement of judgments from both systems, particularly in cases of conflict.',
    'If secular law holds primacy, the constraint''s extractiveness and suppression may be mitigated by state-mandated protections. If religious authority is primary or operates with significant autonomy, the constraint''s original structural properties are more likely to persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_vs_religious_authority_primacy, conceptual, 'Clarifying the hierarchy of legal authority in mixed-jurisdiction contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__muslim_shariat_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__muslim_shariat_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__muslim_shariat_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__muslim_shariat_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__muslim_shariat_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(fami_be_t10, family_law_authority__muslim_shariat_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fami_be_t20, family_law_authority__muslim_shariat_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(fami_be_t30, family_law_authority__muslim_shariat_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(fami_be_t40, family_law_authority__muslim_shariat_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(fami_be_t50, family_law_authority__muslim_shariat_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fami_su_t10, family_law_authority__muslim_shariat_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fami_su_t20, family_law_authority__muslim_shariat_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fami_su_t30, family_law_authority__muslim_shariat_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(fami_su_t40, family_law_authority__muslim_shariat_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(fami_su_t50, family_law_authority__muslim_shariat_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, inheritance_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, child_custody_law_authority__muslim_shariat_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
