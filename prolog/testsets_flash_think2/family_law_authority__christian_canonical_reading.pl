% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage (Sacramental/Denominational)
 *   domain: religious_governance/social_norms/family_law
 *
 * SUMMARY:
 *   This constraint describes Christian canonical marriage, viewed as a
 *   sacrament (Catholic) or a divinely ordained institution (Protestant),
 *   governed by ecclesiastical authority or denominational rules. It is one
 *   reading of the broader 'family_law_authority' kernel, which encompasses
 *   diverse religious and secular approaches to marriage. This reading
 *   emphasizes permanence, spiritual significance, and the authority of
 *   religious institutions over marital validity, often in tension with
 *   secular contractual views.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.65).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.75).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Marriage (Sacramental/Denominational)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious_governance/social_norms/family_law").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '556ee1a0-d415-443a-935b-cf3b0bea4149').
narrative_ontology:cs_kernel_codification('556ee1a0-d415-443a-935b-cf3b0bea4149', formalized).
narrative_ontology:cs_authority_grounding('556ee1a0-d415-443a-935b-cf3b0bea4149', lineage).
narrative_ontology:cs_interpretation_layer_present('556ee1a0-d415-443a-935b-cf3b0bea4149').
narrative_ontology:cs_reading_relation('556ee1a0-d415-443a-935b-cf3b0bea4149', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('556ee1a0-d415-443a-935b-cf3b0bea4149', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('556ee1a0-d415-443a-935b-cf3b0bea4149', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('556ee1a0-d415-443a-935b-cf3b0bea4149', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('556ee1a0-d415-443a-935b-cf3b0bea4149', foundational, marriage_as_sacrament_or_divine_covenant).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_or_divine_covenant, holdable).
narrative_ontology:cs_axiom_grounding('556ee1a0-d415-443a-935b-cf3b0bea4149', marriage_as_sacrament_or_divine_covenant, theological).
narrative_ontology:cs_axiom('556ee1a0-d415-443a-935b-cf3b0bea4149', foundational, ecclesiastical_authority_over_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_validity, holdable).
narrative_ontology:cs_axiom_grounding('556ee1a0-d415-443a-935b-cf3b0bea4149', ecclesiastical_authority_over_validity, conventional).
narrative_ontology:cs_reference_frame('556ee1a0-d415-443a-935b-cf3b0bea4149', divine_institution_of_marriage).
narrative_ontology:cs_drift_state('556ee1a0-d415-443a-935b-cf3b0bea4149', contemporary_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('556ee1a0-d415-443a-935b-cf3b0bea4149', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, adherents_seeking_stability).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, adherents_seeking_divorce_or_remarriage).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, individuals_seeking_secular_marriage_recognition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, adherents_seeking_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Catholic Church or various Protestant denominations that define, administer, and enforce the rules of Christian marriage. They derive authority, adherence, and often financial support from this role. They set doctrinal standards and adjudicate marital validity.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, religious_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals who seek to marry within the Christian canonical framework, benefiting from the spiritual significance, community recognition, and perceived stability it offers. Their choices are constrained by doctrinal requirements and institutional rules.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, adherents_seeking_marriage, beneficiary,
    moderate, biographical, constrained, local).

% Individuals who, having entered a Christian marriage, seek divorce or remarriage. They face significant doctrinal barriers (especially in Catholicism) and social stigma, often leading to exclusion from full participation in their religious community if they deviate from canonical rules. Their faith identity locks them into the system.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, adherents_seeking_divorce_or_remarriage, payer,
    powerless, biographical, identity_locked, local).

% Priests, ministers, and other religious leaders who officiate marriages, provide pastoral care, and enforce canonical rules at the local level. They are bound by the doctrines of their respective institutions but wield significant authority over individual adherents.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, clergy, agenda_setter,
    institutional, biographical, constrained, local).

% State legal frameworks that recognize (or do not recognize) religious marriages, and which provide civil alternatives for marriage, divorce, and family law. They observe the operation of religious marriage but operate on different foundational principles.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% Individuals who choose civil marriage outside of religious institutions, or whose marriages are not recognized by Christian canonical law (e.g., same-sex couples). They are excluded from the benefits and recognition of this specific religious constraint, though they have the option of state-sanctioned marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, individuals_seeking_secular_marriage_recognition, excluded,
    powerless, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, religious_institutions).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, divinely sanctioned framework for family formation, child-rearing, and community cohesion within Christian traditions, providing clear roles and expectations for adherents regarding marital fidelity and permanence.
% TRANSFER_FUNCTION: Transfers commitment, adherence to religious doctrine, and often financial contributions from adherents to religious institutions, in exchange for spiritual guidance, community support, and a religiously recognized status. It also transfers authority over marital validity from individuals to ecclesiastical bodies.
% ABSENT_VOICES: Those who advocate for greater individual autonomy in marital decisions, secular definitions of marriage, or more liberal divorce/remarriage policies within Christian faiths. They are often marginalized, face doctrinal censure, or leave the religious community.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the structure of Christian religious communities, the personal identities of millions of adherents, and aspects of family law (where religious law is recognized) would fundamentally shift. Marriage would become purely a civil or personal matter, stripping religious institutions of a core function and authority.
% FOUNDING_PROBLEM: To establish a divinely ordained, stable, and morally guided institution for procreation and family life, distinct from secular or pagan practices, ensuring spiritual purity and social order within early Christian communities.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutions and many devout adherents attest to the ongoing need for a divinely sanctioned marital framework to maintain spiritual purity and social order. Secular observers or former adherents might contest the *necessity* of ecclesiastical authority, but acknowledge the historical problem of establishing stable family structures.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderately high (0.65) due to the significant commitment, adherence to doctrine, and restrictions on personal autonomy (especially regarding divorce/remarriage) required of adherents. Suppression is high (0.75) for devout adherents, as religious doctrine and community pressure strongly discourage or forbid alternatives. Theater ratio is low (0.15) because the rituals and rules are considered functionally efficacious within the belief system, not merely performative. Accessibility collapse is high (0.8) for those committed to the religious framework, as alternatives are foreclosed by faith. Resistance is moderate (0.4), reflecting internal reform movements and external challenges from secular society.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious institutions, this constraint is a divinely ordained 'Rope' or 'Mountain' providing essential coordination for spiritual and social order. From the perspective of adherents seeking divorce or remarriage, it operates as a 'Snare' or 'Tangled Rope,' extracting adherence and autonomy through doctrinal enforcement and social pressure. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions are primary beneficiaries and agenda-setters, gaining authority, adherence, and resources. Adherents seeking marriage are beneficiaries of stability and spiritual meaning, but also payers of commitment and adherence. Adherents seeking divorce or remarriage are primary targets/victims, facing significant costs and identity-locked exit options. Secular legal systems act as observers, while individuals seeking secular marriage are excluded from this specific religious framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_ambiguity,
    'Is Christian marriage primarily a spiritual sacrament with immutable divine laws, or does it also function as a social contract subject to evolving human interpretation and societal norms?',
    'Analysis of doctrinal shifts over time, particularly regarding divorce and remarriage, and the degree to which ecclesiastical courts incorporate contemporary social science or legal principles in their adjudications.',
    'If primarily sacramental, the constraint''s extractiveness and suppression are inherent to its divine nature. If significantly contractual, these metrics reflect institutional choices that could be reformed, potentially reclassifying it closer to a Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_ambiguity, conceptual, 'Ambiguity between divine and human-made aspects of marriage.').

omega_variable(
    ecclesiastical_vs_state_authority,
    'Where does ultimate authority over marriage reside for adherents: with ecclesiastical bodies or with the secular state, especially when their laws conflict?',
    'Empirical study of adherent behavior when religious and state laws diverge (e.g., regarding divorce, same-sex marriage), and the legal recognition afforded to religious marriages by the state.',
    'If state authority is paramount, the religious constraint''s effective suppression is lower, as adherents have a viable exit to civil marriage. If ecclesiastical authority retains strong hold, suppression remains high for those identity-locked by faith.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecclesiastical_vs_state_authority, empirical, 'Conflict of authority between church and state over marriage.').

omega_variable(
    divorce_doctrine_variability,
    'To what extent can Christian doctrines on divorce and remarriage vary (e.g., Catholic indissolubility vs. Protestant allowances) while maintaining the core claim of marriage as a divinely ordained institution?',
    'Comparative theological and historical analysis of different Christian traditions, examining the theological justifications for varying divorce policies and their impact on adherents'' experiences.',
    'If significant variability is possible, the specific rules (e.g., Catholic no-divorce) represent a choice that amplifies extraction and suppression, rather than an immutable divine command. If variability is limited, the constraint''s core nature is more fixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divorce_doctrine_variability, conceptual, 'Variability of divorce doctrines within Christian marriage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1000, family_law_authority__christian_canonical_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(fami_tr_t1300, family_law_authority__christian_canonical_reading, theater_ratio, 1300, 0.12).
narrative_ontology:measurement(fami_tr_t1600, family_law_authority__christian_canonical_reading, theater_ratio, 1600, 0.13).
narrative_ontology:measurement(fami_tr_t1800, family_law_authority__christian_canonical_reading, theater_ratio, 1800, 0.14).
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__christian_canonical_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__christian_canonical_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fami_be_t1000, family_law_authority__christian_canonical_reading, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(fami_be_t1300, family_law_authority__christian_canonical_reading, base_extractiveness, 1300, 0.6).
narrative_ontology:measurement(fami_be_t1600, family_law_authority__christian_canonical_reading, base_extractiveness, 1600, 0.62).
narrative_ontology:measurement(fami_be_t1800, family_law_authority__christian_canonical_reading, base_extractiveness, 1800, 0.63).
narrative_ontology:measurement(fami_be_t1950, family_law_authority__christian_canonical_reading, base_extractiveness, 1950, 0.64).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__christian_canonical_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1000, family_law_authority__christian_canonical_reading, suppression_requirement, 1000, 0.8).
narrative_ontology:measurement(fami_su_t1300, family_law_authority__christian_canonical_reading, suppression_requirement, 1300, 0.82).
narrative_ontology:measurement(fami_su_t1600, family_law_authority__christian_canonical_reading, suppression_requirement, 1600, 0.78).
narrative_ontology:measurement(fami_su_t1800, family_law_authority__christian_canonical_reading, suppression_requirement, 1800, 0.76).
narrative_ontology:measurement(fami_su_t1950, family_law_authority__christian_canonical_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__christian_canonical_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, inheritance_law_christian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, child_rearing_norms_christian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, religious_community_membership_christian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the `family_law_authority` kernel, each defining marriage and family structure according to different foundational principles. It is linked to other constraints that govern family life within the Christian tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
