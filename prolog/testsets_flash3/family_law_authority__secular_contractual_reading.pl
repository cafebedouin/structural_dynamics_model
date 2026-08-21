% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Marriage as Civil Contract under State Law
 *   domain: comparative_law/political_theory
 *
 * SUMMARY:
 *   This constraint describes marriage as a civil contract between autonomous
 *   individuals, recognized and regulated solely by state law. It is a
 *   specific reading of the broader 'family_law_authority' kernel,
 *   emphasizing gender-symmetric rights, state registration as the sole
 *   validity criterion, and permission for interfaith marriages, distinct
 *   from religious or customary interpretations. The constraint aims to
 *   provide a universal, non-discriminatory legal framework for marriage
 *   within a secular state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.25).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.3).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Civil Contract under State Law").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '474bcd08-8c46-4668-b487-79f7afa02c36').
narrative_ontology:cs_kernel_codification('474bcd08-8c46-4668-b487-79f7afa02c36', formalized).
narrative_ontology:cs_authority_grounding('474bcd08-8c46-4668-b487-79f7afa02c36', lineage).
narrative_ontology:cs_interpretation_layer_present('474bcd08-8c46-4668-b487-79f7afa02c36').
narrative_ontology:cs_reading_relation('474bcd08-8c46-4668-b487-79f7afa02c36', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('474bcd08-8c46-4668-b487-79f7afa02c36', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('474bcd08-8c46-4668-b487-79f7afa02c36', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('474bcd08-8c46-4668-b487-79f7afa02c36', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('474bcd08-8c46-4668-b487-79f7afa02c36', foundational, state_sovereignty_over_personal_status).
narrative_ontology:cs_axiom_status(state_sovereignty_over_personal_status, holdable).
narrative_ontology:cs_axiom_grounding('474bcd08-8c46-4668-b487-79f7afa02c36', state_sovereignty_over_personal_status, conventional).
narrative_ontology:cs_axiom('474bcd08-8c46-4668-b487-79f7afa02c36', foundational, gender_equality_in_marital_rights).
narrative_ontology:cs_axiom_status(gender_equality_in_marital_rights, holdable).
narrative_ontology:cs_axiom_grounding('474bcd08-8c46-4668-b487-79f7afa02c36', gender_equality_in_marital_rights, deontological).
narrative_ontology:cs_reference_frame('474bcd08-8c46-4668-b487-79f7afa02c36', enlightenment_liberal_state).
narrative_ontology:cs_drift_state('474bcd08-8c46-4668-b487-79f7afa02c36', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('474bcd08-8c46-4668-b487-79f7afa02c36', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, marrying_individuals).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, children_of_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who choose to enter into a state-recognized marriage, gaining legal rights and responsibilities. They benefit from the clarity and enforceability of a secular contract, with freedom to define their relationship within legal bounds.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, marrying_individuals, beneficiary,
    moderate, biographical, mobile, national).

% The governmental authority that defines, registers, and enforces the terms of civil marriage. It benefits from a standardized framework for family units, facilitating administration of rights, property, and social services.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Religious bodies that may offer their own forms of marriage but whose ceremonies are not legally binding without state registration. They are excluded from the primary legal authority over marriage, though they may influence public opinion.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_institutions, excluded,
    organized, generational, constrained, national).

% Benefit from the legal protections and stability afforded by state-recognized parental relationships, including inheritance rights and support obligations. Their situation is entirely shaped by the legal framework.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, children_of_marriage, beneficiary,
    powerless, generational, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, legally enforceable framework for intimate relationships, ensuring clarity on rights, responsibilities, and dissolution, which facilitates social and economic planning for individuals and the state.
% TRANSFER_FUNCTION: Transfers legal rights, obligations, and social recognition from the state to marrying individuals, and in return, individuals accept state jurisdiction over their marital status.
% ABSENT_VOICES: Religious authorities who advocate for their own canonical definitions of marriage as the primary legal standard are excluded from setting the terms of civil marriage. They would argue for the primacy of religious law.
% DISAPPEARANCE_RATIONALE: If state-recognized civil marriage vanished, the legal and social landscape would be profoundly disrupted. Property rights, inheritance, parental responsibilities, and social benefits tied to marital status would become chaotic, forcing a complete reorganization of family law and social support structures.
% FOUNDING_PROBLEM: Historically, diverse religious and customary marriage practices led to legal ambiguities, unequal rights, and difficulties in state administration of family matters, particularly regarding property, inheritance, and the status of women.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and human rights organizations attest that the problem of ensuring equal rights and legal clarity in marriage remains live, particularly in contexts where religious or customary laws still conflict with secular principles. International human rights conventions also corroborate the need for state-backed, non-discriminatory marriage laws.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively low (0.25) as the state primarily provides a service of legal recognition and enforcement, with costs generally seen as administrative rather than extractive. Suppression (0.30) is moderate, reflecting the state's enforcement of its legal monopoly on marriage recognition, which suppresses alternative (e.g., purely religious) legal frameworks. Theater ratio is low (0.10) as the state's role is largely functional and administrative. The historical trend shows decreasing extractiveness and suppression as secular legal systems matured and became more inclusive, reducing the 'cost' of state recognition and the need to suppress religious alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marrying individuals, the constraint is a beneficial coordination mechanism providing legal certainty and rights. From the perspective of religious institutions, it may be seen as an imposition or an exclusion of their authority, even if they acknowledge its practical benefits. The state legal system views it as a necessary and efficient administrative framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Marrying individuals are beneficiaries, gaining legal rights and protections. The state legal system is an agenda-setter and beneficiary, establishing order and facilitating governance. There are no direct 'victims' in this secular contractual reading, as individuals are free to choose whether to enter this contract, and the state's role is primarily facilitative. Religious institutions are 'excluded' from legal authority, but not directly 'victimized' by the civil contract itself, as they can still perform religious ceremonies.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate remains live: to provide a universal, non-discriminatory legal framework for marriage. Its function has not atrophied; rather, it has evolved to become more inclusive (e.g., same-sex marriage). The classification as a 'rope' reflects its genuine coordination function and relatively low extraction, preventing mislabeling it as a 'snare' despite its enforcement of state authority over religious alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_vs_religious_primacy,
    'To what extent does the secular contractual reading of marriage genuinely coexist with, or subtly undermine, the authority of religious marriage frameworks in a pluralistic society?',
    'Empirical study of legal disputes involving interfaith marriages or conflicts between civil and religious divorce decrees; analysis of legislative efforts to reconcile or separate civil and religious family law.',
    'If the secular framework is found to consistently override or invalidate religious practices, its ''coexists_with'' relation to religious readings might be reclassified as ''influences'' or even ''forecloses'' in practice, indicating a stronger, more extractive assertion of state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_vs_religious_primacy, empirical, 'Ambiguity regarding the practical relationship between secular and religious marriage authorities.').

omega_variable(
    autonomy_vs_social_pressure,
    'Does the ''autonomous individual'' premise of the secular contract fully account for social and cultural pressures that may constrain individual choice in marriage, particularly for vulnerable populations?',
    'Sociological research on marriage patterns, divorce rates, and experiences of individuals from diverse cultural backgrounds within the secular legal framework; analysis of legal aid cases involving forced marriage or marital coercion.',
    'If significant social pressures are found to undermine individual autonomy, the ''mobile'' exit option for marrying individuals might be reclassified as ''constrained'' or ''identity_locked'' for certain groups, increasing their effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_social_pressure, empirical, 'The extent to which individual autonomy in marriage is truly unconstrained by social factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__secular_contractual_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__secular_contractual_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__secular_contractual_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__secular_contractual_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__secular_contractual_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(fami_be_t1950, family_law_authority__secular_contractual_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__secular_contractual_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__secular_contractual_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__secular_contractual_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(fami_su_t1950, family_law_authority__secular_contractual_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__secular_contractual_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__secular_contractual_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on the secular contractual interpretation. It is structurally distinct from religious interpretations of marriage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
