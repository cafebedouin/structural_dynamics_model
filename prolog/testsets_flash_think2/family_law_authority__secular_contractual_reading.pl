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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Secular Marriage as Civil Contract under State Law
 *   domain: comparative_law/political_theory/social
 *
 * SUMMARY:
 *   This constraint describes marriage as a civil contract between autonomous
 *   individuals, regulated exclusively by state law, without religious
 *   requirements or gender-based restrictions. It is one reading of the
 *   broader 'family_law_authority' kernel, emphasizing secular principles and
 *   individual rights. The state's role is to provide a uniform, accessible
 *   legal framework for unions, ensuring clarity for property, inheritance,
 *   and parental rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.15).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.4).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Secular Marriage as Civil Contract under State Law").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/social").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '489de741-2483-4e02-bb86-9c894ef84c1d').
narrative_ontology:cs_kernel_codification('489de741-2483-4e02-bb86-9c894ef84c1d', formalized).
narrative_ontology:cs_authority_grounding('489de741-2483-4e02-bb86-9c894ef84c1d', lineage).
narrative_ontology:cs_interpretation_layer_present('489de741-2483-4e02-bb86-9c894ef84c1d').
narrative_ontology:cs_reading_relation('489de741-2483-4e02-bb86-9c894ef84c1d', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('489de741-2483-4e02-bb86-9c894ef84c1d', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('489de741-2483-4e02-bb86-9c894ef84c1d', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('489de741-2483-4e02-bb86-9c894ef84c1d', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('489de741-2483-4e02-bb86-9c894ef84c1d', foundational, individual_autonomy_in_union).
narrative_ontology:cs_axiom_status(individual_autonomy_in_union, holdable).
narrative_ontology:cs_axiom_grounding('489de741-2483-4e02-bb86-9c894ef84c1d', individual_autonomy_in_union, deontological).
narrative_ontology:cs_axiom('489de741-2483-4e02-bb86-9c894ef84c1d', foundational, state_as_sole_legal_arbiter).
narrative_ontology:cs_axiom_status(state_as_sole_legal_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('489de741-2483-4e02-bb86-9c894ef84c1d', state_as_sole_legal_arbiter, conventional).
narrative_ontology:cs_reference_frame('489de741-2483-4e02-bb86-9c894ef84c1d', enlightenment_liberal_contract).
narrative_ontology:cs_drift_state('489de741-2483-4e02-bb86-9c894ef84c1d', recognition_of_same_sex_marriage, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('489de741-2483-4e02-bb86-9c894ef84c1d', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, married_individuals).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, unmarried_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal recognition, inheritance rights, tax benefits, and social legitimacy provided by the state. They adhere to the contractual terms and legal processes for marriage and divorce. Exit is constrained by legal procedures and potential financial/social costs.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, married_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Defines, registers, and enforces the legal framework for marriage. Benefits from clear legal status for citizens, social stability, and administrative order. It is the ultimate authority for the civil contract.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_legal_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, state_legal_system, beneficiary).

% While not directly paying into the system, they bear the indirect costs of not having access to marriage-specific legal and social benefits. They are free to marry or not, but the legal framework creates a default set of advantages for married couples.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, unmarried_individuals, payer,
    moderate, biographical, mobile, national).

% Are excluded from defining the legal validity of marriage under state law, though they may perform religious ceremonies. Their authority is limited to the spiritual domain, not the civil. They would prefer their definitions to hold legal weight.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_institutions, excluded,
    organized, generational, constrained, national).

% Analyze the evolution, impact, and philosophical underpinnings of secular marriage law. They provide critical commentary and propose reforms, but do not directly administer or participate in the constraint.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, non-religious legal framework for intimate relationships, ensuring clear and equitable rights, responsibilities, and inheritance for individuals and their families, independent of religious affiliation.
% TRANSFER_FUNCTION: Transfers legal rights, responsibilities, and social recognition to married individuals, and transfers administrative burden and legal clarity to the state by providing a uniform system for unions.
% ABSENT_VOICES: Religious institutions that seek to define marriage exclusively by their tenets and assert legal authority over it; advocates for alternative relationship structures (e.g., polyamorous unions) who desire equivalent legal recognition.
% DISAPPEARANCE_RATIONALE: If the state's civil contract for marriage vanished overnight, the legal and social landscape would be profoundly disrupted. Inheritance, property rights, parental rights, and social welfare benefits would become ambiguous, requiring a complete overhaul of family law and social support systems. The stability of many households and the clarity of legal personhood would be undermined.
% FOUNDING_PROBLEM: To establish a uniform, non-religious basis for the legal recognition of unions, ensuring equal rights and responsibilities for all citizens regardless of faith or background, and to provide legal clarity for property, inheritance, and children within a secular state.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and secular advocacy groups corroborate the ongoing need for a non-discriminatory, state-regulated framework for unions, distinct from religious definitions, citing historical inequities and the need for universal application of rights.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Rope due to its primary function of solving a collective action problem (standardizing legal unions) with net benefits for participants (married individuals gain legal protections). Extractiveness is low (0.15) as the state primarily provides a service rather than extracting rents. Suppression is moderate (0.40) because the state maintains a monopoly on legal recognition of marriage, effectively suppressing alternative, non-state-sanctioned forms of legal union. Theater ratio is low (0.10) as the legal system is largely functional in its administration of marriage. Accessibility collapse is moderate (0.40) because while alternatives to marriage exist (e.g., civil unions, domestic partnerships), they may not offer full legal or social parity, and the state's definition of marriage remains dominant. Resistance is low (0.15) as this framework is widely accepted in secular societies, though some religious groups or advocates for broader relationship recognition may contest its scope.
 *
 * PERSPECTIVAL GAP:
 *   While the state views this framework as a neutral, equitable coordination mechanism, religious institutions may perceive it as an encroachment on their traditional authority. Advocates for broader relationship recognition may see it as a form of exclusion for non-traditional unions. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Married individuals are beneficiaries, gaining legal and social advantages. The state legal system is both an agenda-setter (defining the terms) and a beneficiary (gaining social stability and administrative clarity). Unmarried individuals are indirectly payers, as they do not access the benefits of marriage. Religious institutions are excluded from legal authority over marriage, making them a target of the state's exclusive claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_neutrality_vs_implicit_bias,
    'Is the state''s exclusive authority over legal marriage genuinely neutral, or does it implicitly privilege certain secular norms (e.g., monogamy, dyadic structure) over others, thereby creating a de facto bias?',
    'Comparative legal analysis of jurisdictions with more expansive definitions of legal unions (e.g., civil partnerships for multiple individuals) and their social outcomes.',
    'If an implicit bias is demonstrated, the constraint''s effective suppression and extractiveness for those outside the privileged norms would be higher than currently measured, potentially shifting its classification towards a Tangled Rope for those specific groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_neutrality_vs_implicit_bias, conceptual, 'Ambiguity regarding the neutrality of secular marriage law.').

omega_variable(
    contractual_vs_relational_essence,
    'Does the contractual framing adequately capture the full relational and social essence of marriage, or does it reduce it to a transactional agreement, potentially undermining non-contractual aspects of commitment and care?',
    'Sociological and psychological studies on the lived experience of marriage under secular law, compared to historical or culturally specific relational models.',
    'If the contractual frame is found to significantly distort or diminish the relational essence, it could indicate a subtle form of ''identity_locked'' exit for individuals who seek a deeper, non-contractual bond, increasing their effective extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contractual_vs_relational_essence, empirical, 'Whether the contractual model fully captures the nature of marriage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__secular_contractual_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(fami_tr_t1925, family_law_authority__secular_contractual_reading, theater_ratio, 1925, 0.09).
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__secular_contractual_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(fami_tr_t1975, family_law_authority__secular_contractual_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__secular_contractual_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__secular_contractual_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__secular_contractual_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(fami_be_t1925, family_law_authority__secular_contractual_reading, base_extractiveness, 1925, 0.12).
narrative_ontology:measurement(fami_be_t1950, family_law_authority__secular_contractual_reading, base_extractiveness, 1950, 0.13).
narrative_ontology:measurement(fami_be_t1975, family_law_authority__secular_contractual_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__secular_contractual_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__secular_contractual_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__secular_contractual_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(fami_su_t1925, family_law_authority__secular_contractual_reading, suppression_requirement, 1925, 0.32).
narrative_ontology:measurement(fami_su_t1950, family_law_authority__secular_contractual_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(fami_su_t1975, family_law_authority__secular_contractual_reading, suppression_requirement, 1975, 0.37).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__secular_contractual_reading, suppression_requirement, 2000, 0.39).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__secular_contractual_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, inheritance_law).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, tax_law).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, parental_rights).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, social_welfare_benefits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
