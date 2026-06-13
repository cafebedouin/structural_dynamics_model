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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Marriage as Civil Contract under Secular State Law
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes marriage as a civil contract between autonomous
 *   individuals, legally defined and enforced by the state, independent of
 *   religious or customary requirements. It is one reading of the broader
 *   'family_law_authority' kernel, emphasizing gender-symmetric rights and
 *   state registration as the sole criterion for validity. This reading
 *   permits interfaith marriage and focuses on the legal and contractual
 *   aspects of the union.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.2).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.15).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Civil Contract under Secular State Law").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, 'cd8c5b28-b78c-4b51-8a9f-08e1622fccd7').
narrative_ontology:cs_kernel_codification('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', formalized).
narrative_ontology:cs_authority_grounding('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', lineage).
narrative_ontology:cs_interpretation_layer_present('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7').
narrative_ontology:cs_reading_relation('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', foundational, state_sovereignty_over_civil_status).
narrative_ontology:cs_axiom_status(state_sovereignty_over_civil_status, holdable).
narrative_ontology:cs_axiom_grounding('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', state_sovereignty_over_civil_status, conventional).
narrative_ontology:cs_axiom('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', foundational, gender_equality_in_contractual_rights).
narrative_ontology:cs_axiom_status(gender_equality_in_contractual_rights, holdable).
narrative_ontology:cs_axiom_grounding('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', gender_equality_in_contractual_rights, deontological).
narrative_ontology:cs_reference_frame('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', enlightenment_liberal_state).
narrative_ontology:cs_drift_state('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cd8c5b28-b78c-4b51-8a9f-08e1622fccd7', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, marrying_individuals).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, children_of_union).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, legal_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals entering into marriage gain legal recognition, rights, and responsibilities, including property rights, inheritance, and parental status, defined by state law. They are bound by the contract but also protected by it.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, marrying_individuals, beneficiary,
    moderate, biographical, constrained, national).

% The state defines the terms of marriage, registers unions, and adjudicates disputes (e.g., divorce, child custody). It benefits from a clear, uniform framework for family formation and social stability, reducing ambiguity and conflict.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Religious bodies may perform ceremonies but their rites do not confer legal status; only state registration does. They are excluded from legal authority over marriage, though they may retain moral or spiritual authority for their adherents.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_institutions, excluded,
    organized, generational, constrained, national).

% Children benefit from the legal clarity of parentage, inheritance, and support obligations established by the civil contract, providing a stable legal framework for their upbringing.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, children_of_union, beneficiary,
    powerless, generational, trapped, local).

% Lawyers, judges, and other legal professionals administer and interpret family law, benefiting from the structured demand for their services in contract formation, dispute resolution, and divorce proceedings.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, legal_professionals, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, legally binding framework for individuals to form recognized partnerships, defining rights and responsibilities related to property, children, and mutual support, thereby stabilizing social units and reducing legal ambiguity.
% TRANSFER_FUNCTION: Transfers legal rights and obligations between individuals, and from individuals to the state (e.g., tax benefits, social security survivor benefits, legal duties of care), in exchange for state recognition and enforcement of the marital contract.
% ABSENT_VOICES: Religious authorities who believe marriage should be primarily governed by divine or ecclesiastical law are excluded from legal authority over marriage. They would argue for the primacy of religious definitions and rites, but their views do not determine legal validity.
% DISAPPEARANCE_RATIONALE: If marriage as a civil contract vanished, the legal framework for family formation, inheritance, property rights, and child custody would collapse, leading to widespread legal chaos and necessitating a complete overhaul of social and legal structures to manage these relationships.
% FOUNDING_PROBLEM: The need for a clear, universal, and enforceable legal framework for partnerships that transcends religious or customary variations, ensuring equal rights and responsibilities for all citizens regardless of faith or background.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and international bodies corroborate the ongoing need for secular, equitable family law to ensure non-discrimination and protect individual rights, particularly in diverse societies. The state legal system itself, through its ongoing legislative and judicial functions, continuously affirms this problem as live.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).

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
 *   The constraint is classified as a Rope because it primarily serves a coordination function, providing a stable and predictable legal framework for individuals to form partnerships. Extractiveness is low (0.2) as the state primarily provides a service rather than extracting rents, though administrative fees and indirect tax implications exist. Suppression is also low (0.15) as individuals freely choose to enter the contract, and alternatives (e.g., civil unions, cohabitation agreements) exist, though they may not confer identical benefits. Theater ratio is minimal (0.05) as the state's role is largely functional and administrative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marrying individuals, the constraint is a beneficial framework for structuring their lives. From the state's perspective, it is an essential tool for social governance. Religious institutions, however, may view this secular reading as an erosion of their traditional authority, experiencing it as a form of exclusion or marginalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Marrying individuals are beneficiaries, gaining legal rights and protections. The state legal system is also a beneficiary, as it establishes a clear framework for social order and reduces legal ambiguity. Religious institutions are excluded from legal authority, meaning their directionality is not directly measured by this constraint, but they are structurally positioned as 'excluded' from the legal definition of marriage.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_vs_religious_primacy,
    'Is the secular contractual reading of marriage truly independent of religious influence, or does it implicitly carry historical religious norms?',
    'Comparative legal analysis across jurisdictions with varying historical religious influences, examining the evolution of ''contractual'' terms (e.g., gender roles, divorce grounds) for residual religious assumptions.',
    'If significant religious influence is found, the ''secular'' claim is weakened, and the constraint might exhibit subtle forms of identity_coordination or even extraction for those whose identities do not align with these implicit norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_vs_religious_primacy, conceptual, 'Ambiguity of secularism in family law.').

omega_variable(
    state_benefit_vs_cost,
    'Does the state''s benefit from a uniform civil marriage framework outweigh the administrative costs and potential for overreach into private life?',
    'Cost-benefit analysis of state family law administration versus social stability metrics, and analysis of legal challenges to state intervention in marital affairs.',
    'If costs or overreach are substantial, the state''s role might shift towards a ''tangled_rope'' or ''snare'' for citizens, where the coordination function is overshadowed by state extraction or control.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_benefit_vs_cost, empirical, 'Balance of state benefit vs. cost in civil marriage.').

omega_variable(
    contractual_autonomy_vs_social_norm,
    'To what extent does the ''autonomous individual'' framing genuinely reflect individual choice, versus reinforcing a social norm of marriage that constrains alternatives?',
    'Sociological studies on marriage rates, cohabitation trends, and social pressure to marry, particularly for different demographic groups, to assess the ''freedom'' of choice.',
    'If social pressure is high, the ''rope'' classification might be too benign, as the constraint could function as a ''tangled_rope'' or ''snare'' for those who feel compelled to marry for social acceptance or benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractual_autonomy_vs_social_norm, empirical, 'Individual autonomy vs. social pressure in marriage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__secular_contractual_reading, theater_ratio, 1900, 0.02).
narrative_ontology:measurement(fami_tr_t1930, family_law_authority__secular_contractual_reading, theater_ratio, 1930, 0.03).
narrative_ontology:measurement(fami_tr_t1960, family_law_authority__secular_contractual_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__secular_contractual_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__secular_contractual_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__secular_contractual_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(fami_be_t1930, family_law_authority__secular_contractual_reading, base_extractiveness, 1930, 0.18).
narrative_ontology:measurement(fami_be_t1960, family_law_authority__secular_contractual_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__secular_contractual_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__secular_contractual_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__secular_contractual_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(fami_su_t1930, family_law_authority__secular_contractual_reading, suppression_requirement, 1930, 0.12).
narrative_ontology:measurement(fami_su_t1960, family_law_authority__secular_contractual_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__secular_contractual_reading, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__secular_contractual_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
