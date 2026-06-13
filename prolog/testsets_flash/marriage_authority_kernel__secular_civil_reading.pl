% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of marriage and family law as
 *   derived from the secular civil code, specifically the Special Marriage
 *   Act 1954, which is grounded in constitutional individual rights. It
 *   provides a legal alternative to religious personal laws, enabling
 *   inter-religious marriages and promoting gender equity. This is one
 *   reading of the broader 'marriage_authority_kernel' in India, which is
 *   contested by various religious personal law frameworks.
 *
 * KEY AGENTS:
 *   - civil_courts: Agenda setter (institutional/analytical) — adjudicates under SMA
 *   - inter_religious_couples: Beneficiary (moderate/constrained) — enabled to marry
 *   - women_seeking_equity: Beneficiary (moderate/constrained) — gain legal protections
 *   - secular_citizens: Beneficiary (organized/mobile) — prefer uniform civil code
 *   - religious_community_leaders_opposed_to_intermarriage: Payer (powerful/trapped) — lose authority
 *   - traditional_community_members: Payer (powerless/identity_locked) — face social costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.45).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.3).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '59ef7a4e-749a-4be1-82f9-30de486ecf0c').
narrative_ontology:cs_kernel_codification('59ef7a4e-749a-4be1-82f9-30de486ecf0c', formalized).
narrative_ontology:cs_authority_grounding('59ef7a4e-749a-4be1-82f9-30de486ecf0c', lineage).
narrative_ontology:cs_interpretation_layer_present('59ef7a4e-749a-4be1-82f9-30de486ecf0c').
narrative_ontology:cs_reading_relation('59ef7a4e-749a-4be1-82f9-30de486ecf0c', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('59ef7a4e-749a-4be1-82f9-30de486ecf0c', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('59ef7a4e-749a-4be1-82f9-30de486ecf0c', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('59ef7a4e-749a-4be1-82f9-30de486ecf0c', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('59ef7a4e-749a-4be1-82f9-30de486ecf0c', foundational, individual_rights_supremacy_in_marriage).
narrative_ontology:cs_axiom_status(individual_rights_supremacy_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('59ef7a4e-749a-4be1-82f9-30de486ecf0c', individual_rights_supremacy_in_marriage, deontological).
narrative_ontology:cs_axiom('59ef7a4e-749a-4be1-82f9-30de486ecf0c', foundational, state_neutrality_in_religious_matters).
narrative_ontology:cs_axiom_status(state_neutrality_in_religious_matters, holdable).
narrative_ontology:cs_axiom_grounding('59ef7a4e-749a-4be1-82f9-30de486ecf0c', state_neutrality_in_religious_matters, conventional).
narrative_ontology:cs_reference_frame('59ef7a4e-749a-4be1-82f9-30de486ecf0c', constitutional_individual_rights_framework).
narrative_ontology:cs_drift_state('59ef7a4e-749a-4be1-82f9-30de486ecf0c', contemporary_political_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('59ef7a4e-749a-4be1-82f9-30de486ecf0c', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_equity).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, secular_citizens).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_community_leaders_opposed_to_intermarriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, traditional_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates marriage, divorce, and succession disputes under the Special Marriage Act 1954, ensuring adherence to constitutional principles of equality and individual rights. Provides a legal framework independent of religious personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% Can legally marry without converting to a partner's religion or being subject to specific religious personal laws. This provides a pathway for unions that would otherwise be legally complex or impossible under community-specific laws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from the Act's gender-neutral provisions regarding divorce, alimony, and inheritance, which often offer greater equity compared to some traditional religious personal laws. This provides a legal recourse for fairer treatment.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_equity, beneficiary,
    moderate, biographical, constrained, national).

% Prefer a uniform civil code and see the Special Marriage Act as a step towards a more secular legal framework, aligning with constitutional ideals of individual liberty and equality over community-based religious identity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, secular_citizens, beneficiary,
    organized, generational, mobile, national).

% Experience a loss of authority over their community members' marital choices, particularly regarding inter-religious unions. They may face social pressure from their communities to resist the Act's provisions, but have no legal power to prevent marriages under it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_community_leaders_opposed_to_intermarriage, payer,
    powerful, generational, trapped, local).

% May face social ostracization or family disapproval for choosing to marry under the Special Marriage Act, especially if it involves an inter-religious union. While legally protected, the social costs can be significant, reflecting a tension between legal rights and community norms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, traditional_community_members, payer,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for marriage and family matters that is independent of religious affiliation, allowing individuals from different religious backgrounds to marry and ensuring uniform legal rights regardless of community personal laws.
% TRANSFER_FUNCTION: Transfers legal authority over marriage from religious personal laws and community institutions to the secular civil courts, and transfers legal rights and protections to individuals, particularly women and inter-religious couples, that may be absent or unequal under religious laws.
% ABSENT_VOICES: Advocates for a fully uniform civil code would argue that the existence of separate personal laws, even alongside the SMA, perpetuates legal inequality. Conversely, proponents of religious autonomy would argue for greater deference to community-specific laws.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act vanished, inter-religious marriages would become legally complex or impossible without conversion, individual rights (especially for women) would be solely subject to diverse and often unequal personal laws, and the secular legal space for marriage would collapse, forcing all citizens into religious frameworks.
% FOUNDING_PROBLEM: The problem of legal pluralism in marriage, where different religious communities had their own personal laws, leading to unequal rights, particularly for women, and making inter-religious marriages legally difficult or impossible.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and human rights organizations corroborate that the problem of legal inequality and the need for secular options remains live, even with the SMA's existence. The ongoing debate around a Uniform Civil Code further attests to the persistence of these issues, corroborated by constitutional law experts and civil society groups.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the social costs and resistance faced by those who opt for secular marriage, rather than direct financial extraction by the state. Suppression (0.30) is low, as the state actively enforces the right to secular marriage, but social pressures from traditional communities can still act as a form of soft suppression. Theater ratio (0.10) is low, as the Act genuinely functions as intended, providing a secular legal option. Accessibility collapse (0.60) is moderate; while a secular option exists, the social and identity-based barriers to choosing it are significant. Resistance (0.40) is moderate, reflecting ongoing social and political contestation from religious groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil courts and secular citizens, the SMA is a progressive rope, coordinating individual rights and secular governance. For religious community leaders and traditional members, it is a payer-seat snare, eroding their authority and imposing social costs, even if it is legally available. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil courts, inter-religious couples, women seeking equity, and secular citizens are beneficiaries (low d) as the constraint empowers them or aligns with their interests. Religious community leaders and traditional community members are payers (high d) as they bear the costs of diminished authority or social ostracization. The 'identity_locked' exit option for traditional community members reflects the deep social and cultural ties that make opting for secular marriage a high-cost choice, even if legally available.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to provide a secular, equitable marriage option) remains live, as evidenced by the 'founding_problem_status: live'. The increasing extractiveness over time reflects the growing social and political contestation around secularism and individual rights versus community identity, rather than a decay of the constraint's core function. The constraint is not mandatrophied; rather, its operation reveals ongoing societal tensions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_cost_vs_legal_right,
    'To what extent do social costs and community pressures effectively nullify the legal rights granted by the Special Marriage Act for individuals seeking secular or inter-religious marriages?',
    'Empirical studies on the lived experiences of couples marrying under the SMA, including rates of family ostracization, threats, and violence, compared to those marrying under personal laws.',
    'If social costs are found to be severely prohibitive, the effective accessibility collapse and suppression for individuals would be much higher than the legal framework suggests, potentially reclassifying the individual''s seat as ''trapped'' or the constraint as more extractive in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_cost_vs_legal_right, empirical, 'The gap between legal rights and social reality for secular marriage.').

omega_variable(
    secularism_vs_pluralism_framing,
    'Is the Special Marriage Act primarily an instrument of secularism (promoting a uniform civil code) or pluralism (providing an option within a diverse legal landscape)?',
    'Analysis of judicial interpretations and legislative intent over time, as well as public discourse and political party platforms regarding a Uniform Civil Code.',
    'If framed as primarily secularist, the constraint''s perceived legitimacy by religious communities would be lower, increasing resistance. If framed as pluralist, it might be seen as a less threatening option, potentially reducing social friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularism_vs_pluralism_framing, conceptual, 'The underlying ideological framing of the Special Marriage Act.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(marr_tr_t1974, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1974, 0.07).
narrative_ontology:measurement(marr_tr_t1994, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1994, 0.08).
narrative_ontology:measurement(marr_tr_t2014, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2014, 0.09).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.3).
narrative_ontology:measurement(marr_be_t1974, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1974, 0.35).
narrative_ontology:measurement(marr_be_t1994, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1994, 0.4).
narrative_ontology:measurement(marr_be_t2014, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2014, 0.43).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(marr_su_t1974, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1974, 0.25).
narrative_ontology:measurement(marr_su_t1994, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1994, 0.28).
narrative_ontology:measurement(marr_su_t2014, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2014, 0.29).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', focusing on the secular civil code. It coexists with and influences other religious personal law readings by providing an alternative legal framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
