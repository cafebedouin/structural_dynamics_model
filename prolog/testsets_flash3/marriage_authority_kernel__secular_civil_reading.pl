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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint describes the authority of secular civil marriage law
 *   (Special Marriage Act 1954) in India, grounded in constitutional
 *   individual rights. It is one reading of the broader
 *   'marriage_authority_kernel' which encompasses multiple religious personal
 *   laws. This reading provides a legal alternative to religious-specific
 *   marriage frameworks, offering greater gender equity and enabling
 *   inter-religious unions. Its persistence is due to its alignment with
 *   constitutional principles and the active advocacy of secular legal
 *   groups, despite resistance from some religious communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.35).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.2).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, 'fc2b251a-a4be-48fd-92a8-33e01ffc784e').
narrative_ontology:cs_kernel_codification('fc2b251a-a4be-48fd-92a8-33e01ffc784e', formalized).
narrative_ontology:cs_authority_grounding('fc2b251a-a4be-48fd-92a8-33e01ffc784e', lineage).
narrative_ontology:cs_interpretation_layer_present('fc2b251a-a4be-48fd-92a8-33e01ffc784e').
narrative_ontology:cs_reading_relation('fc2b251a-a4be-48fd-92a8-33e01ffc784e', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc2b251a-a4be-48fd-92a8-33e01ffc784e', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc2b251a-a4be-48fd-92a8-33e01ffc784e', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc2b251a-a4be-48fd-92a8-33e01ffc784e', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('fc2b251a-a4be-48fd-92a8-33e01ffc784e', foundational, individual_rights_supremacy_in_marriage).
narrative_ontology:cs_axiom_status(individual_rights_supremacy_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('fc2b251a-a4be-48fd-92a8-33e01ffc784e', individual_rights_supremacy_in_marriage, deontological).
narrative_ontology:cs_axiom('fc2b251a-a4be-48fd-92a8-33e01ffc784e', foundational, state_neutrality_in_religious_matters).
narrative_ontology:cs_axiom_status(state_neutrality_in_religious_matters, holdable).
narrative_ontology:cs_axiom_grounding('fc2b251a-a4be-48fd-92a8-33e01ffc784e', state_neutrality_in_religious_matters, conventional).
narrative_ontology:cs_reference_frame('fc2b251a-a4be-48fd-92a8-33e01ffc784e', constitutional_secularism_and_individual_autonomy).
narrative_ontology:cs_drift_state('fc2b251a-a4be-48fd-92a8-33e01ffc784e', contemporary_political_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fc2b251a-a4be-48fd-92a8-33e01ffc784e', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_equity).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, secular_legal_advocates).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_community_leaders_opposed_to_intermarriage).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, individuals_seeking_religious_only_sanction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a legal framework that enables their marriage without requiring religious conversion or adherence to personal laws. They face social pressure but legal recognition is secure. Exit from this framework would mean either converting or living without legal marital status.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from the gender-equitable provisions of the secular code, which often offer stronger protections in divorce, inheritance, and maintenance compared to some personal laws. Their exit options are limited by the prevailing personal laws of their communities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_equity, beneficiary,
    moderate, generational, constrained, national).

% Actively promote and defend the secular civil code as a cornerstone of individual rights and constitutional secularism. They work through courts and legislative advocacy. Their 'exit' would be a retreat from the secular project, which is not structurally constrained.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, secular_legal_advocates, agenda_setter,
    organized, generational, mobile, national).

% Bear the cost of the secular code undermining their authority over community members' marital choices, particularly regarding inter-religious unions. They cannot legally prevent such marriages but exert social pressure. Their exit is to accept the secular framework, which challenges their traditional power.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_community_leaders_opposed_to_intermarriage, payer,
    powerful, generational, constrained, local).

% May find the secular code's requirements (e.g., notice period, lack of religious ceremony) to be a burden if they desire only religious sanction for their marriage, or if their community views secular marriage as illegitimate. Their identity is often deeply tied to religious community norms, making exit from those norms difficult.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, individuals_seeking_religious_only_sanction, payer,
    powerless, biographical, identity_locked, local).

% Are the primary adjudicators and enforcers of the Special Marriage Act, interpreting its provisions in line with constitutional principles. They provide the institutional backbone for the secular reading of marriage authority. Their 'exit' would be a constitutional crisis.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform legal framework for marriage and divorce that transcends religious personal laws, ensuring legal recognition and rights for all citizens, especially in inter-religious unions or when seeking gender-equitable provisions.
% TRANSFER_FUNCTION: Transfers legal authority over marriage from diverse religious personal laws to a single secular civil code, granting individual rights and state oversight in exchange for community-specific religious sanction.
% ABSENT_VOICES: Hardline religious fundamentalists who reject the very concept of secular marriage authority are structurally excluded from the legal discourse, as their claims are outside the constitutional framework. They would argue for the supremacy of religious law.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act vanished, inter-religious couples would lose legal recognition, women's rights in marriage and divorce would revert to potentially less equitable personal laws, and the constitutional principle of secularism in family matters would be severely undermined. The legal and social landscape of marriage would fundamentally shift.
% FOUNDING_PROBLEM: To provide a legal option for marriage that was independent of religious personal laws, particularly for inter-religious couples, and to ensure a more equitable framework for women's rights within marriage.
% FOUNDING_PROBLEM_CORROBORATION: Secular legal scholars, women's rights organizations, and constitutional experts consistently corroborate that the problems of religious personal law fragmentation and gender inequity remain live, making the secular code's function ongoing. While religious leaders may contest the 'problem' itself, the legal and social need for the SMA is widely attested by independent parties.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.35) is moderate, reflecting the social costs and occasional legal challenges faced by those who choose secular marriage, particularly from religious communities. Suppression (0.20) is low, as the state actively enforces the right to secular marriage, but social pressures can still be significant. Theater ratio (0.10) is low, indicating the law is genuinely functional and not merely performative. The accessibility collapse is moderate (0.40) because while the secular option exists, social and identity-based pressures can make it feel less accessible than community-specific laws. Resistance (0.30) is present from religious groups who see it as undermining their authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular legal advocates, this is a Rope, a vital coordination mechanism for individual rights. From the perspective of religious community leaders, it is a Snare, undermining their traditional authority and extracting adherence to a secular framework they reject. The engine's classification will reflect this divergence based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Inter-religious couples and women seeking equity are clear beneficiaries, as the law directly enables their choices and protects their rights. Secular legal advocates and civil courts act as agenda-setters and enforcers, benefiting from the vindication of constitutional principles. Religious community leaders opposed to intermarriage and individuals seeking only religious sanction are payers, as the secular code challenges their traditional authority or identity-bound choices.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_vs_legal_suppression,
    'To what extent does social pressure from religious communities effectively suppress the choice of secular marriage, despite its legal availability?',
    'Sociological studies tracking rates of inter-religious marriage, community ostracization, and legal challenges to secular marriages, disaggregated by region and community.',
    'If social suppression is high, the effective suppression for individuals choosing secular marriage is higher than the legal measure suggests, potentially shifting the individual''s seat classification towards ''constrained'' or ''identity_locked'' even if the law is a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_vs_legal_suppression, empirical, 'Ambiguity between legal availability and social enforceability of secular marriage.').

omega_variable(
    constitutional_supremacy_vs_religious_autonomy,
    'Is the constitutional grounding of individual rights in marriage inherently superior to claims of religious community autonomy, or are these incommensurable normative frameworks?',
    'Philosophical and legal analysis of constitutional pluralism, examining whether a ''meta-framework'' can adjudicate between these claims without privileging one. This is a conceptual, not empirical, resolution.',
    'If constitutional supremacy is taken as foundational, the secular reading is a Mountain for individual rights. If incommensurable, the secular reading is a Rope that coordinates one set of values, but its ''extraction'' from religious communities is a legitimate cost of choosing a different normative order, not a defect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_supremacy_vs_religious_autonomy, conceptual, 'The fundamental normative conflict between individual rights and religious community autonomy in marriage law.').


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
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.2).
narrative_ontology:measurement(marr_be_t1974, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1974, 0.25).
narrative_ontology:measurement(marr_be_t1994, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1994, 0.3).
narrative_ontology:measurement(marr_be_t2014, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2014, 0.33).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.15).
narrative_ontology:measurement(marr_su_t1974, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1974, 0.17).
narrative_ontology:measurement(marr_su_t1994, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1994, 0.18).
narrative_ontology:measurement(marr_su_t2014, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2014, 0.19).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel'. Its secular, individual-rights-based approach influences and coexists with other religious personal law readings, offering an alternative legal path.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
