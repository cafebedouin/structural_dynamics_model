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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of marriage and family law
 *   derived from India's secular civil code, specifically the Special
 *   Marriage Act (SMA) of 1954, which is grounded in constitutional
 *   individual rights. It offers an alternative to various religious personal
 *   laws, enabling inter-religious marriages and promoting gender equity.
 *   This story is one reading of the broader 'marriage_authority_kernel',
 *   focusing on the secular framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.35).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.45).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '9eae75b7-669e-47c4-9485-3b6577965950').
narrative_ontology:cs_kernel_codification('9eae75b7-669e-47c4-9485-3b6577965950', formalized).
narrative_ontology:cs_authority_grounding('9eae75b7-669e-47c4-9485-3b6577965950', lineage).
narrative_ontology:cs_interpretation_layer_present('9eae75b7-669e-47c4-9485-3b6577965950').
narrative_ontology:cs_reading_relation('9eae75b7-669e-47c4-9485-3b6577965950', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eae75b7-669e-47c4-9485-3b6577965950', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eae75b7-669e-47c4-9485-3b6577965950', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eae75b7-669e-47c4-9485-3b6577965950', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('9eae75b7-669e-47c4-9485-3b6577965950', foundational, individual_autonomy_in_marriage).
narrative_ontology:cs_axiom_status(individual_autonomy_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('9eae75b7-669e-47c4-9485-3b6577965950', individual_autonomy_in_marriage, deontological).
narrative_ontology:cs_axiom('9eae75b7-669e-47c4-9485-3b6577965950', foundational, gender_equality_in_family_law).
narrative_ontology:cs_axiom_status(gender_equality_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('9eae75b7-669e-47c4-9485-3b6577965950', gender_equality_in_family_law, deontological).
narrative_ontology:cs_reference_frame('9eae75b7-669e-47c4-9485-3b6577965950', constitutional_individual_rights).
narrative_ontology:cs_drift_state('9eae75b7-669e-47c4-9485-3b6577965950', contemporary_pluralistic_legal_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9eae75b7-669e-47c4-9485-3b6577965950', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, individuals_seeking_secular_marriage).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_equity).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_personal_law_boards).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, conservative_community_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates marriage and divorce cases under the Special Marriage Act, ensuring adherence to constitutional principles of equality and individual rights. Interprets the law and sets precedents.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, secular_civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% Opt for marriage under the SMA to bypass religious personal laws, seeking legal recognition and rights independent of religious affiliation. They benefit from a uniform, rights-based framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, individuals_seeking_secular_marriage, beneficiary,
    moderate, biographical, mobile, national).

% Utilize the SMA as the primary legal avenue for their unions, as religious personal laws often do not recognize inter-faith marriages or impose conversion requirements. They face social pressure but gain legal recognition.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from the SMA's explicit provisions for gender equality in marriage, divorce, and inheritance, which often offer greater protections than traditional religious personal laws. They may face community backlash for choosing this path.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_equity, beneficiary,
    moderate, biographical, constrained, national).

% Experience a reduction in their exclusive authority over marriage and family matters for those who opt for the SMA. They view it as an encroachment on their traditional jurisdiction and may actively resist its broader adoption.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_personal_law_boards, payer,
    institutional, generational, constrained, national).

% Bear the cost of diminished social control and influence over community members who choose secular marriage. They often exert social pressure and ostracization against those who opt out of traditional religious frameworks.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, conservative_community_leaders, payer,
    powerful, generational, constrained, local).

% Analyze the interplay between secular civil law and religious personal laws, assessing the SMA's effectiveness in upholding constitutional rights and promoting a uniform civil code.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, rights-based legal framework for marriage and divorce that is independent of religious affiliation, enabling inter-religious unions and ensuring gender equity where personal laws may not.
% TRANSFER_FUNCTION: Transfers legal authority over marriage and family matters from religious/communal bodies to the secular state, and transfers enhanced legal protections and rights to individuals, particularly women, that may be absent or unequal in personal laws.
% ABSENT_VOICES: Traditional religious authorities and conservative community groups who believe marriage should be solely governed by divine or community law are structurally marginalized in the SMA's operation; they would object to its existence and expansion.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act vanished overnight, individuals seeking secular or inter-religious marriages would lose their legal framework, forcing them back into personal laws or informal arrangements. This would lead to significant legal and social disruption, particularly for women and minorities seeking equitable rights.
% FOUNDING_PROBLEM: The founding problem was the fragmentation and inequality inherent in diverse religious personal laws, the lack of legal recourse for inter-religious couples, and the gender discrimination prevalent within many religious frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, women's rights organizations, and human rights advocates consistently corroborate the ongoing relevance of these problems, citing continued disparities in personal laws and persistent challenges faced by inter-religious couples and women seeking equitable rights.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The SMA is classified as a Rope because it provides a genuine coordination function (a uniform legal framework for marriage) with net benefits for participants (individual rights, gender equity, inter-religious marriage). Its extractiveness (0.35) is moderate, reflecting the legal costs and social friction associated with opting out of community-based laws, but significantly lower than purely extractive systems. Suppression (0.45) is also moderate, as it's an actively enforced legal framework, but it offers an alternative rather than coercively imposing itself. Resistance (0.50) is notable due to ongoing opposition from traditional religious authorities. Theater ratio is low (0.10) as the SMA is a functional legal instrument.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular civil courts and rights advocates, the SMA is a vital instrument for individual liberty and equality. However, from the perspective of traditional religious authorities, it represents an encroachment on their communal jurisdiction and a threat to religious identity. The engine's classification will reflect the SMA's function as a beneficial coordination mechanism, while acknowledging the resistance it faces.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals seeking secular or inter-religious marriages, and women seeking greater equity, are clear beneficiaries (low directionality) as the SMA provides them with legal avenues and protections often unavailable under personal laws. Religious personal law boards and conservative community leaders are targets (high directionality) as the SMA diminishes their exclusive authority and social control over marriage. Secular civil courts act as agenda-setters, enforcing the law and upholding its principles.
 *
 * MANDATROPHY ANALYSIS:
 *   The SMA's mandate remains live, as the problems it was designed to address—inequality in personal laws, lack of options for inter-religious couples—persist. Its function has not atrophied; rather, its importance is arguably growing as society evolves. The classification as a Rope reflects its ongoing utility and coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct, self-contained reading of the marriage_authority_kernel, or is its structural identity inextricably linked to the contestation with sibling readings?',
    'Analyze the SMA''s operational independence: if its legal principles and enforcement mechanisms function coherently without direct reference to personal laws, it is a distinct reading. If its meaning is primarily defined by what it is *not* (i.e., not a personal law), its identity is more relational.',
    'If more relational, its effective extractiveness and suppression might be higher due to the constant social friction of its existence within a pluralistic legal landscape, potentially shifting its classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural identity of the secular civil reading within the kernel contest.').

omega_variable(
    social_cost_of_exit,
    'What is the true social cost (ostracization, family rupture, community pressure) for individuals who opt for secular marriage, and how does this ''soft'' suppression affect effective accessibility?',
    'Sociological studies and qualitative interviews with couples who have married under the SMA, comparing their experiences to those under personal laws. Quantify the non-legal barriers to access.',
    'If social costs are high and pervasive, the effective accessibility_collapse for individuals is higher than the legal framework suggests, increasing the effective suppression and potentially pushing the constraint towards a Tangled Rope for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_of_exit, empirical, 'Impact of social pressure on the accessibility and effective freedom of choice under the SMA.').

omega_variable(
    enforcement_reach_vs_community_pressure,
    'How effectively can secular civil courts enforce the rights granted by the SMA against strong community pressure or informal religious adjudications?',
    'Case law analysis of SMA-related disputes where community pressure was a factor, and empirical studies on the implementation of court orders in such contexts.',
    'If enforcement is frequently undermined by community pressure, the SMA''s effective suppression is lower for those who would resist it, but its effective coordination function is also weakened, potentially leading to a Piton-like atrophy in certain contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_reach_vs_community_pressure, empirical, 'The practical limits of secular legal enforcement against social and religious norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement(marr_tr_t1966, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1966, 0.09).
narrative_ontology:measurement(marr_tr_t1978, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1978, 0.09).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(marr_tr_t2002, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(marr_tr_t2014, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.25).
narrative_ontology:measurement(marr_be_t1966, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1966, 0.28).
narrative_ontology:measurement(marr_be_t1978, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(marr_be_t1990, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(marr_be_t2002, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2002, 0.33).
narrative_ontology:measurement(marr_be_t2014, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2014, 0.34).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.4).
narrative_ontology:measurement(marr_su_t1966, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1966, 0.42).
narrative_ontology:measurement(marr_su_t1978, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1978, 0.43).
narrative_ontology:measurement(marr_su_t1990, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1990, 0.44).
narrative_ontology:measurement(marr_su_t2002, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2002, 0.44).
narrative_ontology:measurement(marr_su_t2014, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2014, 0.45).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'marriage_authority_kernel', each representing a distinct legal or social framework for marriage in India. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
