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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Marriage/Family Law Authority from Secular Civil Code (Special Marriage Act 1954)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'secular_civil_reading' of the
 *   'marriage_authority_kernel', focusing on the Special Marriage Act (SMA)
 *   1954 in India. The SMA provides a legal framework for marriage and
 *   divorce independent of religious personal laws, grounded in
 *   constitutional individual rights and secular principles. It enables
 *   inter-religious marriages and offers gender-equitable provisions, often
 *   challenging the authority of traditional religious institutions. The
 *   metrics reflect its function as a coordination mechanism for individual
 *   rights, while acknowledging the extraction of authority from traditional
 *   systems and the social suppression faced by its users.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.45).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.55).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Marriage/Family Law Authority from Secular Civil Code (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '8915261e-aec7-4d7c-8c79-e95ae5c36b64').
narrative_ontology:cs_kernel_codification('8915261e-aec7-4d7c-8c79-e95ae5c36b64', formalized).
narrative_ontology:cs_authority_grounding('8915261e-aec7-4d7c-8c79-e95ae5c36b64', lineage).
narrative_ontology:cs_interpretation_layer_present('8915261e-aec7-4d7c-8c79-e95ae5c36b64').
narrative_ontology:cs_reading_relation('8915261e-aec7-4d7c-8c79-e95ae5c36b64', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('8915261e-aec7-4d7c-8c79-e95ae5c36b64', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('8915261e-aec7-4d7c-8c79-e95ae5c36b64', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8915261e-aec7-4d7c-8c79-e95ae5c36b64', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('8915261e-aec7-4d7c-8c79-e95ae5c36b64', foundational, individual_autonomy_supremacy).
narrative_ontology:cs_axiom_status(individual_autonomy_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('8915261e-aec7-4d7c-8c79-e95ae5c36b64', individual_autonomy_supremacy, deontological).
narrative_ontology:cs_axiom('8915261e-aec7-4d7c-8c79-e95ae5c36b64', foundational, state_neutrality_in_personal_law).
narrative_ontology:cs_axiom_status(state_neutrality_in_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('8915261e-aec7-4d7c-8c79-e95ae5c36b64', state_neutrality_in_personal_law, conventional).
narrative_ontology:cs_reference_frame('8915261e-aec7-4d7c-8c79-e95ae5c36b64', constitutional_secular_pluralism).
narrative_ontology:cs_drift_state('8915261e-aec7-4d7c-8c79-e95ae5c36b64', contemporary_political_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8915261e-aec7-4d7c-8c79-e95ae5c36b64', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, individuals_seeking_interreligious_marriage).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_equity).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, secular_advocates).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, traditional_religious_authorities).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, individuals_facing_social_ostracism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_community_members).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_individual_rights).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, gender_equality_principle).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, state_secularism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates marriage and divorce cases under the Special Marriage Act (SMA), ensuring adherence to constitutional principles of equality and individual rights. Its authority is derived from the Constitution, often challenging traditional religious interpretations.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% Utilize the SMA to legally solemnize marriages across religious lines, bypassing personal laws that might prohibit or complicate such unions. They gain legal recognition and autonomy but may face social disapproval from their communities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, individuals_seeking_interreligious_marriage, beneficiary,
    moderate, biographical, mobile, national).

% Benefit from the SMA's gender-neutral provisions and equal rights in divorce, maintenance, and inheritance, which often offer greater protections than some personal laws. Their ability to access these rights can be constrained by social norms and community pressure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_equity, beneficiary,
    moderate, biographical, constrained, national).

% Experience a reduction in their exclusive authority over marriage and family matters for individuals choosing the SMA. They may view the SMA as an encroachment on religious autonomy and a threat to community cohesion, leading to resistance and advocacy for their personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, traditional_religious_authorities, payer,
    institutional, generational, constrained, national).

% Actively promote the SMA as a progressive step towards a uniform civil code and greater individual freedoms, aligning with constitutional values. They support its enforcement and expansion.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, secular_advocates, beneficiary,
    organized, generational, analytical, national).

% May perceive the SMA as undermining traditional religious and community structures, leading to social pressure or ostracism for those who opt for it. Their identity is often deeply intertwined with their community's personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_community_members, payer,
    moderate, biographical, identity_locked, local).

% Individuals who choose to marry under the SMA, particularly in inter-religious unions, may face social exclusion, family disapproval, or even threats from their communities, despite legal protection.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, individuals_facing_social_ostracism, payer,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for marriage and divorce that is independent of religious personal laws, enabling inter-religious unions and ensuring uniform application of individual rights, particularly gender equity, across diverse communities.
% TRANSFER_FUNCTION: Transfers legal authority over marriage from religious personal laws to the secular state, granting individual autonomy and equal rights to spouses, often at the cost of traditional community control and religious authority.
% ABSENT_VOICES: Hardline religious conservatives and fundamentalist groups who advocate for the supremacy of their respective personal laws and reject state interference in religious matters are largely excluded from the legislative and judicial processes that shape the SMA's interpretation and enforcement.
% DISAPPEARANCE_RATIONALE: If the SMA vanished, individuals seeking inter-religious marriages would lose their primary legal avenue, forcing them into conversions or informal unions. Women in some communities would lose critical legal protections, and the state's role in upholding individual rights in marriage would be severely diminished, leading to a resurgence of purely religious adjudication and potential legal chaos.
% FOUNDING_PROBLEM: To address the legal fragmentation, inequalities, and lack of options for inter-religious marriages arising from India's diverse and often conflicting religious personal laws, and to align marriage law with the constitutional principles of equality and secularism.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional legal scholars, women's rights organizations, and human rights activists consistently corroborate that the founding problems of inequality and lack of inter-religious marriage options remain live, despite ongoing resistance from some religious groups. Judicial pronouncements also frequently reaffirm the constitutional mandate for such a law.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The SMA is classified as a 'rope' because its primary function is to coordinate individual rights and provide a secular alternative to personal laws, benefiting those seeking inter-religious unions or greater equity. Extractiveness (0.45) is moderate, as it extracts authority from traditional religious systems but also provides a valuable service. Suppression (0.55) is also moderate, reflecting the state's enforcement of secular law alongside the social pressures and resistance from religious communities. Theater ratio (0.20) is low, indicating that the law's enforcement is genuinely functional, though some performative aspects of state secularism may exist. Resistance (0.60) is high due to ongoing challenges from religious groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals seeking inter-religious marriage or women seeking equity, the SMA is a vital tool for autonomy and justice. From the perspective of traditional religious authorities and many community members, it is an encroachment on religious freedom and community norms. Civil courts and secular advocates see it as upholding constitutional values. The engine will compute these divergent classifications based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil courts, individuals seeking inter-religious marriage, women seeking equity, and secular advocates are beneficiaries, as the SMA directly serves their interests or aligns with their mandates. Traditional religious authorities and religious community members are payers, as the SMA diminishes their exclusive authority and challenges their social norms. Individuals facing social ostracism are also payers, bearing the social costs of choosing the secular path.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_acceptance_gap,
    'Is the SMA''s authority genuinely accepted by all communities, or is its enforcement primarily coercive in contexts where religious personal law is deeply entrenched?',
    'Sociological studies on community-level acceptance and adherence to SMA provisions versus personal laws, particularly in rural or conservative areas.',
    'If acceptance is low and enforcement is perceived as purely coercive, the effective suppression and extractiveness of the SMA from traditional communities would be higher, potentially shifting its classification towards a ''tangled_rope'' for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_acceptance_gap, empirical, 'Assesses the gap between legal authority and social legitimacy of the SMA.').

omega_variable(
    social_cost_as_suppression,
    'To what extent do social ostracism and community pressure, faced by those opting for the SMA, function as an effective form of suppression, limiting its accessibility despite legal provisions?',
    'Qualitative research and surveys among individuals who have opted for the SMA, documenting experiences of social exclusion, family disapproval, and the perceived costs of exercising their legal rights.',
    'If social costs are a significant barrier, the ''constrained'' exit options for individuals would be amplified, increasing their effective directionality towards the target end and raising the overall effective suppression of the constraint for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_as_suppression, empirical, 'Examines the role of social pressure as a form of suppression for SMA users.').

omega_variable(
    uniform_civil_code_trajectory,
    'Is the SMA a standalone option, or is it a transitional step towards a uniform civil code, and how would its classification change if it were explicitly framed as a scaffold for a future uniform code?',
    'Analysis of legislative intent, judicial pronouncements, and political discourse regarding a potential future uniform civil code. If a clear sunset or transitional mandate emerges, reclassify as ''scaffold''.',
    'If the SMA is explicitly recognized as a ''scaffold'' for a uniform civil code, its ''has_sunset_clause'' would become true (conceptually), and its justification would shift from steady-state coordination to transitional support, altering its classification and lifecycle analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uniform_civil_code_trajectory, conceptual, 'Explores the SMA''s role in the broader debate over a uniform civil code.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(marr_tr_t1974, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1974, 0.15).
narrative_ontology:measurement(marr_tr_t1994, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement(marr_tr_t2014, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.3).
narrative_ontology:measurement(marr_be_t1974, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1974, 0.35).
narrative_ontology:measurement(marr_be_t1994, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1994, 0.4).
narrative_ontology:measurement(marr_be_t2014, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2014, 0.43).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.4).
narrative_ontology:measurement(marr_su_t1974, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1974, 0.45).
narrative_ontology:measurement(marr_su_t1994, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1994, 0.5).
narrative_ontology:measurement(marr_su_t2014, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2014, 0.53).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', which encompasses various legal and religious frameworks for marriage in India. This secular civil reading provides an alternative to the personal laws, influencing their operational context without foreclosing their existence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
