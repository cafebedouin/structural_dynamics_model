% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Act 1955 as Interpreted by Civil Courts
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of marriage and family law for
 *   the Hindu community in India, derived from the Hindu Marriage Act 1955 as
 *   interpreted by civil courts. It represents one reading of the broader
 *   'marriage_authority_kernel' in India, which is characterized by a
 *   pluralistic legal system where different religious communities are
 *   governed by their own personal laws, alongside a secular civil code. This
 *   reading provides uniform rules within the Hindu community and is
 *   adjudicated by state courts, aiming for moderate gender equity compared
 *   to other personal laws, but often falling short of secular standards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.45).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act 1955 as Interpreted by Civil Courts").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, '08b00c3a-9436-48ee-97c2-11c9e8e007ae').
narrative_ontology:cs_kernel_codification('08b00c3a-9436-48ee-97c2-11c9e8e007ae', formalized).
narrative_ontology:cs_authority_grounding('08b00c3a-9436-48ee-97c2-11c9e8e007ae', lineage).
narrative_ontology:cs_interpretation_layer_present('08b00c3a-9436-48ee-97c2-11c9e8e007ae').
narrative_ontology:cs_reading_relation('08b00c3a-9436-48ee-97c2-11c9e8e007ae', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('08b00c3a-9436-48ee-97c2-11c9e8e007ae', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('08b00c3a-9436-48ee-97c2-11c9e8e007ae', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('08b00c3a-9436-48ee-97c2-11c9e8e007ae', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('08b00c3a-9436-48ee-97c2-11c9e8e007ae', foundational, hindu_personal_law_autonomy).
narrative_ontology:cs_axiom_status(hindu_personal_law_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('08b00c3a-9436-48ee-97c2-11c9e8e007ae', hindu_personal_law_autonomy, conventional).
narrative_ontology:cs_axiom('08b00c3a-9436-48ee-97c2-11c9e8e007ae', foundational, state_adjudication_of_personal_law).
narrative_ontology:cs_axiom_status(state_adjudication_of_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('08b00c3a-9436-48ee-97c2-11c9e8e007ae', state_adjudication_of_personal_law, conventional).
narrative_ontology:cs_reference_frame('08b00c3a-9436-48ee-97c2-11c9e8e007ae', codified_hindu_law_with_state_oversight).
narrative_ontology:cs_drift_state('08b00c3a-9436-48ee-97c2-11c9e8e007ae', contemporary_gender_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('08b00c3a-9436-48ee-97c2-11c9e8e007ae', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_judiciary).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_men).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, hindu_personal_law_autonomy).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, state_secularism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for the preservation and interpretation of Hindu personal law, influencing legislative amendments and judicial interpretations. They benefit from the social cohesion and traditional authority maintained by the Act.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders, agenda_setter,
    organized, generational, constrained, national).

% Interprets and enforces the Hindu Marriage Act 1955, balancing traditional principles with constitutional rights. Benefits from maintaining legal order and state authority over personal law, even if it means navigating complex cultural norms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Generally benefit from the stability and traditional recognition of marriage under the Act, which historically favored patriarchal structures, though modern interpretations have introduced more equity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_men, beneficiary,
    moderate, biographical, mobile, local).

% Face procedural hurdles and social stigma when seeking divorce under the Act, despite its provisions for dissolution. While the Act provides a legal path, the process can be lengthy and financially burdensome, reflecting residual patriarchal norms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_divorce, payer,
    powerless, biographical, constrained, local).

% Cannot marry under the Hindu Marriage Act if one partner is not Hindu, forcing them to use the Special Marriage Act. This can lead to social pressure, family disapproval, and legal complexities, effectively extracting a cost for non-conformity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples, payer,
    powerless, biographical, identity_locked, local).

% Advocate for a uniform civil code, critiquing the personal laws for perpetuating gender inequality and communal divisions. They analyze the Act's impact and propose legislative changes.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, secular_legal_reformers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized legal framework for marriage, divorce, and family matters for individuals identifying as Hindu, ensuring legal recognition and dispute resolution within that community.
% TRANSFER_FUNCTION: Transfers legal authority over Hindu family matters from purely religious/customary bodies to state civil courts, while retaining the substantive principles of Hindu law. It also transfers social costs to those whose marital choices deviate from community norms.
% ABSENT_VOICES: Many women's rights advocates argue that the voices of women, particularly those from marginalized communities within the Hindu fold, were not adequately represented during the drafting and initial interpretations of the Act, leading to provisions that, despite reforms, still carry patriarchal biases. Interfaith couples, by definition, are excluded from its direct application.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act vanished overnight, the legal framework for millions of marriages and divorces would collapse, leading to immense legal chaos, social instability, and a vacuum that would force a rapid, potentially contentious, reorganization of family law for the Hindu community.
% FOUNDING_PROBLEM: Prior to 1955, Hindu personal law was a complex, uncodified mix of diverse customs, regional variations, and scriptural interpretations, leading to legal uncertainty, gender inequality, and difficulty in dispute resolution.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historical records corroborate the pre-1955 chaos. While the Act brought significant reform, its status as 'live' is contested by secular reformers who argue it still perpetuates inequality, and by some traditionalists who feel it deviates too much from 'true' Hindu law. The civil judiciary, however, largely views it as a functional, if evolving, framework.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the Act's dual nature: it codified and reformed Hindu law, introducing some gender equity, but still retains elements that can disadvantage women or interfaith couples. Suppression (0.6) is significant because individuals identifying as Hindu are largely bound by this Act, with limited alternatives for marriage and divorce that avoid social or legal complications. The theater ratio (0.2) is low, as the Act remains a functional legal framework, though some of its original justifications for gender disparity have become performative. The decreasing extractiveness over time reflects judicial activism and legislative amendments that have incrementally improved gender equity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hindu community leaders and the civil judiciary, the Act is a necessary and evolving framework for community governance. From the perspective of Hindu women seeking divorce or interfaith couples, it can be an extractive and suppressive mechanism that limits their autonomy and imposes costs for non-conformity. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu community leaders and the civil judiciary are beneficiaries and agenda-setters, as they maintain authority and order. Hindu men generally benefit from the traditional stability. Hindu women seeking divorce and interfaith couples are payers, facing higher costs and constrained options. Secular legal reformers are observers, advocating for a uniform civil code.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equity_vs_tradition,
    'To what extent do judicial interpretations of the Hindu Marriage Act genuinely advance gender equity versus merely reinterpreting traditional norms to fit modern legal discourse?',
    'Empirical analysis of court judgments over time, specifically tracking outcomes for women in divorce, property, and maintenance cases, compared against secular legal standards.',
    'If interpretations primarily reframe tradition without substantive equity gains, the extractiveness for women is higher than currently assessed; if genuine equity is achieved, extractiveness is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_vs_tradition, empirical, 'Ambiguity in the actual impact of judicial interpretations on gender equity.').

omega_variable(
    uniformity_vs_pluralism,
    'Is the goal of a uniform civil code (which would supersede personal laws) a desirable and achievable outcome, or does the pluralistic system, despite its flaws, better reflect India''s diverse social fabric?',
    'Sociological studies on the impact of a uniform civil code in diverse societies, and political consensus-building processes.',
    'If a uniform civil code is deemed desirable and achievable, the current pluralistic system (including the HMA) would be reclassified as a more extractive and suppressive constraint on individual autonomy; if pluralism is affirmed, the HMA''s coordination function is emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uniformity_vs_pluralism, preference, 'The fundamental policy choice between legal uniformity and religious pluralism in family law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.6).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.7).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', which encompasses various personal laws and a secular civil code in India. Each reading represents a distinct legal and social framework for marriage and family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
