% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Indian Christian Marriage Law (Canonical Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the legal framework for marriage and family
 *   among Christians in India, as defined by the Indian Christian Marriage
 *   Act 1872, which largely codifies Christian canonical law. It is one
 *   reading of the broader 'marriage_authority_kernel' in India, which
 *   encompasses diverse religious and secular legal systems. This reading is
 *   characterized by restrictive, fault-based divorce, the involvement of
 *   church tribunals for annulment, and moderate gender equity compared to
 *   more progressive secular laws. The constraint is claimed as a Tangled
 *   Rope, reflecting its dual function of coordinating community identity and
 *   extracting individual autonomy through its restrictive provisions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.65).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.75).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Indian Christian Marriage Law (Canonical Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, 'c3afdaf7-3b1a-415b-b1be-f267de9d8f2e').
narrative_ontology:cs_kernel_codification('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', formalized).
narrative_ontology:cs_authority_grounding('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', lineage).
narrative_ontology:cs_interpretation_layer_present('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e').
narrative_ontology:cs_reading_relation('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', foundational, marriage_as_sacrament_indissoluble).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', marriage_as_sacrament_indissoluble, theological).
narrative_ontology:cs_axiom('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', foundational, canonical_law_supremacy_in_marriage).
narrative_ontology:cs_axiom_status(canonical_law_supremacy_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', canonical_law_supremacy_in_marriage, conventional).
narrative_ontology:cs_reference_frame('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', traditional_canonical_interpretation).
narrative_ontology:cs_drift_state('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', contemporary_secular_pressure_and_reform_movements, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3afdaf7-3b1a-415b-b1be-f267de9d8f2e', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_community_leaders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, traditional_christian_families).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_spouses_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_equality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Christian canonical law as codified in the Indian Christian Marriage Act 1872. They benefit from maintaining traditional community identity and social order, and from the authority vested in them by the legal framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_community_leaders, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from a clear, religiously sanctioned framework for marriage, family structure, and inheritance. They value the social stability and community support derived from adherence to traditional norms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, traditional_christian_families, beneficiary,
    organized, generational, identity_locked, national).

% Face significant legal and social hurdles for divorce, often requiring fault-based grounds and navigating lengthy church annulment processes, which can be costly and emotionally taxing. Their autonomy in marital dissolution is restricted.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_spouses_seeking_divorce, payer,
    moderate, biographical, constrained, national).

% May experience gender-based disparities in property rights, maintenance, or custody under traditional interpretations of the law, with limited avenues for redress within the canonical framework compared to secular options.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_equality, payer,
    moderate, biographical, constrained, national).

% Advocate for a uniform civil code and greater individual rights, challenging the religious personal laws in Indian courts. They analyze the impact of these laws on gender equality and individual autonomy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_legal_advocates, observer,
    organized, generational, analytical, national).

% Enforce the Indian Christian Marriage Act 1872, but also interpret it in light of constitutional principles of equality and secularism. This often creates tension with purely canonical interpretations, leading to ongoing legal evolution.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, civil_courts_india, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, civil_courts_india, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To define and regulate marriage, family structure, inheritance, and social order for the Christian community in India, providing a stable framework for personal law rooted in religious tradition.
% TRANSFER_FUNCTION: Transfers authority over marital disputes, family norms, and individual autonomy in personal matters from individuals to church bodies and the codified canonical law, limiting individual choice in areas like divorce.
% ABSENT_VOICES: Those advocating for a uniform civil code or more liberal, rights-based interpretations of Christian personal law are often marginalized in traditional community discussions, though their arguments are heard in civil courts and public discourse.
% DISAPPEARANCE_RATIONALE: If the Indian Christian Marriage Act 1872 and its underlying canonical authority vanished overnight, the legal and social structure governing Christian families in India would collapse, requiring an entirely new framework for marriage, divorce, and inheritance, likely under a secular civil code.
% FOUNDING_PROBLEM: To provide a specific legal framework for marriage and family for the Christian population in British India, respecting their religious traditions while integrating them into the broader legal system.
% FOUNDING_PROBLEM_CORROBORATION: Traditional Christian community members and church bodies attest to the ongoing relevance of the Act for preserving religious identity and social order. Reformers and secular legal scholars contest its current necessity and fairness, arguing that the original problem of legal recognition has been superseded by issues of equality and individual rights; legislative debates and legal challenges support the contested status.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high due to the limitations on divorce and the authority of church tribunals, which can impose significant costs on individuals seeking to exit a marriage. Suppression (0.75) is high, stemming from both legal enforcement by civil courts and strong social and religious pressure within the Christian community. The theater ratio (0.20) is low-moderate; while some aspects of enforcement may be performative, the core functions of defining and regulating marriage are actively maintained. Accessibility collapse (0.60) is moderate, as secular alternatives exist (Special Marriage Act 1954) but are often difficult to access due to identity-lock and community expectations. Resistance (0.55) is moderate, driven by reform movements and legal challenges seeking greater individual rights and gender equality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Christian community leaders and traditional families, this framework is a legitimate and necessary Rope, coordinating religious identity and social stability. From the perspective of spouses seeking divorce or women seeking equality, it operates as a Snare or Tangled Rope, extracting autonomy and imposing burdens. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Christian community leaders and traditional Christian families are beneficiaries, as the constraint reinforces their authority, identity, and preferred social order. Christian spouses seeking divorce and Christian women seeking equality are victims, bearing the costs of restrictive laws and potential gender disparities. Civil courts act as both agenda-setters (enforcing the law) and observers (interpreting it constitutionally), while secular legal advocates primarily serve as observers, challenging the status quo.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canonical_vs_state_authority_ambiguity,
    'Is the authority of the Indian Christian Marriage Act 1872 primarily derived from Christian canonical law, or is it maintained by the Indian state''s codification and enforcement?',
    'Analysis of judicial rulings on conflicts between canonical interpretations and statutory provisions, and the extent to which civil courts defer to or override church tribunals.',
    'If primarily state-maintained, the constraint''s extractiveness might be more attributable to state power than religious doctrine; if canonical, the identity-lock mechanism is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_vs_state_authority_ambiguity, conceptual, 'Ambiguity regarding the ultimate source of authority for Christian marriage law in India.').

omega_variable(
    coordination_vs_extraction_in_divorce,
    'Is the restrictive, fault-based divorce framework a necessary coordination mechanism for upholding the sanctity of marriage within the Christian community, or an extractive mechanism limiting individual autonomy and imposing undue burdens?',
    'Empirical studies on the social and psychological impact of restrictive divorce laws versus more liberal ones on community stability and individual well-being, particularly in contexts where both options exist.',
    'If primarily coordination, the measured extraction is a legitimate cost; if primarily extraction, the constraint''s coercive nature is amplified, potentially shifting its classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_in_divorce, empirical, 'Whether restrictive divorce primarily serves coordination or extraction.').

omega_variable(
    identity_adherence_vs_social_pressure,
    'To what extent does adherence to this legal framework stem from genuine religious belief and identity within the Christian community versus social pressure and lack of accessible alternatives?',
    'Sociological surveys and qualitative studies exploring individual motivations for adherence, and the perceived costs and benefits of opting for secular legal alternatives.',
    'If adherence is primarily due to social pressure, the ''identity_locked'' exit option for many stakeholders is more akin to ''constrained'', potentially increasing their effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_adherence_vs_social_pressure, empirical, 'Distinguishing genuine religious adherence from social coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.1).
narrative_ontology:measurement(marr_tr_t1920, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(marr_tr_t1968, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(marr_tr_t2012, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2012, 0.19).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.5).
narrative_ontology:measurement(marr_be_t1920, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(marr_be_t1968, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(marr_be_t2012, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2012, 0.64).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.65).
narrative_ontology:measurement(marr_su_t1920, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(marr_su_t1968, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(marr_su_t2012, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2012, 0.74).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'marriage_authority_kernel' in India, each representing a distinct legal and social framework for marriage and family law. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
