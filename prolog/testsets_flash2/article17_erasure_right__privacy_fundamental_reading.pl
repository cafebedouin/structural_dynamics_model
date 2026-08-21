% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: GDPR Article 17: Right to Erasure (Privacy-Fundamental Reading)
 *   domain: technology_governance/data_protection_law
 *
 * SUMMARY:
 *   This constraint represents the 'privacy-fundamental' reading of GDPR
 *   Article 17, which establishes the right to erasure as a core mechanism
 *   for individual data sovereignty. In this reading, the constraint
 *   primarily functions as a coordination mechanism that empowers data
 *   subjects and imposes clear obligations on data controllers, with minimal
 *   inherent extraction. The metrics reflect a well-functioning legal right,
 *   where compliance costs are seen as necessary for privacy protection
 *   rather than undue extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.2).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.1).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "GDPR Article 17: Right to Erasure (Privacy-Fundamental Reading)").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/data_protection_law").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '72220e08-9f05-4ec4-96ba-eccad9f0cf8d').
narrative_ontology:cs_kernel_codification('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', fixed_text).
narrative_ontology:cs_authority_grounding('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', lineage).
narrative_ontology:cs_interpretation_layer_present('72220e08-9f05-4ec4-96ba-eccad9f0cf8d').
narrative_ontology:cs_reading_relation('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', foundational, individual_data_sovereignty_is_fundamental).
narrative_ontology:cs_axiom_status(individual_data_sovereignty_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', individual_data_sovereignty_is_fundamental, deontological).
narrative_ontology:cs_axiom('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', foundational, data_retention_must_be_justified_by_necessity).
narrative_ontology:cs_axiom_status(data_retention_must_be_justified_by_necessity, holdable).
narrative_ontology:cs_axiom_grounding('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', data_retention_must_be_justified_by_necessity, conventional).
narrative_ontology:cs_reference_frame('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', post_gdpr_privacy_framework).
narrative_ontology:cs_drift_state('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('72220e08-9f05-4ec4-96ba-eccad9f0cf8d', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, privacy_advocates).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, data_controllers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose personal data is processed. They benefit from the ability to request erasure of their data, asserting control over their digital footprint and mitigating risks of data misuse or retention beyond necessity. Their power is moderate, as they must actively invoke the right, but it is legally binding.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects, beneficiary,
    moderate, biographical, constrained, global).

% Organizations (e.g., social media platforms, e-commerce sites) that collect and process personal data. They bear the cost of implementing systems to identify and erase data upon request, and face legal penalties for non-compliance. Their exit options are constrained by the legal obligation to operate within GDPR jurisdictions.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_controllers, payer,
    institutional, biographical, constrained, global).

% Regulatory bodies responsible for enforcing GDPR. They interpret Article 17, investigate complaints, and impose fines for non-compliance, acting as the primary enforcers of the right to erasure. They shape the practical application of the constraint.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and individuals who champion data privacy rights. They benefit from the existence and enforcement of Article 17 as a foundational legal instrument for individual data control, using it to push for stronger privacy protections and hold data controllers accountable.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, privacy_advocates, beneficiary,
    organized, generational, mobile, global).

% Analyze the legal implications, effectiveness, and challenges of Article 17. They contribute to its interpretation and critique its application, influencing future policy and judicial decisions.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear legal framework for individuals to control their personal data held by organizations, coordinating expectations between data subjects and data controllers regarding data retention and deletion.
% TRANSFER_FUNCTION: Transfers control over personal data from data controllers back to data subjects, enabling the deletion of data that is no longer necessary or for which consent has been withdrawn.
% ABSENT_VOICES: While this reading prioritizes individual privacy, other perspectives (e.g., those concerned with the impact on free speech or historical record keeping) are often marginalized in the core privacy discourse. They would argue for a more balanced approach to erasure requests.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished, data subjects would lose a fundamental mechanism for data control, leading to indefinite data retention by many controllers. This would fundamentally alter the power dynamic in data governance, increasing risks of surveillance and data misuse, and necessitating new, fragmented approaches to data deletion.
% FOUNDING_PROBLEM: Individuals lacked effective legal means to control their personal data, leading to indefinite retention, misuse, and a power imbalance between data subjects and large data-processing entities.
% FOUNDING_PROBLEM_CORROBORATION: Data protection authorities, privacy advocates, and numerous civil society organizations attest that the problem of unchecked data retention and lack of individual control remains live, even with Article 17 in place, due to ongoing challenges in enforcement and compliance. Independent reports on data breaches and privacy violations corroborate the continuing need for such a right.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).
:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because the primary function is to empower individuals, not to extract rents. Compliance costs for data controllers are considered a necessary part of respecting fundamental rights. Suppression is low (0.1) as the constraint's persistence relies on legal enforcement and public support for privacy, not on suppressing alternatives for data subjects. Theater ratio is low (0.05) as the right is genuinely exercised and enforced, with little performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of Article 17 (e.g., as a censorship mechanism or competitive moat) would yield significantly different extractiveness and suppression metrics, as they focus on unintended or exploitative outcomes. This story explicitly focuses on the intended privacy-enhancing function.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects are the primary beneficiaries, gaining control over their data. Data controllers are the payers, bearing the costs of compliance. Data protection authorities act as agenda-setters, enforcing the right. Privacy advocates benefit from the strengthened legal framework. All these relationships align with the goal of individual data sovereignty.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_divergence_competitive_moat,
    'To what extent do the compliance costs of Article 17 disproportionately burden smaller data controllers, inadvertently creating a competitive moat for larger, established players?',
    'Empirical studies on the differential impact of GDPR compliance costs on SMEs versus large corporations, and analysis of market concentration trends post-GDPR.',
    'If significant, the ''competitive_moat_reading'' gains empirical support, suggesting a hidden extractive function that benefits incumbents, potentially reclassifying the constraint as a Tangled Rope from the perspective of smaller businesses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_divergence_competitive_moat, empirical, 'Assesses the validity of the ''competitive_moat_reading'' of Article 17.').

omega_variable(
    reading_divergence_censorship_mechanism,
    'To what extent is Article 17 being weaponized to suppress legitimate content or journalistic archives under the guise of privacy requests?',
    'Analysis of erasure request data, specifically identifying patterns of requests targeting public interest information, journalistic content, or critical commentary, and judicial rulings on such cases.',
    'If significant, the ''censorship_mechanism_reading'' gains empirical support, suggesting an extractive function that suppresses speech, potentially reclassifying the constraint as a Snare from the perspective of free expression advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_divergence_censorship_mechanism, empirical, 'Assesses the validity of the ''censorship_mechanism_reading'' of Article 17.').

omega_variable(
    epistemic_friction_for_requests,
    'What is the actual epistemic friction (difficulty, time, legal knowledge required) for data subjects to successfully exercise their right to erasure?',
    'User experience studies, analysis of data subject complaint rates, and legal aid case statistics related to Article 17 requests.',
    'High friction would indicate that the effective accessibility collapse for data subjects is higher than measured, and the constraint''s ''rope'' classification is overly optimistic, potentially pushing it towards a ''tangled_rope'' or ''snare'' from the individual''s perspective due to practical barriers to exercising the right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_friction_for_requests, empirical, 'Measures the practical barriers to exercising the right to erasure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(arti_tr_t5, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(arti_tr_t10, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(arti_be_t5, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(arti_be_t10, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 10, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(arti_su_t5, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(arti_su_t10, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 10, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
