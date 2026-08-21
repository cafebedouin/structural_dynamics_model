% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__narrow_warning_reading, []).

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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation Doctrine (Narrow Warning Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint represents the 'narrow warning' reading of the beta
 *   designation doctrine, where beta status is understood as a time-bounded
 *   disclosure for genuine testing, preserving base product liability. It
 *   aims to balance software innovation with consumer protection, ensuring
 *   that 'beta' is not an indefinite shield against responsibility. This
 *   reading emphasizes good-faith testing and clear communication to users,
 *   rather than broad liability waivers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.25).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.15).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, rope).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '9d0153d4-8292-4a4c-9ff4-6e0befa63a2b').
narrative_ontology:cs_kernel_codification('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', formalized).
narrative_ontology:cs_authority_grounding('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', lineage).
narrative_ontology:cs_interpretation_layer_present('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b').
narrative_ontology:cs_reading_relation('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', beta_designation_doctrine__expansive_shield_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', beta_designation_doctrine__severity_carve_out_reading, influences).
narrative_ontology:cs_axiom('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', foundational, beta_status_is_time_bounded_testing).
narrative_ontology:cs_axiom_status(beta_status_is_time_bounded_testing, holdable).
narrative_ontology:cs_axiom_grounding('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', beta_status_is_time_bounded_testing, conventional).
narrative_ontology:cs_axiom('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', foundational, base_product_liability_is_preserved).
narrative_ontology:cs_axiom_status(base_product_liability_is_preserved, holdable).
narrative_ontology:cs_axiom_grounding('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', base_product_liability_is_preserved, deontological).
narrative_ontology:cs_reference_frame('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', balanced_innovation_consumer_protection).
narrative_ontology:cs_drift_state('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9d0153d4-8292-4a4c-9ff4-6e0befa63a2b', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, early_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and release software in beta, benefiting from user feedback and a limited liability shield during a genuine testing phase. They must clearly disclose the beta status and its implications, and ensure the testing phase is time-bounded and purposeful.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, agenda_setter,
    powerful, biographical, mobile, global).

% Gain early access to new software features and influence product development through feedback. They accept the risks associated with beta software, provided they are adequately warned and fundamental product liability is preserved.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, early_adopters, beneficiary,
    moderate, immediate, mobile, global).

% Benefits from more thoroughly tested software reaching the market eventually. This reading ensures they are not unknowingly exposed to undue risk by products masquerading as beta indefinitely.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, general_public, observer,
    powerless, generational, analytical, global).

% Enforce consumer protection laws and ensure that beta designations are not abused to circumvent liability. They monitor developer practices and user disclosures to maintain a fair balance between innovation and safety.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expectations and responsibilities between software developers and early adopters during a product's genuine testing phase, allowing for iterative development and feedback while managing risk.
% TRANSFER_FUNCTION: Transfers user feedback and bug reports from early adopters to developers, in exchange for early access to software and a temporary, limited reduction in developer liability.
% ABSENT_VOICES: Users who might be harmed by indefinite 'beta' products without adequate disclosure or preserved liability are implicitly represented by regulatory bodies, but their direct voice is often diffuse.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, developers would face full liability for all pre-release software, likely stifling innovation and public testing. Users would lose early access opportunities, and the software development lifecycle would become more rigid and less responsive to feedback.
% FOUNDING_PROBLEM: Software development requires user testing to identify bugs and improve features, but developers need a mechanism to manage liability during this inherently unstable phase without fully waiving user protections.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and consumer advocacy groups, alongside industry bodies, generally agree that a mechanism for managed pre-release testing is necessary, though they contest the scope and duration of liability limitations. This reading reflects a consensus on balancing innovation with consumer safety.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__narrow_warning_reading_tests).
:- end_tests(beta_designation_doctrine__narrow_warning_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the liability reduction is limited and temporary, primarily covering risks inherent to genuine testing. Suppression is also low (0.15) as users retain fundamental rights and can exit if dissatisfied with the beta experience. Theater ratio is low (0.1) because the doctrine, under this reading, requires genuine testing activity, minimizing performative 'beta' labels. The metrics reflect a functional coordination mechanism with minimal extractive overhead.
 *
 * PERSPECTIVAL GAP:
 *   Developers might prefer a broader liability shield, while consumer advocates would push for even stricter liability. This reading attempts to find a middle ground, which is why it computes as a Rope from most seats, indicating a mutually beneficial coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers are beneficiaries as they gain a managed testing environment. Early adopters are also beneficiaries, getting early access and influence. Regulatory bodies act as agenda-setters, ensuring the balance is maintained. No clear 'victims' exist under this narrow reading, as base product liability is preserved.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by tying the 'beta' status to a genuine, time-bounded testing phase. If the testing phase becomes indefinite or merely a label to avoid liability, the constraint would drift towards a Snare or Piton. The emphasis on 'genuine testing phase' and 'base product liability preserved' is crucial for its continued function as a Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beta_duration_genuineness,
    'Is the ''testing phase'' genuinely time-bounded and purposeful, or is it used to indefinitely extend a liability shield?',
    'Empirical analysis of software release cycles and post-beta defect rates. Legal challenges to ''perpetual beta'' products.',
    'If the duration is found to be indefinite or disingenuous, the constraint''s extractiveness and theater_ratio would increase, potentially reclassifying it as a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beta_duration_genuineness, empirical, 'Uncertainty regarding the good faith application of beta duration limits.').

omega_variable(
    kernel_reading_difference_expansive_shield,
    'How would the classification change if the ''expansive shield'' reading of the beta designation doctrine were adopted?',
    'Analysis of legal precedents and industry practices under an expansive interpretation.',
    'The ''expansive shield'' reading (beta = comprehensive liability waiver, indefinite duration) would significantly increase extractiveness and suppression, likely reclassifying the constraint as a Snare, with users as clear victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference_expansive_shield, conceptual, 'Impact of an alternative, broader interpretation of beta liability.').

omega_variable(
    kernel_reading_difference_severity_carve_out,
    'How would the classification change if the ''severity carve-out'' reading were adopted?',
    'Analysis of regulatory frameworks and legal outcomes in jurisdictions with such carve-outs.',
    'The ''severity carve-out'' reading (beta unavailable for critical systems) would reduce the scope of the constraint but likely maintain its Rope classification within its narrower domain, potentially increasing its coordination purity by removing high-risk applications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_difference_severity_carve_out, conceptual, 'Impact of an alternative reading that excludes critical systems from beta designation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t2000, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(beta_tr_t2008, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement(beta_tr_t2016, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2016, 0.09).
narrative_ontology:measurement(beta_tr_t2024, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(beta_be_t2000, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(beta_be_t2008, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement(beta_be_t2016, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2016, 0.24).
narrative_ontology:measurement(beta_be_t2024, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t2000, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(beta_su_t2008, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2008, 0.12).
narrative_ontology:measurement(beta_su_t2016, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2016, 0.14).
narrative_ontology:measurement(beta_su_t2024, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
