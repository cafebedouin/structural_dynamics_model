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
 *   designation doctrine, which holds that beta status is a time-bounded
 *   disclosure for genuine testing, preserving base product liability. It
 *   aims to balance software innovation with consumer protection. This
 *   reading is one interpretation of the broader 'beta_designation_doctrine'
 *   kernel, which is contested by other readings that seek either more
 *   expansive liability waivers or categorical exclusions for critical
 *   systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.15).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.1).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, rope).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, 'be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4').
narrative_ontology:cs_kernel_codification('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', formalized).
narrative_ontology:cs_authority_grounding('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', lineage).
narrative_ontology:cs_interpretation_layer_present('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4').
narrative_ontology:cs_reading_relation('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', foundational, product_liability_is_fundamental).
narrative_ontology:cs_axiom_status(product_liability_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', product_liability_is_fundamental, deontological).
narrative_ontology:cs_axiom('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', foundational, beta_is_temporary_testing).
narrative_ontology:cs_axiom_status(beta_is_temporary_testing, holdable).
narrative_ontology:cs_axiom_grounding('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', beta_is_temporary_testing, conventional).
narrative_ontology:cs_reference_frame('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', good_faith_testing_disclosure).
narrative_ontology:cs_drift_state('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('be1be9f9-c87e-4b7a-a0a0-190bd8b86eb4', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_testers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, injured_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to conduct public testing and gather feedback on pre-release software with a clear understanding of their liability. They must genuinely be in a testing phase and disclose the beta status clearly. Their alternative is private testing or full release with immediate full liability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, agenda_setter,
    organized, biographical, constrained, global).

% Gain early access to new software and contribute to its development, while being clearly informed of the experimental nature. Crucially, their fundamental product liability rights are preserved for core defects, even during beta. They can choose not to participate.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_testers, beneficiary,
    moderate, immediate, mobile, global).

% Enforce the boundaries of beta designation, ensuring that developers adhere to the requirements of genuine testing, clear disclosure, and preserved base liability. They act to protect consumers from misapplication of the doctrine.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Interpret and apply the beta designation doctrine, adjudicating disputes between developers and users. They are responsible for upholding the principles of time-bounded testing, genuine disclosure, and preserved base product liability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% If a beta product causes harm due to a core defect, this reading ensures they can still pursue product liability claims, as the beta designation does not waive fundamental rights. They bear the initial harm but are not victimized by the doctrine itself.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, injured_users, payer,
    powerless, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinates the expectations and responsibilities between software developers and early adopters, allowing for public testing and feedback while maintaining a baseline of consumer protection.
% TRANSFER_FUNCTION: It temporarily transfers some risk of minor, expected bugs from developers to beta testers during a genuine testing phase, but ensures that fundamental product liability for core defects remains with the developer.
% ABSENT_VOICES: Developers seeking an expansive liability shield for all beta software, and users demanding zero risk for any beta product, are not fully represented in this balanced interpretation.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, developers would likely become much more cautious about public beta programs, fearing full liability for every minor bug, or would seek more restrictive legal waivers. The software development cycle, especially for innovative products, would be significantly impacted.
% FOUNDING_PROBLEM: The problem was how to foster software innovation and allow for real-world testing with user feedback, without either exposing developers to excessive liability for early-stage imperfections or leaving consumers entirely unprotected from potentially harmful, untested products.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars specializing in technology law, consumer advocacy groups, and some industry bodies (particularly those focused on open-source or rapid iteration) corroborate the ongoing need for a balanced approach to beta software liability.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.15) reflects that this reading primarily functions as a coordination mechanism for testing, not a means to extract value by waiving liability. Suppression (0.10) is minimal because users retain core rights. Theater ratio (0.05) is low, as the doctrine requires genuine testing, not a perpetual 'beta' label to avoid responsibility. The claimed type is 'rope' because it solves a genuine collective action problem (public testing) with minimal coercive overhead and net benefits for participants, without suppressing alternatives (users can still pursue claims).
 *
 * PERSPECTIVAL GAP:
 *   Under this reading, all parties generally benefit from the clarity and balance it provides. The primary perspectival gap exists between this reading and its siblings, where other interpretations would shift the balance of risk and benefit significantly.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers are beneficiaries as they gain a structured way to test. Beta testers are also beneficiaries, getting early access with preserved rights. Consumer protection agencies and courts act as agenda-setters, ensuring the doctrine's proper application. Injured users, while bearing initial harm, are not victims of the doctrine itself under this reading, as their base liability is preserved.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by insisting on 'genuine testing phase' and 'base product liability preserved.' If these conditions are not met, the constraint would drift towards a Snare (if developers exploit the 'beta' label for extraction) or a Piton (if the testing function atrophies but the label persists theatrically). This reading's strict interpretation is a bulwark against such drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_testing_definition,
    'What constitutes a ''genuine testing phase'' in practice, and how is its duration objectively determined?',
    'Judicial precedent establishing clear criteria for testing phase commencement and termination, or industry standards adopted by regulatory bodies.',
    'If ''genuine testing'' is ill-defined, developers could perpetually label products as beta, shifting this reading towards a Tangled Rope or Snare by extending the liability waiver beyond its intended scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_definition, empirical, 'Ambiguity in defining the scope and duration of a legitimate beta testing phase.').

omega_variable(
    base_liability_scope,
    'What specific types of product liability are ''preserved'' under this reading, and what constitutes a ''core defect'' versus an ''expected beta bug''?',
    'Further legal clarification through case law or legislative action, providing specific examples and thresholds for different types of software and potential harms.',
    'If the scope of preserved liability is unclear, developers might interpret it narrowly, effectively creating a de facto liability shield and shifting the constraint towards a Snare for injured users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(base_liability_scope, conceptual, 'Ambiguity in the scope of preserved product liability during beta testing.').

omega_variable(
    reading_contest_resolution,
    'Which reading of the beta designation doctrine (narrow warning, expansive shield, or severity carve-out) will ultimately prevail in legal and regulatory practice?',
    'Landmark court decisions, legislative reforms, or widespread industry adoption of one interpretation over others.',
    'If the ''expansive_shield_reading'' prevails, this constraint would be reclassified as a Snare. If the ''severity_carve_out_reading'' prevails, this reading''s applicability would be significantly limited, potentially leading to a new, more restrictive Rope or Scaffold for critical systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_resolution, preference, 'Uncertainty regarding the dominant legal interpretation of beta designation doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t2000, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(beta_tr_t2005, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2005, 0.04).
narrative_ontology:measurement(beta_tr_t2010, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(beta_tr_t2015, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2015, 0.05).
narrative_ontology:measurement(beta_tr_t2020, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2020, 0.05).
narrative_ontology:measurement(beta_tr_t2025, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(beta_be_t2000, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(beta_be_t2005, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2005, 0.13).
narrative_ontology:measurement(beta_be_t2010, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(beta_be_t2015, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(beta_be_t2020, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(beta_be_t2025, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t2000, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(beta_su_t2005, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2005, 0.09).
narrative_ontology:measurement(beta_su_t2010, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(beta_su_t2015, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2015, 0.1).
narrative_ontology:measurement(beta_su_t2020, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2020, 0.1).
narrative_ontology:measurement(beta_su_t2025, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is the 'narrow_warning_reading' of the 'beta_designation_doctrine' kernel. It is structurally distinct from the 'expansive_shield_reading' and 'severity_carve_out_reading' due to differing ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
