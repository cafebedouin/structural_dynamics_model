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
 *   designation doctrine, where 'beta' signifies a time-bounded testing phase
 *   with clear disclosure, and fundamental product liability is preserved. It
 *   aims to facilitate genuine software testing while preventing developers
 *   from using 'beta' as an indefinite shield against responsibility. This
 *   reading emphasizes good faith in testing and explicit communication with
 *   users, ensuring that the designation is not a mere formality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.2).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.1).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, rope).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '247ea44a-8fda-4e11-b5ac-e78e942c5538').
narrative_ontology:cs_kernel_codification('247ea44a-8fda-4e11-b5ac-e78e942c5538', formalized).
narrative_ontology:cs_authority_grounding('247ea44a-8fda-4e11-b5ac-e78e942c5538', lineage).
narrative_ontology:cs_interpretation_layer_present('247ea44a-8fda-4e11-b5ac-e78e942c5538').
narrative_ontology:cs_reading_relation('247ea44a-8fda-4e11-b5ac-e78e942c5538', beta_designation_doctrine__expansive_shield_reading, coexists_with).
narrative_ontology:cs_reading_relation('247ea44a-8fda-4e11-b5ac-e78e942c5538', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('247ea44a-8fda-4e11-b5ac-e78e942c5538', foundational, liability_preservation_principle).
narrative_ontology:cs_axiom_status(liability_preservation_principle, holdable).
narrative_ontology:cs_axiom_grounding('247ea44a-8fda-4e11-b5ac-e78e942c5538', liability_preservation_principle, deontological).
narrative_ontology:cs_axiom('247ea44a-8fda-4e11-b5ac-e78e942c5538', foundational, genuine_testing_requirement).
narrative_ontology:cs_axiom_status(genuine_testing_requirement, holdable).
narrative_ontology:cs_axiom_grounding('247ea44a-8fda-4e11-b5ac-e78e942c5538', genuine_testing_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('247ea44a-8fda-4e11-b5ac-e78e942c5538', balanced_innovation_consumer_protection).
narrative_ontology:cs_drift_state('247ea44a-8fda-4e11-b5ac-e78e942c5538', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('247ea44a-8fda-4e11-b5ac-e78e942c5538', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_testers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and release software in beta. This reading allows them to test products with reduced, but not eliminated, liability, provided they genuinely intend to test and disclose the beta status clearly. They benefit from early feedback and controlled risk.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, agenda_setter,
    organized, biographical, mobile, global).

% Voluntarily use beta software, understanding its experimental nature. They receive early access to new features and influence product development. This reading ensures they are adequately warned but retain basic consumer protections.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_testers, beneficiary,
    moderate, immediate, mobile, global).

% Interpret and enforce software liability laws. This reading aligns with their goal of balancing innovation with consumer protection, ensuring that 'beta' is not a loophole for avoiding responsibility.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Are indirectly affected by the quality and safety of software. This reading protects them by ensuring that even beta software has a baseline of accountability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, general_public, observer,
    powerless, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expectations and responsibilities between software developers and early users during a genuine product testing phase, allowing for iterative development while maintaining a baseline of consumer protection.
% TRANSFER_FUNCTION: Transfers a limited degree of liability risk from developers to beta testers, in exchange for early access and influence over product development, while preserving fundamental product liability.
% ABSENT_VOICES: Developers who wish for an expansive, indefinite liability shield for beta products are implicitly excluded; they would argue for minimal developer responsibility during any 'beta' phase, regardless of duration or disclosure quality.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, developers would face full liability for all pre-release software, likely leading to fewer public beta tests, slower innovation cycles, or a shift to private, non-public testing. Testers would either gain full liability protection (potentially at the cost of early access) or be exposed to greater undisclosed risks.
% FOUNDING_PROBLEM: The need to balance rapid software innovation and iterative development with consumer protection, particularly for experimental or incomplete products, without allowing 'beta' to become a permanent shield from accountability.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, consumer advocacy groups, and some forward-thinking industry associations corroborate that the problem of balancing innovation and liability for pre-release software remains live, and that clear boundaries for 'beta' are essential to prevent abuse. Court rulings and legislative debates also attest to this ongoing tension.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).

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
 *   Extractiveness is low (0.2) because the liability reduction is limited to the genuine testing phase and does not waive fundamental product liability. Suppression is also low (0.1) as it relies on clear disclosure and voluntary participation, not coercion. Theater ratio is minimal (0.05) because the core function of facilitating genuine testing is maintained, with little performative activity. The metrics reflect a well-functioning coordination mechanism with minimal extractive or suppressive elements, consistent with a Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   Developers seeking an expansive liability shield would experience this as a more restrictive constraint, while consumer advocates would see it as a necessary protection. This reading balances these perspectives by allowing for testing while maintaining accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers and beta testers are both beneficiaries, as they gain from a structured, lower-risk testing environment. Regulatory bodies act as agenda-setters, defining and enforcing the boundaries of this doctrine to ensure its integrity. There are no direct 'victims' in this reading, as the intent is to prevent harm through clear rules.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by ensuring the 'beta' designation remains tied to its original function of genuine testing. If the testing phase becomes indefinite or disclosure is inadequate, the constraint's integrity (and thus its Rope classification) would be challenged, potentially shifting towards a Tangled Rope or Snare if extraction or suppression increased without a clear coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beta_designation_scope_ambiguity,
    'Is the ''beta'' designation genuinely time-bounded and for testing purposes, or is it being used as an indefinite liability waiver?',
    'Judicial review of specific cases where ''beta'' status is invoked, focusing on the duration of the beta phase, the nature of disclosed risks, and evidence of ongoing testing and development.',
    'If ''beta'' is found to be indefinite or a de facto waiver, the constraint shifts towards the ''expansive_shield_reading'', increasing extractiveness and suppression, potentially reclassifying as a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beta_designation_scope_ambiguity, empirical, 'Ambiguity regarding the true purpose and duration of ''beta'' designation.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''narrow_warning_reading'' of the ''beta_designation_doctrine'' kernel. How would the classification change under the ''expansive_shield_reading'' or ''severity_carve_out_reading''?',
    'Analysis of hypothetical scenarios or actual legal precedents where alternative readings are applied, comparing their impact on extractiveness, suppression, and beneficiary/victim structures.',
    'The ''expansive_shield_reading'' would likely result in higher extractiveness and suppression, potentially leading to a Snare classification due to developers gaining broad immunity. The ''severity_carve_out_reading'' would introduce a Mountain-like element for critical systems, where beta designation is simply not applicable, reducing the scope of this constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative readings of the beta designation doctrine kernel on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t1995, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 1995, 0.03).
narrative_ontology:measurement(beta_tr_t2005, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2005, 0.04).
narrative_ontology:measurement(beta_tr_t2015, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2015, 0.04).
narrative_ontology:measurement(beta_tr_t2025, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(beta_be_t1995, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(beta_be_t2005, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2005, 0.18).
narrative_ontology:measurement(beta_be_t2015, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2015, 0.19).
narrative_ontology:measurement(beta_be_t2025, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2025, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t1995, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 1995, 0.08).
narrative_ontology:measurement(beta_su_t2005, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2005, 0.09).
narrative_ontology:measurement(beta_su_t2015, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2015, 0.09).
narrative_ontology:measurement(beta_su_t2025, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2025, 0.1).


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
