% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__management_compliance_narrative, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Commission Findings: Management Compliance Narrative
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This constraint represents the 'management compliance narrative' reading
 *   of the Rogers Commission findings. It interprets the findings as
 *   establishing a process where management can proceed with operations by
 *   demonstrating documented risk awareness and mitigation, rather than being
 *   subject to an absolute engineering veto. This reading prioritizes program
 *   continuity and management authority, while imposing a moderate
 *   bureaucratic burden on engineering teams. The constraint is claimed as a
 *   Rope, reflecting its coordination function in managing complex risks, but
 *   its metrics show a degree of extraction and suppression inherent in
 *   shifting authority and accountability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.45).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.6).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.45).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Findings: Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '6ff5f899-6162-4f1d-bcac-8b05a44afa41').
narrative_ontology:cs_kernel_codification('6ff5f899-6162-4f1d-bcac-8b05a44afa41', formalized).
narrative_ontology:cs_authority_grounding('6ff5f899-6162-4f1d-bcac-8b05a44afa41', lineage).
narrative_ontology:cs_interpretation_layer_present('6ff5f899-6162-4f1d-bcac-8b05a44afa41').
narrative_ontology:cs_reading_relation('6ff5f899-6162-4f1d-bcac-8b05a44afa41', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('6ff5f899-6162-4f1d-bcac-8b05a44afa41', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('6ff5f899-6162-4f1d-bcac-8b05a44afa41', foundational, risk_management_is_a_process).
narrative_ontology:cs_axiom_status(risk_management_is_a_process, holdable).
narrative_ontology:cs_axiom_grounding('6ff5f899-6162-4f1d-bcac-8b05a44afa41', risk_management_is_a_process, conventional).
narrative_ontology:cs_axiom('6ff5f899-6162-4f1d-bcac-8b05a44afa41', foundational, documented_mitigation_enables_proceeding).
narrative_ontology:cs_axiom_status(documented_mitigation_enables_proceeding, holdable).
narrative_ontology:cs_axiom_grounding('6ff5f899-6162-4f1d-bcac-8b05a44afa41', documented_mitigation_enables_proceeding, instrumental).
narrative_ontology:cs_reference_frame('6ff5f899-6162-4f1d-bcac-8b05a44afa41', post_rogers_compliance_framework).
narrative_ontology:cs_drift_state('6ff5f899-6162-4f1d-bcac-8b05a44afa41', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6ff5f899-6162-4f1d-bcac-8b05a44afa41', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_continuity).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, senior_management).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ultimate launch authority, but must demonstrate documented risk awareness and mitigation efforts. Benefits from maintaining program schedules and avoiding public accountability for failures if documentation is in place.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, senior_management, agenda_setter,
    institutional, biographical, constrained, national).

% Must produce extensive documentation of risk assessments and mitigation plans. Their technical veto power is reduced to an input into a compliance process, rather than an absolute threshold. Bears the cost of increased bureaucratic overhead.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_teams, payer,
    organized, biographical, constrained, national).

% The overall space program benefits from a process that allows operations to proceed, even with known risks, as long as they are documented and mitigated. This reading prioritizes the continuation of missions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_continuity, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__management_compliance_narrative, program_continuity).

% Oversees the compliance process, ensuring that management adheres to the documentation requirements. Can intervene if the process is not followed, but does not dictate absolute technical thresholds.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standardized process for managing and documenting known risks, allowing complex technical programs to proceed by coordinating risk communication and mitigation efforts across different organizational levels.
% TRANSFER_FUNCTION: Transfers the burden of proof for safety from an absolute engineering veto to a documented compliance process, shifting accountability for risk acceptance to management while enabling program continuation.
% ABSENT_VOICES: Engineers advocating for an absolute technical safety threshold, independent safety auditors with no organizational ties, and the public (who might demand higher safety margins) are not directly represented in this compliance-focused narrative.
% DISAPPEARANCE_RATIONALE: If this compliance narrative vanished, management would lose its primary justification for proceeding with operations despite known risks, leading to potential paralysis or a return to more stringent, engineering-driven safety protocols. Program schedules would be severely impacted.
% FOUNDING_PROBLEM: The Challenger disaster revealed a failure to adequately address known technical risks, leading to catastrophic loss and a crisis of public trust in NASA's safety culture.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission report itself, subsequent regulatory reforms, and ongoing public scrutiny of spaceflight safety corroborate the founding problem. While the specific O-ring issue was resolved, the underlying challenge of managing complex technical risks in high-stakes environments remains live.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).
:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the cost to engineering teams in documentation and the loss of absolute veto power, balanced against the benefit of program continuity. Suppression (0.6) is present as engineering concerns are channeled into a compliance framework, rather than allowing for outright halts. Theater ratio (0.2) is low, as the documentation process is genuinely intended to improve safety, though it can be gamed. Accessibility collapse (0.4) is moderate, as alternatives (e.g., an absolute engineering veto) are constrained but not entirely eliminated. Resistance (0.3) is moderate, as engineering teams may push back on documentation burdens but generally comply.
 *
 * PERSPECTIVAL GAP:
 *   From management's perspective, this is a necessary coordination mechanism for complex programs. From engineering's perspective, it can be seen as a dilution of safety authority, forcing them to 'document' risks rather than eliminate them. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior management and program continuity are beneficiaries, as this reading preserves management's authority and enables missions to proceed. Engineering teams are payers, bearing the cost of increased compliance and reduced direct authority. Regulatory bodies act as observers, ensuring process adherence. This structure allows management to navigate risks while maintaining operational tempo.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the compliance process as pure extraction by acknowledging its genuine coordination function in managing complex technical programs. However, the metrics and stakeholder analysis highlight the extractive elements where engineering's authority is diminished in favor of management's operational goals. The 'live' status of the founding problem suggests the mandate is still relevant, but the 'contested' status of its resolution points to ongoing tension in how the findings are applied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_vs_safety_efficacy,
    'Does documented risk awareness and mitigation truly lead to enhanced safety, or does it primarily serve as a liability shield for management?',
    'Longitudinal studies correlating compliance documentation levels with actual safety outcomes (e.g., incident rates, near-misses) across multiple programs and organizations.',
    'If documentation primarily serves as a liability shield, the constraint''s effective extractiveness and theater_ratio would be higher, reclassifying it closer to a Snare. If it genuinely enhances safety, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_vs_safety_efficacy, empirical, 'Assesses the true impact of compliance documentation on safety outcomes.').

omega_variable(
    engineering_authority_dilution,
    'To what extent has the compliance narrative diluted the effective authority of engineering teams to halt operations based on technical safety concerns?',
    'Analysis of internal decision-making records, incident reports, and expert testimony from engineers regarding their ability to enforce safety thresholds post-Rogers Commission.',
    'If engineering authority is substantially diluted, the suppression metric would be higher for engineering teams, and their exit_options might shift closer to ''trapped'' or ''identity_locked'', pushing the constraint towards a Tangled Rope or Snare from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_authority_dilution, empirical, 'Measures the shift in power dynamics between management and engineering regarding safety decisions.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''management compliance narrative'' reading genuinely distinct from the ''actuarial risk acceptance'' and ''engineering absolute threshold'' readings, or do they represent different facets of the same underlying commitment?',
    'Detailed comparative analysis of policy documents, organizational practices, and legal interpretations across different agencies and time periods, focusing on how each reading handles specific safety dilemmas.',
    'If the readings are found to be less distinct, it would suggest a single, more complex constraint rather than a kernel with multiple readings, potentially altering the classification to a more hybrid type. If they are truly distinct, the current decomposition is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Examines the conceptual boundaries between different interpretations of the Rogers Commission findings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1986, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(roge_tr_t1996, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(roge_tr_t2006, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(roge_tr_t2015, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(roge_tr_t2024, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(roge_be_t1986, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1986, 0.35).
narrative_ontology:measurement(roge_be_t1996, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1996, 0.4).
narrative_ontology:measurement(roge_be_t2006, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2006, 0.43).
narrative_ontology:measurement(roge_be_t2015, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(roge_be_t2024, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1986, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1986, 0.5).
narrative_ontology:measurement(roge_su_t1996, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1996, 0.55).
narrative_ontology:measurement(roge_su_t2006, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2006, 0.58).
narrative_ontology:measurement(roge_su_t2015, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(roge_su_t2024, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
