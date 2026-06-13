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
    narrative_ontology:affects_constraint/2,
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
 *   of the Rogers Commission findings, which established a process-oriented
 *   approach to risk management. Under this reading, the findings mandate
 *   documented risk awareness and mitigation efforts as sufficient to proceed
 *   with operations, rather than imposing absolute technical thresholds. It
 *   allows management to retain launch authority as long as the compliance
 *   narrative is maintained.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.4).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.6).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.4).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Findings: Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, 'e6b72bdd-4d20-46ad-b16e-491a7b0b0470').
narrative_ontology:cs_kernel_codification('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', formalized).
narrative_ontology:cs_authority_grounding('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', lineage).
narrative_ontology:cs_interpretation_layer_present('e6b72bdd-4d20-46ad-b16e-491a7b0b0470').
narrative_ontology:cs_reading_relation('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', foundational, documented_process_is_sufficient_for_safety).
narrative_ontology:cs_axiom_status(documented_process_is_sufficient_for_safety, holdable).
narrative_ontology:cs_axiom_grounding('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', documented_process_is_sufficient_for_safety, conventional).
narrative_ontology:cs_axiom('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', foundational, management_retains_ultimate_launch_authority).
narrative_ontology:cs_axiom_status(management_retains_ultimate_launch_authority, holdable).
narrative_ontology:cs_axiom_grounding('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', management_retains_ultimate_launch_authority, conventional).
narrative_ontology:cs_reference_frame('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', post_challenger_procedural_compliance).
narrative_ontology:cs_drift_state('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('e6b72bdd-4d20-46ad-b16e-491a7b0b0470', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_continuity).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, senior_management).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_veto_power).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, safety_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ultimate launch authority, provided they can demonstrate documented risk awareness and mitigation efforts. This reading allows them to proceed with missions even with known risks, as long as the process is followed.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, senior_management, agenda_setter,
    institutional, biographical, constrained, national).

% Are required to document risks and mitigation efforts, but their technical judgment on absolute safety thresholds can be overridden by management if the compliance process is followed. This diminishes their effective veto power.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_teams, payer,
    organized, generational, constrained, national).

% Push for higher safety standards and absolute thresholds, but under this reading, their concerns are channeled into a documentation process rather than necessarily halting operations. They bear the cost of potential residual risk.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, safety_advocates, payer,
    moderate, generational, constrained, national).

% The mission and operational schedule can proceed without being halted by absolute technical thresholds, as long as the procedural requirements for risk management are met. This ensures the program's ongoing existence and funding.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_continuity, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__management_compliance_narrative, program_continuity).

% Oversee the implementation of the Rogers Commission findings, ensuring that organizations establish and follow documented risk management processes. They can enforce compliance with the process, but not necessarily with specific risk thresholds.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the decision-making process for high-stakes operations by requiring a documented, auditable trail of risk identification, assessment, and mitigation, allowing diverse technical and managerial perspectives to be integrated into a single go/no-go decision.
% TRANSFER_FUNCTION: Transfers the authority to halt operations from engineering judgment (based on absolute technical thresholds) to senior management (based on documented compliance with risk management procedures).
% ABSENT_VOICES: Engineers advocating for an absolute technical safety threshold (e.g., 'no flight until O-ring redesign') are present in the documentation process but their 'veto' is effectively absent from the final decision-making power. The voice of 'absolute safety' is heard but not necessarily heeded.
% DISAPPEARANCE_RATIONALE: If this compliance narrative vanished, organizations would either revert to pre-Rogers ad-hoc decision-making (increasing risk) or adopt more stringent, absolute safety thresholds (halting operations more frequently). The current balance of managerial authority and documented risk would be lost.
% FOUNDING_PROBLEM: The Challenger disaster revealed a failure to adequately address known engineering risks, leading to catastrophic loss. The problem was a lack of a robust, documented process for integrating engineering warnings into launch decisions.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and independent safety boards continue to corroborate the need for robust risk management processes, citing ongoing incidents where process failures contribute to accidents. While the specific O-ring issue was resolved, the underlying problem of managing complex technical risks remains live.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).

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
 *   The extractiveness (0.4) is moderate, as it extracts some veto power from engineering but does not completely suppress all risk discussion. Suppression (0.6) is higher because it actively channels and limits the scope of engineering dissent, requiring it to fit within a 'documented process' framework. The theater ratio (0.5) is significant, reflecting that the 'documentation' can sometimes become an end in itself, a performance of compliance that may not always translate to genuine risk reduction, especially when management is incentivized for program continuity. Accessibility collapse is moderate (0.4) as alternatives (like an absolute engineering veto) are constrained but not entirely eliminated; resistance (0.3) is present from safety advocates but often absorbed by the process.
 *
 * PERSPECTIVAL GAP:
 *   Senior management perceives this as a necessary coordination mechanism for complex operations, balancing risk and mission. Engineering teams and safety advocates, however, experience it as a mechanism that extracts their authority and potentially compromises safety by prioritizing process over absolute technical limits. The engine will compute this divergence from the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior management and program continuity are beneficiaries (d near 0.0-0.2) as this reading preserves their authority and mission schedule. Engineering teams and safety advocates are payers (d near 0.7-0.9) as their ability to halt operations is diminished, and their concerns are proceduralized. Regulatory bodies are observers (d near 0.5) enforcing the process, not necessarily the outcome.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    process_vs_outcome_efficacy,
    'Does documented compliance with risk awareness and mitigation efforts reliably lead to actual risk reduction, or does it primarily serve as a legal/managerial shield?',
    'Longitudinal studies correlating compliance audit scores with actual safety outcomes and incident rates, controlling for other factors.',
    'If compliance is found to be largely performative (low correlation with safety), the constraint''s effective extractiveness and theater ratio would be higher, reclassifying it closer to a Snare or Piton. If highly correlated, it would reinforce the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(process_vs_outcome_efficacy, empirical, 'The efficacy of process-based compliance in achieving safety outcomes.').

omega_variable(
    management_authority_vs_engineering_veto,
    'Is the retention of ultimate launch authority by management, even with documented risks, a necessary function of organizational leadership or an undue extraction of engineering''s safety mandate?',
    'Comparative analysis of high-reliability organizations (HROs) with different authority structures, examining their safety records and decision-making processes.',
    'If HROs with stronger engineering veto power show superior safety records without undue mission paralysis, this reading''s extractiveness would be re-evaluated upward. If management authority is shown to be critical for HRO function, the current extractiveness would be seen as a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_authority_vs_engineering_veto, conceptual, 'The appropriate balance of authority between management and engineering in safety-critical domains.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''management compliance narrative'' reading of the Rogers findings the most accurate interpretation, or do the ''engineering absolute threshold'' or ''actuarial risk acceptance'' readings better capture the commission''s intent and structural implications?',
    'Historical analysis of the commission''s full report, subsequent legislative intent, and expert testimony, weighed against the observed operational outcomes and organizational culture shifts.',
    'If an alternative reading is found to be more structurally accurate, this constraint would be reclassified, likely to a Mountain (for engineering threshold) or a Rope (for actuarial risk acceptance), with different beneficiary/victim structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Ambiguity in the authoritative interpretation of the Rogers Commission findings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1986, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(roge_tr_t1996, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1996, 0.35).
narrative_ontology:measurement(roge_tr_t2006, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2006, 0.5).
narrative_ontology:measurement(roge_tr_t2016, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2016, 0.5).
narrative_ontology:measurement(roge_tr_t2024, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2024, 0.5).

% Extraction over time
narrative_ontology:measurement(roge_be_t1986, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1986, 0.3).
narrative_ontology:measurement(roge_be_t1996, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1996, 0.35).
narrative_ontology:measurement(roge_be_t2006, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2006, 0.4).
narrative_ontology:measurement(roge_be_t2016, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2016, 0.4).
narrative_ontology:measurement(roge_be_t2024, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1986, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1986, 0.5).
narrative_ontology:measurement(roge_su_t1996, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1996, 0.55).
narrative_ontology:measurement(roge_su_t2006, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2006, 0.6).
narrative_ontology:measurement(roge_su_t2016, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(roge_su_t2024, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Rogers Commission findings, each with different structural implications and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
