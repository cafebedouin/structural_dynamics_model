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
 *   This constraint represents one reading of the Rogers Commission findings,
 *   focusing on the establishment of a compliance process. It mandates that
 *   management must demonstrate documented risk awareness and mitigation
 *   efforts to proceed with operations. This reading allows for program
 *   continuity while shifting the burden of proof for safety from an absolute
 *   engineering veto to a management-led narrative of process adherence. The
 *   constraint is claimed as a Rope, reflecting its coordination function in
 *   managing complex risks, but its metrics show moderate extractiveness and
 *   suppression, indicating the costs borne by engineering teams and the
 *   active enforcement required to maintain this balance.
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
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, 'f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8').
narrative_ontology:cs_kernel_codification('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', formalized).
narrative_ontology:cs_authority_grounding('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', lineage).
narrative_ontology:cs_interpretation_layer_present('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8').
narrative_ontology:cs_reading_relation('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', foundational, process_adherence_ensures_accountability).
narrative_ontology:cs_axiom_status(process_adherence_ensures_accountability, holdable).
narrative_ontology:cs_axiom_grounding('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', process_adherence_ensures_accountability, conventional).
narrative_ontology:cs_axiom('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', foundational, management_retains_ultimate_authority).
narrative_ontology:cs_axiom_status(management_retains_ultimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', management_retains_ultimate_authority, conventional).
narrative_ontology:cs_reference_frame('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', post_rogers_commission_process_governance).
narrative_ontology:cs_drift_state('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', contemporary_regulatory_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f3a0e082-1e37-43b9-bc9e-0bfdfe949ff8', '').
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

% Required to produce extensive documentation of risk assessments and mitigation plans. Their technical veto power is reduced to an input into a compliance process, rather than an absolute threshold. Bears the cost of increased bureaucratic overhead.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_teams, payer,
    organized, biographical, constrained, national).

% The overall space program benefits from a process that allows operations to continue, provided risks are acknowledged and documented, rather than halting for absolute technical thresholds. This ensures missions proceed, albeit with managed risk.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_continuity, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__management_compliance_narrative, program_continuity).

% Oversees the compliance process, ensuring that management adheres to the documentation requirements. Their role is to verify the process, not necessarily to second-guess the technical risk assessments themselves.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the decision-making process for high-stakes operations by requiring a formal, documented approach to risk identification and mitigation, ensuring all parties acknowledge known risks before proceeding.
% TRANSFER_FUNCTION: Transfers the burden of proof for safety from an absolute engineering veto to a management-led compliance narrative, shifting accountability from technical certainty to documented process adherence. This transfers some decision-making power from engineers to managers.
% ABSENT_VOICES: The 'absolute safety' perspective, which would demand operations cease until all known technical flaws are resolved, is marginalized. This perspective would argue that documentation is not a substitute for fundamental safety redesign.
% DISAPPEARANCE_RATIONALE: If this compliance narrative vanished, management would lose a key justification for proceeding with operations despite known risks. Decision-making would likely revert to either an absolute engineering veto or a purely political calculus, fundamentally altering how high-stakes programs are run.
% FOUNDING_PROBLEM: The Challenger disaster highlighted a failure to adequately address known technical risks, leading to catastrophic loss. The problem was a lack of formal process for acknowledging and mitigating risks in high-stakes decision-making.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission's findings and subsequent regulatory reforms corroborate the existence and persistence of this problem. While processes are in place, the tension between operational pressure and safety remains a live issue, attested by ongoing safety reviews and incident investigations across high-risk industries.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) reflects the cost to engineering teams in terms of increased documentation and reduced direct veto power, while management gains the ability to proceed. Suppression (0.6) is necessary to ensure compliance with the documentation process and to prevent engineering concerns from unilaterally halting operations. The theater ratio (0.2) is low, as the documentation process serves a genuine function, though it can be gamed. Accessibility collapse is moderate (0.4) as alternatives (e.g., absolute engineering veto, purely political decisions) are constrained but not entirely eliminated. Resistance (0.3) comes from engineering teams pushing for more substantive safety measures beyond mere documentation.
 *
 * PERSPECTIVAL GAP:
 *   Management views this as a necessary coordination mechanism for complex programs, ensuring accountability and progress. Engineering teams, however, may perceive it as a bureaucratic hurdle that dilutes their technical authority and shifts responsibility without necessarily enhancing safety. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior management and program continuity are beneficiaries, as this process allows operations to continue with a documented rationale, protecting management from direct blame in case of failure. Engineering teams are payers, bearing the cost of extensive documentation and losing some direct authority over go/no-go decisions. Regulatory bodies act as observers, verifying the process rather than directly participating in the risk assessment.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring documented risk awareness) remains live, as the problem of managing complex risks in high-stakes operations persists. The classification as a Rope acknowledges its genuine coordination function, preventing it from being mislabeled as pure extraction, while the metrics capture the costs and enforcement required to maintain this coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_vs_actual_safety,
    'Does documented risk awareness and mitigation truly lead to safer operations, or does it primarily serve as a liability shield for management?',
    'Longitudinal studies comparing safety outcomes in organizations with robust compliance narratives versus those with more engineering-driven safety cultures, controlling for other variables.',
    'If documentation is primarily a liability shield, the constraint''s effective extractiveness from engineering teams is higher, and its coordination function is weaker, potentially reclassifying it as a Tangled Rope or Snare from the engineering seat. If it genuinely enhances safety, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_vs_actual_safety, empirical, 'The effectiveness of compliance narratives in improving actual safety outcomes.').

omega_variable(
    engineering_authority_erosion,
    'To what extent has the ''management compliance narrative'' reading eroded the traditional authority of engineering teams to halt operations based on technical concerns?',
    'Analysis of incident reports and decision-making processes over time, specifically looking for instances where engineering concerns were overridden by documented compliance, and the subsequent outcomes.',
    'If engineering authority has been substantially eroded, the suppression metric for engineering teams is effectively higher, and their exit options are more constrained, pushing their seat classification towards Snare. If their input remains genuinely influential, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_authority_erosion, empirical, 'The impact of compliance narratives on engineering''s decision-making authority.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''management compliance narrative'' reading of the Rogers Commission findings genuinely compatible with the ''engineering absolute threshold'' and ''actuarial risk acceptance'' readings within a single coherent safety framework?',
    'Conceptual analysis of the underlying normative principles of each reading, and empirical observation of how organizations attempt to integrate or prioritize these different interpretations in practice.',
    'If the readings are fundamentally incompatible, this constraint''s persistence relies on suppressing alternative interpretations, increasing its effective suppression. If they can be integrated, the constraint functions as a more robust coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'The compatibility of different interpretations of the Rogers Commission findings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.1).
narrative_ontology:measurement(roge_tr_t5, rogers_commission_findings__management_compliance_narrative, theater_ratio, 5, 0.13).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__management_compliance_narrative, theater_ratio, 10, 0.16).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__management_compliance_narrative, theater_ratio, 15, 0.18).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(roge_be_t5, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(roge_su_t5, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 20, 0.6).


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
