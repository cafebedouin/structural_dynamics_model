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
 *   This constraint instantiates the 'management compliance narrative'
 *   reading of the `rogers_commission_findings` kernel. This reading
 *   emphasizes establishing a documented process for risk awareness and
 *   mitigation, allowing operations to proceed with management retaining
 *   ultimate launch authority. Sibling readings include 'engineering absolute
 *   threshold' (flight operations must cease until O-ring redesign certified)
 *   and 'actuarial risk acceptance' (acceptable to fly if failure probability
 *   documented and accepted by informed decision-makers). The constraint is a
 *   Tangled Rope because it genuinely coordinates program continuity but also
 *   extracts engineering veto power through a compliance process that can
 *   become performative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.55).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.6).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.55).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Findings: Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '13a5d22f-ca7a-4e5c-a238-942889802606').
narrative_ontology:cs_kernel_codification('13a5d22f-ca7a-4e5c-a238-942889802606', formalized).
narrative_ontology:cs_authority_grounding('13a5d22f-ca7a-4e5c-a238-942889802606', lineage).
narrative_ontology:cs_interpretation_layer_present('13a5d22f-ca7a-4e5c-a238-942889802606').
narrative_ontology:cs_reading_relation('13a5d22f-ca7a-4e5c-a238-942889802606', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('13a5d22f-ca7a-4e5c-a238-942889802606', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('13a5d22f-ca7a-4e5c-a238-942889802606', foundational, management_retains_ultimate_authority).
narrative_ontology:cs_axiom_status(management_retains_ultimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('13a5d22f-ca7a-4e5c-a238-942889802606', management_retains_ultimate_authority, conventional).
narrative_ontology:cs_axiom('13a5d22f-ca7a-4e5c-a238-942889802606', foundational, documented_risk_mitigation_is_sufficient).
narrative_ontology:cs_axiom_status(documented_risk_mitigation_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('13a5d22f-ca7a-4e5c-a238-942889802606', documented_risk_mitigation_is_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('13a5d22f-ca7a-4e5c-a238-942889802606', post_challenger_accountability_framework).
narrative_ontology:cs_drift_state('13a5d22f-ca7a-4e5c-a238-942889802606', contemporary_operational_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13a5d22f-ca7a-4e5c-a238-942889802606', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, organizational_continuity).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_teams).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ultimate authority to proceed with operations, provided they can demonstrate documented risk awareness and mitigation efforts. Benefits from maintaining program schedules and operational control.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_management, agenda_setter,
    institutional, biographical, constrained, national).

% Must provide risk assessments and mitigation strategies, but their technical veto power is superseded by management's documented compliance. Bears the cost of reduced direct control over safety decisions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_teams, payer,
    powerful, biographical, constrained, national).

% Push for higher safety standards but must operate within a framework that allows management to proceed with documented risks. Bears the cost of a system that may prioritize process over absolute safety thresholds.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, safety_advocates, payer,
    moderate, generational, constrained, national).

% The ability of the organization to maintain its programs and operations without being halted by absolute engineering vetoes, provided a compliance process is followed.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, organizational_continuity, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__management_compliance_narrative, organizational_continuity).

% Oversee the organization's compliance with safety regulations and commission findings. Can impose penalties or mandate changes if the compliance process is deemed insufficient or fraudulent.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, documented process for identifying, assessing, and mitigating operational risks, allowing complex programs to proceed while addressing safety concerns and maintaining public trust.
% TRANSFER_FUNCTION: Transfers the ultimate authority to halt operations from engineering teams (based on technical thresholds) to program management (based on documented risk awareness and mitigation), shifting the burden of proof for safety from absolute prevention to managed risk.
% ABSENT_VOICES: Engineers advocating for an absolute safety threshold that would halt operations until all identified risks are eliminated or fully redesigned, rather than mitigated. Also, independent external safety experts who might challenge the sufficiency of internal documentation.
% DISAPPEARANCE_RATIONALE: If this compliance process vanished, organizations would either revert to a state where engineering vetoes could indefinitely halt programs, or management would proceed with insufficient risk documentation, leading to increased catastrophic failures and severe regulatory and public backlash. The balance of power and accountability would fundamentally shift.
% FOUNDING_PROBLEM: The Challenger disaster exposed a critical failure in organizational decision-making, where engineering warnings about O-ring integrity were overridden by management pressure to launch, leading to catastrophic loss of life and spacecraft. The problem was how to ensure safety warnings were adequately addressed without paralyzing operations.
% FOUNDING_PROBLEM_CORROBORATION: Subsequent accident investigations (e.g., Columbia disaster), ongoing regulatory oversight, and academic studies in organizational safety continue to corroborate that balancing operational imperatives with critical safety concerns, and ensuring effective communication between engineering and management, remains a persistent and live problem in high-stakes environments.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because while management gains flexibility, they must still invest in documentation and mitigation. Suppression is moderate (0.60) as it actively suppresses direct engineering vetoes but requires management to adhere to a process. Theater ratio is moderate (0.40) because the documentation can, over time, become more about satisfying the process than genuinely enhancing safety, especially under operational pressure. The metrics show a slight increase in extractiveness and suppression over time, suggesting a drift towards leveraging the process for operational expediency.
 *
 * PERSPECTIVAL GAP:
 *   From management's perspective, this constraint is a necessary and responsible coordination mechanism for complex operations. From the perspective of engineering teams and safety advocates, it can be seen as a mechanism that allows management to bypass critical safety concerns through bureaucratic means, potentially leading to a 'check-the-box' mentality rather than genuine risk reduction.
 *
 * DIRECTIONALITY LOGIC:
 *   Program management and organizational continuity are beneficiaries, as the constraint enables continued operations and maintains management's authority. Engineering teams and safety advocates are victims, as their ability to impose absolute safety halts is constrained by the compliance process. Regulatory bodies act as observers, evaluating the effectiveness and integrity of the process.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling pure extraction (management ignoring safety) as coordination by requiring documented risk awareness and mitigation. It also prevents mislabeling genuine coordination (structured risk assessment) as pure extraction by allowing operations to proceed under a defined framework. The omegas address the potential for the process to drift into mere theatrical compliance, which would indicate mandatrophy if the founding problem (ensuring safety) is no longer genuinely addressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    management_accountability_ambiguity,
    'Is the documented risk awareness and mitigation truly sufficient for accountability, or does it primarily serve as legal cover for management decisions?',
    'Analysis of post-incident investigations: if documented compliance consistently fails to prevent recurrence of known risks, it suggests a shift towards legal cover over genuine accountability.',
    'If primarily legal cover, the constraint''s effective extractiveness and theater ratio are higher, indicating a Snare where the coordination story is a facade for management''s operational freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_accountability_ambiguity, empirical, 'Whether documentation ensures genuine accountability or provides legal protection.').

omega_variable(
    process_vs_outcome_focus,
    'Does the compliance process prioritize documenting adherence to procedures over achieving actual safety outcomes?',
    'Comparison of compliance audit results with independent safety performance metrics (e.g., incident rates, near-miss reporting quality). Divergence suggests a process-over-outcome focus.',
    'If process-focused, the constraint''s coordination function is degraded, and its theater ratio increases, pushing it closer to a Piton or Snare, as the original mandate of safety is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(process_vs_outcome_focus, empirical, 'Focus on process adherence versus actual safety outcomes.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the `rogers_commission_findings` kernel, specifically the `management_compliance_narrative`. What would a sibling reading change structurally?',
    'Conceptual analysis of the alternative framings and their implications for decision-making authority and safety thresholds.',
    'If the ''engineering absolute threshold'' reading were adopted, engineering teams would gain veto power, extractiveness from them would decrease, and program management''s authority would be substantially constrained. If the ''actuarial risk acceptance'' reading were adopted, the focus would shift to quantifiable risk probabilities, potentially altering the nature of documentation and accountability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading of the Rogers Commission findings kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.25).
narrative_ontology:measurement(roge_tr_t5, rogers_commission_findings__management_compliance_narrative, theater_ratio, 5, 0.3).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__management_compliance_narrative, theater_ratio, 10, 0.35).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__management_compliance_narrative, theater_ratio, 15, 0.38).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.39).
narrative_ontology:measurement(roge_tr_t25, rogers_commission_findings__management_compliance_narrative, theater_ratio, 25, 0.4).
narrative_ontology:measurement(roge_tr_t30, rogers_commission_findings__management_compliance_narrative, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(roge_be_t5, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(roge_be_t25, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(roge_be_t30, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(roge_su_t5, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(roge_su_t25, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(roge_su_t30, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 30, 0.6).


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
