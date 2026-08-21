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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Rogers Commission Management Compliance Narrative
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This constraint represents one reading of the Rogers Commission findings,
 *   emphasizing a management-led compliance process where documented risk
 *   awareness and mitigation are sufficient to proceed with operations. It
 *   contrasts with readings that prioritize absolute technical thresholds or
 *   purely actuarial risk acceptance. The constraint coordinates management
 *   decision-making by providing a structured, defensible path, but it
 *   extracts the absolute veto power of engineering, making it a Tangled
 *   Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.45).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.55).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.45).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '02a09765-e715-4f2c-a10c-57b9e8165117').
narrative_ontology:cs_kernel_codification('02a09765-e715-4f2c-a10c-57b9e8165117', formalized).
narrative_ontology:cs_authority_grounding('02a09765-e715-4f2c-a10c-57b9e8165117', lineage).
narrative_ontology:cs_interpretation_layer_present('02a09765-e715-4f2c-a10c-57b9e8165117').
narrative_ontology:cs_reading_relation('02a09765-e715-4f2c-a10c-57b9e8165117', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('02a09765-e715-4f2c-a10c-57b9e8165117', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('02a09765-e715-4f2c-a10c-57b9e8165117', foundational, management_retains_final_authority).
narrative_ontology:cs_axiom_status(management_retains_final_authority, holdable).
narrative_ontology:cs_axiom_grounding('02a09765-e715-4f2c-a10c-57b9e8165117', management_retains_final_authority, conventional).
narrative_ontology:cs_axiom('02a09765-e715-4f2c-a10c-57b9e8165117', foundational, documented_process_is_sufficient_for_safety).
narrative_ontology:cs_axiom_status(documented_process_is_sufficient_for_safety, holdable).
narrative_ontology:cs_axiom_grounding('02a09765-e715-4f2c-a10c-57b9e8165117', documented_process_is_sufficient_for_safety, conventional).
narrative_ontology:cs_reference_frame('02a09765-e715-4f2c-a10c-57b9e8165117', management_prerogative_with_oversight).
narrative_ontology:cs_drift_state('02a09765-e715-4f2c-a10c-57b9e8165117', contemporary_organizational_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('02a09765-e715-4f2c-a10c-57b9e8165117', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, management_leadership).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_continuity).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_safety_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ultimate authority to proceed with programs, provided a documented compliance process is followed. Benefits from a clear, defensible path to decision-making and maintaining program momentum.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, management_leadership, agenda_setter,
    institutional, biographical, constrained, national).

% Must document risks and mitigation efforts, but lose the ability to impose an absolute veto on operations based purely on technical thresholds. Their concerns are integrated into a process that management ultimately controls.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_safety_advocates, payer,
    organized, biographical, constrained, national).

% Benefit from a structured framework that allows them to navigate complex safety concerns and proceed with projects, provided they adhere to the documentation requirements. This reduces ambiguity in decision-making.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_managers, beneficiary,
    powerful, biographical, constrained, national).

% Oversee the implementation and effectiveness of compliance processes, ensuring organizations adhere to the spirit and letter of safety regulations. They can audit documentation and challenge decisions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% Represent the broader public interest in safety, often advocating for stricter, more transparent safety measures. Their direct influence on internal compliance processes is limited, often channeled through regulatory bodies or media pressure.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, public_safety_advocates, excluded,
    organized, generational, constrained, national).

% The ongoing existence and progress of high-stakes technological programs, which benefits from a clear, documented path for risk management that avoids paralysis or indefinite delays.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_continuity, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__management_compliance_narrative, program_continuity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, management_leadership).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standardized, documented process for identifying, assessing, and mitigating risks, allowing management to make informed decisions and proceed with complex, high-stakes programs while demonstrating due diligence.
% TRANSFER_FUNCTION: Transfers the locus of final decision-making authority from an absolute engineering veto to a management-led compliance process, requiring documented rationale for proceeding with identified risks.
% ABSENT_VOICES: Public safety advocates and proponents of absolute safety thresholds are largely excluded from the direct internal compliance process, their concerns typically mediated through regulatory oversight or external critique.
% DISAPPEARANCE_RATIONALE: If this compliance process vanished, organizations would revert to less structured, potentially more arbitrary or paralyzed decision-making regarding high-risk programs. The institutional memory of the Challenger disaster would still exist, but the formal mechanism for addressing its lessons would be gone, leading to a reorganization of safety governance.
% FOUNDING_PROBLEM: The Challenger disaster revealed a critical failure in organizational decision-making where engineering concerns about safety were overridden without adequate documentation, mitigation, or a clear process for management accountability.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety boards, regulatory agencies, and academic studies of organizational accidents continue to corroborate the ongoing challenge of balancing innovation with safety, validating the need for robust risk management processes, even if the specific implementation is debated.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.45) is moderate, reflecting the cost to engineering of losing absolute veto power and the benefit to management of retaining authority. Suppression (0.55) is moderate, as engineering concerns are not silenced but are channeled and ultimately subject to management's documented decision. The theater ratio (0.40) reflects the risk that documentation can become a performative exercise rather than a genuine driver of safety. The metrics show a slight increase in extractiveness and suppression as the process becomes more entrenched, but then stabilize.
 *
 * PERSPECTIVAL GAP:
 *   From management's perspective, this is a necessary coordination mechanism for complex operations. From engineering's perspective, it can be seen as a mechanism that dilutes their safety authority. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Management leadership and program continuity are beneficiaries, as the process enables them to proceed with programs while demonstrating due diligence. Engineering safety advocates are payers, as their ability to halt operations based on unmitigated technical risk is curtailed. Regulatory bodies and public safety advocates act as observers or excluded voices, influencing the process externally rather than directly controlling it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    process_vs_outcome_effectiveness,
    'Does adherence to the documented compliance process genuinely lead to safer outcomes, or does it primarily serve as a liability shield for management?',
    'Longitudinal studies comparing safety incident rates in organizations strictly adhering to the process versus those with alternative safety governance models, controlling for other factors.',
    'If the process is found to be primarily performative (liability shield), the constraint''s effective extractiveness and theater ratio would be higher, potentially reclassifying it closer to a Snare or Piton. If genuinely effective, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(process_vs_outcome_effectiveness, empirical, 'Assesses whether the compliance process''s function is substantive safety improvement or merely formal justification.').

omega_variable(
    management_authority_legitimacy,
    'Is management''s retained launch authority, even with documented compliance, truly legitimate in the face of unresolved engineering concerns, or does it represent an overreach of authority?',
    'Analysis of organizational culture and decision-making dynamics in post-accident investigations, particularly where documented risks were accepted but failures still occurred. This would involve examining the power dynamics and ethical frameworks at play.',
    'If deemed an overreach, the constraint''s suppression and extractiveness would be viewed as more severe, amplifying the ''victim'' status of engineering and potentially shifting the classification towards a Snare. If legitimate, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_authority_legitimacy, conceptual, 'Examines the ethical and structural legitimacy of management''s final decision-making power within the compliance framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of engineering''s absolute veto structural (due to management''s formal authority) or internalized (engineers accepting the process as the new norm)?',
    'Post-exit suppression trajectory: if engineers continue to self-censor or frame concerns within the compliance narrative even when formal management pressure is absent, it suggests internalized suppression. Surveys and interviews with engineers in different organizational contexts could also provide insight.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would amplify the extractive nature of the constraint from the engineering seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism regarding engineering''s safety advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.3).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__management_compliance_narrative, theater_ratio, 10, 0.35).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__management_compliance_narrative, theater_ratio, 20, 0.4).
narrative_ontology:measurement(roge_tr_t30, rogers_commission_findings__management_compliance_narrative, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(roge_be_t30, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(roge_su_t30, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 30, 0.55).


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
