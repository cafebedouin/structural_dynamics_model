% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__engineering_absolute_threshold, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission Engineering Absolute Threshold
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission findings established a technical safety boundary in
 *   the wake of the Challenger disaster: shuttle flight operations were to
 *   cease entirely until the solid rocket booster O-ring seal was physically
 *   redesigned and the redesign certified by engineering review. This reading
 *   treats the Commission's recommendation as an absolute engineering
 *   thresholdâoperations stop until the hardware is proven safeânot as a
 *   risk-quantification exercise or a compliance-documentation threshold. The
 *   constraint transferred veto authority over Flight Readiness Reviews from
 *   program management to engineering safety panels and imposed a clear
 *   sunset condition: the constraint expires when the O-ring redesign is
 *   certified.
 *
 * KEY AGENTS:
 *   - flight_crew: Primary beneficiary (moderate/constrained) â lives protected by the engineering veto
 *   - engineering_safety_panel: Agenda setter (institutional/analytical) â holds veto authority over launches
 *   - program_management: Primary payer (powerful/constrained) â bears schedule, budget, and political costs of halted operations
 *   - contractor_workforce: Secondary payer (moderate/constrained) â absorbs employment instability from launch delays
 *   - oversight_authority: Analytical observer (institutional/analytical) â authored the findings and monitors compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.72).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.85).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.72).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, scaffold).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Engineering Absolute Threshold").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:has_sunset_clause(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, 'a8765af4-426c-4ff1-9bce-3849dc522558').
narrative_ontology:cs_kernel_codification('a8765af4-426c-4ff1-9bce-3849dc522558', fixed_text).
narrative_ontology:cs_authority_grounding('a8765af4-426c-4ff1-9bce-3849dc522558', lineage).
narrative_ontology:cs_interpretation_layer_present('a8765af4-426c-4ff1-9bce-3849dc522558').
narrative_ontology:cs_reading_relation('a8765af4-426c-4ff1-9bce-3849dc522558', rogers_commission_findings__management_compliance_narrative, forecloses).
narrative_ontology:cs_reading_relation('a8765af4-426c-4ff1-9bce-3849dc522558', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_axiom('a8765af4-426c-4ff1-9bce-3849dc522558', foundational, technical_flaw_requires_physical_remediation_before_operations).
narrative_ontology:cs_axiom_status(technical_flaw_requires_physical_remediation_before_operations, holdable).
narrative_ontology:cs_axiom_grounding('a8765af4-426c-4ff1-9bce-3849dc522558', technical_flaw_requires_physical_remediation_before_operations, conventional).
narrative_ontology:cs_axiom('a8765af4-426c-4ff1-9bce-3849dc522558', foundational, engineering_veto_overrides_schedule_authority).
narrative_ontology:cs_axiom_status(engineering_veto_overrides_schedule_authority, holdable).
narrative_ontology:cs_axiom_grounding('a8765af4-426c-4ff1-9bce-3849dc522558', engineering_veto_overrides_schedule_authority, conventional).
narrative_ontology:cs_reference_frame('a8765af4-426c-4ff1-9bce-3849dc522558', engineering_supremacy_in_safety_decisions).
narrative_ontology:cs_drift_state('a8765af4-426c-4ff1-9bce-3849dc522558', post_return_to_flight_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a8765af4-426c-4ff1-9bce-3849dc522558', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, program_management).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, contractor_workforce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Astronauts assigned to shuttle missions who depend on the engineering safety veto to prevent launch of vehicles with uncertified critical hardware; they cannot individually exit the flight assignment pool without ending their careers, and they have no alternative human spaceflight employer.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crew, beneficiary,
    moderate, biographical, constrained, national).

% Post-Rogers Commission, engineers on the Flight Readiness Review panel hold formal veto authority over launch approval based on technical safety criteria; they enforce the stand-down until the O-ring redesign is physically certified, irrespective of schedule pressure.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, engineering_safety_panel, agenda_setter,
    institutional, generational, analytical, national).

% NASA program directors and mission managers responsible for maintaining launch schedules, contractor milestones, and Congressional budget cycles; they bear the political and operational costs of halted operations but cannot override the engineering safety veto without violating the Rogers findings.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, program_management, payer,
    powerful, biographical, constrained, national).

% Engineers, technicians, and production staff at aerospace contractors whose employment and project timelines depend on launch cadence; they absorb layoffs, schedule disruptions, and rework costs during the stand-down but lack authority to accelerate the redesign certification.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, contractor_workforce, payer,
    moderate, biographical, constrained, national).

% The Rogers Commission and Congressional oversight committees that authored the findings and monitor NASA compliance; they sit outside the daily operational tension but hold the formal authority that makes the engineering veto binding.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, oversight_authority, observer,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(rogers_commission_findings__engineering_absolute_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents crew loss by ensuring that spaceflight operations do not resume until a known catastrophic engineering flawâthe O-ring seal failure under cold temperaturesâis physically remediated and independently certified as safe.
% TRANSFER_FUNCTION: Transfers launch-no-go authority from schedule-driven program management to engineering safety panels; transfers the operational and financial costs of delay from flight-crew risk exposure to program budgets, contractor payrolls, and mission timelines.
% ABSENT_VOICES: Contractors facing insolvency from indefinite delays and astronauts' families are present in the Commission record but structurally excluded from ongoing Flight Readiness Reviews where the engineering veto is exercised; their exclusion means the deliberation room contains only NASA employees and contractors with institutional stakes.
% DISAPPEARANCE_RATIONALE: If the absolute engineering threshold vanished overnight, program management would regain unilateral launch authority, the O-ring redesign would no longer gate flight operations, and flight crew would be exposed to the same pre-disaster risk profile that produced the Challenger loss.
% FOUNDING_PROBLEM: NASA's schedule-driven launch culture had overridden explicit engineering warnings about O-ring erosion and blow-by in cold weather, resulting in the destruction of Challenger and loss of crew.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission Report documented the engineering warnings and management override with primary-source testimony. Congressional hearings and independent aerospace safety analyses outside NASA's beneficiary chain corroborated the physical failure mode and the cultural override; program management contested the need for complete cessation but not the existence of the flaw.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint imposes a total operational halt with significant schedule and budgetary costs, though it is not monetary rent extraction. Suppression (0.85) is high because the constraint's persistence depends on actively overriding program management's schedule authority through the engineering veto; without that enforcement, launches would resume under political pressure. Theater ratio (0.15) is low because the safety function is genuine and grounded in a demonstrated fatal flaw, though some performative compliance emerges as the stand-down lengthens. Accessibility collapse (0.80) is high because once the Commission established the O-ring redesign as the gating criterion, alternatives such as flying with awareness or probabilistic waivers collapsed within NASA's decision framework. Resistance (0.50) reflects persistent but subterranean pushback from program management and contractors who chafed under the halt but could not openly challenge the Commission's authority.
 *
 * PERSPECTIVAL GAP:
 *   Program management and the contractor workforce experience the constraint as a destructive, externally imposed barrier to legitimate operational activity: their metrics are schedule, budget, and throughput, all of which are degraded. The flight crew experiences the identical constraint as protective: their metric is survival, and the engineering veto prevents an unsafe vehicle from carrying them. The engineering safety panel experiences it as a restored professional normâthe pre-disaster override of engineering judgment by management is reversed. These divergent computed types arise from the same structural facts because directionality distributes the constraint's effects asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Flight crew are declared beneficiaries (low d, subsidized by the constraint's protection). Program management and contractor workforce are declared victims/payers (high d, extraction manifests as halted operations and lost revenue). The engineering safety panel is neither beneficiary nor victim in the base arrays; their directionality defaults toward symmetric because they enforce without collecting rents, though their institutional power places them near the coordination-administration pole. No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as scaffold because it carries an explicit sunset clause: operations cease until O-ring redesign certified. The founding problemâa known catastrophic hardware flawâwas specific and remediable. If the constraint had persisted after the redesign was certified, or if the engineering veto had generalized to non-O-ring issues without new sunset clauses, the classification would drift toward tangled rope or piton. The authored metrics and the explicit sunset keep the classification honest: the extraction is the transitional cost of safety, not a permanent rent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_adherence_post_certification,
    'Did the engineering veto authority actually sunset when the O-ring redesign was certified, or did it persist and generalize to other hardware issues beyond its transitional mandate?',
    'Audit Flight Readiness Review records and engineering veto invocations from return-to-flight (STS-26) through the subsequent decade to determine whether veto usage remained bounded to the certified O-ring redesign or expanded to other systems.',
    'If the veto persisted beyond the O-ring sunset, the scaffold transformed into a tangled rope or pitonâcoordination function generalized, extraction became permanent, and the constraint''s justification shifted from transition to steady-state authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_adherence_post_certification, empirical, 'Whether the scaffold actually sun-setted after O-ring redesign certification').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of launch operations structurally enforced by the engineering veto, or has program management internalized the safety boundary as a cultural norm?',
    'Compare early post-disaster Flight Readiness Reviews (where management contested engineering holds) to late-interval and post-return reviews; measure whether engineering vetoes are still required to prevent launches or whether management self-enforces.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s active enforcement requirement is lower than authored; if still structurally enforced, the constraint remains dependent on the veto mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the flight readiness process').

omega_variable(
    cost_bearer_concentration,
    'Does the cost of launch cessation fall primarily on program management and contractors, or does it diffuse to scientific investigators, international partners, and the taxpayer-funded standing army?',
    'Trace budget overruns, schedule slips, and contractor claims across the stand-down interval to identify cost concentration.',
    'If costs are highly concentrated on contractors, the victim set is narrower and extraction is more targeted; if diffuse, the constraint imposes broad societal deadweight loss without a concentrated payer coalition to resist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_bearer_concentration, empirical, 'Concentration versus diffusion of stand-down costs across the program ecosystem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_eng_abs_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rogers_eng_abs_tr_t6, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 6, 0.07).
narrative_ontology:measurement(rogers_eng_abs_tr_t12, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 12, 0.09).
narrative_ontology:measurement(rogers_eng_abs_tr_t18, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 18, 0.11).
narrative_ontology:measurement(rogers_eng_abs_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.13).
narrative_ontology:measurement(rogers_eng_abs_tr_t30, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 30, 0.14).
narrative_ontology:measurement(rogers_eng_abs_tr_t36, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 36, 0.15).

% Extraction over time
narrative_ontology:measurement(rogers_eng_abs_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(rogers_eng_abs_be_t6, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(rogers_eng_abs_be_t12, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(rogers_eng_abs_be_t18, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(rogers_eng_abs_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(rogers_eng_abs_be_t30, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(rogers_eng_abs_be_t36, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 36, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rogers_eng_abs_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(rogers_eng_abs_su_t6, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 6, 0.88).
narrative_ontology:measurement(rogers_eng_abs_su_t12, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(rogers_eng_abs_su_t18, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 18, 0.84).
narrative_ontology:measurement(rogers_eng_abs_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(rogers_eng_abs_su_t30, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(rogers_eng_abs_su_t36, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 36, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the rogers_commission_findings kernel. The engineering_absolute_threshold reading treats the findings as establishing a physical remediation gate; the management_compliance_narrative reading treats them as establishing a documentation threshold; the actuarial_risk_acceptance reading treats them as establishing a quantified risk threshold. They are structurally distinct constraints because their epsilon values, beneficiary/victim structures, and sunset conditions differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
