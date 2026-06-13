% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission O-Ring Redesign Absolute Threshold
 *   domain: organizational_safety/technology_governance
 *
 * SUMMARY:
 *   Following the Challenger disaster, the Rogers Commission investigated and
 *   found that NASA's Flight Readiness Review process had treated a known
 *   O-ring temperature sensitivity hazard as an acceptable risk. Management
 *   had proceeded despite documented engineer objections. The Commission's
 *   report recommended that flight operations cease until O-ring design was
 *   fundamentally changed and certified safe. THIS READING
 *   (engineering_absolute_threshold) instantiates the interpretation that the
 *   Rogers findings establish an absolute technical boundary: no flight until
 *   O-ring redesign is certified, period. Engineers hold veto authority in
 *   Flight Readiness Reviews; the constraint operates by suppressing launch
 *   operations until technical certification is complete. This reading
 *   competes with two siblings: (1) actuarial_risk_acceptance, which reads
 *   Rogers as requiring documented risk quantification and informed
 *   acceptance rather than absolute prohibition, and (2)
 *   management_compliance_narrative, which reads Rogers as requiring visible
 *   risk-management process rather than a specific technical outcome. All
 *   three are live readings of the same kernel (the Rogers Commission
 *   report); they differ in what structural requirement the report is read to
 *   impose.
 *
 * KEY AGENTS:
 *   - flight_crew: benefit from the absolute safety boundary; life-or-death outcome
 *   - nasa_engineers: hold technical veto authority; certify redesign completeness; gate the constraint
 *   - launch_cadence_operators: pay in schedule delays and mission postponements; constrained by technical certification requirement
 *   - rogers_commission: external authority that investigated and recommended the boundary; statutory legitimacy
 *   - political_leadership: excluded from technical decision authority; forecloses political override of technical assessment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.19).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.92).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.19).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, mountain).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission O-Ring Redesign Absolute Threshold").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance").

domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, 'e10e0349-6b44-4642-836c-8cfb4dae96e1').
narrative_ontology:cs_kernel_codification('e10e0349-6b44-4642-836c-8cfb4dae96e1', fixed_text).
narrative_ontology:cs_authority_grounding('e10e0349-6b44-4642-836c-8cfb4dae96e1', lineage).
narrative_ontology:cs_interpretation_layer_present('e10e0349-6b44-4642-836c-8cfb4dae96e1').
narrative_ontology:cs_reading_relation('e10e0349-6b44-4642-836c-8cfb4dae96e1', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_reading_relation('e10e0349-6b44-4642-836c-8cfb4dae96e1', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('e10e0349-6b44-4642-836c-8cfb4dae96e1', foundational, o_ring_failure_unacceptable_absolute).
narrative_ontology:cs_axiom_status(o_ring_failure_unacceptable_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e10e0349-6b44-4642-836c-8cfb4dae96e1', o_ring_failure_unacceptable_absolute, deontological).
narrative_ontology:cs_axiom('e10e0349-6b44-4642-836c-8cfb4dae96e1', foundational, engineering_authority_independent).
narrative_ontology:cs_axiom_status(engineering_authority_independent, holdable).
narrative_ontology:cs_axiom_grounding('e10e0349-6b44-4642-836c-8cfb4dae96e1', engineering_authority_independent, conventional).
narrative_ontology:cs_reference_frame('e10e0349-6b44-4642-836c-8cfb4dae96e1', engineering_veto_over_launch).
narrative_ontology:cs_drift_state('e10e0349-6b44-4642-836c-8cfb4dae96e1', post_rogers_commission_institutionalization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e10e0349-6b44-4642-836c-8cfb4dae96e1', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, ExtMetricName, E),
    domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading is authored as a MOUNTAIN because the underlying mechanism (O-ring failure under cold stress) is a physical fact that does not change based on who is assessing it or what institutional arrangements exist. The suppression value is extremely high (0.92) because the constraint operates by WITHHOLDING operational authority — no launch can proceed without certification; there is no negotiation with physical reality. Extraction is very low (0.19) because the constraint does not extract value from flight crew or redirect it to a beneficiary; instead it prevents operations. The beneficiary (flight_crew) collects safety, not economic rents. Theater is minimal (0.08) because the constraint is functionally what it claims to be — technical assessment prevents launches, not performance of assessment. The accessibility_collapse is high (0.88) because once engineers certify, the question is resolved; once they refuse certification, the option to launch-anyway is foreclosed by the physical design. The resistance is moderate (0.34) because schedule-driven actors push back against delays, but the pushback does not overcome the technical gate — once an engineer says 'not certified,' operations stop, and resistance is absorbed as schedule cost. The measured extraction accumulates modestly over time (0.05 → 0.19 across 36 months) as the delay costs mount and the program absorbs the schedule slip as a permanent budget tax, but the core mechanism remains a technical gate, not an extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (launch operators) and the beneficiary seat (flight crew) perceive opposite truths from the same constraint. Operators see a coordination mechanism that prevents them from launching by schedule; crew sees a protection mechanism. Neither is wrong — they are observing different flows from the same structure. The agenda-setter seat (engineers) perceives technical authority where operators perceive constraint and crew perceives safety. The engine's per-seat classification captures this divergence: the constraint is a mountain from crew's perspective (immutable boundary, zero degrees of freedom, enormous benefit), a tangled_rope or snare from operators' perspective (suppression without their consent, asymmetric cost), and something closer to an enforcement_mechanism from engineers' perspective (they choose the gate position). The authored claim (mountain) reflects the underlying physical mechanism, not any seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   From the flight_crew perspective: d ≈ 0.0 (beneficiary — the constraint subsidizes their survival probability). From the launch_cadence_operators perspective: d ≈ 0.95 (target — the constraint imposes delays, cost, and schedule uncertainty on them with no corresponding benefit they control). From the nasa_engineers perspective: d ≈ 0.5 (symmetric/analytical — they hold authority and prevent harm, but absorb the social cost of being the institutional voice of 'no'). The constraint operates differently at each seat: crew experiences it as protection, operators experience it as suppression, engineers experience it as responsibility. The engine computes this divergence; the authored claim (mountain) reflects what the underlying mechanism IS, not which seat's perspective is correct.
 *
 * MANDATROPHY ANALYSIS:
 *   The Rogers reading (this file) instantiates the interpretation that the founding problem (technical override leading to catastrophic failure) is LIVE and requires an ABSOLUTE technical boundary. Under this reading, the constraint is permanent because the founding problem is permanent: thermal physics does not change, and the hazard mode cannot be negotiated. The constraint does not answer 'we learned to manage risk better'; it answers 'we must not place humans in this failure mode ever again.' This mandatrophy analysis is meant to distinguish this reading from the management_compliance reading, which would argue the founding problem is SOLVED by better documentation of risk, permitting the launch decision to proceed once risk is formally accepted. Those are genuinely different interpretations of what Rogers found. This omega-tier ambiguity is recorded in the natural_law_vs_constructed_boundary omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is the O-ring failure mechanism a natural physical law (material properties under thermal stress) or a socially constructed policy boundary imposed on an inherently quantifiable risk?',
    'Examine whether the constraint treats the O-ring failure mode as: (a) an absolute physical boundary where the failure mechanism cannot be tolerated at any probability, or (b) a designed specification that sets a safe operating envelope but could theoretically be accepted at quantified low probability by informed decision-makers. Post-Rogers Commission policy language, engineering handbooks, and Flight Readiness Review procedures provide the evidence.',
    'If (a) — natural law reading — the constraint is a genuine mountain: the failure physics is immutable and the prohibition emerges from physical necessity. If (b) — constructed boundary reading — beneficiaries (flight crew) and political losers (schedule-delayed programs) reveal the constraint as socially chosen, possibly contestable. This omega documents the irreducible ambiguity the Rogers Commission reading instantiates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'Whether O-ring safety requirement is physics-based absolute or policy-constructed threshold.').

omega_variable(
    sibling_reading_contestation,
    'What structural facts distinguish THIS reading (engineering absolute threshold) from the sibling readings (management compliance narrative, actuarial risk acceptance)?',
    'Observe how NASA Flight Readiness Reviews actually decide: (1) Do they require zero-probability O-ring failure or quantified-low-probability acceptance? (2) Who has veto authority — engineers alone or engineers + risk managers? (3) Are launch decisions contingent on documented risk acceptance by senior management, or are they contingent on engineering sign-off independent of risk acceptance? The answers reveal which reading is structurally operative.',
    'If (1) zero-probability and (2) engineers hold veto independently, this reading (absolute threshold) is operative. If (1) quantified-low and (2) decision-makers choose between documented risk options, the actuarial reading is operative. If (3) compliance with risk-documentation process matters more than the numerical threshold, the management narrative reading is operative. This omega records that the Rogers kernel is genuinely contested at the structural level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contestation, empirical, 'Which of the three Rogers readings is actually operative in Flight Readiness decisions.').

omega_variable(
    engineering_capture_ambiguity,
    'Do engineers hold veto authority because they possess irreplaceable technical knowledge, or because they are the designated political losers in a settlement that gives them power as a side payment for absorbing schedule slippage?',
    'Historical analysis of pre- and post-Rogers Commission engineering authority: did engineers hold this veto before Rogers (or was it overridden)? Do they retain it in domains where no political catastrophe occurred? If engineering veto authority is unevenly distributed across safety domains depending on political salience, the authority is likely a side payment rather than a technical necessity.',
    'If technical knowledge → authority, the constraint reflects engineering reality. If political settlement → authority, the constraint reflects organizational power redistribution and is less immutable than the natural-law framing suggests. The Rogers reading (engineering absolute threshold) assumes the first; the management compliance reading assumes the second.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_capture_ambiguity, empirical, 'Whether engineering veto authority derives from technical necessity or political settlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(roge_tr_t6, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 6, 0.06).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 12, 0.07).
narrative_ontology:measurement(roge_tr_t18, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 18, 0.08).
narrative_ontology:measurement(roge_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.08).
narrative_ontology:measurement(roge_tr_t30, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 30, 0.08).
narrative_ontology:measurement(roge_tr_t36, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 36, 0.08).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 6, 0.08).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 12, 0.12).
narrative_ontology:measurement(roge_be_t18, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(roge_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(roge_be_t30, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(roge_be_t36, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 36, 0.19).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 6, 0.89).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 12, 0.91).
narrative_ontology:measurement(roge_su_t18, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 18, 0.92).
narrative_ontology:measurement(roge_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.92).
narrative_ontology:measurement(roge_su_t30, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 30, 0.92).
narrative_ontology:measurement(roge_su_t36, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 36, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% The Rogers Commission kernel is instantiated in three constraint stories, each reading the same text as imposing a different structural requirement. engineering_absolute_threshold (this file) reads the findings as establishing a hard technical boundary requiring redesign certification. management_compliance_narrative reads the findings as establishing a process-compliance requirement permitting flight once risk documentation is adequate. actuarial_risk_acceptance reads the findings as establishing a risk-quantification requirement permitting flight at documented low probability. These three readings remain live in NASA decision-making; they differ in whether the constraint gates operations (boundary reading, this file), enables compliance (process reading), or permits quantified-risk decisions (actuarial reading). The three stories share the Rogers kernel but have incompatible ε values, beneficiary/victim structures, and claimed types. Each story is authored independently per ε-invariance principle; they are linked via network.affects_constraints to signal the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
