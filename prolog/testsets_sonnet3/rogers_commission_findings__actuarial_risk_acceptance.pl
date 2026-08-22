% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__actuarial_risk_acceptance, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Commission Actuarial Risk-Acceptance Standard
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This story instantiates one of three structurally distinct readings of
 *   the Rogers Commission's post-Challenger findings on acceptable flight
 *   risk: the actuarial reading, under which flight is acceptable so long as
 *   failure probability is documented and formally accepted by an informed
 *   decision-maker. This is NOT the engineering-absolute-threshold reading
 *   (which treats any uncertified failure mode as a hard stop) nor the
 *   management-compliance-narrative reading (which treats documented process
 *   as sufficient regardless of the probability's substance). Each of the
 *   three is authored as its own constraint with its own epsilon; they are
 *   linked via the kernel and via network.affects_constraints. Under the
 *   actuarial reading, mission planners and program-schedule stakeholders
 *   benefit from converting a categorical engineering veto into a negotiable
 *   probability threshold; categorical safety norms, flight crews, and field
 *   engineers pay the cost of that conversion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.68).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.58).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk-Acceptance Standard").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, 'aae1adbe-bd5b-4872-8206-7a6e858b41a1').
narrative_ontology:cs_kernel_codification('aae1adbe-bd5b-4872-8206-7a6e858b41a1', formalized).
narrative_ontology:cs_authority_grounding('aae1adbe-bd5b-4872-8206-7a6e858b41a1', extraction).
narrative_ontology:cs_interpretation_layer_present('aae1adbe-bd5b-4872-8206-7a6e858b41a1').
narrative_ontology:cs_reading_relation('aae1adbe-bd5b-4872-8206-7a6e858b41a1', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('aae1adbe-bd5b-4872-8206-7a6e858b41a1', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_axiom('aae1adbe-bd5b-4872-8206-7a6e858b41a1', foundational, quantified_and_accepted_probability_is_sufficient_for_flight).
narrative_ontology:cs_axiom_status(quantified_and_accepted_probability_is_sufficient_for_flight, holdable).
narrative_ontology:cs_axiom_grounding('aae1adbe-bd5b-4872-8206-7a6e858b41a1', quantified_and_accepted_probability_is_sufficient_for_flight, instrumental).
narrative_ontology:cs_axiom('aae1adbe-bd5b-4872-8206-7a6e858b41a1', secondary, informed_institutional_signature_discharges_risk_responsibility).
narrative_ontology:cs_axiom_status(informed_institutional_signature_discharges_risk_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('aae1adbe-bd5b-4872-8206-7a6e858b41a1', informed_institutional_signature_discharges_risk_responsibility, conventional).
narrative_ontology:cs_reference_frame('aae1adbe-bd5b-4872-8206-7a6e858b41a1', pre_challenger_categorical_engineering_veto).
narrative_ontology:cs_drift_state('aae1adbe-bd5b-4872-8206-7a6e858b41a1', post_columbia_normalization_of_deviance_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aae1adbe-bd5b-4872-8206-7a6e858b41a1', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, program_schedule_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, flight_crews).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, field_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, informed_consent_doctrine_for_organizational_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set launch schedules and program milestones against fixed budget and political calendars. Adopt the actuarial reading of the Rogers findings because it lets flight operations continue as long as a failure probability is computed, documented, and formally accepted by an authorized decision-maker. They administer the documentation and acceptance process and benefit directly from schedule continuity.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary).

% Contractors, congressional sponsors, and agency leadership whose funding and reputational capital depend on maintaining launch cadence. They gain from a framework that converts hard technical stop conditions into negotiable probability thresholds; they can shift blame or resources elsewhere if a specific mission is delayed rather than absorbing systemic schedule risk.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_schedule_stakeholders, beneficiary,
    powerful, biographical, mobile, national).

% The prior engineering norm — do not fly until the failure mode is eliminated or the safety margin is restored to design intent — is displaced by a probability-acceptance standard. As a non-agent standard it cannot resist, negotiate, or exit; it is simply redefined out of governing force whenever a documented probability is accepted instead.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).

% Fly the missions whose risk has been quantified and accepted by others upstream. They bear the actual failure probability in their bodies while having no seat in the acceptance decision beyond nominal briefings; declining to fly a specific mission on stated risk grounds is a career-ending exit in practice, not a real option.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, flight_crews, payer,
    moderate, immediate, trapped, national).

% Identify anomalies (e.g. O-ring erosion under cold conditions) and recommend halting flight until root cause is resolved. Under the actuarial reading, their qualitative technical objections are converted into a probability estimate that can be accepted upward, effectively overriding their categorical recommendation without requiring anyone to rebut it on engineering grounds.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, field_engineers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, field_engineers, excluded).

% Senior officials who formally accept the documented failure probability and thereby authorize flight. Their signature converts an engineering uncertainty into an organizationally ratified acceptable risk, discharging institutional liability upward while the physical risk remains downward on crews.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers, beneficiary).

% Post-accident review bodies and subsequent safety panels examine whether documented probability acceptance actually substituted for engineering resolution. They can recommend structural changes to the acceptance process but do not control mission go/no-go decisions in real time.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, independent_safety_oversight, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal mechanism for continuing operations under known, non-zero technical risk by requiring that the risk be quantified and explicitly accepted by an authorized decision-maker, rather than either ignoring the risk silently or halting all operations for any nonzero failure probability.
% TRANSFER_FUNCTION: Moves authority over what counts as acceptable risk from field engineers holding a categorical safety threshold to institutional decision-makers holding a probability threshold, and moves the consequence of that risk from the decision-makers who accept it on paper to the flight crews who bear it in practice.
% ABSENT_VOICES: Field engineers whose qualitative 'do not fly under these conditions' recommendations are recast as inputs to a probability estimate rather than as binding technical vetoes; flight crews themselves, who have no formal seat in the acceptance signature despite bearing the entire consequence if the accepted probability is realized.
% DISAPPEARANCE_RATIONALE: If the actuarial-acceptance reading disappeared and the categorical engineering threshold reading governed instead, flight operations would halt whenever any credible failure mode existed without a certified fix, regardless of computed probability. Program schedules, contractor revenue timing, and mission cadence would all have to reorganize around a hard-stop discipline rather than a documented-and-accepted-risk discipline.
% FOUNDING_PROBLEM: After the Challenger disaster, the Rogers Commission needed to state a standard for when NASA could resume flight operations despite the O-ring failure mode not being fully eliminated — some workable rule was needed to distinguish reckless flight from responsible risk-taking under uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: Mission planners and program stakeholders attest the actuarial standard is a mature, appropriately conservative risk-management discipline still serving its founding purpose. Independent safety oversight bodies (including the post-Columbia Rogers-style reviews) and outside engineering historians attest that in practice the standard was used to normalize deviance — accepted probabilities crept upward over successive missions without corresponding engineering fixes, meaning the founding problem of distinguishing genuine risk acceptance from schedule-driven rationalization was not solved by this reading and in some tellings was made worse by it.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 by interval end) reflects that the probability-acceptance mechanism systematically transfers risk-bearing downward (to crews) while transferring risk-defining authority upward (to institutional signatories), without a corresponding transfer of consequence upward. Suppression (0.58) is moderate: field engineers can still raise objections, but their categorical recommendations are structurally reframed as inputs to a number rather than binding vetoes, which suppresses the practical force of dissent without eliminating the voice itself. Theater ratio rises across the interval (0.20 to 0.42) as documentation and sign-off procedures become more elaborate relative to actual engineering resolution of failure modes — a classic Goodhart drift where paperwork thoroughness substitutes for hazard elimination.
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners and informed decision-makers sit near the beneficiary end: they administer the acceptance process and capture the schedule-continuity benefit, with arbitrage-grade exit (they are rarely the ones exposed to the accepted risk). Flight crews and field engineers sit near the target end: crews are trapped (declining a mission on stated-risk grounds is not a real option) and field engineers are constrained (their technical objections survive only as inputs to a process they do not control). Categorical safety norms, as a non-agent structural standard, cannot be assigned directionality in the ordinary sense but are declared as payer to register that the norm itself is what the reading extracts against.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to responsibly resume flight after a known but unresolved failure mode — remains genuinely live in the abstract (uncertain engineering systems still require some resumption standard), so this is not a simple case of a dead mandate persisting by inertia. What is contested is whether THIS particular resolution (probability documentation plus institutional sign-off) still serves that founding problem or has drifted into a schedule-protection mechanism wearing the founding problem's justification. The corroboration split — beneficiaries say it still works, independent post-accident reviews say it normalized deviance — is exactly the R5 mismatch this framework is built to surface, and is why founding_problem_status is authored as contested rather than resolved in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Did the Rogers Commission findings themselves specify which of the three readings (actuarial acceptance, engineering absolute threshold, or management compliance narrative) was intended as the governing standard, or is the ambiguity original to the findings rather than a later interpretive drift?',
    'Close textual analysis of the Commission''s final report language alongside internal NASA memoranda from the period immediately following the report''s release, cross-checked against how the standard was actually invoked in subsequent flight-readiness reviews.',
    'If the findings were textually ambiguous from the start, the actuarial reading is a legitimate co-equal interpretation rather than a captured drift; if the findings clearly specified an engineering-absolute standard, the actuarial reading represents a later institutional substitution that this story should treat as more contested than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the three sibling readings reflect original textual ambiguity or later interpretive drift from a single intended standard.').

omega_variable(
    probability_threshold_calibration,
    'Was the accepted failure probability under this reading calibrated to genuine engineering uncertainty, or was it calibrated backward from the schedule outcome mission planners already wanted?',
    'Compare documented pre-mission probability estimates against post-accident forensic probability reconstructions across multiple missions; a pattern of estimates clustering just under whatever threshold permitted the desired launch date would indicate backward calibration.',
    'Backward calibration would indicate the actuarial framework functions primarily as a rationalization mechanism (supporting the tangled_rope classification''s extraction component); forward calibration from genuine uncertainty would support treating this reading as closer to legitimate risk management under irreducible uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_threshold_calibration, empirical, 'Whether documented risk probabilities were derived from engineering evidence or reverse-engineered from schedule pressure.').

omega_variable(
    crew_consent_authenticity,
    'Is flight crew acceptance of the documented and institutionally accepted risk meaningfully informed consent, or is it structurally coerced by career and institutional incentives such that the ''informed decision-maker'' framing does not extend to the people actually bearing the risk?',
    'Interview and archival record analysis of instances where crew members raised risk objections and trace what happened to their subsequent assignments and standing.',
    'If crew consent is structurally coerced, the victim classification of flight_crews strengthens and the tangled_rope''s extraction component is larger than the base metrics currently reflect; if crews had genuine, exercised veto power in practice, the extraction assessment should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crew_consent_authenticity, empirical, 'Whether flight crew acceptance of accepted risk constitutes genuine informed consent or structurally coerced compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.2).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 4, 0.26).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 8, 0.33).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 12, 0.37).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 16, 0.4).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 4, 0.49).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(roge_su_t16, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the Rogers Commission findings on acceptable flight risk' per the ε-invariance principle. Each sibling reads the same historical findings as establishing a structurally different operative standard (probability acceptance vs. engineering certification vs. compliance documentation), with different beneficiary/victim structures and different ε. actuarial_risk_acceptance is authored here as tangled_rope (genuine coordination function of enabling operation under irreducible uncertainty, paired with asymmetric extraction shifting risk-bearing downward); the engineering_absolute_threshold sibling is expected to read closer to rope or mountain-adjacent (a hard technical floor); the management_compliance_narrative sibling is expected to read closer to snare or piton (documentation substituting for substance). All three should be linked bidirectionally in their network.affects_constraints arrays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
