% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   The Rogers Commission's investigation into the Challenger disaster
 *   produced findings that colloquially get read as a single verdict, but the
 *   findings actually support at least three structurally distinct readings
 *   of what the Commission established as the operative standard going
 *   forward. This story instantiates ONE such reading: the actuarial
 *   risk-acceptance reading, under which the Commission's findings are taken
 *   to mean that flight is acceptable when a failure probability is
 *   documented and formally accepted by informed decision-makers, rather than
 *   requiring the underlying defect to be eliminated (the
 *   engineering-absolute-threshold reading) or requiring only a compliance
 *   narrative demonstrating due diligence (the
 *   management-compliance-narrative reading). Under this actuarial reading,
 *   mission planners and program stakeholders benefit from continued flight
 *   cadence; the categorical safety norm that an unresolved failure mode
 *   halts flight is the structural victim, along with the flight crews and
 *   engineers whose risk exposure and professional judgment are subordinated
 *   to a probability figure produced under schedule pressure.
 *
 * KEY AGENTS:
 *   - mission_planners: agenda_setter, institutional power, sets the acceptance threshold and captures schedule benefit
 *   - flight_crews: powerless payer, trapped exit, bears the physical risk the probability figure quantifies
 *   - line_engineers: moderate power payer/excluded, professional judgment overridden by the numeric acceptance process
 *   - categorical_safety_norms: non-agent payer, the institutional commitment displaced by the actuarial standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.68).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.58).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk-Acceptance Standard").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, '1c74846b-eaee-478e-8272-6664e1c5bd8a').
narrative_ontology:cs_kernel_codification('1c74846b-eaee-478e-8272-6664e1c5bd8a', formalized).
narrative_ontology:cs_authority_grounding('1c74846b-eaee-478e-8272-6664e1c5bd8a', extraction).
narrative_ontology:cs_interpretation_layer_present('1c74846b-eaee-478e-8272-6664e1c5bd8a').
narrative_ontology:cs_reading_relation('1c74846b-eaee-478e-8272-6664e1c5bd8a', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('1c74846b-eaee-478e-8272-6664e1c5bd8a', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('1c74846b-eaee-478e-8272-6664e1c5bd8a', foundational, quantified_bounded_risk_is_sufficient_for_flight).
narrative_ontology:cs_axiom_status(quantified_bounded_risk_is_sufficient_for_flight, holdable).
narrative_ontology:cs_axiom_grounding('1c74846b-eaee-478e-8272-6664e1c5bd8a', quantified_bounded_risk_is_sufficient_for_flight, instrumental).
narrative_ontology:cs_axiom('1c74846b-eaee-478e-8272-6664e1c5bd8a', secondary, informed_decision_maker_signoff_substitutes_for_defect_elimination).
narrative_ontology:cs_axiom_status(informed_decision_maker_signoff_substitutes_for_defect_elimination, holdable).
narrative_ontology:cs_axiom_grounding('1c74846b-eaee-478e-8272-6664e1c5bd8a', informed_decision_maker_signoff_substitutes_for_defect_elimination, conventional).
narrative_ontology:cs_reference_frame('1c74846b-eaee-478e-8272-6664e1c5bd8a', categorical_engineering_veto_authority).
narrative_ontology:cs_drift_state('1c74846b-eaee-478e-8272-6664e1c5bd8a', post_challenger_organizational_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1c74846b-eaee-478e-8272-6664e1c5bd8a', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, program_schedule_stakeholders).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, launch_contractors).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, flight_crews).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, line_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, launch_contractors).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, quantified_risk_management_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set launch schedules and program milestones. Adopt the probability-bound framing because it lets flights proceed against a documented, bounded risk figure rather than an unresolved engineering red flag. They select which failure-probability estimate becomes the operative one and who signs off on it, and they capture the schedule and funding benefit of continued flight.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, agenda_setter,
    institutional, biographical, arbitrage, national).

% Depend on maintaining launch cadence for funding renewal, contractor payment milestones, and political visibility. The actuarial framing lets them treat a known defect as a managed probability rather than a stop-work condition, preserving the flight manifest.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_schedule_stakeholders, beneficiary,
    institutional, biographical, mobile, national).

% Supply the O-ring hardware and other flight components under contract. A quantified-and-accepted risk standard lets them continue delivering and billing against schedule rather than absorbing a stop-work redesign cycle, though they also carry liability exposure if the documented probability proves wrong.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, launch_contractors, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, launch_contractors, payer).

% Fly the mission the probability estimate is attached to. They did not set the acceptance threshold, cannot independently verify the documented failure probability, and have no exit once assigned to a flight — the actuarial acceptance is made on their behalf, at their physical risk, by decision-makers several organizational layers removed.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, flight_crews, payer,
    powerless, immediate, trapped, local).

% Identified the O-ring cold-temperature failure mode and argued for a categorical no-fly threshold. Under the actuarial framing their concern is translated into a probability figure that management can accept, which overrides their professional judgment that the defect is not yet bounded well enough to fly. Raising further objection risks being read as insubordination against a documented, signed-off risk acceptance.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, line_engineers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, line_engineers, excluded).

% The prior engineering norm — that an unresolved failure mode halts flight until eliminated — is displaced by the actuarial standard. It is not an actor and collects nothing; it is the institutional commitment that erodes each time a probability-and-acceptance decision substitutes for a design fix.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms, payer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).

% Investigate accidents after the fact and reconstruct whether the documented probability and its acceptance process were genuine risk quantification or a rationalization built to permit a schedule-driven launch. Their findings shape whether the actuarial standard is reformed or entrenched.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, post_hoc_review_boards, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision procedure for continuing operations under known, unresolved technical uncertainty: rather than an indefinite halt whenever any failure mode is unquantified, the organization can document a probability estimate, have it reviewed, and proceed if an informed decision-maker accepts it. This solves a genuine problem — perfect safety is unattainable and some threshold for acceptable residual risk is unavoidable in any flight program.
% TRANSFER_FUNCTION: Moves the authority to accept life-safety risk away from the engineers who identified the failure mode and toward program management and mission planners, and moves the consequence of an underestimated probability onto flight crews and, ultimately, onto the credibility of the categorical safety norm the engineers were defending.
% ABSENT_VOICES: Flight crews are not party to the acceptance decision that governs their own risk exposure. Line engineers who flagged the O-ring behavior are structurally present but their categorical objection is converted into a number they did not choose and may not endorse; their dissent is documented but overridden by the sign-off process.
% DISAPPEARANCE_RATIONALE: If the actuarial acceptance standard were removed and replaced by a categorical threshold (any unresolved failure mode halts flight), mission planners would lose the mechanism that lets schedule pressure override open engineering objections; launch cadence would slow substantially, and the balance of authority would shift back toward engineering veto power.
% FOUNDING_PROBLEM: Spaceflight has genuine going-concern engineering uncertainty that cannot always be resolved before a scheduled launch; some standard is needed to distinguish 'documented and bounded' residual risk from 'unknown and unbounded' risk so that flight operations are not paralyzed by every open engineering question.
% FOUNDING_PROBLEM_CORROBORATION: Program management and mission planners attest the actuarial standard remains necessary because flight cannot wait for every uncertainty to be eliminated. The Rogers Commission itself, external to management, found that the actual practice diverged from genuine quantified risk assessment — probability estimates were produced and accepted in a process shaped by schedule pressure rather than independent technical confidence, per its own investigative record and outside engineering testimony (including from line engineers who dissented at the time).
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is substantial (0.68) and rising across the measured interval because each successful actuarial acceptance episode further normalizes probability-and-sign-off as an acceptable substitute for defect elimination, entrenching the pattern. Theater ratio rises correspondingly (0.46) because a growing share of the acceptance process becomes procedural — producing a documented figure and a signature — rather than substantively resolving the engineering uncertainty. Suppression (0.58) reflects the real but partial coercive weight behind the standard: engineers who object face institutional and career pressure to accept the numeric framing, though this is not absolute suppression since dissent is documented rather than erased.
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners and program stakeholders sit near the beneficiary end: they set the terms of acceptance and capture the schedule/funding benefit of continued flight. Flight crews sit at the full-target end: trapped exit, immediate time horizon, zero say in the acceptance decision that governs their own life-safety exposure. Line engineers occupy an intermediate position — they have professional standing and document their objection, but their exit options are constrained by employment and program dependency, and their categorical judgment is overridden rather than adopted.
 *
 * MANDATROPHY ANALYSIS:
 *   The actuarial standard did solve a genuine problem (perfect certainty is unattainable and some risk-acceptance mechanism is necessary for any flight program to operate). Reading it as pure extraction would miss that a working coordination function exists. But the founding problem's status is contested: post-hoc investigation found the actual acceptance episodes were shaped by schedule pressure rather than independent technical confidence, meaning the standard's coordination function had been substantially captured by the extraction it was supposed to bound — this is the tangled-rope signature (genuine coordination function + asymmetric extraction under active enforcement), not a clean rope or a clean snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_reading_vs_engineering_reading_kernel_ambiguity,
    'Do the Rogers Commission findings actually endorse quantified-probability acceptance as sufficient grounds for flight, or does that reading retroactively launder a schedule-driven decision that the Commission itself criticized as a departure from sound engineering judgment?',
    'Close textual analysis of the Commission''s report language distinguishing its factual findings (what happened) from its normative recommendations (what standard should govern future decisions), cross-referenced against subsequent NASA safety culture reforms and independent historical analysis of how the finding was operationalized.',
    'If the Commission''s normative recommendation was closer to the engineering-absolute-threshold reading, then the actuarial reading modeled here is itself an artifact of the extraction it enables — a reading selected because it permits continued flight, not because it is the best-supported interpretation of the findings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_reading_vs_engineering_reading_kernel_ambiguity, conceptual, 'Whether the actuarial reading is a faithful interpretation of Rogers or a self-serving selection among competing readings.').

omega_variable(
    probability_estimate_good_faith_ambiguity,
    'Were the documented failure-probability estimates produced under this standard genuine technical assessments, or were they numbers reverse-engineered to justify a schedule-driven decision already made?',
    'Comparison of pre-decision engineering risk estimates against the numbers ultimately documented and accepted, and examination of whether dissenting technical staff''s estimates were higher and were overridden rather than incorporated.',
    'If estimates were reverse-engineered, the coordination function claimed for this reading (genuine risk quantification) is largely theatrical, pushing the classification further toward snare; if estimates were produced independently and merely disagreed with by some engineers, the coordination function is more substantively real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_estimate_good_faith_ambiguity, empirical, 'Whether the risk-quantification process was substantively independent or captured by the decision it was meant to inform.').

omega_variable(
    sibling_reading_foreclosure_ambiguity,
    'Does adopting the actuarial reading as institutional practice functionally foreclose the engineering-absolute-threshold reading in future similar cases, even though both remain textually available in the Commission''s findings?',
    'Track subsequent program decisions (e.g., post-Challenger and post-Columbia safety culture changes) for whether categorical halt authority was restored to engineering staff or whether actuarial acceptance remained the operative default.',
    'If institutional practice consistently defaults to actuarial acceptance, the coexists_with relation declared here may understate the practical dominance this reading has achieved over its sibling, even though no single framework logically requires that dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_ambiguity, conceptual, 'Whether practical institutional adoption has functionally subordinated the engineering reading despite formal coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.2).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 4, 0.27).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 8, 0.33).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 12, 0.38).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 16, 0.43).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 20, 0.46).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(roge_su_t16, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rogers_commission_findings kernel, decomposed per the epsilon-invariance principle because the colloquial label 'Rogers findings' covers structurally distinct claims with different beneficiary/victim structures and different extraction profiles. actuarial_risk_acceptance (this story) treats a documented, accepted failure probability as sufficient for flight; engineering_absolute_threshold treats the findings as requiring defect elimination before flight resumes; management_compliance_narrative treats the findings as requiring only a demonstrated due-diligence process. Each carries its own epsilon and stakeholder set; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
