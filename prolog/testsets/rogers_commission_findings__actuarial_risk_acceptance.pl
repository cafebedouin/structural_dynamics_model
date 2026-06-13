% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Commission Actuarial Risk Acceptance Framework
 *   domain: organizational/safety/governance
 *
 * SUMMARY:
 *   The Rogers Commission's investigation of the 1986 Challenger disaster
 *   established that O-ring erosion risk was quantified but not
 *   systematically presented to informed decision-makers for explicit
 *   acceptance. The actuarial-risk-acceptance reading interprets the Rogers
 *   mandate as requiring documented failure probability and informed
 *   decision-maker authorization before flight. This reading sits in direct
 *   contestation with two siblings: the engineering-absolute-threshold
 *   reading (Rogers mandates halt-until-redesigned), which prioritizes
 *   technical certainty, and the management-compliance-narrative reading
 *   (Rogers mandates documented-effort demonstration), which prioritizes
 *   compliance process over quantified probability. The three readings
 *   instantiate the same kernel (the Rogers Commission's findings and
 *   prescriptions) but extract opposite operational consequences. This story
 *   captures the actuarial reading: mission planners benefit from operational
 *   flexibility within quantified bounds; engineering safety doctrine is
 *   displaced as the mandatory closure condition; informed decision-makers
 *   become the enforcement gate.
 *
 * KEY AGENTS:
 *   - NASA administration: agenda-setter controlling the informed-decision-maker frame and the approval threshold.
 *   - Mission planners: beneficiaries authorized to continue flights within quantified probability bounds.
 *   - Informed decision-makers: dual payers/agenda-setters who hold veto authority and bear legal liability.
 *   - Engineering safety culture: institutional doctrine displaced by quantified-risk acceptance; victim of the constraint.
 *   - Astronauts and crew: excluded from the informed-decision-maker circle; ultimate targets if failure occurs within accepted bounds.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.68).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.71).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk Acceptance Framework").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational/safety/governance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, 'e7065c95-7675-4807-8f5d-c4adea0d1556').
narrative_ontology:cs_kernel_codification('e7065c95-7675-4807-8f5d-c4adea0d1556', fixed_text).
narrative_ontology:cs_authority_grounding('e7065c95-7675-4807-8f5d-c4adea0d1556', extraction).
narrative_ontology:cs_interpretation_layer_present('e7065c95-7675-4807-8f5d-c4adea0d1556').
narrative_ontology:cs_reading_relation('e7065c95-7675-4807-8f5d-c4adea0d1556', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('e7065c95-7675-4807-8f5d-c4adea0d1556', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('e7065c95-7675-4807-8f5d-c4adea0d1556', foundational, quantified_probability_permits_flight).
narrative_ontology:cs_axiom_status(quantified_probability_permits_flight, holdable).
narrative_ontology:cs_axiom_grounding('e7065c95-7675-4807-8f5d-c4adea0d1556', quantified_probability_permits_flight, empirically_contingent).
narrative_ontology:cs_axiom('e7065c95-7675-4807-8f5d-c4adea0d1556', foundational, informed_decision_maker_authority_supreme).
narrative_ontology:cs_axiom_status(informed_decision_maker_authority_supreme, holdable).
narrative_ontology:cs_axiom_grounding('e7065c95-7675-4807-8f5d-c4adea0d1556', informed_decision_maker_authority_supreme, conventional).
narrative_ontology:cs_reference_frame('e7065c95-7675-4807-8f5d-c4adea0d1556', actuarial_risk_quantification_framework).
narrative_ontology:cs_drift_state('e7065c95-7675-4807-8f5d-c4adea0d1556', contemporary_schedule_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7065c95-7675-4807-8f5d-c4adea0d1556', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, nasa_administration).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, quantifiable_risk_doctrine).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_maker_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authorized to continue flight operations provided failure probability is documented and presented to informed decision-makers for acceptance. They gain operational flexibility: instead of mandatory redesign cycles, they execute missions within quantified risk bounds. Exit would require returning to engineering-absolute-threshold readings that foreclose launch until technical redesign is certified.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary,
    institutional, biographical, constrained, national).

% Sets and enforces the actuarial framework. Administers the informed-decision-maker approval gate, determines who qualifies as informed, and adjudicates risk acceptance. Benefits from operational continuity and cost avoidance of mandatory redesigns. Controls the framework's application and can shift approval thresholds.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, nasa_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, nasa_administration, beneficiary).

% Accept documented risk on behalf of the flight program and ultimately the public. They bear legal and reputational liability for mission failures within accepted probability bounds. Their authorization is the enforcement mechanism: without their documented acceptance, the constraint cannot persist. They can withdraw acceptance, triggering redesign requirements.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers, agenda_setter).

% An institutional norm and identity substrate that treats unknown failure modes as categorical prohibitions until engineered away. The actuarial framework subordinates this doctrine to quantified-risk acceptance, shifting from 'redesign until certain' to 'document probability and proceed if accepted.' This is not an agent but the normative structure whose displacement the constraint enacts.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, engineering_safety_culture, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, engineering_safety_culture).

% Serve as the ultimate targets if mission failure occurs. They are excluded from the informed-decision-maker frame — their own risk acceptance is not solicited at the gate, and they cannot withdraw crew authority. They depend on the decision-makers' judgment and have no arbiter if they believe the accepted probability is unreasonable.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, astronauts_and_crew, excluded,
    moderate, immediate, trapped, national).

% The pre-Rogers understanding that flight programs must cease operations and conduct full redesigns when failure modes are unknown or inadequately understood. This doctrine is not eliminated by the actuarial reading — it persists in parallel institutional framings — but is no longer the mandatory closure condition. Represents the analytical seat observing the constraint's shift.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, historical_engineering_doctrine, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, historical_engineering_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, nasa_administration).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single decision protocol for flight operations when technical uncertainty remains: document the failure probability, present it to qualified decision-makers, obtain informed acceptance, and proceed. Without this framework, each flight program would independently navigate the engineering-vs-mission tension with no standardized risk language or approval gateway.
% TRANSFER_FUNCTION: Transfers the locus of risk authority from engineering doctrine (no-fly-until-engineered-certain) to quantified probability and decision-maker acceptance. Mission planners and program administration gain operational authority to continue missions within accepted probability bounds; engineering safety culture loses categorical veto power over unknowns; crews and public assume quantified-probability risk instead of categorical-redesign assurance.
% ABSENT_VOICES: Astronauts and crew members are structurally absent from the informed-decision-maker circle. They would contest whether the probability bounds are acceptably low and whether their own consent should gate the decision. Families of crew, future astronauts not yet selected, and the general public whose safety is implicit in space program operations are also outside the conversation.
% DISAPPEARANCE_RATIONALE: If the actuarial-risk-acceptance framework vanished, flight programs would revert to engineering-absolute-threshold readings: all flights with known hazards would halt pending redesign, OR programs would adopt management-compliance-narrative readings (document efforts, proceed without explicit probability quantification). The entire operational posture and decision authority structure would reorganize around whichever sibling reading became dominant.
% FOUNDING_PROBLEM: The 1986 Space Shuttle Challenger disaster revealed that O-ring erosion risk was quantified internally but not presented to informed decision-makers, and that the engineering culture's categorical warnings were overridden by schedule pressure without explicit risk acceptance. The Rogers Commission mandate was to ensure that future missions proceed only with documented failure probability and documented informed acceptance by authorized decision-makers.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission itself (external authority, 1986) attests the founding problem and prescribes the actuarial framework as solution. NASA's own safety directives (post-Rogers) attest to the problem. Engineering organizations (Marshall Space Flight Center, Thiokol engineers) attest that quantification existed but was not systematized into decision gates. Dissenting voices — represented in the engineering_absolute_threshold reading — attest that quantified-risk acceptance is itself a failure mode and that engineering doctrine should remain categorical. No single voice corroborates the founding problem other than Rogers itself; internal NASA accounts vary on whether the problem is 'solved' vs. 'managed' under the actuarial framework.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).

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
 *   Extractiveness rises from 0.48 at interval start to 0.68 at end and plateaus, reflecting consolidation of the actuarial framework's authority over time (early resistance subsides, the gate becomes routine). Theater ratio rises from 0.32 to 0.58 over the interval, indicating increasing performative components: early decision-makers conduct genuine probability analysis; later decisions rely on ritualized documentation and probability language without equivalent substantive recalibration. This is Goodhart drift — the quantification proxy is optimized independently of what it was meant to track. Suppression requirement stays high (0.55–0.71) because the framework actively excludes alternative readings and crew input. Accessibility collapse is moderate (0.62) because the engineering absolute-threshold reading persists in parallel institutional structures and external safety communities; alternatives are not completely foreclosed. Resistance is moderate (0.54) because engineering organizations and external safety advocates contest the reading throughout the interval; the constraint requires continuous enforcement to maintain dominance.
 *
 * PERSPECTIVAL GAP:
 *   From mission planners' seat, the constraint is enabling: it provides a rational framework for proceeding with missions despite residual unknowns, replacing yes/no veto with quantified acceptance. From engineering safety's seat, it is constraining and extractive: it displaces the doctrine that 'unknown hazards halt programs' with 'unknown hazards are acceptable if probability is documented.' From the crew's seat (excluded), it is invisible until failure occurs — they then discover they were the target of a risk-acceptance decision they did not participate in. The engine computes divergent type classifications from these structural positions: mission planners may see rope (coordination enabling their function); engineers see snare (veto displaced); crew sees structural exposure without say in the gate. The authored claim (tangled rope) reflects the system-level view: genuine coordination (documented risk) plus genuine extraction (doctrine displacement).
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners and NASA administration benefit from operational authority — their directionality is low (beneficiaries get d ≈ 0.2–0.3); they face mobile or arbitrage exit options. Engineering safety doctrine is the victim: it loses categorical veto power and is subordinated to a quantified framework it did not author — its directionality is high (d ≈ 0.8–0.9); it is identity-locked to the engineering community (cannot exit). Informed decision-makers sit near symmetric (d ≈ 0.5) because they gain authorization authority but bear legal liability and reputational risk. The constraint is asymmetric at a system level (benefits flow to mission planners and administration, costs to engineering doctrine and crew); this asymmetry is what makes it tangled rope rather than pure rope — genuine coordination (documented risk acceptance) mixed with structured extraction (doctrine displacement).
 *
 * MANDATROPHY ANALYSIS:
 *   The actuarial framework's founding problem is real and live: quantification and informed decision-maker authorization do address the Challenger failure mode (risk accepted without documentation). But the framework carries mandatrophy risk because it can decay into theater — documentation becomes ritual, probability estimates become rhetorical, and the gate becomes a compliance symbol rather than a functional closure condition. The theater_ratio measurement (rising from 0.32 to 0.58) suggests this decay is underway. The constraint remains functionally tangled rope (real coordination plus real extraction) at interval start but drifts toward piton (performative maintenance of a coordination function that has atrophied into schedule accommodation) by interval end. The theater trajectory documents the mandatrophy in process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Rogers Commission findings is structurally authoritative: actuarial-risk-acceptance (this reading), engineering-absolute-threshold (sibling), or management-compliance-narrative (sibling)?',
    'Textual analysis of the Rogers Commission report''s actual prescriptions; institutional audit of NASA''s post-Rogers decision processes; reconstruction of which reading governed flight decisions in contested cases (e.g., Hubble servicing missions, Shuttle-ISS assembly risk approvals).',
    'If engineering-absolute-threshold is the true reading, the actuarial framework is a cover story and the constraint is a snare (extraction via false quantification), not a tangled rope. If management-compliance-narrative dominates, documentation replaces quantification and the constraint is performative. This reading''s classification hinges on which kernel reading NASA''s structures actually instantiate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest among three structurally distinct instantiations of the Rogers mandate.').

omega_variable(
    informed_decision_maker_capture,
    'Does the informed-decision-maker gate become regulatory capture when NASA administration controls who qualifies as informed and sets the threshold for acceptable probability?',
    'Track the set of decision-makers who have rejected flight proposals on probability grounds; measure whether approval thresholds shift with schedule pressure; audit the criteria for ''informed'' status and whether external voices (independent safety boards, crew representatives, scientific advisors) are included or excluded.',
    'If the decision-maker circle is administratively controlled and resistant to external input, suppression rises and the constraint moves toward snare. If the circle is genuinely multi-party with real veto power, the tangled-rope classification (asymmetric but coordinating) holds. The current stakeholder framing assumes the latter; capture would flip the governance model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_decision_maker_capture, empirical, 'Whether the informed-decision-maker gate is genuinely multi-party or captured by NASA administration.').

omega_variable(
    probability_quantification_precision,
    'Are the probabilities used in the informed-decision-maker gate actually quantified to meaningful precision, or are they rhetorical bounds that appear quantitative without substantive precision?',
    'Reconstruct the probability estimates used in specific flight-approval cases; compare estimates to post-flight outcome data to assess calibration; audit the methods used to generate estimates (engineering analysis, similarity to past failures, or scenario speculation with arbitrary confidence intervals).',
    'If probabilities are precision estimates with real evidentiary backing, the actuarial framing is genuine and the constraint coordinates risk quantification. If they are rhetorical or poorly calibrated, quantification becomes theater and the constraint is closer to management-compliance-narrative — documentation that appears to satisfy the gate while preserving schedule flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_quantification_precision, empirical, 'Whether probability quantification carries meaningful precision or is performative.').

omega_variable(
    categorical_safety_suppression,
    'Does enforcing the actuarial framework actively suppress the engineering safety doctrine, or do the two coexist in NASA''s institutional structure?',
    'Audit internal NASA safety directives post-Rogers for mentions of both actuarial risk and categorical redesign requirements; interview engineering organizations on whether they maintain independent no-fly thresholds; track whether missions that pass the actuarial gate are ever halted by engineering objections.',
    'If the frameworks actively suppress each other, the constraint is extractive (one doctrine overrides the other). If they coexist in different institutional slots, the constraint is coordinating a choice protocol without eliminating the alternative. Coexistence suggests tangled rope; suppression suggests snare or piton (theater masking the suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_safety_suppression, empirical, 'Whether actuarial and categorical safety doctrines coexist or suppress each other in NASA''s institutional structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(roge_tr_t0, observed).
narrative_ontology:measurement(roge_tr_t5, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(roge_tr_t5, observed).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(roge_tr_t10, observed).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(roge_tr_t15, observed).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 20, 0.55).
narrative_ontology:measurement_basis(roge_tr_t20, observed).
narrative_ontology:measurement(roge_tr_t25, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 25, 0.57).
narrative_ontology:measurement_basis(roge_tr_t25, observed).
narrative_ontology:measurement(roge_tr_t30, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(roge_tr_t30, observed).
narrative_ontology:measurement(roge_tr_t40, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(roge_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(roge_be_t0, observed).
narrative_ontology:measurement(roge_be_t5, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(roge_be_t5, observed).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(roge_be_t10, observed).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(roge_be_t15, observed).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(roge_be_t20, observed).
narrative_ontology:measurement(roge_be_t25, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(roge_be_t25, observed).
narrative_ontology:measurement(roge_be_t30, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(roge_be_t30, observed).
narrative_ontology:measurement(roge_be_t40, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(roge_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(roge_su_t0, observed).
narrative_ontology:measurement(roge_su_t5, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(roge_su_t5, observed).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(roge_su_t10, observed).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(roge_su_t15, observed).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(roge_su_t20, observed).
narrative_ontology:measurement(roge_su_t25, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(roge_su_t25, observed).
narrative_ontology:measurement(roge_su_t30, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(roge_su_t30, observed).
narrative_ontology:measurement(roge_su_t40, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(roge_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__actuarial_risk_acceptance, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% The Rogers Commission findings decompose into three structurally distinct constraints, each instantiating a different reading of the kernel. The actuarial-risk-acceptance reading (this story) characterizes flight operations continuing with quantified probability bounds accepted by informed decision-makers. The engineering-absolute-threshold reading characterizes operations halting until technical redesign is certified. The management-compliance-narrative reading characterizes operations continuing with documented compliance efforts. ε values differ substantially across readings: actuarial permits operations (moderate extraction of doctrine displacement), absolute-threshold forbids them (high extraction of schedule pressure, lower doctrine displacement), compliance prioritizes documentation (high theater, lower genuine extraction). The three stories are not alternative measurements of one constraint — they are three constraints instantiating one contested kernel. Network edges link them via affects_constraints to enable contamination propagation analysis: shifts in institutional authority from one reading to another should propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__actuarial_risk_acceptance, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
