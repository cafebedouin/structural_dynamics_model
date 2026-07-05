% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission Engineering Absolute Threshold: O-Ring Redesign Certification Gate
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This story instantiates the engineering-absolute-threshold reading of the
 *   Rogers Commission findings following the 1986 Challenger disaster: the
 *   O-ring erosion failure mode identified in the investigation is treated as
 *   an unconditional flight-stop criterion, resolvable only by certified
 *   redesign, with engineering judgment holding binding veto power over the
 *   Flight Readiness Review rather than advisory input weighed against
 *   schedule. This is distinct from the actuarial_risk_acceptance reading
 *   (which treats the same findings as a documentation/quantification
 *   requirement permitting flight if risk is disclosed and accepted) and the
 *   management_compliance_narrative reading (which treats them as a
 *   process-compliance standard satisfied by demonstrated mitigation effort,
 *   not by resolution of the underlying hazard). All three readings share the
 *   same source text and the same historical trigger event but diverge
 *   sharply on where the bar sits and who holds veto power — under
 *   ε-invariance, they are authored as three separate constraint stories
 *   rather than one story with a measurement parameter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.28).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.81).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.28).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Engineering Absolute Threshold: O-Ring Redesign Certification Gate").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, '5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3').
narrative_ontology:cs_kernel_codification('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', formalized).
narrative_ontology:cs_authority_grounding('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', expertise).
narrative_ontology:cs_interpretation_layer_present('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3').
narrative_ontology:cs_reading_relation('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', rogers_commission_findings__management_compliance_narrative, forecloses).
narrative_ontology:cs_reading_relation('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_axiom('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', foundational, unresolved_hazard_bars_flight_regardless_of_documentation).
narrative_ontology:cs_axiom_status(unresolved_hazard_bars_flight_regardless_of_documentation, holdable).
narrative_ontology:cs_axiom_grounding('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', unresolved_hazard_bars_flight_regardless_of_documentation, deontological).
narrative_ontology:cs_axiom('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', foundational, engineering_technical_judgment_is_binding_not_advisory).
narrative_ontology:cs_axiom_status(engineering_technical_judgment_is_binding_not_advisory, holdable).
narrative_ontology:cs_axiom_grounding('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', engineering_technical_judgment_is_binding_not_advisory, conventional).
narrative_ontology:cs_reference_frame('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', pre_challenger_discretionary_flight_readiness_review).
narrative_ontology:cs_drift_state('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', post_columbia_caib_assessment, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5a78e1dc-2c7c-4f3f-bfe7-8efa569e1ac3', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, engineering_veto_holders).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, launch_schedule_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, contractor_program_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Thiokol and NASA propulsion engineers who identified O-ring erosion in cold-temperature static tests hold formal authority within the Flight Readiness Review to certify or withhold flight-worthiness. Under this reading, their technical finding is treated as a hard stop: no launch proceeds until the joint redesign is certified against the identified failure mode. They benefit in the sense that their professional judgment is institutionally binding rather than advisory, and their vindication after Challenger cemented this authority into subsequent review structure.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, engineering_veto_holders, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, engineering_veto_holders, beneficiary).

% Astronauts assigned to Shuttle missions have no direct voice in the Flight Readiness Review and no ability to independently verify O-ring performance data. They are structurally trapped inside whatever risk determination the review process reaches — they cannot exit the mission once assigned without professional cost, and they cannot audit the engineering judgment themselves. This reading's absolute threshold is the only structural protection available to them.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crew, beneficiary,
    powerless, immediate, trapped, local).

% NASA program management, manifest planners, and the political apparatus depending on launch cadence (commercial payload commitments, Congressional funding narratives, the Teacher in Space program timeline) absorb the full cost of an indefinite grounding. Under the absolute-threshold reading, cadence considerations carry zero weight against the certification requirement — this seat cannot negotiate the boundary, only wait it out or attempt to reclassify the finding as advisory rather than binding.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, launch_schedule_stakeholders, payer,
    powerful, biographical, constrained, national).

% Morton Thiokol management, who initially recommended against launch and then reversed under schedule pressure, bear contractual and reputational exposure under this reading: their reversal is precisely the failure mode the absolute threshold is designed to prevent from recurring. Post-Rogers, this reading forecloses their ability to override engineering objections through management channels, converting what was a negotiable internal disagreement into a fixed veto they cannot appeal past.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, contractor_program_managers, payer,
    powerful, biographical, constrained, national).

% The presidential commission investigating the Challenger disaster produced the findings this constraint interprets. It has no ongoing enforcement role but its report is the textual kernel from which this reading (and its two siblings) are drawn.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, rogers_commission, observer,
    institutional, generational, analytical, national).

% Astronauts on missions after the redesign certification were never consulted on whether the threshold was set correctly; they simply inherit whichever reading of the Rogers findings prevailed in institutional practice. If the compliance-narrative or actuarial-acceptance reading displaces this one in later practice, they bear that consequence without having had a seat in the determination.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, future_mission_crews, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a single, non-negotiable technical criterion — certified O-ring redesign — as the sole condition for resuming flight, removing the erosion failure mode from any subsequent Flight Readiness Review's discretionary weighing.
% TRANSFER_FUNCTION: Moves decision authority away from program management and schedule stakeholders and concentrates it in engineering veto holders; moves the cost of the resulting delay from flight crews (who bear residual failure risk) onto launch cadence, contractor revenue, and program political capital.
% ABSENT_VOICES: The flight crews whose safety is nominally the point are not parties to the Flight Readiness Review and have no mechanism to demand the threshold be enforced or to object if it is quietly softened in later missions; future crews inheriting whichever reading prevails are entirely absent from the determination.
% DISAPPEARANCE_RATIONALE: If this absolute-threshold reading disappeared and reverted to a discretionary risk-acceptance framework, the Flight Readiness Review would regain the ability to weigh schedule and cost against unresolved technical objections — precisely the pre-Challenger configuration the Commission identified as the causal failure. Launch cadence would recover, contractor exposure would ease, and the specific protective mechanism the reading created (a technical veto that cannot be overridden by management) would cease to exist.
% FOUNDING_PROBLEM: The Challenger disaster occurred because a known, unresolved engineering objection (O-ring cold-temperature erosion) was overridden by management under schedule pressure at the January 1986 Flight Readiness Review. The founding problem was the absence of any structural mechanism preventing a documented technical objection from being negotiated away.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission itself, an investigative body external to both NASA management and the engineering corps, attests the founding problem was real and specifically identifies the override of Thiokol engineers' launch recommendation as a proximate cause. However, subsequent NASA safety culture assessments (including the Columbia Accident Investigation Board in 2003) found that the absolute-threshold reading had eroded in practice back toward compliance-narrative and risk-acceptance framings within roughly a decade, corroborated by an investigative body independent of the beneficiaries of either reading.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).
:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.15 rising to 0.28) because the absolute-threshold reading's primary effect is a redistribution of decision authority, not a resource transfer in the conventional extractive sense — the cost it imposes (grounded fleet, lost cadence) is a genuine byproduct of a real coordination function (preventing recurrence of a documented catastrophic failure mode), not rent-seeking. Suppression starts very high (0.95) in the immediate post-Challenger period when the finding functioned as an absolute, actively enforced bar on flight operations, then eases modestly (to 0.81) as certification proceeds and institutional memory of the disaster's proximate cause begins to fade — this is the beginning of the drift toward the sibling readings that later investigations (Columbia Accident Investigation Board) documented. Theater ratio is authored low throughout (0.05-0.15) because in this reading's ideal form the veto is functionally real, not performative; the modest rise reflects early erosion signals, not wholesale capture.
 *
 * PERSPECTIVAL GAP:
 *   From the engineering veto holder seat, this structure is Rope-adjacent: a genuine coordination mechanism that finally lets a documented hazard block a decision it should have blocked all along. From the launch schedule and contractor seats, the same structure reads as Tangled Rope at minimum — a real safety function bundled with what they experience as an inflexible, costly veto that removes their traditional latitude to manage risk against schedule. The engine computing these as different per-seat types from the same structural data is the point: the story does not resolve which seat is 'right,' it exposes the asymmetry the kernel contest is about.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering veto holders are the structural agenda-setters and incidental beneficiaries: the reading vests them with binding authority they previously lacked, and their earlier warnings are vindicated. Flight crews are the intended beneficiaries but are powerless and trapped — they cannot invoke or enforce the threshold themselves; the protection is structural, not something they can claim. Launch schedule stakeholders and contractor program managers are targets: the absolute threshold directly overrides their prior ability to weigh schedule against unresolved technical risk, converting a previously negotiable tradeoff into a fixed cost they must simply bear.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unresolved engineering objections being overridden by schedule pressure) is authored as contested rather than resolved, because the corroborating record (CAIB 2003) shows the absolute-threshold reading's suppression softened within roughly a decade — precisely the drift pattern that would let a later Columbia-style override recur under a different technical objection. This constraint does not claim the mandate is dead; it documents that its enforcement strength (suppression_requirement) declined even while the underlying founding problem (silent erosion of engineering veto authority under schedule pressure) remained live, which is the standard mandatrophy risk signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Did the Rogers Commission''s findings themselves mandate the absolute-threshold reading, or did NASA''s post-Challenger institutional response construct this reading from findings that were actually more procedural/documentary in the Commission''s own language?',
    'Close textual analysis of the Rogers Commission report''s specific recommendations versus NASA''s internal post-1986 Flight Readiness Review policy revisions, cross-referenced against contemporaneous congressional testimony distinguishing what the Commission required from what NASA chose to implement.',
    'If the absolute-threshold reading is a NASA institutional choice rather than a direct Commission mandate, this constraint''s claimed_type shifts from a Rope-flavored coordination mechanism toward a self-imposed Scaffold (temporary institutional commitment) that could legitimately sunset — undermining the ''permanent veto'' framing this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the absolute threshold originates in the Commission''s text or in NASA''s subsequent institutional interpretation of it.').

omega_variable(
    sibling_reading_displacement,
    'At what point, and through what mechanism, did institutional practice shift from this absolute-threshold reading toward the actuarial_risk_acceptance or management_compliance_narrative readings documented by later investigations (e.g., foam-strike risk acceptance preceding Columbia)?',
    'Historical trace of Flight Readiness Review decision records and waiver processes from 1988 (return to flight) through 2003 (Columbia), identifying the specific junctures where a hazard was accepted as documented risk rather than resolved as a hard stop.',
    'A clear displacement trajectory would corroborate the founding_problem_status of ''contested'' moving toward ''dead in practice, live in doctrine'' — a specific and citable mandatrophy pattern rather than a general suspicion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_displacement, empirical, 'Tracing the historical transition from this reading to its sibling readings in actual NASA practice.').

omega_variable(
    beneficiary_status_of_flight_crew,
    'Is ''flight crew safety'' correctly modeled as a beneficiary group, given that flight crews have no agency in invoking, monitoring, or enforcing the threshold on their own behalf?',
    'Compare against cases where crew members (e.g., through the Astronaut Office) did or did not have documented input into Flight Readiness Review risk acceptance decisions in the years following Challenger.',
    'If crew input was systematically absent, ''beneficiary'' may overstate their structural position relative to a more passive ''protected-but-voiceless'' status, which would not change the classification but would sharpen the absent_voices analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_status_of_flight_crew, conceptual, 'Whether flight crews function as genuine beneficiaries or as passive, voiceless protected parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 4, 0.06).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 8, 0.08).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 12, 0.1).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 16, 0.12).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 20, 0.14).
narrative_ontology:measurement(roge_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 4, 0.18).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(roge_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 4, 0.9).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(roge_su_t16, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 16, 0.83).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(roge_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the rogers_commission_findings kernel, decomposed per the ε-invariance principle: engineering_absolute_threshold (this story, ε≈0.15-0.28, low-extraction Rope/Tangled-Rope-flavored coordination), management_compliance_narrative (higher theater ratio, process-substitutes-for-substance dynamic, likely Piton-flavored), and actuarial_risk_acceptance (moderate extraction, risk transferred to crew via documented-but-accepted probability, likely Tangled-Rope-flavored). Each reading has a distinct ε and distinct victim/beneficiary structure and must not be averaged into a single measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
