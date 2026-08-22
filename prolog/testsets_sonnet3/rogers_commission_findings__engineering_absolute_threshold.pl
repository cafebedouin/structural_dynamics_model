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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission Engineering Veto: O-Ring Redesign as Absolute Flight-Readiness Threshold
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This story instantiates one reading of the Rogers Commission kernel: that
 *   the Commission's findings establish an absolute technical boundary on
 *   flight operations — the Solid Rocket Booster field joint O-ring erosion
 *   must be structurally redesigned and certified before any further launch,
 *   with no schedule, budget, or documented-risk-acceptance exception
 *   admissible. This is the engineering-absolute-threshold reading. Two
 *   sibling readings of the same Rogers findings exist as separate
 *   constraints: the actuarial_risk_acceptance reading (flight is permissible
 *   if failure probability is quantified and formally accepted by informed
 *   decision-makers) and the management_compliance_narrative reading (flight
 *   is permissible upon demonstrating a documented risk-mitigation process,
 *   independent of whether the underlying hardware risk is actually
 *   eliminated). This story does not adjudicate between the three; it authors
 *   only the absolute-threshold reading as its own ε-invariant constraint,
 *   per Rule 1.
 *
 * KEY AGENTS:
 *   - thiokol_engineers: agenda_setter (moderate/constrained) — hold the technical veto this reading vests with dispositive authority
 *   - flight_crews: beneficiary (powerless/trapped) — protected party the threshold exists to shield from undisclosed catastrophic risk
 *   - launch_schedule_stakeholders: payer (institutional/constrained) — bear cadence and budget cost of cessation
 *   - program_management: payer/agenda_setter (institutional/constrained) — administers resumption but cannot override the threshold under this reading
 *   - rogers_commission: observer (institutional/analytical) — issues the finding this reading treats as dispositive
 *   - congress_and_public: excluded (organized/mobile) — fund and politically sponsor the program without a seat in the certification room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.22).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.78).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.22).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, rope).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Engineering Veto: O-Ring Redesign as Absolute Flight-Readiness Threshold").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, '283cfbb8-47b3-4c2d-820b-13c7d05be481').
narrative_ontology:cs_kernel_codification('283cfbb8-47b3-4c2d-820b-13c7d05be481', formalized).
narrative_ontology:cs_authority_grounding('283cfbb8-47b3-4c2d-820b-13c7d05be481', expertise).
narrative_ontology:cs_interpretation_layer_present('283cfbb8-47b3-4c2d-820b-13c7d05be481').
narrative_ontology:cs_reading_relation('283cfbb8-47b3-4c2d-820b-13c7d05be481', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_reading_relation('283cfbb8-47b3-4c2d-820b-13c7d05be481', rogers_commission_findings__management_compliance_narrative, forecloses).
narrative_ontology:cs_axiom('283cfbb8-47b3-4c2d-820b-13c7d05be481', foundational, undisclosed_catastrophic_risk_to_crew_is_categorically_impermissible).
narrative_ontology:cs_axiom_status(undisclosed_catastrophic_risk_to_crew_is_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('283cfbb8-47b3-4c2d-820b-13c7d05be481', undisclosed_catastrophic_risk_to_crew_is_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('283cfbb8-47b3-4c2d-820b-13c7d05be481', foundational, physical_hazard_elimination_required_not_documentation_of_awareness).
narrative_ontology:cs_axiom_status(physical_hazard_elimination_required_not_documentation_of_awareness, holdable).
narrative_ontology:cs_axiom_grounding('283cfbb8-47b3-4c2d-820b-13c7d05be481', physical_hazard_elimination_required_not_documentation_of_awareness, instrumental).
narrative_ontology:cs_reference_frame('283cfbb8-47b3-4c2d-820b-13c7d05be481', engineering_veto_supremacy_pre_challenger).
narrative_ontology:cs_drift_state('283cfbb8-47b3-4c2d-820b-13c7d05be481', post_challenger_return_to_flight, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('283cfbb8-47b3-4c2d-820b-13c7d05be481', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crews).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, astronaut_families).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, launch_schedule_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, program_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold technical knowledge of O-ring cold-temperature erosion and are positioned, under this reading, to exercise veto authority over Flight Readiness Review certification. Their exit is constrained by employment dependency on the same contracting relationship whose launch cadence they must sometimes block; the Rogers findings retroactively vindicate the veto they were overridden on before the loss.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, thiokol_engineers, agenda_setter,
    moderate, immediate, constrained, national).

% Directly bear the physical consequence of a certification failure and have no independent means to verify hardware safety; under this reading they are the party the technical threshold exists to protect. They cannot personally inspect or contest engineering data — the threshold is the only structural protection available to them.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crews, beneficiary,
    powerless, immediate, trapped, local).

% NASA program schedulers and manifest planners bear the cost of the cessation: delayed launches, cascading manifest disruption, budget-cycle and congressional-funding exposure tied to demonstrated launch cadence. They cannot bypass the technical threshold once it is authoritatively established, and this reading treats their scheduling pressure as illegitimate grounds for exception.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, launch_schedule_stakeholders, payer,
    institutional, biographical, constrained, national).

% NASA and contractor management previously held authority to weigh schedule against engineering objection and approved launch over documented engineer concern. Under this reading their discretion is void where the technical threshold applies — they administer resumption decisions but cannot override the certification requirement, converting their prior latitude into a pure cost center.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, program_management, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, program_management, agenda_setter).

% Investigated the Challenger loss and issued findings establishing, in this reading, that the O-ring failure mode constitutes a hard technical boundary rather than an acceptable risk parameter. Its authority is retrospective and advisory but is treated by this reading as dispositive for future certification.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, rogers_commission, observer,
    institutional, generational, analytical, national).

% Fund and politically sponsor the program and bear reputational and civic cost of failures, but are not present in the Flight Readiness Review room where the technical veto would actually be exercised. They would likely endorse the absolute-threshold reading if consulted, but their voice enters only after the fact, through hearings like Rogers' own.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, congress_and_public, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__engineering_absolute_threshold, flight_crews).
narrative_ontology:fixing_cost_class(rogers_commission_findings__engineering_absolute_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared technical standard for flight readiness: hardware performance data (O-ring resiliency across the operating temperature envelope) is centralized into a single certifying judgment so that no launch proceeds while a known catastrophic failure mode remains uncorrected.
% TRANSFER_FUNCTION: Moves decision authority from program management and schedule stakeholders to engineering technical judgment; moves the cost of delay from flight crews (who would otherwise bear undisclosed physical risk) onto the launch program's schedule, budget, and manifest.
% ABSENT_VOICES: Congress and the flying public are not present in the Flight Readiness Review room where the veto is exercised; they receive the outcome (grounded fleet, redesign timeline) without participating in the technical adjudication, learning of the underlying risk calculus only through post-hoc commission findings.
% DISAPPEARANCE_RATIONALE: If the technical threshold were not binding, program management could resume launches on a documented-risk-acceptance or compliance-process basis (the sibling readings) without the redesign being physically certified — the entire post-Challenger stand-down and Solid Rocket Booster redesign program would not have been structurally mandatory, and a comparable failure mode could recur under schedule pressure.
% FOUNDING_PROBLEM: Engineers at Thiokol raised documented cold-temperature O-ring erosion concerns the night before the Challenger launch; program management proceeded to launch anyway, and the vehicle was lost with all crew, killing the launch cadence's implicit trade against undisclosed catastrophic risk.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission itself, staffed by independent investigators including Richard Feynman, corroborates from outside NASA program management that the engineering veto was overridden under schedule pressure and that the underlying failure mode was known and dismissed; NASA's own post-accident engineering directorate and the Government Accountability Office's subsequent oversight reports independently support that the technical threshold, not a risk-acceptance or compliance framing, was the Commission's operative finding. Program management and schedule stakeholders, by contrast, have historically favored the compliance-process reading, which is why this reading's status remains contested rather than settled.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction under this reading is low-to-moderate (0.22 at interval end) because the arrangement is genuinely protective rather than rent-extracting — the primary transfer is delay cost, not value capture by an identifiable rent-collector. Suppression is high (0.78-0.90) because the reading is explicitly absolute: no launch may proceed regardless of schedule pressure, congressional political cost, or program management preference, which is a strong coercive foreclosure of alternatives once the technical finding is established. Theater ratio stays low throughout (0.05 to 0.15) because under this reading the redesign-and-certify requirement is functionally load-bearing, not performative — the joint was physically redesigned (capture features, third O-ring, heater elements) and static-tested before Discovery's STS-26 return to flight, so the measured activity tracked the actual engineering work rather than compliance theater. The declining extraction trajectory reflects the interval's endpoint: as the redesign matured and certification approached, the arrangement's coercive bite (grounding an entire program) diminished because the underlying hazard was actually being retired, not merely managed.
 *
 * PERSPECTIVAL GAP:
 *   From the thiokol_engineers and flight_crews seats, this reading experiences as protective, coordination-shaped rope: a hard-won technical standard finally given teeth. From the program_management and launch_schedule_stakeholders seats, the same structural facts (high suppression, no negotiation path, mandatory cessation) would compute much closer to a costly external constraint imposed without recourse — closer to how a tangled_rope or even snare-flavored seat would read the same suppression score, even though this story's claimed_type is authored as rope from the reading's own analytical vantage. The engine's per-seat computation is expected to surface this divergence rather than collapse it.
 *
 * DIRECTIONALITY LOGIC:
 *   Flight crews are the clear structural beneficiary — the threshold exists, under this reading, precisely to protect them from a physical risk they cannot themselves detect or refuse, so they sit near the full-beneficiary end of directionality despite holding no institutional power. Thiokol engineers occupy an unusual agenda_setter position: they hold the technical knowledge the threshold operationalizes, but their exit options are constrained by contractor dependency, so their directionality is not simple beneficiary — they bear reputational and professional risk for exercising the veto (as history recorded before Challenger). Launch schedule stakeholders and program management are the targets: the absolute threshold extracts schedule and budget flexibility from them with no negotiation path once triggered, placing them near the full-target end. Congress and the public are excluded from the adjudicating room entirely, which the six_questions absent_voices field records but which does not, per R3, feed a classification override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an overridden engineering veto preceding a fatal failure — is genuinely dead in the narrow sense that the specific O-ring joint was redesigned and requalified, but the broader problem (schedule pressure overriding engineering judgment) recurred in the Columbia loss under a different technical mechanism, which is why founding_problem_status is authored as contested rather than dead: the specific hazard was retired but the underlying institutional dynamic the Commission also diagnosed was not permanently resolved by this reading's threshold alone. This reading's classification should not be read as claiming the absolute-threshold arrangement solved NASA's schedule-pressure problem generally — only that it solved the specific O-ring hazard it was aimed at.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Do the Rogers Commission findings themselves mandate an absolute technical threshold, or do they leave room for a documented-risk-acceptance or compliance-process resumption path that program management could legitimately invoke?',
    'Close textual and procedural analysis of the Commission''s nine formal recommendations and NASA''s implementation response (the ''Return to Flight'' actions), cross-referenced against subsequent oversight (GAO reports, House Committee on Science and Technology hearings) for which reading NASA''s own post-Challenger governance actually adopted in practice versus rhetoric.',
    'If the absolute-threshold reading is the one NASA''s institutional practice actually adopted (SRB redesign was physically completed and static-tested before STS-26), this reading''s low extraction and high suppression are well-grounded; if institutional practice actually reverted to a compliance-narrative resumption logic for subsequent risk decisions (as later diagnosed in the Columbia Accident Investigation Board findings), that would support the sibling management_compliance_narrative reading being the operative one going forward, without changing this reading''s own authored values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the Commission''s findings structurally require the absolute threshold this reading authors, versus permitting a compliance or risk-acceptance resumption path.').

omega_variable(
    engineer_veto_durability,
    'Does the engineering veto authority this reading establishes persist as durable institutional practice, or does it erode back toward schedule-driven discretion once the specific post-Challenger political attention fades?',
    'Longitudinal comparison of Flight Readiness Review override patterns and dissent documentation across the Shuttle program''s remaining operational life, particularly examining whether the Columbia foam-strike risk assessment process preserved or eroded the absolute-threshold precedent.',
    'If the veto authority eroded (as the Columbia Accident Investigation Board''s normalization-of-deviance findings suggest), this reading''s high suppression score describes an initial post-accident state that did not hold structurally over the program''s full lifetime — a genuine drift case rather than a stable classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineer_veto_durability, empirical, 'Whether the absolute engineering veto persisted institutionally or eroded under renewed schedule pressure.').

omega_variable(
    beneficiary_vs_natural_law_ambiguity,
    'Is the technical threshold this reading authors a discovered physical fact about O-ring cold-temperature performance (mountain-adjacent), or a constructed institutional policy choice about how much residual risk to tolerate (constructed, with flight crews as identifiable beneficiaries)?',
    'Engineering analysis distinguishing the physically demonstrated failure mechanism (documented, replicated in post-accident testing) from the policy threshold set for acceptable residual joint rotation and blow-by (a chosen margin, not a law of nature).',
    'This distinction matters because the constraint is authored as rope, not mountain — but the physical failure mode underlying it has genuine natural-law character, while the specific certification threshold chosen is a policy construct. Keeping these separate prevents the constraint from being miscategorized as either pure physics or pure politics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_vs_natural_law_ambiguity, conceptual, 'Distinguishing the discovered physical failure mechanism from the constructed certification policy built atop it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 4, 0.08).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 8, 0.1).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 16, 0.12).
narrative_ontology:measurement(roge_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.14).
narrative_ontology:measurement(roge_tr_t32, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 32, 0.15).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(roge_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.22).
narrative_ontology:measurement(roge_be_t32, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 32, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 4, 0.88).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(roge_su_t16, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 16, 0.82).
narrative_ontology:measurement(roge_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.79).
narrative_ontology:measurement(roge_su_t32, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 32, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rogers_commission_findings kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: engineering_absolute_threshold (this story, low extraction/high suppression, claimed rope), actuarial_risk_acceptance (permits flight on documented and accepted risk quantification), and management_compliance_narrative (permits flight on demonstrated compliance process independent of underlying hazard elimination). All three interpret the identical historical record and Commission text but differ in what resumption condition they treat as structurally required. Network edges link this story to both siblings to preserve the constraint-family relationship for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
