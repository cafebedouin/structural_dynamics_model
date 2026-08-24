% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Rogers Commission Engineering Absolute Threshold: Flight Cease Until O-Ring Redesign Certified
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission's central engineering finding was that the Solid
 *   Rocket Booster O-ring design was fundamentally flawed and could not be
 *   made safe for flight at low temperatures without physical redesign. This
 *   reading instantiates that finding as an absolute technical threshold: no
 *   flight until the hardware is physically redesigned and certified.
 *   Engineers hold veto authority over Flight Readiness Reviews. The
 *   constraint coordinates safety by eliminating management discretion to
 *   accept known catastrophic risks. It extracts from launch cadence (delays,
 *   cost, schedule pressure) to purchase crew safety. The claimed type is
 *   tangled_rope: genuine coordination (single unambiguous safety gate) plus
 *   asymmetric extraction (program management bears delay costs) plus active
 *   enforcement (engineering veto).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.65).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.85).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.65).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Engineering Absolute Threshold: Flight Cease Until O-Ring Redesign Certified").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, 'b504f8c5-7ad6-40b6-8d3f-652be73a5fca').
narrative_ontology:cs_kernel_codification('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', formalized).
narrative_ontology:cs_authority_grounding('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', expertise).
narrative_ontology:cs_interpretation_layer_present('b504f8c5-7ad6-40b6-8d3f-652be73a5fca').
narrative_ontology:cs_reading_relation('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_reading_relation('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', rogers_commission_findings__management_compliance_narrative, forecloses).
narrative_ontology:cs_axiom('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', foundational, flight_safety_boundary_is_absolute_not_probabilistic).
narrative_ontology:cs_axiom_status(flight_safety_boundary_is_absolute_not_probabilistic, holdable).
narrative_ontology:cs_axiom_grounding('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', flight_safety_boundary_is_absolute_not_probabilistic, empirically_contingent).
narrative_ontology:cs_axiom('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', secondary, engineering_veto_required_for_known_catastrophic_hazards).
narrative_ontology:cs_axiom_status(engineering_veto_required_for_known_catastrophic_hazards, holdable).
narrative_ontology:cs_axiom_grounding('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', engineering_veto_required_for_known_catastrophic_hazards, empirically_contingent).
narrative_ontology:cs_reference_frame('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', absolute_technical_safety_threshold).
narrative_ontology:cs_drift_state('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', post_columbia_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b504f8c5-7ad6-40b6-8d3f-652be73a5fca', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, astronaut_office).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, launch_operations_directorate).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, program_management).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_engineering).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_management).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, engineering_authority_over_flight_readiness).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, physical_redesign_requirement_over_risk_acceptance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Astronauts who fly the vehicle. Their lives depend on the integrity of the O-ring seal. They have no authority over Flight Readiness Reviews and cannot exit the program without ending their careers. They are the primary beneficiaries of the absolute safety threshold.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crew, beneficiary,
    powerless, biographical, trapped, local).

% The collective body of NASA astronauts. They advocate for crew safety within the organization but hold no formal veto. Their professional identity is fused with the flight program, making exit nearly unthinkable. They benefit from the engineering veto but cannot enforce it themselves.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, astronaut_office, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, astronaut_office, observer).

% NASA engineering organizations (Marshall, Johnson, Headquarters) that hold veto authority over Flight Readiness Reviews post-Rogers. They set and enforce the technical safety boundary. Their authority derives from demonstrated technical competence. They can move to other engineering roles but their institutional position gives them structural power over launch decisions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, nasa_engineering, agenda_setter,
    institutional, generational, analytical, national).

% Contractor engineers who originally raised the O-ring temperature concern and were overruled pre-Challenger. Post-Rogers, they hold shared veto authority through the redesigned joint verification process. They bear redesign costs and schedule pressure but gained structural authority. Exit means leaving the contract or industry.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_engineering, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_engineering, payer).

% NASA program management (Level I/II) responsible for launch cadence, budget, and schedule. They bear the costs of delays, redesign, and standby operations. They formally accept the engineering veto but face institutional pressure to maintain flight rate. Exit is constrained by career investment in the program.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, program_management, payer,
    institutional, biographical, constrained, national).

% KSC launch operations teams who execute the countdown. They absorb the operational disruption of holds, scrubs, and redesign verification. Their performance metrics tie to launch rate. They have technical knowledge but no decision authority over the threshold.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, launch_operations_directorate, payer,
    organized, immediate, constrained, local).

% Contractor management who overruled their engineers pre-Challenger. Post-Rogers, they fund and execute the redesign under NASA oversight. They bear financial and reputational costs. Their exit options are constrained by the sole-source nature of the SRB contract and corporate identity.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, morton_thiokol_management, payer,
    powerful, biographical, constrained, national).

% ASAP (Aerospace Safety Advisory Panel) and other independent review bodies. They audit the implementation of the absolute threshold but hold no operational authority. Their analytical exit is unconstrained; they observe and report.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, external_safety_panels, observer,
    analytical, generational, analytical, national).

% Congress and GAO who investigate after failures but are structurally excluded from Flight Readiness Reviews. They would object to both excessive caution (schedule/cost) and insufficient safety, but their voice enters only post-hoc through hearings and legislation.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, congressional_oversight, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a single, unambiguous technical go/no-go criterion for flight readiness: the physical redesign of a known catastrophic failure mode must be certified before any crewed flight proceeds. This replaces probabilistic risk assessment with a deterministic engineering gate.
% TRANSFER_FUNCTION: Moves launch opportunities, schedule margin, and program budget from the launch operations directorate and program management to the engineering organizations and contractor redesign teams. The transfer is time (delay), money (redesign cost), and authority (engineering veto over management proceed).
% ABSENT_VOICES: The flight crew themselves are present only as beneficiaries, not decision-makers. Congressional oversight and the public are excluded from the Flight Readiness Review room. The Rogers Commission's own recommendation for an independent safety oversight office was not implemented as a standing FRR participant.
% DISAPPEARANCE_RATIONALE: If the absolute threshold vanished overnight, NASA would revert to probabilistic risk acceptance for known failure modes. Launch cadence would accelerate, engineering veto would become advisory, and the next O-ring-type failure would likely fly with documented risk acceptance rather than physical redesign. The Shuttle program's safety culture would fundamentally reorganize around management discretion.
% FOUNDING_PROBLEM: The Challenger disaster revealed that NASA's Flight Readiness Review process allowed management to accept known, unquantified risks (O-ring erosion at low temperature) over engineering objections. The founding problem was the absence of a structural mechanism that could force physical redesign before flight when engineers identified a catastrophic hazard.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission report itself (presidential commission, not NASA) attests the founding problem. The NASA engineering community attests it remains live (citing Columbia foam debris as analogous). NASA management of the 1990s-2000s attested it was resolved by new processes; the CAIB (Columbia Accident Investigation Board) contradicted this, finding the same cultural pattern. External corroboration comes from the CAIB and independent safety scholars.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.65) reflects the real but bounded transfer from schedule/budget to safety: the program pays in time and money for physical redesign, but the transfer is capped by the certification endpoint. Suppression (0.85) is high because the constraint's persistence depends on actively maintaining engineering veto power against institutional pressure to fly. Theater (0.15) is low because the safety function is genuine and the veto is real, though post-Columbia analysis shows some ceremonial compliance. Accessibility collapse (0.9) is near-total: once the absolute threshold is understood, no alternative path to flight exists without physical redesign. Resistance (0.4) is moderate: management resisted the veto initially but the constraint survived Columbia, indicating institutionalization.
 *
 * PERSPECTIVAL GAP:
 *   From the engineering seat, the constraint is a rope (pure coordination: one unambiguous safety gate). From the program management seat, it is a snare (pure extraction: schedule held hostage to engineering perfectionism). From the crew seat, it is a mountain (immutable physical law). The engine computes this divergence from the structural data: same constraint, different effective extraction per seat. The authored claimed_type (tangled_rope) captures the system-level hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Flight crew and astronaut office are structural beneficiaries (d near 0): the constraint subsidizes their survival. NASA engineering and Morton Thiokol engineering are agenda_setters with analytical exit (d near 0.3): they administer the constraint and gain professional authority. Program management and launch operations are payers with constrained exit (d near 0.8): they bear delay costs but cannot leave the program easily. Morton Thiokol management are payers with constrained exit (d near 0.75): they fund redesign but hold the contract. Congressional oversight is excluded (d undefined): they bear political cost of failure but have no FRR seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by making the coordination function explicit (single technical gate) and the extraction explicit (management bears delay costs). The mandated redesign was completed (1987-1988), but the veto authority persists as a standing FRR requirement for any similar hazard. This is not mandatrophy: the coordination function (preventing management risk acceptance of known catastrophic hazards) remains live, as Columbia demonstrated. The constraint has not atrophied into performance; the veto is exercised.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the Rogers Commission report text support an absolute engineering threshold, a risk quantification requirement, or a compliance process as the binding interpretation?',
    'Textual analysis of the Commission''s specific recommendations (especially Recommendation I on SRB redesign and Recommendation II on Shuttle management structure) cross-referenced with NASA''s formal implementation in NSTS 07700 and FRR procedures.',
    'If the report text forecloses probabilistic acceptance, the actuarial reading is a later construction. If the report is ambiguous, all three readings coexist as legitimate interpretations, and the engineering absolute threshold is a political achievement of the post-Challenger engineering coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel text itself contains one determinate meaning or underdetermines the three readings.').

omega_variable(
    physical_vs_social_boundary,
    'Is the O-ring temperature limit a genuine physical law (Mountain) or a socially constructed engineering judgment that hardens into a constraint?',
    'Materials science analysis: does the O-ring sealing physics exhibit a sharp phase transition at the certified temperature, or a probabilistic degradation curve? If the latter, the ''absolute'' threshold is a social choice of safety margin.',
    'If physical phase transition, the constraint is a Mountain and the engineering veto merely recognizes nature. If probabilistic curve, the threshold is a chosen safety factor and the constraint is Tangled Rope (coordination on a chosen margin + extraction from schedule).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(physical_vs_social_boundary, empirical, 'Whether the technical boundary is physically absolute or socially chosen.').

omega_variable(
    veto_authority_persistence,
    'Does the engineering veto authority established post-Challenger remain structurally real, or has it become ceremonial (theater) under schedule pressure?',
    'Longitudinal analysis of FRR dissent records: count of engineering ''no-go'' votes that stopped a flight vs. overruled dissents. Compare pre-Challenger, post-Challenger pre-Columbia, and post-Columbia eras.',
    'If veto is routinely exercised and respected, the constraint remains Tangled Rope. If veto exists only on paper and management proceeds regardless, theater_ratio rises and the constraint drifts toward Piton (inertial performance of safety).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_authority_persistence, empirical, 'Whether the active enforcement mechanism (engineering veto) remains functional over the interval.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression (0.85) structural (formal FRR veto rules, contractual redesign requirements) or internalized (engineers self-censor, management learns not to ask)?',
    'Post-exit suppression trajectory: track engineers who left NASA/contractor roles. If they continue to advocate absolute thresholds in new contexts, suppression was structural. If they adopt risk-acceptance framing, suppression was partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agents. If structural, suppression drops when the formal mechanism is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the engineering-management relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_eng_abs_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rogers_eng_abs_tr_t5, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 5, 0.12).
narrative_ontology:measurement(rogers_eng_abs_tr_t10, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 10, 0.14).
narrative_ontology:measurement(rogers_eng_abs_tr_t17, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 17, 0.18).
narrative_ontology:measurement(rogers_eng_abs_tr_t25, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 25, 0.16).
narrative_ontology:measurement(rogers_eng_abs_tr_t38, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 38, 0.15).

% Extraction over time
narrative_ontology:measurement(rogers_eng_abs_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(rogers_eng_abs_be_t5, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(rogers_eng_abs_be_t10, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(rogers_eng_abs_be_t17, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 17, 0.68).
narrative_ontology:measurement(rogers_eng_abs_be_t25, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(rogers_eng_abs_be_t38, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 38, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rogers_eng_abs_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(rogers_eng_abs_su_t5, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(rogers_eng_abs_su_t10, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(rogers_eng_abs_su_t17, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 17, 0.88).
narrative_ontology:measurement(rogers_eng_abs_su_t25, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 25, 0.85).
narrative_ontology:measurement(rogers_eng_abs_su_t38, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 38, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__engineering_absolute_threshold, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, columbia_accident_investigation_board_findings).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, nasa_flight_readiness_review_process).

% DUAL FORMULATION NOTE:
% This constraint decomposes the Rogers Commission findings into the engineering absolute threshold reading. The sibling readings (actuarial_risk_acceptance, management_compliance_narrative) instantiate different structural interpretations of the same kernel. This reading's ε (0.65) differs from the actuarial reading's ε (estimated ~0.35, lower extraction as risk acceptance is cheaper) and the compliance narrative's ε (estimated ~0.25, lowest extraction as documentation is cheapest). The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__engineering_absolute_threshold, organized, 0.35).
constraint_indexing:directionality_override(rogers_commission_findings__engineering_absolute_threshold, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
