% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness Regime (Hybrid Competence/Ritual Reading)
 *   domain: institutional/safety
 *
 * SUMMARY:
 *   A regional preparedness mandate bundles two structurally distinct
 *   functions under one compliance regime: mandatory engineering inspection
 *   of structural and fire-safety systems, and mandatory evacuation drills.
 *   Inspection retains real technical bite — findings are instrumented,
 *   evidence-driven, and consequential (retrofit or condemnation). Drills
 *   have calcified into scripted, low-variance rituals that certify
 *   compliance without testing adaptive response to novel failure conditions.
 *   Both are reported to regulators as a single 'preparedness compliance'
 *   metric, obscuring the divergence.
 *
 * KEY AGENTS:
 *   - structural_engineering_firms: primary beneficiary of the competent subsystem — paid for real technical work with real consequence
 *   - emergency_management_agencies: administers both subsystems, benefits from aggregate reporting that obscures the divergence
 *   - building_occupants: bear the false-security cost of ritualized drills, trapped exit options
 *   - frontline_facility_staff: run the ritual, bear liability, no design control
 *   - regulatory_auditors: analytical seat whose instrument cannot distinguish the two subsystems
 *   - insurance_underwriters: excluded seat that would have reason to differentiate premiums by subsystem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.38).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.42).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness Regime (Hybrid Competence/Ritual Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/safety").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, 'a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a').
narrative_ontology:cs_kernel_codification('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', formalized).
narrative_ontology:cs_authority_grounding('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', practice).
narrative_ontology:cs_interpretation_layer_present('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a').
narrative_ontology:cs_reading_relation('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', preparedness_persistence__husk_reading, influences).
narrative_ontology:cs_reading_relation('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', preparedness_persistence__competence_reading, influences).
narrative_ontology:cs_axiom('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', foundational, preparedness_is_structurally_heterogeneous).
narrative_ontology:cs_axiom_status(preparedness_is_structurally_heterogeneous, holdable).
narrative_ontology:cs_axiom_grounding('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', preparedness_is_structurally_heterogeneous, empirically_contingent).
narrative_ontology:cs_axiom('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', secondary, aggregate_reporting_obscures_subsystem_divergence).
narrative_ontology:cs_axiom_status(aggregate_reporting_obscures_subsystem_divergence, holdable).
narrative_ontology:cs_axiom_grounding('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', aggregate_reporting_obscures_subsystem_divergence, empirically_contingent).
narrative_ontology:cs_reference_frame('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', dual_function_founding_mandate).
narrative_ontology:cs_drift_state('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', contemporary_compliance_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9b9bd14-b99a-479d-a4eb-ea790e2e7a5a', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, structural_engineering_firms).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, building_occupants).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_facility_staff).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform periodic engineering inspections of load-bearing systems, seismic retrofits, and fire suppression infrastructure. This subsystem retains real technical rigor: inspections are instrumented, documented against physical failure modes, and update in response to new evidence (code revisions, near-miss incident data). Firms bill for this work and their findings carry real consequence — buildings get retrofitted or condemned based on it.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, structural_engineering_firms, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, structural_engineering_firms, agenda_setter).

% Administer both subsystems under one preparedness mandate — mandatory inspection regimes and mandatory evacuation drills — and report compliance as a single aggregate metric to oversight bodies. They benefit from being able to point to 'preparedness compliance' without the reporting distinguishing which component is doing the work. Could disaggregate the metric and redirect drill resources toward competence-building, but the administrative cost of overhauling drill design and retraining facilitators exceeds the diffuse benefit any single agency captures from doing so.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, regional).

% Participate in evacuation drills that have calcified into scripted, low-variance rituals — same route, same signal, no injected failure conditions, no measurement of actual response competence. They bear the cost of a false sense of readiness: in an actual event whose failure mode differs from the drilled scenario, the rehearsed script may not transfer. They have no channel to demand drills be redesigned around real contingency.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, building_occupants, payer,
    powerless, immediate, trapped, local).

% Run the drills as designated wardens or coordinators, aware from repeated experience that the drill no longer tests anything ambiguous, but are required to certify compliance regardless. Some raise concerns internally; these are typically absorbed as scheduling requests rather than acted on as a competence gap. They carry professional liability if a real event goes badly, despite not controlling drill design.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_facility_staff, payer,
    moderate, biographical, constrained, local).

% Audit compliance records for both inspection and drill subsystems, typically checking that inspections occurred and drills were logged, but rarely testing whether drills exercise genuine adaptive response. Their audit instrument does not distinguish the competent subsystem from the ritualized one, which is part of why the stratification persists undetected in official reporting.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, regulatory_auditors, observer,
    institutional, generational, analytical, national).

% Price risk partly on preparedness compliance records but are not part of the process that designs or reviews drill content. If they had visibility into the stratification, they would likely differentiate premiums or demand drill redesign — their absence from the room means this pressure never reaches the ritualized subsystem.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, insurance_underwriters, excluded,
    powerful, biographical, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces catastrophic-failure risk by maintaining two linked functions under one preparedness mandate: verified structural integrity (engineering inspection) and rehearsed emergency response (evacuation drills), so that occupants are protected from both slow-building structural failure and sudden-onset events.
% TRANSFER_FUNCTION: Moves inspection fees and compliance-administration costs from building owners/agencies to engineering firms (a genuine service purchase) and moves compliance-certification labor from frontline staff and occupants (drill time, disrupted routine) to emergency management agencies, which convert it into a reportable aggregate compliance figure without returning proportional readiness value on the drill side.
% ABSENT_VOICES: Insurance underwriters, who price real risk and would have reason to differentiate the two subsystems, are not present in preparedness governance. Building occupants have no mechanism to request drills be redesigned around genuinely unscripted contingencies.
% DISAPPEARANCE_RATIONALE: If the whole preparedness regime vanished overnight, the engineering-inspection component would need to be rebuilt immediately — structural failure risk is real and the coordination it solves is genuine, so this half of the world clearly rearranges. The drill component's disappearance is more contested: frontline staff and safety researchers argue nothing operationally would change because the drills no longer test anything; agencies argue the ritual itself has residual value (familiarity with exits, muscle memory) even in degraded form. The two subsystems would not disappear symmetrically, which is exactly the structural claim this reading makes.
% FOUNDING_PROBLEM: Historical building collapses and mass-casualty fire/evacuation failures (repeated 20th-century industrial and high-rise incidents) created pressure for mandatory structural verification and rehearsed emergency egress, on the theory that both engineering defects and human panic/disorientation were preventable causes of death.
% FOUNDING_PROBLEM_CORROBORATION: Structural failure risk is corroborated as still live by independent post-incident engineering reports (e.g., bridge and building collapse investigations) that continue to attribute deaths to inspection failures — this is outside-party attestation for the competence subsystem. For the drill subsystem, independent human-factors researchers and post-incident behavioral studies (not affiliated with emergency management agencies) report that scripted, unvaried drills correlate poorly with real evacuation performance under novel failure conditions — an outside corroboration that the founding problem for THAT subsystem has drifted from live risk-reduction toward compliance theater, even though agencies administering the drills continue to certify them as effective.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).
:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) and theater_ratio (0.47) are authored as composites: the inspection subsystem alone would score low on both (perhaps 0.1 extraction, 0.1 theater) while the drill subsystem alone would score considerably higher (perhaps 0.5+ extraction via false-security cost to occupants, 0.7+ theater). Suppression (0.42) is moderate rather than high because compliance is mandated but not adversarially enforced against genuine resistance — the mechanism persists more through administrative inertia in the drill subsystem and genuine risk-management necessity in the inspection subsystem. Accessibility_collapse (0.4) and resistance (0.48) reflect that alternatives to the current drill design are conceivable and increasingly argued for (hence real resistance), whereas the inspection subsystem faces little resistance because its function is not contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural engineering firms sit near the beneficiary end (d low) — they are paid for verified work with clear technical accountability. Emergency management agencies are agenda-setters who benefit from the aggregation itself rather than from either subsystem's function directly — the override below reflects this: their derived d as an institutional agenda_setter would default near-symmetric, but their actual structural position benefits from NOT disaggregating, which is a distinct extraction vector from either subsystem's operation. Building occupants and frontline staff are targets: they bear the false-security cost (occupants) or professional liability without design control (staff), both with constrained/trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading is specifically constructed to prevent two mislabeling errors: (1) labeling the entire preparedness bundle 'coordination working as intended' because the inspection subsystem is genuinely competent (the competence_reading's error, as this reading sees it), and (2) labeling the entire bundle 'pure ritual extraction' because the drill subsystem has hollowed out (the husk_reading's error, as this reading sees it). By localizing extraction to a specific subsystem rather than the whole mandate, this reading identifies where reform pressure should concentrate (drill redesign, decoupled reporting) without threatening the genuinely functional inspection regime that reform pressure could otherwise sweep up collaterally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_preparedness_persistence,
    'Is preparedness a single arrangement best described as uniformly competent (competence_reading), uniformly ritualized (husk_reading), or genuinely stratified by subsystem (hybrid_reading, this story)?',
    'Subsystem-disaggregated audit: measure whether inspection findings drive real retrofit/condemnation decisions (competence signal) versus whether drill content varies across cycles to test novel failure conditions (competence signal) or repeats an identical script (ritual signal). If both subsystems show the same signal, this reading is wrong and one of the siblings is correct instead.',
    'If disaggregated audit shows both subsystems are competent, this reading collapses into competence_reading (lower composite ε). If both show ritual atrophy, this reading collapses into husk_reading (higher composite ε, likely reclassifying toward pure piton or snare on the drill side). The hybrid reading is only correct if the subsystems diverge, which is what the authored metrics here assume as a working premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_preparedness_persistence, conceptual, 'Which kernel reading of preparedness_persistence is structurally correct — routes committer content per Rule 2.').

omega_variable(
    aggregation_incentive_ambiguity,
    'Does the emergency management agency''s benefit from single aggregate compliance reporting reflect deliberate strategic obfuscation, or an unexamined administrative legacy (nobody designed the aggregation to hide the divergence; it just never got disaggregated)?',
    'Internal agency records or interviews on whether disaggregated reporting was ever proposed and rejected, versus never considered.',
    'Deliberate obfuscation would push the agency''s directionality toward the beneficiary end more strongly (intentional extraction via reporting design); unexamined legacy would support treating the agency''s role as closer to inertial administration (piton-consistent) rather than active extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregation_incentive_ambiguity, empirical, 'Whether the agency''s aggregation benefit is strategic or inertial.').

omega_variable(
    drill_atrophy_threshold,
    'At what point does drill ritualization cross from ''imperfect but still net-positive practice'' into ''net-negative false security that would be better replaced with no drill at all''?',
    'Comparative outcome studies: real-event evacuation performance in facilities with scripted drills versus facilities with varied/adversarial drill design versus facilities with no drills, controlling for building type and occupant demographics.',
    'If scripted drills still outperform no drills, the extraction is real but the coordination floor (boltzmann_floor) is not zero. If scripted drills perform no better than no drills at all under novel failure conditions, the drill subsystem''s coordination claim collapses entirely and the extractiveness authored here (0.38 composite) understates the drill subsystem''s true extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_atrophy_threshold, empirical, 'Whether ritualized drills retain any residual coordination value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__hybrid_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__hybrid_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__hybrid_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__hybrid_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.47).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__hybrid_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__hybrid_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__hybrid_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__hybrid_reading, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t8, preparedness_persistence__hybrid_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(prep_su_t16, preparedness_persistence__hybrid_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__hybrid_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(prep_su_t32, preparedness_persistence__hybrid_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.42).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(prep_grid_01, preparedness_persistence__hybrid_reading, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(prep_grid_02, preparedness_persistence__hybrid_reading, accessibility_collapse(class), 40, 0.5).
narrative_ontology:measurement(prep_grid_03, preparedness_persistence__hybrid_reading, accessibility_collapse(individual), 0, 0.4).
narrative_ontology:measurement(prep_grid_04, preparedness_persistence__hybrid_reading, accessibility_collapse(individual), 40, 0.55).
narrative_ontology:measurement(prep_grid_05, preparedness_persistence__hybrid_reading, accessibility_collapse(organizational), 0, 0.28).
narrative_ontology:measurement(prep_grid_06, preparedness_persistence__hybrid_reading, accessibility_collapse(organizational), 40, 0.38).
narrative_ontology:measurement(prep_grid_07, preparedness_persistence__hybrid_reading, accessibility_collapse(structural), 0, 0.3).
narrative_ontology:measurement(prep_grid_08, preparedness_persistence__hybrid_reading, accessibility_collapse(structural), 40, 0.4).
narrative_ontology:measurement(prep_grid_09, preparedness_persistence__hybrid_reading, resistance(class), 0, 0.25).
narrative_ontology:measurement(prep_grid_10, preparedness_persistence__hybrid_reading, resistance(class), 40, 0.42).
narrative_ontology:measurement(prep_grid_11, preparedness_persistence__hybrid_reading, resistance(individual), 0, 0.3).
narrative_ontology:measurement(prep_grid_12, preparedness_persistence__hybrid_reading, resistance(individual), 40, 0.48).
narrative_ontology:measurement(prep_grid_13, preparedness_persistence__hybrid_reading, resistance(organizational), 0, 0.2).
narrative_ontology:measurement(prep_grid_14, preparedness_persistence__hybrid_reading, resistance(organizational), 40, 0.28).
narrative_ontology:measurement(prep_grid_15, preparedness_persistence__hybrid_reading, resistance(structural), 0, 0.15).
narrative_ontology:measurement(prep_grid_16, preparedness_persistence__hybrid_reading, resistance(structural), 40, 0.2).
narrative_ontology:measurement(prep_grid_17, preparedness_persistence__hybrid_reading, stakes_inflation(class), 0, 0.18).
narrative_ontology:measurement(prep_grid_18, preparedness_persistence__hybrid_reading, stakes_inflation(class), 40, 0.3).
narrative_ontology:measurement(prep_grid_19, preparedness_persistence__hybrid_reading, stakes_inflation(individual), 0, 0.12).
narrative_ontology:measurement(prep_grid_20, preparedness_persistence__hybrid_reading, stakes_inflation(individual), 40, 0.2).
narrative_ontology:measurement(prep_grid_21, preparedness_persistence__hybrid_reading, stakes_inflation(organizational), 0, 0.15).
narrative_ontology:measurement(prep_grid_22, preparedness_persistence__hybrid_reading, stakes_inflation(organizational), 40, 0.22).
narrative_ontology:measurement(prep_grid_23, preparedness_persistence__hybrid_reading, stakes_inflation(structural), 0, 0.2).
narrative_ontology:measurement(prep_grid_24, preparedness_persistence__hybrid_reading, stakes_inflation(structural), 40, 0.32).
narrative_ontology:measurement(prep_grid_25, preparedness_persistence__hybrid_reading, suppression(class), 0, 0.28).
narrative_ontology:measurement(prep_grid_26, preparedness_persistence__hybrid_reading, suppression(class), 40, 0.38).
narrative_ontology:measurement(prep_grid_27, preparedness_persistence__hybrid_reading, suppression(individual), 0, 0.32).
narrative_ontology:measurement(prep_grid_28, preparedness_persistence__hybrid_reading, suppression(individual), 40, 0.44).
narrative_ontology:measurement(prep_grid_29, preparedness_persistence__hybrid_reading, suppression(organizational), 0, 0.3).
narrative_ontology:measurement(prep_grid_30, preparedness_persistence__hybrid_reading, suppression(organizational), 40, 0.4).
narrative_ontology:measurement(prep_grid_31, preparedness_persistence__hybrid_reading, suppression(structural), 0, 0.25).
narrative_ontology:measurement(prep_grid_32, preparedness_persistence__hybrid_reading, suppression(structural), 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the preparedness_persistence kernel. competence_reading authors the bundle as uniformly functional (low composite ε); husk_reading authors it as uniformly hollowed (high composite ε, likely piton-to-snare on the drill side); this hybrid_reading disaggregates and authors divergent ε per subsystem, composited here into a single ε for the bundle-as-reported (0.38) that sits between the siblings' bundle-wide estimates. All three share the same underlying institutional facts and differ only in how they read the stratification — per DP-001 ε-invariance, each reading is its own constraint with its own stable ε, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
