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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness System (Hybrid Reading)
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   This story instantiates the 'hybrid_reading' of the
 *   preparedness_persistence kernel: it takes as its core premise that
 *   'disaster preparedness' as a colloquial label conflates two structurally
 *   distinct subsystems with divergent trajectories. Structural engineering
 *   inspection is exercised knowledge, load-tested continuously against real
 *   physical failure and independently corroborated by post-disaster
 *   forensics — it behaves like a Mountain-adjacent competent constraint.
 *   Evacuation drills, bundled under the same institutional umbrella and the
 *   same budget line, have ritualized: the completion metric (attendance) has
 *   decoupled from the outcome metric (evacuation speed under real panic
 *   conditions) it was built to produce — it behaves like a Piton, form
 *   outlasting function. This story does not adjudicate whether
 *   'preparedness' overall is competent or hollow; it asserts that the honest
 *   answer is 'both, in different subsystems,' and that treating the whole
 *   category uniformly (as either the competence_reading or husk_reading
 *   would) obscures the stratification that is the actual structural fact on
 *   the ground.
 *
 * KEY AGENTS:
 *   - structural_engineering_inspectorate: competent subsystem, institutional beneficiary of continued verified authority
 *   - emergency_management_agencies: ritualized subsystem, institutional beneficiary of compliance-signal credibility without matched operational investment
 *   - building_occupants_relying_on_evacuation_drills: powerless payers, trapped exit, bear the invisible gap between drilled confidence and real evacuation performance
 *   - municipal_budget_holders: moderate-power payers unable to differentiate high-value from low-value preparedness spend within one budget category
 *   - structural_failure_forensics_community: analytical observer, external corroborating source for the founding-problem status of each subsystem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness System (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, 'b7eb92e6-4cc4-44ba-a6e2-1c323ed15202').
narrative_ontology:cs_kernel_codification('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', distributed).
narrative_ontology:cs_authority_grounding('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', practice).
narrative_ontology:cs_interpretation_layer_present('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202').
narrative_ontology:cs_reading_relation('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', foundational, preparedness_components_have_independent_epistemic_status).
narrative_ontology:cs_axiom_status(preparedness_components_have_independent_epistemic_status, holdable).
narrative_ontology:cs_axiom_grounding('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', preparedness_components_have_independent_epistemic_status, empirically_contingent).
narrative_ontology:cs_axiom('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', secondary, bundled_labeling_obscures_differential_atrophy).
narrative_ontology:cs_axiom_status(bundled_labeling_obscures_differential_atrophy, holdable).
narrative_ontology:cs_axiom_grounding('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', bundled_labeling_obscures_differential_atrophy, empirically_contingent).
narrative_ontology:cs_reference_frame('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', unified_preparedness_doctrine).
narrative_ontology:cs_drift_state('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', contemporary_forensic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7eb92e6-4cc4-44ba-a6e2-1c323ed15202', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, structural_engineering_inspectorate).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, building_occupants_relying_on_evacuation_drills).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, municipal_budget_holders).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, preparedness_infrastructure_reduces_disaster_mortality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conducts load-bearing, seismic-retrofit, and material-fatigue inspections on a fixed technical schedule. Findings are load-tested against physical failure modes and produce binding remediation orders. This function is exercised continuously against real structural failures elsewhere in the world and remains genuinely competent — its authority is not ritual, it is verified by post-disaster forensic audits.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, structural_engineering_inspectorate, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, structural_engineering_inspectorate, beneficiary).

% Administers evacuation drills as a scheduled, low-cost compliance activity: staff walk a fixed route once or twice a year, sign an attendance sheet, and the drill is logged as complete. No scenario variation, no timing under real crowd conditions, no post-drill failure analysis feeds back into route design. Collects the credibility of 'we are prepared' without maintaining the operational capacity the drills nominally test.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, emergency_management_agencies, beneficiary).

% Participate in the ritualized evacuation drills, told the exercise validates their safety route. In an actual event they would rely on muscle memory the drill never actually built — crowd bottlenecks, blocked exits, and panic dynamics are never simulated. They bear the cost of the gap between the credibility the drill manufactures and the operational readiness it fails to build, but this cost is invisible until an actual emergency occurs.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, building_occupants_relying_on_evacuation_drills, payer,
    powerless, immediate, trapped, local).

% Fund both subsystems from the same preparedness line item, without a mechanism to distinguish the inspectorate's high-value spend from the emergency-management drill budget's low-value spend. Cannot easily redirect drill funding toward genuine readiness upgrades (route redesign, timed exercises with real crowd dynamics) because the drill's completion metric (attendance logged) already satisfies the compliance requirement the budget is checked against.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, municipal_budget_holders, payer,
    moderate, biographical, constrained, regional).

% Post-disaster investigators who examine which preparedness components actually held under real conditions. Their reports consistently validate the inspection regime (structures inspected under current codes perform as predicted) while flagging evacuation performance as decoupled from drill history — occupants who drilled regularly evacuate no faster than those who never drilled, once a real crowd-panic dynamic is present.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, structural_failure_forensics_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preparedness as a category solves a real coordination problem: aligning many independent actors (engineers, building managers, occupants, regulators) around a shared readiness standard so that disaster response does not have to be improvised from zero each time.
% TRANSFER_FUNCTION: Moves inspection compliance costs from building owners to the inspectorate (a real transfer purchasing verified structural safety) and moves compliance-signaling costs from emergency managers to building occupants (a transfer purchasing a credibility signal — 'the building is prepared' — without a matched increase in actual evacuation capacity).
% ABSENT_VOICES: Building occupants who would ask for drills tested under realistic crowd-panic conditions are not in the room when drill design is set; the drill's completion criterion (attendance) is set unilaterally by emergency-management administrators who are evaluated on compliance rates, not on evacuation-time outcomes.
% DISAPPEARANCE_RATIONALE: If the inspection regime disappeared, buildings would begin failing structurally within a measurable timeframe — the world clearly rearranges around its absence. If evacuation drills disappeared, the forensic evidence suggests actual evacuation outcomes would barely change, but the perceived-safety economy (insurance ratings, compliance certifications, occupant confidence) would rearrange sharply. The verdict is contested precisely because the two subsystems, bundled under one label, would produce opposite answers if disaggregated.
% FOUNDING_PROBLEM: Both subsystems were built to solve the same underlying problem: prevent disaster casualties by ensuring structures don't fail and people know how to leave them. Structural inspection was built to catch material and design failure before occupancy; evacuation drills were built to convert abstract emergency plans into embodied, rehearsed action.
% FOUNDING_PROBLEM_CORROBORATION: The structural-failure forensics community, whose members are professionally independent of both agencies and are not compensated by either program's continuation, corroborates that the inspection function still solves its founding problem (structures inspected under current codes perform as predicted in real failures) while the evacuation-drill function has drifted from its founding problem (drilled and undrilled populations show statistically similar evacuation performance in real events). Neither agency itself, if asked in isolation, would concede this asymmetry — the corroboration comes specifically from outside both benefiting institutions.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored moderate (0.42) rather than high because it is localized: the inspection subsystem contributes near-zero extraction (its function is intact and its cost tracks its service), while the drill subsystem contributes the bulk of the extraction (a credibility signal is sold to occupants and budget holders at a cost that no longer purchases the readiness it claims to). Averaged across the bundled category this yields a moderate figure, consistent with the expected structural delta for this reading. Theater ratio rises across the interval (0.20 to 0.48) purely from the drill subsystem's drift — the inspection subsystem's theater contribution is flat and near zero throughout, but because theater_ratio is authored at the story level (one shared grid), the rising trajectory reflects the composite. Suppression is authored moderate-low (0.38) because neither subsystem depends on coercion to persist: inspection persists because it works, drills persist because their failure mode is invisible until a real event, not because alternatives are actively blocked.
 *
 * PERSPECTIVAL GAP:
 *   From the inspectorate's seat, preparedness is a Mountain-adjacent, competently maintained technical function verified against real failure. From the emergency-management agency's seat, preparedness is functioning exactly as designed — the drill is completed, the compliance box is checked, the metric that matters to them is satisfied. From the building occupant's seat, both subsystems present identically as 'the preparedness program,' and the occupant has no visibility into which half of the bundle is real. The engine should compute these divergently: the inspectorate seat should score near-Mountain, the emergency-management seat should score Piton-adjacent (theater rising, function flat), and the occupant seat should register as bearing extraction without knowing its source.
 *
 * DIRECTIONALITY LOGIC:
 *   The inspectorate and emergency-management agencies are both authored as beneficiaries because both derive institutional standing from the preparedness label, but their directionality diverges sharply in practice: the inspectorate's benefit is earned through continuously verified performance (low effective χ despite institutional power, because the coordination function is genuine and undisputed), while the emergency-management agency's benefit increasingly rests on an unverified credibility signal (higher effective χ, because the coordination story is now serving primarily as cover for an atrophied function). Building occupants are victims specifically of the drill subsystem's drift, not of the inspection subsystem — their trapped exit option (they cannot opt out of relying on the building's official evacuation plan) amplifies their exposure to exactly the subsystem that has hollowed out.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid reading is explicitly a mandatrophy-detection device: bundling both subsystems under one 'preparedness' label and asking 'has the mandate outlived its function' produces an incoherent answer, because the mandate has NOT outlived its function for inspection but HAS for drills. Declaring the constraint as a single hybrid/piton reading, rather than forcing a binary competence-vs-husk verdict, is itself the corrective — it prevents the coordination function that is genuinely alive (inspection) from being discredited by association with the subsystem that has ritualized (drills), and prevents the ritualized subsystem from borrowing legitimacy from the competent one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_versus_uniform_reading,
    'Is ''preparedness'' genuinely stratified into distinct competent and ritualized subsystems (this reading), or does the apparent stratification collapse into a single uniform verdict once measured consistently — either fully competent (competence_reading) or fully ritualized (husk_reading)?',
    'Disaggregated post-disaster forensic comparison across many events: if inspection-subsystem performance and drill-subsystem performance track significantly different outcome curves across a large sample, the hybrid reading is supported; if they converge, one of the uniform readings is correct instead.',
    'If the hybrid reading is wrong and preparedness is uniformly competent, this story''s authored moderate extraction and rising theater_ratio should collapse toward the mountain end. If preparedness is uniformly ritualized, extraction and theater should be authored much higher across both subsystems, collapsing this story into the husk_reading''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_versus_uniform_reading, empirical, 'Whether the stratified hybrid reading, versus either uniform sibling reading, is the correct structural account of preparedness.').

omega_variable(
    drill_theater_ratio_measurement,
    'The story-level theater_ratio is authored as a composite across both subsystems even though the drift is localized entirely to the drill subsystem — is a single bundled theater_ratio measurement obscuring the magnitude of drift within the drill subsystem specifically?',
    'Decompose the story into two sibling constraints at finer grain (per the ε-invariance principle) if independent stakeholder or funding structures for inspection versus drills can be documented separately, rather than measuring theater_ratio at the bundled ''preparedness program'' level.',
    'A decomposed measurement would likely show the drill subsystem''s theater_ratio well above 0.48 by interval end, while the inspection subsystem''s would remain near 0.05-0.10 throughout — the bundled figure understates the drill subsystem''s actual hollowing and overstates the inspection subsystem''s theatricality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_theater_ratio_measurement, conceptual, 'Whether the bundled measurement of a genuinely stratified constraint masks the magnitude of drift in its worse-performing component.').

omega_variable(
    occupant_awareness_of_stratification,
    'Do building occupants have any structural means of learning which half of the ''preparedness'' bundle they are relying on, or is the stratification itself invisible to the party who bears its cost?',
    'Survey occupant understanding of the distinction between code-compliance inspection status and drill-tested evacuation readiness; check whether disclosure requirements exist for either.',
    'If occupants cannot distinguish the two subsystems, their trapped exit option is compounded by an information asymmetry that the constraint''s administrators (particularly emergency-management agencies) have no incentive to correct, since correction would expose the drill subsystem''s atrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(occupant_awareness_of_stratification, empirical, 'Whether the victim class can even perceive the stratification this reading asserts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__hybrid_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__hybrid_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__hybrid_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__hybrid_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__hybrid_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__hybrid_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__hybrid_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__hybrid_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the preparedness_persistence kernel. competence_reading and husk_reading each assert a uniform verdict across the full 'preparedness' category (fully live vs. fully hollowed); this hybrid_reading asserts the stratification itself is the structural fact, with different subsystems (inspection vs. drills) tracking the two uniform readings' premises simultaneously. All three share the same underlying institutional referent but diverge on whether disaggregation is warranted — resolving the omega 'hybrid_versus_uniform_reading' would favor one reading over the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
