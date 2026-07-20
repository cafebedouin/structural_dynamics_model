% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Competence Reading of Preparedness Persistence
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the competence_reading of the
 *   preparedness_persistence kernel, which treats institutional drills and
 *   inspections as live exercised knowledge that maintains operational
 *   readiness over time. The reading asserts that preparedness is not merely
 *   recorded in manuals or embodied in static infrastructure, but must be
 *   actively practiced to persist. The constraint coordinates distributed
 *   personnel across turnover and time, ensuring that response capabilities
 *   do not atrophy. The sibling husk_reading claims this activity has become
 *   empty ritual, while hybrid_reading sees a mix; this reading forecloses
 *   the husk interpretation by asserting genuine competence transfer.
 *
 * KEY AGENTS:
 *   - emergency_management_officials (agenda_setter / institutional / constrained) â designs and enforces drill protocols
 *   - operational_personnel (beneficiary / moderate / constrained) â bears practice costs and receives competence
 *   - public_at_risk (beneficiary / powerless / trapped) â downstream beneficiary of maintained readiness
 *   - regulatory_oversight_bodies (observer / institutional / analytical) â audits compliance from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.12).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.18).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Competence Reading of Preparedness Persistence").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '45f8b6a7-b055-48d6-bcd1-406d9bbf97b8').
narrative_ontology:cs_kernel_codification('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', formalized).
narrative_ontology:cs_authority_grounding('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', practice).
narrative_ontology:cs_interpretation_layer_present('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8').
narrative_ontology:cs_reading_relation('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', preparedness_persistence__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', foundational, live_exercise_preserves_competence).
narrative_ontology:cs_axiom_status(live_exercise_preserves_competence, holdable).
narrative_ontology:cs_axiom_grounding('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', live_exercise_preserves_competence, empirically_contingent).
narrative_ontology:cs_axiom('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', foundational, operational_readiness_requires_regular_practice).
narrative_ontology:cs_axiom_status(operational_readiness_requires_regular_practice, holdable).
narrative_ontology:cs_axiom_grounding('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', operational_readiness_requires_regular_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', operational_competence_baseline).
narrative_ontology:cs_drift_state('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('45f8b6a7-b055-48d6-bcd1-406d9bbf97b8', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_management_officials).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, operational_personnel).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, public_at_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers drill protocols and inspection schedules; maintains the institutional commitment to readiness; believes that regular practice sustains operational competence and prevents skill decay across the organization.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_management_officials, agenda_setter,
    institutional, generational, constrained, national).

% Participates in regular drills and equipment inspections; exercises emergency protocols in simulated conditions; depends on maintained competence for personal safety and effective performance during actual incidents.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, operational_personnel, beneficiary,
    moderate, biographical, constrained, regional).

% Relies on emergency services that maintain readiness through drills and inspections; receives downstream protection from competent response during disasters; cannot opt out of depending on institutional preparedness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, public_at_risk, beneficiary,
    powerless, biographical, trapped, national).

% Audits compliance with preparedness mandates from outside the operational chain; reviews drill records and inspection logs; evaluates whether institutions meet legally prescribed readiness standards.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, regulatory_oversight_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains distributed operational competence across time and personnel turnover by requiring regular collective practice of emergency protocols and physical inspection of readiness infrastructure.
% TRANSFER_FUNCTION: Moves time, attention, and labor from routine operations into rehearsal and verification activities; moves competence from experienced personnel to newer personnel through repeated joint practice.
% ABSENT_VOICES: Personnel who experience drills as bureaucratic burden rather than learning opportunities; alternative readiness models such as just-in-time immersive simulation or decentralized mutual-aid networks are underrepresented in mandate design.
% DISAPPEARANCE_RATIONALE: Without regular drills and inspections, operational competence would decay due to skill atrophy, equipment degradation, and personnel turnover; emergency response would shift toward ad-hoc improvisation, increasing catastrophic risk.
% FOUNDING_PROBLEM: Emergency response skills and equipment readiness decay without regular exercise; institutional memory fragments across personnel turnover; no individual responder can maintain system-wide readiness alone.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster-sociology research, post-incident after-action reviews, and safety-science literature attest that response effectiveness correlates with training frequency and recency; these sources are outside the direct beneficiary set of drill-administering agencies.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).
:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the arrangement moves resources into genuine coordination rather than rent collection. Suppression is low (0.18) because participation, while mandatory, is understood as professionally necessary rather than coercively enforced. Theater ratio is minimal (0.08) under the competence reading, which asserts functional rather than performative maintenance. Accessibility collapse is moderate (0.42): alternatives to regular practice exist (simulation, just-in-time training) but are understood to be inferior for distributed team coordination. Resistance is low (0.10) because the founding problem remains live and broadly recognized. Measurements track a stable, low-extraction trajectory over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is not between beneficiary and victim seats (there are no victims in this reading) but between this reading and its sibling kernels. From the competence_reading, the constraint is rope: net-beneficial coordination with low theater. From the husk_reading, the same observed behavior would compute as piton or snare: high theater, extraction of labor for no readiness return. The gap is observer-relative because the same drills are read through different commitment frameworks.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared agents are either agenda_setters or beneficiaries. Emergency management officials administer the constraint and benefit from institutional legitimacy; operational personnel bear participation costs but receive maintained competence and safety; the public receives downstream protection. No victim group is declared, so directionality derivation produces uniformly low d (beneficial/sub symmetric) across all seats. The lack of a payer-who-is-extracted is structurally constitutive of the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is avoided because the founding problem (skill decay and memory fragmentation) is live, and the disappearance verdict is world_rearranges. The constraint is not a zombie: it would be missed if it vanished, and its justification is current function rather than historical precedent. If the founding problem were dead but the drills persisted, the constraint would drift toward piton; the low theater ratio and live founding problem status prevent this misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the observed preparedness activity live exercised knowledge (competence_reading), empty memorial performance (husk_reading), or a stratified mix (hybrid_reading)?',
    'Longitudinal comparison of drill-exercised units versus non-drilled controls under actual incident conditions; ethnographic observation of drill engagement versus ritual compliance.',
    'If husk_reading or hybrid_reading is correct, base_extractiveness and theater_ratio would be substantially higher, likely shifting classification from rope to piton or tangled_rope; the competence_reading''s low-extraction claim would be falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity between competence, ritual, and hybrid readings of the preparedness persistence kernel.').

omega_variable(
    competence_measurement_validity,
    'Can operational readiness be verified independently of drill participation metrics and self-assessment?',
    'Objective performance benchmarking under simulated high-fidelity incident scenarios, blind to drill history.',
    'If readiness is not independently verifiable, the competence reading''s empirical foundation weakens and the constraint approaches identity_coordination or theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_measurement_validity, empirical, 'Whether preparedness competence has an external measurement standard.').

omega_variable(
    participation_cost_asymmetry,
    'Do the personnel who bear the primary participation costs (time, physical risk in drills) receive proportionate readiness benefits, or is the benefit captured diffusely by the institution?',
    'Agent-level outcome tracking linking individual drill exposure to incident performance and safety outcomes.',
    'If costs fall on personnel while benefits accrue institutionally without returning to personnel, directionality shifts toward extraction despite the coordination frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(participation_cost_asymmetry, empirical, 'Distribution of costs and benefits across drill participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__competence_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__competence_reading, theater_ratio, 16, 0.06).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__competence_reading, theater_ratio, 24, 0.07).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__competence_reading, theater_ratio, 32, 0.07).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__competence_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__competence_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__competence_reading, base_extractiveness, 16, 0.11).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__competence_reading, base_extractiveness, 24, 0.12).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__competence_reading, base_extractiveness, 32, 0.12).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__competence_reading, base_extractiveness, 40, 0.13).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_persistence kernel. The kernel decomposes into multiple constraints because the observable evaluation of drills (performance under incident conditions versus ritual compliance metrics) yields different epsilon values. Each reading carries its own structural data and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
