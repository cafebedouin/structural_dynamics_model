% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Preparedness Drills and Inspections as Live Exercised Knowledge
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint story represents the competence_reading of the
 *   preparedness_persistence kernel: the claim that drills and inspections
 *   function as live exercised knowledge that genuinely maintains operational
 *   readiness. The reading asserts a coordination function (Rope) —
 *   converting static plans into tested, interoperable capability through
 *   repeated practice — with no extraction structure. Physical infrastructure
 *   standards (building codes, redundant systems) constitute a separate
 *   Mountain constraint; this story addresses the practice/coordination layer
 *   only. Metrics reflect low extractiveness, low suppression, low theater —
 *   the regime persists because participants experience genuine coordination
 *   value, not because they are coerced or deceived.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.12).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.08).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Preparedness Drills and Inspections as Live Exercised Knowledge").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '0b294f58-8dc2-49d4-8404-149e7821daa0').
narrative_ontology:cs_kernel_codification('0b294f58-8dc2-49d4-8404-149e7821daa0', distributed).
narrative_ontology:cs_authority_grounding('0b294f58-8dc2-49d4-8404-149e7821daa0', practice).
narrative_ontology:cs_interpretation_layer_present('0b294f58-8dc2-49d4-8404-149e7821daa0').
narrative_ontology:cs_reading_relation('0b294f58-8dc2-49d4-8404-149e7821daa0', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b294f58-8dc2-49d4-8404-149e7821daa0', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('0b294f58-8dc2-49d4-8404-149e7821daa0', foundational, live_exercise_maintains_operational_readiness).
narrative_ontology:cs_axiom_status(live_exercise_maintains_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('0b294f58-8dc2-49d4-8404-149e7821daa0', live_exercise_maintains_operational_readiness, empirically_contingent).
narrative_ontology:cs_axiom('0b294f58-8dc2-49d4-8404-149e7821daa0', secondary, institutional_memory_requires_continuous_practice).
narrative_ontology:cs_axiom_status(institutional_memory_requires_continuous_practice, holdable).
narrative_ontology:cs_axiom_grounding('0b294f58-8dc2-49d4-8404-149e7821daa0', institutional_memory_requires_continuous_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('0b294f58-8dc2-49d4-8404-149e7821daa0', operational_readiness_through_continuous_practice).
narrative_ontology:cs_drift_state('0b294f58-8dc2-49d4-8404-149e7821daa0', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('0b294f58-8dc2-49d4-8404-149e7821daa0', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, critical_infrastructure_operators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, protected_communities).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, first_responder_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, critical_infrastructure_operators).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, operational_readiness_through_continuous_practice).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, institutional_memory_requires_live_exercise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, mandate, and oversee drill and inspection regimes. Their organizational legitimacy depends on demonstrating that exercises translate to real-world capability. They bear the cost of designing and administering the regime but collect the legitimacy benefit when it works.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Run mandated drills and inspections on their systems (power grids, water treatment, transport). They pay the direct operational costs of exercises but gain validated readiness and regulatory compliance. Their exit is constrained by licensing and public safety obligations.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, critical_infrastructure_operators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, critical_infrastructure_operators, payer).

% Participate in multi-agency exercises that build interoperability and muscle memory. They gain coordination capability and trust across agencies. Exit is mobile — they could train independently but lose the cross-agency coordination value.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, first_responder_organizations, beneficiary,
    organized, biographical, mobile, local).

% Receive the downstream safety benefit when drills translate to effective response. They have no direct role in designing or running exercises and cannot exit the risk environment — their safety depends on the regime's genuine competence.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, protected_communities, beneficiary,
    moderate, generational, trapped, local).

% Evaluate whether exercises and inspections actually validate capability versus checking boxes. They provide the external verification that the coordination mechanism is functioning as claimed, not merely performing compliance.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, independent_auditors_inspectors, observer,
    moderate, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes distributed organizational knowledge into tested, interoperable operational capability — ensures that when a disaster occurs, multiple agencies and systems can act together without prior ad-hoc negotiation.
% TRANSFER_FUNCTION: Moves organizational attention, staff time, and operational budget into exercised rehearsal; the return is validated coordination protocols and identified failure modes before they occur in live events.
% ABSENT_VOICES: Small municipalities and volunteer organizations that lack resources for full-scale exercises but would bear disproportionate failure consequences; future populations who inherit the preparedness posture but have no say in its design.
% DISAPPEARANCE_RATIONALE: If drill and inspection regimes vanished, inter-agency coordination would degrade to ad-hoc improvisation during crises, failure modes would go undiscovered until live events, and the institutional memory linking current operators to past lessons would sever — disaster outcomes would worsen measurably.
% FOUNDING_PROBLEM: After major disasters (e.g., 1906 San Francisco earthquake, 1947 Texas City disaster), investigations revealed that written plans alone failed because organizations had never practiced together — coordination collapsed under stress. The drill/inspection regime was built to convert static plans into exercised capability.
% FOUNDING_PROBLEM_CORROBORATION: After-action reports from recent major events (Hurricane Katrina, 2011 Tohoku earthquake, 2023 Maui wildfires) consistently cite exercised interoperability as a decisive factor in effective response; the National Academy of Sciences 2022 review of preparedness research confirms live exercise as the highest-evidence readiness intervention, attested by researchers outside the benefiting agencies.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.12) because the costs of participation (staff time, operational disruption) are reciprocated in validated readiness — the transfer is mutual, not asymmetric. Suppression is minimal (0.08) because compliance is driven by professional norms and demonstrated utility, not enforcement machinery. Theater ratio is low (0.15) because exercises are designed to stress systems honestly; the slight rise over the interval reflects growing compliance paperwork, not a shift to performative ritual. Accessibility collapse (0.35) is moderate: alternatives (tabletop exercises, simulation-only) exist but lack the cross-agency friction that live exercises reveal. Resistance (0.18) is low because the primary participants (emergency managers, operators) are the regime's architects and beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   The competence reading computes as Rope from every seat because the structural data declares beneficiaries but no victims, no active enforcement, and low theater. The husk_reading would compute differently (higher theater, higher suppression) because it sees the same drills as performative. The hybrid_reading would show seat divergence — engineering inspections as Rope, evacuation drills as Tangled Rope or Snare. This story authors the competence reading's structural truth; the engine will compute per-seat types from this data.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies are agenda_setters with institutional power — they set the regime and collect legitimacy (d near beneficiary end). Critical infrastructure operators are dual-role: they pay direct costs but gain validated readiness and regulatory standing (d near symmetric). First responders are beneficiaries with mobile exit — they gain interoperability but could train alone (d beneficiary). Protected communities are trapped beneficiaries — they cannot exit the risk but gain safety if the regime works (d beneficiary but structurally vulnerable). Independent auditors are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination collapse under stress) remains live — recent disasters confirm that unexercised plans fail. The regime has not outlived its function; mandating exercises continues to solve the coordination problem it was built for. No mandatrophy is present in this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the competence_reading accurately describe the entire drill/inspection regime, or does the hybrid_reading capture a real stratification where some components (engineering inspections) remain competent while others (evacuation drills) have become ritualized?',
    'Component-level after-action analysis: measure exercise fidelity and outcome correlation separately for engineering inspections vs. evacuation drills vs. tabletop exercises. If evacuation drills show high theater and low outcome correlation while inspections show low theater and high correlation, the hybrid reading is structurally validated and the competence reading applies only to a subset.',
    'If hybrid is validated, the competence_reading overgeneralizes — it describes a proper subset of the regime (inspections) as if it were the whole. The kernel would decompose into three constraints: inspection_competence (Rope), evacuation_ritual (Snare/Piton), and tabletop_coordination (Rope/Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the competence reading''s unitary Rope claim holds across all drill/inspection types or whether the regime is stratified as the hybrid reading claims.').

omega_variable(
    readiness_outcome_measurement,
    'Can operational readiness from drills be measured independently of the drill regime itself, or is the validation circular (drills validate readiness which is defined by drill performance)?',
    'Natural experiment: compare disaster outcomes for jurisdictions with high vs. low exercise frequency, controlling for hazard exposure and resource levels. Alternatively, use red-team/blue-team unannounced exercises with independent observers.',
    'If readiness cannot be independently validated, the coordination function''s efficacy becomes a matter of faith — the Rope claim rests on internal coherence rather than external verification. This would not change the classification (still Rope if participants experience value) but would elevate the omega to conceptual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readiness_outcome_measurement, conceptual, 'Epistemic circularity in validating the coordination function''s output.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__competence_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__competence_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__competence_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__competence_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__competence_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__competence_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__competence_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__competence_reading, base_extractiveness, 40, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__competence_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__competence_reading, suppression_requirement, 20, 0.07).
narrative_ontology:measurement(prep_su_t30, preparedness_persistence__competence_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__competence_reading, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, physical_infrastructure_standards).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into at least three constraint stories: (1) this competence_reading — the practice/coordination layer as Rope; (2) husk_reading — the same regime as ritualized performance (Snare/Piton); (3) hybrid_reading — stratified competence across components. A fourth constraint, physical_infrastructure_standards, is the Mountain layer (building codes, redundancy requirements) that the kernel also references. The competence_reading influences the hybrid_reading by providing the 'competent components' empirical basis; it coexists_with the husk_reading as a competing live interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
