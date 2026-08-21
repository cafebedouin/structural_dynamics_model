% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes disaster preparedness as a system of 'live
 *   exercised knowledge' – routines and training that actively maintain
 *   operational capacity and transfer critical skills across generations. It
 *   emphasizes genuine competence and adaptive capacity, contrasting with
 *   purely performative or 'husk' forms of preparedness. The system is
 *   actively managed and enforced to ensure participation and quality, but
 *   its low extractiveness and theater ratio reflect its functional
 *   effectiveness.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.25).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, 'fb65cd55-4a4d-4726-8df6-d1d1a9522301').
narrative_ontology:cs_kernel_codification('fb65cd55-4a4d-4726-8df6-d1d1a9522301', formalized).
narrative_ontology:cs_authority_grounding('fb65cd55-4a4d-4726-8df6-d1d1a9522301', expertise).
narrative_ontology:cs_interpretation_layer_present('fb65cd55-4a4d-4726-8df6-d1d1a9522301').
narrative_ontology:cs_reading_relation('fb65cd55-4a4d-4726-8df6-d1d1a9522301', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('fb65cd55-4a4d-4726-8df6-d1d1a9522301', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('fb65cd55-4a4d-4726-8df6-d1d1a9522301', foundational, operational_capacity_is_primary).
narrative_ontology:cs_axiom_status(operational_capacity_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('fb65cd55-4a4d-4726-8df6-d1d1a9522301', operational_capacity_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('fb65cd55-4a4d-4726-8df6-d1d1a9522301', foundational, knowledge_is_exercised_and_transferred).
narrative_ontology:cs_axiom_status(knowledge_is_exercised_and_transferred, holdable).
narrative_ontology:cs_axiom_grounding('fb65cd55-4a4d-4726-8df6-d1d1a9522301', knowledge_is_exercised_and_transferred, empirically_contingent).
narrative_ontology:cs_reference_frame('fb65cd55-4a4d-4726-8df6-d1d1a9522301', adaptive_competence_paradigm).
narrative_ontology:cs_drift_state('fb65cd55-4a4d-4726-8df6-d1d1a9522301', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fb65cd55-4a4d-4726-8df6-d1d1a9522301', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, community_members).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, emergency_management_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, elected_officials).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, training_specialists).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, first_responders).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, proactive_risk_management).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, adaptive_capacity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing, implementing, and enforcing preparedness routines and training. They benefit from effective disaster response and the public trust it engenders. Their exit is constrained by their mandate and public expectation.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Invest significant time and effort in training, drills, and maintaining operational readiness. They are direct beneficiaries of a system that works, as it enhances their safety and effectiveness during actual emergencies. Exit is constrained by professional identity and public service commitment.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, first_responders, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, first_responders, beneficiary).

% Benefit from enhanced safety, faster recovery, and reduced losses during disasters due to effective preparedness. Their participation in drills or awareness campaigns is often voluntary. They can move to areas with better preparedness, but are generally mobile.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, community_members, beneficiary,
    moderate, immediate, mobile, local).

% Benefit from the political capital and public confidence generated by a robust and effective preparedness system. They allocate resources and set policy, but are not directly involved in day-to-day operational maintenance. Their exit options are relatively mobile, as they can shift priorities.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, elected_officials, beneficiary,
    powerful, biographical, mobile, national).

% Design and deliver the training programs and exercises that maintain live knowledge. They are beneficiaries of a system that values and funds their expertise. Their exit is constrained by their specialized professional identity.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, training_specialists, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, training_specialists, beneficiary).

% May question the cost of preparedness in the absence of immediate threats, viewing it as an unnecessary expense. Their voices are often diffuse and not directly integrated into preparedness planning, but they can influence public opinion and elections.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, skeptical_taxpayers, excluded,
    powerless, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a shared, up-to-date body of knowledge and practiced routines for effective disaster response across diverse agencies and personnel, mitigating the impact of generational turnover.
% TRANSFER_FUNCTION: Transfers critical operational knowledge, skills, and adaptive capacity from experienced personnel to new generations, and allocates resources to maintain this live competence.
% ABSENT_VOICES: Skeptical taxpayers who view preparedness as an unnecessary cost in the absence of immediate threats, and those who advocate for purely reactive or market-based disaster response models.
% DISAPPEARANCE_RATIONALE: If live exercised knowledge vanished, institutional memory would rapidly degrade, leading to chaotic, uncoordinated, and highly ineffective responses to inevitable disasters, resulting in catastrophic loss of life, property, and public trust. The entire social fabric would be severely strained.
% FOUNDING_PROBLEM: The historical experience of uncoordinated, ineffective, and costly responses to recurring disasters, exacerbated by the loss of experienced personnel through retirement or attrition, leading to a 'D5 break' (degradation of institutional memory and competence).
% FOUNDING_PROBLEM_CORROBORATION: Post-disaster analyses, academic studies on organizational learning and institutional memory, historical records of disaster impacts, and actuarial data on risk management all corroborate the ongoing challenge of maintaining competence through generational turnover, from sources outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that costs are primarily for genuine training, equipment, and personnel, not rent-seeking. Suppression (0.25) is present to ensure compliance with drills and standards, but it's relatively low because participants generally recognize the value. The theater ratio (0.1) is low, indicating that drills are functional tests of competence, not mere performances. Accessibility collapse (0.6) is moderate, as alternatives to organized preparedness are limited in complex disaster scenarios. Resistance (0.18) is low due to the clear benefits. The temporal measurements show a stable, well-maintained system, with metrics remaining consistently low over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of emergency management and first responders, this system is a vital, effective Rope. From the perspective of skeptical taxpayers, it might appear as an unnecessary cost, but this reading asserts its genuine functional value. The engine's classification will reflect the structural reality of low extraction and high coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies and training specialists are agenda-setters and beneficiaries, designing and maintaining the system while benefiting from its effectiveness. First responders are payers (time/effort) but also primary beneficiaries (enhanced safety/effectiveness). Community members are beneficiaries of safety. Elected officials benefit from public trust. Skeptical taxpayers are excluded voices, bearing costs without direct input.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drift_to_husk_or_hybrid,
    'Is the observed preparedness truly ''live exercised knowledge'', or is there an unacknowledged drift towards ''memorial performance'' (husk_reading) or a ''layered system'' (hybrid_reading) where performative elements dominate?',
    'Independent, unannounced operational audits and stress tests that simulate real-world conditions, focusing on decision-making under pressure and adaptive capacity, rather than mere procedural compliance.',
    'If drift towards husk_reading is detected, the constraint''s true theater_ratio and extractiveness would be higher, reclassifying it towards Piton or Snare. If drift towards hybrid_reading is detected, the classification might shift to Tangled Rope, acknowledging both coordination and extraction from performative elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_to_husk_or_hybrid, empirical, 'Assessing whether the system maintains genuine competence or has degraded to mere performance.').

omega_variable(
    generational_knowledge_transfer_efficacy,
    'How effectively is operational knowledge and adaptive capacity transferred across generational turnover, and what is the actual rate of ''D5 break'' (degradation of institutional memory)?',
    'Longitudinal studies tracking the performance of new cohorts in drills and actual incidents compared to experienced personnel, and detailed post-incident analyses focusing on knowledge gaps related to turnover.',
    'If transfer efficacy is low and D5 break is significant, the constraint''s true effectiveness is lower than assumed, potentially increasing the effective extractiveness (as resources are spent for less actual benefit) and shifting its classification towards Piton or Snare due to functional atrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_efficacy, empirical, 'Measuring the success of knowledge transfer and institutional memory retention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(prep_tr_t50, preparedness_commitment__competence_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(prep_be_t50, preparedness_commitment__competence_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__competence_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__competence_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__competence_reading, suppression_requirement, 30, 0.26).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(prep_su_t50, preparedness_commitment__competence_reading, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, disaster_response_funding).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, public_trust_in_government).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'preparedness_commitment' kernel. This 'competence_reading' focuses on genuine operational capacity, contrasting with the 'husk_reading' (memorial performance) and 'hybrid_reading' (layered system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
