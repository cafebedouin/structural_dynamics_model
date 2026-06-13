% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Preparedness Persistence (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'competence reading' of preparedness
 *   persistence: that drills and inspections are genuine, live exercises of
 *   knowledge that maintain operational readiness. It posits that the
 *   constraint is fundamentally a Mountain (the physical reality of knowledge
 *   decay and coordination complexity) combined with a Rope (the coordination
 *   mechanism of drills). There is minimal extraction, suppression, or
 *   theatricality, as the activities are seen as directly contributing to a
 *   shared, essential goal. This reading emphasizes the functional, rather
 *   than performative, aspects of preparedness.
 *
 * KEY AGENTS:
 *   - public_safety_agencies: Agenda setter (institutional/constrained) — designs and implements drills.
 *   - citizens: Beneficiary (organized/mobile) — directly benefit from effective response.
 *   - critical_infrastructure_operators: Payer (powerful/constrained) — bear costs of participation and readiness.
 *   - elected_officials: Beneficiary (institutional/mobile) — benefit from public trust in safety.
 *   - analytical_observers: Observer (analytical/analytical) — independently assess effectiveness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.05).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.1).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, mountain).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Preparedness Persistence (Competence Reading)").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:emerges_naturally(preparedness_persistence__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb').
narrative_ontology:cs_kernel_codification('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', implicit).
narrative_ontology:cs_authority_grounding('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', expertise).
narrative_ontology:cs_interpretation_layer_present('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb').
narrative_ontology:cs_reading_relation('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', foundational, operational_knowledge_decays_without_practice).
narrative_ontology:cs_axiom_status(operational_knowledge_decays_without_practice, holdable).
narrative_ontology:cs_axiom_grounding('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', operational_knowledge_decays_without_practice, empirically_contingent).
narrative_ontology:cs_axiom('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', foundational, exercised_coordination_is_essential_for_resilience).
narrative_ontology:cs_axiom_status(exercised_coordination_is_essential_for_resilience, holdable).
narrative_ontology:cs_axiom_grounding('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', exercised_coordination_is_essential_for_resilience, empirically_contingent).
narrative_ontology:cs_reference_frame('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', continuously_exercised_competence).
narrative_ontology:cs_drift_state('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('03c8c83d-7f5a-43e6-a27d-e92f0ed7feeb', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, public_safety_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, elected_officials).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, critical_infrastructure_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing, implementing, and evaluating preparedness drills and inspections. They benefit from the maintained operational readiness and public trust, but are constrained by budget and political will.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, public_safety_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from effective disaster response and mitigation, which is maintained by drills and inspections. They participate in drills and rely on the competence of emergency services. Their exit options are limited to relocating.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, citizens, beneficiary,
    organized, biographical, mobile, local).

% Bear the costs of participating in drills, conducting internal inspections, and implementing readiness measures. They are legally and reputationally bound to comply, but seek to minimize disruption to operations.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, critical_infrastructure_operators, payer,
    powerful, biographical, constrained, regional).

% Benefit from the public perception of effective governance and safety, especially in the aftermath of a disaster. They allocate resources and set policy for preparedness, but are sensitive to short-term political cycles.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, elected_officials, beneficiary,
    institutional, immediate, mobile, national).

% Academics and researchers who study disaster preparedness, institutional memory, and organizational resilience. They analyze the effectiveness of drills and inspections, providing independent assessment.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that diverse agencies, infrastructure operators, and the public can coordinate effectively during a disaster, by regularly exercising communication protocols, response plans, and resource allocation mechanisms.
% TRANSFER_FUNCTION: Transfers knowledge, skills, and operational readiness from training and planning into live, exercised competence across a complex network of actors, ultimately transferring safety and resilience to citizens.
% ABSENT_VOICES: The 'husk' reading of preparedness, which would argue that many drills are performative rituals rather than genuine competence-building, is absent from this framing. It would highlight the costs of theatrical compliance without real benefit.
% DISAPPEARANCE_RATIONALE: If the commitment to maintaining operational readiness through drills and inspections vanished, the ability of society to respond to disasters would rapidly degrade. Coordination failures, infrastructure collapses, and loss of life would increase dramatically, forcing a complete reorganization of public safety and governance.
% FOUNDING_PROBLEM: The inherent decay of operational knowledge and the complexity of coordinating multiple actors in high-stress, low-frequency disaster events.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management professionals, disaster historians, and public policy experts consistently corroborate that operational knowledge decays without practice and that complex coordination requires regular exercise. Post-disaster reviews frequently highlight the importance of prior drills.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(preparedness_persistence__competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(preparedness_persistence__competence_reading),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that the costs of drills are seen as necessary investments in public safety, not rents. Suppression (0.1) is minimal, representing the inherent friction of mandatory participation rather than coercion. Theater ratio (0.05) is low because, in this reading, the activities are genuinely functional. Accessibility collapse (0.9) is high because the 'natural law' of knowledge decay means there are few alternatives to active practice for maintaining readiness. Resistance (0.05) is low because the value of preparedness is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public safety agencies and citizens, this constraint is a clear Mountain/Rope, essential for survival. From the perspective of critical infrastructure operators, it's a necessary cost, but still fundamentally a coordination mechanism. There is little divergence in perceived type within this reading, as all parties generally agree on the necessity and efficacy of the activities.
 *
 * DIRECTIONALITY LOGIC:
 *   Public safety agencies and elected officials are beneficiaries, as they gain public trust and effective operational capacity. Citizens are direct beneficiaries of safety. Critical infrastructure operators are payers, bearing the direct costs of compliance. All are aligned towards the common goal of readiness, with costs seen as investments.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently prevents mandatrophy by asserting the continued 'liveness' of the founding problem and the direct efficacy of the solution. The low theater ratio and extractiveness indicate that the constraint's mandate is actively fulfilled, not atrophied. The engine's classification should align with a Mountain or Rope, confirming the absence of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_ambiguity,
    'Is this constraint truly about maintaining operational competence, or is it primarily a performative ''husk'' of activity?',
    'Empirical evaluation of drill outcomes (e.g., post-disaster performance metrics, independent audits of operational readiness) vs. self-reported compliance rates. If outcomes consistently fall short despite high reported activity, the ''husk'' reading gains support.',
    'If resolved towards the ''husk'' reading, the constraint would reclassify from Mountain/Rope to Piton or Snare, with significantly higher theater ratio and extractiveness, as the coordination story would be revealed as cover for inertial or extractive practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_ambiguity, empirical, 'Distinguishes genuine competence from performative compliance.').

omega_variable(
    natural_law_vs_constructed_coordination,
    'Is the necessity of drills and inspections a ''natural law'' of organizational decay and coordination complexity, or is the specific form of preparedness a socially constructed choice?',
    'Cross-cultural and historical comparison of preparedness regimes: if fundamental principles are universal but specific implementations vary widely in efficacy and cost, it suggests a constructed element within a natural necessity.',
    'If resolved towards ''constructed choice'', the ''Mountain'' aspect would diminish, and the ''Rope'' aspect would be scrutinized more for potential extractive elements, especially if beneficiaries are identifiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_coordination, conceptual, 'Ambiguity between inherent necessity and chosen implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1950, preparedness_persistence__competence_reading, theater_ratio, 1950, 0.03).
narrative_ontology:measurement(prep_tr_t1965, preparedness_persistence__competence_reading, theater_ratio, 1965, 0.04).
narrative_ontology:measurement(prep_tr_t1980, preparedness_persistence__competence_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(prep_tr_t1995, preparedness_persistence__competence_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(prep_tr_t2010, preparedness_persistence__competence_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(prep_tr_t2024, preparedness_persistence__competence_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(prep_be_t1950, preparedness_persistence__competence_reading, base_extractiveness, 1950, 0.03).
narrative_ontology:measurement(prep_be_t1965, preparedness_persistence__competence_reading, base_extractiveness, 1965, 0.04).
narrative_ontology:measurement(prep_be_t1980, preparedness_persistence__competence_reading, base_extractiveness, 1980, 0.04).
narrative_ontology:measurement(prep_be_t1995, preparedness_persistence__competence_reading, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement(prep_be_t2010, preparedness_persistence__competence_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(prep_be_t2024, preparedness_persistence__competence_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1950, preparedness_persistence__competence_reading, suppression_requirement, 1950, 0.08).
narrative_ontology:measurement(prep_su_t1965, preparedness_persistence__competence_reading, suppression_requirement, 1965, 0.09).
narrative_ontology:measurement(prep_su_t1980, preparedness_persistence__competence_reading, suppression_requirement, 1980, 0.09).
narrative_ontology:measurement(prep_su_t1995, preparedness_persistence__competence_reading, suppression_requirement, 1995, 0.1).
narrative_ontology:measurement(prep_su_t2010, preparedness_persistence__competence_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(prep_su_t2024, preparedness_persistence__competence_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'competence reading' of the 'preparedness_persistence' kernel. It contrasts with the 'husk_reading' (performative compliance) and 'hybrid_reading' (stratified competence/ritual) by asserting the genuine efficacy and necessity of drills and inspections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
