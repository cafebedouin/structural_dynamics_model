% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This constraint represents the 'competence reading' of preparedness,
 *   where drills and inspections are understood as genuine
 *   competence-preserving practices. It focuses on maintaining operational
 *   capacity and adaptive skill, with a low ceremony-to-competence ratio.
 *   Resource allocation prioritizes skill retention and adaptive capacity,
 *   aiming for population safety as the primary beneficiary. This reading
 *   explicitly contrasts with views that see preparedness as mere ritual or
 *   stratified competence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.05).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'c1422117-42e6-4a06-8e52-56fac23e3e98').
narrative_ontology:cs_kernel_codification('c1422117-42e6-4a06-8e52-56fac23e3e98', implicit).
narrative_ontology:cs_authority_grounding('c1422117-42e6-4a06-8e52-56fac23e3e98', expertise).
narrative_ontology:cs_interpretation_layer_present('c1422117-42e6-4a06-8e52-56fac23e3e98').
narrative_ontology:cs_reading_relation('c1422117-42e6-4a06-8e52-56fac23e3e98', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1422117-42e6-4a06-8e52-56fac23e3e98', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c1422117-42e6-4a06-8e52-56fac23e3e98', foundational, competence_is_exercised_knowledge).
narrative_ontology:cs_axiom_status(competence_is_exercised_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('c1422117-42e6-4a06-8e52-56fac23e3e98', competence_is_exercised_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('c1422117-42e6-4a06-8e52-56fac23e3e98', foundational, preparedness_optimizes_for_adaptive_capacity).
narrative_ontology:cs_axiom_status(preparedness_optimizes_for_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('c1422117-42e6-4a06-8e52-56fac23e3e98', preparedness_optimizes_for_adaptive_capacity, instrumental).
narrative_ontology:cs_reference_frame('c1422117-42e6-4a06-8e52-56fac23e3e98', adaptive_competence_paradigm).
narrative_ontology:cs_drift_state('c1422117-42e6-4a06-8e52-56fac23e3e98', contemporary_budget_cycles, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c1422117-42e6-4a06-8e52-56fac23e3e98', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, general_population).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, governing_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from effective disaster response and reduced harm due to maintained operational capacity. Bears indirect costs through taxes funding preparedness, but these are seen as a worthwhile investment for safety.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, general_population, beneficiary,
    organized, biographical, constrained, local).

% Responsible for maintaining and exercising preparedness knowledge. They conduct drills, inspections, and training to ensure operational capacity. They are direct beneficiaries of the clarity and effectiveness this constraint provides, enabling them to perform their duties efficiently.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_responders, agenda_setter,
    institutional, biographical, constrained, local).

% Allocate resources for preparedness activities, including training, equipment, and personnel. They bear the fiscal cost but benefit from public trust and reduced post-disaster recovery expenses. Their exit options involve shifting funding priorities, but at the risk of public safety and political fallout.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, governing_authorities, payer,
    institutional, generational, mobile, national).

% Evaluate the effectiveness of preparedness programs, assessing the gap between declared capacity and actual competence. They provide independent analysis and recommendations, influencing policy and resource allocation.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that diverse emergency response units, government agencies, and community organizations can effectively coordinate their actions during a disaster, by maintaining shared, exercised knowledge and operational capacity.
% TRANSFER_FUNCTION: Transfers resources (funding, time, personnel) from governing authorities to emergency responders and training programs, in exchange for maintained operational competence and public safety.
% ABSENT_VOICES: Those who would advocate for purely symbolic preparedness measures, prioritizing cost savings over genuine competence, are largely absent from this reading's discourse, as the focus is on demonstrable operational capacity.
% DISAPPEARANCE_RATIONALE: If the constraint of 'preparedness as live exercised knowledge' vanished, operational capacity would degrade, drills would become perfunctory, and actual disaster response would become chaotic and ineffective, leading to greater loss of life and property. The entire system of disaster management would need to be rebuilt from scratch.
% FOUNDING_PROBLEM: The problem of ensuring effective, coordinated response to unpredictable disasters, preventing loss of life and minimizing damage through proactive skill development and readiness.
% FOUNDING_PROBLEM_CORROBORATION: Emergency responders and independent disaster management experts consistently attest that the problem of maintaining live competence is ongoing and critical, citing continuous threats and the need for adaptive capacity. This is corroborated by post-disaster reviews and academic studies of institutional memory.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary goal is public safety, not rent collection; any 'extraction' is the necessary cost of maintaining a public good. Suppression is low (0.05) as compliance is driven by shared understanding of necessity rather than coercion. Theater ratio is low (0.1) because practices are genuinely aimed at skill retention, not just appearance. Accessibility collapse is low (0.2) as alternatives (e.g., ignoring preparedness) are not suppressed but simply recognized as ineffective. Resistance is low (0.1) because the value of genuine competence is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of emergency responders, this constraint is a pure Rope, enabling their core mission. From the perspective of governing authorities, it is a Rope with necessary costs. The key is that all parties recognize the genuine coordination function and the low extractive overhead.
 *
 * DIRECTIONALITY LOGIC:
 *   The general population and emergency responders are direct beneficiaries, gaining safety and operational effectiveness. Governing authorities are payers, bearing the fiscal costs but also benefiting from public trust and reduced disaster impact. There are no identifiable victims, as the system is designed for collective benefit. Analytical observers provide external validation without direct benefit or cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_ambiguity,
    'How reliably can ''live exercised knowledge'' and ''operational capacity'' be measured to distinguish genuine competence from performative compliance?',
    'Development of independent, adaptive assessment protocols that simulate novel disaster scenarios, rather than rehearsed drills, and track real-time decision-making under stress.',
    'If competence is difficult to measure, the constraint could drift towards a higher theater_ratio without detection, potentially reclassifying towards a Piton or even a Snare if resources are diverted from actual capacity to symbolic displays.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_measurement_ambiguity, empirical, 'Uncertainty in distinguishing genuine operational competence from mere performative compliance in preparedness exercises.').

omega_variable(
    resource_allocation_drift,
    'Is the current resource allocation genuinely optimizing for skill retention and adaptive capacity, or is it susceptible to political pressures that favor visible but less effective measures?',
    'Longitudinal studies comparing resource allocation patterns with actual disaster outcomes and post-event performance reviews, adjusting funding models based on demonstrated efficacy.',
    'If resource allocation drifts towards visible but ineffective measures, the constraint''s extractiveness could rise (diverting funds from effective use) and its theater_ratio would increase, pushing it towards a Piton or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_drift, empirical, 'Risk of resource allocation drifting from competence optimization to political visibility.').

omega_variable(
    reading_distinction_clarity,
    'Is the distinction between ''live exercised knowledge'' (competence_reading) and ''memorial performance'' (husk_reading) sufficiently clear in practice, or do they blur in real-world implementation?',
    'Qualitative ethnographic studies of preparedness drills and post-disaster debriefs, focusing on participant perceptions of utility vs. ritual, and the actual application of learned skills.',
    'If the readings blur, the ''competence_reading'' may be an idealized type that rarely manifests purely, suggesting that most real-world preparedness constraints are closer to the ''hybrid_reading'' or ''husk_reading''. This would shift the typical classification of preparedness efforts away from a pure Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_distinction_clarity, conceptual, 'Conceptual clarity of distinguishing competence-based preparedness from performative preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__competence_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__competence_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__competence_reading, suppression_requirement, 10, 0.04).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__competence_reading, suppression_requirement, 20, 0.04).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__competence_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__competence_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(prep_su_t50, preparedness_retention__competence_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_retention' kernel. Its siblings, 'husk_reading' and 'hybrid_reading', represent alternative interpretations of preparedness, with different structural properties and classifications. This reading emphasizes genuine competence and low extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
