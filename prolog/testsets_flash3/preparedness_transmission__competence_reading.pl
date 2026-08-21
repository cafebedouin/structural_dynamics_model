% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission (Competence Reading): Drills and Inspections as Live Exercised Knowledge
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint describes drills and inspections as a mechanism for
 *   transmitting and re-validating live, exercised knowledge within disaster
 *   risk management. It emphasizes high adaptive capacity, where inspectors
 *   recognize novel failure signatures and drill participants improvise
 *   effectively under scenario variation. This is one reading of the
 *   'preparedness_transmission' kernel, focusing on the active maintenance of
 *   competence.
 *
 * KEY AGENTS:
 *   - civil_defense_agencies: Agenda setter (institutional/constrained)
 *   - first_responders: Beneficiary (organized/constrained)
 *   - public_citizens: Beneficiary (powerless/trapped)
 *   - inspectors: Agenda setter (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.05).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission (Competence Reading): Drills and Inspections as Live Exercised Knowledge").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, 'e67e4731-e0d9-4f01-b556-3f0e6be0cc3e').
narrative_ontology:cs_kernel_codification('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', formalized).
narrative_ontology:cs_authority_grounding('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', expertise).
narrative_ontology:cs_interpretation_layer_present('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e').
narrative_ontology:cs_reading_relation('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', preparedness_transmission__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', foundational, adaptive_capacity_is_paramount).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', adaptive_capacity_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', foundational, knowledge_is_exercised_not_stored).
narrative_ontology:cs_axiom_status(knowledge_is_exercised_not_stored, holdable).
narrative_ontology:cs_axiom_grounding('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', knowledge_is_exercised_not_stored, conventional).
narrative_ontology:cs_reference_frame('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', continuous_adaptive_learning_system).
narrative_ontology:cs_drift_state('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e67e4731-e0d9-4f01-b556-3f0e6be0cc3e', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, public_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, implements, and evaluates drills and inspections. Benefits from a highly competent and adaptive system that can respond to novel threats. Bears the cost of continuous training and resource allocation.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Participates in drills, gaining practical experience and validating their skills. Benefits from clear protocols and the ability to improvise effectively in dynamic scenarios. Their professional identity is tied to their competence.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, first_responders, beneficiary,
    organized, biographical, constrained, local).

% Are the ultimate beneficiaries of effective disaster preparedness, relying on the system for safety and recovery. They bear indirect costs through taxes but directly benefit from reduced risk and effective response.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, public_citizens, beneficiary,
    powerless, immediate, trapped, local).

% Conducts inspections, identifying vulnerabilities and ensuring compliance. Their expertise is critical for recognizing novel failure signatures and adapting protocols. They are key to the continuous re-validation of capability.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inspectors, agenda_setter,
    powerful, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates complex, multi-agency responses to disaster scenarios, ensuring that diverse teams can integrate their efforts, communicate effectively, and adapt to unforeseen challenges through practiced, live-exercised knowledge.
% TRANSFER_FUNCTION: Transfers operational knowledge, adaptive capacity, and validated competence across generations of personnel and evolving threat landscapes, from experienced practitioners to new recruits and from lessons learned to updated protocols.
% ABSENT_VOICES: Those who advocate for purely theoretical or 'paper' preparedness, or those who believe that past successes guarantee future performance, are implicitly excluded. Their absence ensures a focus on active, adaptive learning.
% DISAPPEARANCE_RATIONALE: If drills and inspections ceased to be live, exercised knowledge, the system's adaptive capacity would rapidly degrade. Institutional memory would become inert, novel threats would go unrecognized, and actual disaster responses would likely fail due to a lack of practiced, integrated competence. The entire civil defense system would need to be rebuilt from first principles.
% FOUNDING_PROBLEM: The challenge of maintaining and transmitting complex, adaptive disaster response capabilities across changing personnel and evolving threats, ensuring that knowledge is live and actionable, not merely theoretical or historical.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management experts, disaster historians, and independent risk assessment bodies consistently corroborate that the problem of maintaining live, exercised competence is ongoing and critical for effective civil defense. Their analyses highlight the continuous need for re-validation through practice, independent of the agencies directly benefiting from the system.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary function is genuine coordination and capability building, with minimal rent-seeking. Suppression is very low (0.05) as participation is largely voluntary and driven by professional duty and public safety. Theater ratio is low (0.1) because the activities are genuinely functional, focused on real-world validation and adaptation, not mere performance. Accessibility collapse is high (0.8) because there are few effective alternatives to live, exercised knowledge for building adaptive capacity. Resistance is low (0.1) as the value of such preparedness is widely recognized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil defense agencies and first responders, this constraint is a vital Rope, ensuring collective safety and operational excellence. From an analytical observer's perspective, it is also a Rope, as its benefits are widely distributed and its costs are primarily for maintenance, not extraction. There is little perspectival divergence in this reading because the system is genuinely functional.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies and inspectors are agenda setters, benefiting from a well-functioning system and bearing the costs of its maintenance. First responders and public citizens are beneficiaries, receiving direct safety and competence benefits. No identifiable victims exist in this reading, as the system is designed for collective good with minimal coercive overhead.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly counters mandatrophy by emphasizing continuous re-validation and adaptation. The low theater ratio and high accessibility collapse (due to lack of effective alternatives) indicate that the constraint's mandate is actively fulfilled and its function is live. The classification as a Rope prevents mislabeling genuine coordination as extraction by focusing on the active, adaptive nature of the knowledge transmission.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_reading,
    'Is the observed preparedness system genuinely transmitting live, exercised knowledge (competence_reading), or is it primarily performing a memorial ritual with hollowed-out operational knowledge (husk_reading)?',
    'Empirical analysis of drill outcomes under novel, unscripted scenarios; assessment of improvisation rates and recognition of emergent failure signatures by inspectors. High improvisation and novel signature recognition would support the competence_reading.',
    'If the husk_reading is true, the constraint would reclassify towards Piton or Snare, with significantly higher theater_ratio and extractiveness (as resources are consumed for non-functional performance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_vs_husk_reading, empirical, 'Distinguishing genuine competence from performative ritual in preparedness transmission.').

omega_variable(
    competence_vs_hybrid_reading,
    'Is competence uniformly high across all domains (competence_reading), or is it stratified, with high physical infrastructure competence but decayed civilian coordination knowledge (hybrid_reading)?',
    'Comparative analysis of drill performance across different domains (e.g., engineering vs. public communication, evacuation logistics). Divergent performance would support the hybrid_reading.',
    'If the hybrid_reading is true, the constraint would decompose into multiple, linked constraints, with the civilian coordination component potentially reclassifying as a Piton or Tangled Rope due to higher theater and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_hybrid_reading, empirical, 'Assessing the uniformity of competence across different aspects of preparedness.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression observed due to genuine voluntary participation and shared goals, or is there an internalized professional identity lock-in that makes non-participation unthinkable, even if the system were to degrade?',
    'Qualitative interviews with first responders and civil defense personnel about perceived autonomy and alternatives. If non-participation is seen as a betrayal of professional identity, it suggests internalized suppression.',
    'If internalized suppression is significant, the effective suppression for individual agents might be higher than the structural measure suggests, even if external barriers are low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in professional contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__competence_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__competence_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__competence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(prep_tr_t50, preparedness_transmission__competence_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__competence_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__competence_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__competence_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(prep_be_t50, preparedness_transmission__competence_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__competence_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__competence_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__competence_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__competence_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(prep_su_t50, preparedness_transmission__competence_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'competence_reading' of the 'preparedness_transmission' kernel. It is linked to its sibling readings, 'husk_reading' and 'hybrid_reading', which represent alternative interpretations of the same underlying system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
