% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission (Hybrid Reading)
 *   domain: disaster_risk_management/institutional_memory/civil_defense_systems
 *
 * SUMMARY:
 *   This constraint describes the stratified transmission of preparedness
 *   knowledge: while engineering competence for physical infrastructure
 *   remains high, the knowledge and capacity for civilian coordination during
 *   disasters have decayed. This 'hybrid reading' acknowledges the functional
 *   success in one domain (physical systems) alongside a critical failure in
 *   another (human coordination), leading to a Tangled Rope classification.
 *   The constraint is actively enforced through drills and protocols, but
 *   these often serve to maintain the appearance of overall preparedness
 *   rather than addressing the underlying decay in civilian knowledge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.45).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.6).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense_systems").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, '88e40dea-3fab-421d-833d-d1724be9f9b3').
narrative_ontology:cs_kernel_codification('88e40dea-3fab-421d-833d-d1724be9f9b3', formalized).
narrative_ontology:cs_authority_grounding('88e40dea-3fab-421d-833d-d1724be9f9b3', lineage).
narrative_ontology:cs_interpretation_layer_present('88e40dea-3fab-421d-833d-d1724be9f9b3').
narrative_ontology:cs_reading_relation('88e40dea-3fab-421d-833d-d1724be9f9b3', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('88e40dea-3fab-421d-833d-d1724be9f9b3', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('88e40dea-3fab-421d-833d-d1724be9f9b3', foundational, stratified_competence_is_real).
narrative_ontology:cs_axiom_status(stratified_competence_is_real, holdable).
narrative_ontology:cs_axiom_grounding('88e40dea-3fab-421d-833d-d1724be9f9b3', stratified_competence_is_real, empirically_contingent).
narrative_ontology:cs_axiom('88e40dea-3fab-421d-833d-d1724be9f9b3', secondary, coordination_decay_is_critical).
narrative_ontology:cs_axiom_status(coordination_decay_is_critical, holdable).
narrative_ontology:cs_axiom_grounding('88e40dea-3fab-421d-833d-d1724be9f9b3', coordination_decay_is_critical, instrumental).
narrative_ontology:cs_reference_frame('88e40dea-3fab-421d-833d-d1724be9f9b3', integrated_disaster_preparedness_ideal).
narrative_ontology:cs_drift_state('88e40dea-3fab-421d-833d-d1724be9f9b3', contemporary_era_of_complex_threats, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88e40dea-3fab-421d-833d-d1724be9f9b3', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_engineers).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, general_public).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, local_emergency_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining preparedness, they benefit from the appearance of competence in infrastructure while struggling with civilian coordination. They enforce drills and protocols, but often lack the resources or political will to address the decay in public knowledge.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Maintain high levels of technical competence in physical infrastructure design, construction, and maintenance. Their knowledge transmission systems are robust, and they are largely insulated from the decay in civilian coordination.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_engineers, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of coordination failure during disasters, experiencing confusion, delayed evacuation, and increased casualties. They are nominally coordinated by civil defense plans but lack the practical knowledge to execute them effectively.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, general_public, payer,
    powerless, immediate, trapped, local).

% Are on the front lines during disasters and directly experience the gap between robust infrastructure and decayed civilian coordination. They pay in increased workload, stress, and risk due to public confusion and lack of preparedness.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, local_emergency_responders, payer,
    moderate, biographical, constrained, local).

% Analyze past disaster responses and institutional memory, identifying patterns of competence and decay. They provide the analytical framework for understanding the stratified nature of preparedness transmission.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the maintenance of physical infrastructure and the nominal framework for civilian response, ensuring that critical systems are robust and that there are plans for public action during emergencies.
% TRANSFER_FUNCTION: Transfers resources and attention to maintaining physical infrastructure competence, while implicitly transferring the burden of coordination failure to the general public and local responders due to decayed civilian knowledge.
% ABSENT_VOICES: Future generations, who will inherit the consequences of decayed civilian coordination knowledge, are absent from current policy discussions. Their interests would argue for greater investment in public education and drills.
% DISAPPEARANCE_RATIONALE: If this stratified transmission vanished, either all preparedness would collapse (leading to catastrophic failures in both infrastructure and coordination) or a more integrated, robust system would emerge, forcing a reorganization of civil defense priorities and public engagement.
% FOUNDING_PROBLEM: The need to protect populations and critical infrastructure from natural and man-made disasters, requiring both robust physical systems and an informed, coordinated public response.
% FOUNDING_PROBLEM_CORROBORATION: Civil defense agencies and infrastructure engineers attest that the problem is live, citing ongoing threats. Disaster historians corroborate that the problem persists, but highlight the differential success in addressing its physical vs. human coordination aspects.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).
:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because the system does provide real benefits in infrastructure resilience, but it extracts a cost from the public through coordination failures. Suppression (0.6) is present as civil defense agencies enforce compliance with plans, even if the public's ability to execute them is low. Theater ratio (0.4) is significant because drills often focus on visible compliance rather than genuine operational readiness for the civilian population. The increasing trend in extractiveness and suppression reflects the growing gap between perceived and actual preparedness.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of infrastructure engineers, the system is a successful Rope, ensuring robust physical systems. From the perspective of the general public and local responders, it functions more like a Snare or Tangled Rope, as they bear the costs of coordination decay. This hybrid reading attempts to capture both realities.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies and infrastructure engineers are beneficiaries, as their core functions are maintained and funded. The general public and local emergency responders are victims, bearing the costs of coordination failures. The system coordinates the efforts of engineers and agencies, but extracts from the public by failing to transmit critical coordination knowledge effectively.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to ensure comprehensive disaster preparedness is partially atrophied. While the physical infrastructure aspect remains robust, the civilian coordination aspect has decayed. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring the decay) or a pure Snare (ignoring the infrastructure competence). The system persists because the beneficiaries (agencies, engineers) maintain their part, while the diffuse costs of decay are borne by victims who lack the power to force change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_decay_measurement,
    'How precisely can the decay in civilian coordination knowledge be quantified and distinguished from a lack of public engagement?',
    'Longitudinal studies of public disaster literacy, post-disaster behavioral analysis, and comparative studies with regions having robust civilian preparedness programs.',
    'More precise measurement would refine the extractiveness and theater_ratio metrics, potentially shifting the classification towards a Snare if the decay is more severe and actively masked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_decay_measurement, empirical, 'Quantifying the extent and nature of civilian coordination knowledge decay.').

omega_variable(
    structural_necessity_of_stratification,
    'Is the stratification of preparedness transmission (high engineering, low civilian coordination) an inevitable outcome of modern institutional complexity, or a remediable policy choice?',
    'Comparative analysis of civil defense systems in different nations, particularly those with high public participation in preparedness, to identify alternative organizational models.',
    'If inevitable, the ''hybrid_reading'' is a more stable classification. If remediable, the constraint''s extractiveness and suppression could be seen as more contingent and subject to policy intervention, potentially shifting it towards a more remediable Tangled Rope or even a Scaffold if a transition plan were adopted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_necessity_of_stratification, conceptual, 'Whether the stratified nature of preparedness is a structural necessity or a policy artifact.').

omega_variable(
    kernel_reading_distinction,
    'What specific empirical signals would definitively distinguish this ''hybrid_reading'' from the ''competence_reading'' or ''husk_reading'' in a real-world disaster scenario?',
    'Post-disaster forensic analysis focusing on the differential performance of physical infrastructure vs. civilian evacuation/coordination, and the specific points of failure in knowledge transmission.',
    'Clear empirical distinction would strengthen the validity of this reading and its classification. Ambiguity would suggest the kernel itself is too ill-defined, or that the readings are not sufficiently distinct to warrant separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, empirical, 'Empirical criteria for distinguishing the hybrid reading from sibling readings of the preparedness transmission kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__hybrid_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__hybrid_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__hybrid_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(prep_tr_t50, preparedness_transmission__hybrid_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__hybrid_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__hybrid_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__hybrid_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(prep_be_t50, preparedness_transmission__hybrid_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__hybrid_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__hybrid_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__hybrid_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(prep_su_t50, preparedness_transmission__hybrid_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'preparedness_transmission' kernel. This 'hybrid_reading' focuses on the stratified nature of competence, where physical infrastructure remains robust but civilian coordination has decayed. It is linked to the 'competence_reading' (which asserts overall competence) and the 'husk_reading' (which asserts overall decay and ritualistic performance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
