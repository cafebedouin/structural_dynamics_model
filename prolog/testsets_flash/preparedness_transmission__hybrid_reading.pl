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
 *   This constraint describes the stratified nature of disaster preparedness
 *   transmission: while engineering competence for physical infrastructure
 *   remains robust, the knowledge and practices for civilian coordination
 *   have decayed. This 'hybrid reading' of the preparedness_transmission
 *   kernel highlights a D5 break in the coordination layer, where the
 *   physical systems perform as designed, but human systems fail under
 *   stress. The constraint is claimed as a Piton because the decay is largely
 *   inertial, with no single party actively benefiting from the coordination
 *   failure, but many bearing its diffuse costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.4).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.3).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, '13d15ee4-ab74-4e6c-8717-fe0797dacd4e').
narrative_ontology:cs_kernel_codification('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', formalized).
narrative_ontology:cs_authority_grounding('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', lineage).
narrative_ontology:cs_interpretation_layer_present('13d15ee4-ab74-4e6c-8717-fe0797dacd4e').
narrative_ontology:cs_reading_relation('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', foundational, preparedness_is_stratified).
narrative_ontology:cs_axiom_status(preparedness_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', preparedness_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', foundational, civilian_coordination_knowledge_decayed).
narrative_ontology:cs_axiom_status(civilian_coordination_knowledge_decayed, holdable).
narrative_ontology:cs_axiom_grounding('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', civilian_coordination_knowledge_decayed, empirically_contingent).
narrative_ontology:cs_reference_frame('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', integrated_holistic_preparedness).
narrative_ontology:cs_drift_state('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13d15ee4-ab74-4e6c-8717-fe0797dacd4e', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_engineers).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, local_community_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of inadequate civilian coordination during disasters, including confusion, delayed evacuation, and increased casualties. They rely on effective preparedness but lack direct control over its maintenance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_populations, payer,
    powerless, biographical, trapped, local).

% Responsible for maintaining preparedness across all sectors. They excel at infrastructure maintenance and engineering aspects but struggle with the decay of civilian coordination knowledge, often due to diffuse responsibility and lack of direct feedback.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Maintain high competence in physical infrastructure design, construction, and maintenance. Their expertise is valued and actively transmitted, ensuring robust physical systems. They benefit from clear mandates and measurable outcomes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_engineers, beneficiary,
    organized, biographical, mobile, national).

% Are on the front lines during disasters, attempting to coordinate civilian response with decayed knowledge. They bear the immediate burden of coordination failures and often lack the resources or training to bridge the knowledge gap.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, local_community_leaders, payer,
    moderate, immediate, constrained, local).

% Study past disaster responses and institutional memory to identify patterns of competence and decay. They observe the stratification of preparedness transmission and can articulate the D5 break in coordination.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, historical_precedent_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure effective response to disasters by coordinating physical infrastructure resilience with civilian evacuation and response protocols.
% TRANSFER_FUNCTION: Transfers responsibility for disaster preparedness from a holistic, integrated system to a stratified one where physical infrastructure competence is maintained, but civilian coordination knowledge decays, effectively transferring risk to civilian populations.
% ABSENT_VOICES: Future generations of civilian populations, who will bear the full cost of coordination failures without having been part of the decision-making that allowed the decay. Also, historical civil defense experts whose integrated knowledge has been fragmented.
% DISAPPEARANCE_RATIONALE: If the stratified preparedness transmission vanished, it would imply either a sudden restoration of holistic coordination knowledge or a complete collapse of all preparedness. In either case, disaster response would fundamentally reorganize, either for the better or worse.
% FOUNDING_PROBLEM: The need to protect populations and critical infrastructure from natural and man-made disasters through robust, integrated preparedness systems.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management agencies and infrastructure engineers attest that the problem of disaster risk is live and ongoing. Historical precedent analysts corroborate that the problem persists, but the effectiveness of the 'solution' (stratified preparedness) is contested, particularly regarding civilian coordination.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.4) because the cost of coordination failure is borne by civilian populations, but it's diffuse and often attributed to 'natural' disaster impacts rather than systemic decay. Suppression is low (0.3) as there's no active coercion preventing coordination, but rather a passive decay of knowledge and practice. Theater ratio is high (0.6) because drills and exercises often focus on physical infrastructure and command-and-control, giving the appearance of comprehensive preparedness while the civilian coordination aspect remains hollowed out. The rising theater ratio over time reflects the increasing performativity of preparedness without corresponding functional improvement in civilian coordination.
 *
 * PERSPECTIVAL GAP:
 *   Emergency management agencies may perceive overall preparedness as stable due to strong infrastructure competence, while civilian populations and local leaders experience a clear decline in effective coordination. The analytical observer (historical precedent analysts) sees the stratification and the D5 break.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations and local community leaders are payers, bearing the costs of coordination failure. Emergency management agencies are agenda-setters, responsible for the system but struggling with the stratified decay. Infrastructure engineers are beneficiaries, as their competence is maintained and valued. No single party directly benefits from the decay of civilian coordination, making it a diffuse cost characteristic of a Piton.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (holistic disaster preparedness) has outlived its function in the civilian coordination layer, but persists in the infrastructure layer. The 'hybrid reading' prevents mislabeling the entire system as a Snare (if it were purely extractive) or a Rope (if it were genuinely coordinating across all layers). It highlights the specific atrophy in one critical component while another remains functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_mechanism_identification,
    'Is the decay in civilian coordination knowledge primarily due to lack of funding, lack of political will, or a fundamental shift in societal structures (e.g., decreased community cohesion)?',
    'Longitudinal sociological studies and policy analysis comparing funding allocations, political priorities, and community engagement metrics over time.',
    'Identifying the primary mechanism would inform targeted interventions. If funding/will, it''s a policy choice; if societal shift, it requires deeper structural solutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_mechanism_identification, empirical, 'Determining the root cause of civilian coordination knowledge decay.').

omega_variable(
    hybrid_vs_husk_distinction,
    'To what extent does the ''hybrid_reading'' truly differ from the ''husk_reading'' in terms of functional impact on civilian safety, or is the distinction primarily analytical?',
    'Comparative analysis of disaster outcomes in regions with strong infrastructure but weak coordination versus regions where all preparedness is purely ritualistic. If outcomes differ significantly, the hybrid reading holds; if not, it converges with the husk reading.',
    'If the hybrid reading converges with the husk, the overall system is more degraded than currently assessed, potentially reclassifying the entire preparedness system as a more severe Piton or even a Snare if the ritual actively obscures risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_husk_distinction, conceptual, 'Clarifying the boundary between stratified decay and complete hollowing out of preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1980, preparedness_transmission__hybrid_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__hybrid_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(prep_tr_t2000, preparedness_transmission__hybrid_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(prep_tr_t2010, preparedness_transmission__hybrid_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(prep_tr_t2024, preparedness_transmission__hybrid_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(prep_be_t1980, preparedness_transmission__hybrid_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__hybrid_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(prep_be_t2000, preparedness_transmission__hybrid_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(prep_be_t2010, preparedness_transmission__hybrid_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(prep_be_t2024, preparedness_transmission__hybrid_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1980, preparedness_transmission__hybrid_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__hybrid_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(prep_su_t2000, preparedness_transmission__hybrid_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(prep_su_t2010, preparedness_transmission__hybrid_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(prep_su_t2024, preparedness_transmission__hybrid_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, disaster_response_funding_allocation).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, public_trust_in_institutions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
