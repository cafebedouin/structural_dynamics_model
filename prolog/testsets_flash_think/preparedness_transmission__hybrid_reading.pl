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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission (Engineering vs. Civilian Coordination)
 *   domain: disaster_risk_management/institutional_memory/civil_defense_systems
 *
 * SUMMARY:
 *   This constraint, the 'hybrid_reading' of the 'preparedness_transmission'
 *   kernel, describes a system where disaster preparedness is stratified.
 *   Physical infrastructure competence (e.g., engineering standards,
 *   maintenance of critical facilities) remains high and functional, while
 *   civilian coordination knowledge (e.g., community-level evacuation plans,
 *   volunteer networks, public communication protocols) has significantly
 *   decayed. The system maintains an outward appearance of overall
 *   preparedness, but its effectiveness is critically compromised in the
 *   social coordination layer.
 *
 * KEY AGENTS:
 *   - engineering_firms: Primary beneficiary (institutional/mobile) — benefits from infrastructure focus
 *   - infrastructure_agencies: Agenda setter (institutional/constrained) — maintains physical competence, overlooks coordination decay
 *   - general_public: Primary payer (powerless/trapped) — bears costs of coordination failure
 *   - local_emergency_responders: Payer (organized/constrained) — experiences direct impact of coordination decay
 *   - civil_defense_advocates: Observer (moderate/mobile) — highlights coordination gaps
 *   - policy_makers: Agenda setter (powerful/constrained) — prioritizes visible infrastructure over social coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.65).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.5).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission (Engineering vs. Civilian Coordination)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense_systems").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, '6e500da6-fedd-467d-a4ff-512a740216ce').
narrative_ontology:cs_kernel_codification('6e500da6-fedd-467d-a4ff-512a740216ce', formalized).
narrative_ontology:cs_authority_grounding('6e500da6-fedd-467d-a4ff-512a740216ce', lineage).
narrative_ontology:cs_interpretation_layer_present('6e500da6-fedd-467d-a4ff-512a740216ce').
narrative_ontology:cs_reading_relation('6e500da6-fedd-467d-a4ff-512a740216ce', preparedness_transmission__competence_reading, influences).
narrative_ontology:cs_reading_relation('6e500da6-fedd-467d-a4ff-512a740216ce', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('6e500da6-fedd-467d-a4ff-512a740216ce', foundational, operational_competence_is_stratified).
narrative_ontology:cs_axiom_status(operational_competence_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('6e500da6-fedd-467d-a4ff-512a740216ce', operational_competence_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('6e500da6-fedd-467d-a4ff-512a740216ce', foundational, civilian_coordination_is_critical_path).
narrative_ontology:cs_axiom_status(civilian_coordination_is_critical_path, holdable).
narrative_ontology:cs_axiom_grounding('6e500da6-fedd-467d-a4ff-512a740216ce', civilian_coordination_is_critical_path, instrumental).
narrative_ontology:cs_reference_frame('6e500da6-fedd-467d-a4ff-512a740216ce', holistic_preparedness_doctrine).
narrative_ontology:cs_drift_state('6e500da6-fedd-467d-a4ff-512a740216ce', contemporary_institutional_memory_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6e500da6-fedd-467d-a4ff-512a740216ce', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_firms).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_agencies).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, general_public).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, local_emergency_responders).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, resilience_through_infrastructure_investment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Profit from contracts to design, build, and maintain critical physical infrastructure. Their competence remains high, and the focus on 'hard' infrastructure ensures their continued relevance and funding.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_firms, beneficiary,
    institutional, biographical, mobile, national).

% Responsible for maintaining physical infrastructure and its associated operational protocols. They uphold high engineering standards but may inadvertently contribute to the decay of civilian coordination by prioritizing tangible assets over less visible social capital.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Rely on effective disaster preparedness for safety and security. They bear the direct costs of coordination failures during emergencies, experiencing confusion, delayed aid, and increased risk due to decayed civilian coordination knowledge.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, general_public, payer,
    powerless, immediate, trapped, local).

% Are on the front lines of disaster response. They directly experience the challenges and inefficiencies caused by decayed civilian coordination knowledge, often having to improvise or compensate for systemic gaps, increasing their own risk and workload.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, local_emergency_responders, payer,
    organized, immediate, constrained, local).

% Actively campaign for holistic preparedness, highlighting the critical gaps in civilian coordination and institutional memory. They provide critical analysis but often lack the direct power to shift institutional priorities or resource allocation.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_defense_advocates, observer,
    moderate, biographical, mobile, national).

% Allocate resources and set strategic priorities for disaster preparedness. They often favor visible, politically popular infrastructure projects over less tangible, long-term investments in social coordination and institutional memory, contributing to the stratification.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, policy_makers, agenda_setter,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, engineering_firms).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure comprehensive and effective disaster response and recovery by maintaining robust physical infrastructure and efficient civilian coordination across all levels of society.
% TRANSFER_FUNCTION: Transfers resources, attention, and political capital towards the maintenance and enhancement of physical infrastructure and engineering competence, while implicitly transferring the risks and burdens of decayed civilian coordination knowledge to the general public and local emergency responders.
% ABSENT_VOICES: Communities in high-risk areas, future generations who will face increasingly complex and frequent disasters, and experts in social resilience and human factors who would emphasize the critical, often overlooked, role of community-level coordination and institutional memory.
% DISAPPEARANCE_RATIONALE: If this stratified transmission mechanism vanished, the system would either rebalance towards holistic preparedness (if the decay mechanism was removed) or both physical and social preparedness would degrade, leading to a catastrophic failure of disaster response. The current stratification is a specific, maintained state.
% FOUNDING_PROBLEM: To protect populations and critical assets from the impacts of natural and man-made disasters through robust planning, infrastructure, and response capabilities.
% FOUNDING_PROBLEM_CORROBORATION: Engineering firms and infrastructure agencies attest that the problem of protecting physical assets is live and their competence is vital. Civil defense advocates and disaster sociologists, citing post-disaster analyses and independent research, corroborate the significant decay in civilian coordination, arguing that the founding problem is only partially addressed and critically unbalanced.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it ostensibly serves a coordination function (disaster preparedness) but exhibits significant asymmetric extraction due to the decay in civilian coordination. Extractiveness (0.65) is moderate-high, reflecting the substantial costs borne by the public and responders during emergencies. Suppression (0.5) is moderate, stemming from institutional inertia and a lack of active investment in social coordination, rather than overt coercion. Theater ratio (0.4) is moderate, as drills and exercises for civilian coordination may continue, but their underlying knowledge base has atrophied, making them more performative than functional. The rising extractiveness and theater ratio over the interval reflect the gradual accumulation of risk and the increasing performativity of coordination efforts.
 *
 * PERSPECTIVAL GAP:
 *   Infrastructure agencies and engineering firms perceive the system as largely effective, focusing on the robust physical infrastructure. In contrast, the general public and local emergency responders experience the system as failing in critical moments due to the breakdown in civilian coordination. Policy makers often operate between these perspectives, balancing visible infrastructure investments with less tangible social preparedness needs.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering firms and infrastructure agencies are beneficiaries, as the system's focus on physical infrastructure ensures their funding and relevance. The general public and local emergency responders are victims, bearing the costs of coordination failures. Policy makers, while agenda setters, also face political costs when the system fails. Civil defense advocates serve an analytical observer role, highlighting the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate for comprehensive disaster preparedness has not fully atrophied, but its execution has become stratified. The 'hybrid_reading' prevents mislabeling it as a pure Rope (which would imply full coordination) or a pure Snare (which would imply no functional coordination at all). Instead, it highlights the partial atrophy in the coordination layer, where the original mandate is no longer fully met, leading to a form of extraction of safety and security from the public.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_decay_mechanism,
    'Is the decay in civilian coordination knowledge primarily due to active institutional neglect, passive generational attrition, or a deliberate shift in policy priorities?',
    'Historical policy analysis, budget allocation studies, and interviews with former and current civil defense officials to identify specific decisions or lack thereof that led to the decay.',
    'If active neglect or deliberate policy shift, the suppression metric might be higher, indicating a more intentional extraction of resources from coordination. If passive attrition, it points to a need for systemic institutional memory mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_decay_mechanism, empirical, 'Understanding the root cause of coordination knowledge decay.').

omega_variable(
    kernel_reading_structural_delta,
    'How would the classification change if the ''competence_reading'' or ''husk_reading'' of preparedness_transmission were adopted?',
    'Comparing the structural properties (extractiveness, suppression, theater_ratio) and stakeholder impacts across the three readings of the preparedness_transmission kernel.',
    'If ''competence_reading'' were true, extractiveness would be significantly lower, and the system would function closer to a Rope. If ''husk_reading'' were true, theater_ratio would be much higher across all domains, and engineering competence would also be degraded, potentially shifting to a Piton or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'The structural implications of alternative readings of preparedness transmission.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of effective civilian coordination structural (lack of funding, institutional barriers) or internalized (public apathy, belief that ''experts will handle it'')?',
    'Post-disaster community surveys and ethnographic studies to assess public engagement and perceived agency in preparedness efforts. If apathy persists after structural barriers are addressed, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the public''s capacity for self-organization is diminished even if external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for civilian coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(prep_tr_t6, preparedness_transmission__hybrid_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(prep_tr_t12, preparedness_transmission__hybrid_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(prep_tr_t18, preparedness_transmission__hybrid_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__hybrid_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__hybrid_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(prep_be_t6, preparedness_transmission__hybrid_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(prep_be_t12, preparedness_transmission__hybrid_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(prep_be_t18, preparedness_transmission__hybrid_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__hybrid_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__hybrid_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(prep_su_t6, preparedness_transmission__hybrid_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement(prep_su_t12, preparedness_transmission__hybrid_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(prep_su_t18, preparedness_transmission__hybrid_reading, suppression_requirement, 18, 0.49).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__hybrid_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__hybrid_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, disaster_response_efficacy).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, public_trust_in_government).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'preparedness_transmission' kernel, each representing a different structural interpretation of how disaster preparedness knowledge and capability are maintained across generations. This 'hybrid_reading' focuses on the stratification of competence, where physical infrastructure is strong but civilian coordination has decayed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
