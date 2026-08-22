% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Preparedness Transmission (Husk Reading): Ritualized Drills and Inspections
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint describes the continued performance of disaster
 *   preparedness drills and inspections, where the primary function of
 *   building operational knowledge has atrophied, and the activities persist
 *   largely as a memorial ritual. Organizational memory of past disasters
 *   drives the *form* of preparedness, but the *content* has hollowed out,
 *   leading to high compliance with protocols but low adaptive capacity for
 *   novel scenarios. This is the 'husk reading' of the
 *   preparedness_transmission kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.65).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.7).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Transmission (Husk Reading): Ritualized Drills and Inspections").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, 'd488bee7-629e-4509-86f3-bb4d4508c8c4').
narrative_ontology:cs_kernel_codification('d488bee7-629e-4509-86f3-bb4d4508c8c4', formalized).
narrative_ontology:cs_authority_grounding('d488bee7-629e-4509-86f3-bb4d4508c8c4', lineage).
narrative_ontology:cs_interpretation_layer_present('d488bee7-629e-4509-86f3-bb4d4508c8c4').
narrative_ontology:cs_reading_relation('d488bee7-629e-4509-86f3-bb4d4508c8c4', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d488bee7-629e-4509-86f3-bb4d4508c8c4', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('d488bee7-629e-4509-86f3-bb4d4508c8c4', foundational, formal_compliance_equals_preparedness).
narrative_ontology:cs_axiom_status(formal_compliance_equals_preparedness, holdable).
narrative_ontology:cs_axiom_grounding('d488bee7-629e-4509-86f3-bb4d4508c8c4', formal_compliance_equals_preparedness, conventional).
narrative_ontology:cs_axiom('d488bee7-629e-4509-86f3-bb4d4508c8c4', secondary, past_solutions_suffice_for_future_threats).
narrative_ontology:cs_axiom_status(past_solutions_suffice_for_future_threats, holdable).
narrative_ontology:cs_axiom_grounding('d488bee7-629e-4509-86f3-bb4d4508c8c4', past_solutions_suffice_for_future_threats, empirically_contingent).
narrative_ontology:cs_reference_frame('d488bee7-629e-4509-86f3-bb4d4508c8c4', post_cold_war_standardized_response).
narrative_ontology:cs_drift_state('d488bee7-629e-4509-86f3-bb4d4508c8c4', contemporary_climate_change_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d488bee7-629e-4509-86f3-bb4d4508c8c4', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_bureaucracy).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, local_emergency_managers).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, vulnerable_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and mandates the drills and inspections, ensuring formal compliance with protocols. Benefits from the appearance of preparedness and continued funding, even as the operational efficacy declines. Its identity is fused with maintaining the 'preparedness' facade.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_bureaucracy, agenda_setter,
    institutional, generational, identity_locked, national).

% Implement the mandated drills and inspections, often with limited resources. They benefit from clear directives and the ability to demonstrate compliance to higher authorities, avoiding scrutiny, even if they privately recognize the drills' limitations. Their careers depend on maintaining the status quo.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, local_emergency_managers, beneficiary,
    organized, biographical, constrained, regional).

% Participate in drills that often feel performative and do not adequately prepare them for novel or complex disaster scenarios. They bear the cost of wasted time and the psychological burden of knowing their training is insufficient, but cannot opt out due to professional obligations.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_responders, payer,
    moderate, immediate, constrained, local).

% Are the ultimate victims of hollowed-out preparedness. They rely on the civil defense system for safety and evacuation, but receive inadequate protection when actual disasters exceed the scope of ritualized drills. They have no direct influence over the design or efficacy of preparedness protocols.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, vulnerable_communities, payer,
    powerless, immediate, trapped, local).

% Observe and analyze the gap between formal preparedness activities and actual adaptive capacity. They identify the ritualistic nature of drills and the decline in operational knowledge, often publishing reports that are ignored by the bureaucracy.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, disaster_risk_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the formal schedule and execution of disaster preparedness drills and inspections across various agencies and jurisdictions, ensuring a baseline level of activity and reporting.
% TRANSFER_FUNCTION: Transfers resources (time, budget, personnel) into the performance of ritualized drills and inspections, generating reports and compliance metrics that flow upwards to the civil defense bureaucracy, while transferring a false sense of security to vulnerable communities.
% ABSENT_VOICES: Independent disaster preparedness experts and community advocates who would demand adaptive, scenario-based training and transparent assessments of actual operational readiness, rather than mere compliance with outdated protocols. Their voices are often dismissed as 'alarmist' or 'unrealistic' by the entrenched bureaucracy.
% DISAPPEARANCE_RATIONALE: If the mandate for these ritualized drills and inspections vanished, the civil defense bureaucracy would lose its primary justification for funding and its institutional identity. Local emergency managers would face a vacuum of directives, and the illusion of preparedness would collapse, forcing a reckoning with actual capabilities and potentially leading to a more adaptive, but initially chaotic, reorganization of disaster response.
% FOUNDING_PROBLEM: The need to systematically prepare for and respond to natural and man-made disasters, ensuring public safety through coordinated action and regularly validated capabilities.
% FOUNDING_PROBLEM_CORROBORATION: The civil defense bureaucracy claims the problem is live, citing ongoing threats. Disaster risk analysts and frontline responders attest that while the *threat* is live, the *founding problem of effective preparedness* is no longer being solved by the current ritualized approach; independent post-disaster reviews and academic studies corroborate the hollowing out of operational knowledge.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high theater_ratio (0.85) reflects that the majority of activity is performative compliance rather than genuine capability building. Extractiveness (0.65) is substantial, as resources are diverted to maintaining the ritual, and the cost of false security is borne by vulnerable communities. Suppression (0.70) is high because the civil defense bureaucracy actively resists external critiques and maintains its authority through formal adherence to protocols, suppressing alternative, more adaptive approaches. The claimed type is 'piton' because the primary function has atrophied, but the constraint persists due to institutional inertia and the concentrated benefits of maintaining the facade for the bureaucracy, while diffuse costs are borne by many.
 *
 * PERSPECTIVAL GAP:
 *   The civil defense bureaucracy perceives the drills as essential for maintaining order and demonstrating accountability, even if the actual operational knowledge is low. Frontline responders and vulnerable communities experience the same drills as a frustrating, potentially dangerous charade. The engine's classification will highlight this divergence between the claimed 'rope' (coordination) and the computed 'piton' (atrophied function with extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   The civil_defense_bureaucracy and local_emergency_managers are beneficiaries, gaining legitimacy and avoiding scrutiny by maintaining the ritual, even if they recognize its limitations. Frontline_responders and vulnerable_communities are payers, bearing the costs of ineffective preparedness and false security. Disaster_risk_analysts are observers, providing critical analysis that is often ignored.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: the original mandate to ensure effective disaster preparedness has been superseded by a mandate to perform the *ritual* of preparedness. The classification as a piton prevents mislabeling this as a functional rope or even a tangled rope, by highlighting the high theater ratio and the hollowing out of operational knowledge. The persistence is due to institutional inertia and the benefits of maintaining the facade, not genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_vs_competence_ambiguity,
    'Is the continued performance of drills and inspections primarily a ritualistic act of organizational memory, or does it still contribute meaningfully to operational competence?',
    'Independent, unannounced, scenario-based drills with novel elements, assessed by external experts against adaptive capacity metrics, rather than compliance with pre-specified checklists.',
    'If found to be purely ritualistic, the theater_ratio would be confirmed as very high, solidifying the piton classification. If some operational competence is retained, the theater_ratio might be lower, suggesting a more complex, possibly tangled_rope, dynamic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ritual_vs_competence_ambiguity, empirical, 'Distinguishing performative compliance from genuine capability building.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative preparedness approaches structural (lack of funding, bureaucratic inertia) or internalized (belief in the efficacy of existing rituals, fear of challenging authority)?',
    'Pilot programs for alternative, adaptive preparedness models in select jurisdictions: if resistance persists even with funding and top-level endorsement, internalized suppression is higher.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as agents carry the resistance to change with them. If purely structural, removing barriers would lead to rapid adoption of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for adaptive preparedness.').

omega_variable(
    framing_underdetermination_preparedness,
    'Is this constraint best framed as a ''husk reading'' (ritualized performance) or as a ''competence reading'' (live exercised knowledge)?',
    'The ''husk reading'' is chosen based on the observed high compliance with protocol form and low adaptive capacity under novel flood scenarios, as well as inspection routines detecting only pre-specified failure modes. The ''competence reading'' would require evidence of continuous re-validation of capability through practice and successful adaptation to unforeseen challenges.',
    'If the ''competence reading'' were adopted, the extractiveness and theater_ratio would be significantly lower, and the constraint would likely classify as a rope, reflecting genuine coordination and capability building.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_preparedness, conceptual, 'Alternative framings of preparedness transmission as ritual vs. competence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__husk_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(prep_tr_t1998, preparedness_transmission__husk_reading, theater_ratio, 1998, 0.4).
narrative_ontology:measurement(prep_tr_t2006, preparedness_transmission__husk_reading, theater_ratio, 2006, 0.6).
narrative_ontology:measurement(prep_tr_t2014, preparedness_transmission__husk_reading, theater_ratio, 2014, 0.75).
narrative_ontology:measurement(prep_tr_t2020, preparedness_transmission__husk_reading, theater_ratio, 2020, 0.82).
narrative_ontology:measurement(prep_tr_t2024, preparedness_transmission__husk_reading, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__husk_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(prep_be_t1998, preparedness_transmission__husk_reading, base_extractiveness, 1998, 0.45).
narrative_ontology:measurement(prep_be_t2006, preparedness_transmission__husk_reading, base_extractiveness, 2006, 0.55).
narrative_ontology:measurement(prep_be_t2014, preparedness_transmission__husk_reading, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement(prep_be_t2020, preparedness_transmission__husk_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(prep_be_t2024, preparedness_transmission__husk_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__husk_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(prep_su_t1998, preparedness_transmission__husk_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(prep_su_t2006, preparedness_transmission__husk_reading, suppression_requirement, 2006, 0.6).
narrative_ontology:measurement(prep_su_t2014, preparedness_transmission__husk_reading, suppression_requirement, 2014, 0.65).
narrative_ontology:measurement(prep_su_t2020, preparedness_transmission__husk_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(prep_su_t2024, preparedness_transmission__husk_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_transmission' kernel. Its high extractiveness and theater_ratio (husk reading) contrast with the lower extraction of the 'competence_reading' and the stratified nature of the 'hybrid_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
