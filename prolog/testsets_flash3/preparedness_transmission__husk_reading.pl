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
 *   preparedness drills and inspections as a memorial ritual, where the
 *   organizational memory of past disasters persists, but the actual
 *   operational knowledge and adaptive capacity have hollowed out. It is one
 *   reading of the 'preparedness_transmission' kernel. The system exhibits
 *   high compliance with protocol form but low adaptive capacity under novel
 *   flood scenarios, and inspection routines detect only pre-specified
 *   failure modes. This is a 'husk_reading' where the form remains but the
 *   substance is gone.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.65).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.4).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Transmission (Husk Reading): Ritualized Drills and Inspections").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, 'f34a3d69-099c-456c-87e7-6248ea721cd6').
narrative_ontology:cs_kernel_codification('f34a3d69-099c-456c-87e7-6248ea721cd6', formalized).
narrative_ontology:cs_authority_grounding('f34a3d69-099c-456c-87e7-6248ea721cd6', lineage).
narrative_ontology:cs_interpretation_layer_present('f34a3d69-099c-456c-87e7-6248ea721cd6').
narrative_ontology:cs_reading_relation('f34a3d69-099c-456c-87e7-6248ea721cd6', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f34a3d69-099c-456c-87e7-6248ea721cd6', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('f34a3d69-099c-456c-87e7-6248ea721cd6', foundational, form_preserves_function).
narrative_ontology:cs_axiom_status(form_preserves_function, holdable).
narrative_ontology:cs_axiom_grounding('f34a3d69-099c-456c-87e7-6248ea721cd6', form_preserves_function, conventional).
narrative_ontology:cs_axiom('f34a3d69-099c-456c-87e7-6248ea721cd6', foundational, institutional_memory_is_operational_knowledge).
narrative_ontology:cs_axiom_status(institutional_memory_is_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('f34a3d69-099c-456c-87e7-6248ea721cd6', institutional_memory_is_operational_knowledge, conventional).
narrative_ontology:cs_reference_frame('f34a3d69-099c-456c-87e7-6248ea721cd6', formal_compliance_as_competence).
narrative_ontology:cs_drift_state('f34a3d69-099c-456c-87e7-6248ea721cd6', contemporary_complex_threat_environment, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f34a3d69-099c-456c-87e7-6248ea721cd6', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, civilian_populations).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, junior_emergency_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the drill and inspection schedules, ensuring formal compliance with protocols. Their careers and institutional legitimacy are tied to maintaining the appearance of preparedness, even if the underlying operational knowledge has atrophied. They benefit from the stability of the ritual.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, senior_civil_defense_leadership, agenda_setter,
    institutional, biographical, identity_locked, national).

% Perform the drills and inspections, often recognizing their performative nature but lacking the authority to change them. They bear the cost of time and effort without gaining genuine operational knowledge, and may experience moral injury from the gap between ritual and reality. Their career progression depends on compliance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, junior_emergency_managers, payer,
    moderate, immediate, constrained, local).

% Are the ultimate targets of disaster preparedness, but are left vulnerable by the hollowed-out operational knowledge. They pay with their safety and trust, receiving only the theatrical reassurance of drills that do not prepare for real threats. They have no direct exit from the system.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civilian_populations, payer,
    powerless, immediate, trapped, local).

% Review compliance with established protocols and checklists. Their mandate often focuses on formal adherence rather than adaptive capacity, reinforcing the ritualistic nature of the drills. They could expose the gap but are often constrained by their own audit frameworks.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, external_auditors, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a formal framework for disaster response, ensuring that different agencies and personnel are familiar with their roles and communication channels, even if the depth of knowledge is superficial.
% TRANSFER_FUNCTION: Transfers a sense of security and institutional legitimacy from the civil defense apparatus to the public and political leadership, in exchange for resources and continued public trust. It also transfers time and effort from junior staff to maintain the ritual.
% ABSENT_VOICES: Experienced, retired emergency managers who witnessed the original competence and would critique the current state; frontline responders who face novel threats not covered by current drills; and the victims of past disasters whose experiences are not integrated into current 'lessons learned' rituals.
% DISAPPEARANCE_RATIONALE: If the drills and inspections vanished, the illusion of preparedness would collapse, forcing a reckoning with the actual state of operational knowledge. This would likely lead to a crisis of confidence in civil defense, but also potentially open the door for genuine reform and rebuilding of adaptive capacity.
% FOUNDING_PROBLEM: To ensure rapid, coordinated, and effective response to large-scale disasters, particularly floods and other natural hazards, through regular training and validation of operational procedures.
% FOUNDING_PROBLEM_CORROBORATION: While civil defense leadership maintains the problem is live, independent disaster analysts and retired senior emergency managers attest that the original problem of coordinated response has evolved significantly, and the current rituals no longer address contemporary threats effectively. The original problem is 'dead' in its initial form, replaced by new challenges the system fails to adapt to.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) reflects the cost borne by junior staff and the public for a system that provides diminishing real-world benefit. Suppression (0.40) is moderate; while there's no overt coercion, the institutional inertia and career path dependence act as soft suppression. The high theater ratio (0.85) is central to this reading: the primary function has atrophied, replaced by performative maintenance. Resistance is low (0.10) because the costs are diffuse and the beneficiaries (senior leadership's legitimacy) are concentrated, making it a classic piton scenario.
 *
 * PERSPECTIVAL GAP:
 *   Senior leadership perceives the drills as essential for maintaining order and public confidence, while junior staff and the public experience them as increasingly detached from real-world threats. The engine's classification will highlight this divergence, showing a piton from the payer seats and a more benign type from the agenda-setter's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior civil defense leadership benefits from the stability and legitimacy conferred by the rituals (low directionality). Junior emergency managers and civilian populations bear the costs of wasted effort and false security (high directionality). External auditors, while powerful, are observers whose mandate often reinforces the theatricality, making them analytical rather than direct beneficiaries or victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a clear case of mandatrophy: the original mandate (effective disaster preparedness) has atrophied, but the rituals persist due to institutional inertia and the concentrated benefits of maintaining the appearance of competence. The classification as a piton prevents mislabeling it as a functional rope or a malicious snare, accurately capturing its inertial, performative nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_knowledge_decay_rate,
    'What is the actual rate of decay of operational knowledge and adaptive capacity within the civil defense system, independent of formal compliance metrics?',
    'Independent, scenario-based simulation exercises with novel threats, evaluated by external experts, rather than checklist-based inspections.',
    'A high decay rate would further solidify the ''husk_reading'' and strengthen the piton classification, potentially triggering a ''critical'' mandatrophy alert. A lower rate might suggest a ''hybrid_reading'' where some competence persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_knowledge_decay_rate, empirical, 'Empirical measurement of the gap between formal compliance and actual adaptive capacity.').

omega_variable(
    ritual_vs_function_framing,
    'Is the continued performance of drills primarily a functional exercise (competence_reading) or a memorial ritual (husk_reading)?',
    'Analysis of resource allocation: if resources are primarily directed towards formal compliance and appearance rather than genuine capability upgrades or adaptation to new threats, it supports the ''husk_reading''.',
    'If framed as purely ritualistic, the theater_ratio and extractiveness are higher, reinforcing the piton classification. If a functional component is identified, it might shift towards a ''tangled_rope'' or ''hybrid_reading'' for specific components.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ritual_vs_function_framing, conceptual, 'The conceptual framing of the drills'' primary purpose.').

omega_variable(
    mandate_relevance_ambiguity,
    'Is the original mandate for disaster preparedness still relevant to contemporary threats, or has it been superseded by new challenges that the current system is not designed to address?',
    'Comparative analysis of historical disaster profiles versus current and projected threat landscapes, coupled with expert assessment of system design adequacy.',
    'If the mandate is found to be largely irrelevant, it strengthens the ''dead'' status of the founding problem and the piton classification. If it''s still relevant but poorly executed, it points to a different class of failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_relevance_ambiguity, empirical, 'Relevance of the original disaster preparedness mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(prep_tr_t5, preparedness_transmission__husk_reading, theater_ratio, 5, 0.7).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.78).
narrative_ontology:measurement(prep_tr_t15, preparedness_transmission__husk_reading, theater_ratio, 15, 0.82).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.85).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(prep_be_t5, preparedness_transmission__husk_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(prep_be_t15, preparedness_transmission__husk_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t5, preparedness_transmission__husk_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__husk_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(prep_su_t15, preparedness_transmission__husk_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__husk_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'husk_reading' of the 'preparedness_transmission' kernel, focusing on the ritualistic performance of drills and inspections. It is distinct from the 'competence_reading' (drills as live knowledge) and 'hybrid_reading' (stratified competence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
