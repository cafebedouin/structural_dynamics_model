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
 *   preparedness drills and inspections as a memorial ritual, where the form
 *   and frequency are maintained, but the underlying operational knowledge
 *   and adaptive capacity have hollowed out. It is a 'husk reading' of the
 *   broader 'preparedness_transmission' kernel. The system appears
 *   functional, but its ability to respond to novel or complex threats is
 *   severely degraded. This reading emphasizes the performative aspect over
 *   genuine competence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.45).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.6).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Transmission (Husk Reading): Ritualized Drills and Inspections").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '4ee1b5d7-e468-4869-8c6b-e8693902b286').
narrative_ontology:cs_kernel_codification('4ee1b5d7-e468-4869-8c6b-e8693902b286', formalized).
narrative_ontology:cs_authority_grounding('4ee1b5d7-e468-4869-8c6b-e8693902b286', lineage).
narrative_ontology:cs_interpretation_layer_present('4ee1b5d7-e468-4869-8c6b-e8693902b286').
narrative_ontology:cs_reading_relation('4ee1b5d7-e468-4869-8c6b-e8693902b286', preparedness_transmission__competence_reading, influences).
narrative_ontology:cs_reading_relation('4ee1b5d7-e468-4869-8c6b-e8693902b286', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('4ee1b5d7-e468-4869-8c6b-e8693902b286', foundational, ritual_maintains_memory).
narrative_ontology:cs_axiom_status(ritual_maintains_memory, holdable).
narrative_ontology:cs_axiom_grounding('4ee1b5d7-e468-4869-8c6b-e8693902b286', ritual_maintains_memory, conventional).
narrative_ontology:cs_axiom('4ee1b5d7-e468-4869-8c6b-e8693902b286', secondary, compliance_equals_readiness).
narrative_ontology:cs_axiom_status(compliance_equals_readiness, holdable).
narrative_ontology:cs_axiom_grounding('4ee1b5d7-e468-4869-8c6b-e8693902b286', compliance_equals_readiness, conventional).
narrative_ontology:cs_reference_frame('4ee1b5d7-e468-4869-8c6b-e8693902b286', formal_compliance_as_readiness).
narrative_ontology:cs_drift_state('4ee1b5d7-e468-4869-8c6b-e8693902b286', contemporary_era_of_complex_disasters, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('4ee1b5d7-e468-4869-8c6b-e8693902b286', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_bureaucracy).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, vulnerable_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and performs drills and inspections according to established protocols. Benefits from the appearance of preparedness and continued funding, even as the operational knowledge required for genuine response atrophies. Their identity is fused with the maintenance of these rituals.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_bureaucracy, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from the public perception of a robust civil defense system, which provides political cover in the event of a disaster. They are insulated from the operational realities and can pivot blame if the system fails.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, political_leadership, beneficiary,
    powerful, immediate, mobile, national).

% Participate in drills and inspections that often feel performative or irrelevant to actual disaster scenarios. They bear the cost of time and effort without gaining genuine adaptive capacity, leading to frustration and burnout. Their professional identity is tied to the system, making exit difficult.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_responders, payer,
    moderate, biographical, constrained, local).

% Are the ultimate victims of a hollowed-out preparedness system. They rely on the civil defense system for safety and are exposed to increased risk when actual disasters strike, as the ritualized drills fail to translate into effective response. They have no direct influence over the system's design or operation.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, vulnerable_communities, payer,
    powerless, biographical, trapped, local).

% Analyze the evolution of civil defense practices, documenting the shift from genuine operational competence to ritualized performance. They can identify the points at which knowledge transmission failed and theatricality increased.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, institutional_historians, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates various agencies and personnel for disaster response through standardized procedures and communication channels. In practice, it coordinates the performance of preparedness rituals.
% TRANSFER_FUNCTION: Transfers public trust and funding to the civil defense bureaucracy and political leadership, in exchange for the appearance of safety and readiness. Transfers time and effort from frontline responders and risk from vulnerable communities.
% ABSENT_VOICES: Former generations of civil defense experts who possessed genuine operational knowledge would object to the current state of ritualized performance. Independent disaster preparedness experts who advocate for adaptive, scenario-based training are often marginalized.
% DISAPPEARANCE_RATIONALE: If the ritualized drills and inspections vanished overnight, the civil defense bureaucracy would lose its primary justification for existence and funding. While the immediate operational capacity might not change much (as it's already hollowed out), the political and institutional landscape around disaster preparedness would be forced to re-evaluate its foundations, potentially leading to a genuine rebuilding of competence or a complete collapse of the system.
% FOUNDING_PROBLEM: To ensure public safety and minimize loss of life and property in the face of natural disasters and other emergencies through a robust, coordinated response system.
% FOUNDING_PROBLEM_CORROBORATION: While the civil defense bureaucracy claims the problem is live, independent disaster analysts, institutional historians, and many frontline responders attest that the original problem of genuine, adaptive preparedness has largely been supplanted by a focus on performative compliance, rendering the founding problem 'dead' in its original operational sense. The problem of disaster impact remains, but the system's response to it has atrophied.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).
:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, as the system primarily extracts political legitimacy and funding rather than direct material wealth. Suppression (0.6) is significant, as dissent about the system's efficacy is often suppressed to maintain the illusion of competence. The theater ratio (0.85) is very high, reflecting that most activity is performative rather than genuinely functional. Accessibility collapse (0.3) is low because the 'alternatives' (genuine adaptive capacity, real-world problem-solving) are not actively suppressed but simply neglected or allowed to atrophy. Resistance (0.1) is low because the system is maintained by inertia and identity-lock, rather than active coercion against strong opposition.
 *
 * PERSPECTIVAL GAP:
 *   The civil defense bureaucracy and political leadership perceive the system as functional and necessary, justifying its continued operation. Frontline responders and vulnerable communities experience it as increasingly detached from reality, leading to a significant divergence in perceived utility and effectiveness. The engine's classification will highlight this gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The civil defense bureaucracy and political leadership are beneficiaries, gaining legitimacy and funding from the ritual. Frontline responders and vulnerable communities are payers, bearing the costs of performative effort and increased risk, respectively. The system's persistence is driven by the beneficiaries' need for political cover and the bureaucracy's identity-locked commitment to its own procedures, even as its original mandate atrophies.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear case of mandatrophy. The original mandate was genuine disaster preparedness, but the function has atrophied, replaced by ritualized performance. The classification as a Piton reflects that the constraint persists due to institutional inertia and the diffuse costs of fixing it, rather than concentrated benefits that would drive active maintenance of genuine function. The high theater ratio and the 'dead' status of the founding problem are key indicators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_knowledge_decay_rate,
    'What is the actual rate of decay of operational knowledge and adaptive capacity within the civil defense system, independent of compliance metrics?',
    'Independent, scenario-based simulations and unannounced, novel-threat drills, assessed by external experts, rather than internal compliance audits.',
    'A higher decay rate would further solidify the ''piton'' classification and highlight the urgency of reform; a lower rate might suggest a ''tangled_rope'' where some genuine function remains, albeit with high overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_knowledge_decay_rate, empirical, 'Quantifying the gap between ritual performance and actual operational competence.').

omega_variable(
    natural_vs_constructed_ritual,
    'Is the ritualized performance of drills a natural outcome of large-scale institutional memory (a ''mountain'' of bureaucracy), or a constructed mechanism for rent-seeking and political cover (a ''snare'' or ''piton'')?',
    'Comparative analysis of similar institutions in different political and cultural contexts: if ritualization correlates with political insulation and lack of accountability, it suggests a constructed mechanism.',
    'If ''natural'', the constraint might be reclassified as a degraded ''rope'' or even a ''mountain'' of institutional physics. If ''constructed'', it reinforces the ''piton'' classification, emphasizing the agency in its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_ritual, conceptual, 'Distinguishing between inherent institutional inertia and deliberate performative maintenance.').

omega_variable(
    husk_vs_competence_framing,
    'Is this constraint best understood as a ''husk'' (ritual without substance) or does it retain a core ''competence'' (even if degraded)?',
    'Empirical evidence from actual disaster responses: if failures are systemic and adaptive capacity is absent, the ''husk'' reading is corroborated. If some core functions still perform, the ''competence_reading'' or ''hybrid_reading'' gains support.',
    'Corroboration of the ''husk_reading'' reinforces the high theater ratio and piton classification. Evidence for ''competence_reading'' would shift the classification towards a degraded rope or tangled rope, with lower theater and higher extractiveness (as genuine function would still be present, but costly).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(husk_vs_competence_framing, conceptual, 'The core framing ambiguity of the preparedness_transmission kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.7).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__husk_reading, theater_ratio, 30, 0.8).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.83).
narrative_ontology:measurement(prep_tr_t50, preparedness_transmission__husk_reading, theater_ratio, 50, 0.85).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__husk_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(prep_be_t50, preparedness_transmission__husk_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__husk_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__husk_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__husk_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__husk_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(prep_su_t50, preparedness_transmission__husk_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_transmission' kernel. Its high theater ratio and low adaptive capacity structurally influence the 'competence_reading' (by making it harder to maintain genuine competence) and the 'hybrid_reading' (by contributing to the decay of civilian coordination knowledge).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
