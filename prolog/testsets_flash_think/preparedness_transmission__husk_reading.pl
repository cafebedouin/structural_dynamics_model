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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Preparedness Transmission (Husk Reading): Ritualized Drills
 *   domain: disaster_risk_management/institutional_memory/civil_defense_systems
 *
 * SUMMARY:
 *   This constraint describes the 'husk reading' of preparedness
 *   transmission, where drills and inspections, while still performed, have
 *   become a memorial ritual rather than a genuine exercise of operational
 *   knowledge. Organizational memory persists in form, but the adaptive
 *   capacity and practical knowledge required for novel disaster scenarios
 *   have hollowed out. The constraint is claimed as a Piton, reflecting its
 *   atrophied function maintained by inertia and theatrical performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.45).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.2).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Transmission (Husk Reading): Ritualized Drills").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '2063abf0-9d17-4222-b11e-dd00e04cdab5').
narrative_ontology:cs_kernel_codification('2063abf0-9d17-4222-b11e-dd00e04cdab5', formalized).
narrative_ontology:cs_authority_grounding('2063abf0-9d17-4222-b11e-dd00e04cdab5', lineage).
narrative_ontology:cs_interpretation_layer_present('2063abf0-9d17-4222-b11e-dd00e04cdab5').
narrative_ontology:cs_reading_relation('2063abf0-9d17-4222-b11e-dd00e04cdab5', preparedness_transmission__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('2063abf0-9d17-4222-b11e-dd00e04cdab5', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('2063abf0-9d17-4222-b11e-dd00e04cdab5', foundational, ritual_substitutes_for_competence).
narrative_ontology:cs_axiom_status(ritual_substitutes_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('2063abf0-9d17-4222-b11e-dd00e04cdab5', ritual_substitutes_for_competence, conventional).
narrative_ontology:cs_axiom('2063abf0-9d17-4222-b11e-dd00e04cdab5', secondary, operational_knowledge_decay).
narrative_ontology:cs_axiom_status(operational_knowledge_decay, holdable).
narrative_ontology:cs_axiom_grounding('2063abf0-9d17-4222-b11e-dd00e04cdab5', operational_knowledge_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('2063abf0-9d17-4222-b11e-dd00e04cdab5', effective_disaster_preparedness).
narrative_ontology:cs_drift_state('2063abf0-9d17-4222-b11e-dd00e04cdab5', contemporary_disaster_landscape, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2063abf0-9d17-4222-b11e-dd00e04cdab5', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_administrators).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, legacy_institutions).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, taxpayers).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, vulnerable_communities).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, institutional_continuity_narrative).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, safety_theater_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining civil defense systems, they oversee the performance of drills and inspections. They benefit from the perceived continuity and budget allocation associated with these activities, even if operational knowledge has atrophied. Exiting the ritual would mean admitting systemic failure.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Fund the civil defense system through taxes, expecting genuine preparedness. They bear the financial cost of the drills and inspections, receiving a false sense of security in return. Their exit options are diffuse (voting, protest) but rarely directly impact the constraint.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, taxpayers, payer,
    moderate, immediate, mobile, national).

% Are most reliant on effective disaster response and are directly exposed to the consequences of inadequate preparedness. They receive a ritualized performance that does not translate into adaptive capacity, making them victims of the hollowed-out system.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, vulnerable_communities, payer,
    powerless, immediate, trapped, local).

% These are historical bodies or traditions that derive symbolic capital and legitimacy from the continued performance of preparedness rituals, reinforcing a narrative of institutional continuity despite functional decay.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, legacy_institutions, beneficiary,
    organized, generational, constrained, national).

% Possess the technical knowledge to discern the gap between ritualistic performance and actual adaptive capacity. They critique the system but often lack the institutional power to force change, operating as external analysts.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, disaster_response_experts, observer,
    analytical, biographical, analytical, global).

% Advocate for modern, adaptive disaster planning that moves beyond rote drills. Their proposals are often marginalized or dismissed by the existing administrative structure, which prioritizes the established rituals.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, adaptive_planning_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, civil_defense_administrators).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement was originally intended to coordinate inter-agency and public response to disaster scenarios, ensuring a unified and effective operational capability.
% TRANSFER_FUNCTION: It transfers financial resources from taxpayers to civil defense administrations and associated legacy institutions, in exchange for the performance of preparedness rituals and the maintenance of a narrative of safety.
% ABSENT_VOICES: Actual disaster victims (past and future) and advocates for adaptive, evidence-based disaster planning are largely absent from the decision-making process. They would highlight the functional decay and demand genuine operational knowledge over ritual.
% DISAPPEARANCE_RATIONALE: If the ritualistic performance of drills and inspections vanished overnight, the illusion of preparedness would collapse, forcing a public and institutional reckoning with the actual state of operational knowledge and adaptive capacity. This would likely lead to a reorganization of civil defense priorities and resource allocation.
% FOUNDING_PROBLEM: The system was built to ensure effective, coordinated, and resilient civil defense against natural and man-made disasters, protecting lives and infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Disaster response experts, independent auditors, and historical analyses of actual disaster outcomes corroborate that the original problem of effective, adaptive preparedness is no longer genuinely addressed by the current ritualistic system. The problem has been functionally superseded by a focus on performative compliance, despite claims from administrators that it remains live.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The `theater_ratio` is high (0.75) because a significant portion of the activity is performative, maintaining an appearance of preparedness without corresponding functional depth. `Extractiveness` is moderate (0.45) as resources are still consumed for these rituals, but the primary benefit is institutional continuity rather than direct rent-seeking. `Suppression` is low (0.20) because the constraint persists more through inertia and unacknowledged decay than active coercion; alternatives are collapsed by institutional resistance to change rather than direct force. `Accessibility_collapse` is moderate-high (0.65) because the institutional structure makes it difficult to adopt alternative, more adaptive approaches to preparedness.
 *
 * PERSPECTIVAL GAP:
 *   Civil defense administrators and legacy institutions perceive the continued performance of drills as a necessary function maintaining order and tradition, thus experiencing it as a Rope or even a Mountain of institutional necessity. Disaster response experts and vulnerable communities, however, experience it as a Piton or Snare, where resources are consumed for a hollowed-out function that fails to provide genuine security.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense administrators and legacy institutions are beneficiaries, as the ritual maintains their roles, budgets, and symbolic authority (low directionality). Taxpayers and vulnerable communities are victims, bearing the financial cost and the risk of inadequate preparedness (high directionality). The low suppression reflects that the system is not actively coercing participation, but rather relies on the inertia of established practice and the difficulty of challenging a 'safety' narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear case of mandatrophy. The original mandate to ensure effective disaster preparedness has atrophied, replaced by a ritualistic performance that serves institutional continuity rather than functional competence. The persistence of the constraint is due to institutional inertia and the political cost of admitting functional decay, rather than a live problem or concentrated benefit that would actively maintain it as a Snare or Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_latent_competence,
    'To what extent does the ''hollowed-out'' operational knowledge retain latent competence that could be reactivated under genuine crisis pressure?',
    'Post-crisis performance analysis: if the system adapts effectively under novel, high-stakes pressure, it suggests latent competence; if it fails catastrophically, the hollowing out is complete.',
    'If significant latent competence exists, the constraint''s true ''theater_ratio'' is lower, and its classification might shift closer to a degraded Rope; if not, the Piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_latent_competence, empirical, 'Ambiguity between ritualistic performance and latent operational capability.').

omega_variable(
    ritual_social_cohesion_function,
    'Does the ritualistic performance of drills, despite functional decay, serve an unacknowledged social cohesion or anxiety-reduction function for the public?',
    'Sociological study of public perception and psychological impact of preparedness rituals, especially in the absence of actual disaster.',
    'If a significant social cohesion function is identified, the constraint might possess a hidden coordination function, making its ''extractiveness'' slightly lower when accounting for this non-obvious benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_social_cohesion_function, conceptual, 'Unacknowledged social function of preparedness rituals.').

omega_variable(
    framing_under_determination_husk_vs_competence,
    'Is the ''husk_reading'' the most appropriate framing, or does the ''competence_reading'' offer a more accurate account of preparedness transmission?',
    'Empirical investigation into the actual adaptive capacity demonstrated by civil defense systems in response to novel threats, compared against formal compliance with protocols.',
    'If the ''competence_reading'' is adopted, the constraint''s ''extractiveness'' and ''theater_ratio'' would be significantly lower, and its classification would shift towards a Rope or even Mountain, reflecting genuine functional capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_husk_vs_competence, conceptual, 'Framing under-determination between the ''husk_reading'' and ''competence_reading'' of preparedness transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(prep_tr_t5, preparedness_transmission__husk_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.68).
narrative_ontology:measurement(prep_tr_t15, preparedness_transmission__husk_reading, theater_ratio, 15, 0.72).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prep_be_t5, preparedness_transmission__husk_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(prep_be_t15, preparedness_transmission__husk_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(prep_su_t5, preparedness_transmission__husk_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__husk_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(prep_su_t15, preparedness_transmission__husk_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__husk_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_transmission' kernel. The 'husk_reading' focuses on the decay of operational knowledge, contrasting with the 'competence_reading' (live knowledge) and 'hybrid_reading' (stratified competence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
