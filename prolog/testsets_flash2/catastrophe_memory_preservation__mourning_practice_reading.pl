% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Memory Preservation (Mourning Practice Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the function of ritual practices in preserving
 *   the memory of past catastrophes, specifically focusing on their role in
 *   maintaining symbolic continuity and collective identity within a
 *   community. This reading emphasizes the non-operational, purely symbolic
 *   and social function of such rituals, distinguishing it from readings that
 *   focus on practical survival skills or the atrophy of such skills.
 *   Participation is voluntary, and the 'extraction' is minimal, representing
 *   the social cost of maintaining the ritual rather than a coercive
 *   transfer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.05).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Memory Preservation (Mourning Practice Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4').
narrative_ontology:cs_kernel_codification('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', implicit).
narrative_ontology:cs_authority_grounding('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', practice).
narrative_ontology:cs_interpretation_layer_present('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4').
narrative_ontology:cs_reading_relation('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', foundational, ritual_as_symbolic_continuity).
narrative_ontology:cs_axiom_status(ritual_as_symbolic_continuity, holdable).
narrative_ontology:cs_axiom_grounding('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', ritual_as_symbolic_continuity, conventional).
narrative_ontology:cs_axiom('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', foundational, collective_identity_through_shared_grief).
narrative_ontology:cs_axiom_status(collective_identity_through_shared_grief, holdable).
narrative_ontology:cs_axiom_grounding('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', collective_identity_through_shared_grief, deontological).
narrative_ontology:cs_reference_frame('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', communal_symbolic_cohesion).
narrative_ontology:cs_drift_state('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', contemporary_secular_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dcef40cd-a5c6-4fbc-82f7-86f7d19fd6d4', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate voluntarily in rituals that commemorate past catastrophes, reinforcing shared identity and emotional bonds. They derive a sense of belonging and continuity from these practices. Exit is possible but entails a loss of communal connection.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, community_members, beneficiary,
    moderate, generational, mobile, local).

% Organize and guide the commemorative rituals, ensuring their proper execution and transmission across generations. They benefit from the social capital and authority derived from their role, but are constrained by the community's expectations and traditions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_leaders, agenda_setter,
    organized, biographical, constrained, local).

% Study the evolution and function of these rituals, analyzing their role in collective memory and identity formation. They provide an external, critical perspective on the constraint's operation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, historical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective mourning and remembrance, providing a shared framework for processing historical trauma and reinforcing group solidarity through symbolic action.
% TRANSFER_FUNCTION: Transfers symbolic meaning, emotional resonance, and a sense of shared history across generations within the community. No direct material or operational transfer is involved.
% ABSENT_VOICES: Those who have left the community or reject the significance of the commemorated catastrophe might view the rituals as irrelevant or burdensome, but their absence is typically due to disinterest rather than active suppression.
% DISAPPEARANCE_RATIONALE: If these mourning practices vanished, the community would lose a vital mechanism for collective identity and memory. While individuals might find other ways to remember, the shared symbolic framework and communal cohesion would erode, leading to a gradual fragmentation of the group's historical consciousness.
% FOUNDING_PROBLEM: The problem of how to collectively process and remember catastrophic events to prevent their recurrence and maintain group cohesion in the face of trauma.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and historical scholars attest that the need for collective memory and identity reinforcement in the face of historical trauma remains a live concern, even if the specific threats have evolved. Anthropological studies of similar communities corroborate the enduring function of such rituals.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because participation is voluntary and the 'cost' is primarily time and emotional engagement, which is reciprocated by a sense of belonging. Suppression is very low (0.05) as there is no active enforcement mechanism; non-participation is met with social distance, not coercion. Theater ratio is low (0.2) because the primary function is symbolic and social, which is inherently performative, but not deceptive. Accessibility collapse is low (0.1) as alternatives for individual remembrance exist, but not for collective identity formation. Resistance is low (0.05) due to the voluntary nature and perceived benefits.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a beneficial, identity-affirming practice. From an external, analytical perspective, it is a social coordination mechanism with minimal overhead. The classification as a Rope reflects this shared benefit and low coercive overhead.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are beneficiaries (d near 0.0) as they gain collective identity and emotional support. Ritual leaders are agenda-setters (d near 0.5) as they facilitate the process and gain social capital, but also bear the responsibility of maintaining the tradition. There are no identifiable victims in this reading, as participation is opt-in and the costs are diffuse and voluntary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_ambiguity,
    'Is the primary function of these rituals truly symbolic and identity-preserving, or do they retain latent operational functions for threat-recognition or survival competence?',
    'Longitudinal ethnographic study comparing ritual content to actual disaster preparedness and response outcomes, or historical analysis of ritual evolution in response to changing threats.',
    'If latent operational functions are significant, the constraint might shift towards a Tangled Rope (if some are coordinated for survival while others pay through ritual burden) or even a Snare (if the symbolic function masks a coercive operational demand). If purely symbolic, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_ambiguity, empirical, 'Ambiguity between symbolic and operational functions of ritual.').

omega_variable(
    voluntary_vs_social_pressure,
    'To what extent is participation truly voluntary, versus being driven by subtle social pressure or identity-lock mechanisms?',
    'Surveys of community members on perceived freedom to exit, analysis of social consequences for non-participants, or comparative studies with communities where similar rituals are actively enforced.',
    'If social pressure is high, the suppression metric might be understated, and the exit_options for community_members could be closer to ''identity_locked'' or ''constrained'', potentially shifting the classification towards a Tangled Rope if the pressure is asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_social_pressure, empirical, 'Distinguishing voluntary participation from social pressure in ritual.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''catastrophe_memory_preservation'' kernel, how do the ''mourning_practice_reading'', ''survival_competence_reading'', and ''hybrid_atrophy_reading'' structurally differ in their claims about ritual function and extraction?',
    'Comparative analysis of the three constraint stories, focusing on their declared extractiveness, suppression, beneficiary/victim sets, and claimed types.',
    'The ''mourning_practice_reading'' (this story) posits low extraction and no victims, classifying as a Rope. The ''survival_competence_reading'' would likely have higher extractiveness (cost of training/preparation) and potential victims (those who fail to comply with survival protocols), possibly a Tangled Rope. The ''hybrid_atrophy_reading'' would show a historical shift in metrics, starting higher and declining, potentially moving from Tangled Rope to Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between sibling readings of the catastrophe memory preservation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_preservation' kernel. This 'mourning_practice_reading' focuses on symbolic and identity-preserving functions, while 'survival_competence_reading' emphasizes operational threat-recognition, and 'hybrid_atrophy_reading' describes a historical degradation of function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
