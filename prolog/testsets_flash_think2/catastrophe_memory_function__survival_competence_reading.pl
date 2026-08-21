% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Ritual Preserves Survival-Competence (Commemorative Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes a commemorative ritual, such as Passover,
 *   interpreted through the lens of its function in transmitting survival
 *   competence (D5). The ritual is understood as an embodied rehearsal and
 *   knowledge transmission mechanism that equips a community for
 *   institutional transformation and decentralized continuity in the face of
 *   future catastrophes. This is one specific reading of the broader
 *   'catastrophe_memory_function' kernel, focusing on its adaptive capacity
 *   rather than solely on mourning or hybrid functions.
 *
 * KEY AGENTS:
 *   - community_members: Primary beneficiaries and participants (organized/constrained)
 *   - ritual_leaders: Agenda-setters and guides of the practice (organized/constrained)
 *   - future_generations: Inheritors of the transmitted competence (powerless/trapped)
 *   - secular_historians: Analytical observers (analytical/analytical)
 *   - disaffected_members: Excluded from the ritual's benefits (powerless/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.1).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Preserves Survival-Competence (Commemorative Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '6b46d2f1-ad58-47a9-a169-6365b3b8c3e4').
narrative_ontology:cs_kernel_codification('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', formalized).
narrative_ontology:cs_authority_grounding('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', lineage).
narrative_ontology:cs_interpretation_layer_present('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4').
narrative_ontology:cs_reading_relation('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', foundational, embodied_memory_transmits_adaptive_capacity).
narrative_ontology:cs_axiom_status(embodied_memory_transmits_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', embodied_memory_transmits_adaptive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', secondary, catastrophe_requires_institutional_transformation).
narrative_ontology:cs_axiom_status(catastrophe_requires_institutional_transformation, holdable).
narrative_ontology:cs_axiom_grounding('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', catastrophe_requires_institutional_transformation, empirically_contingent).
narrative_ontology:cs_reference_frame('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', post_catastrophe_reconstitution).
narrative_ontology:cs_drift_state('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6b46d2f1-ad58-47a9-a169-6365b3b8c3e4', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, ritual_leaders).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, gaining a shared framework for understanding past catastrophes and rehearsing adaptive responses. They invest time and effort in the practice, but are net beneficiaries of the transmitted competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, community_members, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, community_members, payer).

% Guide the ritual, ensuring its proper execution and the accurate transmission of its lessons. They gain status and authority within the community for their role in preserving collective memory and adaptive capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_leaders, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, ritual_leaders, beneficiary).

% Inherit the survival competence and adaptive capacity transmitted through the ritual, benefiting from the resilience and continuity it provides without direct participation in its creation. Their 'trapped' exit reflects their inheritance of the tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Analyze the ritual's historical development and social function, assessing its efficacy in transmitting adaptive capacity from an external, academic perspective. They do not participate in the ritual's internal dynamics.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, secular_historians, observer,
    analytical, generational, analytical, global).

% Individuals who have left the community or reject the ritual's premises. They are excluded from the direct benefits of the competence transmission, but their departure does not undermine the ritual's function for those who remain.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, disaffected_members, excluded,
    powerless, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits collective memory of past catastrophes and embodied practices for institutional transformation and decentralized continuity, ensuring the community's long-term survival and adaptive capacity.
% TRANSFER_FUNCTION: Transfers knowledge, resilience, and a shared framework for interpreting and responding to future crises across generations, from past experiences to future challenges.
% ABSENT_VOICES: Disaffected members or those who reject the ritual's efficacy are structurally excluded from its benefits, but their absence does not prevent the ritual from functioning for its participants.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would lose a vital, embodied mechanism for collective memory, adaptive capacity, and intergenerational resilience. This would likely make it more vulnerable to future catastrophes and hinder its ability to transform and maintain continuity, leading to a significant reorganization of its social and adaptive structures.
% FOUNDING_PROBLEM: The need to survive and adapt after a catastrophic event, ensuring the community's continuity and preventing future generations from repeating past mistakes or being unprepared for similar challenges.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and sociologists studying collective memory and resilience attest to the ongoing function of such rituals in maintaining group cohesion and adaptive capacity, independent of the community's internal claims. Historical records often corroborate the occurrence of the founding catastrophe.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect the ritual's primary function as a beneficial coordination mechanism, where participation is largely voluntary and driven by perceived collective benefit rather than coercion. The low theater ratio (0.10) indicates that the ritual's performative aspects are genuinely functional in transmitting its lessons. Accessibility collapse is moderate (0.40) because while other forms of historical education exist, the embodied, collective, and intergenerational nature of the ritual offers a unique mode of competence transmission. Resistance is low (0.10) as the ritual is generally embraced by its participants for its perceived value.
 *
 * PERSPECTIVAL GAP:
 *   Participants (community_members, ritual_leaders) experience the ritual as a vital, beneficial practice for collective survival and identity. External observers (secular_historians) may analyze its social function and efficacy, potentially questioning its empirical claims but generally acknowledging its role in cultural transmission. Disaffected members, having exited, would likely view it as irrelevant or even burdensome, but their perspective does not negate its function for those who remain.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are direct beneficiaries, gaining adaptive capacity and resilience. Ritual leaders also benefit from the status and authority derived from their role in maintaining this vital function. There are no identifiable victims, as the costs (time, effort) are shared and generally outweighed by the collective benefits. Disaffected members are excluded by their choice to leave, not by active extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is clearly live. The founding problem of surviving and adapting to catastrophe remains an ongoing concern for any community. The ritual's function in transmitting survival competence is directly relevant to this problem, preventing it from becoming a piton or snare. The classification as a Rope reflects its ongoing, beneficial coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint primarily a mechanism for transmitting survival competence, or is its primary function related to mourning and identity maintenance?',
    'Comparative analysis of ritual texts and participant testimonies across different communities, focusing on explicit statements of purpose and observed behavioral outcomes in response to crises.',
    'If the primary function is mourning, the constraint would align more closely with the ''mourning_practice_reading'' (potentially a different classification, e.g., a more identity-focused Rope or Tangled Rope if coercive). If it''s a hybrid, it would align with the ''hybrid_transformation_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing the primary function of the catastrophe memory ritual.').

omega_variable(
    empirical_efficacy_of_competence_transmission,
    'To what extent does the ritual empirically succeed in transmitting actual survival competence and adaptive capacity, rather than merely symbolic or identity-affirming content?',
    'Longitudinal ethnographic studies and comparative historical analysis of communities that practice such rituals versus those that do not, assessing their differential resilience and adaptive success in the face of real-world catastrophes.',
    'If empirical efficacy is low, the extractiveness might be higher (as participants invest effort for limited return), and the classification might drift towards a Piton (theatrical maintenance) or a Snare (if leaders benefit from false claims of competence). If efficacy is high, the Rope classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_efficacy_of_competence_transmission, empirical, 'Assessing the actual adaptive impact of the ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__survival_competence_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__survival_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__survival_competence_reading, theater_ratio, 75, 0.11).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 25, 0.14).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 75, 0.16).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 75, 0.1).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'catastrophe_memory_function' kernel. This 'survival_competence_reading' focuses on adaptive capacity, while 'mourning_practice_reading' focuses on loss-memory and 'hybrid_transformation_reading' combines both. Each is a separate constraint with distinct structural properties and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
