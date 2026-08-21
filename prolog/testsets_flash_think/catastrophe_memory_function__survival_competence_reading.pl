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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Ritual Preserves Survival Competence (Commemorative Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes a commemorative ritual, such as Passover,
 *   interpreted through the lens of its function in transmitting survival
 *   competence (D5). It is one reading of the broader
 *   'catastrophe_memory_function' kernel. The ritual is understood as an
 *   embodied rehearsal and knowledge transmission mechanism that equips a
 *   community with adaptive capacity for future institutional transformation
 *   and decentralized continuity in the face of crises. The metrics reflect a
 *   low-extraction, functional coordination mechanism.
 *
 * KEY AGENTS:
 *   - community_members: Primary beneficiary (organized/constrained) — gain resilience
 *   - future_generations: Secondary beneficiary (powerless/constrained) — inherit competence
 *   - ritual_leaders: Agenda setter (institutional/identity_locked) — facilitate transmission
 *   - historical_scholars: Analytical observer (analytical/analytical) — study function
 *   - secular_educators: Excluded (organized/mobile) — offer alternative approaches
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Preserves Survival Competence (Commemorative Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '971f8634-21b5-488d-8e76-67fee840d340').
narrative_ontology:cs_kernel_codification('971f8634-21b5-488d-8e76-67fee840d340', formalized).
narrative_ontology:cs_authority_grounding('971f8634-21b5-488d-8e76-67fee840d340', lineage).
narrative_ontology:cs_interpretation_layer_present('971f8634-21b5-488d-8e76-67fee840d340').
narrative_ontology:cs_reading_relation('971f8634-21b5-488d-8e76-67fee840d340', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('971f8634-21b5-488d-8e76-67fee840d340', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('971f8634-21b5-488d-8e76-67fee840d340', foundational, embodied_memory_transmits_resilience).
narrative_ontology:cs_axiom_status(embodied_memory_transmits_resilience, holdable).
narrative_ontology:cs_axiom_grounding('971f8634-21b5-488d-8e76-67fee840d340', embodied_memory_transmits_resilience, empirically_contingent).
narrative_ontology:cs_reference_frame('971f8634-21b5-488d-8e76-67fee840d340', ancestral_adaptive_transmission).
narrative_ontology:cs_drift_state('971f8634-21b5-488d-8e76-67fee840d340', contemporary_community_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('971f8634-21b5-488d-8e76-67fee840d340', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, internalizing its narratives and practices, thereby gaining a shared framework for understanding and responding to catastrophe. They benefit from enhanced collective resilience.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, community_members, beneficiary,
    organized, biographical, constrained, local).

% Are the ultimate recipients of the adaptive capacity and survival competence transmitted through the ritual. They inherit the collective memory and resilience without direct participation in its creation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, constrained, local).

% Facilitate, transmit, and interpret the ritual, ensuring its continuity and relevance. Their identity is often deeply intertwined with the preservation of the ritual and its function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_leaders, agenda_setter,
    institutional, generational, identity_locked, local).

% Analyze the ritual's historical development, social function, and efficacy in transmitting adaptive capacity. They provide an external, academic perspective on its operation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, historical_scholars, observer,
    analytical, civilizational, analytical, global).

% Offer alternative, non-ritualistic methods for transmitting historical knowledge and resilience skills. While not directly harmed by the ritual, their approaches are often seen as distinct or competing, and they are not typically involved in the ritual's internal discourse or transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, secular_educators, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits collective memory of catastrophe and adaptive strategies across generations through embodied rehearsal and shared narratives, enabling institutional transformation and decentralized continuity in the face of future crises.
% TRANSFER_FUNCTION: Transfers embodied knowledge, shared narratives, and psychological resilience from past generations to present and future community members, ensuring the continuity of adaptive capacity.
% ABSENT_VOICES: Those who prioritize purely secular or individualistic approaches to resilience might find the ritual's methods inefficient or superstitious, but they are not part of the ritual's internal discourse or decision-making processes.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would lose a vital, embodied, and communal mechanism for collective resilience and adaptive capacity. This would likely lead to a significant gap in the transmission of survival competence, making the community more vulnerable to future crises and requiring new, potentially less effective, methods of collective memory and adaptation.
% FOUNDING_PROBLEM: The existential challenge of ensuring the long-term survival and adaptive capacity of a community in the face of recurring or catastrophic events, preventing the loss of hard-won lessons and collective resilience across generations.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of ritual, historical accounts of community resilience after crises, and sociological analyses of collective memory all corroborate the role of such rituals in maintaining group cohesion and adaptive capacity. These external analyses support the claim that the founding problem remains relevant and that the ritual addresses it effectively.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.15) and suppression (0.20) reflect that the ritual is primarily a beneficial coordination mechanism, maintained by voluntary participation and perceived value rather than coercion. The low theater ratio (0.10) indicates its functional efficacy. Accessibility collapse (0.40) is moderate because while other forms of education exist, the ritual provides a unique, embodied, and communal mode of transmission. Resistance (0.10) is low as participants generally embrace its perceived benefits. The temporal measurements show a stable, low-extraction profile, consistent with a well-functioning Rope.
 *
 * PERSPECTIVAL GAP:
 *   While the community members and future generations experience the ritual as a direct benefit, and ritual leaders see it as their core function, external observers like historical scholars might analyze its efficacy or historical evolution. Secular educators, though excluded from the ritual's internal framework, might offer alternative, non-ritualistic methods for achieving similar adaptive outcomes, leading to a difference in preferred approaches rather than a direct conflict over the ritual's internal operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are direct beneficiaries, receiving the transmitted competence. Ritual leaders, while facilitating, are also deeply invested in and benefit from the ritual's continuity, placing them near the beneficiary end. There are no direct victims, as the ritual is not designed to extract from participants. Secular educators are excluded from the ritual's framework but are not victims of it.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Rope prevents mislabeling the ritual as a Snare or Tangled Rope. Its low extractiveness and suppression, coupled with clear benefits for participants, confirm its primary function as a genuine coordination mechanism for collective resilience, rather than a disguised form of extraction or a decaying institution. The 'live' status of the founding problem further supports this, indicating the mandate has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily a mechanism for transmitting survival competence, or is its dominant function related to mourning practice or a hybrid of both?',
    'Comparative ethnographic studies across different communities and rituals, focusing on participant-declared intent and observed behavioral outcomes in crisis response, to determine the primary functional emphasis.',
    'If the primary function is found to be mourning or hybrid, the constraint would be reclassified as ''mourning_practice_reading'' or ''hybrid_transformation_reading'' of the ''catastrophe_memory_function'' kernel, potentially altering its extractiveness and beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing the specific reading of the catastrophe memory function kernel.').

omega_variable(
    survival_competence_measurability,
    'How can the ''transmission of survival competence'' be empirically measured and verified as a direct outcome of ritual participation?',
    'Longitudinal studies tracking communities'' adaptive responses to crises, correlating ritual participation levels with resilience metrics, and qualitative analysis of how ritual narratives inform practical decision-making during adversity.',
    'If empirical evidence for competence transmission is weak, the ''empirically_contingent'' grounding of the core axiom would be challenged, potentially shifting the reading''s status or its perceived efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_measurability, empirical, 'Empirical verification of the ritual''s claimed adaptive outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1970, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(cata_tr_t1980, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_memory_function__survival_competence_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_memory_function__survival_competence_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(cata_tr_t2020, catastrophe_memory_function__survival_competence_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t1970, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(cata_be_t1980, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(cata_be_t1990, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1990, 0.14).
narrative_ontology:measurement(cata_be_t2000, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(cata_be_t2010, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(cata_be_t2020, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1970, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(cata_su_t1980, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1980, 0.19).
narrative_ontology:measurement(cata_su_t1990, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(cata_su_t2000, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(cata_su_t2010, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(cata_su_t2020, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
