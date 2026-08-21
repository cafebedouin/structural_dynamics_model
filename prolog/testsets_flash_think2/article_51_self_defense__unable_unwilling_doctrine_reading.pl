% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: 'Unwilling or Unable' Doctrine
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'unwilling or unable' doctrine as an
 *   interpretation of UN Charter Article 51, which permits states to use
 *   force in self-defense. This reading asserts that self-defense is
 *   triggered when a non-state actor attack originates from a host state that
 *   is unwilling or unable to suppress the threat, thereby justifying
 *   unilateral intervention. It is a hybrid constraint, aiming to coordinate
 *   security against transnational threats while extracting a cost in terms
 *   of host state sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.75).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: 'Unwilling or Unable' Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'e055d308-a1e6-4af3-91ec-14c964212e52').
narrative_ontology:cs_kernel_codification('e055d308-a1e6-4af3-91ec-14c964212e52', fixed_text).
narrative_ontology:cs_authority_grounding('e055d308-a1e6-4af3-91ec-14c964212e52', lineage).
narrative_ontology:cs_interpretation_layer_present('e055d308-a1e6-4af3-91ec-14c964212e52').
narrative_ontology:cs_reading_relation('e055d308-a1e6-4af3-91ec-14c964212e52', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('e055d308-a1e6-4af3-91ec-14c964212e52', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_axiom('e055d308-a1e6-4af3-91ec-14c964212e52', foundational, state_sovereignty_is_conditional_on_threat_suppression).
narrative_ontology:cs_axiom_status(state_sovereignty_is_conditional_on_threat_suppression, holdable).
narrative_ontology:cs_axiom_grounding('e055d308-a1e6-4af3-91ec-14c964212e52', state_sovereignty_is_conditional_on_threat_suppression, conventional).
narrative_ontology:cs_axiom('e055d308-a1e6-4af3-91ec-14c964212e52', foundational, effective_self_defense_against_non_state_actors_is_necessary).
narrative_ontology:cs_axiom_status(effective_self_defense_against_non_state_actors_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e055d308-a1e6-4af3-91ec-14c964212e52', effective_self_defense_against_non_state_actors_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('e055d308-a1e6-4af3-91ec-14c964212e52', post_9_11_security_paradigm).
narrative_ontology:cs_drift_state('e055d308-a1e6-4af3-91ec-14c964212e52', contemporary_counterterrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e055d308-a1e6-4af3-91ec-14c964212e52', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, counterterrorism_alliances).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_deemed_unable_unwilling).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that invoke the doctrine to justify military action against non-state actors in other sovereign territories. They benefit from the legal flexibility to address perceived threats directly, bypassing host state consent when the host is deemed 'unwilling or unable'.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states, agenda_setter,
    institutional, generational, mobile, global).

% Sovereign states whose territory is used by non-state actors and who are then subjected to military intervention by other states under this doctrine. They bear the cost of sovereignty infringement, potential instability, and loss of control over their own territory.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_deemed_unable_unwilling, payer,
    powerless, biographical, trapped, national).

% The primary targets of interventions under this doctrine. While they are the initial threat, the doctrine's application often leads to their displacement or fragmentation rather than outright suppression, sometimes exacerbating regional instability.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, regional).

% The primary international body responsible for authorizing the use of force. Its members often debate the legality and prudence of interventions under this doctrine, sometimes legitimizing or condemning specific actions, but lacking a unified stance on the doctrine itself.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% Academics and legal experts who analyze and critique the doctrine's evolution, consistency with international law, and practical implications. They influence legal discourse but have no direct enforcement power.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% Organizations concerned with civilian protection and humanitarian aid. They often bear the consequences of military interventions and advocate for adherence to international humanitarian law, but their concerns are frequently secondary to security imperatives.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, humanitarian_organizations, excluded,
    moderate, immediate, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the collective security problem posed by transnational non-state armed groups operating from states that are genuinely unwilling or unable to suppress them, providing a framework for states to act in self-defense.
% TRANSFER_FUNCTION: Transfers the effective right to use force against non-state actors from the host state to the intervening state, and shifts the burden of suppressing such threats to the intervening state, often at the cost of host state sovereignty.
% ABSENT_VOICES: The populations of host states, who often bear the brunt of interventions, and some international legal purists who argue for strict adherence to state sovereignty and UN Security Council authorization. Their perspectives are often marginalized in the security discourse.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, intervening states would face a significant legal vacuum for addressing cross-border non-state threats. This would likely lead to either increased impunity for non-state actors, or a return to more overtly illegal unilateral interventions without any legal justification, fundamentally reorganizing international security responses.
% FOUNDING_PROBLEM: The rise of transnational non-state armed groups (e.g., Al-Qaeda) operating from states that either supported them or lacked the capacity to control them, challenging traditional notions of state-on-state armed attack and creating a perceived security gap.
% FOUNDING_PROBLEM_CORROBORATION: Intervening states and their intelligence agencies consistently attest to the problem's live status, citing ongoing transnational terrorist threats. Some international security analysts and think tanks also corroborate the persistent challenge. However, host states and some legal scholars contest the doctrine's necessity or legality, arguing it's a pretext for intervention.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the doctrine allows for significant infringement on host state sovereignty without explicit consent. Suppression is also high (0.75) as it involves military force and diplomatic pressure to override host state objections. Theater ratio is low (0.15) because the doctrine is invoked for serious security concerns, though its application is often contested. Accessibility collapse is moderate (0.6) for host states, as their options are limited once deemed 'unwilling or unable'. Resistance is high (0.7) from host states and some international legal bodies, reflecting the doctrine's controversial nature.
 *
 * PERSPECTIVAL GAP:
 *   Intervening states perceive this doctrine as a necessary and legitimate tool for collective security in an era of transnational threats. Host states, however, often view it as an illegitimate infringement on their sovereignty and a dangerous precedent for unilateral action. The engine's per-seat classification will reflect this divergence, with intervening states likely seeing a 'rope' or 'scaffold' and host states experiencing a 'snare' or 'tangled_rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states are clear beneficiaries, gaining the legal justification for action. Host states deemed 'unwilling or unable' are the primary victims, bearing the costs of sovereignty bypass and intervention. Non-state armed groups are targets of the intervention. The UN Security Council and legal scholars act as observers, while humanitarian organizations are often excluded from the core decision-making.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine avoids being a pure snare by claiming a genuine coordination function: addressing a security vacuum created by non-state actors. It avoids being a piton because it is actively enforced and vigorously debated, indicating live function and contestation rather than mere inertial performance. The 'unwilling or unable' criterion, while contested, provides a legal hook that prevents it from being purely extractive without justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_unable_objectivity,
    'Is the ''unwilling or unable'' criterion an objectively verifiable standard, or is it primarily a political determination made by intervening states?',
    'Analysis of UN Security Council resolutions, ICJ advisory opinions, and independent fact-finding missions on specific cases to assess the consistency and impartiality of the ''unwilling or unable'' determination.',
    'If primarily political, the doctrine''s extractiveness is higher, as it serves as a pretext for intervention rather than a genuine legal threshold, pushing it closer to a Snare. If objectively verifiable, its coordination function is stronger, supporting a Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwilling_unable_objectivity, empirical, 'Objectivity vs. political determination of the ''unwilling or unable'' criterion.').

omega_variable(
    doctrine_effectiveness_vs_displacement,
    'Does the application of the ''unwilling or unable'' doctrine effectively suppress non-state actor threats, or does it primarily displace them to other regions, creating new security challenges?',
    'Longitudinal empirical studies tracking the operational outcomes of interventions, including the long-term trajectory of targeted non-state actors and regional stability metrics.',
    'If it primarily displaces threats, the doctrine''s coordination function is weaker, and its overall utility as a security mechanism is diminished, increasing its effective extractiveness from host states without achieving its stated goal. If it effectively suppresses threats, its coordination function is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_effectiveness_vs_displacement, empirical, 'Effectiveness of the doctrine in suppressing threats versus mere displacement.').

omega_variable(
    kernel_interpretation_legitimacy,
    'Is this ''unable_unwilling_doctrine_reading'' a legitimate evolution of international law, or a self-serving reinterpretation that undermines the foundational principle of state sovereignty?',
    'Consensus among international legal bodies (e.g., ICJ, ILC), widespread state practice, and the absence of consistent, strong condemnation from the UN General Assembly or Security Council.',
    'If deemed an illegitimate reinterpretation, the doctrine''s legal authority erodes, increasing its effective suppression and extractiveness as it relies more on raw power than legal justification. If recognized as legitimate, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_legitimacy, conceptual, 'Legitimacy of the ''unwilling or unable'' doctrine as an interpretation of Article 51.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(arti_tr_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(arti_tr_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2009, 0.14).
narrative_ontology:measurement(arti_tr_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2013, 0.16).
narrative_ontology:measurement(arti_tr_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2017, 0.17).
narrative_ontology:measurement(arti_tr_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2021, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(arti_be_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(arti_be_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2009, 0.63).
narrative_ontology:measurement(arti_be_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2013, 0.66).
narrative_ontology:measurement(arti_be_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2017, 0.68).
narrative_ontology:measurement(arti_be_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2021, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(arti_su_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(arti_su_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2009, 0.73).
narrative_ontology:measurement(arti_su_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2013, 0.76).
narrative_ontology:measurement(arti_su_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2017, 0.78).
narrative_ontology:measurement(arti_su_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2021, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, un_charter_article_2_4_prohibition_on_force).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, state_sovereignty_principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
