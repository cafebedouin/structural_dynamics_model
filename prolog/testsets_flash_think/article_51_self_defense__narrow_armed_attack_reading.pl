% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Self-Defense: Narrow Armed Attack Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents the narrow interpretation of Article 51 of the
 *   UN Charter, limiting the right of self-defense to responses to an actual
 *   or imminent armed attack by a state, with strict requirements for
 *   attribution. It is a reading that prioritizes state sovereignty and the
 *   collective security mechanism of the UN, acting as a significant
 *   constraint on unilateral force. This story is one reading of the
 *   'article_51_self_defense' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.65).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.78).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense: Narrow Armed Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '65f56510-9a0f-416f-93f2-2cdc0e509cdc').
narrative_ontology:cs_kernel_codification('65f56510-9a0f-416f-93f2-2cdc0e509cdc', fixed_text).
narrative_ontology:cs_authority_grounding('65f56510-9a0f-416f-93f2-2cdc0e509cdc', lineage).
narrative_ontology:cs_interpretation_layer_present('65f56510-9a0f-416f-93f2-2cdc0e509cdc').
narrative_ontology:cs_reading_relation('65f56510-9a0f-416f-93f2-2cdc0e509cdc', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('65f56510-9a0f-416f-93f2-2cdc0e509cdc', article_51_self_defense__unable_unwilling_doctrine_reading, forecloses).
narrative_ontology:cs_axiom('65f56510-9a0f-416f-93f2-2cdc0e509cdc', foundational, state_attribution_is_paramount).
narrative_ontology:cs_axiom_status(state_attribution_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('65f56510-9a0f-416f-93f2-2cdc0e509cdc', state_attribution_is_paramount, conventional).
narrative_ontology:cs_axiom('65f56510-9a0f-416f-93f2-2cdc0e509cdc', foundational, force_only_against_state_armed_attack).
narrative_ontology:cs_axiom_status(force_only_against_state_armed_attack, holdable).
narrative_ontology:cs_axiom_grounding('65f56510-9a0f-416f-93f2-2cdc0e509cdc', force_only_against_state_armed_attack, deontological).
narrative_ontology:cs_reference_frame('65f56510-9a0f-416f-93f2-2cdc0e509cdc', un_charter_state_centric_order).
narrative_ontology:cs_drift_state('65f56510-9a0f-416f-93f2-2cdc0e509cdc', contemporary_post_9_11_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65f56510-9a0f-416f-93f2-2cdc0e509cdc', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, un_charter_supremacy).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, state_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constraint on unilateral force by powerful states, which protects their sovereignty and territorial integrity. Their security relies on the collective security framework and the strict interpretation of self-defense.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    powerless, generational, constrained, global).

% Are constrained in their strategic freedom to use force unilaterally, particularly against non-state actors or in preemptive scenarios not directly attributable to a state. They bear the cost of needing UN Security Council authorization or clear attribution for military action, but also shape international law.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, powerful_states, agenda_setter).

% Holds the primary authority for authorizing the use of force under international law, reinforcing the narrow reading of Article 51. Its legitimacy and power are preserved by this constraint, as it centralizes decisions on collective security.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% Analyze and interpret international law, often advocating for strict adherence to the UN Charter's provisions on the use of force. They provide intellectual support for the narrow reading, influencing judicial and political discourse.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% Their authority and role in maintaining international peace and security are reinforced by the narrow interpretation, which channels disputes and uses of force through established legal and political mechanisms.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, beneficiary,
    organized, generational, constrained, global).

% Are not considered subjects of international law in the same way as states, and their actions do not directly trigger Article 51 self-defense unless attributable to a state. They are outside the formal legal framework governing inter-state use of force.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_state_armed_groups, excluded,
    powerless, immediate, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior to prevent unilateral uses of force, maintain international peace and security, and uphold the principle of state sovereignty by channeling responses to aggression through a collective security framework.
% TRANSFER_FUNCTION: Transfers strategic freedom and the right to unilaterally use force from individual powerful states to the collective security framework, primarily the UN Security Council. It also transfers legitimacy to multilateral institutions as arbiters of international law.
% ABSENT_VOICES: States advocating for broader interpretations of self-defense (e.g., preemptive action against non-state actors without clear state attribution) are often marginalized in formal legal discourse, as are non-state actors who are not recognized as legitimate subjects of Article 51.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished, it would likely lead to a significant increase in unilateral uses of force by powerful states, a weakening of the UN Security Council's authority, and a general erosion of international law governing the use of force, fundamentally reorganizing global security dynamics.
% FOUNDING_PROBLEM: The problem of states resorting to unilateral force and aggression, leading to widespread conflict and undermining international peace and security, as witnessed in the World Wars.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, weaker states, and UN officials consistently attest that the founding problem of preventing unilateral aggression remains live, even as its application to new threats (like non-state actors) is contested. The UN Charter itself is a testament to this ongoing concern.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates international peace and protects weaker states (beneficiaries) while simultaneously extracting strategic freedom from powerful states (victims) by limiting their unilateral use of force. Extractiveness (0.65) is substantial due to the significant limitations placed on powerful states' strategic options. Suppression (0.78) is high, reflecting the active diplomatic, legal, and political enforcement required to maintain this interpretation against pressures for broader readings. Theater ratio (0.20) is relatively low, as the core function of preventing unilateral aggression remains a serious concern, though some performative adherence exists. Resistance (0.75) is high, as powerful states frequently challenge or circumvent this narrow reading.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states, as payers, experience this constraint as a significant limitation on their ability to respond to perceived threats, especially from non-state actors, leading them to seek alternative interpretations or justifications for force. Weaker states and multilateral institutions, as beneficiaries, perceive it as a vital safeguard for international order and their own sovereignty. The UN Security Council, as an agenda-setter, benefits from the centralization of authority over the use of force.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states are targets (high d) as their strategic freedom is curtailed. Weaker states and multilateral institutions are beneficiaries (low d) as their security and authority are enhanced. The UN Security Council, while an agenda-setter, also benefits from its central role in authorizing force. International legal scholars act as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to prevent unilateral aggression remains live, but its application is contested in the face of evolving threats (e.g., terrorism, cyber warfare). The classification as a Tangled Rope prevents mislabeling it as a pure Snare (ignoring its coordination function for weaker states) or a pure Rope (ignoring the extraction from powerful states). The ongoing contestation over its interpretation highlights the tension between its founding problem and contemporary security challenges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_definition_ambiguity,
    'What constitutes an ''imminent'' armed attack in the context of modern threats (e.g., cyberattacks, rapidly developing WMD programs)?',
    'Development of new international legal precedents or a UN General Assembly resolution clarifying the temporal and evidentiary thresholds for imminence.',
    'A stricter definition would further constrain powerful states, reinforcing the narrow reading. A looser definition would move towards the ''expansive_preventive_reading'', increasing extraction from weaker states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''imminent armed attack''.').

omega_variable(
    state_attribution_threshold,
    'What level of state involvement or control is required for an armed attack by a non-state actor to be ''attributable'' to a state under international law?',
    'Further ICJ jurisprudence or a consensus among states on the ''effective control'' or ''overall control'' standards for attribution.',
    'A high attribution threshold reinforces the narrow reading, limiting self-defense against non-state actors. A lower threshold would move towards the ''unable_unwilling_doctrine_reading'', increasing the scope for unilateral action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_attribution_threshold, empirical, 'Ambiguity in the threshold for state attribution of non-state actor attacks.').

omega_variable(
    reading_persistence_under_pressure,
    'Can the narrow reading of Article 51 persist as the dominant interpretation given ongoing pressure from powerful states for broader interpretations in response to non-state actor threats?',
    'Analysis of state practice, ICJ rulings, and UN Security Council resolutions over the next decade to determine if the narrow reading is consistently upheld or increasingly circumvented/reinterpreted.',
    'If the narrow reading is consistently circumvented, its effective suppression and extractiveness will decrease, potentially leading to a reclassification towards a Piton (if its function atrophies) or a shift in the dominant kernel reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_persistence_under_pressure, empirical, 'The long-term viability of the narrow reading against geopolitical pressures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(arti_tr_t1965, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(arti_tr_t1985, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(arti_tr_t2010, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(arti_tr_t2025, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement(arti_be_t1965, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(arti_be_t1985, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(arti_be_t2010, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(arti_be_t2025, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(arti_su_t1965, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1965, 0.72).
narrative_ontology:measurement(arti_su_t1985, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1985, 0.74).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.76).
narrative_ontology:measurement(arti_su_t2010, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(arti_su_t2025, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, un_collective_security_system).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, international_humanitarian_law).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'article_51_self_defense' kernel. Each reading has a different ε value and structural implications, necessitating separate constraint stories. This reading emphasizes strict state attribution and an actual/imminent armed attack.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
