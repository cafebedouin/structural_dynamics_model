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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Article 51 Self-Defense (Narrow Armed Attack Reading)
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'narrow armed attack' reading of Article
 *   51 of the UN Charter, which limits the right of self-defense to responses
 *   to an actual or imminent armed attack attributable to a state. This
 *   reading emphasizes state sovereignty and the UN Security Council's role,
 *   constraining unilateral force by powerful states. It is one reading of
 *   the 'article_51_self_defense' kernel, distinct from more expansive
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.65).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.75).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense (Narrow Armed Attack Reading)").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, 'e0406b44-ceb2-4c74-bdd8-5d64c8516b0d').
narrative_ontology:cs_kernel_codification('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', fixed_text).
narrative_ontology:cs_authority_grounding('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', lineage).
narrative_ontology:cs_interpretation_layer_present('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d').
narrative_ontology:cs_reading_relation('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', foundational, force_prohibition_primacy).
narrative_ontology:cs_axiom_status(force_prohibition_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', force_prohibition_primacy, deontological).
narrative_ontology:cs_axiom('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', foundational, state_attribution_necessity).
narrative_ontology:cs_axiom_status(state_attribution_necessity, holdable).
narrative_ontology:cs_axiom_grounding('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', state_attribution_necessity, conventional).
narrative_ontology:cs_reference_frame('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', un_charter_original_intent).
narrative_ontology:cs_drift_state('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', post_9_11_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e0406b44-ceb2-4c74-bdd8-5d64c8516b0d', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_legal_order).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_threats).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constraint on unilateral force by powerful states, which protects their sovereignty and reduces the risk of intervention. Their security depends on the international legal order's stability.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    powerless, generational, trapped, global).

% Their authority to authorize force is preserved and strengthened by this narrow reading, which channels responses to armed attacks through collective security mechanisms. They administer the legal framework.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, agenda_setter,
    institutional, generational, constrained, global).

% The coherence and stability of the international legal framework, particularly the prohibition on the use of force, are upheld by this reading. It is a vindicated proposition, not an actor.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_51_self_defense__narrow_armed_attack_reading, international_legal_order).

% Their strategic freedom to use force unilaterally, especially against non-state actors or emerging threats, is significantly constrained. They bear the cost of needing UN Security Council authorization or clear attribution to a state.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states, payer,
    powerful, biographical, constrained, global).

% Face threats from non-state actors that may not be attributable to a host state, limiting their ability to invoke Article 51 for self-defense. They bear the cost of delayed or legally ambiguous responses, potentially increasing their vulnerability.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_threats, payer,
    moderate, immediate, identity_locked, national).

% Analyze and interpret Article 51, advocating for the narrow reading as essential for maintaining the integrity of the UN Charter's prohibition on the use of force. Their work reinforces the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state responses to armed attacks by channeling them through a clear legal framework, preventing unilateral uses of force and preserving the UN Security Council's primary role in maintaining international peace and security.
% TRANSFER_FUNCTION: Transfers the authority to determine the legality of force from individual states to the international legal framework and multilateral institutions, particularly the UN Security Council.
% ABSENT_VOICES: States advocating for a broader right to self-defense against non-state actors or emerging threats, who argue that the narrow reading leaves them vulnerable. They are often powerful states whose strategic interests are constrained by this interpretation.
% DISAPPEARANCE_RATIONALE: If this narrow reading vanished, powerful states would likely assert a broader right to use force, leading to increased unilateral interventions, a weakening of the UN Security Council's authority, and a more volatile international security environment. The international legal order would be significantly destabilized.
% FOUNDING_PROBLEM: The problem of preventing unilateral uses of force by states and ensuring that self-defense is not used as a pretext for aggression, following the devastation of two World Wars.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and weaker states consistently attest that the problem of preventing unilateral aggression remains live, and that the narrow reading is crucial for maintaining international peace and security. The UN Charter's drafters' intent and subsequent state practice (outside of specific contested cases) corroborate this.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.65) because powerful states perceive a significant cost in their strategic freedom. Suppression (0.75) is also high, reflecting the active diplomatic and legal pressure to adhere to this interpretation, especially from weaker states and multilateral institutions. Theater ratio (0.20) is relatively low, as the legal arguments and enforcement mechanisms are generally genuine, though some states may pay lip service while seeking loopholes. The post-9/11 period (around 2001) saw a peak in extractiveness and suppression as powerful states sought to broaden self-defense, leading to increased contestation and enforcement pressure against the narrow reading.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states experience this as a highly extractive constraint on their sovereignty, while weaker states and international institutions perceive it as a vital coordination mechanism for global security. The engine's per-seat classification will reflect this divergence based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states and multilateral institutions are beneficiaries (low d) as this reading protects their sovereignty and authority. Powerful states and those facing non-state threats are payers (high d) as their strategic options are curtailed. The international legal order itself is a non-agent beneficiary, as its integrity is upheld.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_standard_ambiguity,
    'What standard of attribution is required to link a non-state actor''s attack to a state, and is this standard consistently applied?',
    'Analysis of ICJ jurisprudence and consistent state practice in attributing non-state actor actions to states. Clarification through UN General Assembly resolutions or Security Council practice.',
    'A high, consistently applied attribution standard reinforces the narrow reading''s constraint on unilateral force. A low or inconsistently applied standard would allow powerful states to bypass the constraint, shifting it towards a more expansive interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_standard_ambiguity, empirical, 'Ambiguity in attributing non-state actor attacks to states under international law.').

omega_variable(
    imminence_threshold_ambiguity,
    'What constitutes ''imminent'' armed attack in the context of modern threats, particularly those involving non-state actors or cyber warfare?',
    'Development of new international legal norms or interpretations by the ICJ or UN Security Council that define ''imminence'' for contemporary threats.',
    'A strict, traditional interpretation of imminence reinforces the narrow reading. A more flexible interpretation, allowing for ''accumulated'' or ''anticipatory'' imminence, would push the constraint towards a more expansive reading, increasing extractiveness on weaker states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_threshold_ambiguity, conceptual, 'Ambiguity in the definition of ''imminent'' armed attack.').

omega_variable(
    reading_coexistence_stability,
    'How stable is the coexistence of the narrow reading with more expansive interpretations, and what factors might lead to one foreclosing the others?',
    'Longitudinal study of state practice, ICJ rulings, and UN Security Council resolutions. Analysis of shifts in geopolitical power and the nature of global threats.',
    'If the narrow reading is increasingly challenged by state practice and powerful state claims, its effective suppression and extractiveness on powerful states may decrease, leading to a de facto shift towards a more expansive interpretation, even if not formally foreclosed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_stability, empirical, 'Stability of the narrow reading''s coexistence with other interpretations of Article 51.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(arti_tr_t1960, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(arti_tr_t1980, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(arti_tr_t2010, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(arti_be_t1960, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(arti_be_t1980, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(arti_be_t2010, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(arti_su_t1960, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(arti_su_t1980, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.8).
narrative_ontology:measurement(arti_su_t2010, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, prohibition_on_use_of_force_jus_cogens).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'article_51_self_defense' kernel. Its structural delta is a high constraint on unilateral force, preserving the authority of multilateral institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
