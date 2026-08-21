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
 *   to an actual or imminent armed attack by a state, attributable under
 *   international law. It explicitly excludes preemptive strikes against
 *   non-state actors unless their actions are directly attributable to a host
 *   state. This reading prioritizes state sovereignty and the collective
 *   security framework over unilateral action, particularly by powerful
 *   states. The constraint is claimed as a Tangled Rope because it genuinely
 *   coordinates the use of force but also extracts strategic freedom from
 *   powerful states and those facing non-state threats, while benefiting
 *   weaker states and multilateral institutions.
 *
 * KEY AGENTS:
 *   - weaker_states: Primary beneficiary (powerless/trapped) — protected by constraint
 *   - multilateral_institutions: Agenda setter (institutional/constrained) — authority preserved
 *   - powerful_states_seeking_unilateral_action: Primary payer (powerful/constrained) — strategic freedom constrained
 *   - states_facing_non_state_actor_threats: Victim (moderate/identity_locked) — constrained in responding to non-state threats
 *   - international_legal_scholars: Observer (analytical/analytical) — interpret and advocate for strict adherence
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
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '14d33150-ca91-4e94-94d6-e29b2cb50db9').
narrative_ontology:cs_kernel_codification('14d33150-ca91-4e94-94d6-e29b2cb50db9', fixed_text).
narrative_ontology:cs_authority_grounding('14d33150-ca91-4e94-94d6-e29b2cb50db9', lineage).
narrative_ontology:cs_interpretation_layer_present('14d33150-ca91-4e94-94d6-e29b2cb50db9').
narrative_ontology:cs_reading_relation('14d33150-ca91-4e94-94d6-e29b2cb50db9', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_reading_relation('14d33150-ca91-4e94-94d6-e29b2cb50db9', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('14d33150-ca91-4e94-94d6-e29b2cb50db9', foundational, force_only_in_response_to_armed_attack).
narrative_ontology:cs_axiom_status(force_only_in_response_to_armed_attack, holdable).
narrative_ontology:cs_axiom_grounding('14d33150-ca91-4e94-94d6-e29b2cb50db9', force_only_in_response_to_armed_attack, deontological).
narrative_ontology:cs_axiom('14d33150-ca91-4e94-94d6-e29b2cb50db9', foundational, state_attribution_is_prerequisite).
narrative_ontology:cs_axiom_status(state_attribution_is_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('14d33150-ca91-4e94-94d6-e29b2cb50db9', state_attribution_is_prerequisite, conventional).
narrative_ontology:cs_reference_frame('14d33150-ca91-4e94-94d6-e29b2cb50db9', post_un_charter_collective_security).
narrative_ontology:cs_drift_state('14d33150-ca91-4e94-94d6-e29b2cb50db9', contemporary_non_state_actor_threats_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('14d33150-ca91-4e94-94d6-e29b2cb50db9', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_legal_order).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states_seeking_unilateral_action).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_actor_threats).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constraint on powerful states' unilateral use of force, which protects their sovereignty and territorial integrity. Their security depends on the strict interpretation of international law.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    powerless, generational, trapped, global).

% Their authority to authorize force (e.g., UN Security Council) is preserved by this narrow reading, reinforcing the collective security framework. They administer and interpret the legal framework.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, agenda_setter,
    institutional, civilizational, constrained, global).

% Bear the cost of constrained strategic freedom, as they cannot unilaterally use force against non-state actors or in preventive strikes without clear attribution to a state. They often push for broader interpretations.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states_seeking_unilateral_action, payer,
    powerful, biographical, constrained, global).

% Are victims when non-state actor threats (e.g., terrorist groups) operate from a neighboring state but cannot be clearly attributed to that state, leaving them without a clear Article 51 self-defense justification for cross-border action. Their identity as sovereign states requires adherence to international law, even when it constrains their immediate security.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_actor_threats, payer,
    moderate, immediate, identity_locked, national).

% Analyze and interpret Article 51, often advocating for strict adherence to its text and historical context to preserve the integrity of international law against unilateralism.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% The abstract system of rules and norms benefits from this reading, as it upholds the principle of non-intervention and limits the resort to force, thereby promoting stability and predictability in international relations.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_order, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_51_self_defense__narrow_armed_attack_reading, international_legal_order).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legitimate use of force in international relations by establishing clear, restrictive conditions for self-defense, preventing a free-for-all where states unilaterally decide when to use force.
% TRANSFER_FUNCTION: Transfers strategic freedom and the right to unilaterally use force from powerful states to the collective security mechanism of the UN Security Council and the international legal order.
% ABSENT_VOICES: States facing significant non-state actor threats that cannot be attributed to a host state would argue for a broader interpretation, but their immediate security concerns are subordinated to the collective interest in limiting unilateral force.
% DISAPPEARANCE_RATIONALE: If this narrow reading vanished, powerful states would likely assert broader rights to use force, leading to increased unilateral interventions, a weakening of multilateral institutions, and a more unstable international system where weaker states' sovereignty is more easily violated.
% FOUNDING_PROBLEM: The problem of preventing aggressive war and limiting the unilateral use of force by states, particularly after the devastation of two World Wars.
% FOUNDING_PROBLEM_CORROBORATION: The UN Charter itself, the International Court of Justice's consistent jurisprudence, and the majority of international legal scholarship corroborate that the founding problem of preventing aggressive war remains live and central to the international legal order. Weaker states and multilateral institutions consistently affirm this interpretation.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.65) is substantial because powerful states lose significant strategic flexibility, and states facing non-state threats are often left without a clear legal basis for action. Suppression (0.75) is high due to the active enforcement by multilateral institutions and the diplomatic/legal pressure exerted by the international community against broader interpretations. Theater ratio (0.20) is relatively low, as the core function of limiting unilateral force is still actively pursued, though some states engage in rhetorical justifications for actions that stretch the narrow reading. Accessibility collapse (0.40) is moderate; while unilateral force is constrained, states still have diplomatic, economic, and UN-sanctioned options. Resistance (0.70) is high, primarily from powerful states and those facing non-state threats who consistently challenge this narrow interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and those facing non-state threats experience this as a highly extractive constraint on their security, while weaker states and multilateral institutions perceive it as a vital coordination mechanism that protects the international order. The engine's per-seat classification should reflect this divergence, with payers computing as more extractive and beneficiaries as more coordinative.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states and multilateral institutions are beneficiaries (low d) as their security and authority are enhanced. Powerful states seeking unilateral action and states facing non-state actor threats are targets (high d) as their strategic options are curtailed. The international legal order, as an abstract entity, also benefits from its principles being upheld.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure extraction (Snare) by acknowledging its genuine coordination function in preventing aggressive war. It also avoids mislabeling it as a pure coordination (Rope) by recognizing the significant strategic costs imposed on certain actors. The contest over its interpretation is precisely what makes it a Tangled Rope, where the coordination function is intertwined with asymmetric extraction of strategic freedom. The founding problem of preventing aggressive war is still live, but the nature of threats (e.g., non-state actors) has evolved, leading to pressure for reinterpretation, which is captured by the 'contested' status of the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_standard_ambiguity,
    'What level of state involvement or control is required for a non-state actor''s actions to be ''attributable'' to a state under international law, thereby triggering Article 51 self-defense?',
    'Further ICJ jurisprudence or a new UN General Assembly resolution clarifying the ''effective control'' vs. ''overall control'' standards for attribution.',
    'A stricter attribution standard would increase the constraint''s extractiveness on states facing non-state threats; a looser standard would reduce it, potentially blurring the line between state and non-state actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attribution_standard_ambiguity, conceptual, 'Ambiguity in the legal standard for attributing non-state actor actions to a state.').

omega_variable(
    imminence_of_attack_definition,
    'How ''imminent'' must an armed attack be to justify self-defense under this narrow reading, particularly in the context of modern threats (e.g., cyberattacks, WMD proliferation)?',
    'Development of customary international law through state practice and opinio juris, or a Security Council resolution providing interpretive guidance.',
    'A very strict definition of imminence would further constrain states'' ability to respond to evolving threats, increasing extractiveness. A slightly more flexible definition, while still narrow, could reduce the perceived burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_of_attack_definition, conceptual, 'Ambiguity in defining ''imminence'' for armed attacks in contemporary security contexts.').

omega_variable(
    kernel_reading_contest,
    'Is this narrow reading of Article 51 the most legitimate interpretation, or do alternative readings (expansive preventive, unable/unwilling doctrine) offer a more appropriate balance for contemporary security challenges?',
    'Ongoing debate among states, international legal scholars, and ICJ/UNSC decisions. No single definitive resolution is expected.',
    'If an alternative reading gains widespread acceptance, this constraint would be reclassified, likely shifting towards a more permissive (less extractive for powerful states) framework for the use of force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, preference, 'This constraint is one reading of the Article 51 kernel; its legitimacy is contested by sibling readings.').


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
narrative_ontology:measurement(arti_tr_t2000, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(arti_tr_t2010, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(arti_be_t1960, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(arti_be_t1980, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(arti_be_t2000, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(arti_be_t2010, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(arti_su_t1960, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(arti_su_t1980, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(arti_su_t2000, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(arti_su_t2010, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article 51 self-defense kernel. Its strict interpretation influences the perceived legitimacy and operational space of the other, broader readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
