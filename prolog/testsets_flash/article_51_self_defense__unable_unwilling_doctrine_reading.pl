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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: UN Article 51 Self-Defense: Unwilling or Unable Doctrine
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'unwilling or unable' doctrine as a
 *   reading of UN Article 51 on self-defense. It permits a state to use force
 *   in self-defense against non-state actors in another state's territory if
 *   the host state is unwilling or unable to suppress the threat. This
 *   doctrine emerged post-9/11 to address transnational terrorism, creating a
 *   hybrid constraint that coordinates counterterrorism efforts while
 *   extracting sovereignty from host states. It is a contested
 *   interpretation, sitting between a narrow, state-centric view and a more
 *   expansive, preventive one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.6).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.7).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "UN Article 51 Self-Defense: Unwilling or Unable Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'fa1ade46-b681-48a1-90a1-d66076c22a62').
narrative_ontology:cs_kernel_codification('fa1ade46-b681-48a1-90a1-d66076c22a62', fixed_text).
narrative_ontology:cs_authority_grounding('fa1ade46-b681-48a1-90a1-d66076c22a62', lineage).
narrative_ontology:cs_interpretation_layer_present('fa1ade46-b681-48a1-90a1-d66076c22a62').
narrative_ontology:cs_reading_relation('fa1ade46-b681-48a1-90a1-d66076c22a62', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa1ade46-b681-48a1-90a1-d66076c22a62', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_axiom('fa1ade46-b681-48a1-90a1-d66076c22a62', foundational, sovereignty_conditional_on_threat_suppression).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_threat_suppression, holdable).
narrative_ontology:cs_axiom_grounding('fa1ade46-b681-48a1-90a1-d66076c22a62', sovereignty_conditional_on_threat_suppression, conventional).
narrative_ontology:cs_axiom('fa1ade46-b681-48a1-90a1-d66076c22a62', foundational, right_to_self_defense_against_non_state_actors).
narrative_ontology:cs_axiom_status(right_to_self_defense_against_non_state_actors, holdable).
narrative_ontology:cs_axiom_grounding('fa1ade46-b681-48a1-90a1-d66076c22a62', right_to_self_defense_against_non_state_actors, conventional).
narrative_ontology:cs_reference_frame('fa1ade46-b681-48a1-90a1-d66076c22a62', post_9_11_counterterrorism_paradigm).
narrative_ontology:cs_drift_state('fa1ade46-b681-48a1-90a1-d66076c22a62', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('fa1ade46-b681-48a1-90a1-d66076c22a62', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_non_state_actor_threats).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert the right to use force in self-defense against non-state actors operating from other states, particularly when the host state is perceived as unwilling or unable to address the threat. They benefit from expanded operational flexibility but face diplomatic and legal challenges.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    institutional, generational, constrained, global).

% These states bear the cost of having their sovereignty bypassed by intervening states. They may lack the capacity or political will to suppress non-state actors, leading to interventions that destabilize their territory and challenge their international standing. Their options are to accept intervention, resist it (risking conflict), or genuinely suppress the threat.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_non_state_actor_threats, payer,
    institutional, biographical, constrained, national).

% These groups are the direct targets of interventions under this doctrine. They operate from host states, often exploiting weak governance or internal conflict. They are not recognized as legitimate actors in the international legal framework governing self-defense.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups, excluded,
    moderate, immediate, trapped, regional).

% The UNSC is the primary body for authorizing the use of force in international law. It observes and debates interventions under this doctrine, often facing gridlock due to veto powers, which can implicitly legitimize unilateral actions by powerful states.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, united_nations_security_council, observer,
    institutional, generational, analytical, global).

% These scholars analyze the evolution and legality of the 'unwilling or unable' doctrine, contributing to the conceptual contestation of UN Article 51. Their work influences state practice and judicial interpretation over the long term.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to respond to cross-border non-state actor threats when the territorial state cannot or will not act, aiming to prevent the escalation of such threats into broader regional or international conflicts.
% TRANSFER_FUNCTION: Transfers the right to use force from the host state (which traditionally holds exclusive territorial sovereignty) to the intervening state, in exchange for the intervening state addressing a security threat that the host state has failed to manage.
% ABSENT_VOICES: Non-state armed groups, whose actions trigger the doctrine, are entirely excluded from the legal and diplomatic discourse, despite being central to the conflict. Their perspectives on grievances, motivations, and the impact of interventions are not formally considered.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, intervening states would lose a key justification for unilateral action against non-state actors, potentially leading to increased inaction against such threats or a return to more traditional (and often more escalatory) state-on-state conflict justifications. The international legal landscape for counterterrorism would be fundamentally altered.
% FOUNDING_PROBLEM: The rise of transnational non-state armed groups (e.g., Al-Qaeda, ISIS) operating from states that could not or would not control them, creating a gap in traditional international law's state-centric framework for self-defense.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by intervening states, host states, and international organizations, as transnational non-state actor threats remain a significant global security challenge. While the doctrine's legality is contested, the underlying problem it seeks to address is broadly acknowledged by parties outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).

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
 *   The doctrine's extractiveness (0.6) stems from its unilateral bypass of host state sovereignty. Suppression (0.7) is high because it actively overrides the host state's control over its territory, often through military force. Theater ratio (0.2) is relatively low, as interventions under this doctrine are typically genuine attempts to address threats, though the 'unwilling or unable' justification can sometimes be a pretext. Resistance (0.75) is high, reflecting the significant diplomatic and legal opposition from many states and international legal bodies.
 *
 * PERSPECTIVAL GAP:
 *   Intervening states perceive this doctrine as a necessary and legitimate adaptation of self-defense to modern threats, enabling coordination against terrorism. Host states, however, experience it as an infringement on their sovereignty and a form of extraction, leading to significant divergence in how the constraint is experienced and classified from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states are beneficiaries (d near 0.0) as they gain expanded operational scope to address threats. Host states are victims (d near 1.0) as their sovereignty is directly targeted and bypassed. The UNSC and legal scholars are observers (d near 0.5), analyzing the doctrine's implications without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine addresses a persistent problem (transnational non-state actors), so it is not mandatrophic in the sense of its founding problem being 'dead'. However, the contestation around its legality and the potential for abuse (e.g., using 'unwilling or unable' as a pretext for intervention) suggests a risk of drift towards pure extraction if the coordination function is overshadowed by unilateral power projection. The classification as a Tangled Rope reflects this hybrid nature, preventing it from being mislabeled as a pure Rope (ignoring extraction) or Snare (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_unable_threshold,
    'What objective criteria define a state as ''unwilling'' or ''unable'' to suppress non-state actor threats, and who adjudicates these criteria?',
    'Development of clear, internationally agreed-upon legal standards and an impartial adjudicative body (e.g., an empowered UNSC or ICJ ruling) to assess host state capacity and will.',
    'Clearer criteria and adjudication would reduce the doctrine''s extractiveness by limiting unilateral interpretations and potential for abuse, potentially shifting it closer to a Rope. Ambiguity allows intervening states to define the terms, increasing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unwilling_unable_threshold, conceptual, 'Ambiguity in defining ''unwilling or unable'' allows for unilateral interpretation and potential abuse.').

omega_variable(
    sovereignty_vs_security_balance,
    'At what point does the imperative of international security against non-state actors legitimately override the principle of state sovereignty, and is this balance stable?',
    'Long-term observation of state practice, international legal developments, and the outcomes of interventions. A shift towards greater international consensus on the limits of sovereignty in the face of transnational threats would indicate a re-calibration.',
    'If the balance shifts decisively towards security, the doctrine''s perceived legitimacy (and thus its classification) might move closer to a Rope. If sovereignty reasserts, the doctrine would be seen as more extractive (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_security_balance, empirical, 'The inherent tension between state sovereignty and collective security in the face of non-state threats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(arti_tr_t2007, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(arti_tr_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(arti_tr_t2018, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(arti_be_t2007, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(arti_be_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement(arti_be_t2018, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(arti_su_t2007, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement(arti_su_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement(arti_su_t2018, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, international_humanitarian_law_compliance).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, state_sovereignty_principle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UN Article 51 self-defense kernel, specifically the 'unwilling or unable' doctrine. It is linked to other readings of the same kernel, which represent alternative interpretations of self-defense in international law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
