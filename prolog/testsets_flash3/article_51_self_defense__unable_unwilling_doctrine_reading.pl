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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable or Unwilling Doctrine
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'unable or unwilling' doctrine as a
 *   reading of Article 51 of the UN Charter, which permits self-defense. This
 *   reading asserts that self-defense is triggered when a non-state actor
 *   attack originates from a host state that is unwilling or unable to
 *   suppress the threat. It is a hybrid constraint, requiring an actual
 *   non-state actor attack (not purely preventive) but permitting unilateral
 *   response when the host state fails to act, bypassing traditional
 *   sovereignty norms. This reading is distinct from a narrow interpretation
 *   (requiring state attribution) and an expansive interpretation (allowing
 *   purely preventive force).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.7).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable or Unwilling Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'a4e6225c-da7c-41ac-8b56-999af712d3d0').
narrative_ontology:cs_kernel_codification('a4e6225c-da7c-41ac-8b56-999af712d3d0', fixed_text).
narrative_ontology:cs_authority_grounding('a4e6225c-da7c-41ac-8b56-999af712d3d0', lineage).
narrative_ontology:cs_interpretation_layer_present('a4e6225c-da7c-41ac-8b56-999af712d3d0').
narrative_ontology:cs_reading_relation('a4e6225c-da7c-41ac-8b56-999af712d3d0', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4e6225c-da7c-41ac-8b56-999af712d3d0', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_axiom('a4e6225c-da7c-41ac-8b56-999af712d3d0', foundational, self_defense_extends_to_non_state_actors).
narrative_ontology:cs_axiom_status(self_defense_extends_to_non_state_actors, holdable).
narrative_ontology:cs_axiom_grounding('a4e6225c-da7c-41ac-8b56-999af712d3d0', self_defense_extends_to_non_state_actors, conventional).
narrative_ontology:cs_axiom('a4e6225c-da7c-41ac-8b56-999af712d3d0', foundational, host_state_failure_justifies_intervention).
narrative_ontology:cs_axiom_status(host_state_failure_justifies_intervention, holdable).
narrative_ontology:cs_axiom_grounding('a4e6225c-da7c-41ac-8b56-999af712d3d0', host_state_failure_justifies_intervention, instrumental).
narrative_ontology:cs_reference_frame('a4e6225c-da7c-41ac-8b56-999af712d3d0', post_9_11_security_paradigm).
narrative_ontology:cs_drift_state('a4e6225c-da7c-41ac-8b56-999af712d3d0', contemporary_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a4e6225c-da7c-41ac-8b56-999af712d3d0', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_non_state_actor_threats).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert the right to use force in self-defense against non-state actors operating from other states, particularly when the host state is perceived as 'unable or unwilling' to address the threat. They benefit from expanded operational flexibility but face diplomatic and legal challenges.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    institutional, generational, constrained, global).

% These states bear the cost of having their sovereignty bypassed by intervening states. They may lack the capacity to suppress non-state actors or may be unwilling to do so for political reasons. Their options are to suppress the threat themselves, accept intervention, or resist, risking further conflict.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_non_state_actor_threats, payer,
    institutional, biographical, constrained, national).

% These groups are the direct targets of self-defense operations under this doctrine. They face direct military action from intervening states, regardless of the host state's consent. Their options are to cease operations, relocate, or escalate conflict.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actors, payer,
    moderate, immediate, trapped, regional).

% These experts analyze the legality and implications of the 'unable or unwilling' doctrine, often highlighting its tension with state sovereignty and the UN Charter's prohibition on the use of force. They influence normative debates but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% The UNSC is the primary body for authorizing the use of force in international law. This doctrine challenges its exclusive authority, as intervening states often act unilaterally. The UNSC's role is to legitimize or condemn such actions, but its power is subject to vetoes.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, united_nations_security_council, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to coordinate responses to non-state actor threats that transcend national borders, particularly when the territorial state cannot or will not act, aiming to prevent the escalation of such threats.
% TRANSFER_FUNCTION: Transfers the right to use force from the host state (which is 'unable or unwilling') to the intervening state, effectively reallocating sovereign control over security operations in specific contexts.
% ABSENT_VOICES: Smaller, less powerful host states, particularly those with limited capacity, often find their sovereignty undermined by this doctrine but lack the diplomatic or military leverage to effectively object or resist intervention. Non-state actors themselves are never consulted.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, intervening states would lose a key justification for unilateral action against non-state actors abroad. This would likely lead to increased diplomatic friction, a potential rise in unchecked non-state actor activity, or a return to more traditional, state-centric interpretations of self-defense, requiring UNSC authorization or host state consent for intervention.
% FOUNDING_PROBLEM: The rise of transnational non-state actor terrorism (e.g., Al-Qaeda) operating from states that could not or would not control them, challenging traditional international law frameworks that focused on state-on-state armed attacks.
% FOUNDING_PROBLEM_CORROBORATION: Intervening states and many security analysts attest that the problem of transnational non-state actor threats remains live. Some international legal scholars, while acknowledging the problem, contest whether this doctrine is the appropriate or legal solution, arguing it creates more instability than it resolves.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The doctrine's extractiveness (0.65) stems from its unilateral bypass of host state sovereignty, imposing costs on states that may genuinely lack capacity. Suppression (0.70) is high because it actively suppresses the host state's sovereign right to control its territory and the non-state actors' ability to operate freely. The theater ratio (0.20) is moderate; while there's a genuine security concern, the 'unable or unwilling' justification can sometimes serve as a diplomatic cover for interventions driven by broader strategic interests. The slight dip in extractiveness and suppression at the end of the interval reflects a period of increased contestation and legal pushback against the doctrine's application.
 *
 * PERSPECTIVAL GAP:
 *   Intervening states perceive this doctrine as a necessary and legitimate adaptation of international law to modern threats, a coordination mechanism for global security. Host states, particularly those with limited capacity, often view it as an infringement on their sovereignty and a form of extraction, where their weakness is exploited to justify external intervention. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states are beneficiaries (d=0.0-0.2) as they gain expanded justification for military action. Host states are targets (d=0.8-1.0) as their sovereignty is directly challenged and bypassed. Non-state actors are also targets (d=0.9-1.0) as they face direct military action. The UN Security Council and international legal scholars act as observers or agenda-setters, influencing the normative landscape but not directly benefiting or being victimized by the constraint's operation in the same way.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_unable_unwilling,
    'What objective criteria define a state as ''unable'' or ''unwilling'' to suppress a non-state actor threat, and who makes that determination?',
    'Development of internationally agreed-upon, verifiable metrics for state capacity and political will, and a multilateral mechanism for making such determinations (e.g., UN Security Council or ICJ advisory opinion).',
    'Clearer criteria would reduce the doctrine''s extractiveness by limiting unilateral interpretations and potential abuse, potentially shifting it towards a more coordinated (Rope) or even Scaffold-like (temporary support) classification. Ambiguity allows for greater extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_unable_unwilling, conceptual, 'Ambiguity in defining ''unable or unwilling'' allows for unilateral interpretation and potential abuse.').

omega_variable(
    sovereignty_vs_security_priority,
    'Which normative principle takes precedence: state sovereignty (non-intervention) or collective security against transnational threats?',
    'A shift in international legal consensus or a new UN General Assembly resolution clarifying the hierarchy of these principles in the context of non-state actor threats.',
    'If sovereignty is prioritized, the doctrine''s legitimacy would erode, increasing its suppression and extractiveness as it becomes more contested. If collective security is prioritized, the doctrine might be formalized, reducing its perceived extractiveness for intervening states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_security_priority, preference, 'The doctrine highlights a fundamental tension between state sovereignty and the need for collective security against transnational threats.').

omega_variable(
    effectiveness_of_intervention,
    'Are unilateral interventions under this doctrine consistently effective in suppressing non-state actor threats without exacerbating regional instability or radicalization?',
    'Longitudinal empirical studies and independent evaluations of interventions conducted under this doctrine, assessing their primary and secondary effects on security and stability.',
    'Consistent ineffectiveness would undermine the doctrine''s functional justification, increasing its theater ratio and potentially reclassifying it as a Piton or even a Snare if the primary outcome is destabilization and continued extraction without genuine security gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_intervention, empirical, 'The actual effectiveness of interventions under this doctrine in achieving stated security goals is often debated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t5, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(arti_tr_t10, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t5, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(arti_be_t10, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(arti_su_t5, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(arti_su_t10, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
